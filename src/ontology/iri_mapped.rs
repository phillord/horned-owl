//! Access `AnnotatedAxiom` by iri.

//! # Overview
//!
//! This module provides an `IRIMappedIndex` which provides rapid
//! access to all axioms associated with a given IRI.
//!
use super::set::SetOntology;
use crate::model::*;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, VecDeque},
    sync::Arc,
};

use super::indexed::{rc_unwrap_or_clone, OneIndexedOntology, TwoIndexedOntology,
    ThreeIndexedOntology, OntologyIndex};
use super::axiom_mapped::AxiomMappedIndex;
use super::declaration_mapped::DeclarationMappedIndex;

use std::collections::HashSet;

macro_rules! some {
    ($body:expr) => {
        (|| Some($body))()
    };
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct IRIMappedIndex {
    irindex: RefCell<BTreeMap<IRI, BTreeSet<Arc<AnnotatedAxiom>>>>,
}

impl IRIMappedIndex {
    /// Create a new ontology.
    pub fn new() -> IRIMappedIndex {
        IRIMappedIndex::default()
    }

    fn aa_to_iris(&self, ax: &AnnotatedAxiom) -> HashSet<IRI> {

        let mut iris = HashSet::new();

        match ax.kind() {
            AxiomKind::DeclareClass |
            AxiomKind::DeclareObjectProperty |
            AxiomKind::DeclareAnnotationProperty |
            AxiomKind::DeclareDataProperty |
            AxiomKind::DeclareDatatype |
            AxiomKind::DeclareNamedIndividual => {
                match ax.clone().axiom {
                    Axiom::DeclareClass(dc) => {iris.insert(dc.0.into());},
                    Axiom::DeclareObjectProperty(op) => {iris.insert(op.0.into());},
                    Axiom::DeclareAnnotationProperty(ap) => {iris.insert(ap.0.into());},
                    Axiom::DeclareDataProperty(dp) => {iris.insert(dp.0.into());},
                    Axiom::DeclareDatatype(dt) => {iris.insert(dt.0.into());},
                    Axiom::DeclareNamedIndividual(ni) => {iris.insert(ni.0.into());},
                    _ => ()
                }
            },
            AxiomKind::AnnotationAssertion => {
                match ax.clone().axiom {
                    Axiom::AnnotationAssertion(AnnotationAssertion{subject,ann:_}) => {iris.insert(subject);},
                    _ => (),
                }
            },
            AxiomKind::SubClassOf => {
                match ax.clone().axiom {
                    Axiom::SubClassOf(SubClassOf{sup:_,sub}) => {
                        match sub {
                            ClassExpression::Class(c) => {iris.insert(c.0.into());},
                            _ => (),
                        }
                    },
                    _ => (),
                }
            },
            AxiomKind::EquivalentClasses => {
                match ax.clone().axiom {
                    Axiom::EquivalentClasses(EquivalentClasses(eles)) => {
                        for clsexp in eles {
                            match clsexp {
                                ClassExpression::Class(c) => {iris.insert(c.0.clone());},
                                _ => (),  //to do - generic method to get IRI for any class expression to avoid duplicating this block?
                            }
                        }
                    },
                    _ => (),
                }
            },
            AxiomKind::DisjointClasses => {
                match ax.clone().axiom {
                    Axiom::DisjointClasses(DisjointClasses(eles)) => {
                        for clsexp in eles {
                            match clsexp {
                                ClassExpression::Class(c) => {iris.insert(c.0.clone());},
                                _ => (),
                            }
                        } 
                    },
                    _ => (),
                }
            },
            AxiomKind::DisjointUnion => {
                match ax.clone().axiom {
                    Axiom::DisjointUnion(DisjointUnion(clsexp, _eles)) => {
                        iris.insert(clsexp.0.clone());
                    },
                    _ => (),
                }
            },
            _ => (),
        };

        iris
    }

    /// Fetch the iris hashmap as a raw pointer.
    ///
    /// This method also ensures that the BTreeSet for `iri` is
    /// instantiated, which means that it effects equality of the
    /// ontology. It should only be used where the intention is to
    /// update the ontology.
    fn axioms_as_ptr(
        &self,
        iri: IRI,
    ) -> *mut BTreeMap<IRI, BTreeSet<Arc<AnnotatedAxiom>>> {
        self.irindex
            .borrow_mut()
            .entry(iri)
            .or_insert_with(BTreeSet::new);
        self.irindex.as_ptr()
    }

    /// Fetch the axioms for the given iri.
    fn set_for_iri(&self, iri: IRI) -> Option<&BTreeSet<Arc<AnnotatedAxiom>>> {
        unsafe { (*self.irindex.as_ptr()).get(&iri) }
    }

    /// Fetch the axioms for given iri as a mutable ref.
    fn mut_set_for_iri(&mut self, iri: IRI) -> &mut BTreeSet<Arc<AnnotatedAxiom>> {
        unsafe { (*self.axioms_as_ptr(iri.clone())).get_mut(&iri).unwrap() }
    }

    /// Gets an iterator that visits the annotated axioms of the ontology.
    pub fn iter(&self) -> IRIMappedIter {
        IRIMappedIter {
            ont: self,
            inner: None,
            iris: unsafe { (*self.irindex.as_ptr()).keys().collect() },
        }
    }

    /// Fetch the AnnotatedAxiom for a given IRI
    ///
    /// See also `axiom` for access to the `Axiom` without annotations.
    pub fn annotated_axiom(&self, iri: IRI) -> impl Iterator<Item = &AnnotatedAxiom> {
        self.set_for_iri(iri)
            // Iterate over option
            .into_iter()
            // flatten option iterator!
            .flat_map(|hs| hs.iter())
            .map(|rc| &**rc)
    }

    /// Fetch the Axiom set iterator for a given iri
    ///
    pub fn axiom(&self, iri: IRI) -> impl Iterator<Item = &Axiom> {
        self.annotated_axiom(iri).map(|ann| &ann.axiom)
    }

}

impl AsRef<IRIMappedIndex> for
    OneIndexedOntology<IRIMappedIndex> {
    fn as_ref(&self) -> &IRIMappedIndex {
        self.i()
    }
}

impl<I: OntologyIndex> AsRef<IRIMappedIndex> for
    TwoIndexedOntology<I, IRIMappedIndex> {
    fn as_ref(&self) -> &IRIMappedIndex {
        self.j()
    }
}

impl<I, J> AsRef<IRIMappedIndex> for
    ThreeIndexedOntology<I, J, IRIMappedIndex>
where I: OntologyIndex,
      J: OntologyIndex,
{
    fn as_ref(&self) -> &IRIMappedIndex {
        self.k()
    }
}


/// An owning iterator over the annotated axioms of an `Ontology`.
impl IntoIterator for IRIMappedIndex {
    type Item = AnnotatedAxiom;
    type IntoIter = std::vec::IntoIter<AnnotatedAxiom>;
    fn into_iter(self) -> Self::IntoIter {
        let btreemap = self.irindex.into_inner();
        let v: Vec<AnnotatedAxiom> = btreemap
            .into_iter()
            .map(|(_k, v)| v)
            .flat_map(BTreeSet::into_iter)
            .map(Arc::try_unwrap)
            .map(Result::unwrap)
            .collect();
        v.into_iter()
    }
}


/// An iterator over the annotated axioms of an `Ontology`.
pub struct IRIMappedIter<'a> {
    ont: &'a IRIMappedIndex,
    iris: VecDeque<&'a IRI>,
    inner: Option<<&'a BTreeSet<Arc<AnnotatedAxiom>> as IntoIterator>::IntoIter>,
}

impl<'a> Iterator for IRIMappedIter<'a> {
    type Item = &'a AnnotatedAxiom;
    fn next(&mut self) -> Option<Self::Item> {
        // Consume the current iterator if there are items left.
        if let Some(ref mut it) = self.inner {
            if let Some(axiom) = it.next() {
                return Some(axiom);
            }
        }
        // Attempt to consume the iterator for the next axiom kind
        if !self.iris.is_empty() {
            let iri = self.iris.pop_front().unwrap();
            self.inner = self.ont.set_for_iri(iri.clone()).map(BTreeSet::iter);
            self.next().map(|rc| &*rc)
        } else {
            None
        }
    }
}


impl<'a> IntoIterator for &'a IRIMappedIndex {
    type Item = &'a AnnotatedAxiom;
    type IntoIter = IRIMappedIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        IRIMappedIter {
            ont: self,
            inner: None,
            iris: unsafe { (*self.irindex.as_ptr()).keys().collect() },
        }
    }
}

impl OntologyIndex for IRIMappedIndex {
    fn index_insert(&mut self, ax: Arc<AnnotatedAxiom>) -> bool {
        let iris = self.aa_to_iris(&*ax);
        if !iris.is_empty() {
            for iri in iris.iter() {
                self.mut_set_for_iri(iri.clone()).insert(ax.clone());
            }
            true
        } else {
            false
        }
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        let iris = self.aa_to_iris(ax);
        if !iris.is_empty() {
            let iri = iris.iter().next();
            if let Some(iri) = iri {
                self.mut_set_for_iri(iri.clone())
                    .take(ax)
                    .map(rc_unwrap_or_clone)
            } else {
                None
            }
        } else {
            None
        }

    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom) -> bool {
        if let Some(iri) = self.aa_to_iris(ax).iter().next() {
            let s = some!{
                self.mut_set_for_iri(iri.clone()).remove(ax)
            };
            s.is_some()
        } else {
            false
        }
    }
}

pub type IRIMappedOntology = ThreeIndexedOntology<IRIMappedIndex, //i
                                                  DeclarationMappedIndex, //j
                                                  AxiomMappedIndex>; //k

impl IRIMappedOntology {
    //Utility method gets an iterator over the axioms in the index for a given IRI
    pub fn get_axs_for_iri(&mut self, iri: IRI) -> impl Iterator<Item = &AnnotatedAxiom> {
        self.i().annotated_axiom(iri)
    }

    //Utility method updates an axiom in the index
    pub fn update_axiom(&mut self, ax: &AnnotatedAxiom, new_ax: AnnotatedAxiom)
                                    -> bool {

        self.take(ax);
        self.insert(new_ax)
    }
}

/// An owning iterator over the annotated axioms of an `Ontology`.
impl IntoIterator for IRIMappedOntology {
    type Item = AnnotatedAxiom;
    type IntoIter = std::vec::IntoIter<AnnotatedAxiom>;
    fn into_iter(self) -> Self::IntoIter {
        self.index().0.into_iter()
    }
}

impl From<SetOntology> for IRIMappedOntology {
    fn from(mut so: SetOntology) -> IRIMappedOntology {
        let mut imo = IRIMappedOntology::default();
        std::mem::swap(imo.mut_id(), &mut so.mut_id());
        for ax in so {
            imo.insert(ax);
        }
        imo
    }
}

impl From<IRIMappedOntology> for SetOntology {
    fn from(mut imo: IRIMappedOntology) -> SetOntology {
        let mut so = SetOntology::default();
        std::mem::swap(so.mut_id(), imo.mut_id());

        for ax in imo {
            so.insert(ax);
        }
        so
    }
}

#[cfg(test)]
mod test {
    use super::IRIMappedOntology;
    use crate::model::*;

    #[test]
    fn test_ontology_cons() {
        let _ = IRIMappedOntology::default();
        assert!(true);
    }

    #[test]
    fn test_ontology_iter_empty() {
        // Empty ontologies should stop iteration right away
        let mut it = IRIMappedOntology::default().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_into_iter() {
        // Setup
        let build = Build::new();
        let mut o = IRIMappedOntology::default();
        let decl1 = DeclareClass(build.class("http://www.example.com#a"));
        let decl2 = DeclareClass(build.class("http://www.example.com#b"));
        let decl3 = DeclareClass(build.class("http://www.example.com#c"));
        let disj1 = DisjointClasses(vec![
            ClassExpression::Class(build.class("http://www.example.com#a")),
            ClassExpression::Class(build.class("http://www.example.com#b")),
        ]);
        let disj2 = DisjointClasses(vec![
            ClassExpression::Class(build.class("http://www.example.com#b")),
            ClassExpression::Class(build.class("http://www.example.com#c")),
        ]);
        o.insert(disj1.clone());
        o.insert(disj2.clone());
        o.insert(decl1.clone());
        o.insert(decl2.clone());
        o.insert(decl3.clone());

        // Iteration is based on ascending order of axiom kinds.
        let mut it = o.into_iter();
        assert_eq!(
            it.next(),
            Some(AnnotatedAxiom::from(Axiom::DeclareClass(decl1)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedAxiom::from(Axiom::DeclareClass(decl2)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedAxiom::from(Axiom::DeclareClass(decl3)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedAxiom::from(Axiom::DisjointClasses(disj1)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedAxiom::from(Axiom::DisjointClasses(disj2)))
        );
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_into_iter_empty() {
        // Empty ontologies should stop iteration right away
        let o = IRIMappedOntology::default();
        let mut it = o.into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_iter() {
        // Setup
        let build = Build::new();
        let mut o = IRIMappedOntology::default();
        let decl1 = DeclareClass(build.class("http://www.example.com#a"));
        let decl2 = DeclareClass(build.class("http://www.example.com#b"));
        let decl3 = DeclareClass(build.class("http://www.example.com#c"));
        let disj1 = DisjointClasses(vec![
            ClassExpression::Class(build.class("http://www.example.com#a")),
            ClassExpression::Class(build.class("http://www.example.com#b")),
        ]);
        let disj2 = DisjointClasses(vec![
            ClassExpression::Class(build.class("http://www.example.com#b")),
            ClassExpression::Class(build.class("http://www.example.com#c")),
        ]);
        o.insert(disj1.clone());
        o.insert(disj2.clone());
        o.insert(decl1.clone());
        o.insert(decl2.clone());
        o.insert(decl3.clone());

        // Iteration is based on ascending order of axiom kinds.
        let mut it = (&o).i().iter();
        assert_eq!(
            it.next(),
            Some(&AnnotatedAxiom::from(Axiom::DeclareClass(decl1)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedAxiom::from(Axiom::DeclareClass(decl2)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedAxiom::from(Axiom::DeclareClass(decl3)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedAxiom::from(Axiom::DisjointClasses(disj1)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedAxiom::from(Axiom::DisjointClasses(disj2)))
        );
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }
}
