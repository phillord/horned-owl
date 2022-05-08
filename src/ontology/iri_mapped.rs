//! Access `AnnotatedAxiom` by iri.

//! # Overview
//!
//! This module provides an `IRIMappedIndex` which provides rapid
//! access to all axioms associated with a given IRI.
//!
use super::set::SetOntology;
use super::indexed::ForIndex;
use crate::model::*;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, VecDeque},
    rc::Rc,
    sync::Arc,
};

use std::marker::PhantomData;

use super::indexed::{OneIndexedOntology, TwoIndexedOntology,
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
pub struct IRIMappedIndex<A, AA> {
    irindex: RefCell<BTreeMap<IRI<A>, BTreeSet<AA>>>,
    pd: PhantomData<A>,
}

impl<A: ForIRI, AA:ForIndex<A>> IRIMappedIndex<A, AA> {
    /// Create a new ontology.
    pub fn new() -> IRIMappedIndex<A,AA> {
        IRIMappedIndex{
            irindex: RefCell::new(BTreeMap::new()),
            pd: Default::default()
        }
    }

    fn aa_to_iris(&self, ax: &AnnotatedAxiom<A>) -> HashSet<IRI<A>> {

        let mut iris:HashSet<IRI<A>> = HashSet::new();
        let build = Build::new();

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
                    Axiom::AnnotationAssertion(AnnotationAssertion{subject,ann:_}) =>
                            {iris.insert(build.iri(subject.deref()));},
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
        iri: IRI<A>,
    ) -> *mut BTreeMap<IRI<A>, BTreeSet<AA>> {
        self.irindex
            .borrow_mut()
            .entry(iri)
            .or_insert_with(BTreeSet::new);
        self.irindex.as_ptr()
    }

    /// Fetch the axioms for the given iri.
    fn set_for_iri(&self, iri: IRI<A>) -> Option<&BTreeSet<AA>> {
        unsafe { (*self.irindex.as_ptr()).get(&iri) }
    }

    /// Fetch the axioms for given iri as a mutable ref.
    fn mut_set_for_iri(&mut self, iri: IRI<A>) -> &mut BTreeSet<AA> {
        unsafe { (*self.axioms_as_ptr(iri)).get_mut(&iri).unwrap() }
    }

    /// Gets an iterator that visits the annotated axioms of the ontology.
    pub fn iter(&self) -> IRIMappedIter<A, AA> {
        IRIMappedIter {
            ont: self,
            inner: None,
            iris: unsafe { (*self.irindex.as_ptr()).keys().collect() },
        }
    }

    /// Fetch the AnnotatedAxiom for a given IRI
    ///
    /// See also `axiom` for access to the `Axiom` without annotations.
    pub fn annotated_axiom(&self, iri: IRI<A>) -> impl Iterator<Item = &AnnotatedAxiom<A>> {
        self.set_for_iri(iri)
            // Iterate over option
            .into_iter()
            // flatten option iterator!
            .flat_map(|hs| hs.iter())
            .map(|rc| rc.borrow())
    }

    /// Fetch the Axiom set iterator for a given iri
    ///
    pub fn axiom(&self, iri: IRI<A>) -> impl Iterator<Item = &Axiom<A>> {
        self.annotated_axiom(iri).map(|ann| &ann.axiom)
    }

}

impl<A: ForIRI, AA: ForIndex<A>> AsRef<IRIMappedIndex<A,AA>> for
    OneIndexedOntology<A,AA,IRIMappedIndex<A,AA>> {
    fn as_ref(&self) -> &IRIMappedIndex<A,AA> {
        self.i()
    }
}

impl<A: ForIRI, AA: ForIndex<A>, I: OntologyIndex<A,AA>> AsRef<IRIMappedIndex<A,AA>> for
    TwoIndexedOntology<A, AA, I, IRIMappedIndex<A,AA>> {
    fn as_ref(&self) -> &IRIMappedIndex<A,AA> {
        self.j()
    }
}

impl<A: ForIRI, AA: ForIndex<A>, I, J> AsRef<IRIMappedIndex<A,AA>> for
    ThreeIndexedOntology<A, AA, I, J, IRIMappedIndex<A,AA>>
where I: OntologyIndex<A,AA>,
      J: OntologyIndex<A,AA>,
{
    fn as_ref(&self) -> &IRIMappedIndex<A,AA> {
        self.k()
    }
}


/// An owning iterator over the annotated axioms of an `Ontology`.
impl<A: ForIRI, AA:ForIndex<A>> IntoIterator for IRIMappedIndex<A, AA> {
    type Item = AnnotatedAxiom<A>;
    type IntoIter = std::vec::IntoIter<AnnotatedAxiom<A>>;
    fn into_iter(self) -> Self::IntoIter {
        let btreemap = self.irindex.into_inner();
        let v: Vec<AnnotatedAxiom<A>> = btreemap
            .into_iter()
            .map(|(_k, v)| v)
            .flat_map(BTreeSet::into_iter)
            .map(|fi| fi.unwrap())
            .collect();
        v.into_iter()
    }
}

/// An iterator over the annotated axioms of an `Ontology`.
pub struct IRIMappedIter<'a, A: ForIRI, AA: ForIndex<A>> {
    ont: &'a IRIMappedIndex<A,AA>,
    iris: VecDeque<&'a IRI<A>>,
    inner: Option<<&'a BTreeSet<AA> as IntoIterator>::IntoIter>,
}

impl<'a, A: ForIRI, AA: ForIndex<A>> Iterator for IRIMappedIter<'a, A, AA> {
    type Item = &'a AnnotatedAxiom<A>;
    fn next(&mut self) -> Option<Self::Item> {
        // Consume the current iterator if there are items left.
        if let Some(ref mut it) = self.inner {
            if let Some(axiom) = it.next() {
                return Some(axiom.borrow());
            }
        }
        // Attempt to consume the iterator for the next IRI
        if !self.iris.is_empty() {
            let iri = self.iris.pop_front().unwrap();
            self.inner = self.ont.set_for_iri(*iri).map(BTreeSet::iter);
            self.next().map(|rc| &*rc)
        } else {
            None
        }
    }
}

impl<'a, A: ForIRI, AA: ForIndex<A>> IntoIterator for &'a IRIMappedIndex<A,AA> {
    type Item = &'a AnnotatedAxiom<A>;
    type IntoIter = IRIMappedIter<'a,A,AA>;
    fn into_iter(self) -> Self::IntoIter {
        IRIMappedIter {
            ont: self,
            inner: None,
            iris: unsafe { (*self.irindex.as_ptr()).keys().collect() },
        }
    }
}

impl<A: ForIRI, AA:ForIndex<A>> OntologyIndex<A,AA> for IRIMappedIndex<A, AA> {
    fn index_insert(&mut self, ax: AA) -> bool {
        let iris = self.aa_to_iris(ax.borrow());
        if !iris.is_empty() {
            for iri in iris.iter() {
                self.mut_set_for_iri(iri).insert(ax);
            }
            true
        } else {
            false
        }
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        let iris = self.aa_to_iris(ax.borrow());
        if !iris.is_empty() {
            let iri = iris.iter().next();
            if let Some(iri) = iri {
                self.mut_set_for_iri(iri.clone())
                    .take(ax)
                    .map(|aax| aax.unwrap() )
            } else {
                None
            }
        } else {
            None
        }

    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom<A>) -> bool {
        if let Some(iri) = self.aa_to_iris(ax.borrow()).iter().next() {
            let s = some!{
                self.mut_set_for_iri(iri.clone()).remove(ax)
            };
            s.is_some()
        } else {
            false
        }
    }
}


#[derive(Default, Debug, Eq, PartialEq)]
pub struct IRIMappedOntology<A,AA>  (ThreeIndexedOntology<A, AA, IRIMappedIndex<A,AA>,
    AxiomMappedIndex<A,AA>, DeclarationMappedIndex<A,AA>>);

pub type RcIRIMappedOntology = IRIMappedOntology<Rc<str>, Rc<AnnotatedAxiom<Rc<str>>>>;
pub type ArcIRIMappedOntology = IRIMappedOntology<Arc<str>, Arc<AnnotatedAxiom<Arc<str>>>>;

impl<A:ForIRI, AA:ForIndex<A>> Ontology<A> for IRIMappedOntology<A, AA> {
    fn id(&self) -> &OntologyID<A> {
        self.0.id()
    }

    fn mut_id(&mut self) -> &mut OntologyID<A> {
        self.0.mut_id()
    }

    fn doc_iri(&self) -> &Option<IRI<A>> {
        self.0.doc_iri()
    }

    fn mut_doc_iri(&mut self) -> &mut Option<IRI<A>> {
        self.0.mut_doc_iri()
    }
}


impl<A:ForIRI, AA:ForIndex<A>> MutableOntology<A> for IRIMappedOntology<A, AA> {
    fn insert<IAA>(&mut self, ax: IAA) -> bool
    where
        IAA: Into<AnnotatedAxiom<A>> {
        self.0.insert(ax)
    }

    fn take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        self.0.take(ax)
    }
}

impl<A:ForIRI, AA:ForIndex<A>> IRIMappedOntology<A, AA> {
    pub fn index(self) -> IRIMappedIndex<A, AA> {
        self.0.index()
    }

    pub fn i(&self) -> &IRIMappedIndex<A, AA> {
        self.0.i()
    }

    pub fn new() -> IRIMappedOntology<A, AA> {
        IRIMappedOntology(
            OneIndexedOntology::new(IRIMappedIndex::new())
        )
    }

    //Utility method gets an iterator over the axioms in the index for a given IRI
    pub fn get_axs_for_iri(&mut self, iri: IRI<A>) -> impl Iterator<Item = &AnnotatedAxiom<A>> {
        self.i().annotated_axiom(iri)
    }

    //Utility method updates an axiom in the index
    pub fn update_axiom(&mut self, ax: &AnnotatedAxiom<A>, new_ax: AnnotatedAxiom<A>)
                                    -> bool {

        self.take(ax);
        self.insert(new_ax)
    }
}


impl RcIRIMappedOntology {
    pub fn new_rc() -> Self {
        IRIMappedOntology::new()
    }
}

impl ArcIRIMappedOntology {
    pub fn new_arc() -> Self {
        IRIMappedOntology::new()
    }
}

/// An owning iterator over the annotated axioms of an `Ontology`.
impl<A: ForIRI, AA: ForIndex<A>> IntoIterator for IRIMappedOntology<A,AA> {
    type Item = AnnotatedAxiom<A>;
    type IntoIter = std::vec::IntoIter<AnnotatedAxiom<A>>;
    fn into_iter(self) -> Self::IntoIter {
        self.index().0.into_iter()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<SetOntology<A>> for IRIMappedOntology<A,AA> {
    fn from(mut so: SetOntology<A>) -> IRIMappedOntology<A,AA> {
        let mut imo = IRIMappedOntology::new();
        std::mem::swap(imo.mut_id(), &mut so.mut_id());
        for ax in so {
            imo.insert(ax);
        }
        imo
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<IRIMappedOntology<A,AA>> for SetOntology<A> {
    fn from(mut imo: IRIMappedOntology<A,AA>) -> SetOntology<A> {
        let mut so = SetOntology::new();
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
        let _ = IRIMappedOntology::new_arc();
        assert!(true);
    }

    #[test]
    fn test_ontology_iter_empty() {
        // Empty ontologies should stop iteration right away
        let mut it = IRIMappedOntology::new_arc().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }
//
//    #[test]
//    fn test_ontology_into_iter() {
//        // Setup
//        let build = Build::new();
//        let mut o = IRIMappedOntology::default();
//        let decl1 = DeclareClass(build.class("http://www.example.com#a"));
//        let decl2 = DeclareClass(build.class("http://www.example.com#b"));
//        let decl3 = DeclareClass(build.class("http://www.example.com#c"));
//        let disj1 = DisjointClasses(vec![
//            ClassExpression::Class(build.class("http://www.example.com#a")),
//            ClassExpression::Class(build.class("http://www.example.com#b")),
//        ]);
//        let disj2 = DisjointClasses(vec![
//            ClassExpression::Class(build.class("http://www.example.com#b")),
//            ClassExpression::Class(build.class("http://www.example.com#c")),
//        ]);
//        o.insert(disj1.clone());
//        o.insert(disj2.clone());
//        o.insert(decl1.clone());
//        o.insert(decl2.clone());
//        o.insert(decl3.clone());
//
//        // Iteration is based on ascending order of iris
//        let mut it = o.into_iter();
//        assert_eq!(
//            it.next(),
//            Some(AnnotatedAxiom::from(Axiom::DeclareClass(decl1)))
//        );
//        assert_eq!(
//            it.next(),
//            Some(AnnotatedAxiom::from(Axiom::DisjointClasses(disj1)))
//        );
//        assert_eq!(
//            it.next(),
//            Some(AnnotatedAxiom::from(Axiom::DeclareClass(decl2)))
//        );
//        assert_eq!(
//            it.next(),
//            Some(AnnotatedAxiom::from(Axiom::DisjointClasses(disj2)))
//        );
//        assert_eq!(
//            it.next(),
//            Some(AnnotatedAxiom::from(Axiom::DeclareClass(decl3)))
//        );
//        assert_eq!(it.next(), None);
//        assert_eq!(it.next(), None);
//    }

    #[test]
    fn test_ontology_into_iter_empty() {
        // Empty ontologies should stop iteration right away
        let o = IRIMappedOntology::new_arc();
        let mut it = o.into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

//    #[test]
//    fn test_ontology_iter() {
//        // Setup
//        let build = Build::new();
//        let mut o = IRIMappedOntology::default();
//        let decl1 = DeclareClass(build.class("http://www.example.com#a"));
//        let decl2 = DeclareClass(build.class("http://www.example.com#b"));
//        let decl3 = DeclareClass(build.class("http://www.example.com#c"));
//        let disj1 = DisjointClasses(vec![
//            ClassExpression::Class(build.class("http://www.example.com#a")),
//            ClassExpression::Class(build.class("http://www.example.com#b")),
//        ]);
//        let disj2 = DisjointClasses(vec![
//            ClassExpression::Class(build.class("http://www.example.com#b")),
//            ClassExpression::Class(build.class("http://www.example.com#c")),
//        ]);
//        o.insert(disj1.clone());
//        o.insert(disj2.clone());
//        o.insert(decl1.clone());
//        o.insert(decl2.clone());
//        o.insert(decl3.clone());
//
//        // Iteration is based on ascending order of axiom kinds.
//        let mut it = (&o).i().iter();
//        assert_eq!(
//            it.next(),
//            Some(&AnnotatedAxiom::from(Axiom::DeclareClass(decl1)))
//        );
//        assert_eq!(
//            it.next(),
//            Some(&AnnotatedAxiom::from(Axiom::DeclareClass(decl2)))
//        );
//        assert_eq!(
//            it.next(),
//            Some(&AnnotatedAxiom::from(Axiom::DeclareClass(decl3)))
//        );
//        assert_eq!(
//            it.next(),
//            Some(&AnnotatedAxiom::from(Axiom::DisjointClasses(disj1)))
//        );
//        assert_eq!(
//            it.next(),
//            Some(&AnnotatedAxiom::from(Axiom::DisjointClasses(disj2)))
//        );
//        //assert_eq!(it.next(), None);
//        //assert_eq!(it.next(), None);
//    }
}
