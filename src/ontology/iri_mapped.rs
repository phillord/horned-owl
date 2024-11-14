//! Access `AnnotatedComponent` by iri.

//! # Overview
//!
//! This module provides an `IRIMappedIndex` which provides rapid
//! access to all components associated with a given IRI.
//!
use super::indexed::ForIndex;
use super::set::SetOntology;
use crate::{
    model::*,
    visitor::immutable::{entity::IRIExtract, Walk},
};
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
    sync::Arc,
};

use super::component_mapped::ComponentMappedIndex;
use super::declaration_mapped::DeclarationMappedIndex;
use super::indexed::{FourIndexedOntology, OntologyIndex};
use super::set::SetIndex;

use std::collections::HashSet;

#[derive(Debug, Eq, PartialEq)]
pub struct IRIMappedIndex<A, AA> {
    irindex: RefCell<BTreeMap<IRI<A>, BTreeSet<AA>>>,
}

impl<A: ForIRI, AA: ForIndex<A>> IRIMappedIndex<A, AA> {
    /// Create a new ontology.
    pub fn new() -> IRIMappedIndex<A, AA> {
        IRIMappedIndex {
            irindex: RefCell::new(BTreeMap::new()),
        }
    }

    fn aa_to_iris(&self, cmp: &AnnotatedComponent<A>) -> HashSet<IRI<A>> {
        let mut w = Walk::new(IRIExtract::default());
        w.annotated_component(cmp);

        w.into_visit().into_vec().into_iter().collect()
    }

    /// Fetch the iris hashmap as a raw pointer.
    ///
    /// This method also ensures that the BTreeSet for `iri` is
    /// instantiated, which means that it effects equality of the
    /// ontology. It should only be used where the intention is to
    /// update the ontology.
    fn components_as_ptr(&self, iri: &IRI<A>) -> *mut BTreeMap<IRI<A>, BTreeSet<AA>> {
        self.irindex.borrow_mut().entry(iri.clone()).or_default();
        self.irindex.as_ptr()
    }

    /// Fetch the axioms for the given iri.
    fn set_for_iri(&self, iri: &IRI<A>) -> Option<&BTreeSet<AA>> {
        unsafe { (*self.irindex.as_ptr()).get(iri) }
    }

    /// Fetch the axioms for given iri as a mutable ref.
    fn mut_set_for_iri(&mut self, iri: &IRI<A>) -> &mut BTreeSet<AA> {
        unsafe { (*self.components_as_ptr(iri)).get_mut(iri).unwrap() }
    }

    /*
    /// Gets an iterator that visits the annotated components of the ontology.
    pub fn iter(&self) -> IRIMappedIter<A, AA> {
        IRIMappedIter {
            ont: self,
            inner: None,
            iris: unsafe { (*self.irindex.as_ptr()).keys().collect() },
        }
    }
     */

    /// Fetch the AnnotatedComponent for a given IRI
    ///
    /// See also `component` for access to the `Component` without annotations.
    pub fn component_for_iri(&self, iri: &IRI<A>) -> impl Iterator<Item = &AnnotatedComponent<A>> {
        self.set_for_iri(iri)
            // Iterate over option
            .into_iter()
            // flatten option iterator!
            .flat_map(|hs| hs.iter())
            .map(|rc| rc.borrow())
    }

    /// Fetch the Component set iterator for a given iri
    ///
    pub fn component(&self, iri: &IRI<A>) -> impl Iterator<Item = &Component<A>> {
        self.component_for_iri(iri).map(|ann| &ann.component)
    }
}

impl<A, AA> Default for IRIMappedIndex<A, AA> {
    fn default() -> Self {
        Self {
            irindex: Default::default(),
        }
    }
}

/*
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
    type Item = AnnotatedComponent<A>;
    type IntoIter = std::vec::IntoIter<AnnotatedComponent<A>>;
    fn into_iter(self) -> Self::IntoIter {
        let btreemap = self.irindex.into_inner();
        let v: Vec<AnnotatedComponent<A>> = btreemap
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
    type Item = &'a AnnotatedComponent<A>;
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
    type Item = &'a AnnotatedComponent<A>;
    type IntoIter = IRIMappedIter<'a,A,AA>;
    fn into_iter(self) -> Self::IntoIter {
        IRIMappedIter {
            ont: self,
            inner: None,
            iris: unsafe { (*self.irindex.as_ptr()).keys().collect() },
        }
    }
}
*/

impl<A: ForIRI, AA: ForIndex<A>> OntologyIndex<A, AA> for IRIMappedIndex<A, AA> {
    fn index_insert(&mut self, cmp: AA) -> bool {
        let iris = self.aa_to_iris(cmp.borrow());
        if !iris.is_empty() {
            for iri in iris.iter() {
                self.mut_set_for_iri(iri).insert(cmp.clone());
            }
            true
        } else {
            false
        }
    }

    fn index_take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.aa_to_iris(cmp).iter().fold(None, |val, iri| {
            self.mut_set_for_iri(iri)
                .take(cmp)
                .map_or(val, |c| Some(c.unwrap()))
        })
    }

    fn index_remove(&mut self, cmp: &AnnotatedComponent<A>) -> bool {
        self.aa_to_iris(cmp).iter().fold(false, |val, iri| {
            self.mut_set_for_iri(iri).remove(cmp) || val
        })
    }
}

#[allow(clippy::type_complexity)]
pub struct IRIMappedOntology<A: ForIRI, AA: ForIndex<A>>(
    FourIndexedOntology<
        A,
        AA,
        SetIndex<A, AA>,
        IRIMappedIndex<A, AA>,
        ComponentMappedIndex<A, AA>,
        DeclarationMappedIndex<A, AA>,
    >,
);

pub type RcIRIMappedOntology = IRIMappedOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>;
pub type ArcIRIMappedOntology = IRIMappedOntology<ArcStr, Arc<AnnotatedComponent<ArcStr>>>;

impl<A: ForIRI, AA: ForIndex<A>> Ontology<A> for IRIMappedOntology<A, AA> {}

impl<A: ForIRI, AA: ForIndex<A>> MutableOntology<A> for IRIMappedOntology<A, AA> {
    fn insert<IAA>(&mut self, cmp: IAA) -> bool
    where
        IAA: Into<AnnotatedComponent<A>>,
    {
        self.0.insert(cmp)
    }

    fn take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.0.take(cmp)
    }
}

impl<A: ForIRI, AA: ForIndex<A>> Default for IRIMappedOntology<A, AA> {
    fn default() -> IRIMappedOntology<A, AA> {
        IRIMappedOntology(FourIndexedOntology::new(
            SetIndex::new(),
            IRIMappedIndex::new(),
            ComponentMappedIndex::new(),
            DeclarationMappedIndex::new(),
        ))
    }
}

impl<A: ForIRI, AA: ForIndex<A>> IRIMappedOntology<A, AA> {
    //Utility method gets an iterator over the components in the index for a given IRI
    pub fn components_for_iri(
        &mut self,
        iri: &IRI<A>,
    ) -> impl Iterator<Item = &AnnotatedComponent<A>> {
        self.0.j().component_for_iri(iri)
    }

    //Utility method gets an iterator over the axioms in the index for a given IRI
    pub fn component_for_kind(
        &mut self,
        cmk: ComponentKind,
    ) -> impl Iterator<Item = &AnnotatedComponent<A>> {
        self.0.k().component_for_kind(cmk)
    }

    //Utility method updates an axiom in the index
    pub fn update_axiom(
        &mut self,
        cmp: &AnnotatedComponent<A>,
        new_cmp: AnnotatedComponent<A>,
    ) -> bool {
        self.take(cmp);
        self.insert(new_cmp)
    }

    pub fn iter(&self) -> std::vec::IntoIter<&AnnotatedComponent<A>> {
        self.0.i().into_iter()
    }
}
impl RcIRIMappedOntology {
    pub fn new_rc() -> Self {
        IRIMappedOntology::default()
    }
}

impl ArcIRIMappedOntology {
    pub fn new_arc() -> Self {
        IRIMappedOntology::default()
    }
}

/// An owning iterator over the annotated axioms of an `Ontology`.
impl<A: ForIRI, AA: ForIndex<A>> IntoIterator for IRIMappedOntology<A, AA> {
    type Item = AnnotatedComponent<A>;
    type IntoIter = std::vec::IntoIter<AnnotatedComponent<A>>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.index().0.into_iter()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<SetOntology<A>> for IRIMappedOntology<A, AA> {
    fn from(so: SetOntology<A>) -> IRIMappedOntology<A, AA> {
        let mut imo = IRIMappedOntology::default();
        for cmp in so {
            imo.insert(cmp);
        }
        imo
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<IRIMappedOntology<A, AA>> for SetOntology<A> {
    fn from(imo: IRIMappedOntology<A, AA>) -> SetOntology<A> {
        imo.0.index().0.into()
    }
}

#[cfg(test)]
mod test {
    use super::{IRIMappedIndex, IRIMappedOntology};
    use crate::{model::*, ontology::indexed::OntologyIndex};

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

    #[test]
    fn test_ontology_into_iter() {
        // Setup
        let build = Build::new();
        let mut o = IRIMappedOntology::new_rc();
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

        // Iteration order is not guaranteed
        let mut v: Vec<_> = o.into_iter().collect();
        v.sort();

        assert_eq!(
            v,
            [
                AnnotatedComponent::from(Component::DeclareClass(decl1)),
                AnnotatedComponent::from(Component::DeclareClass(decl2)),
                AnnotatedComponent::from(Component::DeclareClass(decl3)),
                AnnotatedComponent::from(Component::DisjointClasses(disj1)),
                AnnotatedComponent::from(Component::DisjointClasses(disj2)),
            ]
        );
    }

    #[test]
    fn test_ontology_into_iter_empty() {
        // Empty ontologies should stop iteration right away
        let o = IRIMappedOntology::new_arc();
        let mut it = o.into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_iter() {
        // Setup
        let build = Build::new();
        let mut o = IRIMappedOntology::new_rc();
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

        // Iteration order is not guaranteed
        let mut v: Vec<_> = o.iter().collect();
        v.sort();

        assert_eq!(
            v,
            [
                &AnnotatedComponent::from(Component::DeclareClass(decl1)),
                &AnnotatedComponent::from(Component::DeclareClass(decl2)),
                &AnnotatedComponent::from(Component::DeclareClass(decl3)),
                &AnnotatedComponent::from(Component::DisjointClasses(disj1)),
                &AnnotatedComponent::from(Component::DisjointClasses(disj2)),
            ]
        );
    }

    #[test]
    fn test_index_remove() {
        let build = Build::new();
        let mut i = IRIMappedIndex::<RcStr, RcAnnotatedComponent>::new();
        let disj1 = AnnotatedComponent {
            component: Component::DisjointClasses(DisjointClasses(vec![
                ClassExpression::Class(build.class("http://www.example.com/#A")),
                ClassExpression::Class(build.class("http://www.example.com/#B")),
            ])),
            ann: Default::default(),
        };

        i.index_insert(RcAnnotatedComponent::new(disj1.clone()));
        i.index_remove(&disj1);

        let irindex = i.irindex.borrow();

        let mut v: Vec<_> = irindex
            .iter()
            .flat_map(|(i, s)| s.into_iter().map(move |c| (i.clone(), c.clone())))
            .collect();
        v.sort();

        assert_eq!(v, Vec::<(IRI<RcStr>, RcAnnotatedComponent)>::new())
    }
}
