//! Indexes for ontologies to enable faster searching

//! # Overview
//!

//! This module provides an interface and a number of multiplexers for
//! an `OntologyIndex`.

//! An indexed `MutableOntology` is one that uses one or more
//! `OntologyIndex` objects as the backing store for its
//! `AnnotatedAxiom`. These `AnnotatedAxiom` objects are shared
//! between different `OntologyIndex` objects using `Rc`. The
//! `OntologyIndex` interace does not provide any mechanisms for
//! searching or querying the index which need to be provided by
//! concrete implementations.

//! Indexes can be add to `OneIndexedOntology`, `TwoIndexedOntology`
//! and `ThreeIndexedOntology`, each of which operate something like a
//! named tuple, allowing differently typed `OntologyIndex` objects to
//! be added.
use crate::model::{AnnotatedAxiom, MutableOntology, Ontology, OntologyID, IRI};
use std::rc::Rc;

/// An `OntologyIndex` object.
///
/// The `OntologyIndex` stores references to an `AnnotatedAxiom` and
/// as they are added (or removed) from an `IndexedObject`. Objects
/// implementing this can provide search facilities over the
/// ``Ontology`; they should, in general, only provide search
/// facilities that they can do rapidly and quickly (e.g. in constant
/// or log time, not linear).
///
/// A given `OntologyIndex` object is not bound to keep references to
/// all `Rc<AnnotatedAxiom>` that are inserted into it, although at
/// least one `OntologyIndex` object for an `IndexedOntology` should
/// do, or the it will be dropped entirely. The `SetIndex` is a simple
/// way to achieving this.
pub trait OntologyIndex {
    /// Potentially insert an AnnotatedAxiom to the index.
    ///
    /// If the index did not have this value present, true is returned.
    ///
    /// If the index did have this value present, false is returned.
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom>) -> bool;

    /// Remove an AnnotatedAxiom from the index.
    ///
    /// If the index did have this value present, true is returned.
    ///
    /// If the index did not have this value present, false is returned.
    fn index_remove(&mut self, ax: &AnnotatedAxiom) -> bool {
        self.index_take(ax).is_some()
    }

    /// Take an AnnotatedAxiom from the index.
    ///
    /// Return the Some<AnnotatedAxiom if it is in the index.
    ///
    /// Return None if it does not.
    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom>;
}

/// A NullOntologyIndex which does nothing.
#[derive(Default)]
pub struct NullIndex();
impl OntologyIndex for NullIndex {
    /// Insert an item, always returns false
    fn index_insert(&mut self, _ax: Rc<AnnotatedAxiom>) -> bool {
        false
    }

    /// Remove an item, always returns false
    fn index_remove(&mut self, _ax: &AnnotatedAxiom) -> bool {
        false
    }

    /// Returns the item, always returns `None`
    fn index_take(&mut self, _ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        None
    }
}

/// A `OneIndexedOntology` operates as a simple adaptor betweeen any
/// `OntologyIndex` and an `Ontology`.
#[derive(Default, Debug, Eq, PartialEq)]
pub struct OneIndexedOntology<I: OntologyIndex>(I, OntologyID, Option<IRI>);

impl<I: OntologyIndex> OneIndexedOntology<I> {
    pub fn new(i: I) -> Self {
        OneIndexedOntology(i, Default::default(), Default::default())
    }

    pub fn i(&self) -> &I {
        &self.0
    }

    pub fn index(self) -> I {
        self.0
    }
}

impl<I: OntologyIndex> Ontology for OneIndexedOntology<I> {
    fn id(&self) -> &OntologyID {
        &self.1
    }

    fn mut_id(&mut self) -> &mut OntologyID {
        &mut self.1
    }

    fn doc_iri(&self) -> &Option<IRI>{
        &self.2
    }

    fn mut_doc_iri(&mut self) -> &Option<IRI>{
        &mut self.2
    }
}

impl<I: OntologyIndex> MutableOntology for OneIndexedOntology<I> {
    fn insert<A: Into<AnnotatedAxiom>>(&mut self, ax: A) -> bool {
        let rc = Rc::new(ax.into());
        self.0.index_insert(rc)
    }

    fn take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.0.index_take(ax)
    }
}

/// A `TwoIndexOntology` implements `Ontology` and supports two
/// `OntologyIndex`. It itself implements `OntologyIndex` so that it
/// can be composed.
#[derive(Default, Debug)]
pub struct TwoIndexedOntology<I: OntologyIndex, J: OntologyIndex>(I, J, OntologyID, Option<IRI>);

impl<I: OntologyIndex, J: OntologyIndex> TwoIndexedOntology<I, J> {
    pub fn new(i: I, j: J, id: OntologyID) -> Self {
        TwoIndexedOntology(i, j, id, Default::default())
    }

    pub fn i(&self) -> &I {
        &self.0
    }

    pub fn j(&self) -> &J {
        &self.1
    }

    pub fn index(self) -> (I, J) {
        (self.0, self.1)
    }
}

impl<I: OntologyIndex, J: OntologyIndex> Ontology for TwoIndexedOntology<I, J> {
    fn id(&self) -> &OntologyID {
        &self.2
    }

    fn mut_id(&mut self) -> &mut OntologyID {
        &mut self.2
    }

    fn doc_iri(&self) -> &Option<IRI> {
        &self.3
    }

    fn mut_doc_iri(&mut self) -> &Option<IRI>{
        &mut self.3
    }
}

impl<I: OntologyIndex, J: OntologyIndex> MutableOntology for TwoIndexedOntology<I, J> {
    fn insert<A: Into<AnnotatedAxiom>>(&mut self, ax: A) -> bool {
        let rc = Rc::new(ax.into());
        self.index_insert(rc)
    }

    fn take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.index_take(ax)
    }
}

impl<I: OntologyIndex, J: OntologyIndex> OntologyIndex for TwoIndexedOntology<I, J> {
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom>) -> bool {
        let rtn = self.0.index_insert(ax.clone());
        // Don't short cirtuit
        self.1.index_insert(ax) || rtn
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        let rtn = self.0.index_take(ax);
        self.1.index_take(ax).or(rtn)
    }
}

/// ThreeIndexedOntology supports three indexes.
#[derive(Default, Debug)]
pub struct ThreeIndexedOntology<I: OntologyIndex, J: OntologyIndex, K: OntologyIndex>(
    TwoIndexedOntology<I, TwoIndexedOntology<J, K>>,
);

impl<I: OntologyIndex, J: OntologyIndex, K: OntologyIndex> ThreeIndexedOntology<I, J, K> {
    pub fn new(i: I, j: J, k: K, id: OntologyID) -> Self {
        ThreeIndexedOntology(TwoIndexedOntology(
            i,
            TwoIndexedOntology(j, k, Default::default(), Default::default()),
            id,
            Default::default()
        ))
    }

    pub fn i(&self) -> &I {
        (&self.0).i()
    }

    pub fn j(&self) -> &J {
        (&self.0).j().i()
    }

    pub fn k(&self) -> &K {
        &(self.0).j().j()
    }

    pub fn index(self) -> (I, J, K) {
        let index = (self.0).1.index();
        ((self.0).0, index.0, index.1)
    }
}

impl<I: OntologyIndex, J: OntologyIndex, K: OntologyIndex> Ontology
    for ThreeIndexedOntology<I, J, K>
{
    fn id(&self) -> &OntologyID {
        self.0.id()
    }

    fn mut_id(&mut self) -> &mut OntologyID {
        self.0.mut_id()
    }

    fn doc_iri(&self) -> &Option<IRI> {
        self.0.doc_iri()
    }

    fn mut_doc_iri(&mut self) -> &Option<IRI>{
        self.0.mut_doc_iri()
    }
}

impl<I: OntologyIndex, J: OntologyIndex, K: OntologyIndex> MutableOntology
    for ThreeIndexedOntology<I, J, K>
{
    fn insert<A: Into<AnnotatedAxiom>>(&mut self, ax: A) -> bool {
        self.0.insert(ax)
    }

    fn take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.0.take(ax)
    }
}

impl<I: OntologyIndex, J: OntologyIndex, K: OntologyIndex> OntologyIndex
    for ThreeIndexedOntology<I, J, K>
{
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom>) -> bool {
        let rtn = (self.0).0.index_insert(ax.clone());
        // Don't short cirtuit
        (self.0).1.index_insert(ax) || rtn
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        let rtn = (self.0).0.index_take(ax);
        (self.0).1.index_take(ax).or(rtn)
    }
}

/// FourIndexedOntology supports three indexes.
#[derive(Default, Debug)]
pub struct FourIndexedOntology<
    I: OntologyIndex,
    J: OntologyIndex,
    K: OntologyIndex,
    L: OntologyIndex,
>(TwoIndexedOntology<I, ThreeIndexedOntology<J, K, L>>);

impl<I: OntologyIndex, J: OntologyIndex, K: OntologyIndex, L: OntologyIndex>
    FourIndexedOntology<I, J, K, L>
{
    pub fn new(i: I, j: J, k: K, l: L, id: OntologyID) -> Self {
        FourIndexedOntology(TwoIndexedOntology(
            i,
            ThreeIndexedOntology::new(j, k, l,
                                      Default::default()),
            id,
            Default::default(),
        ))
    }

    pub fn i(&self) -> &I {
        (&self.0).i()
    }

    pub fn j(&self) -> &J {
        (&self.0).j().i()
    }

    pub fn k(&self) -> &K {
        &(self.0).j().j()
    }

    pub fn l(&self) -> &L {
        &(self.0).j().k()
    }

    pub fn index(self) -> (I, J, K, L) {
        let index = (self.0).1.index();
        ((self.0).0, index.0, index.1, index.2)
    }
}

impl<I: OntologyIndex, J: OntologyIndex, K: OntologyIndex, L: OntologyIndex> Ontology
    for FourIndexedOntology<I, J, K, L>
{
    fn id(&self) -> &OntologyID {
        self.0.id()
    }

    fn mut_id(&mut self) -> &mut OntologyID {
        self.0.mut_id()
    }

    fn doc_iri(&self) -> &Option<IRI> {
        self.0.doc_iri()
    }

    fn mut_doc_iri(&mut self) -> &Option<IRI>{
        self.0.mut_doc_iri()
    }
}

impl<I: OntologyIndex, J: OntologyIndex, K: OntologyIndex, L: OntologyIndex> MutableOntology
    for FourIndexedOntology<I, J, K, L>
{
    fn insert<A: Into<AnnotatedAxiom>>(&mut self, ax: A) -> bool {
        self.0.insert(ax)
    }

    fn take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.0.take(ax)
    }
}

// Utility
pub(crate) fn rc_unwrap_or_clone(rcax: Rc<AnnotatedAxiom>) -> AnnotatedAxiom {
    Rc::try_unwrap(rcax).unwrap_or_else(|rcax| (*rcax).clone())
}

#[cfg(test)]
mod test {

    use super::{
        FourIndexedOntology, NullIndex, OneIndexedOntology, ThreeIndexedOntology,
        TwoIndexedOntology,
    };
    use crate::{
        model::{AnnotatedAxiom, Build, MutableOntology, NamedEntity},
        ontology::set::SetIndex,
    };

    fn stuff() -> (AnnotatedAxiom, AnnotatedAxiom, AnnotatedAxiom) {
        let b = Build::new();
        let c: NamedEntity = b.class("http://www.example.com/c").into();
        let o: NamedEntity = b.object_property("http://www.example.com/p").into();
        let b: NamedEntity = b.data_property("http://www.example.com/d").into();

        (c.into(), o.into(), b.into())
    }

    #[test]
    fn one_cons() {
        let _o = OneIndexedOntology::new(SetIndex::default());
        assert!(true);
    }

    #[test]
    fn one_insert() {
        let mut o = OneIndexedOntology::new(SetIndex::default());
        let e = stuff();
        o.insert(e.0);
        o.insert(e.1);
        o.insert(e.2);

        assert_eq!(o.i().into_iter().count(), 3);
    }

    #[test]
    fn one_remove() {
        let mut o = OneIndexedOntology::new(SetIndex::default());
        let e = stuff();
        o.insert(e.0.clone());
        o.insert(e.1.clone());
        o.insert(e.2.clone());

        assert_eq!(o.i().into_iter().count(), 3);
        assert!(o.remove(&e.0));
        assert!(o.remove(&e.1));
        assert!(o.remove(&e.2));

        assert_eq!(o.i().into_iter().count(), 0);
        assert!(!o.remove(&e.0));
        assert!(!o.remove(&e.1));
        assert!(!o.remove(&e.2));
    }

    #[test]
    fn two_cons() {
        let _o =
            TwoIndexedOntology::new(SetIndex::default(), SetIndex::default(), Default::default());
        assert!(true);

        let _o = TwoIndexedOntology::new(
            SetIndex::default(),
            NullIndex::default(),
            Default::default(),
        );
        assert!(true);
    }

    #[test]
    fn two_insert() {
        let mut o =
            TwoIndexedOntology::new(SetIndex::default(), SetIndex::default(), Default::default());
        let e = stuff();
        o.insert(e.0);
        o.insert(e.1);
        o.insert(e.2);

        assert_eq!(o.i().into_iter().count(), 3);
        assert_eq!(o.j().into_iter().count(), 3);
        assert_eq!(o.i(), o.j());
    }

    #[test]
    fn two_remove() {
        let mut o =
            TwoIndexedOntology::new(SetIndex::default(), SetIndex::default(), Default::default());

        let e = stuff();
        o.insert(e.0.clone());
        o.insert(e.1.clone());
        o.insert(e.2.clone());

        assert_eq!(o.i().into_iter().count(), 3);
        assert!(o.remove(&e.0));
        assert!(o.remove(&e.1));
        assert!(o.remove(&e.2));

        assert_eq!(o.i().into_iter().count(), 0);
        assert!(!o.remove(&e.0));
        assert!(!o.remove(&e.1));
        assert!(!o.remove(&e.2));
        assert_eq!(o.i(), o.j());
    }

    #[test]
    fn three_remove() {
        let mut o = ThreeIndexedOntology::new(
            SetIndex::default(),
            SetIndex::default(),
            SetIndex::default(),
            Default::default(),
        );

        let e = stuff();
        o.insert(e.0.clone());
        o.insert(e.1.clone());
        o.insert(e.2.clone());

        assert_eq!(o.i().into_iter().count(), 3);
        assert!(o.remove(&e.0));
        assert!(o.remove(&e.1));
        assert!(o.remove(&e.2));

        assert_eq!(o.i().into_iter().count(), 0);
        assert!(!o.remove(&e.0));
        assert!(!o.remove(&e.1));
        assert!(!o.remove(&e.2));

        assert_eq!(o.i(), o.j());
        assert_eq!(o.i(), o.k());
    }

    #[test]
    fn four_remove() {
        let mut o = FourIndexedOntology::new(
            SetIndex::default(),
            SetIndex::default(),
            SetIndex::default(),
            SetIndex::default(),
            Default::default(),
        );

        let e = stuff();
        o.insert(e.0.clone());
        o.insert(e.1.clone());
        o.insert(e.2.clone());

        assert_eq!(o.i().into_iter().count(), 3);
        assert!(o.remove(&e.0));
        assert!(o.remove(&e.1));
        assert!(o.remove(&e.2));

        assert_eq!(o.i().into_iter().count(), 0);
        assert!(!o.remove(&e.0));
        assert!(!o.remove(&e.1));
        assert!(!o.remove(&e.2));

        assert_eq!(o.i(), o.j());
        assert_eq!(o.i(), o.k());
        assert_eq!(o.i(), o.l());
    }
}
