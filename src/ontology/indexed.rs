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
use crate::model::{AnnotatedAxiom, MutableOntology, Ontology, OntologyID, IRI, ForIRI};
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
pub trait OntologyIndex<A: ForIRI> {
    /// Potentially insert an AnnotatedAxiom to the index.
    ///
    /// If the index did not have this value present, true is returned.
    ///
    /// If the index did have this value present, false is returned.
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom<A>>) -> bool;

    /// Remove an AnnotatedAxiom from the index.
    ///
    /// If the index did have this value present, true is returned.
    ///
    /// If the index did not have this value present, false is returned.
    fn index_remove(&mut self, ax: &AnnotatedAxiom<A>) -> bool {
        self.index_take(ax).is_some()
    }

    /// Take an AnnotatedAxiom from the index.
    ///
    /// Return the Some<AnnotatedAxiom if it is in the index.
    ///
    /// Return None if it does not.
    fn index_take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>>;
}

/// A NullOntologyIndex which does nothing.
#[derive(Default)]
pub struct NullIndex();
impl<A: ForIRI> OntologyIndex<A> for NullIndex {
    /// Insert an item, always returns false
    fn index_insert(&mut self, _ax: Rc<AnnotatedAxiom<A>>) -> bool {
        false
    }

    /// Remove an item, always returns false
    fn index_remove(&mut self, _ax: &AnnotatedAxiom<A>) -> bool {
        false
    }

    /// Returns the item, always returns `None`
    fn index_take(&mut self, _ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        None
    }
}

/// A `OneIndexedOntology` operates as a simple adaptor betweeen any
/// `OntologyIndex` and an `Ontology`.
#[derive(Default, Debug, Eq, PartialEq)]
pub struct OneIndexedOntology<A: ForIRI, I: OntologyIndex<A>>(I, OntologyID<A>, Option<IRI<A>>);

impl<A: ForIRI, I: OntologyIndex<A>> OneIndexedOntology<A, I> {
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

impl<A: ForIRI,I: OntologyIndex<A>> Ontology<A> for OneIndexedOntology<A, I> {
    fn id(&self) -> &OntologyID<A> {
        &self.1
    }

    fn mut_id(&mut self) -> &mut OntologyID<A> {
        &mut self.1
    }

    fn doc_iri(&self) -> &Option<IRI<A>> {
        &self.2
    }

    fn mut_doc_iri(&mut self) -> &mut Option<IRI<A>> {
        &mut self.2
    }
}

impl<A: ForIRI, I: OntologyIndex<A>> MutableOntology<A> for OneIndexedOntology<A, I> {
    fn insert<AA: Into<AnnotatedAxiom<A>>>(&mut self, ax: AA) -> bool {
        let rc = Rc::new(ax.into());
        self.0.index_insert(rc)
    }

    fn take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        self.0.index_take(ax)
    }
}

/// A `TwoIndexOntology` implements `Ontology` and supports two
/// `OntologyIndex`. It itself implements `OntologyIndex` so that it
/// can be composed.
#[derive(Default, Debug)]
pub struct TwoIndexedOntology<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>>
    (I, J, OntologyID<A>, Option<IRI<A>>);

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>> TwoIndexedOntology<A, I, J> {
    pub fn new(i: I, j: J, id: OntologyID<A>) -> Self {
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

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>> Ontology<A>
    for TwoIndexedOntology<A, I, J> {
    fn id(&self) -> &OntologyID<A> {
        &self.2
    }

    fn mut_id(&mut self) -> &mut OntologyID<A> {
        &mut self.2
    }

    fn doc_iri(&self) -> &Option<IRI<A>> {
        &self.3
    }

    fn mut_doc_iri(&mut self) -> &mut Option<IRI<A>> {
        &mut self.3
    }
}

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>> MutableOntology<A> for TwoIndexedOntology<A, I, J> {
    fn insert<AA: Into<AnnotatedAxiom<A>>>(&mut self, ax: AA) -> bool {
        let rc = Rc::new(ax.into());
        self.index_insert(rc)
    }

    fn take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        self.index_take(ax)
    }
}

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>> OntologyIndex<A> for TwoIndexedOntology<A, I, J> {
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom<A>>) -> bool {
        let rtn = self.0.index_insert(ax.clone());
        // Don't short cirtuit
        self.1.index_insert(ax) || rtn
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        let rtn = self.0.index_take(ax);
        self.1.index_take(ax).or(rtn)
    }
}

/// ThreeIndexedOntology supports three indexes.
#[derive(Default, Debug)]
pub struct ThreeIndexedOntology<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>,
                                K: OntologyIndex<A>>(
    TwoIndexedOntology<A, I, TwoIndexedOntology<A, J, K>>,
);

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>,
     K: OntologyIndex<A>> ThreeIndexedOntology<A, I, J, K> {
    pub fn new(i: I, j: J, k: K, id: OntologyID<A>) -> Self {
        ThreeIndexedOntology(TwoIndexedOntology(
            i,
            TwoIndexedOntology(j, k, Default::default(), Default::default()),
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

    pub fn index(self) -> (I, J, K) {
        let index = (self.0).1.index();
        ((self.0).0, index.0, index.1)
    }
}

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>,
     K: OntologyIndex<A>> Ontology<A>
    for ThreeIndexedOntology<A, I, J, K>
{
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

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>,
     K: OntologyIndex<A>> MutableOntology<A>
    for ThreeIndexedOntology<A, I, J, K>
{
    fn insert<AA: Into<AnnotatedAxiom<A>>>(&mut self, ax: AA) -> bool {
        self.0.insert(ax)
    }

    fn take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        self.0.take(ax)
    }
}

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>,
     K: OntologyIndex<A>> OntologyIndex<A>
    for ThreeIndexedOntology<A, I, J, K>
{
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom<A>>) -> bool {
        let rtn = (self.0).0.index_insert(ax.clone());
        // Don't short cirtuit
        (self.0).1.index_insert(ax) || rtn
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        let rtn = (self.0).0.index_take(ax);
        (self.0).1.index_take(ax).or(rtn)
    }
}

/// FourIndexedOntology supports three indexes.
#[derive(Default, Debug)]
pub struct FourIndexedOntology<
        A: ForIRI, 
    I: OntologyIndex<A>,
    J: OntologyIndex<A>,
    K: OntologyIndex<A>,
    L: OntologyIndex<A>,
>(TwoIndexedOntology<A, I, ThreeIndexedOntology<A, J, K, L>>);

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>,
     K: OntologyIndex<A>, L: OntologyIndex<A>>
    FourIndexedOntology<A, I, J, K, L>
{
    pub fn new(i: I, j: J, k: K, l: L, id: OntologyID<A>) -> Self {
        FourIndexedOntology(TwoIndexedOntology(
            i,
            ThreeIndexedOntology::new(j, k, l, Default::default()),
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

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>,
     K: OntologyIndex<A>, L: OntologyIndex<A>> Ontology<A>
    for FourIndexedOntology<A, I, J, K, L>
{
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

impl<A: ForIRI, I: OntologyIndex<A>, J: OntologyIndex<A>,
     K: OntologyIndex<A>, L: OntologyIndex<A>> MutableOntology<A>
    for FourIndexedOntology<A, I, J, K, L>
{
    fn insert<AA: Into<AnnotatedAxiom<A>>>(&mut self, ax: AA) -> bool {
        self.0.insert(ax)
    }

    fn take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        self.0.take(ax)
    }
}

// Utility
pub(crate) fn rc_unwrap_or_clone<A: ForIRI>(rcax: Rc<AnnotatedAxiom<A>>) -> AnnotatedAxiom<A> {
    Rc::try_unwrap(rcax).unwrap_or_else(|rcax| (*rcax).clone())
}

#[cfg(test)]
mod test {

    use std::rc::Rc;

    use super::{
        FourIndexedOntology, NullIndex, OneIndexedOntology, ThreeIndexedOntology,
        TwoIndexedOntology,
    };
    use crate::{
        model::{AnnotatedAxiom, Build, MutableOntology, NamedEntity},
        ontology::set::SetIndex,
    };

    fn stuff() -> (AnnotatedAxiom<Rc<str>>, AnnotatedAxiom<Rc<str>>, AnnotatedAxiom<Rc<str>>) {
        let b = Build::new_rc();
        let c: NamedEntity<_> = b.class("http://www.example.com/c").into();
        let o: NamedEntity<_> = b.object_property("http://www.example.com/p").into();
        let b: NamedEntity<_> = b.data_property("http://www.example.com/d").into();

        (c.into(), o.into(), b.into())
    }

    #[test]
    fn one_cons() {
        let _o:OneIndexedOntology<Rc<str>, _> = OneIndexedOntology::new(SetIndex::new());
        assert!(true);
    }

    #[test]
    fn one_insert() {
        let mut o = OneIndexedOntology::new(SetIndex::new());
        let e = stuff();
        o.insert(e.0);
        o.insert(e.1);
        o.insert(e.2);

        assert_eq!(o.i().into_iter().count(), 3);
    }

    #[test]
    fn one_remove() {
        let mut o = OneIndexedOntology::new(SetIndex::new());
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
            TwoIndexedOntology::new(SetIndex::new_rc(), SetIndex::new(), Default::default());
        assert!(true);

        let _o = TwoIndexedOntology::new(
            SetIndex::new_rc(),
            NullIndex::default(),
            Default::default(),
        );
        assert!(true);
    }

    #[test]
    fn two_insert() {
        let mut o =
            TwoIndexedOntology::new(SetIndex::new_rc(), SetIndex::new(), Default::default());
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
            TwoIndexedOntology::new(SetIndex::new_rc(), SetIndex::new(), Default::default());

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
            SetIndex::new_rc(),
            SetIndex::new(),
            SetIndex::new(),
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
            SetIndex::new_rc(),
            SetIndex::new(),
            SetIndex::new(),
            SetIndex::new(),
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
