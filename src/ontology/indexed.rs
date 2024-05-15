//! Indexes for ontologies to enable faster searching

//! # Overview
//!

//! This module provides an interface and a number of multiplexers for
//! an `OntologyIndex`.

//! An indexed `MutableOntology` is one that uses one or more
//! `OntologyIndex` objects as the backing store for its
//! `AnnotatedAxiom`. These `AnnotatedAxiom` objects are shared
//! between different `OntologyIndex` objects using `Rc`. The
//! `OntologyIndex` interface does not provide any mechanisms for
//! searching or querying the index which need to be provided by
//! concrete implementations.

//! Indexes can be add to `OneIndexedOntology`, `TwoIndexedOntology`
//! and `ThreeIndexedOntology`, each of which operate something like a
//! named tuple, allowing differently typed `OntologyIndex` objects to
//! be added.
use crate::model::{AnnotatedComponent, ArcStr, ForIRI, MutableOntology, Ontology, RcStr, IRI};
use std::borrow::Borrow;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::rc::Rc;
use std::sync::Arc;

pub trait ForIndex<A: ForIRI>:
    Borrow<AnnotatedComponent<A>>
    + Clone
    + Debug
    + Eq
    + From<AnnotatedComponent<A>>
    + Hash
    + Ord
    + PartialEq
    + PartialOrd
{
    // fn unwrap(&self) -> AnnotatedComponent<A> {
    //     (*self.borrow()).clone()
    // }
}

impl<A: ForIRI, T: ?Sized> ForIndex<A> for T where
    T: Borrow<AnnotatedComponent<A>>
        + Clone
        + Debug
        + Eq
        + From<AnnotatedComponent<A>>
        + Hash
        + Ord
        + PartialEq
        + PartialOrd
{
}

/// An `OntologyIndex` object.
///
/// The `OntologyIndex` stores references to an `AnnotatedComponent` and
/// as they are added (or removed) from an `IndexedObject`. Objects
/// implementing this can provide search facilities over the
/// ``Ontology`; they should, in general, only provide search
/// facilities that they can do rapidly and quickly (e.g. in constant
/// or log time, not linear).
///
/// A given `OntologyIndex` object is not bound to keep references to
/// all `Rc<AnnotatedComponent>` that are inserted into it, although at
/// least one `OntologyIndex` object for an `IndexedOntology` should
/// do, or the it will be dropped entirely. The `SetIndex` is a simple
/// way to achieving this.
pub trait OntologyIndex<A, AA> {
    /// Potentially insert an AnnotatedComponent to the index.
    ///
    /// If the index did not have this value present, true is returned.
    ///
    /// If the index did have this value present, false is returned.
    fn index_insert(&mut self, cmp: AA) -> bool;

    /// Remove an AnnotatedComponent from the index.
    ///
    /// If the index did have this value present, true is returned.
    ///
    /// If the index did not have this value present, false is returned.
    fn index_remove(&mut self, cmp: &AnnotatedComponent<A>) -> bool;

    fn index_take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>>
    where
        A: Clone,
    {
        if self.index_remove(cmp) {
            Some(cmp.clone())
        } else {
            None
        }
    }
}

/// A NullOntologyIndex which does nothing.
#[derive(Default)]
pub struct NullIndex();
impl<A, AA> OntologyIndex<A, AA> for NullIndex {
    /// Insert an item, always returns false
    fn index_insert(&mut self, _cmp: AA) -> bool {
        false
    }

    /// Remove an item, always returns false
    fn index_remove(&mut self, _cmp: &AnnotatedComponent<A>) -> bool {
        false
    }
}

/// A `OneIndexedOntology` operates as a simple adaptor between any
/// [ontology index](OntologyIndex) and an [ontology](Ontology).
#[derive(Debug, Default, Eq, PartialEq)]
pub struct OneIndexedOntology<A, AA, I>(I, Option<IRI<A>>, PhantomData<AA>);

impl<A, AA, I: OntologyIndex<A, AA>> From<I> for OneIndexedOntology<A, AA, I> {
    fn from(value: I) -> Self {
        OneIndexedOntology(value, Default::default(), Default::default())
    }
}

impl<A, AA, I> OneIndexedOntology<A, AA, I> {
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

impl<I> OneIndexedOntology<RcStr, Rc<AnnotatedComponent<RcStr>>, I>
where
    I: OntologyIndex<RcStr, Rc<AnnotatedComponent<RcStr>>>,
{
    pub fn new_rc(i: I) -> OneIndexedOntology<RcStr, Rc<AnnotatedComponent<RcStr>>, I> {
        Self::new(i)
    }
}

impl<I> OneIndexedOntology<ArcStr, Arc<AnnotatedComponent<ArcStr>>, I>
where
    I: OntologyIndex<ArcStr, Arc<AnnotatedComponent<ArcStr>>>,
{
    pub fn new_arc(i: I) -> OneIndexedOntology<ArcStr, Arc<AnnotatedComponent<ArcStr>>, I> {
        Self::new(i)
    }
}

impl<A: Clone, AA, I: Clone> Clone for OneIndexedOntology<A, AA, I> {
    fn clone(&self) -> Self {
        OneIndexedOntology(self.0.clone(), self.1.clone(), Default::default())
    }
}

impl<A, AA, I: OntologyIndex<A, AA>> Ontology<A> for OneIndexedOntology<A, AA, I> {}

impl<A: Clone, AA: From<AnnotatedComponent<A>>, I: OntologyIndex<A, AA>> MutableOntology<A>
    for OneIndexedOntology<A, AA, I>
{
    fn insert<IAA: Into<AnnotatedComponent<A>>>(&mut self, cmp: IAA) -> bool {
        let cmp = cmp.into();
        self.0.index_insert(cmp.into())
    }

    fn take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.0.index_take(cmp)
    }
}

/// A `TwoIndexOntology` implements `Ontology` and supports two
/// `OntologyIndex`. It itself implements `OntologyIndex` so that it
/// can be composed.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct TwoIndexedOntology<A, AA, I, J>(I, J, Option<IRI<A>>, PhantomData<AA>);

impl<A, AA, I: OntologyIndex<A, AA>, J: OntologyIndex<A, AA>> From<(I, J)>
    for TwoIndexedOntology<A, AA, I, J>
{
    fn from(value: (I, J)) -> Self {
        TwoIndexedOntology(value.0, value.1, Default::default(), Default::default())
    }
}

impl<A, AA, I, J> TwoIndexedOntology<A, AA, I, J> {
    pub fn new(i: I, j: J) -> Self {
        TwoIndexedOntology(i, j, Default::default(), Default::default())
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

impl<A, AA, I: OntologyIndex<A, AA>, J: OntologyIndex<A, AA>> Ontology<A>
    for TwoIndexedOntology<A, AA, I, J>
{
}

impl<
        A: Clone,
        AA: From<AnnotatedComponent<A>> + Clone,
        I: OntologyIndex<A, AA>,
        J: OntologyIndex<A, AA>,
    > MutableOntology<A> for TwoIndexedOntology<A, AA, I, J>
{
    fn insert<IAA: Into<AnnotatedComponent<A>>>(&mut self, cmp: IAA) -> bool {
        let cmp = cmp.into();
        self.index_insert(cmp.into())
    }

    fn take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.index_take(cmp)
    }
}

impl<A: Clone, AA: Clone, I: OntologyIndex<A, AA>, J: OntologyIndex<A, AA>> OntologyIndex<A, AA>
    for TwoIndexedOntology<A, AA, I, J>
{
    fn index_insert(&mut self, cmp: AA) -> bool {
        let rtn = self.0.index_insert(cmp.clone());
        // Don't short circuit
        self.1.index_insert(cmp) || rtn
    }

    fn index_remove(&mut self, cmp: &AnnotatedComponent<A>) -> bool {
        let rtn = self.0.index_remove(cmp);
        // Don't short circuit
        self.1.index_remove(cmp) || rtn
    }
}

/// ThreeIndexedOntology supports three indexes.
#[derive(Debug, Default)]
pub struct ThreeIndexedOntology<A, AA, I, J, K>(
    TwoIndexedOntology<A, AA, I, TwoIndexedOntology<A, AA, J, K>>,
);

impl<A, AA, I: OntologyIndex<A, AA>, J: OntologyIndex<A, AA>, K: OntologyIndex<A, AA>>
    ThreeIndexedOntology<A, AA, I, J, K>
{
    pub fn new(i: I, j: J, k: K) -> Self {
        ThreeIndexedOntology(TwoIndexedOntology(
            i,
            TwoIndexedOntology(j, k, Default::default(), Default::default()),
            Default::default(),
            Default::default(),
        ))
    }

    pub fn i(&self) -> &I {
        self.0.i()
    }

    pub fn j(&self) -> &J {
        self.0.j().i()
    }

    pub fn k(&self) -> &K {
        self.0.j().j()
    }

    pub fn index(self) -> (I, J, K) {
        let index = (self.0).1.index();
        ((self.0).0, index.0, index.1)
    }
}

impl<A, AA, I: OntologyIndex<A, AA>, J: OntologyIndex<A, AA>, K: OntologyIndex<A, AA>> Ontology<A>
    for ThreeIndexedOntology<A, AA, I, J, K>
{
}

impl<
        A: Clone,
        AA: From<AnnotatedComponent<A>> + Clone,
        I: OntologyIndex<A, AA>,
        J: OntologyIndex<A, AA>,
        K: OntologyIndex<A, AA>,
    > MutableOntology<A> for ThreeIndexedOntology<A, AA, I, J, K>
{
    fn insert<IAA: Into<AnnotatedComponent<A>>>(&mut self, cmp: IAA) -> bool {
        self.0.insert(cmp)
    }

    fn take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.0.take(cmp)
    }
}

impl<
        A: Clone,
        AA: Clone,
        I: OntologyIndex<A, AA>,
        J: OntologyIndex<A, AA>,
        K: OntologyIndex<A, AA>,
    > OntologyIndex<A, AA> for ThreeIndexedOntology<A, AA, I, J, K>
{
    fn index_insert(&mut self, cmp: AA) -> bool {
        let rtn = (self.0).0.index_insert(cmp.clone());
        // Don't short cirtuit
        (self.0).1.index_insert(cmp) || rtn
    }

    fn index_remove(&mut self, cmp: &AnnotatedComponent<A>) -> bool {
        let rtn = self.0.index_remove(cmp);
        // Don't short circuit
        (self.0).1.index_remove(cmp) || rtn
    }
}

/// FourIndexedOntology supports three indexes.
#[derive(Debug, Default)]
pub struct FourIndexedOntology<A, AA, I, J, K, L>(
    TwoIndexedOntology<A, AA, I, ThreeIndexedOntology<A, AA, J, K, L>>,
);

impl<
        A,
        AA,
        I: OntologyIndex<A, AA>,
        J: OntologyIndex<A, AA>,
        K: OntologyIndex<A, AA>,
        L: OntologyIndex<A, AA>,
    > FourIndexedOntology<A, AA, I, J, K, L>
{
    pub fn new(i: I, j: J, k: K, l: L) -> Self {
        FourIndexedOntology(TwoIndexedOntology(
            i,
            ThreeIndexedOntology::new(j, k, l),
            Default::default(),
            Default::default(),
        ))
    }

    pub fn i(&self) -> &I {
        self.0.i()
    }

    pub fn j(&self) -> &J {
        self.0.j().i()
    }

    pub fn k(&self) -> &K {
        self.0.j().j()
    }

    pub fn l(&self) -> &L {
        (self.0).j().k()
    }

    pub fn index(self) -> (I, J, K, L) {
        let index = (self.0).1.index();
        ((self.0).0, index.0, index.1, index.2)
    }
}

impl<
        A,
        AA,
        I: OntologyIndex<A, AA>,
        J: OntologyIndex<A, AA>,
        K: OntologyIndex<A, AA>,
        L: OntologyIndex<A, AA>,
    > Ontology<A> for FourIndexedOntology<A, AA, I, J, K, L>
{
}

impl<
        A: Clone,
        AA: From<AnnotatedComponent<A>> + Clone,
        I: OntologyIndex<A, AA>,
        J: OntologyIndex<A, AA>,
        K: OntologyIndex<A, AA>,
        L: OntologyIndex<A, AA>,
    > MutableOntology<A> for FourIndexedOntology<A, AA, I, J, K, L>
{
    fn insert<IAA: Into<AnnotatedComponent<A>>>(&mut self, cmp: IAA) -> bool {
        self.0.insert(cmp)
    }

    fn take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.0.take(cmp)
    }
}

#[cfg(test)]
mod test {

    use super::{
        FourIndexedOntology, NullIndex, OneIndexedOntology, ThreeIndexedOntology,
        TwoIndexedOntology,
    };
    use crate::{
        model::{AnnotatedComponent, Build, MutableOntology, NamedOWLEntity, RcStr},
        ontology::set::SetIndex,
    };

    fn stuff() -> (
        AnnotatedComponent<RcStr>,
        AnnotatedComponent<RcStr>,
        AnnotatedComponent<RcStr>,
    ) {
        let b = Build::new_rc();
        let c: NamedOWLEntity<_> = b.class("http://www.example.com/c").into();
        let o: NamedOWLEntity<_> = b.object_property("http://www.example.com/p").into();
        let b: NamedOWLEntity<_> = b.data_property("http://www.example.com/d").into();

        (c.into(), o.into(), b.into())
    }

    #[test]
    fn one_cons() {
        let _o = OneIndexedOntology::new_rc(SetIndex::new());
    }

    #[test]
    fn one_insert() {
        let mut o = OneIndexedOntology::new_rc(SetIndex::new());
        let e = stuff();
        o.insert(e.0);
        o.insert(e.1);
        o.insert(e.2);

        assert_eq!(o.i().into_iter().count(), 3);
    }

    #[test]
    fn one_remove() {
        let mut o = OneIndexedOntology::new_rc(SetIndex::new());
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
        let _o = TwoIndexedOntology::from((SetIndex::new_rc(), SetIndex::new()));

        let _o = TwoIndexedOntology::from((SetIndex::new_rc(), NullIndex::default()));
    }

    #[test]
    fn two_insert() {
        let mut o = TwoIndexedOntology::new(SetIndex::new_rc(), SetIndex::new());
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
        let mut o = TwoIndexedOntology::new(SetIndex::new_rc(), SetIndex::new());

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
        let mut o = ThreeIndexedOntology::new(SetIndex::new_rc(), SetIndex::new(), SetIndex::new());

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
