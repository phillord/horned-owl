//! Rapid, simple, in-memory `Ontology` and `OntologyIndex`
use std::{collections::HashSet, hash::Hash, iter::FromIterator, rc::Rc};

use super::indexed::ForIndex;
use super::indexed::{OneIndexedOntology, OntologyIndex};
use crate::model::*;
use std::marker::PhantomData;

/// An Ontology backed by a set. This should be the fastest and least
/// overhead implementation of an ontology. It provides rapid testing
/// of whether an equivalent component exists, and is iterable.
#[derive(Debug, Eq, PartialEq)]
pub struct SetOntology<A: ForIRI>(
    OneIndexedOntology<A, AnnotatedComponent<A>, SetIndex<A, AnnotatedComponent<A>>>,
);

impl<A: ForIRI> Clone for SetOntology<A> {
    fn clone(&self) -> Self {
        SetOntology(self.0.clone())
    }
}

impl<A: ForIRI> Default for SetOntology<A> {
    fn default() -> Self {
        SetOntology(OneIndexedOntology::new(SetIndex::new()))
    }
}

impl SetOntology<RcStr> {
    pub fn new_rc() -> SetOntology<RcStr> {
        SetOntology::new()
    }
}

impl<A: ForIRI> SetOntology<A> {
    /// Create a new ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::ontology::set::SetOntology;
    /// let o = SetOntology::new_rc();
    /// let o2 = SetOntology::new();
    ///
    /// assert_eq!(o, o2);
    /// ```
    pub fn new() -> SetOntology<A> {
        SetOntology(OneIndexedOntology::new(SetIndex::new()))
    }

    pub fn from_index(index: SetIndex<A, AnnotatedComponent<A>>) -> Self {
        SetOntology(OneIndexedOntology::new(index))
    }

    pub fn i(&self) -> &SetIndex<A, AnnotatedComponent<A>> {
        self.0.i()
    }

    /// Gets an iterator that visits the annotated components of the ontology.
    pub fn iter(&self) -> SetIter<'_, A> {
        SetIter(self.0.i().0.iter())
    }
}

impl<A: ForIRI> Ontology<A> for SetOntology<A> {}

impl<A: ForIRI, AA: ForIndex<A>> From<SetIndex<A, AA>> for SetOntology<A> {
    fn from(index: SetIndex<A, AA>) -> Self {
        // Unpack ForIndex'd entities by unwrapping and turn them into
        // direct references for SetOntology.
        let mut so = SetOntology::new();
        for c in index.into_iter() {
            so.insert(c.unwrap());
        }
        so
    }
}

/*
impl<A:ForIRI> From<SetIndex<A, AnnotatedComponent<A>>> for SetOntology<A> {
    fn from(value: SetIndex<A, AnnotatedComponent<A>>) -> Self {
        SetOntology(OneIndexedOntology::new(value))
    }
}*/

/// An Interator for `SetOntology`
pub struct SetIter<'a, A: ForIRI>(std::collections::hash_set::Iter<'a, AnnotatedComponent<A>>);

impl<'a, A: ForIRI> Iterator for SetIter<'a, A> {
    type Item = &'a AnnotatedComponent<A>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a, A: ForIRI> FusedIterator for SetIter<'a, A> {
}

impl<'a, A: ForIRI> ExactSizeIterator for SetIter<'a, A> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a, A: ForIRI> IntoIterator for &'a SetOntology<A> {
    type Item = &'a AnnotatedComponent<A>;
    type IntoIter = SetIter<'a, A>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// An owning iterator over the annotated components of an `Ontology`.
pub struct SetIntoIter<A: ForIRI>(std::vec::IntoIter<AnnotatedComponent<A>>);

impl<A: ForIRI> Iterator for SetIntoIter<A> {
    type Item = AnnotatedComponent<A>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<A: ForIRI> FusedIterator for SetIntoIter<A> {
}

impl<A: ForIRI> ExactSizeIterator for SetIntoIter<A> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<A: ForIRI> IntoIterator for SetOntology<A> {
    type Item = AnnotatedComponent<A>;
    type IntoIter = SetIntoIter<A>;
    fn into_iter(self) -> Self::IntoIter {
        SetIntoIter(self.0.index().into_iter())
    }
}

impl<A: ForIRI> MutableOntology<A> for SetOntology<A> {
    /// Insert an axiom into the ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// # use horned_owl::ontology::set::SetOntology;
    /// let mut o = SetOntology::new();
    /// let b = Build::new_rc();
    /// o.insert(DeclareClass(b.class("http://www.example.com/a")));
    /// o.insert(DeclareObjectProperty(b.object_property("http://www.example.com/r")));
    /// ```
    ///
    /// See `declare` for an easier way to declare named entities.
    fn insert<AA>(&mut self, cmp: AA) -> bool
    where
        AA: Into<AnnotatedComponent<A>>,
    {
        self.0.insert(cmp)
    }

    fn remove(&mut self, cmp: &AnnotatedComponent<A>) -> bool {
        self.0.remove(cmp)
    }

    fn take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.0.take(cmp)
    }
}

impl<A: ForIRI> FromIterator<AnnotatedComponent<A>> for SetOntology<A> {
    fn from_iter<I: IntoIterator<Item = AnnotatedComponent<A>>>(iter: I) -> Self {
        iter.into_iter().collect::<MutableOntologyWrapper<SetOntology<_>>>().0
    }
}

impl<A: ForIRI> Extend<AnnotatedComponent<A>> for SetOntology<A> {
    fn extend<T: IntoIterator<Item = AnnotatedComponent<A>>>(&mut self, iter: T) {
        MutableOntologyWrapper(self).extend(iter.into_iter());
    }
}

impl<A: ForIRI, I> From<I> for SetOntology<A>
where
    I: Iterator<Item = AnnotatedComponent<A>>,
{
    fn from(i: I) -> SetOntology<A> {

        let mut so = SetOntology::new();
        for c in i {
            so.insert(c);
        }
        so
    }
}

/// An `OntologyIndex` implemented over an in-memory HashSet. When
/// combined with an `IndexedOntology` this should be nearly as
/// fastest as `SetOntology`.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct SetIndex<A, AA: Hash + Eq>(HashSet<AA>, PhantomData<A>);

impl<A: ForIRI, AA: ForIndex<A>> OntologyIndex<A, AA> for SetIndex<A, AA> {
    fn index_insert(&mut self, cmp: AA) -> bool {
        self.0.insert(cmp)
    }

    fn index_remove(&mut self, cmp: &AnnotatedComponent<A>) -> bool {
        self.0.remove(cmp)
    }
}

impl<A, AA: Hash + Eq> Default for SetIndex<A, AA> {
    fn default() -> Self {
        SetIndex(Default::default(), Default::default())
    }
}

impl<A: ForIRI, AA: ForIndex<A>> SetIndex<A, AA> {
    pub fn new() -> SetIndex<A, AA> {
        SetIndex(Default::default(), Default::default())
    }

    pub fn contains(&self, cmp: &AnnotatedComponent<A>) -> bool {
        self.0.contains(cmp)
    }

    pub fn the_ontology_id(&self) -> Option<OntologyID<A>> {
        self.0
            .iter()
            .filter_map(|item| match &item.borrow().component {
                Component::OntologyID(id) => Some(id),
                _ => None,
            })
            .next()
            .cloned()
    }

    pub fn the_ontology_id_or_default(&self) -> OntologyID<A> {
        self.the_ontology_id().unwrap_or_default()
    }
}

impl SetIndex<RcStr, Rc<AnnotatedComponent<RcStr>>> {
    pub fn new_rc() -> Self {
        Self::new()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> IntoIterator for SetIndex<A, AA> {
    type Item = AnnotatedComponent<A>;
    type IntoIter = std::vec::IntoIter<AnnotatedComponent<A>>;
    fn into_iter(self) -> Self::IntoIter {
        #[allow(clippy::needless_collect)]
        let v: Vec<AnnotatedComponent<_>> = self.0.into_iter().map(|fi| fi.unwrap()).collect();
        v.into_iter()
    }
}

impl<'a, A: ForIRI, AA: ForIndex<A>> IntoIterator for &'a SetIndex<A, AA> {
    type Item = &'a AnnotatedComponent<A>;
    type IntoIter = std::vec::IntoIter<&'a AnnotatedComponent<A>>;
    fn into_iter(self) -> Self::IntoIter {
        #[allow(clippy::needless_collect)]
        let v: Vec<&'a AnnotatedComponent<A>> = self.0.iter().map(|fiac| fiac.borrow()).collect();
        v.into_iter()
    }
}

#[cfg(test)]
mod test {
    use super::{SetIndex, SetOntology};
    use crate::{model::*, ontology::indexed::OneIndexedOntology};

    #[test]
    fn test_ontology_cons() {
        let _ = SetOntology::new_rc();
        assert!(true);
    }

    #[test]
    fn
        test_ontology_id() {
        let mut so = SetOntology::new_rc();
        let b = Build::new_rc();
        let oid = OntologyID {
            iri: Some(b.iri("http://www.example.com/iri")),
            viri: Some(b.iri("http://www.example.com/viri")),
        };

        so.insert(oid.clone());

        let so_id = so.i().the_ontology_id_or_default();
        assert_eq!(so_id.iri, Some(b.iri("http://www.example.com/iri")));
        assert_eq!(so_id.viri, Some(b.iri("http://www.example.com/viri")));
    }

    #[test]
    fn test_ontology_clone() {
        let mut so = SetOntology::new_rc();
        let b = Build::new_rc();
        let oid = OntologyID {
            iri: Some(b.iri("http://www.example.com/iri")),
            viri: Some(b.iri("http://www.example.com/viri")),
        };

        so.insert(oid.clone());

        let cso = so.clone();
        assert_eq!(cso.i().the_ontology_id_or_default(), oid);
    }

    #[test]
    fn test_ontology_iter_empty() {
        // Empty ontologies should stop iteration right away
        let mut it = SetOntology::new_rc().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_into_iter() {
        // Setup
        let build = Build::new_rc();
        let mut o = SetOntology::new();
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
        let mut v: Vec<_> = o.into_iter().collect();
        v.sort();
        let mut it = v.into_iter();
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DeclareClass(decl1)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DeclareClass(decl2)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DeclareClass(decl3)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DisjointClasses(disj1)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DisjointClasses(disj2)))
        );
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_into_iter_empty() {
        // Empty ontologies should stop iteration right away
        let o = SetOntology::new_rc();
        let mut it = (&o).into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_iter() {
        // Setup
        let build = Build::new_rc();
        let mut o = SetOntology::new();
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

        // Iteration is set based so undefined in order. So, sort first.
        let mut v: Vec<_> = (&o).into_iter().collect();
        v.sort();
        let mut it = v.into_iter();
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl1)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl2)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl3)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DisjointClasses(disj1)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DisjointClasses(disj2)))
        );
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_into() {
        // Setup
        let build = Build::new_rc();
        let mut o = SetOntology::new();
        let decl1 = DeclareClass(build.class("http://www.example.com#a"));
        let decl2 = DeclareClass(build.class("http://www.example.com#b"));
        let decl3 = DeclareClass(build.class("http://www.example.com#c"));

        o.insert(decl1.clone());
        o.insert(decl2.clone());
        o.insert(decl3.clone());

        let newo: SetOntology<_> = o.into_iter().into();

        // Iteration is set based so undefined in order. So, sort first.
        let mut v: Vec<_> = (&newo).into_iter().collect();
        v.sort();
        let mut it = v.into_iter();
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl1)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl2)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl3)))
        );
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_index_cons() {
        let _ = SetIndex::new_rc();
        assert!(true);
    }

    #[test]
    fn test_index_iter_empty() {
        // Empty ontologies should stop iteration right away
        let mut it = SetIndex::new_rc().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_index_into_iter() {
        // Setup
        let build = Build::new_rc();
        let mut o = OneIndexedOntology::new(SetIndex::new_rc());
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
        let mut v: Vec<_> = o.index().into_iter().collect();
        v.sort();
        let mut it = v.into_iter();
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DeclareClass(decl1)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DeclareClass(decl2)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DeclareClass(decl3)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DisjointClasses(disj1)))
        );
        assert_eq!(
            it.next(),
            Some(AnnotatedComponent::from(Component::DisjointClasses(disj2)))
        );
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_index_into_iter_empty() {
        // Empty ontologies should stop iteration right away
        let o = OneIndexedOntology::new(SetIndex::new_rc());
        let mut it = o.i().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_index_iter() {
        // Setup
        let build = Build::new_rc();
        let mut o = OneIndexedOntology::new(SetIndex::new_rc());
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

        // Iteration is set based so undefined in order. So, sort first.
        let mut v: Vec<_> = o.i().into_iter().collect();
        v.sort();
        let mut it = v.into_iter();
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl1)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl2)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DeclareClass(decl3)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DisjointClasses(disj1)))
        );
        assert_eq!(
            it.next(),
            Some(&AnnotatedComponent::from(Component::DisjointClasses(disj2)))
        );
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }
}
