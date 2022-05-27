//! Rapid, simple, in-memory `Ontology` and `OntologyIndex`
use std::{collections::HashSet, iter::FromIterator, rc::Rc};

use super::indexed::ForIndex;
use super::indexed::{OneIndexedOntology, OntologyIndex};
use crate::model::*;
use std::marker::PhantomData;

/// An Ontology backed by a set. This should be the fastest and least
/// overhead implementation of an ontology. It provides rapid testing
/// of whether an equivalent axiom exists, and is iterable.
#[derive(Debug, Eq, PartialEq)]
pub struct SetOntology<A: ForIRI>(
    OneIndexedOntology<A, AnnotatedAxiom<A>, SetIndex<A, AnnotatedAxiom<A>>>,
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

impl SetOntology<Rc<str>> {
    pub fn new_rc() -> SetOntology<Rc<str>> {
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

    pub fn from_index<AA: ForIndex<A>>(oid: OntologyID<A>, si: SetIndex<A, AA>) -> SetOntology<A> {
        (oid, si.into_iter().map(|s| s.unwrap())).into()
    }

    /// Gets an iterator that visits the annotated axioms of the ontology.
    pub fn iter(&self) -> SetIter<'_, A> {
        SetIter(self.0.i().0.iter())
    }
}

impl<A: ForIRI> Ontology<A> for SetOntology<A> {
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

/// An Interator for `SetOntology`
pub struct SetIter<'a, A: ForIRI>(std::collections::hash_set::Iter<'a, AnnotatedAxiom<A>>);

impl<'a, A: ForIRI> Iterator for SetIter<'a, A> {
    type Item = &'a AnnotatedAxiom<A>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a, A: ForIRI> IntoIterator for &'a SetOntology<A> {
    type Item = &'a AnnotatedAxiom<A>;
    type IntoIter = SetIter<'a, A>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// An owning iterator over the annotated axioms of an `Ontology`.
pub struct SetIntoIter<A: ForIRI>(std::vec::IntoIter<AnnotatedAxiom<A>>);

impl<A: ForIRI> Iterator for SetIntoIter<A> {
    type Item = AnnotatedAxiom<A>;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<A: ForIRI> IntoIterator for SetOntology<A> {
    type Item = AnnotatedAxiom<A>;
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
    fn insert<AA>(&mut self, ax: AA) -> bool
    where
        AA: Into<AnnotatedAxiom<A>>,
    {
        self.0.insert(ax)
    }

    fn remove(&mut self, ax: &AnnotatedAxiom<A>) -> bool {
        self.0.remove(ax)
    }

    fn take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        self.0.take(ax)
    }
}

impl<A: ForIRI> FromIterator<AnnotatedAxiom<A>> for SetOntology<A> {
    fn from_iter<I: IntoIterator<Item = AnnotatedAxiom<A>>>(iter: I) -> Self {
        SetOntology(OneIndexedOntology::new(SetIndex(
            HashSet::from_iter(iter),
            Default::default(),
        )))
    }
}

impl<A: ForIRI, I> From<(OntologyID<A>, I)> for SetOntology<A>
where
    I: Iterator<Item = AnnotatedAxiom<A>>,
{
    fn from((mut oid, i): (OntologyID<A>, I)) -> SetOntology<A> {
        let mut so: SetOntology<_> = i.collect();
        std::mem::swap(so.mut_id(), &mut oid);
        so
    }
}

/*
impl<A: ForIRI, O, I> From<(O, I)> for SetOntology<A>
where
    I: Iterator<Item = AnnotatedAxiom<A>>,
    O: Ontology<A>,
{
    fn from((mut o, i): (O, I)) -> SetOntology<A> {
        (o.mut_id().clone(), i).into()
    }
}
*/

/// An `OntologyIndex` implemented over an in-memory HashSet. When
/// combined with an `IndexedOntology` this should be nearly as
/// fastest as `SetOntology`.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SetIndex<A: ForIRI, AA: ForIndex<A>>(HashSet<AA>, PhantomData<A>);

impl<A: ForIRI, AA: ForIndex<A>> OntologyIndex<A, AA> for SetIndex<A, AA> {
    fn index_insert(&mut self, ax: AA) -> bool {
        self.0.insert(ax)
    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom<A>) -> bool {
        self.0.remove(ax)
    }
}

impl<A: ForIRI, AA: ForIndex<A>> SetIndex<A, AA> {
    pub fn new() -> SetIndex<A, AA> {
        SetIndex(Default::default(), Default::default())
    }

    pub fn contains(&self, ax: &AnnotatedAxiom<A>) -> bool {
        self.0.contains(ax)
    }
}

impl SetIndex<Rc<str>, Rc<AnnotatedAxiom<Rc<str>>>> {
    pub fn new_rc() -> Self {
        Self::new()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> IntoIterator for SetIndex<A, AA> {
    type Item = AnnotatedAxiom<A>;
    type IntoIter = std::vec::IntoIter<AnnotatedAxiom<A>>;
    fn into_iter(self) -> Self::IntoIter {
        #[allow(clippy::needless_collect)]
        let v: Vec<AnnotatedAxiom<_>> = self.0.into_iter().map(|fi| fi.unwrap()).collect();
        v.into_iter()
    }
}

impl<'a, A: ForIRI, AA: ForIndex<A>> IntoIterator for &'a SetIndex<A, AA> {
    type Item = &'a AnnotatedAxiom<A>;
    type IntoIter = std::vec::IntoIter<&'a AnnotatedAxiom<A>>;
    fn into_iter(self) -> Self::IntoIter {
        #[allow(clippy::needless_collect)]
        let v: Vec<&'a AnnotatedAxiom<A>> = self.0.iter().map(|fiac| fiac.borrow()).collect();
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
    fn test_ontology_id() {
        let mut so = SetOntology::new_rc();
        let b = Build::new_rc();
        let mut oid = OntologyID {
            iri: Some(b.iri("http://www.example.com/iri")),
            viri: Some(b.iri("http://www.example.com/viri")),
        };

        std::mem::swap(so.mut_id(), &mut oid);

        assert_eq!(so.id().iri, Some(b.iri("http://www.example.com/iri")));
        assert_eq!(so.id().viri, Some(b.iri("http://www.example.com/viri")))
    }

    #[test]
    fn test_ontology_clone() {
        let mut so = SetOntology::new_rc();
        let b = Build::new_rc();
        let mut oid = OntologyID {
            iri: Some(b.iri("http://www.example.com/iri")),
            viri: Some(b.iri("http://www.example.com/viri")),
        };

        std::mem::swap(so.mut_id(), &mut oid);

        let cso = so.clone();

        assert_eq!(cso.id().iri, Some(b.iri("http://www.example.com/iri")));
        assert_eq!(cso.id().viri, Some(b.iri("http://www.example.com/viri")))
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

        let newo: SetOntology<_> = (o.mut_id().clone(), o.into_iter()).into();

        // Iteration is set based so undefined in order. So, sort first.
        let mut v: Vec<_> = (&newo).into_iter().collect();
        v.sort();
        let mut it = v.into_iter();
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
