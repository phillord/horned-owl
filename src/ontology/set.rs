//! Rapid, simple, in-memory `Ontology` and `OntologyIndex`
use std::{collections::HashSet, iter::FromIterator, rc::Rc};

use super::indexed::{rc_unwrap_or_clone, OntologyIndex};
use crate::model::*;

/// An Ontology backed by a set. This should be the fastest and least
/// overhead implementation of an ontology. It provides rapid testing
/// of whether an equivalent axiom exists, and is iterable.
///
/// It should be more rapid that the using `SetIndex` inside
/// `OneIndexedOntology`, as it involves no `Rc` overhead.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct SetOntology {
    id: OntologyID,
    // The use an BTreeMap keyed on AxiomKind allows efficient
    // retrieval of axioms. Otherwise, we'd have to iterate through
    // the lot every time.
    axiom: HashSet<AnnotatedAxiom>,
    doc_iri: Option<IRI>
}

impl SetOntology {
    /// Create a new ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::ontology::set::SetOntology;
    /// let o = SetOntology::new();
    /// let o2 = SetOntology::new();
    ///
    /// assert_eq!(o, o2);
    /// ```
    pub fn new() -> SetOntology {
        SetOntology::default()
    }

    /// Gets an iterator that visits the annotated axioms of the ontology.
    pub fn iter(&self) -> SetIter<'_> {
        SetIter(self.axiom.iter())
    }
}

impl Ontology for SetOntology {
    fn id(&self) -> &OntologyID {
        &self.id
    }

    fn mut_id(&mut self) -> &mut OntologyID {
        &mut self.id
    }

    fn doc_iri(&self) -> &Option<IRI> {
        &self.doc_iri
    }

    fn mut_doc_iri(&mut self) -> &Option<IRI>{
        &mut self.doc_iri
    }
}

/// An Interator for `SetOntology`
pub struct SetIter<'a>(std::collections::hash_set::Iter<'a, AnnotatedAxiom>);

impl<'a> Iterator for SetIter<'a> {
    type Item = &'a AnnotatedAxiom;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> IntoIterator for &'a SetOntology {
    type Item = &'a AnnotatedAxiom;
    type IntoIter = SetIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// An owning iterator over the annotated axioms of an `Ontology`.
pub struct SetIntoIter(std::collections::hash_set::IntoIter<AnnotatedAxiom>);

impl Iterator for SetIntoIter {
    type Item = AnnotatedAxiom;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl IntoIterator for SetOntology {
    type Item = AnnotatedAxiom;
    type IntoIter = SetIntoIter;
    fn into_iter(self) -> Self::IntoIter {
        SetIntoIter(self.axiom.into_iter())
    }
}

impl MutableOntology for SetOntology {
    /// Insert an axiom into the ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// # use horned_owl::ontology::set::SetOntology;
    /// let mut o = SetOntology::new();
    /// let b = Build::new();
    /// o.insert(DeclareClass(b.class("http://www.example.com/a")));
    /// o.insert(DeclareObjectProperty(b.object_property("http://www.example.com/r")));
    /// ```
    ///
    /// See `declare` for an easier way to declare named entities.
    fn insert<A>(&mut self, ax: A) -> bool
    where
        A: Into<AnnotatedAxiom>,
    {
        self.axiom.insert(ax.into())
    }

    fn remove(&mut self, ax: &AnnotatedAxiom) -> bool {
        self.axiom.remove(ax)
    }

    fn take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.axiom.take(ax)
    }
}

impl FromIterator<AnnotatedAxiom> for SetOntology {
    fn from_iter<I: IntoIterator<Item = AnnotatedAxiom>>(iter: I) -> Self {
        SetOntology {
            id: Default::default(),
            axiom: HashSet::from_iter(iter),
            ..Default::default()
        }
    }
}

impl<I> From<(OntologyID, I)> for SetOntology
where I:Iterator<Item=AnnotatedAxiom>
{
    fn from((mut oid,i):(OntologyID, I)) -> SetOntology {
        let mut so:SetOntology = i.collect();
        std::mem::swap(so.mut_id(), &mut oid);
        so
    }
}

impl<O, I> From<(O, I)> for SetOntology
where I:Iterator<Item=AnnotatedAxiom>,
      O: Ontology
{
    fn from((mut o,i):(O, I)) -> SetOntology {
        (o.mut_id().clone(), i).into()
    }
}


/// An `OntologyIndex` implemented over an in-memory HashSet. When
/// combined with an `IndexedOntology` this should be nearly as
/// fastest as `SetOntology`.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct SetIndex(HashSet<Rc<AnnotatedAxiom>>);

impl OntologyIndex for SetIndex {
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom>) -> bool {
        self.0.insert(ax)
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.0.take(ax).map(rc_unwrap_or_clone)
    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom) -> bool {
        self.0.remove(ax)
    }
}

impl SetIndex {
    pub fn contains(&self, ax:&AnnotatedAxiom) -> bool {
        self.0.contains(ax)
    }
}

impl IntoIterator for SetIndex {
    type Item = AnnotatedAxiom;
    type IntoIter = std::vec::IntoIter<AnnotatedAxiom>;
    fn into_iter(self) -> Self::IntoIter {
        let v: Vec<AnnotatedAxiom> = self
            .0
            .into_iter()
            .map(Rc::try_unwrap)
            .map(Result::unwrap)
            .collect();
        v.into_iter()
    }
}

impl<'a> IntoIterator for &'a SetIndex {
    type Item = &'a AnnotatedAxiom;
    type IntoIter = std::vec::IntoIter<&'a AnnotatedAxiom>;
    fn into_iter(self) -> Self::IntoIter {
        let v: Vec<&'a AnnotatedAxiom> = self.0.iter().map(|rcax| &**rcax).collect();
        v.into_iter()
    }
}

#[cfg(test)]
mod test {
    use super::{SetIndex, SetOntology};
    use crate::{model::*, ontology::indexed::OneIndexedOntology};

    #[test]
    fn test_ontology_cons() {
        let _ = SetOntology::new();
        assert!(true);
    }

    #[test]
    fn test_ontology_iter_empty() {
        // Empty ontologies should stop iteration right away
        let mut it = SetOntology::new().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_into_iter() {
        // Setup
        let build = Build::new();
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
        let o = SetOntology::new();
        let mut it = (&o).into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_iter() {
        // Setup
        let build = Build::new();
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
        let build = Build::new();
        let mut o = SetOntology::new();
        let decl1 = DeclareClass(build.class("http://www.example.com#a"));
        let decl2 = DeclareClass(build.class("http://www.example.com#b"));
        let decl3 = DeclareClass(build.class("http://www.example.com#c"));

        o.insert(decl1.clone());
        o.insert(decl2.clone());
        o.insert(decl3.clone());

        let newo: SetOntology = (o.mut_id().clone(), o.into_iter()).into();

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
        let _ = SetIndex::default();
        assert!(true);
    }

    #[test]
    fn test_index_iter_empty() {
        // Empty ontologies should stop iteration right away
        let mut it = SetIndex::default().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_index_into_iter() {
        // Setup
        let build = Build::new();
        let mut o = OneIndexedOntology::new(SetIndex::default());
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
        let o = OneIndexedOntology::new(SetIndex::default());
        let mut it = o.i().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_index_iter() {
        // Setup
        let build = Build::new();
        let mut o = OneIndexedOntology::new(SetIndex::default());
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
