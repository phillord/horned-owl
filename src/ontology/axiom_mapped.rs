//! Access `AnnotatedAxiom` by their kind.

//! # Overview
//!
//! This module provides an `AxiomMappedIndex` which provides rapid
//! access to all axioms of a given type.
//!
//! As well as being iterable, it provides `axiom` and
//! `axiom_for_kind` which iterate over a particular `AxiomKind` of
//! `Axiom` or `AnnotatedAxiom`, or methods such as `sub_class_of`, or
//! `object_property_domain` which iterate over `SubClassOf` or
//! `ObjectPropertyDomain` axioms respectively.
use super::indexed::ForIndex;
use super::set::SetOntology;
use crate::model::*;
use std::{
    cell::RefCell,
    collections::{BTreeMap, BTreeSet, VecDeque},
    rc::Rc,
    sync::Arc,
};

use std::marker::PhantomData;

use super::indexed::{OneIndexedOntology, OntologyIndex};

/// Return all axioms of a specific `AxiomKind`
#[allow(unused_macros)]
macro_rules! on {
    ($ont:ident, $kind:ident) => {
        $ont.axiom(AxiomKind::$kind).map(|ax| match ax {
            Axiom::$kind(n) => n,
            _ => panic!(),
        })
    };
}

/// Add a method to `Ontology` which returns axioms of a specific
/// `AxiomKind`.
#[allow(unused_macros)]
macro_rules! onimpl {
    ($kind:ident, $method:ident) => {
        onimpl!($kind, $method, stringify!($kind));
    };
    ($kind:ident, $method:ident, $skind:expr) => {
        impl<A: ForIRI, AA: ForIndex<A>> AxiomMappedIndex<A, AA> {
            #[doc = "Return all instances of"]
            #[doc = $skind]
            #[doc = "in the ontology."]
            pub fn $method(&self) -> impl Iterator<Item = &$kind<A>> {
                on!(self, $kind)
            }
        }
    };
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct AxiomMappedIndex<A, AA> {
    axiom: RefCell<BTreeMap<AxiomKind, BTreeSet<AA>>>,
    pd: PhantomData<A>,
}

impl<A: ForIRI, AA: ForIndex<A>> AxiomMappedIndex<A, AA> {
    /// Create a new ontology.
    ///
    /// # Examples
    /// ```
    /// # use std::rc::Rc;
    /// # use horned_owl::ontology::axiom_mapped::AxiomMappedOntology;
    /// let o = AxiomMappedOntology::new_rc();
    /// let o2 = AxiomMappedOntology::new_rc();
    ///
    /// assert_eq!(o, o2);
    /// ```
    pub fn new() -> AxiomMappedIndex<A, AA> {
        AxiomMappedIndex {
            axiom: RefCell::new(BTreeMap::new()),
            pd: Default::default(),
        }
    }

    /// Fetch the axioms hashmap as a raw pointer.
    ///
    /// This method also ensures that the BTreeSet for `axk` is
    /// instantiated, which means that it effects equality of the
    /// ontology. It should only be used where the intention is to
    /// update the ontology.
    fn axioms_as_ptr(&self, axk: AxiomKind) -> *mut BTreeMap<AxiomKind, BTreeSet<AA>> {
        self.axiom
            .borrow_mut()
            .entry(axk)
            .or_insert_with(BTreeSet::new);
        self.axiom.as_ptr()
    }

    /// Fetch the axioms for the given kind.
    fn set_for_kind(&self, axk: AxiomKind) -> Option<&BTreeSet<AA>> {
        unsafe { (*self.axiom.as_ptr()).get(&axk) }
    }

    /// Fetch the axioms for given kind as a mutable ref.
    fn mut_set_for_kind(&mut self, axk: AxiomKind) -> &mut BTreeSet<AA> {
        unsafe { (*self.axioms_as_ptr(axk)).get_mut(&axk).unwrap() }
    }

    /// Gets an iterator that visits the annotated axioms of the ontology.
    pub fn iter(&self) -> AxiomMappedIter<A, AA> {
        // TODO -- what can't this just use flat_map?
        AxiomMappedIter {
            ont: self,
            inner: None,
            kinds: unsafe { (*self.axiom.as_ptr()).keys().collect() },
        }
    }

    /// Fetch the AnnotatedAxiom for a given kind
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// # use horned_owl::ontology::axiom_mapped::AxiomMappedOntology;
    /// let mut o = AxiomMappedOntology::new_rc();
    /// let b = Build::new_rc();
    /// o.declare(b.class("http://www.example.com/a"));
    /// o.declare(b.object_property("http://www.example.com/r"));
    ///
    /// assert_eq!(o.i().axiom_for_kind(AxiomKind::DeclareClass).count(), 1);
    /// ```
    ///
    /// See also `axiom` for access to the `Axiom` without annotations.
    pub fn axiom_for_kind(&self, axk: AxiomKind) -> impl Iterator<Item = &AnnotatedAxiom<A>> {
        self.set_for_kind(axk)
            // Iterate over option
            .into_iter()
            // flatten option iterator!
            .flat_map(|hs| hs.iter())
            .map(|fi| fi.borrow())
    }

    /// Fetch the Axiom for a given kind
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// # use horned_owl::ontology::axiom_mapped::AxiomMappedOntology;
    /// let mut o = AxiomMappedOntology::new_rc();
    /// let b = Build::new_rc();
    /// o.declare(b.class("http://www.example.com/a"));
    /// o.declare(b.object_property("http://www.example.com/r"));
    ///
    /// assert_eq!(o.i().axiom(AxiomKind::DeclareClass).count(), 1);
    /// ```
    ///
    /// See methods such as `declare_class` for access to the Axiom
    /// struct directly.
    pub fn axiom(&self, axk: AxiomKind) -> impl Iterator<Item = &Axiom<A>> {
        self.axiom_for_kind(axk).map(|ann| &ann.axiom)
    }
}
// In the ideal world, we would have generated these onimpl! calls as
// part of the axiom macro. This should be possible, as their is a
// fixed relationship between the struct name and the method name.
// But rust won't let us generate new identifiers or make string like
// manipulations on the them. So we can't.
//
// "Whoever does not understand LISP is doomed to reinvent it" (badly)
onimpl! {Import, import}
onimpl! {OntologyAnnotation, ontology_annotation}
onimpl! {DeclareClass, declare_class}
onimpl! {DeclareObjectProperty, declare_object_property}
onimpl! {DeclareAnnotationProperty, declare_annotation_property}
onimpl! {DeclareDataProperty, declare_data_property}
onimpl! {DeclareNamedIndividual, declare_named_individual}
onimpl! {DeclareDatatype, declare_datatype}
onimpl! {SubClassOf, sub_class_of}
onimpl! {EquivalentClasses, equivalent_class}
onimpl! {DisjointClasses, disjoint_class}
onimpl! {DisjointUnion, disjoint_union}
onimpl! {SubObjectPropertyOf, sub_object_property_of}
onimpl! {EquivalentObjectProperties, equivalent_object_properties}
onimpl! {DisjointObjectProperties, disjoint_object_properties}
onimpl! {InverseObjectProperties, inverse_object_properties}
onimpl! {ObjectPropertyDomain, object_property_domain}
onimpl! {ObjectPropertyRange, object_property_range}
onimpl! {FunctionalObjectProperty, functional_object_property}
onimpl! {InverseFunctionalObjectProperty, inverse_functional_object_property}
onimpl! {ReflexiveObjectProperty, reflexive_object_property}
onimpl! {IrreflexiveObjectProperty, irreflexive_object_property}
onimpl! {SymmetricObjectProperty, symmetric_object_property}
onimpl! {AsymmetricObjectProperty, assymmetric_object_property}
onimpl! {TransitiveObjectProperty, transitive_object_property}
onimpl! {SubDataPropertyOf, sub_data_property_of}
onimpl! {EquivalentDataProperties, equivalent_data_properties}
onimpl! {DisjointDataProperties, disjoint_data_properties}
onimpl! {DataPropertyDomain, data_property_domain}
onimpl! {DataPropertyRange, data_property_range}
onimpl! {FunctionalDataProperty, functional_data_property}
onimpl! {DatatypeDefinition, datatype_definition}
onimpl! {HasKey, has_key}
onimpl! {SameIndividual, same_individual}
onimpl! {DifferentIndividuals, different_individuals}
onimpl! {ClassAssertion, class_assertion}
onimpl! {ObjectPropertyAssertion, object_property_assertion}
onimpl! {NegativeObjectPropertyAssertion, negative_object_property_assertion}
onimpl! {DataPropertyAssertion, data_property_assertion}
onimpl! {NegativeDataPropertyAssertion, negative_data_property_assertion}
onimpl! {AnnotationAssertion, annotation_assertion}
onimpl! {SubAnnotationPropertyOf, sub_annotation_property_of}
onimpl! {AnnotationPropertyDomain, annotation_property_domain}
onimpl! {AnnotationPropertyRange, annotation_property_range}

/// An owning iterator over the annotated axioms of an `Ontology`.
impl<A: ForIRI, AA: ForIndex<A>> IntoIterator for AxiomMappedIndex<A, AA> {
    type Item = AnnotatedAxiom<A>;
    type IntoIter = std::vec::IntoIter<AnnotatedAxiom<A>>;
    fn into_iter(self) -> Self::IntoIter {
        let btreemap = self.axiom.into_inner();

        // The collect switches the type which shows up in the API. Blegh.
        #[allow(clippy::needless_collect)]
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
pub struct AxiomMappedIter<'a, A: ForIRI, AA: ForIndex<A>> {
    ont: &'a AxiomMappedIndex<A, AA>,
    kinds: VecDeque<&'a AxiomKind>,
    inner: Option<<&'a BTreeSet<AA> as IntoIterator>::IntoIter>,
}

impl<'a, A: ForIRI, AA: ForIndex<A>> Iterator for AxiomMappedIter<'a, A, AA> {
    type Item = &'a AnnotatedAxiom<A>;
    fn next(&mut self) -> Option<Self::Item> {
        // Consume the current iterator if there are items left.
        if let Some(ref mut it) = self.inner {
            if let Some(axiom) = it.next() {
                return Some(axiom.borrow());
            }
        }
        // Attempt to consume the iterator for the next axiom kind
        if !self.kinds.is_empty() {
            let kind = self.kinds.pop_front().unwrap();
            self.inner = self.ont.set_for_kind(*kind).map(BTreeSet::iter);
            self.next().map(|rc| &*rc)
        } else {
            None
        }
    }
}

impl<'a, A: ForIRI, AA: ForIndex<A>> IntoIterator for &'a AxiomMappedIndex<A, AA> {
    type Item = &'a AnnotatedAxiom<A>;
    type IntoIter = AxiomMappedIter<'a, A, AA>;
    fn into_iter(self) -> Self::IntoIter {
        AxiomMappedIter {
            ont: self,
            inner: None,
            kinds: unsafe { (*self.axiom.as_ptr()).keys().collect() },
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> OntologyIndex<A, AA> for AxiomMappedIndex<A, AA> {
    fn index_insert(&mut self, ax: AA) -> bool {
        self.mut_set_for_kind(ax.borrow().kind()).insert(ax)
    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom<A>) -> bool {
        self.mut_set_for_kind(ax.kind()).remove(ax)
    }
}

#[derive(Default, Debug, Eq, PartialEq)]
pub struct AxiomMappedOntology<A, AA>(OneIndexedOntology<A, AA, AxiomMappedIndex<A, AA>>);

pub type RcAxiomMappedOntology = AxiomMappedOntology<RcStr, Rc<AnnotatedAxiom<RcStr>>>;
pub type ArcAxiomMappedOntology = AxiomMappedOntology<ArcStr, Arc<AnnotatedAxiom<ArcStr>>>;

impl<A: ForIRI, AA: ForIndex<A>> Ontology<A> for AxiomMappedOntology<A, AA> {
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

impl<A: ForIRI, AA: ForIndex<A>> MutableOntology<A> for AxiomMappedOntology<A, AA> {
    fn insert<IAA>(&mut self, ax: IAA) -> bool
    where
        IAA: Into<AnnotatedAxiom<A>>,
    {
        self.0.insert(ax)
    }

    fn take(&mut self, ax: &AnnotatedAxiom<A>) -> Option<AnnotatedAxiom<A>> {
        self.0.take(ax)
    }
}

impl<A: ForIRI, AA: ForIndex<A>> AxiomMappedOntology<A, AA> {
    pub fn index(self) -> AxiomMappedIndex<A, AA> {
        self.0.index()
    }

    pub fn i(&self) -> &AxiomMappedIndex<A, AA> {
        self.0.i()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> AxiomMappedOntology<A, AA> {
    pub fn new() -> AxiomMappedOntology<A, AA> {
        AxiomMappedOntology(OneIndexedOntology::new(AxiomMappedIndex::new()))
    }
}

impl RcAxiomMappedOntology {
    pub fn new_rc() -> Self {
        AxiomMappedOntology::new()
    }
}

impl ArcAxiomMappedOntology {
    pub fn new_arc() -> Self {
        AxiomMappedOntology::new()
    }
}

/// An owning iterator over the annotated axioms of an `Ontology`.
impl<A: ForIRI, AA: ForIndex<A>> IntoIterator for AxiomMappedOntology<A, AA> {
    type Item = AnnotatedAxiom<A>;
    type IntoIter = std::vec::IntoIter<AnnotatedAxiom<A>>;
    fn into_iter(self) -> Self::IntoIter {
        self.index().into_iter()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<SetOntology<A>> for AxiomMappedOntology<A, AA> {
    fn from(mut so: SetOntology<A>) -> AxiomMappedOntology<A, AA> {
        let mut amo = AxiomMappedOntology::new();
        std::mem::swap(amo.mut_id(), so.mut_id());
        for ax in so {
            amo.insert(ax);
        }
        amo
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<AxiomMappedOntology<A, AA>> for SetOntology<A> {
    fn from(mut amo: AxiomMappedOntology<A, AA>) -> SetOntology<A> {
        let mut so = SetOntology::new();
        std::mem::swap(so.mut_id(), amo.mut_id());

        for ax in amo {
            so.insert(ax);
        }
        so
    }
}

#[cfg(test)]
mod test {
    use super::AxiomMappedOntology;
    use super::RcAxiomMappedOntology;
    use crate::model::*;
    use crate::ontology::set::SetOntology;

    #[test]
    fn test_ontology_cons() {
        let _ = AxiomMappedOntology::new_rc();
        assert!(true);
    }

    #[test]
    fn test_ontology_iter_empty() {
        // Empty ontologies should stop iteration right away
        let mut it = AxiomMappedOntology::new_rc().into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn from_set() {
        let b = Build::new_rc();
        let mut so = SetOntology::new();
        let mut oid = OntologyID {
            iri: Some(b.iri("http://www.example.com/iri")),
            viri: Some(b.iri("http://www.example.com/viri")),
        };

        std::mem::swap(so.mut_id(), &mut oid);
        assert_eq!(so.id().iri, Some(b.iri("http://www.example.com/iri")));
        assert_eq!(so.id().viri, Some(b.iri("http://www.example.com/viri")));

        let amo: RcAxiomMappedOntology = so.into();

        assert_eq!(amo.id().iri, Some(b.iri("http://www.example.com/iri")));
        assert_eq!(amo.id().viri, Some(b.iri("http://www.example.com/viri")))
    }

    #[test]
    fn test_ontology_into_iter() {
        // Setup
        let build = Build::new();
        let mut o = AxiomMappedOntology::new_rc();
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
        let o = AxiomMappedOntology::new_rc();
        let mut it = o.into_iter();
        assert_eq!(it.next(), None);
        assert_eq!(it.next(), None);
    }

    #[test]
    fn test_ontology_iter() {
        // Setup
        let build = Build::new_rc();
        let mut o = AxiomMappedOntology::new_rc();
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
