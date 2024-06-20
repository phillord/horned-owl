//! Transforms Ontology to a normalized representation

//! # Overview
//!
//! In OWL2 it is possible to represent logically equivalent
//! ontologies in more than one way, either through variations in metadata or the different
//! uses of axioms.
//!
//! This module provides methods to standardize these to a singe representation.
use crate::{
    model::{AnnotatedComponent, AnonymousIndividual, Build, ComponentKind, ForIRI, Kinded},
    visitor::mutable::{VisitMut, WalkMut},
};

pub fn normalize<A: ForIRI>(o: Vec<AnnotatedComponent<A>>) -> Vec<AnnotatedComponent<A>> {
    let mut o = reanonymize(simplify(o));
    o.sort();
    o
}

pub fn normalize_and_compare<A: ForIRI>(
    o1: Vec<AnnotatedComponent<A>>,
    o2: Vec<AnnotatedComponent<A>>,
) -> bool {
    normalize(o1).eq(&normalize(o2))
}

pub fn normalize_and_assert_eq<A: ForIRI>(
    o1: Vec<AnnotatedComponent<A>>,
    o2: Vec<AnnotatedComponent<A>>,
) {
    assert_eq!(normalize(o1), normalize(o2))
}

pub fn simplify<A: ForIRI>(o: Vec<AnnotatedComponent<A>>) -> Vec<AnnotatedComponent<A>> {
    o.into_iter()
        .filter(|ac| ac.kind() != ComponentKind::DocIRI)
        .collect()
}

struct Reanonymize<A: ForIRI> {
    count: usize,
    b: Build<A>,
}

impl<A: ForIRI> Reanonymize<A> {
    fn new(b: Build<A>) -> Self {
        Reanonymize { count: 0, b }
    }
}

impl<A: ForIRI> VisitMut<A> for Reanonymize<A> {
    fn visit_anonymous_individual(&mut self, ai: &mut AnonymousIndividual<A>) {
        self.count += 1;
        std::mem::swap(ai, &mut self.b.anon(&format!("anon_{}", self.count)))
    }
}

pub fn reanonymize<A: ForIRI>(mut o: Vec<AnnotatedComponent<A>>) -> Vec<AnnotatedComponent<A>> {
    let mut walk: WalkMut<A, _> = WalkMut::new(Reanonymize::new(Build::new()));

    walk.ontology_vec(&mut o);

    o
}

#[cfg(test)]
mod test {
    use crate::{
        model::{
            AnnotatedComponent, Build, DeclareClass, DeclareObjectProperty, DocIRI, SameIndividual,
        },
        normalize::{normalize_and_assert_eq, reanonymize, simplify},
    };

    #[test]
    fn reorder() {
        let b = Build::new_rc();

        let v: Vec<AnnotatedComponent<_>> = vec![
            DeclareClass(b.class("http://example.com/c")).into(),
            DeclareObjectProperty(b.object_property("http://www.example.com/op")).into(),
        ];

        let v1 = v.clone().into_iter().rev().collect();

        assert_ne!(v, v1);

        normalize_and_assert_eq(v, v1);
    }

    #[test]
    fn doc_iri() {
        let b = Build::new_rc();

        let v = vec![DocIRI(b.iri("http://example.com")).into()];

        let v = simplify(v);

        assert_eq!(v.len(), 0);
    }

    #[test]
    fn anon() {
        let b = Build::new_rc();

        let v = vec![SameIndividual(vec![b.anon("a1").into(), b.anon("a2").into()]).into()];

        let v2 = vec![SameIndividual(vec![b.anon("x1").into(), b.anon("x2").into()]).into()];

        assert_ne!(v, v2);

        let v = reanonymize(v);
        let v2 = reanonymize(v2);

        assert_eq!(v, v2);
    }
}
