//! Search facilities for Ontologies
//!
//! This library provides simple search facilities.
//!
//! It is currently being deprecated in favour of [`OntologyIndex`](../ontology/indexed/OntologyIndex.html)
use crate::io::rdf::reader::RDFOntology;

use crate::model::*;

pub fn find_logically_equal_axiom<'a>(
    o: &'a RDFOntology,
    axiom: &AnnotatedAxiom,
) -> Option<&'a AnnotatedAxiom> {
    // Find any axiom in Ontology which is the same as AnnotatedAxiom,
    // ignoring the Annotations
    o.i()
        .annotated_axiom(axiom.kind())
        .find(|ax| ax.logical_eq(axiom))
}

// Find an axiom which is logically equal and merge it's annotations
pub fn update_logically_equal_axiom<'a>(o: &mut RDFOntology, mut axiom: AnnotatedAxiom) {
    let some_eq_axiom = find_logically_equal_axiom(o, &axiom);

    if let Some(eq_axiom) = some_eq_axiom.cloned() {
        let mut taken_axiom = o.take(&eq_axiom).unwrap();
        axiom.ann.append(&mut taken_axiom.ann);
    }

    o.insert(axiom);
}

#[cfg(test)]
mod test {
    use super::*;
    //use crate::model::*;

    #[test]
    fn test_find_equal_axiom() {
        let b = Build::new();
        let mut o = RDFOntology::default();

        let c = b.class("http://www.example.com");
        o.declare(c);

        let ne: NamedEntity = b.class("http://www.example.com").into();
        let ax: Axiom = ne.into();
        let dec: AnnotatedAxiom = ax.into();

        let flea = find_logically_equal_axiom(&o, &dec);
        assert!(flea.is_some());

        let flea = flea.unwrap();
        assert_eq!(flea.kind(), AxiomKind::DeclareClass);

        if let Axiom::DeclareClass(ref dc) = flea.axiom {
            assert_eq!(dc.0, b.class("http://www.example.com"));
        }
    }

    #[test]
    fn test_update_equal_axiom() {
        let b = Build::new();
        {
            let mut o = RDFOntology::default();
            let ne: NamedEntity = b.class("http://www.example.com").into();
            let ax: Axiom = ne.into();
            let mut dec: AnnotatedAxiom = ax.into();

            dec.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a1").into(),
            });

            let ne: NamedEntity = b.class("http://www.example.com").into();
            let ax: Axiom = ne.into();
            let mut dec2: AnnotatedAxiom = ax.into();

            dec2.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a2").into(),
            });

            o.insert(dec);
            o.insert(dec2);
            assert_eq!(o.i().iter().count(), 2);
        }

        {
            let mut o = RDFOntology::default();
            let ne: NamedEntity = b.class("http://www.example.com").into();
            let ax: Axiom = ne.into();
            let mut dec: AnnotatedAxiom = ax.into();
            dec.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a1").into(),
            });

            let ne: NamedEntity = b.class("http://www.example.com").into();
            let ax: Axiom = ne.into();
            let mut dec2: AnnotatedAxiom = ax.into();
            dec2.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a2").into(),
            });

            o.insert(dec);
            update_logically_equal_axiom(&mut o, dec2);
            assert_eq!(o.i().iter().count(), 1);

            let aa = o.i().iter().next().unwrap();

            assert_eq!(aa.ann.iter().count(), 2);
        }
    }

}
