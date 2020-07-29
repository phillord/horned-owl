//! Search facilities for Ontologies
//!
//! This library provides simple search facilities.
//!
//! It is currently being deprecated in favour of [`OntologyIndex`](../ontology/indexed/OntologyIndex.html)
use crate::io::rdf::reader::RDFOntology;

use crate::model::*;

use std::rc::Rc;

// Find an axiom which is logically equal and merge it's annotations
pub fn update_logically_equal_axiom<'a>(o: &mut RDFOntology, mut axiom: AnnotatedAxiom) {

    let src = o.k().logical_get_rc(&axiom);
    // Does the logically equal axiom exist
    if let Some(rc) = src {
        // Remove the rc from everywhere
        o.remove(&*rc);
        //dbg!(&rc);
        //dbg!(Rc::strong_count(&rc));

        // Un-rc
        let mut logical_axiom = Rc::try_unwrap(rc).unwrap();
        // Extend it
        logical_axiom.ann.append(&mut axiom.ann);
        // Insert it
        o.insert(logical_axiom);
    }
    else {
        // Otherwise put the one we have in
        o.insert(axiom);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    //use crate::model::*;

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
            assert_eq!(o.i().into_iter().count(), 2);
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
            assert_eq!(o.i().into_iter().count(), 1);

            let aa = o.i().into_iter().next().unwrap();

            assert_eq!(aa.ann.iter().count(), 2);
        }
    }
}
