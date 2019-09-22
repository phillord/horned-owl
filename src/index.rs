use crate::model::*;

pub fn find_logically_equal_axiom<'a>(
    o: &'a Ontology,
    axiom: &AnnotatedAxiom,
) -> Option<&'a AnnotatedAxiom> {
    // Find any axiom in Ontology which is the same as AnnotatedAxiom,
    // ignoring the Annotations

    o.annotated_axiom(axiom.kind())
        .find(|ax| ax.logical_eq(axiom))
}

// Find an axiom which is logically equal and merge it's annotations
pub fn update_logically_equal_axiom<'a>(o: &'a mut Ontology, mut axiom: AnnotatedAxiom) {
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
        let mut o = Ontology::new();

        let c = b.class("http://www.example.com");
        o.declare(c);

        let dec: AnnotatedAxiom = declaration(b.class("http://www.example.com").into()).into();

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
            let mut o = Ontology::new();
            let mut dec: AnnotatedAxiom =
                declaration(b.class("http://www.example.com").into()).into();
            dec.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a1").into(),
            });

            let mut dec2: AnnotatedAxiom =
                declaration(b.class("http://www.example.com").into()).into();
            dec2.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a2").into(),
            });

            o.insert(dec);
            o.insert(dec2);
            assert_eq!(o.iter().count(), 2);
        }

        {
            let mut o = Ontology::new();
            let mut dec: AnnotatedAxiom =
                declaration(b.class("http://www.example.com").into()).into();
            dec.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a1").into(),
            });

            let mut dec2: AnnotatedAxiom =
                declaration(b.class("http://www.example.com").into()).into();
            dec2.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a2").into(),
            });

            o.insert(dec);
            update_logically_equal_axiom(&mut o, dec2);
            assert_eq!(o.iter().count(), 1);

            let aa = o.iter().next().unwrap();

            assert_eq!(aa.ann.iter().count(), 2);

        }
    }

}
