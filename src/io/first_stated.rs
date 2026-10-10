//! An ontology a reader inserts into in the order its document states each
//! component, comparing components as OWL API's object model compares them.
//!
//! A literal typed `xsd:string` and the untyped literal of the same text are
//! two literals here: they sort apart, the untyped one by `rdf:PlainLiteral`
//! and the typed one by `xsd:string`. They are one literal for equality, so of
//! two components that differ only there the ontology holds one, the one the
//! document states first.

use std::collections::HashSet;

use crate::model::{AnnotatedComponent, ForIRI, Literal, MutableOntology, Ontology};
use crate::visitor::mutable::{VisitMut, WalkMut};

const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";

/// `O`, whose `insert` keeps the first of any two components that are equal
/// once every `xsd:string` literal is read as untyped.
pub(crate) struct FirstStated<A: ForIRI, O> {
    ont: O,
    /// The untyped form of every component inserted with an `xsd:string`
    /// literal in it.
    typed: HashSet<AnnotatedComponent<A>>,
}

impl<A: ForIRI, O: Default> Default for FirstStated<A, O> {
    fn default() -> Self {
        FirstStated { ont: O::default(), typed: HashSet::new() }
    }
}

impl<A: ForIRI, O> FirstStated<A, O> {
    pub(crate) fn into_inner(self) -> O {
        self.ont
    }
}

impl<A: ForIRI, O: Ontology<A>> IntoIterator for FirstStated<A, O> {
    type Item = AnnotatedComponent<A>;
    type IntoIter = O::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.ont.into_iter()
    }
}

impl<A: ForIRI, O: Ontology<A>> Ontology<A> for FirstStated<A, O> {
    type ComponentIter<'c>
        = O::ComponentIter<'c>
    where
        Self: 'c,
        A: 'c;

    fn iter(&self) -> Self::ComponentIter<'_> {
        self.ont.iter()
    }
}

impl<A: ForIRI, O: MutableOntology<A>> MutableOntology<A> for FirstStated<A, O> {
    fn insert<AA>(&mut self, ax: AA) -> bool
    where
        AA: Into<AnnotatedComponent<A>>,
    {
        let ac = ax.into();
        match untyped_strings(&ac) {
            None => {
                if !self.typed.is_empty() && self.typed.contains(&ac) {
                    return false;
                }
                self.ont.insert(ac)
            }
            Some(untyped) => {
                if self.typed.contains(&untyped) {
                    return false;
                }
                if let Some(stated) = self.ont.take(&untyped) {
                    self.ont.insert(stated);
                    return false;
                }
                self.typed.insert(untyped);
                self.ont.insert(ac)
            }
        }
    }

    fn take(&mut self, ax: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.ont.take(ax)
    }
}

/// `ac` with every `xsd:string` literal untyped, when it has one.
fn untyped_strings<A: ForIRI>(ac: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
    struct Untype;
    impl<A: ForIRI> VisitMut<A> for Untype {
        fn visit_literal(&mut self, l: &mut Literal<A>) {
            if let Literal::Datatype { literal, datatype_iri } = l
                && datatype_iri.as_ref() == XSD_STRING
            {
                *l = Literal::Simple { literal: std::mem::take(literal) };
            }
        }
    }
    if !has_typed_string(ac) {
        return None;
    }
    let mut untyped = ac.clone();
    WalkMut::new(Untype).annotated_component(&mut untyped);
    Some(untyped)
}

/// Whether `ac` holds an `xsd:string` literal anywhere.
fn has_typed_string<A: ForIRI>(ac: &AnnotatedComponent<A>) -> bool {
    use crate::visitor::immutable::{Visit, Walk};
    struct Find(bool);
    impl<A: ForIRI> Visit<A> for Find {
        fn visit_literal(&mut self, l: &Literal<A>) {
            if let Literal::Datatype { datatype_iri, .. } = l
                && datatype_iri.as_ref() == XSD_STRING
            {
                self.0 = true;
            }
        }
    }
    let mut walk = Walk::new(Find(false));
    walk.annotated_component(ac);
    walk.into_visit().0
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::model::{Annotation, AnnotationAssertion, AnnotationValue, Build, RcStr};
    use crate::ontology::set::SetOntology;

    fn label(b: &Build<RcStr>, l: Literal<RcStr>, comment: Option<Literal<RcStr>>) -> AnnotatedComponent<RcStr> {
        let ann = comment
            .map(|c| Annotation {
                ap: b.annotation_property("http://www.w3.org/2000/01/rdf-schema#comment"),
                av: AnnotationValue::Literal(c),
                ann: Default::default(),
            })
            .into_iter()
            .collect();
        AnnotatedComponent {
            component: AnnotationAssertion {
                subject: b.iri("http://example.org/A").into(),
                ann: Annotation {
                    ap: b.annotation_property("http://www.w3.org/2000/01/rdf-schema#label"),
                    av: AnnotationValue::Literal(l),
                    ann: Default::default(),
                },
            }
            .into(),
            ann,
        }
    }

    fn plain(s: &str) -> Literal<RcStr> {
        Literal::Simple { literal: s.into() }
    }

    fn typed(b: &Build<RcStr>, s: &str) -> Literal<RcStr> {
        Literal::Datatype { literal: s.into(), datatype_iri: b.iri(XSD_STRING) }
    }

    fn held(o: FirstStated<RcStr, SetOntology<RcStr>>) -> Vec<AnnotatedComponent<RcStr>> {
        o.into_inner().into_iter().collect()
    }

    #[test]
    fn the_first_of_a_typed_and_an_untyped_string_is_kept() {
        let b = Build::new_rc();
        let untyped = label(&b, plain("x"), None);
        let typed = label(&b, typed(&b, "x"), None);
        for (first, second) in [(&untyped, &typed), (&typed, &untyped)] {
            let mut o: FirstStated<RcStr, SetOntology<RcStr>> = Default::default();
            assert!(o.insert(first.clone()));
            assert!(!o.insert(second.clone()));
            assert_eq!(held(o), vec![first.clone()]);
        }
    }

    #[test]
    fn a_string_in_an_axiom_annotation_compares_untyped() {
        let b = Build::new_rc();
        let first = label(&b, plain("x"), Some(typed(&b, "c")));
        let second = label(&b, typed(&b, "x"), Some(plain("c")));
        let mut o: FirstStated<RcStr, SetOntology<RcStr>> = Default::default();
        assert!(o.insert(first.clone()));
        assert!(!o.insert(second));
        assert_eq!(held(o), vec![first]);
    }

    /// The values of the label assertions `o` holds.
    fn labels(o: &SetOntology<RcStr>) -> Vec<Literal<RcStr>> {
        o.iter()
            .filter_map(|ac| match &ac.component {
                crate::model::Component::AnnotationAssertion(aa) => match &aa.ann.av {
                    AnnotationValue::Literal(l) => Some(l.clone()),
                    _ => None,
                },
                _ => None,
            })
            .collect()
    }

    #[test]
    fn each_reader_keeps_the_string_its_document_states_first() {
        let b = Build::new_rc();
        let cfg = || crate::io::ParserConfiguration::new(&b);
        let ofn = r#"Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)
Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)
Ontology(<http://example.org/d>
AnnotationAssertion(rdfs:label <http://example.org/A> "x"^^xsd:string)
AnnotationAssertion(rdfs:label <http://example.org/A> "x")
)"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::ofn::reader::read(&mut ofn.as_bytes(), cfg()).unwrap();
        assert_eq!(labels(&o), vec![typed(&b, "x")], "functional syntax");

        let owx = r#"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#" ontologyIRI="http://example.org/d">
    <AnnotationAssertion>
        <AnnotationProperty IRI="http://www.w3.org/2000/01/rdf-schema#label"/>
        <IRI>http://example.org/A</IRI>
        <Literal datatypeIRI="http://www.w3.org/2001/XMLSchema#string">x</Literal>
    </AnnotationAssertion>
    <AnnotationAssertion>
        <AnnotationProperty IRI="http://www.w3.org/2000/01/rdf-schema#label"/>
        <IRI>http://example.org/A</IRI>
        <Literal>x</Literal>
    </AnnotationAssertion>
</Ontology>"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::owx::reader::read(&mut owx.as_bytes(), cfg()).unwrap();
        assert_eq!(labels(&o), vec![typed(&b, "x")], "OWL/XML");

        let omn = r#"Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>
Ontology: <http://example.org/d>
AnnotationProperty: rdfs:label
Class: <http://example.org/A>
    Annotations: rdfs:label "x", rdfs:label "x"^^xsd:string
"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::omn::reader::read(omn.as_bytes(), cfg()).unwrap();
        assert_eq!(labels(&o), vec![plain("x")], "Manchester syntax");
    }

    #[test]
    fn a_language_tag_keeps_a_string_apart() {
        let b = Build::new_rc();
        let tagged = label(&b, Literal::Language { literal: "x".into(), lang: "en".into() }, None);
        let mut o: FirstStated<RcStr, SetOntology<RcStr>> = Default::default();
        assert!(o.insert(tagged));
        assert!(o.insert(label(&b, typed(&b, "x"), None)));
        assert_eq!(held(o).len(), 2);
    }
}
