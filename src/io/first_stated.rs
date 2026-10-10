//! An ontology a reader inserts into in the order its document states each
//! component, comparing components as OWL API's object model compares them.
//!
//! A literal typed `xsd:string` and the untyped literal of the same text are
//! two literals here: they sort apart, the untyped one by `rdf:PlainLiteral`
//! and the typed one by `xsd:string`. They are one literal for equality, so of
//! two components that differ only there the ontology holds one, the one the
//! document states first.
//!
//! An ontology annotation equal to one inserted before, by the same
//! comparison, adds nothing, and the ontology holds one annotation for a
//! property and a value, the first inserted, whatever annotations each
//! carries of its own.

use std::collections::HashSet;

use crate::io::HoldOntologyAnnotations;
use crate::model::{
    AnnotatedComponent, Annotation, AnnotationProperty, AnnotationValue, Component, ForIRI, Literal,
    MutableOntology, Ontology, OntologyAnnotation,
};
use crate::visitor::mutable::{VisitMut, WalkMut};

const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";

/// `O`, whose `insert` keeps the first of any two components that are equal
/// once every `xsd:string` literal is read as untyped, and of the ontology
/// annotations inserted those [`first_of_each`] holds, or those a hold
/// function chooses.
pub(crate) struct FirstStated<A: ForIRI, O> {
    ont: O,
    /// Which ontology annotations the ontology holds, of every one inserted;
    /// with it, they wait in `added` until the reader is done.
    hold: Option<HoldOntologyAnnotations<A>>,
    added: Vec<Annotation<A>>,
    /// The untyped form of every component inserted with an `xsd:string`
    /// literal in it.
    typed: HashSet<AnnotatedComponent<A>>,
    /// The ontology annotations inserted, without a hold function.
    first: FirstOfEach<A>,
}

/// Of the annotations an ontology is given of itself, in order, those it holds
/// when nothing else chooses: each but one equal to an annotation given before,
/// once every `xsd:string` literal is read as untyped, and one with the
/// property and the value of an annotation held, whatever annotations each
/// carries of its own.
pub(crate) fn first_of_each<A: ForIRI>(added: Vec<Annotation<A>>) -> Vec<Annotation<A>> {
    let mut first = FirstOfEach::default();
    added.into_iter().filter(|a| first.holds(a)).collect()
}

/// The annotations an ontology was given of itself, for [`first_of_each`].
struct FirstOfEach<A: ForIRI> {
    /// The untyped form of every one given, held or not.
    given: HashSet<AnnotatedComponent<A>>,
    /// The property and the value of every one held.
    held: HashSet<(AnnotationProperty<A>, AnnotationValue<A>)>,
}

impl<A: ForIRI> Default for FirstOfEach<A> {
    fn default() -> Self {
        FirstOfEach { given: HashSet::new(), held: HashSet::new() }
    }
}

impl<A: ForIRI> FirstOfEach<A> {
    /// Whether `a`, given after those given before, is held.
    fn holds(&mut self, a: &Annotation<A>) -> bool {
        let ac: AnnotatedComponent<A> = OntologyAnnotation(a.clone()).into();
        let untyped = untyped_strings(&ac).unwrap_or(ac);
        self.given.insert(untyped) && self.held.insert((a.ap.clone(), a.av.clone()))
    }
}

impl<A: ForIRI, O: Default> Default for FirstStated<A, O> {
    fn default() -> Self {
        FirstStated::new(None)
    }
}

impl<A: ForIRI, O: Default> FirstStated<A, O> {
    /// An empty ontology that holds the ontology annotations `hold` chooses.
    pub(crate) fn new(hold: Option<HoldOntologyAnnotations<A>>) -> Self {
        FirstStated {
            ont: O::default(),
            hold,
            added: Vec::new(),
            typed: HashSet::new(),
            first: FirstOfEach::default(),
        }
    }
}

impl<A: ForIRI, O: MutableOntology<A>> FirstStated<A, O> {
    pub(crate) fn into_inner(mut self) -> O {
        if let Some(hold) = self.hold {
            for a in hold(std::mem::take(&mut self.added)) {
                self.ont.insert(OntologyAnnotation(a));
            }
        }
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
        if let Component::OntologyAnnotation(OntologyAnnotation(a)) = &ac.component {
            if self.hold.is_some() {
                self.added.push(a.clone());
                return true;
            }
            return self.first.holds(a) && self.ont.insert(ac);
        }
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
    use crate::model::{Annotation, AnnotationAssertion, AnnotationValue, Build, OntologyAnnotation, RcStr};
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

    fn comment(b: &Build<RcStr>, l: Literal<RcStr>, label: Option<&str>) -> AnnotatedComponent<RcStr> {
        let ann = label
            .map(|x| Annotation {
                ap: b.annotation_property("http://www.w3.org/2000/01/rdf-schema#label"),
                av: AnnotationValue::Literal(plain(x)),
                ann: Default::default(),
            })
            .into_iter()
            .collect();
        AnnotatedComponent {
            component: OntologyAnnotation(Annotation {
                ap: b.annotation_property("http://www.w3.org/2000/01/rdf-schema#comment"),
                av: AnnotationValue::Literal(l),
                ann,
            })
            .into(),
            ann: Default::default(),
        }
    }

    #[test]
    fn the_first_ontology_annotation_of_a_property_and_value_is_held() {
        let b = Build::new_rc();
        let bare = comment(&b, plain("c"), None);
        let annotated = comment(&b, plain("c"), Some("x"));
        for (first, second) in [(&bare, &annotated), (&annotated, &bare)] {
            let mut o: FirstStated<RcStr, SetOntology<RcStr>> = Default::default();
            assert!(o.insert(first.clone()));
            assert!(!o.insert(second.clone()));
            assert_eq!(held(o), vec![first.clone()]);
        }
    }

    #[test]
    fn an_untyped_and_a_typed_string_are_two_values_of_an_ontology_annotation() {
        let b = Build::new_rc();
        let typed_annotated = comment(&b, typed(&b, "e"), Some("x"));
        let untyped = comment(&b, plain("e"), None);
        let mut o: FirstStated<RcStr, SetOntology<RcStr>> = Default::default();
        assert!(o.insert(typed_annotated));
        assert!(o.insert(untyped));
        assert_eq!(held(o).len(), 2);
    }

    #[test]
    fn an_ontology_annotation_equal_to_one_not_held_adds_nothing() {
        let b = Build::new_rc();
        let mut o: FirstStated<RcStr, SetOntology<RcStr>> = Default::default();
        let first = comment(&b, plain("c"), None);
        assert!(o.insert(first.clone()));
        assert!(!o.insert(comment(&b, plain("c"), Some("x"))));
        // Equal to the one not held once its string is untyped, though no
        // annotation held has its value.
        assert!(!o.insert(comment(&b, typed(&b, "c"), Some("x"))));
        assert_eq!(held(o), vec![first]);
    }

    #[test]
    fn each_reader_holds_the_ontology_annotation_its_document_states_first() {
        let b = Build::new_rc();
        let cfg = || crate::io::ParserConfiguration::new(&b);
        let comments = |o: &SetOntology<RcStr>| -> Vec<AnnotatedComponent<RcStr>> {
            o.iter().filter(|ac| matches!(ac.component, crate::model::Component::OntologyAnnotation(_))).cloned().collect()
        };
        let ofn = r#"Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)
Ontology(<http://example.org/o>
Annotation(rdfs:comment "c")
Annotation(Annotation(rdfs:label "x") rdfs:comment "c")
)"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::ofn::reader::read(&mut ofn.as_bytes(), cfg()).unwrap();
        assert_eq!(comments(&o), vec![comment(&b, plain("c"), None)], "functional syntax");

        let owx = r#"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#" ontologyIRI="http://example.org/o">
    <Annotation>
        <Annotation>
            <AnnotationProperty IRI="http://www.w3.org/2000/01/rdf-schema#label"/>
            <Literal>x</Literal>
        </Annotation>
        <AnnotationProperty IRI="http://www.w3.org/2000/01/rdf-schema#comment"/>
        <Literal>c</Literal>
    </Annotation>
    <Annotation>
        <AnnotationProperty IRI="http://www.w3.org/2000/01/rdf-schema#comment"/>
        <Literal>c</Literal>
    </Annotation>
</Ontology>"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::owx::reader::read(&mut owx.as_bytes(), cfg()).unwrap();
        assert_eq!(comments(&o), vec![comment(&b, plain("c"), Some("x"))], "OWL/XML");

        let omn = r#"Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
Ontology: <http://example.org/o>
Annotations: rdfs:comment "c", Annotations: rdfs:label "x" rdfs:comment "c"
"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::omn::reader::read(omn.as_bytes(), cfg()).unwrap();
        assert_eq!(comments(&o), vec![comment(&b, plain("c"), None)], "Manchester syntax");
    }

    #[test]
    fn a_hold_function_chooses_the_ontology_annotations_each_reader_holds() {
        fn last(mut added: Vec<Annotation<RcStr>>) -> Vec<Annotation<RcStr>> {
            added.drain(..added.len().saturating_sub(1));
            added
        }
        let b = Build::new_rc();
        let cfg = || {
            let mut cfg = crate::io::ParserConfiguration::new(&b);
            cfg.hold_ontology_annotations = Some(last);
            cfg
        };
        let held_by = |o: &SetOntology<RcStr>| -> Vec<AnnotatedComponent<RcStr>> {
            o.iter().filter(|ac| matches!(ac.component, crate::model::Component::OntologyAnnotation(_))).cloned().collect()
        };
        let second = vec![comment(&b, plain("d"), None)];

        let ofn = r#"Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)
Ontology(<http://example.org/o>
Annotation(rdfs:comment "c")
Annotation(rdfs:comment "d")
)"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::ofn::reader::read(&mut ofn.as_bytes(), cfg()).unwrap();
        assert_eq!(held_by(&o), second, "functional syntax");

        let owx = r#"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#" ontologyIRI="http://example.org/o">
    <Annotation>
        <AnnotationProperty IRI="http://www.w3.org/2000/01/rdf-schema#comment"/>
        <Literal>c</Literal>
    </Annotation>
    <Annotation>
        <AnnotationProperty IRI="http://www.w3.org/2000/01/rdf-schema#comment"/>
        <Literal>d</Literal>
    </Annotation>
</Ontology>"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::owx::reader::read(&mut owx.as_bytes(), cfg()).unwrap();
        assert_eq!(held_by(&o), second, "OWL/XML");

        let omn = r#"Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
Ontology: <http://example.org/o>
Annotations: rdfs:comment "c", rdfs:comment "d"
"#;
        let (o, _): (SetOntology<RcStr>, _) = crate::io::omn::reader::read(omn.as_bytes(), cfg()).unwrap();
        assert_eq!(held_by(&o), second, "Manchester syntax");

        let rdf = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#">
    <owl:Ontology rdf:about="http://example.org/o">
        <rdfs:comment>c</rdfs:comment>
        <rdfs:comment>d</rdfs:comment>
    </owl:Ontology>
</rdf:RDF>"#;
        let config = crate::io::RDFParserConfiguration { common: cfg(), format: None };
        let (o, _): (crate::io::rdf::reader::ConcreteRcRDFOntology, _) =
            crate::io::rdf::reader::read(&mut rdf.as_bytes(), config).unwrap();
        let o: SetOntology<RcStr> = o.into();
        assert_eq!(held_by(&o).len(), 1, "RDF/XML");
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
