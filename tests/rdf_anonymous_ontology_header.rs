//! An ontology header whose subject is a blank node (OWL 2 Mapping to RDF
//! Graphs 3.1.2) is consumed, so a complete parse reports complete.

use horned_owl::io::rdf::reader::{ConcreteRDFOntology, read};
use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;
use std::rc::Rc;

fn parse(src: &str, format: oxrdfio::RdfFormat) -> (SetOntology<RcStr>, bool) {
    let config = RDFParserConfiguration {
        common: ParserConfiguration::default(),
        format: Some(format),
    };
    let (ont, incomplete): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
        read(&mut src.as_bytes(), config).unwrap();
    (ont.into(), incomplete.is_complete())
}

const TTL: &str = "@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
";

#[test]
fn turtle_anonymous_header_is_complete() {
    let src = format!("{TTL}[] a owl:Ontology . <http://example.org/A> a owl:Class .");
    let (ont, complete) = parse(&src, oxrdfio::RdfFormat::Turtle);
    assert!(complete);
    assert!(
        !ont.iter()
            .any(|c| matches!(c.component, Component::OntologyID(_)))
    );
}

#[test]
fn rdfxml_anonymous_header_is_complete() {
    let src = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:owl="http://www.w3.org/2002/07/owl#">
  <owl:Ontology/>
  <owl:Class rdf:about="http://example.org/A"/>
</rdf:RDF>"#;
    let (_, complete) = parse(src, oxrdfio::RdfFormat::RdfXml);
    assert!(complete);
}

#[test]
fn anonymous_header_keeps_annotations() {
    let src = format!(
        "{TTL}[] a owl:Ontology ; owl:versionInfo \"1\" ; rdfs:comment \"c\" .
<http://example.org/A> a owl:Class ."
    );
    let (ont, complete) = parse(&src, oxrdfio::RdfFormat::Turtle);
    assert!(complete);
    let n = ont
        .iter()
        .filter(|c| matches!(c.component, Component::OntologyAnnotation(_)))
        .count();
    assert_eq!(n, 2);
}
