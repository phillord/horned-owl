//! An annotation assertion whose property is declared only in an import.
//!
//! The OWL 2 mapping to RDF graphs takes declarations from the ontology and its import closure.
//! The compare-to-OWL/XML fixtures read one document at a time, so they cannot supply a closure.

use std::io::Cursor;

use horned_owl::io::rdf::reader::{ConcreteRcRDFOntology, IncompleteParse, OntologyParser};
use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const VOCAB: &str = r#"@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/vocab#> .

<http://example.org/vocab> a owl:Ontology .
ex:note a owl:AnnotationProperty .
"#;

const MAIN: &str = r#"@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix ex: <http://example.org/vocab#> .

<http://example.org/main> a owl:Ontology ;
    owl:imports <http://example.org/vocab> .
<http://example.org/main#A> a owl:Class ;
    ex:note "A note." .
<http://example.org/main#B> a owl:Class .
_:b ex:note "A note on a blank node." .
"#;

type Parser<'a> =
    OntologyParser<RcStr, RcAnnotatedComponent, ConcreteRcRDFOntology, &'a Build<RcStr>>;

fn parse(
    b: &Build<RcStr>,
    text: &str,
    ic: &[&ConcreteRcRDFOntology],
) -> (ConcreteRcRDFOntology, IncompleteParse<RcStr>) {
    let mut config: RDFParserConfiguration<RcStr, &Build<RcStr>> =
        ParserConfiguration::new(b).into();
    config.format = Some(oxrdfio::RdfFormat::Turtle);
    let mut p: Parser = OntologyParser::from_bufread(&mut Cursor::new(text.as_bytes()), config)
        .expect("the Turtle document parses");
    p.parse_imports().expect("imports");
    p.parse_declarations().expect("declarations");
    let closure: Vec<_> = ic.iter().map(|o| o.j()).collect();
    p.finish_parse(&closure).expect("finish_parse");
    p.parse().expect("parse")
}

#[test]
fn annotation_property_declared_in_import() {
    let b = Build::new_rc();
    let (vocab, _) = parse(&b, VOCAB, &[]);
    let (main, incomplete) = parse(&b, MAIN, &[&vocab]);

    assert!(incomplete.is_complete(), "parse left triples behind");
    let main = SetOntology::from(main);
    let assertions = main
        .iter()
        .filter(|c| matches!(&c.component, Component::AnnotationAssertion(_)))
        .count();
    assert_eq!(assertions, 2);
}
