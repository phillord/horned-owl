//! An ontology with neither an IRI nor a version IRI has no `OntologyID`, in every reader.

use horned_owl::io::ParserConfiguration;
use horned_owl::io::{ofn, omn, owx};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const OFN: &str = "Prefix(:=<http://example.com/>)\nOntology(\nDeclaration(Class(:A))\n)";
const OMN: &str = "Prefix: : <http://example.com/>\nOntology:\nClass: :A\n";
const OWX: &str = r#"<Ontology xmlns="http://www.w3.org/2002/07/owl#">
<Declaration><Class IRI="http://example.com/A"/></Declaration>
</Ontology>"#;

fn has_id(o: &SetOntology<RcStr>) -> bool {
    o.iter()
        .any(|c| matches!(c.component, Component::OntologyID(_)))
}

fn stream_has_id<I, E>(items: I) -> bool
where
    I: Iterator<Item = Result<horned_owl::io::StreamComponent<AnnotatedComponent<RcStr>>, E>>,
    E: std::fmt::Debug,
{
    items.map(|i| i.unwrap()).any(|i| {
        matches!(i, horned_owl::io::StreamComponent::Component(c)
            if matches!(c.component, Component::OntologyID(_)))
    })
}

#[test]
fn ofn_anonymous_has_no_ontology_id() {
    let (o, _): (SetOntology<RcStr>, _) =
        ofn::reader::read(&mut OFN.as_bytes(), ParserConfiguration::default()).unwrap();
    assert!(o.iter().count() > 0 && !has_id(&o));
    let b = Build::new_rc();
    assert!(!stream_has_id(ofn::reader::read_to_stream(
        OFN.as_bytes(),
        ParserConfiguration::new(&b)
    )));
}

#[test]
fn omn_anonymous_has_no_ontology_id() {
    let (o, _): (SetOntology<RcStr>, _) =
        omn::reader::read(&mut OMN.as_bytes(), ParserConfiguration::default()).unwrap();
    assert!(o.iter().count() > 0 && !has_id(&o));
    let b = Build::new_rc();
    assert!(!stream_has_id(omn::reader::read_to_stream(
        OMN.as_bytes(),
        ParserConfiguration::new(&b)
    )));
}

#[test]
fn owx_anonymous_has_no_ontology_id() {
    let (o, _): (SetOntology<RcStr>, _) =
        owx::reader::read(&mut OWX.as_bytes(), ParserConfiguration::default()).unwrap();
    assert!(o.iter().count() > 0 && !has_id(&o));
    let b = Build::new_rc();
    assert!(!stream_has_id(owx::reader::read_to_stream(
        OWX.as_bytes(),
        ParserConfiguration::new(&b)
    )));
}
