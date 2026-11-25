//! An ontology with neither an IRI nor a version IRI has no `OntologyID`.

use horned_owl::model::{Component, RcStr};
use horned_owl::ontology::set::SetOntology;

#[test]
fn ofn_anonymous_has_no_ontology_id() {
    let doc = "Prefix(:=<http://example.com/>)\nOntology(\nDeclaration(Class(:A))\n)";
    let (o, _): (SetOntology<RcStr>, _) =
        horned_pest::ofn::read(&mut doc.as_bytes(), Default::default()).unwrap();
    assert!(o.iter().count() > 0);
    assert!(
        !o.iter()
            .any(|c| matches!(c.component, Component::OntologyID(_)))
    );
}
