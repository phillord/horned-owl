use assert_cmd::prelude::*; // Add methods on commands
use std::process::Command; // Run programs

#[test]
fn integration_round_ontology_xml() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-round")?;

    // and.owx is the output from bubo so will not be the
    // syntactically comparible
    cmd.arg("../src/ont/owl-xml/and.owx");
    cmd.assert().success();

    Ok(())
}

#[test]
fn integration_round_ontology_rdf() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-round")?;
    // and.owx is the output from bubo so will not be the
    // syntactically comparible
    cmd.arg("../src/ont/owl-rdf/and.owl");
    cmd.assert().success();

    Ok(())
}
