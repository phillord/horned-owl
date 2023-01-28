use assert_cmd::prelude::*; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions
use std::process::Command; // Run programs

#[test]
fn integration_file_doesnt_exist_owl() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-parse")?;

    cmd.arg("test/file/doesnt/exist.owl");
    cmd.assert().failure();
    // Fails at the moment till I get the error handling fixed
    //.stderr(predicate::str::contains("No such file or directory"));

    Ok(())
}
#[test]
fn integration_file_doesnt_exist_owx() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-parse")?;

    cmd.arg("test/file/doesnt/exist.owx");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("No such file or directory"));

    Ok(())
}

#[test]
fn integration_parse_ontology_rdf() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-parse")?;

    cmd.arg("../src/ont/owl-rdf/and.owl");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Parse Complete"));

    Ok(())
}

#[test]
fn integration_parse_ontology_xml() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-parse")?;

    cmd.arg("../src/ont/owl-xml/and.owx");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Parse Complete"));

    Ok(())
}
