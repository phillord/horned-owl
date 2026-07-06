use assert_cmd::cargo;
use assert_cmd::prelude::*; // Add methods on commands

use predicates::prelude::*; // Used for writing assertions
use std::process::Command; // Run programs

#[test]
fn integration_convert_ofn_to_owx() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned-convert"));

    cmd.arg("../src/ont/owl-functional/and.ofn")
        .arg("--to")
        .arg("owx");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("<Ontology"));

    Ok(())
}

#[test]
fn integration_convert_owx_to_ttl() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned-convert"));

    cmd.arg("../src/ont/owl-xml/and.owx").arg("--to").arg("ttl");
    cmd.assert().success().stdout(predicate::str::contains(
        "http://www.w3.org/2002/07/owl#Class",
    ));

    Ok(())
}

#[test]
fn integration_convert_rdf_to_ofn() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned-convert"));

    cmd.arg("../src/ont/owl-rdf/and.owl").arg("--to").arg("ofn");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Ontology("));

    Ok(())
}

#[test]
fn integration_convert_to_file() -> Result<(), Box<dyn std::error::Error>> {
    let dir = mktemp::Temp::new_dir()?;
    let out_file = dir.join("and.owx");

    let mut cmd = Command::new(cargo::cargo_bin!("horned-convert"));
    cmd.arg("../src/ont/owl-functional/and.ofn")
        .arg("--to")
        .arg("owx")
        .arg("--to-file")
        .arg(&out_file);
    cmd.assert().success().stdout(predicate::str::is_empty());

    let written = std::fs::read_to_string(&out_file)?;
    assert!(written.contains("<Ontology"));

    Ok(())
}

#[test]
fn integration_convert_round_trip_via_ttl() -> Result<(), Box<dyn std::error::Error>> {
    let dir = mktemp::Temp::new_dir()?;
    let ttl_file = dir.join("and.ttl");

    // Convert the OFN fixture to Turtle...
    let mut to_ttl = Command::new(cargo::cargo_bin!("horned-convert"));
    to_ttl
        .arg("../src/ont/owl-functional/and.ofn")
        .arg("--to")
        .arg("ttl")
        .arg("--to-file")
        .arg(&ttl_file);
    to_ttl.assert().success();

    // ...then read that Turtle file back and convert it to OFN.
    let mut from_ttl = Command::new(cargo::cargo_bin!("horned-convert"));
    from_ttl.arg(&ttl_file).arg("--to").arg("ofn");
    from_ttl
        .assert()
        .success()
        .stdout(predicate::str::contains("Ontology("));

    Ok(())
}

#[test]
fn integration_convert_unknown_output_format() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned-convert"));

    cmd.arg("../src/ont/owl-functional/and.ofn")
        .arg("--to")
        .arg("bogus");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Format is unknown"));

    Ok(())
}

#[test]
fn integration_convert_unknown_input_extension() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned-convert"));

    cmd.arg("../src/ont/owl-functional/and.bogus")
        .arg("--to")
        .arg("ofn");
    cmd.assert().failure().stderr(predicate::str::contains(
        "Cannot parse a file of this format",
    ));

    Ok(())
}
