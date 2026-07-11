use assert_cmd::cargo;
use assert_cmd::prelude::*; // Add methods on commands

use predicates::prelude::*; // Used for writing assertions
use std::process::Command; // Run programs

#[test]
fn integration_local_only_allows_purely_local_parse() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned"));

    cmd.arg("--local-only")
        .arg("parse")
        .arg("../src/ont/owl-rdf/and.owl");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Parse Complete"));

    Ok(())
}

#[test]
fn integration_local_only_blocks_remote_import() -> Result<(), Box<dyn std::error::Error>> {
    let dir = mktemp::Temp::new_dir()?;
    let ont_file = dir.join("imports-unreachable.owl");

    // RFC 5737 TEST-NET-1 (192.0.2.0/24): reserved for documentation, never
    // routable. If --local-only did not short-circuit before the network
    // call, this would hang/time out rather than fail fast.
    std::fs::write(
        &ont_file,
        r#"<?xml version="1.0"?>
<rdf:RDF xmlns:owl="http://www.w3.org/2002/07/owl#"
     xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <owl:Ontology rdf:about="http://www.example.com/local-only-test">
        <owl:imports rdf:resource="http://192.0.2.1/unreachable.owl"/>
    </owl:Ontology>
</rdf:RDF>
"#,
    )?;

    let mut cmd = Command::new(cargo::cargo_bin!("horned"));
    cmd.arg("--local-only").arg("parse").arg(&ont_file);
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("local-only mode is enabled"));

    Ok(())
}

#[test]
fn integration_local_only_not_available_on_standalone_binary()
-> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned-parse"));

    cmd.arg("--local-only").arg("../src/ont/owl-rdf/and.owl");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("--local-only"));

    Ok(())
}
