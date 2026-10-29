use assert_cmd::cargo;
use assert_cmd::prelude::*;

use predicates::prelude::*;
use std::process::Command;

const OML: &str = "../src/ont/oml-from-bubo/class.oml";

#[cfg(feature = "oml")]
#[test]
fn integration_parse_reads_oml() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned"));
    cmd.arg("--local-only").arg("parse").arg(OML);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Parse Complete"));
    Ok(())
}

#[cfg(feature = "oml")]
#[test]
fn integration_summary_counts_oml() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned"));
    cmd.arg("summary").arg(OML);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Declare Class"));
    Ok(())
}

#[cfg(not(feature = "oml"))]
#[test]
fn integration_oml_needs_the_feature() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned"));
    cmd.arg("--local-only").arg("parse").arg(OML);
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("oml feature"));
    Ok(())
}
