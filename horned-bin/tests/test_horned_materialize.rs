use assert_cmd::prelude::*; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions
use std::{path::Path, process::Command}; // Run programs

#[test]
fn integration_run() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-materialize")?;

    cmd.assert().failure().stderr(predicate::str::contains(
        "The following required arguments were not provided",
    ));

    Ok(())
}

// ignore by default because it is requires network access

#[test]
#[ignore]
fn integration_ont_with_bfo() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-materialize")?;

    let predicate_fn = predicate::path::exists();
    assert!(!predicate_fn.eval(Path::new("../tmp/bfo.owl")));

    cmd.arg("../tmp/ont-with-bfo.owl");
    cmd.assert().success();

    assert!(predicate_fn.eval(Path::new("../tmp/bfo.owl")));

    Ok(())
}
