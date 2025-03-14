use assert_cmd::prelude::*;
use predicates::prelude::*;
use std::process::Command;

#[test]
fn integration_run() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::cargo_bin("horned-big")?;

    // Positive arguments are allowed
    cmd.arg("10").assert().success();
    // Negative arguments are not allowed
    cmd.arg("-1").assert().failure();
    // Arguments must be integers
    cmd.arg("a").assert().failure();
    // Lack of arguments should result in help message
    cmd.arg("").assert().code(predicate::eq(2));

    Ok(())
}
