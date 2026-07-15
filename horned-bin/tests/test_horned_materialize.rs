use assert_cmd::cargo;
use assert_cmd::prelude::*; // Add methods on commands
use predicates::prelude::*; // Used for writing assertions
use std::{fs, path::Path, process::Command}; // Run programs

#[test]
fn integration_run() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned-materialize"));

    cmd.assert().failure().stderr(predicate::str::contains(
        "The following required arguments were not provided",
    ));

    Ok(())
}

// Ignored by default because it requires network access: it fetches the BFO
// import (http://purl.obolibrary.org/obo/bfo.owl) referenced by the fixture.
// Run explicitly with `cargo test -- --ignored integration_ont_with_bfo`.
#[test]
#[ignore]
fn integration_ont_with_bfo() -> Result<(), Box<dyn std::error::Error>> {
    // Stage the in-repo fixture into a fresh temp dir so the test is hermetic:
    // no reliance on a hand-placed tmp/ fixture, and no pollution of the work
    // tree (the previous version wrote bfo.owl into the shared tmp/ and never
    // cleaned up, so it could only pass once).
    let dir = std::env::temp_dir().join(format!("horned-materialize-{}", std::process::id()));
    if dir.exists() {
        fs::remove_dir_all(&dir)?;
    }
    fs::create_dir_all(&dir)?;

    let fixture = Path::new(env!("CARGO_MANIFEST_DIR")).join("../src/ont/owl-rdf/ont-with-bfo.owl");
    let ont = dir.join("ont-with-bfo.owl");
    fs::copy(&fixture, &ont)?;

    // The BFO import (http://purl.obolibrary.org/obo/bfo.owl) is localized
    // relative to the input file's directory using horned-owl's "favored"
    // scheme, which joins the full IRI path with underscores — so it
    // materializes to <dir>/obo_bfo.owl. It must not exist before we run.
    let bfo = dir.join("obo_bfo.owl");
    let exists = predicate::path::exists();
    assert!(
        !exists.eval(bfo.as_path()),
        "import file should not exist yet"
    );

    let mut cmd = Command::new(cargo::cargo_bin!("horned-materialize"));
    cmd.arg(&ont);
    cmd.assert().success();

    assert!(
        exists.eval(bfo.as_path()),
        "materialize should have downloaded the BFO import to {}",
        bfo.display()
    );

    fs::remove_dir_all(&dir)?;
    Ok(())
}
