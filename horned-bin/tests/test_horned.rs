use assert_cmd::cargo;
use assert_cmd::prelude::*; // Add methods on commands

use predicates::prelude::*; // Used for writing assertions
use std::process::Command; // Run programs

#[test]
fn integration_parse_sniffs_owl_xml_content_under_owl_extension()
-> Result<(), Box<dyn std::error::Error>> {
    // #281: `.owl` is used in the wild for both RDF/XML and OWL/XML; a file
    // with genuinely OWL/XML content and a `.owl` extension must not be
    // force-fed to the RDF/XML reader.
    let dir = mktemp::Temp::new_dir()?;
    let ont_file = dir.join("owl-xml-content.owl");

    std::fs::write(
        &ont_file,
        r#"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#"
     ontologyIRI="http://www.example.com/test">
</Ontology>
"#,
    )?;

    let mut cmd = Command::new(cargo::cargo_bin!("horned"));
    cmd.arg("--local-only").arg("parse").arg(&ont_file);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Parse Complete"));

    Ok(())
}

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

#[test]
fn integration_version_reports_horned_owl_version() -> Result<(), Box<dyn std::error::Error>> {
    let mut cmd = Command::new(cargo::cargo_bin!("horned"));
    cmd.arg("--version");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("horned-owl"))
        .stdout(predicate::str::contains(env!("CARGO_PKG_VERSION")));

    Ok(())
}

#[test]
fn integration_version_reports_on_standalone_binary_and_subcommand()
-> Result<(), Box<dyn std::error::Error>> {
    let mut standalone = Command::new(cargo::cargo_bin!("horned-parse"));
    standalone.arg("--version");
    standalone
        .assert()
        .success()
        .stdout(predicate::str::contains("horned-owl"));

    let mut subcommand = Command::new(cargo::cargo_bin!("horned"));
    subcommand.arg("big").arg("--version");
    subcommand
        .assert()
        .success()
        .stdout(predicate::str::contains("horned-owl"));

    Ok(())
}

// Copies a mixed-format closure from src/ont/closure and then breaks the
// imported document, so that only a parse which follows imports notices.
fn broken_import(
    dir_name: &str,
    importer: &str,
    imported: &str,
) -> Result<(mktemp::Temp, std::path::PathBuf), Box<dyn std::error::Error>> {
    let dir = mktemp::Temp::new_dir()?;
    let from = std::path::Path::new("../src/ont/closure").join(dir_name);
    std::fs::copy(from.join(importer), dir.join(importer))?;
    std::fs::write(dir.join(imported), "not an ontology <<<")?;
    let importer = dir.join(importer);
    Ok((dir, importer))
}

#[test]
fn integration_imports_flag_reads_import_closure_of_non_rdf()
-> Result<(), Box<dyn std::error::Error>> {
    for (dir, importer, imported) in [
        (
            "owx-imports-rdf",
            "import-property.owx",
            "other-property.owx",
        ),
        (
            "ofn-imports-ttl",
            "import-property.ofn",
            "other-property.ofn",
        ),
        (
            "obo-imports-rdf",
            "import-property.obo",
            "other-property.obo",
        ),
    ] {
        let (_dir, file) = broken_import(dir, importer, imported)?;

        Command::new(cargo::cargo_bin!("horned"))
            .arg("parse")
            .arg(&file)
            .assert()
            .success();

        Command::new(cargo::cargo_bin!("horned"))
            .arg("--imports")
            .arg("parse")
            .arg(&file)
            .assert()
            .failure();
    }

    Ok(())
}

#[test]
fn integration_imports_flag_parses_a_good_closure() -> Result<(), Box<dyn std::error::Error>> {
    for file in [
        "../src/ont/closure/owx-imports-rdf/import-property.owx",
        "../src/ont/closure/ofn-imports-ttl/import-property.ofn",
        "../src/ont/closure/obo-imports-rdf/import-property.obo",
        "../src/ont/closure/rdf-imports-ttl/import-property.owl",
    ] {
        Command::new(cargo::cargo_bin!("horned"))
            .arg("--imports")
            .arg("parse")
            .arg(file)
            .assert()
            .success()
            .stdout(predicate::str::contains("Parse Complete"));
    }

    Ok(())
}
