//! `horned-owl2oml`: OWL that `horned-oml2owl` made comes back as the same OML.
#![cfg(feature = "cli")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

mod common;
use common::{Set, normalize};

use horned_oml::catalog::CatalogResolver;
use horned_oml::reader::{Config, read_config};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const ONT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../src/ont");

fn copy(from: &Path, to: &Path) {
    fs::create_dir_all(to).unwrap();
    for entry in fs::read_dir(from).unwrap() {
        let path = entry.unwrap().path();
        let target = to.join(path.file_name().unwrap());
        if path.is_dir() {
            copy(&path, &target);
        } else {
            fs::copy(&path, &target).unwrap();
        }
    }
}

/// A directory with the hand-written OML under `oml/` and a catalog over it.
fn project() -> tempfile::TempDir {
    let dir = tempfile::tempdir().unwrap();
    copy(&Path::new(ONT).join("oml-manual"), &dir.path().join("oml"));
    fs::write(
        dir.path().join("catalog.xml"),
        r#"<?xml version="1.0"?><catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog" prefer="public"><rewriteURI uriStartString="http://" rewritePrefix="oml/"/></catalog>"#,
    )
    .unwrap();
    dir
}

fn oml2owl(dir: &Path, args: &[&str]) -> Output {
    let out = Command::new(env!("CARGO_BIN_EXE_horned-oml2owl"))
        .arg("-i")
        .arg(dir.join("catalog.xml"))
        .arg("-o")
        .arg(dir.join("owl/catalog.xml"))
        .args(args)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    out
}

fn owl2oml(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_horned-owl2oml"))
        .arg("-i")
        .arg(dir.join("owl/catalog.xml"))
        .arg("-o")
        .arg(dir.join("back/catalog.xml"))
        .args(args)
        .output()
        .unwrap()
}

fn files(dir: &Path) -> Vec<PathBuf> {
    fn walk(p: &Path, out: &mut Vec<PathBuf>) {
        for entry in fs::read_dir(p).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                walk(&path, out);
            } else if path.extension().is_some_and(|e| e == "oml") {
                out.push(path);
            }
        }
    }
    let mut v = vec![];
    if dir.exists() {
        walk(dir, &mut v);
    }
    v.sort();
    v
}

fn read(path: &Path, catalog: &Path) -> Set {
    let text = fs::read_to_string(path).unwrap();
    let resolver = CatalogResolver::from_path(catalog).unwrap();
    let build = Build::new_rc();
    let (o, _): (SetOntology<RcStr>, _) =
        read_config(&text, &build, &resolver, Config::default()).unwrap();
    o.iter().map(normalize).collect()
}

#[test]
fn reads_back_what_oml2owl_made() {
    let dir = project();
    oml2owl(dir.path(), &["-f", "ttl"]);
    let out = owl2oml(
        dir.path(),
        &["-if", "ttl", "-s", dir.path().join("back").to_str().unwrap()],
    );
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(String::from_utf8_lossy(&out.stdout).contains("5 oml file(s) are saved"));

    let back = files(&dir.path().join("back"));
    assert_eq!(back.len(), 5);
    for file in back {
        let rel = file.strip_prefix(dir.path().join("back")).unwrap();
        let original = dir.path().join("oml").join(rel);
        assert_eq!(
            read(&file, &dir.path().join("back/catalog.xml")),
            read(&original, &dir.path().join("catalog.xml")),
            "{}",
            rel.display()
        );
    }
}

#[test]
fn only_updates_the_source_paths() {
    let dir = project();
    oml2owl(dir.path(), &["-f", "ttl"]);
    let only = dir.path().join("back/example.com/vocab");
    let out = owl2oml(dir.path(), &["-if", "ttl", "-s", only.to_str().unwrap()]);
    assert!(out.status.success());
    let back = files(&dir.path().join("back"));
    assert_eq!(back.len(), 3, "{back:?}");
    assert!(back.iter().all(|p| p.starts_with(&only)));
}

#[test]
fn skips_ontologies_without_oml_annotations() {
    let dir = project();
    oml2owl(dir.path(), &["-f", "ttl", "-an", "suppress"]);
    let out = owl2oml(dir.path(), &["-if", "ttl"]);
    assert!(out.status.success());
    assert!(String::from_utf8_lossy(&out.stdout).contains("0 oml file(s) are saved"));
    assert!(String::from_utf8_lossy(&out.stderr).contains("no oml:type"));
    assert!(files(&dir.path().join("back")).is_empty());
}

#[test]
fn takes_other_input_formats() {
    for (output, input) in [("fss", "fss"), ("owl", "owl"), ("owl", "xml")] {
        let dir = project();
        oml2owl(dir.path(), &["-f", output]);
        let out = owl2oml(dir.path(), &["-if", input]);
        // oml2owl writes the extension that -f names, so xml finds nothing.
        let expected = if input == output { 5 } else { 0 };
        assert!(
            out.status.success(),
            "{input}: {}",
            String::from_utf8_lossy(&out.stderr)
        );
        assert_eq!(files(&dir.path().join("back")).len(), expected, "{input}");
    }
}

#[test]
fn refuses_what_it_cannot_do() {
    let dir = project();
    oml2owl(dir.path(), &["-f", "ttl"]);
    let out = owl2oml(dir.path(), &["-if", "jsonld"]);
    assert!(!out.status.success());
    let out = owl2oml(dir.path(), &["-of", "omlxmi"]);
    assert!(!out.status.success());
}
