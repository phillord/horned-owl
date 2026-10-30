//! `horned-oml2owl`: the same options as `oml2owl`, and the same output.
#![cfg(feature = "cli")]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

mod common;
use common::{expected, namespace};

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

fn run(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_horned-oml2owl"))
        .args(["-i"])
        .arg(dir.join("catalog.xml"))
        .arg("-o")
        .arg(dir.join("owl/catalog.xml"))
        .args(args)
        .output()
        .unwrap()
}

fn outputs(dir: &Path, ext: &str) -> Vec<PathBuf> {
    fn walk(p: &Path, ext: &str, out: &mut Vec<PathBuf>) {
        for entry in fs::read_dir(p).unwrap() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                walk(&path, ext, out);
            } else if path.extension().is_some_and(|e| e == ext) {
                out.push(path);
            }
        }
    }
    let mut v = vec![];
    if dir.join("owl").exists() {
        walk(&dir.join("owl"), ext, &mut v);
    }
    v.sort();
    v
}

#[test]
fn makes_the_axioms_that_oml2owl_makes() {
    let dir = project();
    let out = run(dir.path(), &["-f", "fss"]);
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    assert!(String::from_utf8_lossy(&out.stdout).contains("5 owl file(s) are saved"));

    let theirs = Path::new(ONT).join("owl-from-oml-manual");
    for stem in [
        "example.com/vocab/base",
        "example.com/vocab/extra",
        "example.com/vocab/bundled",
        "example.com/data/people",
        "example.com/data/everything",
    ] {
        let ours = dir.path().join(format!("owl/{stem}.fss"));
        let ns = namespace(&expected(&ours, ""));
        assert_eq!(
            expected(&ours, &ns),
            expected(&theirs.join(format!("{stem}.ofn")), &ns),
            "{stem}"
        );
    }
}

#[test]
fn writes_an_output_catalog_for_the_schemes() {
    let dir = project();
    assert!(run(dir.path(), &["-f", "fss"]).status.success());
    let catalog = fs::read_to_string(dir.path().join("owl/catalog.xml")).unwrap();
    assert!(catalog.contains(r#"uriStartString="http://" rewritePrefix="./""#));
}

#[test]
fn converts_only_the_closure_of_a_root() {
    let dir = project();
    let out = run(
        dir.path(),
        &["-f", "fss", "-r", "http://example.com/vocab/bundled"],
    );
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    let files: Vec<String> = outputs(dir.path(), "fss")
        .iter()
        .map(|p| p.file_name().unwrap().to_str().unwrap().to_string())
        .collect();
    assert_eq!(files, ["base.fss", "bundled.fss", "extra.fss"]);
}

#[test]
fn takes_the_oml_annotations_option() {
    let dir = project();
    assert!(
        run(dir.path(), &["-f", "fss", "-an", "suppress"])
            .status
            .success()
    );
    let text = fs::read_to_string(dir.path().join("owl/example.com/data/people.fss")).unwrap();
    // The structure that says how relations join their ends stays.
    assert!(
        !text.contains("Annotation(<http://opencaesar.io/oml#"),
        "{text}"
    );
    assert!(
        !text.contains("AnnotationAssertion(<http://opencaesar.io/oml#"),
        "{text}"
    );
}

#[test]
fn takes_the_disjoint_unions_option() {
    let dir = project();
    assert!(run(dir.path(), &["-f", "fss", "-u"]).status.success());
    let text = fs::read_to_string(dir.path().join("owl/example.com/vocab/bundled.fss")).unwrap();
    assert!(text.contains("DisjointUnion("));
}

#[test]
fn writes_rdf_xml_by_default() {
    let dir = project();
    assert!(run(dir.path(), &[]).status.success());
    let owl = outputs(dir.path(), "owl");
    assert_eq!(owl.len(), 5);
    assert!(fs::read_to_string(&owl[0]).unwrap().contains("rdf:RDF"));
}

#[test]
fn stops_on_a_validation_error() {
    let dir = project();
    fs::create_dir_all(dir.path().join("oml/example.com/invalid")).unwrap();
    fs::copy(
        Path::new(ONT).join("oml-invalid/ref-forward.oml"),
        dir.path().join("oml/example.com/invalid/ref-forward.oml"),
    )
    .unwrap();
    let out = run(dir.path(), &["-f", "fss"]);
    assert!(!out.status.success());
    assert!(String::from_utf8_lossy(&out.stderr).contains("Cannot name a forward relation"));
    assert!(outputs(dir.path(), "fss").is_empty());
}

#[test]
fn refuses_what_it_cannot_do() {
    let dir = project();
    assert!(!run(dir.path(), &["-f", "docx"]).status.success());
    assert!(!run(dir.path(), &["-rl"]).status.success());
}
