//! Errors from an imported document name that document.

use std::path::Path;

use horned_owl::error::HornedError;
use horned_owl::io::closure_reader::read_to_closure;
use horned_owl::io::rdf::reader::ConcreteRcRDFOntology;
use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::*;
use horned_owl::resolve::path_to_file_iri;

fn ofn(imports: &[&str], body: &str) -> String {
    let imports: String = imports.iter().map(|i| format!("Import(<{i}>)\n")).collect();
    format!("Ontology(<http://example.org/doc>\n{imports}{body}\n)\n")
}

fn rdfxml(imports: &[&str]) -> String {
    let imports: String = imports
        .iter()
        .map(|i| format!("<owl:imports rdf:resource=\"{i}\"/>"))
        .collect();
    format!(
        "<?xml version=\"1.0\"?>\n\
         <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" \
         xmlns:owl=\"http://www.w3.org/2002/07/owl#\">\n\
         <owl:Ontology rdf:about=\"http://example.org/doc\">{imports}</owl:Ontology>\n\
         </rdf:RDF>\n"
    )
}

fn file_iri(b: &Build<RcStr>, p: &Path) -> String {
    path_to_file_iri(b, p).to_string()
}

fn read(root: &Path) -> Result<usize, HornedError> {
    let b = Build::new_rc();
    let mut config: RDFParserConfiguration<RcStr, &Build<RcStr>> =
        ParserConfiguration::new(&b).into();
    config.common.local_only = true;
    let v: Vec<(ConcreteRcRDFOntology, _)> = read_to_closure(&path_to_file_iri(&b, root), config)?;
    Ok(v.len())
}

fn write(dir: &Path, name: &str, text: &str) -> std::path::PathBuf {
    let p = dir.join(name);
    std::fs::write(&p, text).unwrap();
    p
}

fn failing(root: &Path) -> HornedError {
    read(root).expect_err("the closure should fail")
}

fn closure_with_broken_b(a_name: &str, b_name: &str, a: fn(&str) -> String, b: &str) {
    let dir = tempfile::tempdir().unwrap();
    let bld = Build::new_rc();
    let b_path = write(dir.path(), b_name, b);
    let a_path = write(dir.path(), a_name, &a(&file_iri(&bld, &b_path)));
    let e = failing(&a_path);
    let text = e.to_string();
    assert!(text.contains(&file_iri(&bld, &b_path)), "{text}");
    assert!(!text.contains(&file_iri(&bld, &a_path)), "{text}");
}

#[test]
fn ofn_parse_error_names_import() {
    closure_with_broken_b(
        "a.ofn",
        "b.ofn",
        |b| ofn(&[b], ""),
        "Ontology(<http://example.org/b> this is not functional syntax",
    );
}

#[test]
fn rdfxml_parse_error_names_import() {
    closure_with_broken_b(
        "a.owl",
        "b.owl",
        |b| rdfxml(&[b]),
        "<?xml version=\"1.0\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"><unclosed>",
    );
}

#[test]
fn missing_import_names_document() {
    let dir = tempfile::tempdir().unwrap();
    let bld = Build::new_rc();
    let missing = file_iri(&bld, &dir.path().join("missing.ofn"));
    let a = write(dir.path(), "a.ofn", &ofn(&[&missing], ""));
    let e = failing(&a);
    assert!(matches!(e, HornedError::IOError(_)));
    assert!(e.to_string().contains(&missing), "{e}");
}

#[test]
fn deep_failure_names_only_the_failing_document() {
    let dir = tempfile::tempdir().unwrap();
    let bld = Build::new_rc();
    let c = write(
        dir.path(),
        "c.ofn",
        "Ontology(<http://example.org/c> garbage",
    );
    let b = write(
        dir.path(),
        "b.ofn",
        &ofn(&[&file_iri(&bld, &c)], "").replace("/doc", "/b"),
    );
    let a = write(dir.path(), "a.ofn", &ofn(&[&file_iri(&bld, &b)], ""));
    let text = failing(&a).to_string();
    assert!(text.contains(&file_iri(&bld, &c)), "{text}");
    assert!(!text.contains(&file_iri(&bld, &b)), "{text}");
    assert_eq!(text.matches("in document").count(), 1, "{text}");
}

#[test]
fn original_error_is_the_source() {
    let dir = tempfile::tempdir().unwrap();
    let bld = Build::new_rc();
    let b = write(
        dir.path(),
        "b.ofn",
        "Ontology(<http://example.org/b> garbage",
    );
    let a = write(dir.path(), "a.ofn", &ofn(&[&file_iri(&bld, &b)], ""));
    match failing(&a) {
        HornedError::ParserError(e, _) => assert!(std::error::Error::source(&*e).is_some()),
        e => panic!("unexpected error {e}"),
    }
}

#[test]
fn top_level_error_is_unchanged() {
    let dir = tempfile::tempdir().unwrap();
    let a = write(
        dir.path(),
        "a.ofn",
        "Ontology(<http://example.org/a> garbage",
    );
    assert!(!failing(&a).to_string().contains("in document"));
}

#[test]
fn good_closure_is_unchanged() {
    let dir = tempfile::tempdir().unwrap();
    let bld = Build::new_rc();
    let b_path = dir.path().join("b.ofn");
    let b_iri = file_iri(&bld, &b_path);
    let b = write(
        dir.path(),
        "b.ofn",
        &ofn(&[], "").replace("http://example.org/doc", &b_iri),
    );
    let a = write(dir.path(), "a.ofn", &ofn(&[&b_iri], ""));
    assert_eq!(read(&a).unwrap(), 2);
    let _ = b;
}
