//! The validator against oml2owl.
//!
//! Each `src/ont/oml-invalid/X.oml` has an `X.oracle` whose first line says
//! whether oml2owl rejected the file or accepted it, with or without
//! warnings. We must report an error for exactly the files it rejected.

use std::fs;
use std::path::{Path, PathBuf};

use horned_oml::validate::{Diagnostic, Severity, validate, validate_path, validate_with};

fn oml_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = vec![];
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            files.extend(oml_files(&path));
        } else if path.extension().is_some_and(|e| e == "oml") {
            files.push(path);
        }
    }
    files.sort();
    files
}

fn ont(sub: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../src/ont")
        .join(sub)
}

/// The hand-written files, which import one another.
fn resolve(iri: &str) -> Option<String> {
    let rest = iri.strip_prefix("http://")?;
    let path = match rest.strip_prefix("example.com/invalid/") {
        Some(name) => ont("oml-invalid").join(format!("{name}.oml")),
        None => ont("oml-manual").join(format!("{rest}.oml")),
    };
    fs::read_to_string(path).ok()
}

fn errors(path: &Path) -> Vec<Diagnostic> {
    validate_with(&fs::read_to_string(path).unwrap(), &resolve)
        .unwrap_or_else(|e| panic!("{}: {e}", path.display()))
        .into_iter()
        .filter(|d| d.severity == Severity::Error)
        .collect()
}

#[test]
fn agrees_with_the_oracle() {
    let files = oml_files(&ont("oml-invalid"));
    assert!(files.len() > 10);
    let mut disagreements = vec![];
    for path in files {
        let oracle = fs::read_to_string(path.with_extension("oracle"))
            .unwrap_or_else(|_| panic!("no oracle for {}", path.display()));
        let rejected = oracle.lines().next() == Some("rejected");
        let found = errors(&path);
        if rejected == found.is_empty() {
            disagreements.push(format!(
                "{}: oml2owl {}, we found {:?}",
                path.display(),
                oracle.lines().next().unwrap_or(""),
                found
            ));
        }
    }
    assert!(disagreements.is_empty(), "{}", disagreements.join("\n"));
}

#[test]
fn reports_warnings() {
    let warnings = |name: &str| -> Vec<String> {
        let text = fs::read_to_string(ont("oml-invalid").join(name)).unwrap();
        validate(&text)
            .unwrap()
            .into_iter()
            .filter(|d| d.severity == Severity::Warning)
            .map(|d| d.message)
            .collect()
    };
    assert_eq!(
        warnings("unused-import.oml"),
        ["Import <http://www.w3.org/2001/XMLSchema#> with prefix is not used"]
    );
    assert!(
        warnings("duplicate-import.oml")
            .iter()
            .any(|m| m.contains("duplicate"))
    );
    assert!(
        warnings("self-import.oml")
            .iter()
            .any(|m| m.contains("to self"))
    );
    assert!(
        warnings("self-specialization.oml")
            .iter()
            .any(|m| m.contains("itself"))
    );
    assert!(warnings("valid.oml").is_empty());
}

#[test]
fn positions_point_at_the_construct() {
    let text = "vocabulary <http://e.com/v#> as v {\n\tconcept C\n\trelation entity E\n}\n";
    let d = validate(text).unwrap();
    assert_eq!(d.len(), 1);
    assert_eq!((d[0].line, d[0].column), (3, 2));
    assert_eq!(
        d[0].to_string(),
        "3:2: error: Must name a forward relation on E"
    );
}

#[test]
fn syntax_errors_are_errors() {
    assert!(validate("vocabulary {").is_err());
}

#[test]
fn leaves_unresolvable_imports_unchecked() {
    let text = "vocabulary <http://e.com/v#> as v {\n\textends <http://e.com/w#> as w\n\tconcept C < w:D\n}\n";
    assert!(validate(text).unwrap().is_empty());
}

fn no_errors_in(dir: &Path) {
    let failures: Vec<String> = oml_files(dir)
        .iter()
        .filter(|p| !errors(p).is_empty())
        .map(|p| format!("{}: {:?}", p.display(), errors(p)))
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

#[test]
fn accepts_the_valid_fixtures() {
    no_errors_in(&ont("oml-from-bubo"));
    no_errors_in(&ont("oml-manual"));
}

// The library vocabularies are not in this repository: set OML_CORPUS to a
// directory with `oml/`.
#[test]
fn accepts_the_library_vocabularies() {
    let Some(dir) = std::env::var_os("OML_CORPUS") else {
        eprintln!("OML_CORPUS not set, skipping");
        return;
    };
    let failures: Vec<String> = oml_files(&Path::new(&dir).join("oml"))
        .iter()
        .filter_map(|p| match validate_path(p) {
            Ok(d) => d
                .into_iter()
                .find(|d| d.severity == Severity::Error)
                .map(|d| format!("{}: {d}", p.display())),
            Err(e) => Some(format!("{}: {e}", p.display())),
        })
        .collect();
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
