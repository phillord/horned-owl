use std::fs;
use std::path::{Path, PathBuf};

use horned_oml::parser::{OmlParser, Rule};
use pest::Parser;

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

fn parse_all(dir: &Path) -> Vec<String> {
    oml_files(dir)
        .iter()
        .filter_map(|path| {
            let text = fs::read_to_string(path).unwrap();
            OmlParser::parse(Rule::oml, &text)
                .err()
                .map(|e| format!("{}: {}", path.display(), e))
        })
        .collect()
}

#[test]
fn parse_bubo_fixtures() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("../src/ont/oml-from-bubo");
    assert!(oml_files(&dir).len() > 100);
    let failures = parse_all(&dir);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

// The OML sources of the common libraries (core, imce, metrology and
// provenance vocabularies), which are not in this repository. Set
// OML_CORPUS to a directory holding them.
#[test]
fn parse_library_vocabularies() {
    let Some(dir) = std::env::var_os("OML_CORPUS") else {
        eprintln!("OML_CORPUS not set, skipping");
        return;
    };
    let dir = Path::new(&dir).join("oml");
    assert!(!oml_files(&dir).is_empty());
    let failures = parse_all(&dir);
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}

fn parses(text: &str) -> bool {
    OmlParser::parse(Rule::oml, text).is_ok()
}

#[test]
fn reject_malformed() {
    assert!(parses("vocabulary <http://x#> as x { aspect A }"));
    // missing closing brace
    assert!(!parses("vocabulary <http://x#> as x { aspect A"));
    // keywords are not names unless escaped
    assert!(!parses("vocabulary <http://x#> as x { aspect range }"));
    assert!(parses("vocabulary <http://x#> as x { aspect ^range }"));
    // a keyword that is a prefix of another is still a name
    assert!(parses("vocabulary <http://x#> as x { aspect reference }"));
    assert!(!parses("vocabulary <http://x#> as x { aspect reflexive }"));
    // a namespace must end in # or /
    assert!(!parses("vocabulary <http://x> as x { }"));
}
