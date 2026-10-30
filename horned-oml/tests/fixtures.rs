//! Compare the OML reader with oml2owl.
//!
//! Each `src/ont/oml-from-bubo/X.oml` is read and compared with
//! `src/ont/owl-from-oml/X.ofn`, which oml2owl made from the same file.

use std::fs;
use std::path::{Path, PathBuf};

mod common;
use common::{Set, expected, namespace, normalize};

use horned_oml::reader::{Config, OmlAnnotations};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

/// Finds imports in a directory laid out by ontology IRI without its scheme.
fn resolver(root: Option<&Path>) -> impl Fn(&str) -> Option<String> + '_ {
    move |iri: &str| {
        let path = root?.join(format!("{}.oml", iri.split_once("://")?.1));
        fs::read_to_string(path).ok()
    }
}

fn ours(path: &Path, root: Option<&Path>, config: Config) -> Result<Set, String> {
    let text = fs::read_to_string(path).unwrap();
    let build = Build::new_rc();
    let (o, _): (SetOntology<RcStr>, _) =
        horned_oml::reader::read_config(&text, &build, &resolver(root), config)
            .map_err(|e| e.to_string())?;
    Ok(o.into_iter().map(|ac| normalize(&ac)).collect())
}

fn compare(oml: &Path, ofn: &Path, root: Option<&Path>, config: Config) -> Result<(), String> {
    let ours = ours(oml, root, config)?;
    // The namespace is an annotation that the options may leave out.
    let ns = namespace(&self::ours(oml, root, Config::default())?);
    let theirs = expected(ofn, &ns);
    if ours == theirs {
        return Ok(());
    }
    let mut msg = String::new();
    for c in theirs.difference(&ours) {
        msg += &format!("  missing: {:?}\n", c.component);
    }
    for c in ours.difference(&theirs) {
        msg += &format!("  extra:   {:?}\n", c.component);
    }
    Err(msg)
}

fn pairs(oml_dir: &Path, ofn_dir: &Path, ext: &str) -> Vec<(PathBuf, PathBuf)> {
    let mut v: Vec<_> = fs::read_dir(oml_dir)
        .unwrap()
        .map(|e| e.unwrap().path())
        .filter(|p| p.extension().is_some_and(|e| e == "oml"))
        .map(|p| {
            let ofn = ofn_dir.join(format!(
                "{}.{ext}",
                p.file_stem().unwrap().to_str().unwrap()
            ));
            (p, ofn)
        })
        .collect();
    v.sort();
    v
}

fn run(
    pairs: Vec<(PathBuf, PathBuf)>,
    root: Option<&Path>,
    config: Config,
    known_failures: &[&str],
) {
    assert!(!pairs.is_empty());
    let mut failed = vec![];
    for (oml, ofn) in &pairs {
        let name = oml.file_stem().unwrap().to_str().unwrap().to_string();
        match compare(oml, ofn, root, config) {
            Ok(()) => {
                assert!(
                    !known_failures.contains(&name.as_str()),
                    "{name} passes but is listed as a known failure"
                );
            }
            Err(msg) => {
                if !known_failures.contains(&name.as_str()) {
                    failed.push(format!("{name}\n{msg}"));
                }
            }
        }
    }
    assert!(
        failed.is_empty(),
        "{} of {} differ:\n{}",
        failed.len(),
        pairs.len(),
        failed.join("\n")
    );
}

const ONT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../src/ont");

#[test]
fn bubo_fixtures() {
    run(
        pairs(
            &Path::new(ONT).join("oml-from-bubo"),
            &Path::new(ONT).join("owl-from-oml"),
            "ofn",
        ),
        None,
        Config::default(),
        &[],
    );
}

#[test]
fn detects_a_difference() {
    let oml = Path::new(ONT).join("oml-from-bubo/and.oml");
    let wrong = Path::new(ONT).join("owl-from-oml/class.ofn");
    assert!(compare(&oml, &wrong, None, Config::default()).is_err());
}

fn all_pairs(oml_dir: &Path, ofn_dir: &Path, ext: &str) -> Vec<(PathBuf, PathBuf)> {
    let mut v = vec![];
    for entry in fs::read_dir(oml_dir).unwrap() {
        let path = entry.unwrap().path();
        let name = path.file_name().unwrap();
        if path.is_dir() {
            v.extend(all_pairs(&path, &ofn_dir.join(name), ext));
        } else if path.extension().is_some_and(|e| e == "oml") {
            let stem = path.file_stem().unwrap().to_str().unwrap();
            v.push((path.clone(), ofn_dir.join(format!("{stem}.{ext}"))));
        }
    }
    v.sort();
    v
}

// The common library vocabularies and the oml2owl output for them. Neither
// is in this repository: set OML_CORPUS to a directory with `oml/` (the
// sources) and `oracle-fss/` (the oml2owl output). The W3C vocabularies are
// skipped as oml2owl does not convert them.
#[test]
fn library_vocabularies() {
    let Some(dir) = std::env::var_os("OML_CORPUS") else {
        eprintln!("OML_CORPUS not set, skipping");
        return;
    };
    let dir = PathBuf::from(dir);
    let pairs = all_pairs(&dir.join("oml"), &dir.join("oracle-fss"), "fss")
        .into_iter()
        .filter(|(_, ofn)| ofn.exists())
        .collect();
    run(pairs, Some(&dir.join("oml")), Config::default(), &[]);
}

// Hand-written OML, for what the generated fixtures do not have: relation
// entities, concepts, instances, rules and bundles.
#[test]
fn manual_fixtures() {
    let oml = Path::new(ONT).join("oml-manual");
    let pairs = all_pairs(&oml, &Path::new(ONT).join("owl-from-oml-manual"), "ofn");
    run(pairs, Some(&oml), Config::default(), &[]);
}

fn manual(variant: &str, config: Config) {
    let oml = Path::new(ONT).join("oml-manual");
    let ofn = Path::new(ONT).join(format!("owl-from-oml-manual-{variant}"));
    run(all_pairs(&oml, &ofn, "ofn"), Some(&oml), config, &[]);
}

// oml2owl -u
#[test]
fn disjoint_unions() {
    manual(
        "disjoint-unions",
        Config {
            disjoint_unions: true,
            ..Config::default()
        },
    );
}

// oml2owl -an suppress
#[test]
fn without_oml_annotations() {
    manual(
        "no-markers",
        Config {
            oml_annotations: OmlAnnotations::Suppress,
            ..Config::default()
        },
    );
}

// oml2owl -a changes nothing in the output for any file we have, as it
// gives no axiom annotations to put on.
#[test]
fn annotations_on_axioms_change_nothing() {
    let oml = Path::new(ONT).join("oml-manual");
    let ofn = Path::new(ONT).join("owl-from-oml-manual");
    let config = Config {
        annotations_on_axioms: true,
        ..Config::default()
    };
    run(all_pairs(&oml, &ofn, "ofn"), Some(&oml), config, &[]);
}

// As `library_vocabularies`, with -u, against `oracle-fss-u/`.
#[test]
fn library_vocabularies_with_disjoint_unions() {
    let Some(dir) = std::env::var_os("OML_CORPUS") else {
        eprintln!("OML_CORPUS not set, skipping");
        return;
    };
    let dir = PathBuf::from(dir);
    if !dir.join("oracle-fss-u").exists() {
        eprintln!("no oracle-fss-u, skipping");
        return;
    }
    let pairs = all_pairs(&dir.join("oml"), &dir.join("oracle-fss-u"), "fss")
        .into_iter()
        .filter(|(_, ofn)| ofn.exists())
        .collect();
    let config = Config {
        disjoint_unions: true,
        ..Config::default()
    };
    run(pairs, Some(&dir.join("oml")), config, &[]);
}
