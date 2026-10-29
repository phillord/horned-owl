//! Compare the OML reader with oml2owl.
//!
//! Each `src/ont/oml-from-bubo/X.oml` is read and compared with
//! `src/ont/owl-from-oml/X.ofn`, which oml2owl made from the same file.

use std::collections::BTreeSet;
use std::fs;
use std::io::BufReader;
use std::path::{Path, PathBuf};

use horned_owl::io::ParserConfiguration;
use horned_owl::io::ofn::reader::read as read_ofn;
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

type Set = BTreeSet<AnnotatedComponent<RcStr>>;

fn sorted<T: Ord>(mut v: Vec<T>) -> Vec<T> {
    v.sort();
    v
}

fn ce(c: &ClassExpression<RcStr>) -> ClassExpression<RcStr> {
    match c {
        ClassExpression::ObjectIntersectionOf(v) => {
            ClassExpression::ObjectIntersectionOf(sorted(v.iter().map(ce).collect()))
        }
        ClassExpression::ObjectUnionOf(v) => {
            ClassExpression::ObjectUnionOf(sorted(v.iter().map(ce).collect()))
        }
        ClassExpression::ObjectOneOf(v) => ClassExpression::ObjectOneOf(sorted(v.clone())),
        other => other.clone(),
    }
}

/// OWL has no order among the operands of these, so neither do we.
fn normalize(ac: &AnnotatedComponent<RcStr>) -> AnnotatedComponent<RcStr> {
    let component: Component<RcStr> = match &ac.component {
        Component::EquivalentClasses(EquivalentClasses(v)) => {
            EquivalentClasses(sorted(v.iter().map(ce).collect())).into()
        }
        Component::DisjointClasses(DisjointClasses(v)) => {
            DisjointClasses(sorted(v.iter().map(ce).collect())).into()
        }
        Component::EquivalentObjectProperties(EquivalentObjectProperties(v)) => {
            EquivalentObjectProperties(sorted(v.clone())).into()
        }
        Component::EquivalentDataProperties(EquivalentDataProperties(v)) => {
            EquivalentDataProperties(sorted(v.clone())).into()
        }
        Component::InverseObjectProperties(InverseObjectProperties(a, b)) => {
            if a <= b {
                InverseObjectProperties(a.clone(), b.clone()).into()
            } else {
                InverseObjectProperties(b.clone(), a.clone()).into()
            }
        }
        Component::HasKey(HasKey { ce: c, vpe }) => HasKey {
            ce: ce(c),
            vpe: sorted(vpe.clone()),
        }
        .into(),
        Component::DatatypeDefinition(DatatypeDefinition {
            kind,
            range: DataRange::DatatypeRestriction(dt, facets),
        }) => DatatypeDefinition {
            kind: kind.clone(),
            range: DataRange::DatatypeRestriction(dt.clone(), sorted(facets.clone())),
        }
        .into(),
        Component::DatatypeDefinition(DatatypeDefinition {
            kind,
            range: DataRange::DataOneOf(literals),
        }) => DatatypeDefinition {
            kind: kind.clone(),
            range: DataRange::DataOneOf(sorted(literals.clone())),
        }
        .into(),
        Component::Rule(horned_owl::model::Rule { head, body }) => horned_owl::model::Rule {
            head: sorted(head.clone()),
            body: sorted(body.clone()),
        }
        .into(),
        Component::SubClassOf(SubClassOf { sup, sub }) => SubClassOf {
            sup: ce(sup),
            sub: ce(sub),
        }
        .into(),
        other => other.clone(),
    };
    AnnotatedComponent {
        component,
        ann: ac.ann.clone(),
    }
}

/// The IRI declared, if this is a declaration.
fn declared(c: &Component<RcStr>) -> Option<String> {
    Some(match c {
        Component::DeclareClass(DeclareClass(x)) => x.0.to_string(),
        Component::DeclareObjectProperty(DeclareObjectProperty(x)) => x.0.to_string(),
        Component::DeclareDataProperty(DeclareDataProperty(x)) => x.0.to_string(),
        Component::DeclareAnnotationProperty(DeclareAnnotationProperty(x)) => x.0.to_string(),
        Component::DeclareDatatype(DeclareDatatype(x)) => x.0.to_string(),
        Component::DeclareNamedIndividual(DeclareNamedIndividual(x)) => x.0.to_string(),
        _ => return None,
    })
}

/// The namespace of the ontology, which is its IRI plus the final character.
fn namespace(o: &Set) -> String {
    for ac in o {
        if let Component::OntologyAnnotation(OntologyAnnotation(a)) = &ac.component {
            if a.ap.0.to_string() == "http://opencaesar.io/oml#namespace" {
                if let AnnotationValue::IRI(iri) = &a.av {
                    return iri.to_string();
                }
            }
        }
    }
    panic!("no oml:namespace");
}

/// oml2owl only declares terms of the ontology itself, but the OWL API
/// writes a declaration for every other entity that is used.
fn expected(path: &Path, ns: &str) -> Set {
    let text = fs::read_to_string(path).unwrap();
    let (o, _): (SetOntology<RcStr>, _) = read_ofn(
        &mut BufReader::new(text.as_bytes()),
        ParserConfiguration::default(),
    )
    .unwrap();
    o.into_iter()
        .filter(|ac| declared(&ac.component).is_none_or(|iri| iri.starts_with(ns)))
        .map(|ac| normalize(&ac))
        .collect()
}

/// Finds imports in a directory laid out by ontology IRI without its scheme.
fn resolver(root: Option<&Path>) -> impl Fn(&str) -> Option<String> + '_ {
    move |iri: &str| {
        let path = root?.join(format!("{}.oml", iri.split_once("://")?.1));
        fs::read_to_string(path).ok()
    }
}

fn ours(path: &Path, root: Option<&Path>) -> Result<Set, String> {
    let text = fs::read_to_string(path).unwrap();
    let build = Build::new_rc();
    let o: SetOntology<RcStr> =
        horned_oml::reader::read_with(&text, &build, &resolver(root)).map_err(|e| e.to_string())?;
    Ok(o.into_iter().map(|ac| normalize(&ac)).collect())
}

fn compare(oml: &Path, ofn: &Path, root: Option<&Path>) -> Result<(), String> {
    let ours = ours(oml, root)?;
    let theirs = expected(ofn, &namespace(&ours));
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

fn run(pairs: Vec<(PathBuf, PathBuf)>, root: Option<&Path>, known_failures: &[&str]) {
    assert!(!pairs.is_empty());
    let mut failed = vec![];
    for (oml, ofn) in &pairs {
        let name = oml.file_stem().unwrap().to_str().unwrap().to_string();
        match compare(oml, ofn, root) {
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
        &[],
    );
}

#[test]
fn detects_a_difference() {
    let oml = Path::new(ONT).join("oml-from-bubo/and.oml");
    let wrong = Path::new(ONT).join("owl-from-oml/class.ofn");
    assert!(compare(&oml, &wrong, None).is_err());
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
#[ignore = "the closure of vocabulary bundles is not read yet"]
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
    run(pairs, Some(&dir.join("oml")), &[]);
}

// Hand-written OML, for what the generated fixtures do not have: relation
// entities, concepts, instances, rules and bundles.
#[test]
fn manual_fixtures() {
    let oml = Path::new(ONT).join("oml-manual");
    let pairs = all_pairs(&oml, &Path::new(ONT).join("owl-from-oml-manual"), "ofn");
    run(pairs, Some(&oml), &[]);
}
