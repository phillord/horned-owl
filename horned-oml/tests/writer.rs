//! The OML writer: read, write, read again, and compare the two readings.

use std::fs;
use std::path::{Path, PathBuf};

mod common;
use common::{Set, normalize};

use horned_oml::reader::{Config, read_config};
use horned_oml::writer::{self, OntologyInfo, WriteError};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const ONT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/../src/ont");

/// Finds imports in a directory laid out by ontology IRI without its scheme.
fn source(root: Option<&Path>, iri: &str) -> Option<String> {
    let path = root?.join(format!("{}.oml", iri.split_once("://")?.1));
    fs::read_to_string(path).ok()
}

fn read(text: &str, root: Option<&Path>) -> Result<SetOntology<RcStr>, String> {
    let build = Build::new_rc();
    let resolver = |iri: &str| source(root, iri);
    read_config(text, &build, &resolver, Config::default())
        .map(|(o, _): (SetOntology<RcStr>, _)| o)
        .map_err(|e| e.to_string())
}

enum Outcome {
    Same,
    /// The writer refused, with its reasons.
    Refused(String),
}

fn round_trip(path: &Path, root: Option<&Path>) -> Result<Outcome, String> {
    let text = fs::read_to_string(path).unwrap();
    let first = read(&text, root)?;
    let imports = |iri: &str| -> Option<OntologyInfo> {
        OntologyInfo::of(&read(&source(root, iri)?, root).ok()?)
    };
    let written = match writer::write(&first, &imports, writer::Config::default()) {
        Ok(w) => w,
        Err(WriteError::Unsupported(p)) => return Ok(Outcome::Refused(p.join("\n"))),
        Err(e) => return Err(e.to_string()),
    };
    if std::env::var_os("OML_SHOW_WRITTEN")
        .is_some_and(|v| path.to_string_lossy().contains(v.to_str().unwrap()))
    {
        eprintln!("{written}");
    }
    let second = read(&written, root).map_err(|e| format!("{e}\n{written}"))?;
    let set = |o: &SetOntology<RcStr>| -> Set { o.iter().map(normalize).collect() };
    let (a, b) = (set(&first), set(&second));
    if a == b {
        return Ok(Outcome::Same);
    }
    let mut msg = String::new();
    for c in a.difference(&b) {
        msg += &format!("  lost:  {:?}\n", c.component);
    }
    for c in b.difference(&a) {
        msg += &format!("  extra: {:?}\n", c.component);
    }
    Err(format!("{msg}{written}"))
}

fn all(dir: &Path) -> Vec<PathBuf> {
    let mut v = vec![];
    for entry in fs::read_dir(dir).unwrap() {
        let path = entry.unwrap().path();
        if path.is_dir() {
            v.extend(all(&path));
        } else if path.extension().is_some_and(|e| e == "oml") {
            v.push(path);
        }
    }
    v.sort();
    v
}

fn run(files: Vec<PathBuf>, root: Option<&Path>) -> (usize, Vec<String>) {
    assert!(!files.is_empty());
    let (mut same, mut refused, mut failed) = (0, vec![], vec![]);
    for f in &files {
        let name = f.file_stem().unwrap().to_str().unwrap();
        match round_trip(f, root) {
            Ok(Outcome::Same) => same += 1,
            Ok(Outcome::Refused(why)) => refused.push(format!("{name}: {why}")),
            Err(msg) => failed.push(format!("{name}\n{msg}")),
        }
    }
    eprintln!(
        "{same} of {} written and read back the same, {} refused",
        files.len(),
        refused.len()
    );
    if std::env::var_os("OML_SHOW_REFUSED").is_some() {
        eprintln!("{}", refused.join("\n"));
    }
    assert!(
        failed.is_empty(),
        "{} of {} differ:\n{}",
        failed.len(),
        files.len(),
        failed.join("\n")
    );
    (same, refused)
}

#[test]
fn bubo_round_trip() {
    run(all(&Path::new(ONT).join("oml-from-bubo")), None);
}

#[test]
fn manual_round_trip() {
    let root = Path::new(ONT).join("oml-manual");
    run(all(&root), Some(&root));
}

#[test]
fn library_round_trip() {
    let Some(dir) = std::env::var_os("OML_CORPUS") else {
        eprintln!("OML_CORPUS not set, skipping");
        return;
    };
    let root = PathBuf::from(dir).join("oml");
    run(all(&root), Some(&root));
}

const FEATURES: &str = r#"
@dc:title "Features"
vocabulary <http://example.com/vocab/features#> as features {
	extends <http://www.w3.org/2001/XMLSchema#> as xsd
	extends <http://purl.org/dc/elements/1.1/> as dc

	annotation property note

	@note "A thing", "with 'two' notes"
	@dc:title "title"
	aspect Thing [
		key id
	]

	scalar property id [
		domain Thing
		range xsd:string
		functional
	]

	scalar Percent = xsd:integer [
		minInclusive 0
		maxInclusive 100
	]

	scalar Code [
		oneOf "a", "b", 3, 4.5, 1.0E3, "x"^^xsd:anyURI, "y"$en
	]

	scalar Word = xsd:string [
		pattern "[a-z]+"
		minLength 1
	]

	scalar property age [
		domain Person
		range xsd:integer
	]

	scalar property yearsOld < age

	scalar property ageInYears = age

	concept Person < Thing [
		restricts some knows to Person
		restricts all knows to Person
		restricts age to exactly 1
		restricts age to min 0 xsd:integer
		restricts id to "none"
		restricts some age to xsd:integer
	]

	concept Adult = Person [
		restricts age to min 18
	]

	concept Pair = Person & Thing [
		restricts knows to max 2 Person
	]

	concept Solo = [
		restricts knows to exactly 0
	]

	concept ^scalar < Thing

	concept Day [
		oneOf dc:monday, dc:tuesday
	]

	relation hasPet [
		from Person
		to ^scalar
	]

	concept Narcissist < Person [
		restricts knows to self
	]

	@note "Who knows whom"
	relation entity Knows [
		from Person
		to Person
		@note "forward"
		forward knows
		@note "reverse"
		reverse knownBy
		transitive
	]

	relation entity Owns [
		from Person
		to Thing
		forward owns
		reverse ownedBy
		functional
		inverse functional
		key since
	]

	scalar property since

	relation entity BestFriend [
		from Person
		to Person
		forward bestFriend
		symmetric
	] < Knows [
		restricts all bestFriend to Person
	]

	relation likes [
		from Person
		to Thing
		reverse likedBy
		asymmetric
		irreflexive
	]

	relation loves < likes

	relation adores = likes

	ref concept dc:Imported < Thing
}
"#;

const DC: &str = r#"
vocabulary <http://purl.org/dc/elements/1.1/> as dc {
	annotation property title
	concept Imported
	relation entity Contains [
		from Imported
		to Imported
		forward contains
	]
}
"#;

fn features_root() -> impl Fn(&str) -> Option<String> {
    |iri: &str| (iri == "http://purl.org/dc/elements/1.1").then(|| DC.to_string())
}

#[test]
fn every_feature_round_trips() {
    let build = Build::new_rc();
    let resolver = features_root();
    let (first, _): (SetOntology<RcStr>, _) =
        read_config(FEATURES, &build, &resolver, Config::default()).unwrap();
    let imports = |iri: &str| -> Option<OntologyInfo> {
        let (o, _): (SetOntology<RcStr>, _) =
            read_config(&resolver(iri)?, &build, &resolver, Config::default()).ok()?;
        OntologyInfo::of(&o)
    };
    let written = writer::write(&first, &imports, writer::Config::default()).unwrap();
    let (second, _): (SetOntology<RcStr>, _) =
        read_config(&written, &build, &resolver, Config::default())
            .unwrap_or_else(|e| panic!("{e}\n{written}"));
    let set = |o: &SetOntology<RcStr>| -> Set { o.iter().map(normalize).collect() };
    let (a, b) = (set(&first), set(&second));
    assert!(
        a == b,
        "lost: {:?}\nextra: {:?}\n{written}",
        a.difference(&b).collect::<Vec<_>>(),
        b.difference(&a).collect::<Vec<_>>()
    );
    // The names that need care.
    assert!(written.contains("concept ^scalar"), "{written}");
    assert!(written.contains("@dc:title \"title\""), "{written}");
    assert!(written.contains("relation entity Knows"), "{written}");
    assert!(written.contains("ref concept dc:Imported"), "{written}");
    if std::env::var_os("OML_SHOW_WRITTEN").is_some() {
        eprintln!("{written}");
    }
}

#[test]
fn refuses_what_oml_cannot_say() {
    let build = Build::new_rc();
    let (mut o, _): (SetOntology<RcStr>, _) = read_config(
        "vocabulary <http://example.com/v#> as v { concept A }",
        &build,
        &|_: &str| None,
        Config::default(),
    )
    .unwrap();
    let a = build.class("http://example.com/v#A");
    let b = build.class("http://example.com/v#B");
    o.insert(DisjointClasses(vec![
        ClassExpression::Class(a),
        ClassExpression::Class(b),
    ]));
    let strict = writer::write(&o, &writer::NoImports, writer::Config::default());
    assert!(matches!(strict, Err(WriteError::Unsupported(_))));
    let lax = writer::write(&o, &writer::NoImports, writer::Config { lax: true }).unwrap();
    assert!(lax.contains("concept A"), "{lax}");
}
