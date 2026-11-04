//! Axioms that differ only in their annotations are distinct axioms.
//!
//! OWL 2 Structural Specification, section 9: "axioms will not be structurally equivalent if
//! their axiom annotations are not structurally equivalent". So an ontology holds both a bare
//! and an annotated copy of the same axiom, and two copies with different annotations. The
//! OFN, OWL/XML, Manchester and OBO readers already do this; the RDF reader used to fold them.
//!
//! The six graphs are the manual graphs in `ontology-corpus/manual-graphs`. They are hand-built
//! because what matters is the exact triples and their order, which no other syntax reproduces.
//! An RDF graph is a set of triples, so every graph is also read in many random orders.

use std::rc::Rc;

use horned_owl::io::rdf::reader::{ConcreteRDFOntology, read};
use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const E: &str = "http://example.com/t#";
const OWL: &str = "http://www.w3.org/2002/07/owl#";
const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";

/// Expand one term of the short form used by the fixtures below.
fn term(t: &str) -> String {
    if t == "a" {
        format!("<{RDF}type>")
    } else if t == "." || t.starts_with("_:") || t.starts_with('"') {
        t.to_string()
    } else if let Some(r) = t.strip_prefix("owl:") {
        format!("<{OWL}{r}>")
    } else if let Some(r) = t.strip_prefix("rdfs:") {
        format!("<{RDFS}{r}>")
    } else if let Some(r) = t.strip_prefix(':') {
        format!("<{E}{r}>")
    } else {
        panic!("unexpected term {t}")
    }
}

/// N-Triples lines, one per statement, in the order given.
fn lines(statements: &[&str]) -> Vec<String> {
    let out: Vec<String> = [
        "<http://example.com/t> a owl:Ontology .",
        ":A a owl:Class .",
        ":B a owl:Class .",
        ":src a owl:AnnotationProperty .",
        ":p a owl:ObjectProperty .",
    ]
    .iter()
    .chain(statements)
    .map(|s| {
        s.split_whitespace()
            .map(|t| {
                if t.starts_with("<http") {
                    t.to_string()
                } else {
                    term(t)
                }
            })
            .collect::<Vec<_>>()
            .join(" ")
    })
    .collect();
    out
}

/// The `owl:Axiom` node `n` annotating `:A subClassOf target` with `:src value`.
fn axiom(n: &str, target: &str, value: &str) -> Vec<String> {
    [
        format!("{n} a owl:Axiom ."),
        format!("{n} owl:annotatedSource :A ."),
        format!("{n} owl:annotatedProperty rdfs:subClassOf ."),
        format!("{n} owl:annotatedTarget {target} ."),
        format!("{n} :src \"{value}\" ."),
    ]
    .into()
}

fn restriction(n: &str) -> Vec<String> {
    [
        format!(":A rdfs:subClassOf {n} ."),
        format!("{n} a owl:Restriction ."),
        format!("{n} owl:onProperty :p ."),
        format!("{n} owl:someValuesFrom :B ."),
    ]
    .into()
}

fn graph(parts: Vec<Vec<String>>) -> Vec<String> {
    let stmts: Vec<String> = parts.into_iter().flatten().collect();
    let refs: Vec<&str> = stmts.iter().map(|s| s.as_str()).collect();
    lines(&refs)
}

/// What the reader makes of `nt`: for each SubClassOf, the `:src` values it carries, sorted.
fn subclass_of(nt: &[String]) -> Vec<Vec<String>> {
    let config = RDFParserConfiguration {
        common: ParserConfiguration::default(),
        format: Some(oxrdfio::RdfFormat::NTriples),
    };
    let (ont, incomplete): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
        read(&mut nt.join("\n").as_bytes(), config).unwrap();
    assert!(incomplete.is_complete(), "parse left triples behind");
    let ont: SetOntology<RcStr> = ont.into();

    let mut found: Vec<Vec<String>> = ont
        .iter()
        .filter(|ac| matches!(ac.component, Component::SubClassOf(_)))
        .map(|ac| {
            let mut vals: Vec<String> = ac
                .ann
                .iter()
                .map(|a| match &a.av {
                    AnnotationValue::Literal(Literal::Simple { literal }) => literal.clone(),
                    other => panic!("unexpected annotation value {other:?}"),
                })
                .collect();
            vals.sort();
            vals
        })
        .collect();
    found.sort();
    found
}

fn expect(sets: &[&[&str]]) -> Vec<Vec<String>> {
    let mut v: Vec<Vec<String>> = sets
        .iter()
        .map(|s| s.iter().map(|x| x.to_string()).collect())
        .collect();
    v.sort();
    v
}

/// Read `nt` as given, then in many random orders: the answer must always be `expected`.
fn check(nt: Vec<String>, expected: Vec<Vec<String>>) {
    assert_eq!(subclass_of(&nt), expected, "as written");

    let mut shuffled = nt;
    let mut seed: u64 = 20261104;
    for round in 0..50 {
        for i in (1..shuffled.len()).rev() {
            seed = seed
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            shuffled.swap(i, (seed >> 33) as usize % (i + 1));
        }
        assert_eq!(
            subclass_of(&shuffled),
            expected,
            "round {round}, order:\n{}",
            shuffled.join("\n")
        );
    }
}

/// Graph 1: one base triple, two `owl:Axiom` nodes. The reader used to keep only the last.
#[test]
fn one_triple_two_annotation_nodes() {
    check(
        graph(vec![
            vec![":A rdfs:subClassOf :B .".into()],
            axiom("_:ax1", ":B", "FMA"),
            axiom("_:ax2", ":B", "MA"),
        ]),
        expect(&[&["FMA"], &["MA"]]),
    );
}

/// Graph 2: the identical triple twice is one statement.
#[test]
fn duplicate_triple_is_one_axiom() {
    let mut nt = graph(vec![vec![":A rdfs:subClassOf :B .".into()]]);
    nt.push(nt.last().unwrap().clone());
    check(nt, expect(&[&[]]));
}

/// Graph 3: two restriction blank nodes with the same content, each annotated differently.
#[test]
fn restriction_two_annotated_copies() {
    check(
        graph(vec![
            restriction("_:r1"),
            axiom("_:ax1", "_:r1", "FMA"),
            restriction("_:r2"),
            axiom("_:ax2", "_:r2", "MA"),
        ]),
        expect(&[&["FMA"], &["MA"]]),
    );
}

/// Graphs 4 and 5: a bare and an annotated copy are two axioms, whichever the reader meets first.
#[test]
fn restriction_bare_and_annotated_copy() {
    check(
        graph(vec![
            restriction("_:r1"),
            restriction("_:r2"),
            axiom("_:ax1", "_:r2", "MA"),
        ]),
        expect(&[&[], &["MA"]]),
    );
}

/// Graph 6: an annotated triple written twice is one annotated axiom, not that plus a bare one.
#[test]
fn annotated_triple_twice_is_one_annotated_axiom() {
    let mut nt = graph(vec![
        vec![":A rdfs:subClassOf :B .".into()],
        axiom("_:ax1", ":B", "MA"),
    ]);
    let base = nt
        .iter()
        .find(|l| l.contains("subClassOf>") && !l.contains("annotated"))
        .unwrap()
        .clone();
    nt.push(base);
    check(nt, expect(&[&["MA"]]));
}
