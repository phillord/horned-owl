//! An annotated property chain axiom whose `owl:annotatedTarget` is a copy of the list.
//!
//! The OWL API writes the annotated target as a fresh `rdf:List` rather than sharing the list
//! node of the `owl:propertyChainAxiom` triple, and the bubo fixtures cannot be used for this:
//! the RDF/XML writer does not round trip a shared list head, which is what a bubo fixture
//! would also exercise.

use std::rc::Rc;

use horned_owl::io::rdf::reader::{ConcreteRDFOntology, read};
use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const OWL: &str = "http://www.w3.org/2002/07/owl#";
const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
const E: &str = "http://example.com/t#";

/// The triples of a list of `items` rooted at the blank node `_:{name}0`.
fn list(name: &str, items: &[&str]) -> Vec<String> {
    let mut out = vec![];
    for (i, item) in items.iter().enumerate() {
        let rest = if i + 1 == items.len() {
            format!("<{RDF}nil>")
        } else {
            format!("_:{name}{}", i + 1)
        };
        out.push(format!("_:{name}{i} <{RDF}first> <{E}{item}> ."));
        out.push(format!("_:{name}{i} <{RDF}rest> {rest} ."));
    }
    out
}

/// A chain axiom for `sup`, annotated with `comment`, whose target is the list `target`.
fn chain(sup: &str, list_name: &str, items: &[&str], target: &str, comment: &str) -> Vec<String> {
    let mut out = list(list_name, items);
    out.push(format!(
        "<{E}{sup}> <{OWL}propertyChainAxiom> _:{list_name}0 ."
    ));
    let ax = format!("_:ax{sup}");
    out.extend([
        format!("{ax} <{RDF}type> <{OWL}Axiom> ."),
        format!("{ax} <{OWL}annotatedSource> <{E}{sup}> ."),
        format!("{ax} <{OWL}annotatedProperty> <{OWL}propertyChainAxiom> ."),
        format!("{ax} <{OWL}annotatedTarget> _:{target}0 ."),
        format!("{ax} <http://www.w3.org/2000/01/rdf-schema#comment> \"{comment}\" ."),
    ]);
    out
}

fn graph(parts: Vec<Vec<String>>) -> String {
    let mut nt = vec![
        format!("<{E}o> <{RDF}type> <{OWL}Ontology> ."),
        format!(
            "<http://www.w3.org/2000/01/rdf-schema#comment> <{RDF}type> <{OWL}AnnotationProperty> ."
        ),
    ];
    for p in ["p", "q", "r", "s"] {
        nt.push(format!("<{E}{p}> <{RDF}type> <{OWL}ObjectProperty> ."));
    }
    nt.extend(parts.into_iter().flatten());
    nt.join("\n")
}

/// The chain axioms read, as (super property, annotation values), sorted.
fn chains(nt: &str) -> Vec<(String, Vec<String>)> {
    let config = RDFParserConfiguration {
        common: ParserConfiguration::default(),
        format: Some(oxrdfio::RdfFormat::NTriples),
    };
    let (ont, incomplete): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
        read(&mut nt.as_bytes(), config).unwrap();
    assert!(incomplete.is_complete(), "parse left triples behind");
    let ont: SetOntology<RcStr> = ont.into();

    let mut found: Vec<_> = ont
        .iter()
        .filter_map(|ac| match &ac.component {
            Component::SubObjectPropertyOf(SubObjectPropertyOf {
                sub: SubObjectPropertyExpression::ObjectPropertyChain(_),
                sup: ObjectPropertyExpression::ObjectProperty(ObjectProperty(sup)),
            }) => Some((
                sup.to_string().replace(E, ""),
                ac.ann
                    .iter()
                    .map(|a| match &a.av {
                        AnnotationValue::Literal(Literal::Simple { literal }) => literal.clone(),
                        other => panic!("unexpected annotation value {other:?}"),
                    })
                    .collect(),
            )),
            _ => None,
        })
        .collect();
    found.sort();
    found
}

fn expected(v: &[(&str, &[&str])]) -> Vec<(String, Vec<String>)> {
    v.iter()
        .map(|(s, a)| (s.to_string(), a.iter().map(|x| x.to_string()).collect()))
        .collect()
}

#[test]
fn target_is_a_copy_of_the_list() {
    let nt = graph(vec![
        chain("p", "l", &["q", "r"], "c", "Why"),
        list("c", &["q", "r"]),
    ]);
    assert_eq!(chains(&nt), expected(&[("p", &["Why"])]));
}

#[test]
fn target_is_the_shared_list() {
    let nt = graph(vec![chain("p", "l", &["q", "r"], "l", "Why")]);
    assert_eq!(chains(&nt), expected(&[("p", &["Why"])]));
}

/// Two chains over the same members are told apart by their super property.
#[test]
fn copies_for_chains_with_equal_members() {
    let nt = graph(vec![
        chain("p", "l", &["q", "r"], "c", "First"),
        list("c", &["q", "r"]),
        chain("s", "m", &["q", "r"], "d", "Second"),
        list("d", &["q", "r"]),
    ]);
    assert_eq!(
        chains(&nt),
        expected(&[("p", &["First"]), ("s", &["Second"])])
    );
}
