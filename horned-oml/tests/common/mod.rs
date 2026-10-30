//! What the fixture and command line tests share: reading oml2owl's output
//! and putting both sides into a form that can be compared.

#![allow(dead_code)]

use std::collections::BTreeSet;
use std::fs;
use std::io::BufReader;
use std::path::Path;

use horned_owl::io::ParserConfiguration;
use horned_owl::io::ofn::reader::read as read_ofn;
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

pub type Set = BTreeSet<AnnotatedComponent<RcStr>>;

pub fn sorted<T: Ord>(mut v: Vec<T>) -> Vec<T> {
    v.sort();
    v
}

pub fn ce(c: &ClassExpression<RcStr>) -> ClassExpression<RcStr> {
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
pub fn normalize(ac: &AnnotatedComponent<RcStr>) -> AnnotatedComponent<RcStr> {
    let component: Component<RcStr> = match &ac.component {
        Component::EquivalentClasses(EquivalentClasses(v)) => {
            EquivalentClasses(sorted(v.iter().map(ce).collect())).into()
        }
        Component::DisjointUnion(DisjointUnion(c, v)) => {
            let mut v: Vec<_> = v.iter().map(ce).collect();
            if v.len() == 1 {
                v.push(ClassExpression::Class(Class(
                    Build::new_rc().iri("http://www.w3.org/2002/07/owl#Nothing"),
                )));
            }
            DisjointUnion(c.clone(), sorted(v)).into()
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
pub fn declared(c: &Component<RcStr>) -> Option<String> {
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
pub fn namespace(o: &Set) -> String {
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

/// The number of class expressions in the rest of an axiom's line: an IRI, a
/// prefixed name, or a name followed by balanced parentheses.
pub fn operands(rest: &str) -> usize {
    let mut chars = rest.chars().peekable();
    let mut n = 0;
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
            continue;
        }
        n += 1;
        if c == '<' {
            for c in chars.by_ref() {
                if c == '>' {
                    break;
                }
            }
            continue;
        }
        while chars
            .peek()
            .is_some_and(|c| !c.is_whitespace() && *c != '(')
        {
            chars.next();
        }
        if chars.peek() == Some(&'(') {
            let mut depth = 0;
            for c in chars.by_ref() {
                match c {
                    '(' => depth += 1,
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    n
}

/// oml2owl writes a disjoint union of one class, which functional syntax does
/// not allow and so the reader will not take. Both sides get a second,
/// empty, class so that it can be compared.
pub fn with_empty_second_operand(text: &str) -> String {
    text.lines()
        .map(|line| match line.strip_prefix("DisjointUnion(") {
            Some(rest) => {
                let rest = rest.strip_suffix(')').unwrap();
                // The first operand is the class, then the disjoint ones.
                let after_class = rest.split_once("> ").map_or("", |(_, r)| r);
                if operands(after_class) == 1 {
                    format!("DisjointUnion({rest} owl:Nothing)")
                } else {
                    line.to_string()
                }
            }
            None => line.to_string(),
        })
        .collect::<Vec<_>>()
        .join("\n")
}

/// oml2owl only declares terms of the ontology itself, but the OWL API
/// writes a declaration for every other entity that is used.
pub fn expected(path: &Path, ns: &str) -> Set {
    let text = with_empty_second_operand(&fs::read_to_string(path).unwrap());
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
