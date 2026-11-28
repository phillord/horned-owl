//! `<Literal/>` and `<IRI/>` are the same OWL/XML as `<Literal></Literal>`
//! and `<IRI></IRI>`; they must read identically and consume nothing after
//! them, through both `read` and `read_to_stream`.

use horned_owl::io::ParserConfiguration;
use horned_owl::io::StreamComponent;
use horned_owl::io::owx::reader::{read, read_to_stream};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const NS: &str = r#"xmlns="http://www.w3.org/2002/07/owl#""#;

fn doc(body: &str) -> String {
    format!("<Ontology {NS} ontologyIRI=\"http://ex.com/o\">\n{body}\n</Ontology>\n")
}

fn parse(text: &str, lax: bool) -> Result<Vec<AnnotatedComponent<RcStr>>, String> {
    let cfg = || {
        let mut c: ParserConfiguration<RcStr> = Default::default();
        c.lax = lax;
        c
    };
    let (o, _): (SetOntology<RcStr>, _) =
        read(&mut text.as_bytes(), cfg()).map_err(|e| e.to_string())?;
    let mut v: Vec<_> = o.into_iter().collect();
    v.sort_by_key(|c| format!("{c:?}"));

    let mut s = vec![];
    for item in read_to_stream::<RcStr, _, _>(text.as_bytes(), cfg()) {
        if let StreamComponent::Component(c) = item.map_err(|e| e.to_string())? {
            s.push(c);
        }
    }
    s.sort_by_key(|c| format!("{c:?}"));
    assert_eq!(v, s);
    Ok(v)
}

const TAIL: &str = r#"<Declaration><Class IRI="http://ex.com/C"/></Declaration>
<AnnotationAssertion><AnnotationProperty IRI="http://ex.com/q"/><IRI>http://ex.com/s</IRI><Literal>after</Literal></AnnotationAssertion>"#;

fn literal_doc(lit: &str) -> String {
    doc(&format!(
        r#"<AnnotationAssertion><AnnotationProperty IRI="http://ex.com/p"/><IRI>http://ex.com/s</IRI>{lit}</AnnotationAssertion>
{TAIL}"#
    ))
}

#[test]
fn empty_literal_forms_agree() {
    for lit in [
        (r#"<Literal/>"#, r#"<Literal></Literal>"#),
        (
            r#"<Literal datatypeIRI="http://ex.com/dt"/>"#,
            r#"<Literal datatypeIRI="http://ex.com/dt"></Literal>"#,
        ),
        (
            r#"<Literal xml:lang="en"/>"#,
            r#"<Literal xml:lang="en"></Literal>"#,
        ),
    ] {
        for lax in [false, true] {
            let empty = parse(&literal_doc(lit.0), lax).unwrap();
            let explicit = parse(&literal_doc(lit.1), lax).unwrap();
            assert_eq!(empty, explicit, "{} lax={lax}", lit.0);
            assert_eq!(empty.len(), 4, "{} lax={lax}", lit.0);
        }
    }
}

#[test]
fn empty_literal_has_empty_lexical_form() {
    let v = parse(
        &literal_doc(r#"<Literal datatypeIRI="http://ex.com/dt"/>"#),
        false,
    )
    .unwrap();
    assert!(v.iter().any(|c| matches!(
        &c.component,
        Component::AnnotationAssertion(a)
            if matches!(&a.ann.av, AnnotationValue::Literal(Literal::Datatype{literal, ..}) if literal.is_empty())
    )));
}

fn iri_doc(iri: &str) -> String {
    doc(&format!(
        r#"<AnnotationAssertion><AnnotationProperty IRI="http://ex.com/p"/>{iri}<Literal>v</Literal></AnnotationAssertion>
{TAIL}"#
    ))
}

#[test]
fn empty_iri_forms_agree_in_lax() {
    let empty = parse(&iri_doc("<IRI/>"), true).unwrap();
    let explicit = parse(&iri_doc("<IRI></IRI>"), true).unwrap();
    assert_eq!(empty, explicit);
    assert_eq!(empty.len(), 4);
}

#[test]
fn empty_iri_forms_agree_in_strict() {
    let empty = parse(&iri_doc("<IRI/>"), false).unwrap_err();
    let explicit = parse(&iri_doc("<IRI></IRI>"), false).unwrap_err();
    assert!(empty.contains("Empty IRI"), "{empty}");
    assert!(explicit.contains("Empty IRI"), "{explicit}");
}
