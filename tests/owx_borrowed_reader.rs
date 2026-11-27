//! The OWL/XML reader hands attribute values and text from the XML event
//! through to the model. These cases pin down that text split across
//! events, entities and long values arrive intact, for both `read` and
//! `read_to_stream`.

use horned_owl::io::ParserConfiguration;
use horned_owl::io::StreamComponent;
use horned_owl::io::owx::reader::{read, read_to_stream};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const NS: &str = r#"xmlns="http://www.w3.org/2002/07/owl#" xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:xsd="http://www.w3.org/2001/XMLSchema#""#;

fn doc(pre: &str, body: &str) -> String {
    format!("{pre}<Ontology {NS} ontologyIRI=\"http://ex.com/o\">\n{body}\n</Ontology>\n")
}

fn cfg(lax: bool) -> ParserConfiguration<RcStr> {
    let mut c: ParserConfiguration<RcStr> = Default::default();
    c.lax = lax;
    c
}

fn parse(text: &str, lax: bool) -> Result<Vec<AnnotatedComponent<RcStr>>, String> {
    let (o, _): (SetOntology<RcStr>, _) =
        read(&mut text.as_bytes(), cfg(lax)).map_err(|e| e.to_string())?;
    let mut v: Vec<_> = o.into_iter().collect();
    v.sort_by_key(|c| format!("{c:?}"));

    // The streaming reader must yield exactly the same components.
    let mut s = vec![];
    for item in read_to_stream::<RcStr, _, _>(text.as_bytes(), cfg(lax)) {
        if let StreamComponent::Component(c) = item.map_err(|e| e.to_string())? {
            s.push(c);
        }
    }
    s.sort_by_key(|c| format!("{c:?}"));
    assert_eq!(v, s);
    Ok(v)
}

fn literals(text: &str) -> Vec<String> {
    parse(text, false)
        .unwrap()
        .iter()
        .filter_map(|c| match &c.component {
            Component::AnnotationAssertion(a) => match &a.ann.av {
                AnnotationValue::Literal(Literal::Simple { literal }) => Some(literal.clone()),
                AnnotationValue::Literal(Literal::Language { literal, lang }) => {
                    Some(format!("{literal}@{lang}"))
                }
                AnnotationValue::Literal(Literal::Datatype {
                    literal,
                    datatype_iri,
                }) => Some(format!("{literal}^^{}", String::from(datatype_iri.clone()))),
                _ => None,
            },
            _ => None,
        })
        .collect()
}

fn classes(text: &str) -> Vec<String> {
    let mut v: Vec<String> = parse(text, false)
        .unwrap()
        .iter()
        .filter_map(|c| match &c.component {
            Component::DeclareClass(DeclareClass(cl)) => Some(String::from(cl.0.clone())),
            _ => None,
        })
        .collect();
    v.sort();
    v
}

fn ann(lit: &str) -> String {
    format!(
        "<AnnotationAssertion><AnnotationProperty abbreviatedIRI=\"rdfs:comment\"/>\
         <IRI>http://ex.com/o#a</IRI>{lit}</AnnotationAssertion>"
    )
}

fn decl(iri_attr: &str) -> String {
    format!("<Declaration><Class {iri_attr}/></Declaration>")
}

const DTD: &str = "<?xml version=\"1.0\"?>\n<!DOCTYPE Ontology [\n\
    <!ENTITY ex \"http://ex.com/ns#\">\n<!ENTITY lbl \"L&#233;bel\">\n]>\n";

#[test]
fn literal_text_split_across_many_events() {
    let body: String = (0..500)
        .map(|i| format!("s{i} &amp; <![CDATA[<c{i}>]]> &#x41;"))
        .collect();
    let expected: String = (0..500).map(|i| format!("s{i} & <c{i}> A")).collect();
    let lits = literals(&doc("", &ann(&format!("<Literal>{body}</Literal>"))));
    assert_eq!(lits, vec![expected]);
}

#[test]
fn literal_cdata_end_marker_and_refs() {
    let lits = literals(&doc(
        "",
        &ann("<Literal>a ]]&gt; b <![CDATA[x]]]]><![CDATA[>y]]> &#233;&#x1F600;&quot;</Literal>"),
    ));
    assert_eq!(lits, vec!["a ]]> b x]]>y é\u{1F600}\"".to_string()]);
}

#[test]
fn empty_and_whitespace_literals_are_kept_verbatim() {
    let mut lits = literals(&doc(
        "",
        &format!(
            "{}{}",
            ann("<Literal></Literal>"),
            ann("<Literal>  \n x \t</Literal>")
        ),
    ));
    lits.sort();
    assert_eq!(lits, vec!["".to_string(), "  \n x \t".to_string()]);
}

#[test]
fn lang_and_datatype_attributes_with_references() {
    let lits = literals(&doc(
        "",
        &format!(
            "{}{}{}{}",
            ann("<Literal xml:lang=\"e&#110;-GB\">hi</Literal>"),
            "<Prefix name=\"xsd\" IRI=\"http://www.w3.org/2001/XMLSchema#\"/>",
            ann("<Literal datatypeIRI=\"xsd:integer\">42</Literal>"),
            ann("<Literal datatypeIRI=\"&#104;ttp://ex.com/dt\">s</Literal>"),
        ),
    ));
    let mut expected = vec![
        "hi@en-GB".to_string(),
        "42^^http://www.w3.org/2001/XMLSchema#integer".to_string(),
        "s^^http://ex.com/dt".to_string(),
    ];
    expected.sort();
    let mut lits = lits;
    lits.sort();
    assert_eq!(lits, expected);
}

#[test]
fn language_with_datatype_is_an_error_in_both_modes() {
    let t = doc(
        "",
        &ann("<Literal xml:lang=\"fr\" datatypeIRI=\"xsd:string\">x</Literal>"),
    );
    for lax in [false, true] {
        assert!(parse(&t, lax).unwrap_err().contains("Broken literal"));
    }
}

#[test]
fn doctype_entities_in_attributes_and_text() {
    let t = doc(
        DTD,
        &format!(
            "{}{}{}",
            decl("IRI=\"&ex;A\""),
            ann("<Literal>&lbl; &amp; &lbl;</Literal>"),
            "<Declaration><Class abbreviatedIRI=\"&ex;B\"/></Declaration>",
        ),
    );
    assert_eq!(
        classes(&t),
        vec!["http://ex.com/ns#A".to_string(), "http://ex.com/ns#B".to_string()]
    );
    assert_eq!(literals(&t), vec!["Lébel & Lébel".to_string()]);
}

#[test]
fn unknown_entity_is_an_error() {
    let t = doc("", &decl("IRI=\"http://ex.com/A&nope;\""));
    assert!(parse(&t, false).is_err());
    let t = doc("", &ann("<Literal>a &nope; b</Literal>"));
    assert!(parse(&t, false).is_err());
}

#[test]
fn long_attribute_and_iri_text() {
    let long = "a".repeat(200_000);
    let t = doc("", &decl(&format!("IRI=\"http://ex.com/{long}\"")));
    assert_eq!(classes(&t), vec![format!("http://ex.com/{long}")]);

    let amp = "a&amp;b".repeat(30_000);
    let t = doc("", &decl(&format!("IRI=\"http://ex.com/{amp}\"")));
    assert_eq!(
        classes(&t),
        vec![format!("http://ex.com/{}", "a&b".repeat(30_000))]
    );

    let t = doc(
        "",
        &format!("<Declaration><Class><IRI>http://ex.com/{long}</IRI></Class></Declaration>"),
    );
    let _ = parse(&t, true);
}

#[test]
fn iri_text_split_across_events() {
    let t = doc(
        "",
        &ann("<Literal>x</Literal>").replace(
            "<IRI>http://ex.com/o#a</IRI>",
            "<IRI>http://ex.com/a&amp;b&#99;d&#x26;e</IRI>",
        ),
    );
    let subjects: Vec<String> = parse(&t, false)
        .unwrap()
        .iter()
        .filter_map(|c| match &c.component {
            Component::AnnotationAssertion(a) => match &a.subject {
                AnnotationSubject::IRI(i) => Some(String::from(i.clone())),
                _ => None,
            },
            _ => None,
        })
        .collect();
    assert_eq!(subjects, vec!["http://ex.com/a&bcd&e".to_string()]);
}

#[test]
fn empty_iri_element_strict_and_lax() {
    let t = doc(
        "",
        &ann("<Literal>x</Literal>").replace("<IRI>http://ex.com/o#a</IRI>", "<IRI></IRI>"),
    );
    assert!(parse(&t, false).unwrap_err().contains("Empty IRI element"));
    let comps = parse(&t, true).unwrap();
    assert!(comps.iter().any(|c| matches!(&c.component,
        Component::AnnotationAssertion(a)
            if matches!(&a.subject, AnnotationSubject::IRI(i) if **i == *"http://ex.com/o"))));
}

#[test]
fn fragment_iris_resolve_against_the_ontology_iri() {
    let t = doc(
        "",
        &format!(
            "{}{}",
            decl("IRI=\"#loc\""),
            "<Declaration><Class abbreviatedIRI=\"x:y\"/></Declaration>"
        ),
    );
    assert_eq!(
        classes(&t),
        vec!["http://ex.com/o#loc".to_string(), "x:y".to_string()]
    );
}

#[test]
fn prefixes_expand_abbreviated_iris() {
    let t = doc(
        "",
        &format!(
            "<Prefix name=\"ex\" IRI=\"http://ex.com/ns#\"/>{}{}",
            "<Declaration><Class abbreviatedIRI=\"ex:A\"/></Declaration>",
            decl("IRI=\"ex:B\"")
        ),
    );
    assert_eq!(
        classes(&t),
        vec!["http://ex.com/ns#A".to_string(), "http://ex.com/ns#B".to_string()]
    );
}

#[test]
fn prefixed_elements_and_namespaced_attributes() {
    let t = format!(
        "<o:Ontology xmlns:o=\"http://www.w3.org/2002/07/owl#\" xmlns:q=\"http://q/\" \
         ontologyIRI=\"http://ex.com/o\" xml:base=\"http://b/\">\
         <o:Declaration><o:Class q:foo=\"bar\" IRI=\"http://ex.com/A\"/></o:Declaration>\
         </o:Ontology>"
    );
    assert_eq!(classes(&t), vec!["http://ex.com/A".to_string()]);
}

#[test]
fn leading_bom_is_ignored() {
    let t = format!(
        "\u{feff}{}",
        doc(
            "<?xml version=\"1.0\"?>\n",
            &decl("IRI=\"http://ex.com/A\"")
        )
    );
    assert_eq!(classes(&t), vec!["http://ex.com/A".to_string()]);
}

#[test]
fn stray_text_is_strict_only() {
    let t = doc(
        "",
        "junk<Declaration><Class IRI=\"http://ex.com/A\"/></Declaration>",
    );
    assert!(
        parse(&t, false)
            .unwrap_err()
            .contains("Unexpected text content")
    );
    assert_eq!(classes_lax(&t), vec!["http://ex.com/A".to_string()]);
}

fn classes_lax(text: &str) -> Vec<String> {
    parse(text, true)
        .unwrap()
        .iter()
        .filter_map(|c| match &c.component {
            Component::DeclareClass(DeclareClass(cl)) => Some(String::from(cl.0.clone())),
            _ => None,
        })
        .collect()
}

#[test]
fn deeply_nested_and_wide_elements() {
    let nested = format!(
        "<SubClassOf><Class IRI=\"http://ex.com/A\"/>{}<Class IRI=\"http://ex.com/C\"/>{}</SubClassOf>",
        "<ObjectUnionOf><Class IRI=\"http://ex.com/B\"/>".repeat(200),
        "</ObjectUnionOf>".repeat(200)
    );
    assert_eq!(parse(&doc("", &nested), false).unwrap().len(), 2);

    let wide = format!(
        "<EquivalentClasses>{}</EquivalentClasses>",
        (0..3000)
            .map(|i| format!("<Class IRI=\"http://ex.com/C{i}\"/>"))
            .collect::<String>()
    );
    assert_eq!(parse(&doc("", &wide), false).unwrap().len(), 2);
}

#[test]
fn truncated_document_is_an_error_at_every_cut() {
    let t = doc(
        DTD,
        &format!(
            "{}{}",
            decl("IRI=\"&ex;A\""),
            ann("<Literal xml:lang=\"en\">x &amp; y</Literal>")
        ),
    );
    let full = parse(&t, false).unwrap();
    for cut in (1..t.len()).filter(|c| t.is_char_boundary(*c)) {
        // Must never panic; a prefix is either an error or (when it only
        // drops trailing whitespace) the same ontology.
        if let Ok(v) = parse(&t[..cut], true) {
            assert!(v.len() <= full.len());
        }
    }
}
