//! CDATA inside `<IRI>` is character data, identical to the same text
//! unescaped, through both `read` and `read_to_stream`.

use horned_owl::io::ParserConfiguration;
use horned_owl::io::StreamComponent;
use horned_owl::io::owx::reader::{read, read_to_stream};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;

const NS: &str = r#"xmlns="http://www.w3.org/2002/07/owl#""#;

fn doc(subject: &str) -> String {
    format!(
        r#"<Ontology {NS} ontologyIRI="http://ex.com/o">
<AnnotationAssertion><AnnotationProperty IRI="http://ex.com/p"/>{subject}<Literal>v</Literal></AnnotationAssertion>
</Ontology>
"#
    )
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

fn subject_iri(v: &[AnnotatedComponent<RcStr>]) -> String {
    v.iter()
        .find_map(|c| match &c.component {
            Component::AnnotationAssertion(a) => match &a.subject {
                AnnotationSubject::IRI(i) => Some(i.to_string()),
                _ => None,
            },
            _ => None,
        })
        .expect("annotation assertion with IRI subject")
}

#[test]
fn cdata_iri_reads_as_text() {
    for lax in [false, true] {
        let plain = parse(&doc("<IRI>http://ex.com/a</IRI>"), lax).unwrap();
        let cdata = parse(&doc("<IRI><![CDATA[http://ex.com/a]]></IRI>"), lax).unwrap();
        assert_eq!(plain, cdata, "lax={lax}");
        assert_eq!(subject_iri(&cdata), "http://ex.com/a");
    }
}

#[test]
fn cdata_mixed_with_text_and_entity() {
    for lax in [false, true] {
        let v = parse(&doc("<IRI>http://ex.com/a<![CDATA[b]]>c&amp;d</IRI>"), lax).unwrap();
        assert_eq!(subject_iri(&v), "http://ex.com/abc&d", "lax={lax}");
    }
}

#[test]
fn cdata_keeps_markup_characters_literal() {
    for lax in [false, true] {
        let v = parse(&doc("<IRI>http://ex.com/<![CDATA[a&b<c]]></IRI>"), lax).unwrap();
        assert_eq!(subject_iri(&v), "http://ex.com/a&b<c", "lax={lax}");
    }
}

#[test]
fn empty_cdata_is_still_an_empty_iri() {
    let doc_ = doc("<IRI><![CDATA[]]></IRI>");
    let e = parse(&doc_, false).unwrap_err();
    assert!(e.contains("Empty IRI"), "{e}");
    let lax = parse(&doc_, true).unwrap();
    assert_eq!(lax, parse(&doc("<IRI></IRI>"), true).unwrap());
}
