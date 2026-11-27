use std::collections::HashSet;
use std::path::PathBuf;

use rstest::rstest;

use crate::model::RcStr;
use crate::ontology::set::SetOntology;

fn nom_read(
    doc: &str,
) -> Result<(SetOntology<RcStr>, curie::PrefixMapping), crate::error::HornedError> {
    super::read(&mut doc.as_bytes(), Default::default())
}

/// Errors must say what is wrong and where.
/// Inputs that pest rejects must also be rejected, and the error must point
/// at the right place.
#[rstest]
#[case("Ontology(SubClassOf(<http://e.com/a>))", "line 1, column 37")]
#[case(
    "Ontology(\n  Foo(<http://e.com/a>))",
    "unknown or unsupported axiom `Foo`"
)]
#[case("Ontology(SubClassOf(x:a x:b))", "undefined prefix")]
#[case("Ontology(EquivalentClasses(<http://e.com/a>))", "at least two")]
#[case("Ontology(Declaration(Class(<http://e.com/a>))", "expected")]
#[case("Ontology() trailing", "end of input")]
#[case(
    "Ontology(DataSomeValuesFrom(<http://e.com/a> <http://e.com/b> <http://e.com/c>))",
    "unknown or unsupported axiom"
)]
#[case(
    "Ontology(SubClassOf(DataSomeValuesFrom(<http://e.com/a> <http://e.com/b> <http://e.com/c>) <http://e.com/d>))",
    "data property chaining"
)]
fn rejects_with_message(#[case] doc: &str, #[case] message: &str) {
    let nom = nom_read(doc).map(|_| ()).expect_err("nom should fail");
    let text = format!("{nom}");
    let debug = format!("{nom:?}");
    assert!(
        text.contains(message) || debug.contains(message),
        "error `{text}` / `{debug}` does not mention `{message}`"
    );
}

#[test]
fn error_message_is_readable() {
    let e = nom_read("Ontology(\n  SubClassOf(<http://e.com/a> ,)\n)").unwrap_err();
    let s = match e {
        crate::error::HornedError::ParserError(e, _) => e.to_string(),
        e => panic!("{e:?}"),
    };
    assert!(s.contains("line 2, column 31"), "{s}");
    assert!(s.contains("in SubClassOf"), "{s}");
    assert!(s.contains("^"), "{s}");
}

#[test]
fn unknown_keyword_is_located() {
    let e = nom_read("Ontology(\n  Declaration(Class(<http://e.com/a>))\n  Foo(<http://e.com/a>))")
        .unwrap_err();
    let s = e.to_string();
    assert!(s.contains("unknown or unsupported axiom `Foo`"), "{s}");
    assert!(s.contains("line 3, column 3"), "{s}");
}

/// Reading in tiny chunks, so that every construct straddles a refill, must
/// give the same ontology as reading in one go.
#[rstest]
fn chunked_same_as_whole(#[files("src/ont/owl-functional/*.ofn")] resource: PathBuf) {
    let doc = slurp::read_all_to_string(&resource).unwrap();
    let (whole, whole_pm) = nom_read(&doc).unwrap();
    for chunk in [1, 3, 7, 64] {
        let build = crate::model::Build::new_rc();
        let mut reader =
            super::Reader::new(doc.as_bytes(), crate::io::ParserConfiguration::new(&build))
                .with_chunk_size(chunk);
        let mut ont = SetOntology::<RcStr>::default();
        for item in reader.by_ref() {
            if let crate::io::StreamComponent::Component(ac) =
                item.unwrap_or_else(|e| panic!("chunk {chunk}: {e}"))
            {
                crate::model::MutableOntology::insert(&mut ont, ac);
            }
        }
        assert_eq!(
            reader.prefixes().mappings().collect::<HashSet<_>>(),
            whole_pm.mappings().collect::<HashSet<_>>()
        );
        pretty_assertions::assert_eq!(ont, whole, "chunk size {chunk}");
    }
}

#[test]
fn stream_yields_document_order() {
    use crate::io::StreamComponent as S;
    let build = crate::model::Build::new_rc();
    let doc = "Prefix(:=<http://e.com/>)\nPrefix(a:=<http://a.com/>)\n\
               Ontology(<http://e.com/o>\n Import(<http://e.com/i>)\n \
               Annotation(a:p \"x\")\n Declaration(Class(:C)))";
    let items: Vec<_> =
        super::read_to_stream(doc.as_bytes(), crate::io::ParserConfiguration::new(&build))
            .collect::<Result<_, _>>()
            .unwrap();
    let kinds: Vec<_> = items
        .iter()
        .map(|i| match i {
            S::Prefix(..) => "prefix",
            S::Component(ac) => match ac.component {
                crate::model::Component::OntologyID(_) => "id",
                crate::model::Component::Import(_) => "import",
                crate::model::Component::OntologyAnnotation(_) => "annotation",
                _ => "axiom",
            },
        })
        .collect();
    assert_eq!(
        kinds,
        ["prefix", "prefix", "id", "import", "annotation", "axiom"]
    );
}

#[rstest]
#[case("Ontology(Declaration(Class(<http://e.com/a>))\n", "expected")]
#[case("Ontology(Declaration(Class(<http://e.com/a>)))\n# ok\n  \n", "")]
#[case("Ontology(Declaration(Class(<http://e.com/a>))) x", "end of input")]
fn stream_endings(#[case] doc: &str, #[case] err: &str) {
    for chunk in [1, 5, 1000] {
        let build = crate::model::Build::new_rc();
        let r: Result<Vec<_>, _> =
            super::Reader::new(doc.as_bytes(), crate::io::ParserConfiguration::new(&build))
                .with_chunk_size(chunk)
                .collect();
        match (r, err) {
            (Ok(_), "") => {}
            (Err(e), m) if !m.is_empty() => assert!(e.to_string().contains(m), "{e}"),
            (r, _) => panic!("chunk {chunk}: unexpected {r:?}"),
        }
    }
}

#[test]
fn stream_error_position_survives_refills() {
    let doc =
        "Ontology(\n  Declaration(Class(<http://e.com/a>))\n  SubClassOf(<http://e.com/a> ,)\n)";
    let build = crate::model::Build::new_rc();
    let e = super::Reader::new(doc.as_bytes(), crate::io::ParserConfiguration::new(&build))
        .with_chunk_size(4)
        .find_map(Result::err)
        .expect("should fail");
    let s = e.to_string();
    assert!(s.contains("line 3, column 31"), "{s}");
    assert!(s.contains("SubClassOf(<http://e.com/a> ,)"), "{s}");
}

/// Stream straight into the OFN writer, with no ontology in between.
#[rstest]
fn streamed_roundtrip(#[files("src/ont/owl-functional/*.ofn")] resource: PathBuf) {
    let doc = slurp::read_all_to_string(&resource).unwrap();
    let (expected, _) = nom_read(&doc).unwrap();

    let build = crate::model::Build::new_rc();
    let mut out = Vec::new();
    crate::io::ofn::writer::write_stream(
        &mut out,
        super::read_to_stream(doc.as_bytes(), crate::io::ParserConfiguration::new(&build)),
    )
    .unwrap();

    let (actual, _) = nom_read(std::str::from_utf8(&out).unwrap()).unwrap();
    pretty_assertions::assert_eq!(actual, expected);
}

/// `owl:`, `rdf:`, `rdfs:` and `xsd:` are predefined; the returned mapping
/// still holds only the declared prefixes.
#[test]
fn predefined_prefixes() {
    let body = "SubClassOf(:A owl:Thing) DataPropertyRange(:p xsd:string) \
                AnnotationAssertion(rdfs:label :A \"A\") AnnotationAssertion(rdfs:seeAlso :A rdf:nil))";
    let explicit = format!(
        "Prefix(:=<http://example.com/>) \
         Prefix(owl:=<http://www.w3.org/2002/07/owl#>) \
         Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>) \
         Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>) \
         Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>) Ontology({body}"
    );
    let implicit = format!("Prefix(:=<http://example.com/>) Ontology({body}");
    let (expected, _) = nom_read(&explicit).unwrap();

    let in_memory: (SetOntology<RcStr>, curie::PrefixMapping) =
        super::read_in_memory(&mut implicit.as_bytes(), Default::default()).unwrap();
    let streamed = nom_read(&implicit).unwrap();
    for (ont, mapping) in [in_memory, streamed] {
        pretty_assertions::assert_eq!(ont, expected);
        assert_eq!(
            mapping
                .mappings()
                .map(|(p, _)| p.as_str())
                .collect::<Vec<_>>(),
            vec![""]
        );
    }
}

/// A document may bind a predefined prefix to another namespace.
#[test]
fn predefined_prefix_can_be_redeclared() {
    let (ont, _) =
        nom_read("Prefix(owl:=<http://example.com/>) Ontology(Declaration(Class(owl:Thing)))")
            .unwrap();
    let iris: Vec<String> = ont.iter().map(|c| format!("{:?}", c.component)).collect();
    assert!(
        iris.iter().any(|c| c.contains("http://example.com/Thing")),
        "{iris:?}"
    );
}
