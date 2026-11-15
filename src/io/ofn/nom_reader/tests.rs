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

fn pest_read(
    doc: &str,
) -> Result<(SetOntology<RcStr>, curie::PrefixMapping), crate::error::HornedError> {
    crate::io::ofn::reader::read(&mut doc.as_bytes(), Default::default())
}

/// The nom reader must produce exactly what the pest reader does.
#[rstest]
fn same_as_pest(#[files("src/ont/owl-functional/*.ofn")] resource: PathBuf) {
    let doc = slurp::read_all_to_string(&resource).unwrap();
    let (nom_ont, nom_pm) = nom_read(&doc).unwrap_or_else(|e| panic!("nom failed: {e}"));
    let (pest_ont, pest_pm) = pest_read(&doc).unwrap();
    assert_eq!(
        nom_pm.mappings().collect::<HashSet<_>>(),
        pest_pm.mappings().collect::<HashSet<_>>()
    );
    pretty_assertions::assert_eq!(nom_ont, pest_ont);
}

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
fn rejects(#[case] doc: &str, #[case] message: &str) {
    let nom = nom_read(doc).map(|_| ()).expect_err("nom should fail");
    assert!(pest_read(doc).is_err(), "pest accepted {doc}");
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
