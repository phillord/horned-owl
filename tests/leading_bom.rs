//! A leading UTF-8 byte order mark must not change what any reader produces (#326).

use std::io::Cursor;

use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;
use rstest::rstest;

const OFN: &str = "Prefix(:=<http://example.org/bom#>)
Ontology(<http://example.org/bom>
Declaration(Class(:A))
)
";

const OMN: &str = "Prefix: : <http://example.org/bom#>
Ontology: <http://example.org/bom>
Class: :A
";

const TTL: &str = "@prefix : <http://example.org/bom#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
<http://example.org/bom> a owl:Ontology .
:A a owl:Class .
";

fn count(format: &str, text: &str) -> usize {
    let b = Build::new_rc();
    let mut r = Cursor::new(text.as_bytes());
    match format {
        "ofn" => horned_owl::io::ofn::reader::read::<RcStr, _, SetOntology<RcStr>, _>(
            &mut r,
            ParserConfiguration::new(&b),
        )
        .unwrap()
        .0
        .iter()
        .count(),
        "omn" => horned_owl::io::omn::reader::read::<RcStr, _, SetOntology<RcStr>, _>(
            r,
            ParserConfiguration::new(&b),
        )
        .unwrap()
        .0
        .iter()
        .count(),
        _ => {
            let mut config: RDFParserConfiguration<RcStr, &Build<RcStr>> =
                ParserConfiguration::new(&b).into();
            config.format = Some(oxrdfio::RdfFormat::Turtle);
            let o = horned_owl::io::rdf::reader::read::<RcStr, RcAnnotatedComponent, _, _>(
                &mut r, config,
            )
            .unwrap()
            .0;
            SetOntology::from(o).iter().count()
        }
    }
}

#[rstest]
#[case("ofn", OFN)]
#[case("omn", OMN)]
#[case("ttl", TTL)]
fn leading_bom(#[case] format: &str, #[case] text: &str) {
    assert_eq!(
        count(format, &format!("\u{feff}{text}")),
        count(format, text)
    );
}
