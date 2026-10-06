//! Regression test for phillord/horned-owl#220.
//!
//! An RDF graph is a set of triples, so the order in which the reader meets them must not change
//! the ontology it builds. Nested data ranges (a `DataUnionOf` whose `rdf:List` holds a
//! `DataIntersectionOf`) used to be lost whenever the outer range was visited before the inner.

use std::rc::Rc;

use horned_owl::io::rdf::reader::{ConcreteRDFOntology, read};
use horned_owl::io::rdf::writer::write_to_rdf_format;
use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::*;
use horned_owl::ontology::component_mapped::ComponentMappedOntology;
use horned_owl::ontology::set::SetOntology;

type Cmo = ComponentMappedOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>;

fn nested_data_range_ontology() -> Cmo {
    let mut src = String::from(
        "Prefix(:=<http://www.example.com/o#>)\n\
         Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\nOntology(\n",
    );
    for i in 0..8 {
        src.push_str(&format!(
            "Declaration(Class(:C{i}))\nDeclaration(DataProperty(:p{i}))\n\
             EquivalentClasses(:C{i} DataExactCardinality(1 :p{i} \
             DataUnionOf(DataIntersectionOf(\
             DatatypeRestriction(xsd:integer xsd:minInclusive \"{i}\"^^xsd:integer) \
             xsd:decimal) xsd:string)))\n"
        ));
    }
    src.push(')');

    let (o, _): (SetOntology<RcStr>, _) =
        horned_owl::io::ofn::reader::read(&mut src.as_bytes(), ParserConfiguration::default())
            .unwrap();
    o.into_iter().collect()
}

fn parse_nt(text: &str) -> (usize, bool) {
    let config = RDFParserConfiguration {
        common: ParserConfiguration::default(),
        format: Some(oxrdfio::RdfFormat::NTriples),
    };
    let (ont, incomplete): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
        read(&mut text.as_bytes(), config).unwrap();
    let ont: SetOntology<RcStr> = ont.into();
    (ont.iter().count(), incomplete.is_complete())
}

#[test]
fn nested_data_ranges_parse_in_any_triple_order() {
    let original = nested_data_range_ontology();
    let nt = String::from_utf8(write_to_rdf_format(vec![], &original, "nt").unwrap()).unwrap();
    let expected = original.iter().count();

    let mut lines: Vec<&str> = nt.lines().collect();
    let mut seed: u64 = 220;
    for round in 0..20 {
        for i in (1..lines.len()).rev() {
            seed = seed
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            lines.swap(i, (seed >> 33) as usize % (i + 1));
        }
        let (count, complete) = parse_nt(&lines.join("\n"));
        assert!(complete, "round {round}: parse left triples behind");
        assert_eq!(count, expected, "round {round}: components lost");
    }
}
