use std::path::PathBuf;

use rstest::rstest;

use crate::io::StreamComponent;
use crate::model::{AnnotatedComponent, RcStr};
use crate::ontology::set::SetOntology;

type Read = Result<(SetOntology<RcStr>, curie::PrefixMapping), crate::error::HornedError>;

fn nom_read(doc: &str) -> Read {
    super::read(doc.as_bytes(), Default::default())
}

fn stream_all(doc: &str, chunk: usize) -> Vec<StreamComponent<AnnotatedComponent<RcStr>>> {
    let reader = super::Reader::new(doc.as_bytes(), Default::default()).with_chunk_size(chunk);
    reader.map(|i| i.unwrap()).collect()
}

/// The units found must not depend on how the input is cut into reads.
#[rstest]
fn chunked_same_as_whole(#[files("src/ont/owl-manchester/*.omn")] resource: PathBuf) {
    let doc = slurp::read_all_to_string(&resource).unwrap();
    if super::read::<_, _, SetOntology<RcStr>, _>(doc.as_bytes(), Default::default()).is_err() {
        return;
    }
    let whole = stream_all(&doc, 1 << 20);
    for chunk in [1, 7, 64] {
        assert_eq!(whole, stream_all(&doc, chunk), "chunk size {chunk}");
    }
}

mod writing {
    use super::*;
    use crate::io::{component_stream, prefix_stream};
    use crate::model::MutableOntology;
    use crate::ontology::component_mapped::ComponentMappedOntology;

    fn load(
        path: &PathBuf,
    ) -> Option<(ComponentMappedOntology<RcStr, RcStr_>, curie::PrefixMapping)> {
        let doc = slurp::read_all_to_string(path).unwrap();
        let (ont, pm): (SetOntology<RcStr>, _) = nom_read(&doc).ok()?;
        Some((ont.into(), pm))
    }

    type RcStr_ = std::rc::Rc<crate::model::AnnotatedComponent<RcStr>>;

    fn streamed(
        ont: &ComponentMappedOntology<RcStr, RcStr_>,
        pm: &curie::PrefixMapping,
        chunk: usize,
    ) -> String {
        let items = prefix_stream(pm).chain(component_stream(ont));
        let out = crate::io::omn::write_stream_chunked(Vec::new(), items, chunk).unwrap();
        String::from_utf8(out).unwrap()
    }

    /// With room for everything the streamed document is the batch one.
    #[rstest]
    fn one_chunk_same_as_write(#[files("src/ont/owl-manchester/*.omn")] resource: PathBuf) {
        let Some((ont, pm)) = load(&resource) else {
            return;
        };
        let batch = crate::io::omn::write(Vec::new(), &ont, Some(&pm)).unwrap();
        assert_eq!(
            String::from_utf8(batch).unwrap(),
            streamed(&ont, &pm, usize::MAX)
        );
    }

    /// Small chunks repeat frames, which must not change what is read.
    #[rstest]
    fn small_chunks_read_back_the_same(#[files("src/ont/owl-manchester/*.omn")] resource: PathBuf) {
        let Some((ont, pm)) = load(&resource) else {
            return;
        };
        let batch =
            String::from_utf8(crate::io::omn::write(Vec::new(), &ont, Some(&pm)).unwrap()).unwrap();
        let (expected, _) = nom_read(&batch).unwrap();
        for chunk in [1, 2, 5] {
            let text = streamed(&ont, &pm, chunk);
            let (got, _) = nom_read(&text).unwrap_or_else(|e| panic!("chunk {chunk}: {e}\n{text}"));
            pretty_assertions::assert_eq!(expected, got, "chunk {chunk}:\n{text}");
        }
    }

    /// A document written in memory can be read one item at a time, with no
    /// prescan: data properties and datatypes come before their uses.
    #[test]
    fn written_document_streams_back_the_same() {
        let ofn = "Prefix(:=<http://ex/>)\nPrefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\nOntology(<http://ex/o>\n\
            Declaration(Class(:A))\nDeclaration(DataProperty(:p))\nDeclaration(Datatype(:dt))\n\
            DatatypeDefinition(:dt xsd:integer)\n\
            SubClassOf(:A DataSomeValuesFrom(:p :dt))\n\
            SubClassOf(:A DataAllValuesFrom(:p xsd:string))\n\
            HasKey(:A () (:p))\n)";
        let (ont, pm): (SetOntology<RcStr>, _) =
            crate::io::ofn::reader::read(&mut ofn.as_bytes(), Default::default()).unwrap();
        let cmo: ComponentMappedOntology<RcStr, RcStr_> = ont.clone().into();
        let text =
            String::from_utf8(crate::io::omn::write(Vec::new(), &cmo, Some(&pm)).unwrap()).unwrap();
        let mut got = SetOntology::new_rc();
        for item in super::super::Reader::new(text.as_bytes(), Default::default()) {
            if let StreamComponent::Component(ac) = item.unwrap() {
                got.insert(ac);
            }
        }
        pretty_assertions::assert_eq!(ont, got, "{text}");
    }

    /// Stream write then stream read gives what batch write then read gives.
    fn assert_stream_matches_batch(ont: SetOntology<RcStr>) {
        let pm = curie::PrefixMapping::default();
        let cmo: ComponentMappedOntology<RcStr, RcStr_> = ont.into();
        let batch =
            String::from_utf8(crate::io::omn::write(Vec::new(), &cmo, Some(&pm)).unwrap()).unwrap();
        let (expected, _): (SetOntology<RcStr>, _) = nom_read(&batch).unwrap();
        for chunk in [1, usize::MAX] {
            let text = streamed(&cmo, &pm, chunk);
            let mut got = SetOntology::new_rc();
            for item in super::super::Reader::new(text.as_bytes(), Default::default()) {
                if let StreamComponent::Component(ac) = item.unwrap() {
                    got.insert(ac);
                }
            }
            pretty_assertions::assert_eq!(expected, got, "chunk {chunk}:\n{text}");
        }
    }

    fn orphan_assertion(
        b: &crate::model::Build<RcStr>,
        p: &str,
    ) -> crate::model::AnnotationAssertion<RcStr> {
        crate::model::AnnotationAssertion {
            subject: b.iri("http://ex/orphan").into(),
            ann: crate::model::Annotation {
                ap: b.annotation_property(p),
                av: crate::model::Literal::Simple {
                    literal: "v".to_string(),
                }
                .into(),
                ann: Default::default(),
            },
        }
    }

    /// A one-member DifferentIndividuals has no readable form; written after
    /// an orphan annotation assertion it must not take the general block with
    /// it.
    #[test]
    fn singleton_different_individuals_keeps_general_block() {
        use crate::model::*;
        let b = Build::new_rc();
        let assertion = orphan_assertion(&b, "http://ex/p");
        let one = |c: Component<RcStr>| {
            Ok(StreamComponent::Component(std::rc::Rc::new(
                AnnotatedComponent::from(c),
            )))
        };
        let items = vec![
            one(assertion.clone().into()),
            one(
                DifferentIndividuals(vec![Individual::Named(b.named_individual("http://ex/i"))])
                    .into(),
            ),
            one(SameIndividual(vec![Individual::Named(b.named_individual("http://ex/j"))]).into()),
        ];
        let out = crate::io::omn::write_stream_chunked(Vec::new(), items.into_iter(), 1).unwrap();
        let text = String::from_utf8(out).unwrap();
        let (got, _): (SetOntology<RcStr>, _) = nom_read(&text).unwrap();
        let comps: Vec<_> = got.iter().map(|ac| ac.component.clone()).collect();
        assert!(comps.contains(&assertion.into()), "{text}");
    }

    /// An annotated declaration is written to the general block, so
    /// assertions on the entity must not become a frame that declares it again.
    #[test]
    fn annotated_declaration_gets_no_extra_declaration() {
        use crate::model::*;
        let b = Build::new_rc();
        let mut o = SetOntology::new_rc();
        let ap = b.annotation_property("http://ex/p");
        o.insert(AnnotatedComponent {
            component: DeclareAnnotationProperty(ap.clone()).into(),
            ann: [Annotation {
                ap: b.annotation_property("http://ex/q"),
                av: Literal::Simple {
                    literal: "d".to_string(),
                }
                .into(),
                ann: Default::default(),
            }]
            .into_iter()
            .collect(),
        });
        o.insert(AnnotationAssertion {
            subject: b.iri("http://ex/p").into(),
            ann: Annotation {
                ap: b.annotation_property("http://ex/q"),
                av: Literal::Simple {
                    literal: "v".to_string(),
                }
                .into(),
                ann: Default::default(),
            },
        });
        assert_stream_matches_batch(o);
    }
}

mod predefined_prefixes {
    use super::*;

    const BODY: &str = "Class: :A\n    SubClassOf: owl:Thing\n    Annotations: rdfs:label \"A\"^^xsd:string, rdfs:seeAlso rdf:nil\n\
                        DataProperty: :p\n    Range: xsd:string\n";

    fn doc(prefixes: &str) -> String {
        format!(
            "Prefix: : <http://example.com/>\n{prefixes}Ontology: <http://example.com/o>\n{BODY}"
        )
    }

    fn declared() -> String {
        doc("Prefix: owl: <http://www.w3.org/2002/07/owl#>\n\
             Prefix: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n\
             Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
             Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n")
    }

    fn names(pm: &curie::PrefixMapping) -> Vec<String> {
        pm.mappings().map(|(p, _)| p.to_string()).collect()
    }

    /// Predefined prefixes expand undeclared; the mapping holds only declared ones.
    #[test]
    fn expand_without_declaration() {
        let (expected, _) = nom_read(&declared()).unwrap();
        let implicit = doc("");
        let batch = nom_read(&implicit).unwrap();
        let streamed: Vec<_> = stream_all(&implicit, 7);
        let mut ont = SetOntology::<RcStr>::default();
        for i in streamed {
            if let StreamComponent::Component(c) = i {
                crate::model::MutableOntology::insert(&mut ont, c);
            }
        }
        assert_eq!(batch.0, expected);
        assert_eq!(ont, expected);
        assert_eq!(names(&batch.1), vec![""]);
    }

    /// A declared prefix wins over the predefined one.
    #[test]
    fn redeclared_prefix_wins() {
        let d = "Prefix: owl: <http://example.com/>\nOntology: <http://example.com/o>\nClass: owl:Thing\n";
        let (ont, pm) = nom_read(d).unwrap();
        let text = format!("{:?}", ont.iter().collect::<Vec<_>>());
        assert!(text.contains("http://example.com/Thing"), "{text}");
        assert!(!text.contains("owl#Thing"), "{text}");
        assert_eq!(names(&pm), vec!["owl"]);
    }

    /// Other undeclared prefixes are still an error.
    #[test]
    fn unknown_prefix_errors() {
        let d = "Ontology: <http://example.com/o>\nClass: foo:A\n";
        let e = nom_read(d).unwrap_err().to_string();
        assert!(e.contains("undefined prefix"), "{e}");
        let streamed: Vec<_> =
            super::super::Reader::<RcStr, _, _>::new(d.as_bytes(), Default::default()).collect();
        assert!(streamed.iter().any(|i| i.is_err()));
    }
}

/// A parenthesised filler is data when a known datatype or a literal
/// enumeration follows IRIs joined by `or`/`and`: neither can stand in a
/// class expression.
#[rstest]
#[case(
    r#":load only (:Percentage or {"0.0"^^xsd:decimal})"#,
    "DataAllValuesFrom"
)]
#[case(":load some (:Percentage and xsd:decimal)", "DataSomeValuesFrom")]
#[case(":load some (xsd:decimal and :Percentage)", "DataSomeValuesFrom")]
#[case(":load only (:Percentage or not xsd:decimal)", "DataAllValuesFrom")]
#[case(":hasPart only (:Axle or :Wheel)", "ObjectAllValuesFrom")]
fn data_range_after_custom_datatype(#[case] filler: &str, #[case] kind: &str) {
    let doc = format!(
        "Prefix: : <http://t/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
         Ontology: <http://t/o>\nClass: :A\n    SubClassOf: {filler}\n"
    );
    let (ont, _) = nom_read(&doc).unwrap_or_else(|e| panic!("{filler}: {e}"));
    let sub = ont
        .iter()
        .find_map(|c| match &c.component {
            crate::model::Component::SubClassOf(s) => Some(format!("{:?}", s.sup)),
            _ => None,
        })
        .expect("a SubClassOf axiom");
    assert!(sub.starts_with(kind), "{filler}: {sub}");
}
