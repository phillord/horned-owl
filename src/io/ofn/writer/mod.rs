use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::io::{StreamComponent, StreamOntology};
use crate::model::AnnotatedComponent;
use crate::model::Component;
use crate::model::ForIRI;
use crate::model::Ontology;
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;

mod as_functional;

pub use self::as_functional::AsFunctional;
pub use self::as_functional::Functional;
use self::as_functional::percent_encode_iri;

/// Write any `Ontology` to `write`, in OWL
/// [Functional-Style](https://www.w3.org/TR/2012/REC-owl2-syntax-20121211/)
/// syntax, using the given `PrefixMapping`. Converts to a
/// `ComponentMappedOntology` then defers to [`write_cmo`]; a caller that
/// already has one built should call `write_cmo` directly instead.
pub fn write<A: ForIRI, O: Ontology<A>, W: Write>(
    write: W,
    ont: &O,
    mapping: Option<&PrefixMapping>,
) -> Result<W, HornedError> {
    let cmo: ComponentMappedOntology<A, AnnotatedComponent<A>> =
        crate::io::into_component_mapped(ont);
    write_cmo(write, &cmo, mapping)
}

/// Write a `ComponentMappedOntology` to `write`, using the given
/// `PrefixMapping` -- the concrete, zero-conversion entry point [`write`]
/// defers to.
pub fn write_cmo<A: ForIRI, AA: ForIndex<A>, W: Write>(
    write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<W, HornedError> {
    let default_mapper = PrefixMapping::default();
    let mapping = mapping.unwrap_or(&default_mapper);

    write_stream(
        write,
        crate::io::prefix_stream(mapping).chain(crate::io::component_stream(ont)),
    )
}

/// Write `components` as an OWL Functional-Style `Ontology(...)` document.
///
/// Components are written in the order they arrive, so the caller is
/// responsible for ordering them to match the specification. If they do
/// not, the output may not conform to the specification, and `Prefix`
/// handling may be broken. Use [`write`] or [`write_cmo`] instead if this
/// ordering can't be guaranteed.
pub fn write_stream<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    components: impl StreamOntology<A, AA>,
) -> Result<W, HornedError> {
    let mut mapping = PrefixMapping::default();
    let mut ontology_opened = false;

    for item in components {
        match item? {
            StreamComponent::Prefix(name, iri) => {
                let _ = mapping.add_prefix(&name, &iri);
                writeln!(write, "Prefix({name}:=<{}>)", percent_encode_iri(&iri))?;
            }
            StreamComponent::Component(ac) => {
                let ac: &AnnotatedComponent<A> = ac.borrow();

                if !ontology_opened {
                    ontology_opened = true;
                    write!(write, "Ontology(")?;

                    if let Component::OntologyID(id) = &ac.component {
                        if let Some(iri) = &id.iri {
                            write!(write, "{}", iri.as_functional_with_prefixes(&mapping))?;
                            if let Some(viri) = &id.viri {
                                writeln!(write, " {}", viri.as_functional_with_prefixes(&mapping))?;
                            } else {
                                writeln!(write)?;
                            }
                        }
                        continue;
                    }
                }

                writeln!(write, "    {}", ac.as_functional_with_prefixes(&mapping))?;
            }
        }
    }

    if !ontology_opened {
        write!(write, "Ontology(")?;
    }
    writeln!(write, ")")?;

    Ok(write)
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::model::AnnotatedComponent;
    use crate::model::RcStr;

    use pretty_assertions::assert_eq;
    use rstest::rstest;
    use std::path::PathBuf;

    #[test]
    fn write_stream_writes_prefixes_then_ontology() {
        let b = crate::model::Build::new_rc();
        let iri = b.iri("http://www.example.com/a");
        let ac = AnnotatedComponent {
            component: crate::model::DeclareClass(crate::model::Class(iri)).into(),
            ann: Default::default(),
        };

        let items: Vec<crate::io::Result<StreamComponent<AnnotatedComponent<RcStr>>>> = vec![
            Ok(StreamComponent::Prefix(
                "eg".to_string(),
                "http://example.com/eg#".to_string(),
            )),
            Ok(StreamComponent::Component(ac)),
        ];

        let mut writer = Vec::new();
        write_stream(&mut writer, items.into_iter()).unwrap();
        let out = String::from_utf8(writer).unwrap();

        assert!(out.starts_with("Prefix(eg:=<http://example.com/eg#>)\n"));
        assert!(out.contains("Declaration(Class(<http://www.example.com/a>))"));

        let b2 = crate::model::Build::new_rc();
        let (ont, _): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(
                std::io::Cursor::new(out),
                crate::io::ParserConfiguration::new(&b2),
            )
            .unwrap();
        assert_eq!(ont.i().declare_class().count(), 1);
    }

    #[rstest]
    fn roundtrip_resource(#[files("src/ont/owl-functional/*.ofn")] resource: PathBuf) {
        let reader = std::fs::File::open(&resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let b = crate::model::Build::new_rc();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(reader, crate::io::ParserConfiguration::new(&b)).unwrap();

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, Some(&prefixes)).unwrap();

        let (ont2, prefixes2) = crate::io::ofn::reader::read(
            std::io::Cursor::new(&writer),
            crate::io::ParserConfiguration::new(&b),
        )
        .unwrap();

        assert_eq!(prefixes, prefixes2, "prefix mapping differ");
        assert_eq!(ont, ont2, "ontologies differ");
    }

    // Regression test for https://github.com/phillord/horned-owl/issues/175
    // Annotations on Annotation (annotationAnnotations in OWL 2 spec) are
    // silently discarded because Annotation lacks an `ann` field. A round-trip
    // ont==ont2 comparison would pass (both drops are identical), so we check
    // the written string directly instead.
    #[test]
    fn roundtrip_nested_annotation_on_annotation() {
        let resource = "src/ont/owl-functional/manual/nested-annotation-on-annotation.ofn";
        let reader = std::fs::File::open(resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(reader, Default::default()).unwrap();

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, Some(&prefixes)).unwrap();
        let output = String::from_utf8(writer).unwrap();

        assert!(
            output.contains("Annotation(Annotation("),
            "nested annotation was lost in round-trip:\n{output}"
        );
    }

    #[cfg(test)]
    mod bubo_test {
        use crate::io::ofn::writer::test::*;
        use crate::io::ofn::writer::write;

        use std::fs::File;
        use std::io::BufReader;
        use std::path::Path;

        fn parse_then_output(in_file: &Path, out: &mut dyn std::io::Write) {
            let reader = BufReader::new(File::open(in_file).unwrap());
            let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
                crate::io::ofn::reader::read(reader, Default::default()).unwrap();

            write(out, &ont, Some(&prefixes)).ok().unwrap();
        }

        #[test]
        fn reparse_ofn() -> Result<(), Box<dyn std::error::Error>> {
            crate::io::tests::run_bubo_reparse("owl-functional", parse_then_output)
        }
    }
}
