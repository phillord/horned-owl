use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::Component;
use crate::model::ComponentKind;
use crate::model::ForIRI;
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;

mod as_functional;

pub use self::as_functional::AsFunctional;
pub use self::as_functional::Functional;

/// Write an Ontology to `write`, using the given `PrefixMapping`.
///
/// The ontology is written in OWL
/// [Functional-Style](https://www.w3.org/TR/2012/REC-owl2-syntax-20121211/)
/// syntax.
pub fn write<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<W, HornedError> {
    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => &default_mapper,
    };

    // Ensure we have a single OntologyID in the ontology.
    let optional_id = {
        let mut components = ont.i().component_for_kind(ComponentKind::OntologyID);
        let component = components.next();
        if components.next().is_some() {
            return Err(HornedError::invalid("multiple ontology IDs found"));
        }
        component.map(|c| {
            if let Component::OntologyID(ontology_id) = &c.component {
                ontology_id
            } else {
                unreachable!()
            }
        })
    };

    // Write prefixes
    write!(
        write,
        "{}",
        <PrefixMapping as AsFunctional<A>>::as_functional(mapping)
    )?;

    // Start the ontology element
    write!(write, "Ontology(")?;

    // Write the IRI and Version IRI if any
    if let Some(ontology_id) = optional_id
        && let Some(iri) = &ontology_id.iri
    {
        write!(write, "{}", iri.as_functional_with_prefixes(mapping))?;
        if let Some(viri) = &ontology_id.viri {
            writeln!(write, " {}", viri.as_functional_with_prefixes(mapping))?;
        } else {
            writeln!(write)?;
        }
    }

    // The OWL 2 Functional-Style Syntax grammar fixes the order of an
    // `Ontology(...)` body: `directlyImportsDocuments` (`Import(...)`)
    // must precede `ontologyAnnotations` (`Annotation(...)`), which in
    // turn must precede the axioms
    // (https://www.w3.org/TR/owl2-syntax/#Ontologies -- see also
    // `Ontology` in `src/grammars/ofn.pest`, which encodes the same
    // order and rejects anything else). `ComponentKind::all_kinds()`
    // does not honour this -- `OntologyAnnotation` is declared before
    // `Import` in the `components!` macro invocation in `model.rs`, so
    // the naive loop below would write annotations before imports and
    // produce unparseable output (see
    // https://github.com/phillord/horned-owl/issues/229). Write
    // `Import` then `OntologyAnnotation` up front, then fall through to
    // every other kind in their `all_kinds()` order.
    let mut other_kinds = ComponentKind::all_kinds();
    other_kinds.retain(|k| {
        *k != ComponentKind::OntologyID
            && *k != ComponentKind::DocIRI
            && *k != ComponentKind::Import
            && *k != ComponentKind::OntologyAnnotation
    });
    let ordered_kinds = [ComponentKind::Import, ComponentKind::OntologyAnnotation]
        .into_iter()
        .chain(other_kinds);

    for kind in ordered_kinds {
        let mut components = ont.i().component_for_kind(kind).collect::<Vec<_>>();
        components.sort();
        for component in components {
            writeln!(
                write,
                "    {}",
                component.as_functional_with_prefixes(mapping)
            )?;
        }
    }

    // Close the ontology
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

    #[rstest]
    fn roundtrip_resource(#[files("src/ont/owl-functional/*.ofn")] resource: PathBuf) {
        let reader = std::fs::File::open(&resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(reader, Default::default()).unwrap();

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, Some(&prefixes)).unwrap();

        let (ont2, prefixes2) =
            crate::io::ofn::reader::read(std::io::Cursor::new(&writer), Default::default())
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

    // Regression test for https://github.com/phillord/horned-owl/issues/229
    // The OFN grammar (and OWL 2 spec) requires `Import(...)` statements to
    // precede ontology `Annotation(...)`s in an `Ontology(...)` body, but the
    // writer used to walk `ComponentKind::all_kinds()` in `components!`
    // macro declaration order, which puts `OntologyAnnotation` before
    // `Import` -- producing output the reader itself then rejects. Check
    // both that a written `Import` textually precedes a written
    // `Annotation`, and that the written bytes reread successfully (the
    // real-world symptom: a `horned-roundtrip` `reread_fail`).
    #[test]
    fn roundtrip_import_before_ontology_annotation() {
        let resource = "src/ont/owl-functional/manual/import-and-annotation.ofn";
        let reader = std::fs::File::open(resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(reader, Default::default()).unwrap();

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, Some(&prefixes)).unwrap();
        let output = String::from_utf8(writer.clone()).unwrap();

        let import_pos = output
            .find("Import(")
            .expect("Import(...) missing from written output");
        let annotation_pos = output
            .find("Annotation(rdfs:label")
            .expect("ontology Annotation(...) missing from written output");
        assert!(
            import_pos < annotation_pos,
            "Import must precede the ontology Annotation in OFN output:\n{output}"
        );

        crate::io::ofn::reader::read::<
            RcStr,
            ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>,
            _,
        >(std::io::Cursor::new(&writer), Default::default())
        .expect("written OFN with both Import and ontology Annotation must reread cleanly");
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
