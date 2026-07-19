use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::Component;
use crate::model::ComponentKind;
use crate::model::DifferentIndividuals;
use crate::model::ForIRI;
use crate::model::SameIndividual;
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;

mod as_functional;

pub use self::as_functional::AsFunctional;
pub use self::as_functional::Functional;

/// Whether `component` has enough operands to be legal OWL Functional-Style
/// Syntax.
///
/// `SameIndividual` and `DifferentIndividuals` both require two or more
/// individuals in the OWL 2 structural grammar
/// (<https://www.w3.org/TR/owl2-syntax/#Individual_Equality>,
/// <https://www.w3.org/TR/owl2-syntax/#Individual_Inequality>) --
/// `src/grammars/ofn.pest`'s `SameIndividual`/`DifferentIndividuals`
/// productions require `Individual{2, }` accordingly. Real-world RDF/XML has
/// been observed (see
/// [#214](https://github.com/phillord/horned-owl/issues/214)) to contain an
/// `owl:AllDifferent` with a single `owl:distinctMembers` entry -- horned-
/// owl's RDF reader accepts this leniently into the model as a one-member
/// `DifferentIndividuals`. Such an axiom asserts nothing (there is no second
/// individual to differ from), so it is semantically vacuous, but writing it
/// out verbatim as OFN produces `DifferentIndividuals(<one IRI>)`, which the
/// grammar's own reader then rejects. Skip writing these degenerate axioms
/// rather than emit syntax our own reader can't parse back.
fn has_writable_arity<A: ForIRI>(component: &Component<A>) -> bool {
    match component {
        Component::SameIndividual(SameIndividual(v)) => v.len() >= 2,
        Component::DifferentIndividuals(DifferentIndividuals(v)) => v.len() >= 2,
        _ => true,
    }
}

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
            if !has_writable_arity(&component.component) {
                continue;
            }
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

    // Regression test for https://github.com/phillord/horned-owl/issues/214
    // Real-world RDF/XML (e.g. the ACGT-MO ontology from the BioPortal
    // corpus) has been observed to contain an `owl:AllDifferent` with a
    // single `owl:distinctMembers` entry. Horned-owl's RDF reader accepts
    // this leniently, producing a one-member `DifferentIndividuals` axiom in
    // the model -- but the OFN grammar requires `Individual{2, }`, so writing
    // it out verbatim produced `DifferentIndividuals(<one IRI>)`, which the
    // writer's own reader then rejected (the real-world symptom: a
    // `horned-roundtrip` `reread_fail`). Since such an axiom asserts nothing,
    // the writer now drops it instead. A well-formed (2+ member)
    // `DifferentIndividuals` must still be written normally.
    #[test]
    fn degenerate_different_individuals_is_dropped_not_written_invalid() {
        use crate::model::Build;
        use crate::model::DifferentIndividuals;
        use crate::model::MutableOntology;

        let build = Build::<RcStr>::new();
        let mut ont: ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>> =
            ComponentMappedOntology::new();
        ont.insert(DifferentIndividuals(vec![
            build
                .named_individual("http://example.com/HERATrial")
                .into(),
        ]));
        ont.insert(DifferentIndividuals(vec![
            build.named_individual("http://example.com/A").into(),
            build.named_individual("http://example.com/B").into(),
        ]));

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, None).unwrap();
        let output = String::from_utf8(writer).unwrap();

        assert!(
            !output.contains("HERATrial"),
            "degenerate single-member DifferentIndividuals should have been \
             dropped, but was written:\n{output}"
        );
        assert!(
            output.contains("DifferentIndividuals(<http://example.com/A> <http://example.com/B>)"),
            "well-formed DifferentIndividuals should still be written:\n{output}"
        );

        // The written output must be re-readable by our own OFN reader --
        // this is the actual `horned-roundtrip` failure mode this test
        // guards against.
        let (_, _): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(std::io::Cursor::new(&output), Default::default())
                .expect("written output must be re-parseable as OFN");
    }

    // Regression test for https://github.com/phillord/horned-owl/issues/235
    // `ObjectIntersectionOf`/`ObjectUnionOf` require at least two operands in
    // the OFN grammar (`ClassExpression{2, }` in `src/grammars/ofn.pest`),
    // but `ClassExpression::ObjectIntersectionOf`/`ObjectUnionOf` are plain
    // `Vec`s with no arity check, and horned-owl's RDF reader is lenient
    // about real-world (if technically malformed) single-member
    // `owl:intersectionOf`/`owl:unionOf` RDF lists -- see the `BCS7` corpus
    // ontology's `owl:intersectionOf ( cst:M1 )`. Such a degenerate
    // single-operand `ObjectIntersectionOf`/`ObjectUnionOf` can't be built
    // by parsing OFN (the reader enforces the grammar's minimum), so it's
    // constructed directly via the model API here, matching how the RDF
    // reader would produce it.
    #[test]
    fn roundtrip_degenerate_single_operand_intersection_and_union() {
        use crate::model::Build;
        use crate::model::ClassExpression;
        use crate::model::MutableOntology;
        use crate::model::SubClassOf;

        let build = Build::<RcStr>::new();
        let mut ont: ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>> =
            ComponentMappedOntology::new();
        ont.insert(SubClassOf {
            sub: ClassExpression::Class(build.class("http://example.com/Sub")),
            sup: ClassExpression::ObjectIntersectionOf(vec![ClassExpression::Class(
                build.class("http://example.com/One"),
            )]),
        });
        ont.insert(SubClassOf {
            sub: ClassExpression::Class(build.class("http://example.com/Sub2")),
            sup: ClassExpression::ObjectUnionOf(vec![ClassExpression::Class(
                build.class("http://example.com/Two"),
            )]),
        });

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, None).unwrap();
        let output = String::from_utf8(writer).unwrap();

        assert!(
            !output.contains("ObjectIntersectionOf("),
            "a single-operand ObjectIntersectionOf must be unwrapped, not \
             written invalid:\n{output}"
        );
        assert!(
            !output.contains("ObjectUnionOf("),
            "a single-operand ObjectUnionOf must be unwrapped, not written \
             invalid:\n{output}"
        );

        // The written output must be re-readable by our own OFN reader --
        // this is the actual `horned-roundtrip` failure mode this test
        // guards against.
        let (_, _): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(std::io::Cursor::new(&output), Default::default())
                .expect("written output must be re-parseable as OFN");
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
