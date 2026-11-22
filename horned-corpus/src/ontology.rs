//! Source-read adapter: turns raw bytes in a known [`Format`] into a
//! `SetOntology<RcStr>`, capturing whatever completeness information the
//! underlying horned-owl reader can provide.
//!
//! Only the RDF/XML reader can produce an incomplete parse (see
//! `horned_owl::io::rdf::reader::IncompleteParse`); the ofn/omn/owx/obo
//! readers either fully succeed or return a hard error, so they always
//! report `incomplete: None`.
//!
//! Dispatch goes through `horned_owl::io::ParserOutput` and its
//! `.decompose()` method (see the API notes in `tests/smoke.rs`) rather than
//! matching each format's raw tuple/struct shape by hand -- that seam
//! normalizes ofn/omn/owx/obo/rdf into a single
//! `(SetOntology<A>, Option<PrefixMapping>, Option<IncompleteParse<A>>)`
//! triple.

use crate::model::{Format, IncompleteSummary};
use curie::PrefixMapping;
use horned_owl::error::HornedError;
use horned_owl::io::{
    ParserConfiguration, ParserOutput, RDFParserConfiguration, StreamComponent, obo, ofn, omn, owx,
    rdf,
};
use horned_owl::model::{
    AnnotatedComponent, Build, Component, MutableOntology, RcAnnotatedComponent, RcStr,
};
use horned_owl::ontology::component_mapped::ComponentMappedOntology;
use horned_owl::ontology::set::SetOntology;
use std::io::Cursor;

/// `HornedError` wraps a `Box<dyn std::error::Error>` (not `Send + Sync`),
/// so it does not satisfy anyhow's blanket `From<E: Error + Send + Sync>`
/// impl and `?` cannot convert it directly into `anyhow::Error`. Render it
/// to a string (via its `thiserror`-derived `Display`) immediately instead.
fn horned_err(e: HornedError) -> anyhow::Error {
    anyhow::anyhow!("{e}")
}

pub struct ReadOk {
    pub model: SetOntology<RcStr>,
    pub prefixes: PrefixMapping,
    pub incomplete: Option<IncompleteSummary>,
}

/// Map an `IncompleteParse<RcStr>`'s leftover collections onto the four
/// counters `IncompleteSummary` tracks.
///
/// `IncompleteParse` actually has eight leftover fields (`simple`, `bnode`,
/// `bnode_seq`, `class_expression`, `object_property_expression`,
/// `data_range`, `atom`, `ann_map`) but `IncompleteSummary` only has four
/// buckets, so related fields are folded together:
/// - `simple` maps directly to `simple`.
/// - `bnode_seq` (bnode triples that form part of an RDF sequence) is folded
///   into `bnode`, since both are "leftover bnode-rooted triple" shapes.
/// - `object_property_expression`, `data_range` and `atom` (unconnected
///   SWRL atoms) are folded into `class_expression`, since all four are
///   "unconnected expression-like" leftovers as opposed to plain triples or
///   annotations.
/// - `ann_map` maps directly to `annotation`, matching its doc comment
///   ("Annotations that are otherwise unconnected to other parts of the
///   Ontology").
fn summarize(incomplete: &rdf::reader::IncompleteParse<RcStr>) -> Option<IncompleteSummary> {
    if incomplete.is_complete() {
        return None;
    }
    Some(IncompleteSummary {
        simple: incomplete.simple.len(),
        bnode: incomplete.bnode.len() + incomplete.bnode_seq.len(),
        class_expression: incomplete.class_expression.len()
            + incomplete.object_property_expression.len()
            + incomplete.data_range.len()
            + incomplete.atom.len(),
        annotation: incomplete.ann_map.len(),
    })
}

/// A streaming nom `Reader`: its items, one at a time, into a
/// `SetOntology`, with the prefixes it saw. Nothing is read ahead.
fn drain<R>(mut reader: R) -> anyhow::Result<(SetOntology<RcStr>, PrefixMapping)>
where
    R: Iterator<Item = Result<StreamComponent<AnnotatedComponent<RcStr>>, HornedError>>
        + IntoPrefixes,
{
    let mut ont = SetOntology::new_rc();
    for item in reader.by_ref() {
        if let StreamComponent::Component(ac) = item.map_err(horned_err)? {
            ont.insert(ac);
        }
    }
    Ok((ont, reader.into_prefixes()))
}

/// The prefixes a streaming `Reader` collected. Each reader has its own
/// `into_prefixes`; this names them as one.
trait IntoPrefixes {
    fn into_prefixes(self) -> PrefixMapping;
}

macro_rules! into_prefixes {
    ($($reader:path),*) => {$(
        impl<B: AsRef<Build<RcStr>>> IntoPrefixes for $reader {
            fn into_prefixes(self) -> PrefixMapping {
                <$reader>::into_prefixes(self)
            }
        }
    )*};
}

into_prefixes!(
    ofn::nom_reader::Reader<RcStr, Cursor<&[u8]>, B>,
    omn::nom_reader::Reader<RcStr, Cursor<&[u8]>, B>,
    obo::nom_reader::Reader<RcStr, Cursor<&[u8]>, B>
);

pub fn read_source(fmt: Format, bytes: &[u8]) -> anyhow::Result<ReadOk> {
    read_source_with(fmt, bytes, false)
}

/// As `read_source`, with the readers in lax mode when `lax` is set.
pub fn read_source_with(fmt: Format, bytes: &[u8], lax: bool) -> anyhow::Result<ReadOk> {
    // The AA (indexed-annotated-component) type parameter is only ever
    // exercised by the RDF variant; the ofn/omn/owx variants carry a plain
    // `SetOntology` and so give the compiler nothing to infer AA from. Pin
    // it explicitly to `RcAnnotatedComponent`, the same type the RDF
    // reader's fixed `ConcreteRDFOntology<RcStr, RcAnnotatedComponent>`
    // return type uses.
    type Output = ParserOutput<RcStr, RcAnnotatedComponent>;

    let build = Build::new_rc();
    let config = || {
        let mut c = ParserConfiguration::new(&build);
        c.lax = lax;
        c
    };

    let (model, prefixes, incomplete) = match fmt {
        Format::Ofn | Format::OfnStreamWrite => {
            let sop = ofn::reader::read(&mut Cursor::new(bytes), config()).map_err(horned_err)?;
            Output::ofn(sop).decompose()
        }
        Format::OfnNom => {
            let sop =
                ofn::nom_reader::read(&mut Cursor::new(bytes), config()).map_err(horned_err)?;
            Output::ofn(sop).decompose()
        }
        Format::Omn | Format::OmnStreamWrite => {
            let sop = omn::reader::read(Cursor::new(bytes), config()).map_err(horned_err)?;
            Output::omn(sop).decompose()
        }
        Format::OmnNom => {
            let sop = omn::nom_reader::read(Cursor::new(bytes), config()).map_err(horned_err)?;
            Output::omn(sop).decompose()
        }
        Format::OfnNomStream | Format::OfnStreamRead => {
            let sop = drain(ofn::nom_reader::read_to_stream(
                Cursor::new(bytes),
                config(),
            ))?;
            Output::ofn(sop).decompose()
        }
        Format::OmnNomStream | Format::OmnStreamRead => {
            let sop = drain(omn::nom_reader::read_to_stream(
                Cursor::new(bytes),
                config(),
            ))?;
            Output::omn(sop).decompose()
        }
        Format::OboNomStream | Format::OboStreamRead => {
            let sop = drain(obo::nom_reader::read_to_stream(
                Cursor::new(bytes),
                config(),
            ))?;
            Output::obo(sop).decompose()
        }
        Format::OwlXml => {
            let sop = owx::reader::read(&mut Cursor::new(bytes), config()).map_err(horned_err)?;
            Output::owx(sop).decompose()
        }
        Format::Obo | Format::OboStreamWrite => {
            let sop = obo::reader::read(&mut Cursor::new(bytes), config()).map_err(horned_err)?;
            Output::obo(sop).decompose()
        }
        Format::OboNom => {
            let sop = obo::nom_reader::read(Cursor::new(bytes), config()).map_err(horned_err)?;
            Output::obo(sop).decompose()
        }
        Format::RdfXml => {
            let rop =
                rdf::reader::read(&mut Cursor::new(bytes), config().into()).map_err(horned_err)?;
            Output::rdf(rop).decompose()
        }
        Format::Turtle => {
            // Same oxrdfio-backed RDF reader, but tell it the input syntax is
            // Turtle (the config defaults to RdfXml otherwise). N-Triples is a
            // Turtle subset, so this parses both. IncompleteParse handling is
            // identical to the RdfXml path.
            let config = RDFParserConfiguration {
                format: Some(oxrdfio::RdfFormat::Turtle),
                ..config().into()
            };
            let rop = rdf::reader::read(&mut Cursor::new(bytes), config).map_err(horned_err)?;
            Output::rdf(rop).decompose()
        }
        Format::Unknown => anyhow::bail!("unknown format"),
    };

    let incomplete = incomplete.as_ref().and_then(summarize);
    let prefixes = prefixes.unwrap_or_default();

    Ok(ReadOk {
        model,
        prefixes,
        incomplete,
    })
}

/// The IRI a component is about, if it has a single one.
fn subject_iri(c: &Component<RcStr>) -> Option<&str> {
    use horned_owl::model::{
        AnnotationSubject, ClassExpression, Individual, ObjectPropertyExpression,
        SubObjectPropertyExpression,
    };
    fn class(ce: &ClassExpression<RcStr>) -> Option<&str> {
        match ce {
            ClassExpression::Class(c) => Some(c.0.as_ref()),
            _ => None,
        }
    }
    fn ope(o: &ObjectPropertyExpression<RcStr>) -> Option<&str> {
        match o {
            ObjectPropertyExpression::ObjectProperty(p) => Some(p.0.as_ref()),
            _ => None,
        }
    }
    match c {
        Component::DeclareClass(d) => Some(d.0.0.as_ref()),
        Component::DeclareObjectProperty(d) => Some(d.0.0.as_ref()),
        Component::DeclareDataProperty(d) => Some(d.0.0.as_ref()),
        Component::DeclareAnnotationProperty(d) => Some(d.0.0.as_ref()),
        Component::DeclareNamedIndividual(d) => Some(d.0.0.as_ref()),
        Component::DeclareDatatype(d) => Some(d.0.0.as_ref()),
        Component::AnnotationAssertion(a) => match &a.subject {
            AnnotationSubject::IRI(i) => Some(i.as_ref()),
            _ => None,
        },
        Component::SubClassOf(s) => class(&s.sub),
        Component::ClassAssertion(a) => match &a.i {
            Individual::Named(n) => Some(n.0.as_ref()),
            _ => None,
        },
        Component::SubObjectPropertyOf(s) => match &s.sub {
            SubObjectPropertyExpression::ObjectPropertyExpression(o) => ope(o),
            _ => None,
        },
        Component::ObjectPropertyDomain(d) => ope(&d.ope),
        Component::FunctionalObjectProperty(o) => ope(&o.0),
        Component::InverseFunctionalObjectProperty(o) => ope(&o.0),
        Component::ReflexiveObjectProperty(o) => ope(&o.0),
        Component::IrreflexiveObjectProperty(o) => ope(&o.0),
        Component::SymmetricObjectProperty(o) => ope(&o.0),
        Component::AsymmetricObjectProperty(o) => ope(&o.0),
        Component::TransitiveObjectProperty(o) => ope(&o.0),
        Component::InverseObjectProperties(i) => Some(i.0.0.as_ref()),
        Component::EquivalentClasses(e) => e.0.iter().find_map(class),
        Component::DisjointClasses(e) => e.0.iter().find_map(class),
        Component::EquivalentObjectProperties(e) => e.0.iter().find_map(ope),
        Component::DisjointObjectProperties(e) => e.0.iter().find_map(ope),
        Component::DisjointUnion(d) => Some(d.0.0.as_ref()),
        Component::ObjectPropertyRange(d) => ope(&d.ope),
        Component::SubDataPropertyOf(s) => Some(s.sub.0.as_ref()),
        Component::SubAnnotationPropertyOf(s) => Some(s.sub.0.as_ref()),
        _ => None,
    }
}

/// `cmo` as the items a streaming writer takes: the prefixes, then the
/// ontology id, imports and ontology annotations, then each entity's
/// components together (sorted by subject IRI: its declaration and
/// `oboInOwl` id, then xrefs, then other annotation assertions, then
/// axioms), then components with no single subject. Real OBO and OMN files
/// group by entity, and the streaming writers emit a new stanza whenever an
/// entity's components are split across chunks. Only references are sorted;
/// components are cloned as the iterator is consumed. The order is
/// deterministic.
fn stream_items<'a>(
    cmo: &'a ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
    prefixes: &PrefixMapping,
) -> impl Iterator<Item = Result<StreamComponent<AnnotatedComponent<RcStr>>, HornedError>> + 'a {
    use horned_owl::model::Ontology;
    const OIO_ID: &str = "http://www.geneontology.org/formats/oboInOwl#id";
    const OIO_XREF: &str = "http://www.geneontology.org/formats/oboInOwl#hasDbXref";
    let key = |ac: &'a AnnotatedComponent<RcStr>| {
        let c = &ac.component;
        match c {
            Component::OntologyID(_) | Component::Import(_) | Component::OntologyAnnotation(_) => {
                (0u8, "", 0u8, ac)
            }
            _ => match subject_iri(c) {
                Some(s) => {
                    let rank = match c {
                        Component::DeclareClass(_)
                        | Component::DeclareObjectProperty(_)
                        | Component::DeclareDataProperty(_)
                        | Component::DeclareAnnotationProperty(_)
                        | Component::DeclareNamedIndividual(_)
                        | Component::DeclareDatatype(_) => 0,
                        Component::AnnotationAssertion(a) if a.ann.ap.0.as_ref() == OIO_ID => 1,
                        Component::AnnotationAssertion(a) if a.ann.ap.0.as_ref() == OIO_XREF => 2,
                        Component::AnnotationAssertion(_) => 3,
                        _ => 4,
                    };
                    (1, s, rank, ac)
                }
                None => (2, "", 0, ac),
            },
        }
    };
    let mut keyed: Vec<_> = cmo.iter().map(key).collect();
    keyed.sort();
    let prefixes: Vec<_> = prefixes
        .mappings()
        .map(|(n, i)| StreamComponent::Prefix(n.to_string(), i.to_string()))
        .collect();
    prefixes
        .into_iter()
        .chain(
            keyed
                .into_iter()
                .map(|(_, _, _, ac)| StreamComponent::Component(ac.clone())),
        )
        .map(Ok)
}

/// Serialize `cmo` into `fmt`'s byte representation.
///
/// Every horned-owl writer (ofn/omn/owx/obo/rdf) takes a
/// `&ComponentMappedOntology<A, AA>` plus an `Option<&PrefixMapping>`. The
/// rdf writer merges `prefixes` in alongside its own fixed rdf/owl/swrl set.
///
/// Takes the `ComponentMappedOntology` directly rather than a `&SetOntology`
/// to convert internally: a sweep over multiple target formats (see
/// `roundtrip::run_bytes`) calls this once per format against the *same*
/// source model, and converting per call cloned and rebuilt the whole
/// ontology on every one of those calls. Building the `ComponentMappedOntology`
/// once and reusing it across every target format's `write_target` call turns
/// that into a single conversion per source ontology, not one per target.
pub fn write_target(
    fmt: Format,
    cmo: &ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
    prefixes: &PrefixMapping,
) -> anyhow::Result<Vec<u8>> {
    let mut out: Vec<u8> = Vec::new();
    match fmt {
        // `ofn::writer::write` is `write_stream` over the ontology's own
        // component stream, so the OFN streaming targets are the same write.
        Format::Ofn
        | Format::OfnNom
        | Format::OfnStreamWrite
        | Format::OfnNomStream
        | Format::OfnStreamRead => {
            ofn::writer::write(&mut out, cmo, Some(prefixes)).map_err(horned_err)?;
        }
        Format::Omn | Format::OmnNom | Format::OmnStreamRead => {
            omn::writer::write(&mut out, cmo, Some(prefixes)).map_err(horned_err)?;
        }
        Format::OmnStreamWrite | Format::OmnNomStream => {
            omn::writer::write_stream(&mut out, stream_items(cmo, prefixes)).map_err(horned_err)?;
        }
        Format::OwlXml => {
            owx::writer::write(&mut out, cmo, Some(prefixes)).map_err(horned_err)?;
        }
        Format::Obo | Format::OboNom | Format::OboStreamRead => {
            obo::writer::write(&mut out, cmo, Some(prefixes)).map_err(horned_err)?;
        }
        Format::OboStreamWrite | Format::OboNomStream => {
            obo::writer::write_stream(&mut out, stream_items(cmo, prefixes)).map_err(horned_err)?;
        }
        Format::RdfXml => {
            rdf::writer::write(&mut out, cmo, Some(prefixes)).map_err(horned_err)?;
        }
        // Turtle is a read-only (source) format — horned-owl has no Turtle
        // writer, so it never appears as a round-trip target.
        Format::Turtle => anyhow::bail!("cannot write turtle (read-only source format)"),
        Format::Unknown => anyhow::bail!("cannot write unknown format"),
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Format;

    #[test]
    fn stream_items_groups_by_entity() {
        use horned_owl::model::{
            AnnotatedComponent, Annotation, AnnotationAssertion, AnnotationSubject,
            ClassExpression, DeclareClass, Literal, OntologyID, SubClassOf,
        };
        let b = Build::new_rc();
        let mut o = SetOntology::<RcStr>::new();
        let xref = b.annotation_property("http://www.geneontology.org/formats/oboInOwl#hasDbXref");
        let id = b.annotation_property("http://www.geneontology.org/formats/oboInOwl#id");
        let rdfs = b.annotation_property("http://www.w3.org/2000/01/rdf-schema#label");
        let ann = |ap: &horned_owl::model::AnnotationProperty<RcStr>, s: &str, v: &str| {
            Component::AnnotationAssertion(AnnotationAssertion {
                subject: AnnotationSubject::IRI(b.iri(s)),
                ann: Annotation {
                    ap: ap.clone(),
                    av: Literal::Simple { literal: v.into() }.into(),
                    ann: Default::default(),
                },
            })
        };
        let ents = ["http://ex/B", "http://ex/A", "http://ex/C"];
        for e in ents {
            let c = b.class(e);
            // Added in the least helpful order: axiom, label, xref, id, decl.
            o.insert(AnnotatedComponent::from(Component::SubClassOf(
                SubClassOf {
                    sup: ClassExpression::Class(b.class("http://ex/Z")),
                    sub: ClassExpression::Class(c.clone()),
                },
            )));
            o.insert(AnnotatedComponent::from(ann(&rdfs, e, "l")));
            o.insert(AnnotatedComponent::from(ann(&xref, e, "X:1")));
            o.insert(AnnotatedComponent::from(ann(&id, e, "E:1")));
            o.insert(AnnotatedComponent::from(Component::DeclareClass(
                DeclareClass(c),
            )));
        }
        o.insert(AnnotatedComponent::from(Component::OntologyID(
            OntologyID::default(),
        )));
        let cmo: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = o.into();
        let run = || -> Vec<AnnotatedComponent<RcStr>> {
            stream_items(&cmo, &PrefixMapping::default())
                .map(|r| match r.unwrap() {
                    StreamComponent::Component(c) => c,
                    _ => panic!("unexpected prefix"),
                })
                .collect()
        };
        let items = run();
        assert_eq!(items.len(), 16);
        assert_eq!(run(), items, "deterministic");
        assert!(matches!(items[0].component, Component::OntologyID(_)));
        // Contiguous per entity, in subject order, rank order within.
        let subj: Vec<_> = items[1..]
            .iter()
            .map(|c| subject_iri(&c.component).unwrap().to_string())
            .collect();
        let expect: Vec<_> = ["A", "B", "C"]
            .iter()
            .flat_map(|e| std::iter::repeat_n(format!("http://ex/{e}"), 5))
            .collect();
        assert_eq!(subj, expect);
        assert!(matches!(items[1].component, Component::DeclareClass(_)));
        assert!(matches!(items[5].component, Component::SubClassOf(_)));
    }

    #[test]
    fn reads_turtle_source() {
        // A Turtle document declaring one class; the oxrdfio-backed RDF reader
        // (told format=Turtle) should parse it into at least one component.
        let ttl = b"@prefix owl: <http://www.w3.org/2002/07/owl#> .\n\
                    <http://ex/A> a owl:Class .\n";
        let r = read_source(Format::Turtle, ttl).expect("read turtle");
        assert!(
            r.model.iter().count() >= 1,
            "expected >=1 component from turtle"
        );
    }

    #[test]
    fn reads_functional_source() {
        let ofn =
            b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)";
        let r = read_source(Format::Ofn, ofn).expect("read");
        assert!(r.model.iter().count() >= 1);
        assert!(r.incomplete.is_none());
    }

    #[test]
    fn reads_manchester_source() {
        let omn = b"Prefix: : <http://ex/>\nOntology: <http://ex/o>\nClass: <http://ex/A>\n";
        let r = read_source(Format::Omn, omn).expect("read");
        assert!(r.model.iter().count() >= 1);
    }

    #[test]
    fn reads_obo_source() {
        let obo = b"format-version: 1.4\nontology: ex\n\n[Term]\nid: EX:0000001\nname: A\n";
        let r = read_source(Format::Obo, obo).expect("read");
        assert!(r.model.iter().count() >= 1);
        assert!(r.incomplete.is_none());
    }

    #[test]
    fn writes_and_rereads_obo_from_obo_source() {
        // Unlike the other targets, OBO's writer only emits a stanza for
        // entities carrying the oboInOwl:id annotation its own reader
        // stamps -- so this can only be exercised fairly starting from a
        // real OBO source, not an arbitrary OWL construct read from
        // another format (writes_and_rereads_each_target intentionally
        // does not include Obo, for this reason).
        let obo = b"format-version: 1.4\nontology: ex\n\n[Term]\nid: EX:0000001\nname: A\n";
        let src = read_source(Format::Obo, obo).expect("read");
        let cmo: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = src.model.clone().into();

        let bytes = write_target(Format::Obo, &cmo, &src.prefixes).expect("write");
        let back = read_source(Format::Obo, &bytes).expect("reread");

        assert!(
            back.model.iter().any(|ac| matches!(
                &ac.component,
                horned_owl::model::Component::DeclareClass(c)
                    if c.0.0.as_ref().ends_with("EX_0000001")
            )),
            "term stanza lost in OBO round trip, got: {:#?}",
            back.model
        );
    }

    #[test]
    fn unknown_format_is_rejected() {
        let r = read_source(Format::Unknown, b"");
        assert!(r.is_err());
    }

    #[test]
    fn writes_and_rereads_each_target() {
        use crate::model::Format;
        let src = read_source(
            Format::Ofn,
            b"Prefix(ex:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(ex:A))\n)",
        )
        .unwrap();
        let cmo: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = src.model.clone().into();
        for t in [Format::Ofn, Format::Omn, Format::OwlXml, Format::RdfXml] {
            let bytes = write_target(t, &cmo, &src.prefixes).expect("write");
            assert!(!bytes.is_empty(), "empty output for {t:?}");
            let back = read_source(t, &bytes).expect("reread");
            assert!(back.model.iter().count() >= 1, "lost content for {t:?}");
        }
    }
}
