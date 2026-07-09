//! Source-read adapter: turns raw bytes in a known [`Format`] into a
//! `SetOntology<RcStr>`, capturing whatever completeness information the
//! underlying horned-owl reader can provide.
//!
//! Only the RDF/XML reader can produce an incomplete parse (see
//! `horned_owl::io::rdf::reader::IncompleteParse`); the ofn/omn/owx readers
//! either fully succeed or return a hard error, so they always report
//! `incomplete: None`.
//!
//! Dispatch goes through `horned_owl::io::ParserOutput` and its
//! `.decompose()` method (see the API notes in `tests/smoke.rs`) rather than
//! matching each format's raw tuple/struct shape by hand -- that seam
//! normalizes ofn/omn/owx/rdf into a single
//! `(SetOntology<A>, Option<PrefixMapping>, Option<IncompleteParse<A>>)`
//! triple.

use crate::model::{Format, IncompleteSummary};
use curie::PrefixMapping;
use horned_owl::error::HornedError;
use horned_owl::io::{ofn, omn, owx, rdf, ParserOutput};
use horned_owl::model::{RcAnnotatedComponent, RcStr};
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

pub fn read_source(fmt: Format, bytes: &[u8]) -> anyhow::Result<ReadOk> {
    // The AA (indexed-annotated-component) type parameter is only ever
    // exercised by the RDF variant; the ofn/omn/owx variants carry a plain
    // `SetOntology` and so give the compiler nothing to infer AA from. Pin
    // it explicitly to `RcAnnotatedComponent`, the same type the RDF
    // reader's fixed `ConcreteRDFOntology<RcStr, RcAnnotatedComponent>`
    // return type uses.
    type Output = ParserOutput<RcStr, RcAnnotatedComponent>;

    let (model, prefixes, incomplete) = match fmt {
        Format::Ofn => {
            let sop =
                ofn::reader::read(Cursor::new(bytes), Default::default()).map_err(horned_err)?;
            Output::ofn(sop).decompose()
        }
        Format::Omn => {
            let sop =
                omn::reader::read(Cursor::new(bytes), Default::default()).map_err(horned_err)?;
            Output::omn(sop).decompose()
        }
        Format::OwlXml => {
            let sop = owx::reader::read(&mut Cursor::new(bytes), Default::default())
                .map_err(horned_err)?;
            Output::owx(sop).decompose()
        }
        Format::RdfXml => {
            let rop = rdf::reader::read(&mut Cursor::new(bytes), Default::default())
                .map_err(horned_err)?;
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

/// Serialize `model` into `fmt`'s byte representation.
///
/// Every horned-owl writer (ofn/omn/owx/rdf) takes a
/// `&ComponentMappedOntology<A, AA>`, not a `&SetOntology`, so `model` is
/// converted via `.into()` first. The ofn/omn/owx writers additionally take
/// an `Option<&PrefixMapping>` to control prefix-qualified output; the rdf
/// writer takes no such parameter at all -- it always emits full/absolute
/// IRIs plus a small fixed prefix set (rdf/owl/swrl) hardcoded internally
/// (see `tests/smoke.rs`'s API notes) -- so the `RdfXml` arm must not pass
/// `prefixes` to it.
pub fn write_target(
    fmt: Format,
    model: &SetOntology<RcStr>,
    prefixes: &PrefixMapping,
) -> anyhow::Result<Vec<u8>> {
    let cmo: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = model.clone().into();
    let mut out: Vec<u8> = Vec::new();
    match fmt {
        Format::Ofn => {
            ofn::writer::write(&mut out, &cmo, Some(prefixes)).map_err(horned_err)?;
        }
        Format::Omn => {
            omn::writer::write(&mut out, &cmo, Some(prefixes)).map_err(horned_err)?;
        }
        Format::OwlXml => {
            owx::writer::write(&mut out, &cmo, Some(prefixes)).map_err(horned_err)?;
        }
        Format::RdfXml => {
            rdf::writer::write(&mut out, &cmo).map_err(horned_err)?;
        }
        Format::Unknown => anyhow::bail!("cannot write unknown format"),
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Format;

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
        for t in [Format::Ofn, Format::Omn, Format::OwlXml, Format::RdfXml] {
            let bytes = write_target(t, &src.model, &src.prefixes).expect("write");
            assert!(!bytes.is_empty(), "empty output for {t:?}");
            let back = read_source(t, &bytes).expect("reread");
            assert!(back.model.iter().count() >= 1, "lost content for {t:?}");
        }
    }
}
