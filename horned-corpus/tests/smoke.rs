// Confirms the exact horned-owl IO signatures the harness relies on.
//
// Pinned rev: 0a9debdbf85243350d3d6edc0dcd617f0ed47d97 (phillord/horned-owl devel HEAD
// as of 2026-07-09). The signatures below were read directly from that commit's
// source under ~/.cargo/git/checkouts/horned-owl-*/0a9debd/src/io/{ofn,omn,owx,rdf}/.
//
// The brief's assumed ofn/omn calls (`.into()` from the tuple, `write(&mut out, &cmo,
// Some(&prefixes))`) matched the real API exactly for those two formats -- no
// adjustment was needed. However the brief's assumption does NOT generalize to all
// four formats: rdf and owx differ from ofn and omn in both the reader's borrowing
// convention and (for rdf) the writer's signature and return shape. Record here for
// Task 4/5:
//
// --- ofn (functional syntax) ---
//   pub fn ofn::reader::read<A, O: MutableOntology<A> + Ontology<A> + Default, R: BufRead>(
//       bufread: R,                     // BY VALUE
//       _config: ParserConfiguration,
//   ) -> Result<(O, PrefixMapping), HornedError>
//   pub fn ofn::writer::write<A, AA: ForIndex<A>, W: Write>(
//       write: W,
//       ont: &ComponentMappedOntology<A, AA>,
//       mapping: Option<&PrefixMapping>,
//   ) -> Result<W, HornedError>
//
// --- omn (Manchester syntax) ---
//   pub fn omn::reader::read<A, O: MutableOntology<A> + Ontology<A> + Default, R: BufRead>(
//       bufread: R,                     // BY VALUE (same shape as ofn)
//       _config: ParserConfiguration,
//   ) -> Result<(O, PrefixMapping), HornedError>
//   pub fn omn::writer::write<A, AA: ForIndex<A>, W: Write>(
//       write: W,
//       ont: &ComponentMappedOntology<A, AA>,
//       mapping: Option<&PrefixMapping>,
//   ) -> Result<W, HornedError>
//   Note: the omn writer emits inexpressible components (general anonymous-subject
//   class axioms, SWRL Rule, anonymous-subject annotation values) into a trailing
//   "# General axioms" block in OWL FUNCTIONAL syntax as a stopgap -- those lines
//   are not valid Manchester. The omn reader skips that block with a warning, so
//   such components will NOT round-trip through omn. Relevant to later
//   categorize/diff tasks.
//
// --- owx (OWL/XML) --- DIFFERS from ofn/omn: reader borrows, not by-value.
//   pub fn owx::reader::read<A, O: MutableOntology<A> + Default, R: BufRead>(
//       bufread: &mut R,                // BY MUTABLE REFERENCE, not by value
//       config: ParserConfiguration,
//   ) -> Result<(O, PrefixMapping), HornedError>
//   pub fn owx::writer::write<A, AA: ForIndex<A>, W: StdWrite>(
//       write: W,
//       ont: &ComponentMappedOntology<A, AA>,
//       mapping: Option<&PrefixMapping>,
//   ) -> Result<W, HornedError>
//   (writer signature matches ofn/omn's shape; only the reader's borrowing differs.)
//
// --- rdf (RDF/XML) --- DIFFERS most: fixed concrete ontology type, no PrefixMapping
// on read, IncompleteParse instead, and no mapping parameter on write.
//   pub fn rdf::reader::read<R: BufRead>(
//       bufread: &mut R,                // BY MUTABLE REFERENCE
//       config: ParserConfiguration,
//   ) -> Result<
//       (ConcreteRDFOntology<RcStr, RcAnnotatedComponent>, IncompleteParse<RcStr>),
//       HornedError,
//   >
//   // No generic O/A type params (fixed to RcStr) and no PrefixMapping in the
//   // return -- instead an IncompleteParse<RcStr>, which callers must check via
//   // `.is_complete()` to detect axioms the RDF reader could not map. Convert
//   // ConcreteRDFOntology -> SetOntology via `.into()` per io/mod.rs's
//   // `From<ParserOutput<..>> for SetOntology<A>` impl.
//   pub fn rdf::writer::write<A, AA: ForIndex<A>, W: Write>(
//       write: W,
//       ont: &ComponentMappedOntology<A, AA>,
//       // NO mapping parameter at all -- rdf writer always emits full/absolute
//       // IRIs plus a small fixed prefix set (rdf/owl/swrl) hardcoded internally.
//   ) -> Result<W, HornedError>
//   Also present: `rdf::writer::write_to_rdf_format(write, ont, format: &str)` for
//   turtle/ntriples/etc. via oxrdfio, and `read_with_build` sister functions on both
//   reader and writer paths of every format for injecting a shared `Build`.
//
// Common thread across all four: every writer returns `Result<W, HornedError>` (the
// writer, not `()`) and takes `&ComponentMappedOntology<A, AA>`, not `&SetOntology`.
// `horned_owl::io::ParserOutput<A, AA>` (io/mod.rs) is an enum wrapping each format's
// raw output and offers `.into()` impls to both `SetOntology<A>` and
// `ComponentMappedOntology<A, AA>`, plus a `.decompose()` -> (SetOntology, Option
// <PrefixMapping>, Option<IncompleteParse<A>>) that normalizes across formats -- but
// note ofn/omn/owx::reader::read do NOT themselves return ParserOutput; they are
// generic over `O: MutableOntology<A> + Ontology<A> + Default` and return `(O,
// PrefixMapping)` directly. Wrapping that tuple in `ParserOutput::ofn(...)` etc. (from
// io/mod.rs) is an opt-in step callers take to get the uniform `.decompose()` seam --
// it is not automatic. Task 4/5's `read_source`/`write_target` should likely do that
// wrapping explicitly to normalize across formats instead of matching per-format
// tuple/enum shapes by hand.
//
// CLIPPY FINDING (confirmed real, not a brief typo): because the `let` bindings below
// pin `O = SetOntology<RcStr>` via their type annotation, `ofn::reader::read(...)
// .into()` and `omn::reader::read(...).into()` are IDENTITY conversions (O is already
// SetOntology<RcStr>, there is no ParserOutput in the picture at all here) --
// `clippy::useless_conversion` flags both. The brief's `.into()` was not wrong (it
// still compiles and passes) but it is dead code for this generic-O calling
// convention; removed below to keep `cargo clippy` clean.
// Real ParserOutput-based dispatch (if Task 4/5 wants runtime format detection rather
// than calling the right format's `reader::read` directly) would need the explicit
// `ParserOutput::ofn((so, pm))` wrap described above, and `.into()`/`.decompose()`
// would then be doing real work on that enum, not on a plain tuple.
//
// If any of these calls do not compile, adjust src/ontology.rs (Task 4/5) to match
// the pinned commit's real API before proceeding.
use horned_owl::io::ParserConfiguration;
use horned_owl::io::ofn;
use horned_owl::io::omn;
use horned_owl::model::{Build, RcAnnotatedComponent, RcStr};
use horned_owl::ontology::component_mapped::ComponentMappedOntology;
use horned_owl::ontology::set::SetOntology;
use std::io::Cursor;

const OFN: &str =
    "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)";

#[test]
fn ofn_read_to_omn_write_and_back() {
    let build = Build::new_rc();
    // read functional
    let (so, prefixes): (SetOntology<RcStr>, _) =
        ofn::reader::read(&mut Cursor::new(OFN), ParserConfiguration::new(&build))
            .expect("read ofn");
    // SetOntology -> ComponentMappedOntology for writing
    let cmo: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = so.clone().into();
    // write manchester
    let mut out: Vec<u8> = Vec::new();
    omn::writer::write(&mut out, &cmo, Some(&prefixes)).expect("write omn");
    assert!(!out.is_empty());
    // read it back
    let (so2, _): (SetOntology<RcStr>, _) =
        omn::reader::read(&mut Cursor::new(&out), ParserConfiguration::new(&build))
            .expect("read omn");
    assert!(so2.iter().count() >= 1);
}
