//! Round-trip engine: ties detect -> read -> {write -> reread -> canonicalize
//! -> diff -> categorize} together per ontology, for every target format.
//!
//! Every horned-owl call (`read_source`, `write_target`, and the reread call
//! into `read_source` again) is wrapped in `catch_unwind` -- the corpus this
//! harness is meant to run over is untrusted, and horned-owl's model is built
//! on `Rc`, so a panicking parse/writer must not abort the whole run. Since
//! the closures capture (or return) `Rc`-bearing types, which are not
//! `UnwindSafe`, each call is additionally wrapped in `AssertUnwindSafe`: a
//! caught panic here never resumes execution on the partially-built value, it
//! only ever gets thrown away in favor of an `Outcome::Panic` record, so the
//! lack of unwind-safety guarantees is immaterial.

use crate::canon::canonicalize;
use crate::categorize::categorize;
use crate::detect::detect;
use crate::diff::diff;
use crate::model::*;
use crate::ontology::{ReadOk, read_source, write_target};
use horned_owl::model::RcStr;
use horned_owl::ontology::set::SetOntology;
use std::collections::BTreeMap;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::time::Instant;

/// File extension to hand ROBOT/the OWL API for `sfmt`'s bytes, so it
/// dispatches to the same syntax horned-owl's own reader used. `Unknown`
/// has no sensible extension -- callers must not reach ROBOT with it (see
/// `run_bytes`, which only attempts the profile check on a successful
/// source read, and a successful read never reports `Format::Unknown`).
fn robot_ext(fmt: Format) -> &'static str {
    match fmt {
        Format::RdfXml => "rdf",
        Format::OwlXml => "owx",
        Format::Ofn => "ofn",
        Format::Omn => "omn",
        Format::Turtle => "ttl",
        Format::Unknown => "owl",
    }
}

/// Run the full read -> {write -> reread -> diff}* pipeline for one
/// ontology's raw bytes against every format in `formats`, plus (per
/// `profile_mode`) a profile-conformance check.
///
/// Returns exactly one `Record::Source` (the source-read report), optionally
/// one `Record::Profile`, followed by one `Record::Case` per entry in
/// `formats`, in `formats` order. If the source read fails or panics, only
/// the `Record::Source` is returned and no `Record::Profile`/`Record::Case`s
/// are produced -- there is nothing to round-trip or profile-check without a
/// model.
pub fn run_bytes(
    ontology: &str,
    bytes: &[u8],
    formats: &[Format],
    profile_mode: ProfileCheckMode,
) -> Vec<Record> {
    let mut recs = Vec::new();
    let sfmt = detect(bytes);

    let t0 = Instant::now();
    let read = catch_unwind(AssertUnwindSafe(|| read_source(sfmt, bytes)));
    let read_us = Some(t0.elapsed().as_micros() as u64);

    let src = match read {
        Ok(Ok(r)) => {
            recs.push(Record::Source(SourceReadReport {
                ontology: ontology.into(),
                source_format: sfmt,
                outcome: Outcome::Ok,
                is_complete: r.incomplete.is_none(),
                incomplete: r.incomplete.clone(),
                error: None,
                read_us,
            }));
            r
        }
        Ok(Err(e)) => {
            recs.push(src_fail(
                ontology,
                sfmt,
                Outcome::ReadFail,
                e.to_string(),
                read_us,
            ));
            return recs;
        }
        Err(_) => {
            recs.push(src_fail(
                ontology,
                sfmt,
                Outcome::Panic,
                "panic".into(),
                read_us,
            ));
            return recs;
        }
    };

    if profile_mode != ProfileCheckMode::Off {
        let with_robot = profile_mode == ProfileCheckMode::HornedAndRobot;
        let ext = robot_ext(sfmt);
        let result = catch_unwind(AssertUnwindSafe(|| {
            crate::profile::check(ontology, &src.model, bytes, ext, with_robot)
        }));
        match result {
            Ok(r) => recs.push(Record::Profile(r)),
            Err(_) => eprintln!("warning: profile check panicked for {ontology}"),
        }
    }

    let src_canon = canonicalize(src.model.clone());
    for &t in formats {
        recs.push(Record::Case(one_case(ontology, sfmt, t, &src, &src_canon)));
    }
    recs
}

/// Build the `Record::Source` for a failed (or panicked) source read.
fn src_fail(
    ontology: &str,
    sfmt: Format,
    outcome: Outcome,
    error: String,
    read_us: Option<u64>,
) -> Record {
    Record::Source(SourceReadReport {
        ontology: ontology.into(),
        source_format: sfmt,
        outcome,
        is_complete: false,
        incomplete: None,
        error: Some(error),
        read_us,
    })
}

/// Write `src.model` into `tfmt`, reread it, canonicalize both sides, diff,
/// and categorize -- producing one `CaseResult` for the (sfmt -> tfmt) case.
fn one_case(
    ontology: &str,
    sfmt: Format,
    tfmt: Format,
    src: &ReadOk,
    src_canon: &SetOntology<RcStr>,
) -> CaseResult {
    let t0 = Instant::now();
    let write = catch_unwind(AssertUnwindSafe(|| {
        write_target(tfmt, &src.model, &src.prefixes)
    }));
    let write_us = Some(t0.elapsed().as_micros() as u64);

    let bytes = match write {
        Ok(Ok(b)) => b,
        Ok(Err(e)) => {
            return case_fail(
                ontology,
                sfmt,
                tfmt,
                Outcome::WriteFail,
                e.to_string(),
                write_us,
                None,
            );
        }
        Err(_) => {
            return case_fail(
                ontology,
                sfmt,
                tfmt,
                Outcome::Panic,
                "panic".into(),
                write_us,
                None,
            );
        }
    };

    let t1 = Instant::now();
    let reread = catch_unwind(AssertUnwindSafe(|| read_source(tfmt, &bytes)));
    let reread_us = Some(t1.elapsed().as_micros() as u64);

    let rt = match reread {
        Ok(Ok(r)) => r,
        Ok(Err(e)) => {
            return case_fail(
                ontology,
                sfmt,
                tfmt,
                Outcome::RereadFail,
                e.to_string(),
                write_us,
                reread_us,
            );
        }
        Err(_) => {
            return case_fail(
                ontology,
                sfmt,
                tfmt,
                Outcome::Panic,
                "panic".into(),
                write_us,
                reread_us,
            );
        }
    };

    let rt_canon = canonicalize(rt.model);
    let raw = diff(src_canon, &rt_canon);
    let exact = raw.only_in_source.is_empty() && raw.only_in_roundtrip.is_empty();
    let diffs = categorize(raw, src_canon, &rt_canon);

    let mut category_counts: BTreeMap<Category, usize> = BTreeMap::new();
    for d in &diffs {
        *category_counts.entry(d.category).or_insert(0) += 1;
    }

    CaseResult {
        ontology: ontology.into(),
        source_format: sfmt,
        target_format: tfmt,
        outcome: Outcome::Ok,
        error: None,
        exact,
        diffs,
        category_counts,
        write_us,
        reread_us,
    }
}

/// Build a `CaseResult` for a failed (or panicked) write/reread step. There
/// is no model to diff, so `diffs`/`category_counts` are empty and `exact` is
/// `false` -- a case that never completed the round-trip cannot be exact.
fn case_fail(
    ontology: &str,
    sfmt: Format,
    tfmt: Format,
    outcome: Outcome,
    error: String,
    write_us: Option<u64>,
    reread_us: Option<u64>,
) -> CaseResult {
    CaseResult {
        ontology: ontology.into(),
        source_format: sfmt,
        target_format: tfmt,
        outcome,
        error: Some(error),
        exact: false,
        diffs: Vec::new(),
        category_counts: BTreeMap::new(),
        write_us,
        reread_us,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Format, Outcome, Record};

    #[test]
    fn produces_source_and_case_records() {
        let ofn =
            b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)";
        let recs = run_bytes("t", ofn, &[Format::Ofn, Format::Omn], ProfileCheckMode::Off);
        assert!(matches!(recs[0], Record::Source(_)));
        let cases = recs.iter().filter(|r| matches!(r, Record::Case(_))).count();
        assert_eq!(cases, 2);
        // S->S (ofn->ofn) should be exact
        if let Some(Record::Case(c)) = recs
            .iter()
            .find(|r| matches!(r, Record::Case(cc) if cc.target_format==Format::Ofn))
        {
            assert!(c.exact);
            assert_eq!(c.outcome, Outcome::Ok);
        } else {
            panic!("expected an ofn->ofn Record::Case");
        }
    }

    #[test]
    fn source_record_is_produced_for_rdf_xml() {
        // Covers the RDF read branch through the engine end-to-end (no test
        // elsewhere exercises Format::RdfXml through run_bytes). A tiny
        // RDF/XML document declaring one class should read completely (no
        // leftover triples) and produce exactly one Source record plus one
        // Case record for the single requested target format.
        let rdf = br#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:owl="http://www.w3.org/2002/07/owl#">
  <owl:Ontology rdf:about="http://ex/o"/>
  <owl:Class rdf:about="http://ex/A"/>
</rdf:RDF>"#;
        let recs = run_bytes("rdf-t", rdf, &[Format::RdfXml], ProfileCheckMode::Off);
        match &recs[0] {
            Record::Source(r) => {
                assert_eq!(r.source_format, Format::RdfXml);
                assert_eq!(r.outcome, Outcome::Ok);
                assert!(r.is_complete, "expected a complete RDF/XML parse: {r:?}");
            }
            other => panic!("expected Record::Source, got {other:?}"),
        }
        let cases = recs.iter().filter(|r| matches!(r, Record::Case(_))).count();
        assert_eq!(cases, 1);
    }

    #[test]
    fn covers_write_fail_branch() {
        // Tests the write failure path: attempt to write to Format::Unknown
        // format, which deterministically fails with a clear error.
        // The source read succeeds, but the write fails, so no reread is
        // attempted.
        let ofn =
            b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)";
        let recs = run_bytes("t", ofn, &[Format::Unknown], ProfileCheckMode::Off);
        assert_eq!(recs.len(), 2, "expected source + case records");

        // Verify source record is Ok
        match &recs[0] {
            Record::Source(r) => {
                assert_eq!(r.outcome, Outcome::Ok, "source read should succeed");
            }
            other => panic!("expected Record::Source at index 0, got {other:?}"),
        }

        // Verify case record shows WriteFail outcome
        match &recs[1] {
            Record::Case(c) => {
                assert_eq!(c.outcome, Outcome::WriteFail);
                assert!(c.error.is_some(), "WriteFail should have an error message");
                assert!(!c.exact, "WriteFail cannot be exact");
                assert!(c.diffs.is_empty(), "WriteFail should have no diffs");
                assert_eq!(
                    c.reread_us, None,
                    "reread should not have run after write failure"
                );
            }
            other => panic!("expected Record::Case at index 1, got {other:?}"),
        }
    }

    #[test]
    fn covers_source_read_fail_skip_round_trips() {
        // Tests the source ReadFail path: garbage input that detect maps to
        // Format::Unknown, and read_source(Unknown, garbage) fails. No Case
        // records are produced because the source read failed -- round-trips
        // are skipped entirely.
        let recs = run_bytes(
            "t",
            b"garbage not an ontology",
            &[Format::Ofn, Format::Omn],
            ProfileCheckMode::Off,
        );
        assert_eq!(
            recs.len(),
            1,
            "expected only source record when source read fails; no cases should be produced"
        );

        // Verify single record is a failed source read
        match &recs[0] {
            Record::Source(r) => {
                assert_eq!(r.outcome, Outcome::ReadFail);
            }
            other => panic!("expected Record::Source with ReadFail, got {other:?}"),
        }
    }
}
