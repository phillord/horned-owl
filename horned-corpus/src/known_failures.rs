//! A hand-curated list of corpus cases known to fail for a documented,
//! understood reason -- so a `roundtrip` run can skip them by default (see
//! `--run-all` on the `roundtrip` subcommand) and `report()` can tell
//! "new, uninvestigated" findings apart from "known, tracked elsewhere" ones,
//! instead of the same cases resurfacing in every run's report forever with
//! no way to mark them seen.
//!
//! The list itself lives in `known_failures.json`, checked into this crate
//! (not the machine-specific `~/src/rust/ontology-corpus/` tree the corpus
//! and results live in) -- it documents an understanding about horned-owl's
//! behaviour, not a run's output, so it's compiled in via `include_str!`
//! rather than read from disk at run time.

use crate::model::{CaseResult, Category, Format, Outcome};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct KnownFailure {
    /// The ontology name, as it appears in `CaseResult::ontology`.
    pub ontology: String,
    /// Restrict to one source format; `None` (omit the field, or `null`)
    /// matches any source format.
    #[serde(default)]
    pub source_format: Option<Format>,
    /// Restrict to one target format; `None` matches any target format.
    #[serde(default)]
    pub target_format: Option<Format>,
    /// Why this case is expected to fail, and the GitHub issue tracking it if
    /// there is one. Shown verbatim in the report, so write it for a reader
    /// who hasn't seen the original investigation.
    pub reason: String,
}

impl KnownFailure {
    /// Whether this entry covers the given case.
    pub fn matches(&self, ontology: &str, source: Format, target: Format) -> bool {
        self.ontology == ontology
            && self.source_format.is_none_or(|f| f == source)
            && self.target_format.is_none_or(|f| f == target)
    }
}

/// Parse a known-failures list from JSON text (an array of `KnownFailure`).
fn parse(text: &str) -> anyhow::Result<Vec<KnownFailure>> {
    Ok(serde_json::from_str(text)?)
}

/// The list checked into this crate, embedded at compile time.
///
/// # Panics
///
/// If `known_failures.json` is malformed -- that's a bug in the checked-in
/// file, not something a caller can recover from.
pub fn known_failures() -> Vec<KnownFailure> {
    parse(include_str!("../known_failures.json")).expect("known_failures.json must parse")
}

/// Cases whose reread needs the reader in lax mode, because the source
/// ontology is invalid and the writer reproduces the invalid content.
/// Same entry shape as `known_failures.json`, in `requires_lax.json`; the
/// `reason` says what is invalid. Keeping this a list, not a global
/// setting, limits lax reading to the cases that need it.
pub fn requires_lax() -> &'static [KnownFailure] {
    static LAX: std::sync::OnceLock<Vec<KnownFailure>> = std::sync::OnceLock::new();
    LAX.get_or_init(|| {
        parse(include_str!("../requires_lax.json")).expect("requires_lax.json must parse")
    })
}

/// Find the first entry (if any) covering the given case.
pub fn find<'a>(
    known: &'a [KnownFailure],
    ontology: &str,
    source: Format,
    target: Format,
) -> Option<&'a KnownFailure> {
    known.iter().find(|k| k.matches(ontology, source, target))
}

/// Whether a case that was actually run still shows a real failure: it did
/// not complete, or it round-tripped with an `Unknown` or `AnnotationLoss`
/// diff. A skipped case was not run, so it shows nothing either way.
pub fn still_fails(c: &CaseResult) -> bool {
    match c.outcome {
        Outcome::Ok => {
            let n = |cat| c.category_counts.get(&cat).copied().unwrap_or(0);
            n(Category::Unknown) + n(Category::AnnotationLoss) > 0
        }
        Outcome::Skipped => false,
        _ => true,
    }
}

/// What re-running a `known_failures` entry found.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EntryStatus {
    /// At least one case the entry covers still fails.
    StillFails,
    /// Every case the entry covers now passes, so the entry is stale.
    NowPasses,
    /// No case the entry covers was run, so nothing can be said.
    NotChecked,
}

/// Judge each entry against `cases`, which must have been run with the
/// known-failure skip off. An entry with wildcard formats covers several
/// cases and is only stale when none of them fail.
pub fn check<'a>(
    known: &'a [KnownFailure],
    cases: &[CaseResult],
) -> Vec<(&'a KnownFailure, EntryStatus)> {
    known
        .iter()
        .map(|k| {
            let run: Vec<&CaseResult> = cases
                .iter()
                .filter(|c| {
                    c.outcome != Outcome::Skipped
                        && k.matches(&c.ontology, c.source_format, c.target_format)
                })
                .collect();
            let status = if run.is_empty() {
                EntryStatus::NotChecked
            } else if run.iter().any(|c| still_fails(c)) {
                EntryStatus::StillFails
            } else {
                EntryStatus::NowPasses
            };
            (k, status)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn case(ontology: &str, target: Format, outcome: Outcome, unknown: usize) -> CaseResult {
        CaseResult {
            ontology: ontology.into(),
            source_format: Format::OwlXml,
            target_format: target,
            outcome,
            error: None,
            exact: unknown == 0,
            diffs: Vec::new(),
            category_counts: if unknown > 0 {
                [(Category::Unknown, unknown)].into()
            } else {
                Default::default()
            },
            write_us: None,
            reread_us: None,
        }
    }

    fn entry(ontology: &str, target: Option<Format>) -> KnownFailure {
        KnownFailure {
            ontology: ontology.into(),
            source_format: None,
            target_format: target,
            reason: "test".into(),
        }
    }

    #[test]
    fn entry_is_stale_only_when_no_covered_case_fails() {
        let known = [entry("A", Some(Format::RdfXml)), entry("B", None)];
        let cases = [
            case("A", Format::RdfXml, Outcome::Ok, 0),
            // B has a wildcard target; one failing case keeps it live.
            case("B", Format::RdfXml, Outcome::Ok, 0),
            case("B", Format::Omn, Outcome::Ok, 3),
        ];
        let got: Vec<_> = check(&known, &cases).into_iter().map(|(_, s)| s).collect();
        assert_eq!(got, [EntryStatus::NowPasses, EntryStatus::StillFails]);
    }

    #[test]
    fn unrun_and_skipped_cases_are_not_checked() {
        let known = [entry("A", None), entry("C", None)];
        let cases = [case("A", Format::RdfXml, Outcome::Skipped, 0)];
        let got: Vec<_> = check(&known, &cases).into_iter().map(|(_, s)| s).collect();
        assert_eq!(got, [EntryStatus::NotChecked, EntryStatus::NotChecked]);
    }

    #[test]
    fn a_failed_reread_still_fails() {
        assert!(still_fails(&case(
            "A",
            Format::RdfXml,
            Outcome::RereadFail,
            0
        )));
        assert!(!still_fails(&case("A", Format::RdfXml, Outcome::Ok, 0)));
    }

    #[test]
    fn wildcard_format_matches_any() {
        let k = KnownFailure {
            ontology: "CRENO".into(),
            source_format: None,
            target_format: Some(Format::RdfXml),
            reason: "test".into(),
        };
        assert!(k.matches("CRENO", Format::OwlXml, Format::RdfXml));
        assert!(k.matches("CRENO", Format::Ofn, Format::RdfXml));
        assert!(!k.matches("CRENO", Format::OwlXml, Format::Omn));
        assert!(!k.matches("OTHER", Format::OwlXml, Format::RdfXml));
    }

    #[test]
    fn exact_format_match_required_when_given() {
        let k = KnownFailure {
            ontology: "NIO".into(),
            source_format: Some(Format::OwlXml),
            target_format: Some(Format::RdfXml),
            reason: "test".into(),
        };
        assert!(k.matches("NIO", Format::OwlXml, Format::RdfXml));
        assert!(!k.matches("NIO", Format::Ofn, Format::RdfXml));
    }

    #[test]
    fn parses_a_minimal_entry() {
        let v = parse(r#"[{"ontology": "CRENO", "target_format": "rdf_xml", "reason": "test"}]"#)
            .unwrap();
        assert_eq!(v.len(), 1);
        assert_eq!(v[0].ontology, "CRENO");
        assert_eq!(v[0].source_format, None);
        assert_eq!(v[0].target_format, Some(Format::RdfXml));
    }

    #[test]
    fn requires_lax_covers_ucva_only_for_its_case() {
        let lax = requires_lax();
        assert!(find(lax, "UCVA", Format::OwlXml, Format::RdfXml).is_some());
        assert!(find(lax, "UCVA", Format::OwlXml, Format::Ofn).is_none());
        assert!(find(lax, "CRENO", Format::OwlXml, Format::RdfXml).is_none());
    }

    #[test]
    fn the_checked_in_file_parses_and_covers_creno() {
        let known = known_failures();
        assert!(!known.is_empty());
        assert!(find(&known, "CRENO", Format::OwlXml, Format::RdfXml).is_some());
    }
}
