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

use crate::model::Format;
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
    /// Why this case is expected to fail and where it's tracked (an issue
    /// number, a doc, a memory name, ...). Shown verbatim in the report, so
    /// write it for a reader who isn't already in this conversation.
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

/// Find the first entry (if any) covering the given case.
pub fn find<'a>(
    known: &'a [KnownFailure],
    ontology: &str,
    source: Format,
    target: Format,
) -> Option<&'a KnownFailure> {
    known.iter().find(|k| k.matches(ontology, source, target))
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn the_checked_in_file_parses_and_covers_creno() {
        let known = known_failures();
        assert!(!known.is_empty());
        assert!(find(&known, "CRENO", Format::OwlXml, Format::RdfXml).is_some());
    }
}
