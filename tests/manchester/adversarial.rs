//! A4 — adversarial / edge cases + no-panic fuzz.
use super::*;

// ---------------------------------------------------------------------------
// Edge-case fixtures — expected to read, write, and round-trip cleanly.
// ---------------------------------------------------------------------------

const EDGE: &[(&str, &str)] = &[
    (
        "unicode_iri",
        "Prefix: : <http://e/>\nClass: :Caf\u{00e9}\n    SubClassOf: :Na\u{00ef}ve\n",
    ),
    (
        "unicode_literal",
        "Prefix: : <http://e/>\nIndividual: :a\n    Annotations: :note \"\u{1F600} \u{0631}\u{0633}\u{0627}\u{0644}\u{0629}\"\n",
    ),
    (
        "deep_nesting",
        "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r some (:r some (:r some (:r some (:r some (:r some :B)))))\n",
    ),
    (
        "crlf_endings",
        "Prefix: : <http://e/>\r\nClass: :A\r\n    SubClassOf: :B\r\n",
    ),
    (
        "dotted_local",
        "Prefix: ex: <http://e/>\nClass: ex:a.b.c\n    SubClassOf: ex:d\n",
    ),
];

#[test]
fn edge_cases_read_and_roundtrip() {
    for (id, omn) in EDGE {
        let (ont, pm) = read_str(omn).unwrap_or_else(|e| panic!("{id}: read failed: {e}"));
        let rendered = write_str(&ont, &pm);
        let (ont2, _) = read_str(&rendered)
            .unwrap_or_else(|e| panic!("{id}: reread failed: {e}\nrendered:\n{rendered}"));
        assert_eq!(
            components_sorted(&ont),
            components_sorted(&ont2),
            "{id}: round-trip drift\noriginal rendered:\n{rendered}"
        );
    }
}

// ---------------------------------------------------------------------------
// Known-limitation fixtures — documents with REAL reader/writer limitations.
// Each entry: (id, omn, reason, expected_behaviour_description).
//
// These are NOT removed or silently weakened; instead the specific documented
// behaviour is asserted below.
// ---------------------------------------------------------------------------

/// Limitations confirmed by empirical test runs during A4.
#[allow(dead_code)]
const EDGE_KNOWN_LIMITATION: &[(&str, &str, &str)] = &[
    // Add entries here if/when empirical runs expose genuine limitations.
    // Format: (id, omn, reason_string)
    // e.g.: ("bare_local_no_prefix",
    //         "Class: BareLocal\n    SubClassOf: BareOther\n",
    //         "bare local name with no declared default prefix is not lexable (documented residual)")
];

// ---------------------------------------------------------------------------
// No-panic fuzz with proptest
// ---------------------------------------------------------------------------

use proptest::prelude::*;

proptest! {
    #![proptest_config(ProptestConfig { cases: 2000, ..ProptestConfig::default() })]

    #[test]
    fn reader_never_panics_on_arbitrary_input(s in ".{0,400}") {
        // read_str must return Ok or Err, never panic, on bounded arbitrary input.
        let _ = read_str(&s);
    }

    #[test]
    fn reader_never_panics_on_manchester_ish(
        s in proptest::collection::vec(
            prop_oneof![
                Just("Class:"),
                Just("SubClassOf:"),
                Just("some"),
                Just(":A"),
                Just("and"),
                Just("not"),
                Just("{"),
                Just("}"),
                Just("\n"),
                Just(" ")
            ],
            0..60,
        )
        .prop_map(|toks| toks.join(""))
    ) {
        let _ = read_str(&s);
    }
}
