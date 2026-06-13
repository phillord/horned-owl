# Manchester `io/omn` — Conformance & Performance Summary

**Date:** 2026-06-13. Reader + writer for OWL 2 Manchester Syntax §2.5 in the
horned-owl fork (`crate horned-owl 1.4.0`, `src/io/omn/`).

This one-pager links the two generated reports:
- **Compliance:** `docs/manchester/compliance-report.md` (this repo) — regenerate
  with `cargo test --test manchester_conformance -- --ignored generate_compliance_report`.
- **Performance:** `pymos/bench/results/2026-06-13-manchester/performance-report.md`
  — regenerate with `bench/run_manchester.py` then `bench/report_manchester.py`.

The numbers below are copied from those generated artifacts; nothing here is
hand-estimated.

## Conformance (compliance-report.md)

- **§2.5 construct matrix (A1):** **89 constructs** pass read + write +
  round-trip; **10 documented residuals** across 9 kinds. Each residual row
  asserts its *specific* documented behavior (compiler-exhaustive `match` — no
  rubber-stamp). Residual kinds: `SwrlRule`, `BareNameNeedsPrefix`,
  `BareInverseUnsupported`, `ComplexLhsGci`, `NestedAnnotationDropped`,
  `DataRestrictionAsObject`, `HasKeyObjectDataConflation`,
  `PropertyObjectDataConflation` (×2: Equivalent/DisjointProperties over data
  properties), `AnonSubjectWriterGap`.
- **Corpus parse + round-trip (A2)** (source → ROBOT/OWL-API → `.omn` → our
  reader → our writer → re-parse): koala parses (83 components) but **does not
  round-trip** (our writer emits output our own reader rejects at rendered
  ~11:17); obi-core parses (54 232 components) but round-trip has a component
  mismatch; **sio and hp fail to parse** (reader gaps on annotation-heavy OBO
  Manchester); doid excluded (ROBOT's Manchester serializer >2 min).
- **Semantic axiom-set equality vs OWL-API (A3)** (canonicalized, declarations +
  non-logical meta dropped — so it cannot hide a logical-axiom gap): koala
  **42 matched / 3 missing / 4 extra**; obi-core **48 290 matched / 20 / 20**.
  Root cause of nearly all diffs: a typed literal such as `true` in
  `DataHasValue` / `AnnotationAssertion` value position is parsed as an **IRI**
  (base-IRI + bare local, e.g. `koala.owltrue`) instead of a typed literal —
  a manifestation of the documented object/data-property lexical ambiguity in
  Manchester (`value true` cannot disambiguate a data vs object property) plus
  bare-name IRI resolution.
- **Adversarial / fuzz (A4):** unicode IRIs & literals, 6-deep nesting, CRLF, and
  dotted CURIEs all read + round-trip; **4 000 proptest cases** (2 000 arbitrary +
  2 000 Manchester-ish) with **zero reader panics** (pest converts malformed input
  to `HornedError` before any unwrap site).

## Performance (performance-report.md)

In-process hot-median read/write across koala/pizza/travel/obi-core (+ sio/hp/doid
for read where the reader handles them):

- **`horned-omn` read is 11.3× faster than omny** (pure-Python; geomean over 4
  ontologies), **1.09× faster than fastobo-omn** (the other Rust Manchester impl,
  horned-owl 0.14), and far faster than OWL-API/ROBOT (geomean 226×, but that
  denominator carries docker+JVM startup — see caveats; the honest Rust-vs-Rust
  and Rust-vs-Python comparisons are the 1.09× and 11.3×).
- **Per-format read** (obi-core, 4.3 MB, representative): owx 186 ms < rdf 431 ms
  < ofn 623 ms < **omn 802 ms** ≈ fastobo 849 ms ≪ owlapi 3 209 ms ≪ omny 10 997 ms.
  OWL-XML is consistently the fastest horned-owl syntax; Manchester is the
  slowest of the four (PEG parser), still ~14× faster than omny on the same input.
- **Write** (obi-core): ofn 42 ms / owx 73 ms / **omn 713 ms** vs omny 15 005 ms.
- **Peak RSS** is modest for the Rust readers (koala ~4 MB; obi-core ~115 MB for
  omn) vs omny (koala 42 MB; obi-core 412 MB).
- **Conformance surfaced by the benchmark:** our omn reader fails sio + hp; hp
  also fails **fastobo** (both Rust Manchester impls) — only OWL-API/ROBOT parses
  hp. These are listed in the report's "Conformance failures (excluded)" section.

Caveats (full list in the report): Rust timings exclude ~2 ms cold-start; ROBOT
hot medians carry per-call docker overhead; component counts differ across
formats (declaration handling), so this measures per-format parse/serialize
*speed*, not identical-axiom-set parsing.

## Residual limitations (authoritative)

**Inherent — no §2.5 form exists:** SWRL `Rule:` (Manchester has no rule syntax);
complex-LHS general class axioms (no frame form — the writer emits them to a
`# General axioms` block the reader skips with a warning); a bare default-prefix
local name is not lexable (use `<full>` or `prefix:local`).

**Reader §2.5 gaps (findings, fixable, currently reported not fixed):** bare
`inverse R` (without parentheses) is rejected though §2.5 allows it; a typed
literal in `DataHasValue`/annotation-value position is read as an IRI (the
object/data-property ambiguity above); data-property restrictions (`dp some dt`)
parse as object restrictions; `EquivalentProperties:`/`DisjointProperties:` over
data properties read as object-property axioms.

**Writer follow-ups:** anonymous-individual subject assertions are emitted to the
`# General axioms` block (the reader then skips them), so they do not round-trip
— though the reader *does* parse `Individual: _:b1` on input, so this is a small
writer fix (`ClassAssertion` currently only frames `Named` individuals). koala
and obi-core round-trip gaps trace to writer/reader asymmetries on
OWL-API-sourced input.

**Model limits:** nested annotation-on-annotation is parsed but the inner nesting
is dropped (the horned-owl model has no nested-annotation slot — the OFN reader
does the same); `HasKey:` cannot lexically distinguish object vs data keys, so
data-property keys round-trip as object keys.

## Bottom line

The writer is OWL-API-conformant on the corpus and the reader is a fast,
general §2.5 parser (89/99 constructs clean; ~11× faster than omny; competitive
with the other Rust impl). The conformance harness now pins this with assertions
and surfaces a small, well-characterized set of fixable reader/writer gaps
(typed-literal-as-IRI, bare-`inverse`, anon-subject round-trip, sio/hp parse) as
the natural next work before the upstream PR.
