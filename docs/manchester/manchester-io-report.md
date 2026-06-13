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
  round-trip; **8 documented residuals** across 7 kinds (down from 10/9 after
  the 2026-06-13 fixes below). Each residual row asserts its *specific*
  documented behavior (compiler-exhaustive `match` — no rubber-stamp). Residual
  kinds: `SwrlRule`, `BareNameNeedsPrefix`, `ComplexLhsGci`,
  `NestedAnnotationDropped`, `DataRestrictionAsObject`,
  `HasKeyObjectDataConflation`, `PropertyObjectDataConflation` (×2:
  Equivalent/DisjointProperties over data properties).
- **Corpus parse + round-trip (A2)** (source → ROBOT/OWL-API → `.omn` → our
  reader → our writer → re-parse): koala parses (83 components) but **does not
  round-trip** (writer emits output our own reader rejects at rendered ~11:17 —
  separate writer bug); obi-core parses (54 232 components) but round-trip has a
  component mismatch; **sio fails to parse** (data-property restriction
  `dp some xsd:double[…]` hits the object `some` arm — see #4) and **hp fails to
  parse** (`Class: <complex expr>` — a complex-LHS general class axiom ROBOT
  emits as a non-strict-§2.5 `Class:` frame subject — the `ComplexLhsGci`
  residual); doid excluded (ROBOT's Manchester serializer >2 min).
- **Semantic axiom-set equality vs OWL-API (A3)** (canonicalized, declarations +
  non-logical meta dropped — so it cannot hide a logical-axiom gap): **after the
  boolean/value fix**, koala **45 matched / 0 missing / 1 extra** (the lone extra
  is n-ary↔pairwise representation noise — full logical-axiom parity); obi-core
  **48 409 matched / 16 missing / 20 extra** (was 20 missing; +119 matched). The
  closed diffs were `value true`/`false` boolean operands; the remaining obi-core
  residual is other constructs (under investigation / partly #4).
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

## Fixes landed (2026-06-13)

Three reader/writer gaps closed (commits `9a5269d`, `365cfa9`, `c4dd599`):
- **Bare `inverse R`** now parses (§2.5 allows `inverse` without parentheses).
- **Anonymous-individual subjects** now render as `Individual: _:<id>` frames
  (were emitted to `# General axioms` and lost on round-trip); all five
  assertion arms fixed.
- **`p value true`/`false`** now parses as `DataHasValue(p, "true"^^xsd:boolean)`
  (lenient OWL-API/Protégé boolean literal) instead of `ObjectHasValue` over a
  bare-name IRI — closing koala's A3 diff to full parity (45/0/1) and recovering
  119 obi-core axioms.

## Residual limitations (authoritative)

**Inherent — no §2.5 form exists:** SWRL `Rule:` (Manchester has no rule syntax);
complex-LHS general class axioms (no §2.5 frame form — the writer emits them to a
`# General axioms` block the reader skips, and ROBOT conversely emits them as a
non-strict `Class: <complex expr>` subject that the reader rejects — this is what
blocks **hp**); a bare default-prefix local name is not lexable.

**Reader gaps remaining (the data-vs-object disambiguation cluster):**
data-property restrictions (`dp some dt`) parse as object restrictions (the
grammar's data arms are dead PEG productions — `DataPropertyIRI` and
`ObjectPropertyIRI` are lexically identical), which is what blocks **sio** and
underlies the `DataRestrictionAsObject` + `PropertyObjectDataConflation`
residuals. Resolving this needs either a filler-shape heuristic
(facet-bracket / recognized `xsd:` datatype ⇒ data restriction) or two-pass
declaration-aware parsing. **This is the open architectural decision.**

**Writer follow-ups:** koala and obi-core fail round-trip on OWL-API-sourced
input (koala re-parse error at rendered ~11:17) — separate writer bugs to
characterize.

**Model limits:** nested annotation-on-annotation is parsed but the inner nesting
is dropped (the horned-owl model has no nested-annotation slot — the OFN reader
does the same); `HasKey:` cannot lexically distinguish object vs data keys, so
data-property keys round-trip as object keys.

## Bottom line

The writer is OWL-API-conformant on the corpus and the reader is a fast,
general §2.5 parser (93/101 constructs clean; ~11× faster than omny; competitive
with the other Rust impl). The conformance harness now pins this with assertions
and surfaces a small, well-characterized set of fixable reader/writer gaps
(typed-literal-as-IRI, bare-`inverse`, anon-subject round-trip, sio/hp parse) as
the natural next work before the upstream PR.
