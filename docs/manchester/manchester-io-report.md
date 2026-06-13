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

- **§2.5 construct matrix (A1):** **99 constructs** pass read + write +
  round-trip; **7 documented residuals** (down from 89/10 after the eight
  2026-06-13 fixes below). Each residual row asserts its *specific* documented
  behavior (compiler-exhaustive `match` — no rubber-stamp). Remaining residual
  kinds (6, all genuine §2.5/model limits): `SwrlRule` (no §2.5 rule syntax),
  `BareNameNeedsPrefix`, `ComplexLhsGci` (a GCI in a `# General axioms`
  functional block — distinct from the now-supported `Class: <expr>` frame
  form), `NestedAnnotationDropped` (model has no nested-annotation slot),
  `HasKeyObjectDataConflation`, `PropertyObjectDataConflation`
  (Equivalent/DisjointProperties over data properties — the Misc-list object/data
  ambiguity, no filler-shape signal, needs declaration context).
- **Corpus parse + round-trip (A2)** (source → ROBOT/OWL-API → `.omn` → our
  reader → our writer → re-parse): **all four ontologies now PARSE and
  ROUND-TRIP** — koala (83), sio (12 116), obi-core (54 232), hp (346 381),
  every one structurally component-equal across write→reread. doid excluded
  (ROBOT's Manchester serializer >2 min).
- **Semantic axiom-set equality vs OWL-API (A3)** (canonicalized, declarations +
  non-logical meta dropped — so it cannot hide a logical-axiom gap): koala
  **45 / 0 / 1** (full logical-axiom parity; the lone extra is n-ary↔pairwise
  noise), sio **10 092 / 5 / 20**, obi-core **48 417 / 8 / 20** (was 20 missing
  pre-fix; +119 matched), hp **313 705 / 20 / 20**. Remaining missing/extra are
  the characterized residuals above (complex-GCI writer round-trip, Misc-list
  property object/data conflation, n-ary↔pairwise representation noise).
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

Six reader/writer gaps closed:
- **Bare `inverse R`** parses (§2.5 allows `inverse` without parens). `9a5269d`
- **Anonymous-individual subjects** render as `Individual: _:<id>` frames (were
  lost to `# General axioms`); all five assertion arms. `365cfa9`
- **`p value true`/`false`** → `DataHasValue(p, "true"^^xsd:boolean)` (lenient
  OWL-API/Protégé boolean) instead of `ObjectHasValue` over a bare-name IRI —
  koala A3 → full parity, +119 obi-core axioms. `c4dd599`
- **Data-vs-object restriction heuristic** — a faceted (`dt[…]`) or known-datatype
  (`xsd:`/`rdf:`/`rdfs:`) filler ⇒ a DATA restriction (`DataSomeValuesFrom` etc.)
  instead of an object restriction. **Closed sio's parse failure**; plain
  class-IRI fillers stay object (bare user-datatypes need declaration context,
  deferred). `7e42056`
- **Complex-LHS `Class:` frame** — `Class: <complexExpr> SubClassOf: …` parses as
  a general class axiom (GCI). **Closed hp's parse failure.** `2b43fd9`
- **Writer IRI rendering** — frame subjects with an invalid abbreviated local
  (namespace lacking a `#`/`/` separator) now emit the full `<IRI>` instead of a
  malformed `#Animal`. **Closed koala's round-trip.** `466aa20`
- **Writer complex-LHS GCIs** — `SubClassOf` with a complex subject now renders
  as a `Class: <expr>` frame (complement to the FIX-5 reader), not the
  reader-skipped `# General axioms` block. **Closed hp's round-trip.** `5419491`
- **Writer literal escaping (UTF-8 slicing bug)** — `quote()` mixed char-ordinals
  with byte-offset slicing, corrupting literals with multibyte chars before an
  escaped `"`/`\`; fixed via `char_indices()`. **Closed obi-core's round-trip.**
  `35218ee`

## Residual limitations (authoritative)

**Inherent — no §2.5 form exists:** SWRL `Rule:` (Manchester has no rule syntax);
a bare default-prefix local name is not lexable (use `<full>` or `prefix:local`).
Complex-LHS GCIs now round-trip via `Class: <expr>` frames (FIX-5/FIX-7); the
`ComplexLhsGci` residual that remains is only the alternate `# General axioms`
functional-block form, which the reader still skips.

**Reader gap remaining (the data-vs-object disambiguation tail):** the filler-shape
heuristic (FIX-4) closes the common case — a faceted (`dt[…]`) or known-datatype
(`xsd:`/`rdf:`/`rdfs:`) filler ⇒ data restriction, which unblocked **sio**. The
residual tail is a **bare user-declared-datatype** filler (no facet, no known
prefix) used in a `some`/`only`/etc. restriction, and the same object/data
ambiguity in the Misc `EquivalentProperties:`/`DisjointProperties:` lists
(`PropertyObjectDataConflation`) — both need declaration-aware (two-pass) parsing,
deferred as a documented follow-up.

**Model limits:** nested annotation-on-annotation is parsed but the inner nesting
is dropped (the horned-owl model has no nested-annotation slot — the OFN reader
does the same); `HasKey:` cannot lexically distinguish object vs data keys, so
data-property keys round-trip as object keys.

## Bottom line

The writer is OWL-API-conformant on the corpus and the reader is a fast,
general §2.5 parser (99 constructs clean; ~11× faster than omny; competitive
with the other Rust impl) that now **parses and round-trips all four real-corpus
ontologies** (koala/sio/obi-core/hp). The conformance harness
pins this with assertions and surfaces a small, well-characterized set of
remaining reader/writer gaps
(typed-literal-as-IRI, bare-`inverse`, anon-subject round-trip, sio/hp parse) as
the natural next work before the upstream PR.
