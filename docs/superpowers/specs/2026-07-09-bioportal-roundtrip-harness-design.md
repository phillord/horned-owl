# horned-roundtrip — BioPortal IO round-trip conformance harness

- **Date:** 2026-07-09
- **Status:** Approved design (pre-implementation)
- **Depends on:** [phillord/horned-owl](https://github.com/phillord/horned-owl) (git `devel`)

## 1. Purpose

A standalone Rust tool that exercises horned-owl's OWL IO across a large, real-world
corpus (the BioPortal ontologies) and reports where round-tripping loses or changes
information. It is a **general IO conformance / regression harness** for all four
horned-owl serialisation formats — RDF/XML, OWL/XML, OWL Functional (OFN), OWL
Manchester (OMN) — not specific to any one of them.

For every ontology it reads the source, writes it out in each target format, reads that
back, and compares the two in-memory models. Differences are categorised into
*known-benign* buckets versus **UNKNOWN**; the UNKNOWN set is the signal that points at
real defects in horned-owl's readers/writers.

### Goals

- Catch IO regressions in horned-owl (read/write correctness) against real ontologies,
  before they reach a release — hence it tracks the upstream `devel` branch.
- Turn a ~1200-file corpus into an actionable, ranked list of real (non-benign) defects.
- Be push-button reproducible: fetch a corpus, run the matrix, produce a report, with a
  single toolchain (`cargo`).

### Non-goals

- Not a semantic reasoner or profile checker (no consistency/entailment checking).
- Does not bundle or redistribute the BioPortal ontologies (licensing + size); it ships
  the code to fetch them.
- Does not add gzip support to horned-owl; decompression lives here.
- No OBO coverage (horned-owl has no OBO reader yet).

## 2. Background and decisions

horned-owl is at v2.0.0 with all four IO formats merged (the OMN reader+writer landed
via #176). phillord asked (PR #176 discussion) that this harness live in its **own
repo** depending on horned-owl rather than bulking up the library; that content-based
format detection is desirable but belongs **in horned-owl separately**; and that gzip
should **not** become a horned-owl dependency (clients wrap their own decompressor).

A prior throwaway version of this harness (`omn-eval`) established the empirical
baseline: over ~880 BioPortal RDF/XML files, OMN write ~99.7% ok, read-back ~98.9%,
exact round-trip ~51% — the remaining ~49% dominated by **benign inferred declarations**
(Manchester and other frame syntaxes imply a `Declare*` axiom on read). That throwaway
lived in a GC'd scratch dir; this repo is the durable replacement.

Design decisions (settled during brainstorming):

| Topic | Decision |
|---|---|
| Core scope | General IO conformance **matrix** (read source → write each target → read back → compare) |
| Comparison | **Raw model diff, then categorize** each difference (benign buckets vs UNKNOWN) |
| Structure | **All-Rust**: round-trip harness + a reqwest downloader in one crate |
| horned-owl dependency | **git dep on `phillord/horned-owl` `devel`** (catches unreleased regressions) |
| Reporting | Per-case **CSV + JSON** + a human **Markdown** summary |
| Architecture | Staged pipeline **`fetch` → `run` → `report`** (Approach B) |

## 3. Architecture

Single Rust crate, `lib` + one `bin`, staged into three CLI subcommands so the expensive
round-trip pass is decoupled from (cheap, iterated) reporting.

```
horned-roundtrip/
├── Cargo.toml          # git dep on horned-owl; reqwest, flate2, rayon, serde/serde_json,
│                       #   csv, clap, anyhow, sha2
├── README.md           # quickstart, API-key setup, the 3-command flow
├── src/
│   ├── main.rs         # clap CLI dispatch (fetch | run | report)
│   ├── lib.rs
│   ├── model.rs        # shared types: Format, Outcome, Category, DiffItem, CaseResult
│   ├── fetch.rs        # BioPortal API client + concurrent downloader (stores .gz)
│   ├── corpus.rs       # enumerate a corpus dir → entries; gzip-aware reader helper
│   ├── detect.rs       # content-based format sniffer → Format
│   ├── roundtrip.rs    # read → write → reread engine over the format matrix
│   ├── diff.rs         # model diff: components only-in-source / only-in-roundtrip
│   ├── categorize.rs   # classify each diff into Category (benign buckets vs UNKNOWN)
│   └── report.rs       # results.jsonl → cases.csv + summary.json + report.md
├── tests/              # unit tests on detect/diff/categorize/report with inline fixtures
└── docs/superpowers/specs/2026-07-09-bioportal-roundtrip-harness-design.md
```

### CLI

- `horned-roundtrip fetch --out <dir> [--api-key <K>] [--limit N]`
  Pull BioPortal submissions, store gzipped, write `manifest.json`. API key from
  `--api-key` or `BIOPORTAL_API_KEY`.
- `horned-roundtrip run --corpus <dir> --out results.jsonl [--formats rdf,owx,ofn,omn] [--jobs N]`
  The expensive matrix pass; emits one JSONL `CaseResult` per (ontology × target format).
- `horned-roundtrip report --in results.jsonl --out-dir report/`
  Aggregates JSONL → `cases.csv`, `summary.json`, `report.md`.

### Cross-cutting

- **Parallelism:** rayon over corpus files (each ontology independent, CPU-bound).
- **gzip:** handled in `corpus.rs` (flate2), so horned-owl stays dependency-light.
- **Model type:** `SetOntology<RcStr>` throughout comparison — set semantics make the
  diff order-independent; `RcStr` for read/clone performance.

## 4. Data flow

```
fetch:  BioPortal API ──► download (gz) ──► <corpus dir>/*.gz + manifest.json
run:    corpus entry ─► detect(S) ─► read_S ─► model_src
                                    └─(read err/panic)─► CaseResult{ReadFail/Panic}
        for T in formats:  write_T(model_src) ─► reread_T ─► model_rt
                           diff(model_src, model_rt) ─► categorize ─► CaseResult ──► results.jsonl
report: results.jsonl ─► aggregate ─► cases.csv + summary.json + report.md
```

Read failure on the source is terminal for that ontology (nothing to compare against);
it is recorded once and its round-trips are skipped. Write/reread failures are per
target format.

## 5. Data model (`model.rs`, serde-serialisable)

```rust
enum Format { RdfXml, OwlXml, Ofn, Omn, Unknown }

enum Outcome { Ok, ReadFail, WriteFail, RereadFail, Panic }

enum Category {
    InferredDeclaration,     // a Declare* present only in the round-trip
    NaryReshape,             // n-ary axiom split/reordered, same logical content
    AnnotationNormalization, // annotation regrouping preserving (subject, prop, value)
    Unknown,                 // real signal
}

enum Side { Source, RoundTrip }

struct DiffItem {
    side: Side,              // only-in-source (lost) vs only-in-roundtrip (gained)
    component_kind: String,  // e.g. "DeclareClass", "SubClassOf"
    category: Category,
    debug: String,           // Debug of the AnnotatedComponent, for investigation
}

struct CaseResult {
    ontology: String,        // corpus file stem / acronym
    source_format: Format,
    target_format: Format,
    outcome: Outcome,
    error: Option<String>,   // message on *Fail / Panic
    exact: bool,             // true iff no diffs
    diffs: Vec<DiffItem>,    // categorized; empty when exact
    category_counts: BTreeMap<Category, usize>,
    read_us: Option<u64>,
    write_us: Option<u64>,
    reread_us: Option<u64>,
}
```

`manifest.json` (from `fetch`): array of `{ acronym, submission_id, reported_language,
stored_path, bytes, sha256 }`.

`cases.csv` columns: `ontology, source_format, target_format, outcome, exact, n_lost,
n_gained, benign_inferred_decl, benign_nary, benign_annotation, n_unknown, read_us,
write_us, reread_us`.

`summary.json`: per-format-pair exact/pass rates, outcome tallies, category totals,
top-N ontologies by UNKNOWN count.

## 6. Components

### `detect.rs` — content-based format detection

Peek the leading non-comment content and match tell-tale openings, returning `Format`:

- **RDF/XML** — `<?xml …` with an `rdf:RDF` root / `xmlns:rdf` declaration.
- **OWL/XML** — XML whose root element is `<Ontology>` (owl2-xml namespace).
- **Functional (OFN)** — first significant token `Prefix(` or `Ontology(`.
- **Manchester (OMN)** — first significant token `Prefix:` or `Ontology:`.
- Otherwise `Unknown` (includes OBO `format-version:`).

The one subtle case is distinguishing RDF/XML from OWL/XML: both begin with an XML
declaration, so the decision is made on the **root element name** after the `<?xml?>`
prologue and any comments. Extension is ignored entirely (BioPortal extensions are
frequently wrong). This module is kept self-contained so it can later be proposed to
horned-owl as the content-sniffer phillord wants.

### `roundtrip.rs` — the engine

For each corpus entry: sniff `S`; read with horned-owl's `S` reader (inside
`catch_unwind`) → `model_src`. On error/panic emit a single `CaseResult` and stop. For
each target `T` in the requested set (default all four, **including S→S** as a
writer/reader self-consistency check): write `model_src` as `T`, read it back, diff,
categorize, emit a `CaseResult`. Per-step timings captured. Every horned-owl call is
wrapped in `catch_unwind` — a real-world corpus will hit edge cases and one panic must
not abort the sweep.

### `diff.rs` — model diff

`model_src` and `model_rt` are `SetOntology<RcStr>` (sets of `AnnotatedComponent`).
Compute `only_in_source` (lost) and `only_in_roundtrip` (gained) by set difference;
`exact` iff both empty. Each element becomes a `DiffItem` (side, component_kind, debug),
awaiting categorisation.

### `categorize.rs` — classify diffs

Operates on the diff **pair** jointly (benign categories are about correspondence, not
individual items):

- **`InferredDeclaration`** — an item only-in-round-trip that is a `Declare*` whose
  entity is referenced by a surviving axiom (frame syntaxes imply the declaration on
  read). Dominant benign case.
- **`NaryReshape`** — an n-ary axiom (`EquivalentClasses`, `DisjointClasses`,
  `SameIndividual`, `DifferentIndividuals`, …) that reappears split into binaries or
  reordered, with identical logical content (same member set / same pairwise closure).
- **`AnnotationNormalization`** — annotation regrouping (e.g. annotatedList split) that
  preserves the set of `(subject, property, value)` triples.
- **`Unknown`** — everything else. This is the reported signal.

Isolated so that a newly-understood benign pattern is a localised change (and, per the
comparison decision, nothing is hidden — new benign patterns first appear as UNKNOWN).

### `report.rs` — aggregation

Pure function of `results.jsonl` → the three artifacts in §5. No horned-owl or network
dependency; fast to iterate. The Markdown report leads with per-format-pair exact rates
and an outcome breakdown, then a **ranked list of UNKNOWN cases** (ontology, format
pair, sample debug lines) and the slowest ontologies.

### `fetch.rs` + `corpus.rs`

`fetch.rs`: BioPortal client (`data.bioontology.org`, `apikey` query param). List
`/ontologies`; for each resolve the latest submission's download; GET it; store
**gzipped** as `<acronym>.gz`; record a `manifest.json` entry with a sha256. Concurrent
and failure-tolerant (a failed download is logged and recorded, not fatal). Mirrors the
proven logic of the existing Python downloader
(`~/code/bioportal-ontology-analysis/scripts/download_all_ontologies.py`).

`corpus.rs`: enumerate a corpus directory into entries; provide a reader helper that
sniffs the gzip magic bytes (`0x1f 0x8b`) and wraps `flate2::GzDecoder` when needed, so
both `.gz` and plain files work. `run` accepts any directory — `fetch` is optional
(bring-your-own corpus is supported).

## 7. Error handling

Three per-case failure layers, all **non-fatal** to the run and captured as *data* in
the `CaseResult`: reader `Err` → `ReadFail`, writer `Err` → `WriteFail`,
`catch_unwind` → `Panic` (with the panic message where recoverable). Fetch
network/IO errors are logged per ontology and recorded in the manifest; the run
continues. `anyhow` is used only at the CLI boundary; per-case outcomes are structured
values, not errors.

## 8. Testing

No network and no bundled corpus in tests; engine tests use tiny inline OWL strings.

- **`detect`** — one snippet per format plus the ambiguous RDF/XML-vs-OWL/XML case →
  correct `Format`.
- **`diff`** — hand-built `SetOntology` pairs → expected lost/gained sets.
- **`categorize`** — the key surface: crafted diff pairs → expected
  `InferredDeclaration` / `NaryReshape` / `AnnotationNormalization` / `Unknown`.
- **`report`** — a small in-memory `results.jsonl` → expected CSV rows / summary shape.

**CI** (GitHub Actions): `cargo test` + `cargo clippy` + `cargo fmt --check`,
corpus-free and network-free. A scheduled real-corpus run would need a BioPortal API-key
secret; documented as a manual step for now (YAGNI).

## 9. Dependencies (Cargo)

`horned-owl` (git, branch `devel`), `reqwest` (blocking + rustls), `flate2`, `rayon`,
`serde` + `serde_json`, `csv`, `clap` (derive), `anyhow`, `sha2`. No JVM, no gzip in
horned-owl.

## 10. Out of scope / future

- **Upstreaming the sniffer:** `detect.rs` is written to be a candidate for a horned-owl
  content-format detection PR (what phillord asked for) — but that is a separate change,
  not part of this repo's deliverable.
- **OBO:** add once horned-owl has an OBO reader.
- **Semantic/normalised comparison mode:** the categorize approach can later grow a
  normalisation pass; not needed initially.
- **Scheduled CI corpus run:** needs an API-key secret; manual for now.
- **Reasoning / profile validation:** never in scope for this tool.
