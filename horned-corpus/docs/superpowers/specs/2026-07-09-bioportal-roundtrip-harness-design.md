# horned-roundtrip — BioPortal IO round-trip conformance harness

- **Date:** 2026-07-09 (revised after advisor review)
- **Status:** Approved design (pre-implementation)
- **Depends on:** [phillord/horned-owl](https://github.com/phillord/horned-owl) (git, `devel`, **pinned to a specific commit**)

## 1. Purpose

A standalone Rust tool that exercises horned-owl's OWL IO across a large, real-world
corpus (the BioPortal ontologies) and reports where round-tripping loses or changes
information. It is a **general IO conformance / regression harness** for all four
horned-owl serialisation formats — RDF/XML, OWL/XML, OWL Functional (OFN), OWL
Manchester (OMN) — not specific to any one of them.

For every ontology it reads the source, writes it out in each target format, reads that
back, and compares the two in-memory models. Differences are categorised into
*known-benign* buckets versus **UNKNOWN**; the UNKNOWN set is the signal that points at
real defects in horned-owl's readers/writers. Additionally, the *source read itself* is
graded for completeness (see §5, `IncompleteParse`), so read-side loss is a first-class
result rather than being silently baked into the baseline.

### Goals

- Catch IO regressions in horned-owl (read/write correctness) against real ontologies,
  before they reach a release — hence it tracks upstream `devel` (pinned per run).
- Turn a ~1200-file corpus into an actionable, ranked list of real (non-benign) defects.
- Be reproducible: a run records the exact horned-owl commit and corpus manifest.

### Non-goals

- Not a semantic reasoner or profile checker (no consistency/entailment checking).
- Does not bundle or redistribute the BioPortal ontologies (licensing + size); it ships
  the code to fetch them.
- Does not add gzip support to horned-owl; decompression lives here.
- Does not resolve `owl:imports` (single-document round-trips only — see §6).
- No OBO coverage (horned-owl has no OBO reader yet).

## 2. Background and decisions

horned-owl has all four IO formats (the OMN reader+writer landed via #176). phillord
asked (PR #176 discussion) that this harness live in its **own repo** depending on
horned-owl rather than bulking up the library; that content-based format detection is
desirable but belongs **in horned-owl separately**; and that gzip should **not** become
a horned-owl dependency (clients wrap their own decompressor).

**Provenance note:** the local horned-owl checkout used while writing this spec is
`1.4.0` on branch `fix/omn-writer-perf`; the published crate and the upstream `devel`
branch have moved past that. The dependency therefore targets `phillord/horned-owl`
`devel` **pinned to an explicit commit** recorded in each run's report (a moving branch
would defeat reproducibility). Verify the reader/writer entry points below against the
pinned commit before implementing.

A prior throwaway version (`omn-eval`) established the empirical baseline: over ~880
BioPortal RDF/XML files, OMN write ~99.7% ok, read-back ~98.9%, exact round-trip ~51% —
the remainder dominated by **benign inferred declarations** (frame syntaxes imply a
`Declare*` axiom on read). That throwaway lived in a GC'd scratch dir; this repo is the
durable replacement.

Design decisions (settled during brainstorming):

| Topic | Decision |
|---|---|
| Core scope | General IO conformance **matrix** (read source → write each target → read back → compare) |
| Comparison | **Raw model diff, then categorize** each difference (benign buckets vs UNKNOWN) |
| Structure | **All-Rust**: round-trip harness + a reqwest downloader in one crate |
| horned-owl dependency | **git dep on `phillord/horned-owl` `devel`, pinned to a commit** |
| Reporting | Per-case **CSV + JSON** + a human **Markdown** summary |
| Architecture | Staged pipeline **`fetch` → `run` → `report`** |

## 3. Architecture

Single Rust crate, `lib` + one `bin`, staged into three CLI subcommands so the expensive
round-trip pass is decoupled from (cheap, iterated) reporting.

```
horned-roundtrip/
├── Cargo.toml          # git dep on horned-owl (rev = <pinned>); reqwest, flate2, rayon,
│                       #   serde/serde_json, csv, clap, anyhow, sha2
├── README.md           # quickstart, API-key setup, the 3-command flow
├── src/
│   ├── main.rs         # clap CLI dispatch (fetch | run | report)
│   ├── lib.rs
│   ├── model.rs        # shared types: Format, Outcome, Category, DiffItem, CaseResult
│   ├── fetch.rs        # BioPortal API client + concurrent downloader (stores .gz)
│   ├── corpus.rs       # enumerate a corpus dir → entries; gzip-aware reader helper
│   ├── detect.rs       # content-based format sniffer → Format
│   ├── ontology.rs     # read/write adapter over horned-owl's io::ParserOutput + writers
│   ├── canon.rs        # blank-node canonicalization of a model before diffing
│   ├── roundtrip.rs    # read → write → reread engine over the format matrix
│   ├── diff.rs         # model diff: components only-in-source / only-in-roundtrip
│   ├── categorize.rs   # classify diffs (needs BOTH models) → Category
│   └── report.rs       # results.jsonl → cases.csv + summary.json + report.md
├── tests/              # unit tests on detect/canon/diff/categorize/report (inline fixtures)
└── docs/superpowers/specs/2026-07-09-bioportal-roundtrip-harness-design.md
```

### CLI

- `horned-roundtrip fetch --out <dir> [--api-key <K>] [--limit N]`
  Pull BioPortal submissions, store gzipped, write `manifest.json`. API key from
  `--api-key` or `BIOPORTAL_API_KEY`.
- `horned-roundtrip run --corpus <dir> --out results.jsonl [--formats rdf,owx,ofn,omn] [--jobs N]`
  The expensive matrix pass; emits one JSONL `CaseResult` per (ontology × target format),
  plus one source-read record per ontology. Records the horned-owl commit in a run header.
- `horned-roundtrip report --in results.jsonl --out-dir report/`
  Aggregates JSONL → `cases.csv`, `summary.json`, `report.md`.

### Cross-cutting

- **Parallelism:** rayon over corpus files (each ontology independent, CPU-bound), with a
  bounded pool and an optional per-file size cap (very large files → OOM/stack risk; see §7).
- **gzip:** handled in `corpus.rs` (flate2), so horned-owl stays dependency-light.
- **Model type:** `SetOntology<RcStr>` for comparison (set semantics = order-independent).
  The write path converts `SetOntology → ComponentMappedOntology` (writers require the
  latter; conversion exists upstream).

## 4. Data flow

```
fetch:  BioPortal API ──► download (gz) ──► <corpus dir>/*.gz + manifest.json

run (per corpus entry):
  detect(S)
  read source S  ─►  io::ParserOutput::decompose()  ─►  (model_src: SetOntology, prefixes, incomplete)
        │                                                 │
        │(read err/panic)                                 ├─ emit source-read record:
        └─► CaseResult{outcome=ReadFail|Panic}            │   {is_complete, incomplete summary}
                                                          ▼
  for T in formats (incl. S→S):
      model_cmo = ComponentMappedOntology::from(model_src)
      write_T(model_cmo, prefixes?) ─► reread_T ─► model_rt   (WriteFail/RereadFail/Panic captured)
      canon(model_src), canon(model_rt)            # blank-node canonicalization
      diff = (only_in_src, only_in_rt)             # set difference over AnnotatedComponent
      categorize(diff, &model_src, &model_rt)       # needs FULL models, not just the diff
      ─► CaseResult ──► results.jsonl

report: results.jsonl ─► aggregate ─► cases.csv + summary.json + report.md
```

Source read failure is terminal for that ontology (nothing to compare); recorded once,
round-trips skipped. A **successful-but-incomplete** source read still proceeds to
round-trips, but the incompleteness is recorded so it is never mistaken for fidelity.

## 5. Data model (`model.rs`, serde-serialisable)

```rust
enum Format { RdfXml, OwlXml, Ofn, Omn, Unknown }

enum Outcome { Ok, ReadFail, WriteFail, RereadFail, Panic }

enum Category {
    InferredDeclaration,     // a Declare* only in RT, kind-matching a SOURCE axiom's usage
    NaryReshape,             // n-ary axiom split/reordered; same member set / pairwise closure
    AnnotationNormalization, // paired lost+gained differing only in annotation set/grouping
    BlankNodeRelabel,        // residual anon-individual id difference after canonicalization
    Unknown,                 // real signal
}

enum Side { Source, RoundTrip }

struct DiffItem { side: Side, component_kind: String, category: Category, debug: String }

// Source-read completeness (from horned-owl's IncompleteParse, RDF path especially).
struct SourceReadReport {
    ontology: String,
    source_format: Format,
    outcome: Outcome,          // Ok | ReadFail | Panic
    is_complete: bool,         // false ⇒ reader dropped triples/expressions
    incomplete: Option<IncompleteSummary>, // counts by kind: simple/bnode/class_expression/ann
    error: Option<String>,
    read_us: Option<u64>,
}

struct CaseResult {
    ontology: String,
    source_format: Format,
    target_format: Format,
    outcome: Outcome,
    error: Option<String>,
    exact: bool,               // no diffs after canonicalization
    diffs: Vec<DiffItem>,      // categorized; empty when exact
    category_counts: BTreeMap<Category, usize>,
    write_us: Option<u64>,
    reread_us: Option<u64>,
}
```

`results.jsonl` interleaves a `run header` (horned-owl commit, timestamp, corpus path),
one `SourceReadReport` per ontology, and `CaseResult` records (tagged records).

`manifest.json` (from `fetch`): array of `{ acronym, submission_id, reported_language,
stored_path, bytes, sha256 }`.

`cases.csv` columns: `ontology, source_format, target_format, outcome, source_complete,
exact, n_lost, n_gained, benign_inferred_decl, benign_nary, benign_annotation,
benign_blanknode, n_unknown, write_us, reread_us`.

`summary.json`: per-format-pair exact/pass rates, outcome tallies, **source-incomplete
rate per source format**, category totals, top-N ontologies by UNKNOWN count.

## 6. Components

### `detect.rs` — content-based format detection

Peek the leading content (skipping a BOM and any XML comments / DOCTYPE internal subset)
and return a `Format`:

- **RDF/XML** — XML whose **resolved root element** is `rdf:RDF` (RDF namespace).
- **OWL/XML** — XML whose resolved root element is `Ontology` (owl2-xml namespace).
- **Functional (OFN)** — first significant token `Prefix(` or `Ontology(`.
- **Manchester (OMN)** — first significant token `Prefix:` or `Ontology:`.
- Otherwise `Unknown` (includes OBO `format-version:`).

The RDF/XML-vs-OWL/XML decision is made on **namespace URI + local name of the root
element** (both begin with an `<?xml?>` prologue), not on the raw prefix string.
Extension is ignored (BioPortal extensions are frequently wrong). Kept self-contained so
it can later be proposed to horned-owl as the content-sniffer phillord wants (upstream
currently only has extension-based `path_type`).

### `ontology.rs` — read/write adapter

Wraps horned-owl's own IO so the harness doesn't hand-roll per-format glue and so the RDF
completeness signal is preserved:

- **Read:** use the format-specific single-document reader and route through
  `io::ParserOutput::decompose() -> (SetOntology, Option<PrefixMapping>, Option<IncompleteParse>)`.
  Crucially use `io::rdf::reader::read` (single document) — **not** `parse_path` /
  `closure_reader`, which follow `owl:imports` over the network. Record `IncompleteParse`
  into `SourceReadReport`.
- **Write:** convert `SetOntology → ComponentMappedOntology` and call the target format's
  writer (all writers take `&ComponentMappedOntology`). Pass the source `PrefixMapping`
  where a writer accepts one (OFN/OMN take `Option<&PrefixMapping>`); `None` yields
  full-IRI output, which is fine for model fidelity. Record the choice so it's explicit.
- Record which `ParserConfiguration` is used (esp. RDF `lax`), since it changes results.

### `canon.rs` — blank-node canonicalization

Anonymous-individual ids are part of `AnnotatedComponent`'s `Eq`/`Hash`, and writers
regenerate them, so an axiom with a blank node round-trips to a structurally-identical
but non-equal component (would appear as both lost and gained). Before diffing, rewrite
both models' anonymous-individual ids to a canonical, position-derived scheme so
structurally-equal components compare equal. Any residual difference after
canonicalization is a real blank-node handling defect → `BlankNodeRelabel` (a *reported*
benign-ish category, not silently dropped).

### `roundtrip.rs` — the engine

Per corpus entry: sniff `S`; read via `ontology.rs` (inside `catch_unwind`) →
`SourceReadReport` + `model_src`. On read error/panic emit the report and stop. For each
target `T` (default all four, **including S→S**): write, reread, canonicalize both,
diff, categorize, emit a `CaseResult`. Per-step timings captured. Every horned-owl call
is wrapped in `catch_unwind` (with `AssertUnwindSafe` — the model is full of `Rc`) — a
real-world corpus hits edge cases and one panic must not abort the sweep.

### `diff.rs` — model diff

Both canonicalized models are `SetOntology<RcStr>`. Compute `only_in_source` (lost) and
`only_in_roundtrip` (gained) by set difference; `exact` iff both empty. Each element
becomes a `DiffItem`.

### `categorize.rs` — classify diffs (takes BOTH full models)

Categorisation requires the full `model_src` and `model_rt`, not just the diff sets:

- **`AnnotationNormalization`** — pair a lost item with a gained item that share a
  *component-minus-annotations* key; if the union of their annotations matches (modulo
  grouping) it's benign. This pairing is why the full diff (both sides) is needed.
- **`NaryReshape`** — a gained binary axiom whose members are covered by a surviving
  **source** n-ary axiom (same member set / same pairwise closure); requires consulting
  `model_src`.
- **`InferredDeclaration`** — a `Declare*` only in RT **whose kind matches** how the
  entity is used by an axiom **present in `model_src`** (frames imply declarations on
  read). Kind-matching rules out punning defects; source-anchoring rules out
  self-consistent spurious cascades; an annotated-declaration whose annotation was
  dropped does **not** qualify (that's a real loss, kept UNKNOWN).
- **`BlankNodeRelabel`** — residual anon-id difference after `canon`.
- **`Unknown`** — everything else. The reported signal.

Isolated so a newly-understood benign pattern is a localised change; nothing is hidden —
new patterns first surface as UNKNOWN.

### `report.rs` — aggregation

Pure function of `results.jsonl` → the three artifacts in §5; no horned-owl/network
dependency. Markdown leads with per-format-pair exact rates, an outcome breakdown, and
the **source-incompleteness rate per format**, then a ranked list of UNKNOWN cases
(ontology, pair, sample debug) and slowest ontologies.

### `fetch.rs` + `corpus.rs`

`fetch.rs`: BioPortal client (`data.bioontology.org`, `apikey` query param on **every**
request incl. the `links.download` URL; follow redirects). `/ontologies` is a single
non-paginated array. For each, resolve the latest submission's download, GET it, store
**gzipped** as `<acronym>.gz`, and record a `manifest.json` entry with sha256. Concurrent
with a **bounded** pool and **429/backoff retry** (BioPortal throttles aggressively).
Failures are logged and recorded, not fatal. Mirrors the logic proven in the Python
client at `~/code/bioportal-ontology-analysis/bioportal_analysis/fetch.py`.

`corpus.rs`: enumerate a corpus directory into entries; a reader helper sniffs the gzip
magic (`0x1f 0x8b`) and wraps `flate2::GzDecoder` when needed, so `.gz` and plain files
both work. `run` accepts any directory — `fetch` is optional (bring-your-own corpus).

**Build order:** validate `run → diff → categorize → report` against a local corpus
first; implement `fetch` last (least novel, most external-flakiness).

## 7. Error handling

Per-case failures are **non-fatal** and captured as data: reader `Err` → `ReadFail`,
writer `Err` → `WriteFail`, `catch_unwind` → `Panic`. `catch_unwind` (with
`AssertUnwindSafe`) catches panics only — **not** stack overflow or OOM. The OFN/OMN
readers read the whole document then run pest recursive-descent, so deeply-nested
expressions can overflow the stack and a large corpus under rayon can OOM. Mitigations:
bound the rayon pool, an optional per-file size cap that marks oversized inputs `Skipped`
rather than risking an abort, and documenting that aborts (not panics) will kill the
worker. Fetch network/IO errors are logged per ontology and recorded; the run continues.
`anyhow` only at the CLI boundary; per-case outcomes are structured values, not errors.

## 8. Testing

No network and no bundled corpus in tests; engine tests use tiny inline OWL strings.

- **`detect`** — one snippet per format, plus ambiguous RDF/XML-vs-OWL/XML, BOM, and a
  leading-comment/DOCTYPE case → correct `Format`.
- **`canon`** — two models differing only by anon-id labels → equal after canonicalization.
- **`diff`** — hand-built `SetOntology` pairs → expected lost/gained.
- **`categorize`** — the key surface; **adversarial** fixtures:
  - inferred-declaration (benign) *vs* a punning declaration (kind mismatch → UNKNOWN),
  - an annotated-declaration whose annotation was dropped → UNKNOWN (not benign),
  - annotation-set change → paired lost+gained → `AnnotationNormalization`,
  - partial-overlap n-ary (surviving source n-ary + gained binary) → `NaryReshape`,
  - blank-node relabel → equal after canon / residual → `BlankNodeRelabel`.
- **source completeness** — an RDF input the reader can't fully parse is reported
  (`is_complete=false`), not silently trusted.
- **`report`** — a small in-memory `results.jsonl` → expected CSV rows / summary shape.

**CI** (GitHub Actions): `cargo test` + `cargo clippy` + `cargo fmt --check`,
corpus-free and network-free, against the pinned horned-owl commit. A scheduled
real-corpus run needs an API-key secret → documented as manual (YAGNI).

## 9. Dependencies (Cargo)

`horned-owl` (git, `rev = <pinned commit>`), `reqwest` (blocking + rustls), `flate2`,
`rayon`, `serde` + `serde_json`, `csv`, `clap` (derive), `anyhow`, `sha2`. No JVM, no
gzip in horned-owl. Harness's own release profile must **not** set `panic = "abort"`.

## 10. Out of scope / future

- **Upstreaming the sniffer:** `detect.rs` is written to be a candidate for a horned-owl
  content-format-detection PR (what phillord asked for) — a separate change, not this
  repo's deliverable.
- **OBO:** add once horned-owl has an OBO reader.
- **Normalised-comparison mode:** the categorize approach can later grow a normalisation
  pass; not needed initially.
- **Scheduled CI corpus run:** needs an API-key secret; manual for now.
- **`owl:imports` closure round-trips:** deliberately excluded; single-document only.
- **Reasoning / profile validation:** never in scope.
```
