# horned-roundtrip

A round-trip **IO conformance harness** for [horned-owl](https://github.com/phillord/horned-owl).

It reads real-world ontologies, writes each one back out in every serialization
format horned-owl supports, reads the result back in, and compares the two in-memory
models. Differences are categorized so that **genuine reader/writer defects stand out
from benign, expected normalization**. Point it at a large corpus (e.g. BioPortal) and
it turns thousands of ontologies into a ranked list of real problems.

Formats exercised: **RDF/XML, OWL/XML, OWL Functional (OFN), OWL Manchester (OMN)** as
read *and* write targets; **Turtle / N-Triples** as read-only sources (horned-owl has no
Turtle writer).

## How it works

```
fetch   BioPortal API ──► gzipped corpus + manifest.json      (optional; needs an API key)
run     each file ─► detect format ─► read ─► for each target format: write ─► read back
                     ─► canonicalize blank nodes ─► diff models ─► categorize ─► JSONL
report  results.jsonl ─► cases.csv + summary.json + report.md
```

- **Format is detected by content, not extension** — real corpora mislabel files
  constantly (Turtle behind a `.owl` extension, etc.).
- Every horned-owl read/write is wrapped in `catch_unwind`, so one malformed ontology
  can never abort a sweep of thousands.
- `run` streams each file's records to disk as it goes, so memory stays flat and partial
  progress survives an interruption. Oversized files are `--max-bytes`-capped and marked
  `Skipped` without being read into memory.
- The RDF reader's `IncompleteParse` (triples it couldn't turn into axioms) is captured
  as a first-class **read-side** loss signal, not silently ignored.

### Diff categories

Each difference between the source model and the round-tripped model is one of:

| Category | Meaning | Signal? |
|---|---|---|
| `inferred_declaration` | a `Declare*` the reader adds because a frame implies it | benign |
| `nary_reshape` | an n-ary axiom re-split with identical logical content | benign |
| `blank_node_relabel` | residual anonymous-individual id difference after canonicalization | benign-ish |
| `annotation_loss` | an axiom's annotation set changed/dropped on round-trip | **real** |
| `unknown` | anything else | **real** |

`unknown` and `annotation_loss` are the defect signal; the benign buckets are reported
but demoted.

## Build

Requires a Rust toolchain. horned-owl is a **git dependency pinned to a specific commit**
(recorded in each run's report header for reproducibility).

```sh
cargo build --release
cargo test
```

## Usage

Run over a directory of ontologies you already have (no API key needed):

```sh
# read → write(×4) → read-back → diff → categorize, streaming to results.jsonl
horned-roundtrip run --corpus /path/to/ontologies --out results.jsonl \
    --jobs 3 --max-bytes 20000000

# aggregate into cases.csv, summary.json, report.md
horned-roundtrip report --in results.jsonl --out-dir report/
```

Fetch a fresh BioPortal corpus first (requires a [BioPortal API key](https://bioportal.bioontology.org/account)):

```sh
export BIOPORTAL_API_KEY=...        # or pass --api-key
horned-roundtrip fetch --out ./corpus            # stores <acronym>.gz + manifest.json
horned-roundtrip run   --corpus ./corpus --out results.jsonl --max-bytes 20000000 --jobs 3
horned-roundtrip report --in results.jsonl --out-dir report/
```

- `run --formats rdf,owx,ofn,omn` restricts which target formats to write (default: all four).
- The corpus is **not** included in this repo (size + mixed licensing) — `fetch` downloads
  your own copy, or bring your own directory.

## Output

- `cases.csv` — one row per (ontology × target format): outcome, exact-match flag, lost/gained
  counts, per-category counts, timings.
- `summary.json` — per-format-pair exact/ok rates, outcome tallies, per-format
  read-incompleteness rates, category totals, top ontologies by `unknown` count.
- `report.md` — a human summary: headline rates, the run's pinned horned-owl commit, and a
  ranked list of the `unknown` / `annotation_loss` cases to investigate.

## Limitations

- **No OBO** — horned-owl has no OBO reader, so `.obo` files are reported as `unknown`.
- **Turtle is read-only** — there is no Turtle writer, so Turtle is a source format only.
- **Blank-node canonicalization is a deterministic first cut** (sort-by-debug), not full
  RDF graph canonicalization; residual blank-node ordering differences are routed to
  `blank_node_relabel` rather than polluting the `unknown` signal, so they under-report
  genuine blank-node defects rather than inventing false ones.

## License

Not yet chosen — add a `LICENSE` file before relying on this in other projects.
