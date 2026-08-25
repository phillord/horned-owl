# horned-corpus

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
fetch     BioPortal API ──► gzipped corpus + manifest.json    (optional; needs an API key)
roundtrip each file ─► detect format ─► read ─► for each target format: write ─► read back
                       ─► canonicalize blank nodes ─► diff models ─► categorize ─► JSONL
profile   each file ─► detect format ─► read ─► check against OWL 2 EL/QL/RL/DL ─► JSONL
reason    each file ─► robot reason (ELK / HermiT / JFact) ─► outcome + timing ─► JSONL
report    results.jsonl ─► cases.csv + summary.json + report.md
```

`roundtrip`, `profile` and `reason` are independent sweeps over the same corpus, each writing its
own JSONL. `report` aggregates any of them, rendering only the sections the records support — so a
reasoning run gets a reasoning report, not a round-trip report full of zeroes.

- **Format is detected by content, not extension** — real corpora mislabel files
  constantly (Turtle behind a `.owl` extension, etc.).
- Every horned-owl read/write is wrapped in `catch_unwind`, so one malformed ontology
  can never abort a sweep of thousands.
- `roundtrip` streams each file's records to disk as it goes, so memory stays flat and partial
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

Requires a Rust toolchain. This crate is a member of the horned-owl workspace and builds
against the horned-owl in the same checkout, so there is nothing to pin.

Each run records what it tested in its report header, as `3.0.0 (2d20450)`: the version is
`horned_owl::VERSION`, fixed at compile time from the crate that was actually linked, and the
commit is the working tree's HEAD when the run started. Between releases the version alone
can't tell two runs apart, which is why the commit is there too — but it is read at runtime,
so build, commit, then run and it will be a commit ahead of the binary. Override the whole
string with `roundtrip --horned-owl-rev <string>` when that isn't what you want recorded.

```sh
cargo build --release
cargo test
```

## Usage

Run over a directory of ontologies you already have (no API key needed):

```sh
# read → write(×4) → read-back → diff → categorize, streaming to results.jsonl
horned-corpus roundtrip --corpus /path/to/ontologies --out results.jsonl \
    --jobs 3 --max-bytes 20000000

# aggregate into cases.csv, summary.json, report.md
horned-corpus report --in results.jsonl --out-dir report/
```

Check the same corpus against the OWL 2 profiles instead (a separate sweep — no round-tripping):

```sh
horned-corpus profile --corpus /path/to/ontologies --out profiles.jsonl --jobs 3
```

Or reason over it:

```sh
horned-corpus reason --corpus /path/to/ontologies --out reasoned.jsonl \
    --reasoners elk,hermit --timeout 300 --max-bytes 20000000
```

Each ontology costs a JVM startup *per reasoner*, so this is far slower than the other sweeps —
`--timeout` bounds each one (the DL reasoners will not finish on the larger ontologies, and
without it a single one stalls the sweep), and `--jobs` multiplies memory since each worker runs
its own JVM. Results record outcome, wall-clock time and the axiom count of the reasoned output,
so the reasoners can be compared against each other on the same ontology.

Only ROBOT's reasoners — ELK, HermiT, JFact — are available today. The Rust ones can't be wired
in yet: whelk-rs, rustdl and hermit-rs all pin `horned-owl ^1.4` while this workspace is on 3.0,
so depending on one resolves a *second* copy of horned-owl rather than sharing ours, and
`[patch.crates-io]` doesn't bridge it (cargo reports the patch "was not used in the crate graph"
and silently builds against 1.4.0). When that closes, ELK is whelk-rs's counterpart and HermiT is
rustdl's and hermit-rs's.

`horned-profile`'s checker is pure Rust and cheap. Add `--robot-ground-truth` to cross-validate
each verdict against ROBOT's `validate-profile` (the OWL API's real checker, so a genuine
independent ground truth) — but that forks a JVM four times per *ontology*, so use it on a
sample, not a full corpus.

Fetch a fresh BioPortal corpus first (requires a [BioPortal API key](https://bioportal.bioontology.org/account)):

```sh
export BIOPORTAL_API_KEY=...        # or pass --api-key, or `cp .env.example .env` and source it
horned-corpus fetch --out ./corpus            # stores <acronym>.gz + manifest.json
horned-corpus roundtrip --corpus ./corpus --out results.jsonl --max-bytes 20000000 --jobs 3
horned-corpus report --in results.jsonl --out-dir report/
```

The key is read from `--api-key` or the `BIOPORTAL_API_KEY` env var. A `.env.example`
template is included; the binary does **not** load `.env` automatically, so source it first
(`set -a; source .env; set +a`) or export the variable.

- `roundtrip --formats rdf,owx,ofn,omn,obo` restricts which target formats to write (default: all five).
- `fetch --timeout <secs>` bounds every request (default `180`). BioPortal's `include=all`
  ontology list alone is multi-MB and routinely exceeds reqwest's 30s default, so a timeout
  is effectively required for `fetch` to complete.
- `fetch --skip-existing` reuses any `<acronym>.gz` already in `--out` (rebuilding its
  manifest entry from disk) and only downloads what's missing, so an interrupted run can
  resume. A corrupt/truncated existing file is re-downloaded rather than skipped.
- The corpus is **not** included in this repo (size + mixed licensing) — `fetch` downloads
  your own copy, or bring your own directory.

## Output

- `cases.csv` — one row per (ontology × target format): outcome, exact-match flag, lost/gained
  counts, per-category counts, timings.
- `summary.json` — per-format-pair exact/ok rates, outcome tallies, per-format
  read-incompleteness rates, category totals, top ontologies by `unknown` count, and per-reasoner
  outcome counts and median time.
- `report.md` — a human summary. For a round-trip run: headline rates, the horned-owl version
  tested, and a ranked list of the `unknown` / `annotation_loss` cases to investigate. For a
  reasoning run: per-reasoner outcomes and timings, the ontologies where two reasoners inferred
  *different numbers of axioms* (they should agree, so each one is worth a look), and failures
  grouped by cause. For a profile run: conformance rates per profile, plus agreement with ROBOT
  when `--robot-ground-truth` was used.

## Limitations

- **No OBO** — horned-owl has no OBO reader, so `.obo` files are reported as `unknown`.
- **Turtle is read-only** — there is no Turtle writer, so Turtle is a source format only.
- **Blank-node canonicalization is a deterministic first cut** (structural sort + a
  visitor-based relabeling pass), not full RDF graph canonicalization; residual blank-node
  ordering differences are routed to `blank_node_relabel` rather than polluting the `unknown`
  signal, so they under-report genuine blank-node defects rather than inventing false ones.

## License

Dual licensed under [MIT](LICENSE-MIT) © 2026 Michel Dumontier, as originally released in the
standalone `horned-roundtrip` repository, or LGPL-3.0 to match the rest of the horned-owl
workspace, at your option.
