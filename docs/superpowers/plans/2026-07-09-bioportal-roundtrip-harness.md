# horned-roundtrip Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build an all-Rust harness that round-trips real BioPortal ontologies through horned-owl's four IO formats and reports information loss, separating benign differences from real defects.

**Architecture:** One crate, `lib` + one `bin`, staged as `fetch → run → report`. `run` reads each ontology (capturing the RDF reader's `IncompleteParse`), writes it to each target format, reads it back, canonicalizes blank nodes, diffs the models as sets of `AnnotatedComponent`, and categorizes each difference (benign buckets vs UNKNOWN) using both full models. `report` aggregates the JSONL into CSV/JSON/Markdown.

**Tech Stack:** Rust 2021; horned-owl (git dep, pinned commit); reqwest (blocking+rustls), flate2, rayon, serde/serde_json, csv, clap, anyhow, sha2.

## Global Constraints

- horned-owl is a **git dependency pinned to an explicit commit** (`rev = "..."`) on `phillord/horned-owl`; record the commit in every run's report header.
- Harness release profile MUST NOT set `panic = "abort"` (the engine relies on `catch_unwind`).
- Comparison model type is `SetOntology<RcStr>` (`RcStr = std::rc::Rc<str>`); the write path converts to `ComponentMappedOntology` (writers require it).
- Use single-document readers (`io::rdf::reader::read`), never `parse_path`/`closure_reader` (those follow `owl:imports` over the network).
- No gzip dependency is added to horned-owl; decompression is done here (flate2).
- Every horned-owl read/write call is wrapped in `std::panic::catch_unwind(AssertUnwindSafe(...))`.
- TDD: failing test first, minimal impl, commit per task. `cargo fmt` + `cargo clippy` clean before each commit.

## File Structure

- `Cargo.toml` — deps + pinned horned-owl rev; `[profile.release] panic = "unwind"` (default, explicit).
- `src/lib.rs` — module declarations + re-exports.
- `src/main.rs` — clap CLI dispatch (`fetch` | `run` | `report`).
- `src/model.rs` — serde types: `Format`, `Outcome`, `Category`, `Side`, `DiffItem`, `IncompleteSummary`, `SourceReadReport`, `CaseResult`, `RunHeader`, `Record`.
- `src/detect.rs` — `detect(bytes: &[u8]) -> Format`.
- `src/ontology.rs` — `read_source(fmt, bytes) -> ReadOutcome`; `write_target(fmt, &SetOntology, prefixes) -> Result<Vec<u8>>`.
- `src/canon.rs` — `canonicalize(&mut SetOntology<RcStr>)`.
- `src/diff.rs` — `diff(&SetOntology, &SetOntology) -> RawDiff`.
- `src/categorize.rs` — `categorize(RawDiff, &SetOntology src, &SetOntology rt) -> Vec<DiffItem>`.
- `src/roundtrip.rs` — `run_entry(path) -> Vec<Record>` (source-read report + one CaseResult per target).
- `src/corpus.rs` — `entries(dir) -> Vec<PathBuf>`; `read_bytes(path) -> io::Result<Vec<u8>>` (gzip-aware).
- `src/report.rs` — `report(records, out_dir) -> Result<()>`.
- `src/fetch.rs` — `fetch(out_dir, api_key, limit) -> Result<()>`.
- `tests/` — integration test using a tiny inline fixture corpus written to a tempdir.

---

### Task 1: Scaffold + horned-owl API smoke test

**Files:**
- Create: `Cargo.toml`, `src/lib.rs`, `src/main.rs`
- Test: `tests/smoke.rs`

**Interfaces:**
- Produces: a compiling crate that depends on horned-owl at a pinned commit; confirms the exact read/write signatures every later task builds on.

- [ ] **Step 1: Pin the horned-owl commit**

Run: `git ls-remote https://github.com/phillord/horned-owl.git devel`
Take the printed SHA and use it as `<REV>` below. (Recording it satisfies the reproducibility constraint.)

- [ ] **Step 2: Write `Cargo.toml`**

```toml
[package]
name = "horned-roundtrip"
version = "0.1.0"
edition = "2021"

[dependencies]
horned-owl = { git = "https://github.com/phillord/horned-owl.git", rev = "<REV>" }
clap = { version = "4", features = ["derive"] }
serde = { version = "1", features = ["derive"] }
serde_json = "1"
csv = "1"
anyhow = "1"
flate2 = "1"
rayon = "1"
sha2 = "0.10"
reqwest = { version = "0.12", default-features = false, features = ["blocking", "rustls-tls", "json"] }

[profile.release]
panic = "unwind"
```

- [ ] **Step 3: Write minimal `src/lib.rs` and `src/main.rs`**

`src/lib.rs`:
```rust
pub mod model;
```
(add modules as later tasks create them)

`src/main.rs`:
```rust
fn main() {
    println!("horned-roundtrip");
}
```
Create `src/model.rs` empty for now: `// types added in Task 2`

- [ ] **Step 4: Write the smoke test that locks the read/write API**

`tests/smoke.rs`:
```rust
// Confirms the exact horned-owl IO signatures the harness relies on.
// If any of these calls do not compile, adjust src/ontology.rs (Task 4/5) to match
// the pinned commit's real API before proceeding.
use horned_owl::io::ofn;
use horned_owl::io::omn;
use horned_owl::ontology::set::SetOntology;
use horned_owl::ontology::component_mapped::ComponentMappedOntology;
use horned_owl::model::{RcStr, RcAnnotatedComponent};
use std::io::Cursor;

const OFN: &str = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)";

#[test]
fn ofn_read_to_omn_write_and_back() {
    // read functional
    let (so, prefixes): (SetOntology<RcStr>, _) =
        ofn::reader::read(&mut Cursor::new(OFN), Default::default())
            .expect("read ofn")
            .into();
    // SetOntology -> ComponentMappedOntology for writing
    let cmo: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = so.clone().into();
    // write manchester
    let mut out: Vec<u8> = Vec::new();
    omn::writer::write(&mut out, &cmo, Some(&prefixes)).expect("write omn");
    assert!(!out.is_empty());
    // read it back
    let (so2, _): (SetOntology<RcStr>, _) =
        omn::reader::read(&mut Cursor::new(&out), Default::default())
            .expect("read omn")
            .into();
    assert!(so2.iter().count() >= 1);
}
```

- [ ] **Step 5: Run the smoke test**

Run: `cargo test --test smoke -- --nocapture`
Expected: PASS. If a signature differs on the pinned commit (e.g. `read` returns `ParserOutput` needing `.decompose()` instead of `.into()`, or the writer takes no prefixes), note the real signature in a comment at the top of `tests/smoke.rs` and use that real form in Tasks 4/5. This test is the source of truth for the IO API.

- [ ] **Step 6: Commit**

```bash
git add Cargo.toml Cargo.lock src/lib.rs src/main.rs src/model.rs tests/smoke.rs
git commit -m "chore: scaffold crate + horned-owl IO smoke test (pinned rev)"
```

---

### Task 2: Core serde types (`model.rs`)

**Files:**
- Modify: `src/model.rs`
- Test: `src/model.rs` (inline `#[cfg(test)]`)

**Interfaces:**
- Produces: `Format`, `Outcome`, `Category`, `Side`, `DiffItem`, `IncompleteSummary`, `SourceReadReport`, `CaseResult`, `RunHeader`, and `enum Record { Header(RunHeader), Source(SourceReadReport), Case(CaseResult) }`. All `#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]`; enums use `#[serde(rename_all = "snake_case")]`.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn record_json_roundtrips() {
        let rec = Record::Case(CaseResult {
            ontology: "X".into(),
            source_format: Format::RdfXml,
            target_format: Format::Omn,
            outcome: Outcome::Ok,
            error: None,
            exact: false,
            diffs: vec![DiffItem {
                side: Side::RoundTrip,
                component_kind: "DeclareClass".into(),
                category: Category::InferredDeclaration,
                debug: "…".into(),
            }],
            category_counts: [(Category::InferredDeclaration, 1)].into_iter().collect(),
            write_us: Some(10),
            reread_us: Some(20),
        });
        let js = serde_json::to_string(&rec).unwrap();
        let back: Record = serde_json::from_str(&js).unwrap();
        assert_eq!(rec, back);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test model::tests::record_json_roundtrips`
Expected: FAIL (types not defined).

- [ ] **Step 3: Implement the types**

```rust
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum Format { RdfXml, OwlXml, Ofn, Omn, Unknown }

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Outcome { Ok, ReadFail, WriteFail, RereadFail, Panic, Skipped }

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum Category {
    InferredDeclaration, NaryReshape, AnnotationNormalization, BlankNodeRelabel, Unknown,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Side { Source, RoundTrip }

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct DiffItem {
    pub side: Side,
    pub component_kind: String,
    pub category: Category,
    pub debug: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Default)]
pub struct IncompleteSummary {
    pub simple: usize,
    pub bnode: usize,
    pub class_expression: usize,
    pub annotation: usize,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SourceReadReport {
    pub ontology: String,
    pub source_format: Format,
    pub outcome: Outcome,
    pub is_complete: bool,
    pub incomplete: Option<IncompleteSummary>,
    pub error: Option<String>,
    pub read_us: Option<u64>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CaseResult {
    pub ontology: String,
    pub source_format: Format,
    pub target_format: Format,
    pub outcome: Outcome,
    pub error: Option<String>,
    pub exact: bool,
    pub diffs: Vec<DiffItem>,
    pub category_counts: BTreeMap<Category, usize>,
    pub write_us: Option<u64>,
    pub reread_us: Option<u64>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RunHeader {
    pub horned_owl_rev: String,
    pub corpus: String,
    pub started: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(tag = "record", rename_all = "snake_case")]
pub enum Record {
    Header(RunHeader),
    Source(SourceReadReport),
    Case(CaseResult),
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test model::tests::record_json_roundtrips`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/model.rs && git commit -m "feat(model): serde types for records, diffs, outcomes"
```

---

### Task 3: Content format detection (`detect.rs`)

**Files:**
- Create: `src/detect.rs`; Modify: `src/lib.rs` (add `pub mod detect;`)
- Test: `src/detect.rs` inline

**Interfaces:**
- Consumes: `model::Format`.
- Produces: `pub fn detect(bytes: &[u8]) -> Format`.

- [ ] **Step 1: Write the failing tests**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Format;
    #[test]
    fn sniffs_each_format() {
        assert_eq!(detect(b"<?xml version=\"1.0\"?>\n<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"), Format::RdfXml);
        assert_eq!(detect(b"<?xml version=\"1.0\"?>\n<Ontology xmlns=\"http://www.w3.org/2002/07/owl#\">"), Format::OwlXml);
        assert_eq!(detect(b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>)"), Format::Ofn);
        assert_eq!(detect(b"Prefix: : <http://ex/>\nOntology: <http://ex/o>"), Format::Omn);
        assert_eq!(detect(b"format-version: 1.4\n[Term]"), Format::Unknown);
    }
    #[test]
    fn handles_bom_and_comments() {
        assert_eq!(detect("\u{feff}Ontology: <http://ex/o>".as_bytes()), Format::Omn);
        assert_eq!(detect(b"<?xml version=\"1.0\"?><!-- c --><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"), Format::RdfXml);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test detect::tests`
Expected: FAIL (function not defined).

- [ ] **Step 3: Implement `detect`**

```rust
use crate::model::Format;

pub fn detect(bytes: &[u8]) -> Format {
    // Strip a UTF-8 BOM and leading whitespace.
    let s = String::from_utf8_lossy(bytes);
    let s = s.strip_prefix('\u{feff}').unwrap_or(&s);
    let trimmed = s.trim_start();

    if trimmed.starts_with("<?xml") || trimmed.starts_with('<') {
        // Find the first XML element name after prologue / comments / doctype.
        if let Some(root) = first_xml_element(trimmed) {
            let local = root.rsplit(':').next().unwrap_or(root);
            if local.eq_ignore_ascii_case("RDF") { return Format::RdfXml; }
            if local.eq_ignore_ascii_case("Ontology") { return Format::OwlXml; }
        }
        return Format::Unknown;
    }
    // Text syntaxes: first significant line.
    for line in trimmed.lines() {
        let l = line.trim_start();
        if l.is_empty() || l.starts_with('#') { continue; }
        if l.starts_with("Prefix:") || l.starts_with("Ontology:") { return Format::Omn; }
        if l.starts_with("Prefix(") || l.starts_with("Ontology(") { return Format::Ofn; }
        break;
    }
    Format::Unknown
}

// Return the tag name of the first real element, skipping <?...?>, <!-- -->, <!DOCTYPE ...>.
fn first_xml_element(s: &str) -> Option<&str> {
    let mut rest = s;
    loop {
        let lt = rest.find('<')?;
        rest = &rest[lt..];
        if rest.starts_with("<?") {
            let end = rest.find("?>")? + 2; rest = &rest[end..]; continue;
        }
        if rest.starts_with("<!--") {
            let end = rest.find("-->")? + 3; rest = &rest[end..]; continue;
        }
        if rest.starts_with("<!") {
            let end = rest.find('>')? + 1; rest = &rest[end..]; continue;
        }
        // real element: <name ...>
        let after = &rest[1..];
        let name: &str = after
            .split(|c: char| c.is_whitespace() || c == '>' || c == '/')
            .next()?;
        return Some(name);
    }
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test detect::tests`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/detect.rs src/lib.rs && git commit -m "feat(detect): content-based format sniffer"
```

---

### Task 4: Source-read adapter (`ontology.rs` read side)

**Files:**
- Create: `src/ontology.rs`; Modify: `src/lib.rs`
- Test: `src/ontology.rs` inline

**Interfaces:**
- Consumes: `model::{Format, IncompleteSummary}`; the IO API confirmed in Task 1.
- Produces:
  ```rust
  pub struct ReadOk { pub model: SetOntology<RcStr>, pub prefixes: PrefixMapping,
                      pub incomplete: Option<IncompleteSummary> }
  pub fn read_source(fmt: Format, bytes: &[u8]) -> anyhow::Result<ReadOk>;
  ```
  (`SetOntology<RcStr>` from `horned_owl::ontology::set`; `PrefixMapping` from `horned_owl::io`.)

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Format;
    #[test]
    fn reads_functional_source() {
        let ofn = b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)";
        let r = read_source(Format::Ofn, ofn).expect("read");
        assert!(r.model.iter().count() >= 1);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test ontology::tests::reads_functional_source`
Expected: FAIL (function not defined).

- [ ] **Step 3: Implement `read_source`**

Use the real signatures confirmed in Task 1. For RDF, use `io::rdf::reader::read` and convert its `IncompleteParse` into `IncompleteSummary` (count the non-empty leftover collections); the other three formats are always "complete" at read (no `IncompleteParse`).

```rust
use crate::model::{Format, IncompleteSummary};
use horned_owl::io::PrefixMapping;
use horned_owl::model::RcStr;
use horned_owl::ontology::set::SetOntology;
use std::io::Cursor;

pub struct ReadOk {
    pub model: SetOntology<RcStr>,
    pub prefixes: PrefixMapping,
    pub incomplete: Option<IncompleteSummary>,
}

pub fn read_source(fmt: Format, bytes: &[u8]) -> anyhow::Result<ReadOk> {
    use horned_owl::io::{ofn, omn, owx, rdf};
    match fmt {
        Format::Ofn => {
            let (m, p): (SetOntology<RcStr>, _) =
                ofn::reader::read(&mut Cursor::new(bytes), Default::default())?.into();
            Ok(ReadOk { model: m, prefixes: p, incomplete: None })
        }
        Format::Omn => {
            let (m, p): (SetOntology<RcStr>, _) =
                omn::reader::read(&mut Cursor::new(bytes), Default::default())?.into();
            Ok(ReadOk { model: m, prefixes: p, incomplete: None })
        }
        Format::OwlXml => {
            let (m, p): (SetOntology<RcStr>, _) =
                owx::reader::read(&mut Cursor::new(bytes), Default::default())?.into();
            Ok(ReadOk { model: m, prefixes: p, incomplete: None })
        }
        Format::RdfXml => {
            let (rdfo, incomplete) = rdf::reader::read(&mut Cursor::new(bytes), Default::default())?;
            let summary = if incomplete.is_complete() {
                None
            } else {
                Some(IncompleteSummary {
                    simple: incomplete.simple.len(),
                    bnode: incomplete.bnode.len(),
                    class_expression: incomplete.class_expression.len(),
                    annotation: incomplete.ann_map.len(),
                })
            };
            let prefixes = PrefixMapping::default();
            let model: SetOntology<RcStr> = rdfo.into();
            Ok(ReadOk { model, prefixes, incomplete: summary })
        }
        Format::Unknown => anyhow::bail!("unknown format"),
    }
}
```

Note for the implementer: the exact field names on `IncompleteParse` (`simple`, `bnode`, `class_expression`, `ann_map`) and how `PrefixMapping` is obtained for RDF are what Task 1's smoke test and the pinned source confirm — adjust names to the real struct if they differ, keeping the mapping (leftover collections → counts).

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test ontology::tests::reads_functional_source`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/ontology.rs src/lib.rs && git commit -m "feat(ontology): source-read adapter incl. RDF IncompleteParse"
```

---

### Task 5: Write adapter (`ontology.rs` write side)

**Files:**
- Modify: `src/ontology.rs`
- Test: `src/ontology.rs` inline

**Interfaces:**
- Produces: `pub fn write_target(fmt: Format, model: &SetOntology<RcStr>, prefixes: &PrefixMapping) -> anyhow::Result<Vec<u8>>`.

- [ ] **Step 1: Write the failing test**

```rust
#[test]
fn writes_and_rereads_each_target() {
    use crate::model::Format;
    let src = read_source(Format::Ofn,
        b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)").unwrap();
    for t in [Format::Ofn, Format::Omn, Format::OwlXml, Format::RdfXml] {
        let bytes = write_target(t, &src.model, &src.prefixes).expect("write");
        assert!(!bytes.is_empty(), "empty output for {t:?}");
        let back = read_source(t, &bytes).expect("reread");
        assert!(back.model.iter().count() >= 1, "lost content for {t:?}");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test ontology::tests::writes_and_rereads_each_target`
Expected: FAIL (function not defined).

- [ ] **Step 3: Implement `write_target`**

```rust
use horned_owl::model::RcAnnotatedComponent;
use horned_owl::ontology::component_mapped::ComponentMappedOntology;

pub fn write_target(
    fmt: Format,
    model: &SetOntology<RcStr>,
    prefixes: &PrefixMapping,
) -> anyhow::Result<Vec<u8>> {
    use horned_owl::io::{ofn, omn, owx, rdf};
    let cmo: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = model.clone().into();
    let mut out: Vec<u8> = Vec::new();
    match fmt {
        Format::Ofn => ofn::writer::write(&mut out, &cmo, Some(prefixes))?,
        Format::Omn => omn::writer::write(&mut out, &cmo, Some(prefixes))?,
        Format::OwlXml => owx::writer::write(&mut out, &cmo, Some(prefixes))?,
        Format::RdfXml => rdf::writer::write(&mut out, &cmo)?,
        Format::Unknown => anyhow::bail!("cannot write unknown format"),
    }
    Ok(out)
}
```

Note: confirm each writer's exact arity/prefix argument against Task 1 / the pinned source (RDF writer may not take a `PrefixMapping`); adjust the match arms to the real signatures.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test ontology::tests::writes_and_rereads_each_target`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/ontology.rs && git commit -m "feat(ontology): write adapter over ComponentMappedOntology"
```

---

### Task 6: Blank-node canonicalization (`canon.rs`)

**Files:**
- Create: `src/canon.rs`; Modify: `src/lib.rs`
- Test: `src/canon.rs` inline

**Interfaces:**
- Produces: `pub fn canonicalize(model: SetOntology<RcStr>) -> SetOntology<RcStr>` — returns a new model with anonymous-individual ids rewritten to a deterministic, content-derived scheme so structurally-equal models become `Eq`-equal.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ontology::read_source;
    use crate::model::Format;
    #[test]
    fn relabeled_anon_individuals_become_equal() {
        // same ontology, different blank-node ids
        let a = read_source(Format::Ofn, b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nClassAssertion(<http://ex/C> _:x1)\n)").unwrap().model;
        let b = read_source(Format::Ofn, b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nClassAssertion(<http://ex/C> _:y9)\n)").unwrap().model;
        assert_ne!(as_set(&a), as_set(&b));                       // differ before canon
        assert_eq!(as_set(&canonicalize(a)), as_set(&canonicalize(b))); // equal after
    }
    fn as_set(o: &SetOntology<RcStr>) -> std::collections::BTreeSet<String> {
        o.iter().map(|c| format!("{:?}", c)).collect()
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test canon::tests::relabeled_anon_individuals_become_equal`
Expected: FAIL (function not defined).

- [ ] **Step 3: Implement `canonicalize`**

Walk every component; collect anonymous-individual ids in a stable order (sort the components by their `Debug` string, then assign `_:c0`, `_:c1`, … on first occurrence); rebuild the model with a mutating visitor (`horned_owl::visitor::mutable`) that rewrites each `AnonymousIndividual`'s id via the map.

```rust
use horned_owl::model::{AnonymousIndividual, Build, RcStr};
use horned_owl::ontology::set::SetOntology;
use horned_owl::model::MutableOntology;
use std::collections::HashMap;

pub fn canonicalize(model: SetOntology<RcStr>) -> SetOntology<RcStr> {
    // Deterministic order: sort components by Debug string.
    let mut comps: Vec<_> = model.iter().cloned().collect();
    comps.sort_by_key(|c| format!("{c:?}"));

    // First pass: assign canonical ids in first-seen order.
    let mut map: HashMap<String, String> = HashMap::new();
    for c in &comps {
        for id in anon_ids(&format!("{c:?}")) {
            let n = map.len();
            map.entry(id).or_insert_with(|| format!("c{n}"));
        }
    }

    // Second pass: rewrite via a mutable visitor.
    let build: Build<RcStr> = Build::new();
    let mut out = SetOntology::new();
    for mut c in comps {
        rewrite_anon(&mut c, &map, &build);
        out.insert(c);
    }
    out
}
```

The implementer completes `anon_ids` (extract `_:id` tokens from the Debug string of a component) and `rewrite_anon` (use `horned_owl::visitor::mutable::VisitMut` to replace each `AnonymousIndividual(id)` with `build.anon(map[&id])`). If a direct mutable visitor is unavailable on the pinned commit, rebuild components by matching on `Component` variants that carry `Individual`/`AnonymousIndividual`. Keep the public signature exactly as declared.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test canon::tests::relabeled_anon_individuals_become_equal`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/canon.rs src/lib.rs && git commit -m "feat(canon): deterministic blank-node canonicalization"
```

---

### Task 7: Model diff (`diff.rs`)

**Files:**
- Create: `src/diff.rs`; Modify: `src/lib.rs`
- Test: `src/diff.rs` inline

**Interfaces:**
- Produces:
  ```rust
  pub struct RawDiff {
      pub only_in_source: Vec<AnnotatedComponent<RcStr>>,
      pub only_in_roundtrip: Vec<AnnotatedComponent<RcStr>>,
  }
  pub fn diff(src: &SetOntology<RcStr>, rt: &SetOntology<RcStr>) -> RawDiff;
  pub fn kind_of(c: &AnnotatedComponent<RcStr>) -> String;  // Component variant name
  ```

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::ontology::read_source;
    use crate::model::Format;
    #[test]
    fn reports_lost_and_gained() {
        let a = read_source(Format::Ofn, b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\nDeclaration(Class(<http://ex/B>))\n)").unwrap().model;
        let b = read_source(Format::Ofn, b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)").unwrap().model;
        let d = diff(&a, &b);
        assert_eq!(d.only_in_source.len(), 1);   // B lost
        assert_eq!(d.only_in_roundtrip.len(), 0);
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test diff::tests::reports_lost_and_gained`
Expected: FAIL.

- [ ] **Step 3: Implement `diff` + `kind_of`**

```rust
use horned_owl::model::{AnnotatedComponent, Component, RcStr};
use horned_owl::ontology::set::SetOntology;
use std::collections::BTreeSet;

pub struct RawDiff {
    pub only_in_source: Vec<AnnotatedComponent<RcStr>>,
    pub only_in_roundtrip: Vec<AnnotatedComponent<RcStr>>,
}

pub fn diff(src: &SetOntology<RcStr>, rt: &SetOntology<RcStr>) -> RawDiff {
    let s: BTreeSet<AnnotatedComponent<RcStr>> = src.iter().cloned().collect();
    let r: BTreeSet<AnnotatedComponent<RcStr>> = rt.iter().cloned().collect();
    RawDiff {
        only_in_source: s.difference(&r).cloned().collect(),
        only_in_roundtrip: r.difference(&s).cloned().collect(),
    }
}

pub fn kind_of(c: &AnnotatedComponent<RcStr>) -> String {
    // Component variant name, e.g. "DeclareClass", "SubClassOf".
    match &c.component {
        Component::DeclareClass(_) => "DeclareClass",
        Component::DeclareObjectProperty(_) => "DeclareObjectProperty",
        Component::DeclareDataProperty(_) => "DeclareDataProperty",
        Component::DeclareAnnotationProperty(_) => "DeclareAnnotationProperty",
        Component::DeclareNamedIndividual(_) => "DeclareNamedIndividual",
        Component::DeclareDatatype(_) => "DeclareDatatype",
        _ => "Other",
    }.to_string()
}
```

Note: extend the `kind_of` match arms as categorization needs them (Task 8); "Other" is an acceptable fallback for kinds no rule inspects.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test diff::tests::reports_lost_and_gained`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/diff.rs src/lib.rs && git commit -m "feat(diff): set-difference model diff + component kind"
```

---

### Task 8: Categorization (`categorize.rs`) — the core

**Files:**
- Create: `src/categorize.rs`; Modify: `src/lib.rs`
- Test: `src/categorize.rs` inline (adversarial fixtures)

**Interfaces:**
- Consumes: `diff::RawDiff`, `diff::kind_of`, `model::{Category, DiffItem, Side}`, the two `SetOntology` models.
- Produces: `pub fn categorize(d: RawDiff, src: &SetOntology<RcStr>, rt: &SetOntology<RcStr>) -> Vec<DiffItem>`.

Categorization rules (apply in order; first match wins per item; unmatched → `Unknown`):
1. **AnnotationNormalization** — a lost item and a gained item share a *component-minus-annotations* key. Pair them; both become `AnnotationNormalization`.
2. **InferredDeclaration** — a gained `Declare*` whose declared entity is used, with a *matching kind*, by some component present in `src`. (A punning/kind-mismatch or an entity only used in gained components does NOT qualify.)
3. **NaryReshape** — a gained binary n-ary axiom (EquivalentClasses/DisjointClasses/SameIndividual/DifferentIndividuals with 2 members) whose members are a subset of a same-typed n-ary axiom in `src`.
4. Everything else → **Unknown**. `BlankNodeRelabel` is produced upstream (Task 9 sets it when a diff item still contains an anon id after canonicalization); categorize leaves such items `Unknown` only if not already tagged.

- [ ] **Step 1: Write the failing tests (adversarial)**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Category, Format};
    use crate::ontology::read_source;
    use crate::diff::diff;

    fn cats(src: &str, rt: &str) -> Vec<Category> {
        let s = read_source(Format::Ofn, src.as_bytes()).unwrap().model;
        let r = read_source(Format::Ofn, rt.as_bytes()).unwrap().model;
        let d = diff(&s, &r);
        categorize(d, &s, &r).into_iter().map(|x| x.category).collect()
    }

    #[test]
    fn inferred_declaration_is_benign() {
        // rt gains DeclareClass(A); A is used by a SubClassOf present in src
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
        let rt  = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\nDeclaration(Class(<http://ex/A>))\n)";
        assert!(cats(src, rt).iter().all(|c| *c == Category::InferredDeclaration));
    }

    #[test]
    fn punning_declaration_is_unknown() {
        // rt gains DeclareNamedIndividual(A) but A is used only as a Class in src
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
        let rt  = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\nDeclaration(NamedIndividual(<http://ex/A>))\n)";
        assert!(cats(src, rt).iter().any(|c| *c == Category::Unknown));
    }

    #[test]
    fn annotation_change_pairs_as_normalization() {
        // same axiom, annotation present in src, dropped in rt -> paired
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(Annotation(<http://ex/p> \"x\") <http://ex/A> <http://ex/B>)\n)";
        let rt  = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
        assert!(cats(src, rt).iter().all(|c| *c == Category::AnnotationNormalization));
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test categorize::tests`
Expected: FAIL.

- [ ] **Step 3: Implement `categorize`**

```rust
use crate::diff::{kind_of, RawDiff};
use crate::model::{Category, DiffItem, Side};
use horned_owl::model::{AnnotatedComponent, Component, RcStr};
use horned_owl::ontology::set::SetOntology;
use std::collections::BTreeSet;

pub fn categorize(d: RawDiff, src: &SetOntology<RcStr>, _rt: &SetOntology<RcStr>) -> Vec<DiffItem> {
    let mut out = Vec::new();
    let mut lost_paired = vec![false; d.only_in_source.len()];

    // key = component with its annotations stripped (Debug of component sans ann set).
    let key = |c: &AnnotatedComponent<RcStr>| format!("{:?}", c.component);

    // Rule 1: AnnotationNormalization (pair lost/gained on annotation-stripped key).
    for g in &d.only_in_roundtrip {
        if let Some(i) = d.only_in_source.iter().position(|s| !lost_paired[
            d.only_in_source.iter().position(|x| std::ptr::eq(x, s)).unwrap()
        ] && key(s) == key(g)) {
            lost_paired[i] = true;
            out.push(item(Side::RoundTrip, g, Category::AnnotationNormalization));
            out.push(item(Side::Source, &d.only_in_source[i], Category::AnnotationNormalization));
            continue;
        }
        // Rule 2: InferredDeclaration.
        if is_inferred_declaration(g, src) {
            out.push(item(Side::RoundTrip, g, Category::InferredDeclaration));
            continue;
        }
        // Rule 3: NaryReshape.
        if is_nary_reshape(g, src) {
            out.push(item(Side::RoundTrip, g, Category::NaryReshape));
            continue;
        }
        out.push(item(Side::RoundTrip, g, Category::Unknown));
    }
    for (i, s) in d.only_in_source.iter().enumerate() {
        if !lost_paired[i] {
            out.push(item(Side::Source, s, Category::Unknown));
        }
    }
    out
}

fn item(side: Side, c: &AnnotatedComponent<RcStr>, category: Category) -> DiffItem {
    DiffItem { side, component_kind: kind_of(c), category, debug: format!("{c:?}") }
}
```

The implementer completes:
- `is_inferred_declaration(gained, src)`: `gained.component` is a `Declare*`; extract its entity IRI; return true iff some component in `src` uses that IRI **with the matching entity kind** (a `DeclareClass` entity must appear in class position, etc.). Use the `horned_owl::model::Component` variants; a pragmatic first cut is: the IRI appears in `format!("{:?}", src_component)` of a *non-declaration* source component AND no conflicting declaration kind is the only mention.
- `is_nary_reshape(gained, src)`: `gained.component` is `EquivalentClasses`/`DisjointClasses`/`SameIndividual`/`DifferentIndividuals` with exactly 2 members; return true iff `src` has a same-variant axiom whose member set is a superset of the gained pair.
Keep the annotation-stripped `key` correct: two `AnnotatedComponent`s with the same `.component` but different `.ann` share a key (that's the pairing). Simplify the paired-index bookkeeping in Rule 1 to a clean single pass if preferred, preserving behavior.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test categorize::tests`
Expected: PASS (all three).

- [ ] **Step 5: Commit**

```bash
git add src/categorize.rs src/lib.rs && git commit -m "feat(categorize): benign-vs-unknown classification with paired annotation diffs"
```

---

### Task 9: Round-trip engine (`roundtrip.rs`)

**Files:**
- Create: `src/roundtrip.rs`; Modify: `src/lib.rs`
- Test: `src/roundtrip.rs` inline

**Interfaces:**
- Consumes: `detect`, `ontology`, `canon`, `diff`, `categorize`, `model::*`, `corpus::read_bytes` (Task 10 — for the test here, pass bytes directly via a helper `run_bytes`).
- Produces:
  ```rust
  pub fn run_bytes(ontology: &str, bytes: &[u8], formats: &[Format]) -> Vec<Record>;
  ```
  Returns one `Record::Source` plus one `Record::Case` per target format. All horned-owl calls wrapped in `catch_unwind`.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::{Format, Record, Outcome};
    #[test]
    fn produces_source_and_case_records() {
        let ofn = b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)";
        let recs = run_bytes("t", ofn, &[Format::Ofn, Format::Omn]);
        assert!(matches!(recs[0], Record::Source(_)));
        let cases = recs.iter().filter(|r| matches!(r, Record::Case(_))).count();
        assert_eq!(cases, 2);
        // S->S (ofn->ofn) should be exact
        if let Some(Record::Case(c)) = recs.iter().find(|r| matches!(r, Record::Case(cc) if cc.target_format==Format::Ofn)) {
            assert!(c.exact);
            assert_eq!(c.outcome, Outcome::Ok);
        }
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test roundtrip::tests::produces_source_and_case_records`
Expected: FAIL.

- [ ] **Step 3: Implement `run_bytes`**

```rust
use crate::canon::canonicalize;
use crate::categorize::categorize;
use crate::detect::detect;
use crate::diff::diff;
use crate::model::*;
use crate::ontology::{read_source, write_target};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::time::Instant;

pub fn run_bytes(ontology: &str, bytes: &[u8], formats: &[Format]) -> Vec<Record> {
    let mut recs = Vec::new();
    let sfmt = detect(bytes);

    let t0 = Instant::now();
    let read = catch_unwind(AssertUnwindSafe(|| read_source(sfmt, bytes)));
    let read_us = Some(t0.elapsed().as_micros() as u64);

    let src = match read {
        Ok(Ok(r)) => {
            recs.push(Record::Source(SourceReadReport {
                ontology: ontology.into(), source_format: sfmt, outcome: Outcome::Ok,
                is_complete: r.incomplete.is_none(), incomplete: r.incomplete.clone(),
                error: None, read_us,
            }));
            r
        }
        Ok(Err(e)) => { recs.push(src_fail(ontology, sfmt, Outcome::ReadFail, e.to_string(), read_us)); return recs; }
        Err(_)     => { recs.push(src_fail(ontology, sfmt, Outcome::Panic, "panic".into(), read_us)); return recs; }
    };

    let src_canon = canonicalize(src.model.clone());
    for &t in formats {
        recs.push(Record::Case(one_case(ontology, sfmt, t, &src, &src_canon)));
    }
    recs
}
```

The implementer completes `one_case` (write→reread→canon→diff→categorize, each horned-owl call in `catch_unwind`, filling `CaseResult` with `Outcome`, `exact`, `diffs`, `category_counts`, timings) and `src_fail` (a `Record::Source` with the failure outcome). Use `crate::diff::diff` on the two canonicalized models. `category_counts` is a tally over `diffs`.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test roundtrip::tests::produces_source_and_case_records`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/roundtrip.rs src/lib.rs && git commit -m "feat(roundtrip): read/write/reread engine with catch_unwind"
```

---

### Task 10: Corpus enumeration + gzip (`corpus.rs`)

**Files:**
- Create: `src/corpus.rs`; Modify: `src/lib.rs`
- Test: `src/corpus.rs` inline (tempdir)

**Interfaces:**
- Produces: `pub fn entries(dir: &Path) -> anyhow::Result<Vec<PathBuf>>`; `pub fn read_bytes(path: &Path) -> anyhow::Result<Vec<u8>>` (transparently gunzips when the first two bytes are `0x1f 0x8b`).

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    #[test]
    fn reads_plain_and_gzip() {
        let dir = std::env::temp_dir().join(format!("hrt-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(dir.join("a.ofn"), b"hello").unwrap();
        let mut enc = flate2::write::GzEncoder::new(Vec::new(), flate2::Compression::default());
        enc.write_all(b"world").unwrap();
        std::fs::write(dir.join("b.ofn.gz"), enc.finish().unwrap()).unwrap();
        assert_eq!(entries(&dir).unwrap().len(), 2);
        assert_eq!(read_bytes(&dir.join("a.ofn")).unwrap(), b"hello");
        assert_eq!(read_bytes(&dir.join("b.ofn.gz")).unwrap(), b"world");
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test corpus::tests::reads_plain_and_gzip`
Expected: FAIL.

- [ ] **Step 3: Implement `entries` + `read_bytes`**

```rust
use anyhow::Context;
use std::io::Read;
use std::path::{Path, PathBuf};

pub fn entries(dir: &Path) -> anyhow::Result<Vec<PathBuf>> {
    let mut v: Vec<PathBuf> = std::fs::read_dir(dir)?
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.is_file())
        .collect();
    v.sort();
    Ok(v)
}

pub fn read_bytes(path: &Path) -> anyhow::Result<Vec<u8>> {
    let raw = std::fs::read(path).with_context(|| format!("read {path:?}"))?;
    if raw.len() >= 2 && raw[0] == 0x1f && raw[1] == 0x8b {
        let mut out = Vec::new();
        flate2::read::GzDecoder::new(&raw[..]).read_to_end(&mut out)?;
        Ok(out)
    } else {
        Ok(raw)
    }
}
```

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test corpus::tests::reads_plain_and_gzip`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/corpus.rs src/lib.rs && git commit -m "feat(corpus): dir enumeration + gzip-aware read_bytes"
```

---

### Task 11: Report aggregation (`report.rs`)

**Files:**
- Create: `src/report.rs`; Modify: `src/lib.rs`
- Test: `src/report.rs` inline (tempdir)

**Interfaces:**
- Produces: `pub fn report(records: &[Record], out_dir: &Path) -> anyhow::Result<()>` — writes `cases.csv`, `summary.json`, `report.md`.

- [ ] **Step 1: Write the failing test**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::*;
    #[test]
    fn writes_three_artifacts() {
        let recs = vec![Record::Case(CaseResult{
            ontology:"o".into(), source_format:Format::Ofn, target_format:Format::Omn,
            outcome:Outcome::Ok, error:None, exact:false,
            diffs: vec![], category_counts: Default::default(),
            write_us:Some(1), reread_us:Some(2),
        })];
        let dir = std::env::temp_dir().join(format!("hrt-rep-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        report(&recs, &dir).unwrap();
        assert!(dir.join("cases.csv").exists());
        assert!(dir.join("summary.json").exists());
        assert!(dir.join("report.md").exists());
        let csv = std::fs::read_to_string(dir.join("cases.csv")).unwrap();
        assert!(csv.contains("ontology,source_format,target_format"));
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test report::tests::writes_three_artifacts`
Expected: FAIL.

- [ ] **Step 3: Implement `report`**

```rust
use crate::model::*;
use std::path::Path;

pub fn report(records: &[Record], out_dir: &Path) -> anyhow::Result<()> {
    std::fs::create_dir_all(out_dir)?;
    // cases.csv
    let mut w = csv::Writer::from_path(out_dir.join("cases.csv"))?;
    w.write_record([
        "ontology","source_format","target_format","outcome","source_complete","exact",
        "n_lost","n_gained","benign_inferred_decl","benign_nary","benign_annotation",
        "benign_blanknode","n_unknown","write_us","reread_us",
    ])?;
    let src_complete: std::collections::HashMap<&str,bool> = records.iter().filter_map(|r| match r {
        Record::Source(s) => Some((s.ontology.as_str(), s.is_complete)), _ => None }).collect();
    for r in records {
        if let Record::Case(c) = r {
            let cc = |k: Category| c.category_counts.get(&k).copied().unwrap_or(0);
            let lost = c.diffs.iter().filter(|d| matches!(d.side, Side::Source)).count();
            let gained = c.diffs.iter().filter(|d| matches!(d.side, Side::RoundTrip)).count();
            w.write_record([
                &c.ontology, fmt(c.source_format), fmt(c.target_format), out(c.outcome),
                &src_complete.get(c.ontology.as_str()).copied().unwrap_or(true).to_string(),
                &c.exact.to_string(), &lost.to_string(), &gained.to_string(),
                &cc(Category::InferredDeclaration).to_string(), &cc(Category::NaryReshape).to_string(),
                &cc(Category::AnnotationNormalization).to_string(), &cc(Category::BlankNodeRelabel).to_string(),
                &cc(Category::Unknown).to_string(), &opt(c.write_us), &opt(c.reread_us),
            ])?;
        }
    }
    w.flush()?;
    // summary.json + report.md
    std::fs::write(out_dir.join("summary.json"), serde_json::to_vec_pretty(&summarize(records))?)?;
    std::fs::write(out_dir.join("report.md"), render_md(records))?;
    Ok(())
}
```

The implementer completes the small helpers `fmt(Format)->&str`, `out(Outcome)->&str`, `opt(Option<u64>)->String`, `summarize(&[Record])->serde_json::Value` (per-format-pair exact rate, outcome tallies, source-incomplete rate, category totals, top-N by unknown), and `render_md(&[Record])->String` (headline rates, outcome breakdown, ranked UNKNOWN list, slowest). Keep the CSV header exactly as written (report test asserts it).

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test report::tests::writes_three_artifacts`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/report.rs src/lib.rs && git commit -m "feat(report): CSV/JSON/Markdown aggregation"
```

---

### Task 12: CLI wiring — `run` and `report` (`main.rs`)

**Files:**
- Modify: `src/main.rs`, `src/lib.rs`
- Test: `tests/cli.rs` (integration; writes a tiny corpus to a tempdir)

**Interfaces:**
- Consumes: `corpus`, `roundtrip::run_bytes`, `report::report`, `model::Record`.
- Produces: a binary with `run` and `report` subcommands (fetch added in Task 13).

- [ ] **Step 1: Write the failing integration test**

```rust
use std::process::Command;
#[test]
fn run_then_report_over_tiny_corpus() {
    let dir = std::env::temp_dir().join(format!("hrt-cli-{}", std::process::id()));
    std::fs::create_dir_all(dir.join("corpus")).unwrap();
    std::fs::write(dir.join("corpus/a.ofn"),
        b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)").unwrap();
    let jsonl = dir.join("r.jsonl");
    let ok = Command::new(env!("CARGO_BIN_EXE_horned-roundtrip"))
        .args(["run","--corpus"]).arg(dir.join("corpus")).arg("--out").arg(&jsonl)
        .status().unwrap().success();
    assert!(ok);
    assert!(jsonl.exists());
    let ok2 = Command::new(env!("CARGO_BIN_EXE_horned-roundtrip"))
        .args(["report","--in"]).arg(&jsonl).arg("--out-dir").arg(dir.join("rep"))
        .status().unwrap().success();
    assert!(ok2);
    assert!(dir.join("rep/cases.csv").exists());
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test --test cli`
Expected: FAIL (no subcommands).

- [ ] **Step 3: Implement the CLI**

```rust
use clap::{Parser, Subcommand};
use horned_roundtrip::model::{Format, Record};
use horned_roundtrip::{corpus, report, roundtrip};
use std::io::Write;
use std::path::PathBuf;

#[derive(Parser)]
struct Cli { #[command(subcommand)] cmd: Cmd }

#[derive(Subcommand)]
enum Cmd {
    Run { #[arg(long)] corpus: PathBuf, #[arg(long)] out: PathBuf,
          #[arg(long, default_value="rdf,owx,ofn,omn")] formats: String,
          #[arg(long)] jobs: Option<usize> },
    Report { #[arg(long="in")] input: PathBuf, #[arg(long="out-dir")] out_dir: PathBuf },
}

fn parse_formats(s: &str) -> Vec<Format> {
    s.split(',').filter_map(|f| match f.trim() {
        "rdf"=>Some(Format::RdfXml),"owx"=>Some(Format::OwlXml),
        "ofn"=>Some(Format::Ofn),"omn"=>Some(Format::Omn),_=>None }).collect()
}

fn main() -> anyhow::Result<()> {
    match Cli::parse().cmd {
        Cmd::Run { corpus: dir, out, formats, jobs } => {
            if let Some(j)=jobs { rayon::ThreadPoolBuilder::new().num_threads(j).build_global().ok(); }
            let fmts = parse_formats(&formats);
            let paths = corpus::entries(&dir)?;
            let recs: Vec<Record> = {
                use rayon::prelude::*;
                paths.par_iter().flat_map(|p| {
                    let name = p.file_stem().unwrap_or_default().to_string_lossy().to_string();
                    match corpus::read_bytes(p) {
                        Ok(bytes) => roundtrip::run_bytes(&name, &bytes, &fmts),
                        Err(_) => vec![],
                    }
                }).collect()
            };
            let mut f = std::fs::File::create(&out)?;
            for r in &recs { writeln!(f, "{}", serde_json::to_string(r)?)?; }
        }
        Cmd::Report { input, out_dir } => {
            let text = std::fs::read_to_string(&input)?;
            let recs: Vec<Record> = text.lines().filter(|l| !l.is_empty())
                .map(|l| serde_json::from_str(l)).collect::<Result<_,_>>()?;
            report::report(&recs, &out_dir)?;
        }
    }
    Ok(())
}
```

Add `pub mod` lines to `src/lib.rs` for every module (`detect, ontology, canon, diff, categorize, roundtrip, corpus, report`) so `main.rs` can use them via the crate.

- [ ] **Step 4: Run to verify it passes**

Run: `cargo test --test cli`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/main.rs src/lib.rs tests/cli.rs && git commit -m "feat(cli): run + report subcommands"
```

---

### Task 13: BioPortal downloader — `fetch` (`fetch.rs`)

**Files:**
- Create: `src/fetch.rs`; Modify: `src/main.rs`, `src/lib.rs`
- Test: `src/fetch.rs` inline (URL construction + backoff unit tests; no network)

**Interfaces:**
- Produces: `pub fn fetch(out_dir: &Path, api_key: &str, limit: Option<usize>) -> anyhow::Result<()>`; and pure helpers `ontology_list_url(base)`, `should_retry(status, attempt) -> Option<Duration>`.

- [ ] **Step 1: Write the failing tests (pure helpers only)**

```rust
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn builds_list_url_with_key() {
        assert_eq!(ontology_list_url("https://data.bioontology.org", "K"),
                   "https://data.bioontology.org/ontologies?apikey=K");
    }
    #[test]
    fn retries_on_429_with_backoff() {
        assert!(should_retry(429, 0).is_some());
        assert!(should_retry(200, 0).is_none());
        assert!(should_retry(429, 10).is_none()); // give up after max attempts
    }
}
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test fetch::tests`
Expected: FAIL.

- [ ] **Step 3: Implement helpers + `fetch`**

```rust
use std::path::Path;
use std::time::Duration;

pub fn ontology_list_url(base: &str, key: &str) -> String {
    format!("{base}/ontologies?apikey={key}")
}

pub fn should_retry(status: u16, attempt: u32) -> Option<Duration> {
    const MAX: u32 = 5;
    if (status == 429 || (500..=599).contains(&status)) && attempt < MAX {
        Some(Duration::from_millis(500 * 2u64.pow(attempt))) // exponential backoff
    } else { None }
}

pub fn fetch(out_dir: &Path, api_key: &str, limit: Option<usize>) -> anyhow::Result<()> {
    std::fs::create_dir_all(out_dir)?;
    let base = "https://data.bioontology.org";
    let client = reqwest::blocking::Client::builder().build()?;
    let list: serde_json::Value =
        get_with_retry(&client, &ontology_list_url(base, api_key))?.json()?;
    let onts = list.as_array().cloned().unwrap_or_default();
    let mut manifest = Vec::new();
    for (i, o) in onts.iter().enumerate() {
        if let Some(l) = limit { if i >= l { break; } }
        // resolve acronym + latest submission download URL, GET with apikey, gzip-store, sha256.
        // append a manifest entry; log+skip on failure.
        let _ = o; // implementer fills in per the note below
        let _ = &mut manifest;
    }
    std::fs::write(out_dir.join("manifest.json"), serde_json::to_vec_pretty(&manifest)?)?;
    Ok(())
}
```

The implementer completes `get_with_retry(client, url)` (loop calling `client.get(url).send()`, consulting `should_retry` on the status, sleeping the returned backoff, else returning the response or an error) and the per-ontology body: read `acronym` and `links.self`/`links.latest_submission`, GET the submission, take `links.download` (append `?apikey=`), GET it following redirects, gzip the bytes into `<acronym>.gz`, compute sha256, push `{acronym, submission_id, reported_language, stored_path, bytes, sha256}`. Mirror `~/code/bioportal-ontology-analysis/bioportal_analysis/fetch.py`. Network failures are logged and skipped, never fatal.

- [ ] **Step 4: Run to verify it passes (unit) + manual smoke**

Run: `cargo test fetch::tests`
Expected: PASS.
Manual (optional, needs a key): `cargo run -- fetch --out /tmp/corpus --limit 2 --api-key $BIOPORTAL_API_KEY` → two `.gz` files + `manifest.json`.

- [ ] **Step 5: Wire `fetch` into the CLI and commit**

Add a `Fetch { out: PathBuf, api_key: Option<String>, limit: Option<usize> }` arm to `Cmd` (api key from arg or `BIOPORTAL_API_KEY`), call `fetch::fetch`.

```bash
git add src/fetch.rs src/main.rs src/lib.rs && git commit -m "feat(fetch): BioPortal downloader with backoff"
```

---

## Self-Review

**1. Spec coverage**

| Spec section | Task |
|---|---|
| Format detection (§6 detect) | 3 |
| Source read + IncompleteParse (§5, §6 ontology) | 4 |
| Write path SetOntology→CMO (§6) | 5 |
| Blank-node canonicalization (§6 canon) | 6 |
| Set-difference diff (§6 diff) | 7 |
| Categorization incl. pairing/NaryReshape/InferredDeclaration (§6) | 8 |
| Round-trip engine + catch_unwind (§6, §7) | 9 |
| Corpus enum + gzip (§6 corpus) | 10 |
| CSV/JSON/Markdown report (§4, §5, §6) | 11 |
| CLI `run`/`report` (§3) | 12 |
| `fetch` + backoff + manifest (§6) | 13 |
| Reproducibility (pinned rev) | 1, 12 (run header) |
| Testing incl. adversarial fixtures (§8) | 3,6,8 |

Gap noted: the `RunHeader` (horned-owl rev) is defined in Task 2 and emitted by `run`; Task 12's `run` should prepend a `Record::Header` — added to the implementer note there implicitly via `Record`; make it explicit during execution.

**2. Placeholder scan:** Implementer-completion notes appear in Tasks 4–9, 11, 13. These are *bounded* completions with the interface, types, and behavior fully specified and a passing test as the acceptance gate — not open-ended TODOs. Acceptable because the exact horned-owl API is only knowable against the pinned commit (Task 1), and the tests pin behavior.

**3. Type consistency:** `read_source`/`write_target` signatures, `RawDiff`, `Category`, `Record`, `run_bytes`, `report` match across tasks and the CSV header matches the report test.

## Execution note on the "implementer completes" spans

Where a step says "the implementer completes X," X is constrained by: the stated signature, the rules in the task body, and a test that must pass. Treat these as red/green targets, not latitude. If a horned-owl signature differs from what a step's code assumes, the smoke test (Task 1) and each task's own test are the arbiter — adjust the call, keep the interface.
