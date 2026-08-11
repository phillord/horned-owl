//! Report aggregation: turns a slice of `Record` (as loaded from a
//! `results.jsonl`) into three artifacts written to `out_dir`:
//! - `cases.csv` — one row per `Record::Case`, flat columns for spreadsheet
//!   analysis.
//! - `summary.json` — a machine-readable rollup (per-format-pair exact
//!   rates, outcome tallies, source-incompleteness, category totals, and a
//!   top-N list of the worst UNKNOWN-diff cases).
//! - `report.md` — the same rollup rendered as a scannable Markdown report.
//!
//! `report()` is a pure function of `records`: no I/O beyond writing the
//! three output files, no horned-owl, no network.

use crate::model::*;
use std::collections::{BTreeMap, HashMap};
use std::path::Path;

/// How many worst-case (highest `n_unknown`) rows to surface in
/// `summary.json`'s `top_unknown` list and in `report.md`'s ranked section.
const TOP_N: usize = 20;

pub fn report(records: &[Record], out_dir: &Path) -> anyhow::Result<()> {
    std::fs::create_dir_all(out_dir)?;
    // cases.csv
    let mut w = csv::Writer::from_path(out_dir.join("cases.csv"))?;
    w.write_record([
        "ontology",
        "source_format",
        "target_format",
        "outcome",
        "source_complete",
        "exact",
        "n_lost",
        "n_gained",
        "benign_inferred_decl",
        "benign_nary",
        "benign_annotation",
        "annotation_loss",
        "benign_blanknode",
        "n_unknown",
        "write_us",
        "reread_us",
    ])?;
    let src_complete: HashMap<&str, bool> = records
        .iter()
        .filter_map(|r| match r {
            Record::Source(s) => Some((s.ontology.as_str(), s.is_complete)),
            _ => None,
        })
        .collect();
    for r in records {
        if let Record::Case(c) = r {
            let cc = |k: Category| c.category_counts.get(&k).copied().unwrap_or(0);
            let lost = c
                .diffs
                .iter()
                .filter(|d| matches!(d.side, Side::Source))
                .count();
            let gained = c
                .diffs
                .iter()
                .filter(|d| matches!(d.side, Side::RoundTrip))
                .count();
            let row: Vec<String> = vec![
                c.ontology.clone(),
                fmt(c.source_format).to_string(),
                fmt(c.target_format).to_string(),
                out(c.outcome).to_string(),
                src_complete
                    .get(c.ontology.as_str())
                    .copied()
                    .unwrap_or(true)
                    .to_string(),
                c.exact.to_string(),
                lost.to_string(),
                gained.to_string(),
                cc(Category::InferredDeclaration).to_string(),
                cc(Category::NaryReshape).to_string(),
                cc(Category::AnnotationNormalization).to_string(),
                cc(Category::AnnotationLoss).to_string(),
                cc(Category::BlankNodeRelabel).to_string(),
                cc(Category::Unknown).to_string(),
                opt(c.write_us),
                opt(c.reread_us),
            ];
            w.write_record(&row)?;
        }
    }
    w.flush()?;
    // summary.json + report.md
    std::fs::write(
        out_dir.join("summary.json"),
        serde_json::to_vec_pretty(&summarize(records))?,
    )?;
    std::fs::write(out_dir.join("report.md"), render_md(records))?;
    Ok(())
}

fn fmt(f: Format) -> &'static str {
    match f {
        Format::RdfXml => "rdf_xml",
        Format::OwlXml => "owl_xml",
        Format::Ofn => "ofn",
        Format::Omn => "omn",
        Format::Turtle => "turtle",
        Format::Unknown => "unknown",
    }
}

fn out(o: Outcome) -> &'static str {
    match o {
        Outcome::Ok => "ok",
        Outcome::ReadFail => "read_fail",
        Outcome::WriteFail => "write_fail",
        Outcome::RereadFail => "reread_fail",
        Outcome::Panic => "panic",
        Outcome::Skipped => "skipped",
    }
}

fn opt(v: Option<u64>) -> String {
    match v {
        Some(n) => n.to_string(),
        None => String::new(),
    }
}

fn cat_str(c: Category) -> &'static str {
    match c {
        Category::InferredDeclaration => "inferred_declaration",
        Category::NaryReshape => "nary_reshape",
        Category::AnnotationNormalization => "annotation_normalization",
        Category::AnnotationLoss => "annotation_loss",
        Category::BlankNodeRelabel => "blank_node_relabel",
        Category::Unknown => "unknown",
    }
}

fn n_unknown(c: &CaseResult) -> usize {
    c.category_counts
        .get(&Category::Unknown)
        .copied()
        .unwrap_or(0)
}

fn n_annotation_loss(c: &CaseResult) -> usize {
    c.category_counts
        .get(&Category::AnnotationLoss)
        .copied()
        .unwrap_or(0)
}

/// `Unknown` and `AnnotationLoss` are both real, reported findings (as
/// opposed to the benign buckets) -- combine them for the "cases to
/// investigate" ranking in `report.md`.
fn n_investigate(c: &CaseResult) -> usize {
    n_unknown(c) + n_annotation_loss(c)
}

fn cases(records: &[Record]) -> impl Iterator<Item = &CaseResult> {
    records.iter().filter_map(|r| match r {
        Record::Case(c) => Some(c),
        _ => None,
    })
}

/// The run's `RunHeader`, if the records include one (it's written first by
/// `main`'s `roundtrip` subcommand, but `report()` is a pure function of whatever
/// slice it's handed, so callers -- and tests -- may omit it).
fn header(records: &[Record]) -> Option<&RunHeader> {
    records.iter().find_map(|r| match r {
        Record::Header(h) => Some(h),
        _ => None,
    })
}

fn sources(records: &[Record]) -> impl Iterator<Item = &SourceReadReport> {
    records.iter().filter_map(|r| match r {
        Record::Source(s) => Some(s),
        _ => None,
    })
}

/// Per-format-pair rollup: total cases, exact matches, `Outcome::Ok` count.
struct PairStats {
    count: usize,
    exact: usize,
    ok: usize,
}

fn by_format_pair(records: &[Record]) -> BTreeMap<(Format, Format), PairStats> {
    let mut m: BTreeMap<(Format, Format), PairStats> = BTreeMap::new();
    for c in cases(records) {
        let e = m
            .entry((c.source_format, c.target_format))
            .or_insert(PairStats {
                count: 0,
                exact: 0,
                ok: 0,
            });
        e.count += 1;
        if c.exact {
            e.exact += 1;
        }
        if c.outcome == Outcome::Ok {
            e.ok += 1;
        }
    }
    m
}

/// Per-source-format rollup of `Record::Source.is_complete`.
struct SourceStats {
    total: usize,
    incomplete: usize,
}

fn source_completeness(records: &[Record]) -> BTreeMap<Format, SourceStats> {
    let mut m: BTreeMap<Format, SourceStats> = BTreeMap::new();
    for s in sources(records) {
        let e = m.entry(s.source_format).or_insert(SourceStats {
            total: 0,
            incomplete: 0,
        });
        e.total += 1;
        if !s.is_complete {
            e.incomplete += 1;
        }
    }
    m
}

fn rate(n: usize, total: usize) -> f64 {
    if total == 0 {
        0.0
    } else {
        n as f64 / total as f64
    }
}

/// Cases with `n_unknown > 0`, worst first (ties broken by ontology name for
/// determinism), capped at `TOP_N`.
fn top_unknown(records: &[Record]) -> Vec<&CaseResult> {
    let mut v: Vec<&CaseResult> = cases(records).filter(|c| n_unknown(c) > 0).collect();
    v.sort_by(|a, b| {
        n_unknown(b)
            .cmp(&n_unknown(a))
            .then(a.ontology.cmp(&b.ontology))
    });
    v.truncate(TOP_N);
    v
}

/// Cases with `n_investigate() > 0` (i.e. any `Unknown` or `AnnotationLoss`
/// diff -- both are real, reported findings, not benign), worst first (ties
/// broken by ontology name for determinism), capped at `TOP_N`. Used by
/// `report.md`'s "Cases to Investigate" section.
fn top_investigate(records: &[Record]) -> Vec<&CaseResult> {
    let mut v: Vec<&CaseResult> = cases(records).filter(|c| n_investigate(c) > 0).collect();
    v.sort_by(|a, b| {
        n_investigate(b)
            .cmp(&n_investigate(a))
            .then(a.ontology.cmp(&b.ontology))
    });
    v.truncate(TOP_N);
    v
}

fn summarize(records: &[Record]) -> serde_json::Value {
    let mut by_pair = serde_json::Map::new();
    for ((sf, tf), s) in by_format_pair(records) {
        by_pair.insert(
            format!("{}->{}", fmt(sf), fmt(tf)),
            serde_json::json!({
                "count": s.count,
                "exact": s.exact,
                "ok": s.ok,
                "exact_rate": rate(s.exact, s.count),
                "ok_rate": rate(s.ok, s.count),
            }),
        );
    }

    let mut outcomes = serde_json::Map::new();
    for o in [
        Outcome::Ok,
        Outcome::ReadFail,
        Outcome::WriteFail,
        Outcome::RereadFail,
        Outcome::Panic,
        Outcome::Skipped,
    ] {
        let n = cases(records).filter(|c| c.outcome == o).count();
        outcomes.insert(out(o).to_string(), serde_json::json!(n));
    }

    let mut source_incomplete = serde_json::Map::new();
    for (f, s) in source_completeness(records) {
        source_incomplete.insert(
            fmt(f).to_string(),
            serde_json::json!({
                "total": s.total,
                "incomplete": s.incomplete,
                "incomplete_rate": rate(s.incomplete, s.total),
            }),
        );
    }

    let mut category_totals = serde_json::Map::new();
    for cat in [
        Category::InferredDeclaration,
        Category::NaryReshape,
        Category::AnnotationNormalization,
        Category::AnnotationLoss,
        Category::BlankNodeRelabel,
        Category::Unknown,
    ] {
        let n: usize = cases(records)
            .map(|c| c.category_counts.get(&cat).copied().unwrap_or(0))
            .sum();
        category_totals.insert(cat_str(cat).to_string(), serde_json::json!(n));
    }

    let top: Vec<serde_json::Value> = top_unknown(records)
        .into_iter()
        .map(|c| {
            serde_json::json!({
                "ontology": c.ontology,
                "source_format": fmt(c.source_format),
                "target_format": fmt(c.target_format),
                "n_unknown": n_unknown(c),
            })
        })
        .collect();

    serde_json::json!({
        "total_cases": cases(records).count(),
        "by_format_pair": by_pair,
        "outcomes": outcomes,
        "source_incomplete": source_incomplete,
        "category_totals": category_totals,
        "top_unknown": top,
    })
}

fn render_md(records: &[Record]) -> String {
    let mut s = String::new();
    s.push_str("# Round-Trip Report\n\n");

    if let Some(h) = header(records) {
        s.push_str(&format!(
            "horned-owl rev: `{}` | corpus: `{}` | started: {}\n\n",
            h.horned_owl_rev, h.corpus, h.started
        ));
    }

    let total = cases(records).count();
    s.push_str(&format!("Total cases: {total}\n\n"));

    s.push_str("## Exact-Match Rates by Format Pair\n\n");
    s.push_str("| Source | Target | Cases | Exact | Exact % | OK | OK % |\n");
    s.push_str("|---|---|---|---|---|---|---|\n");
    for ((sf, tf), st) in by_format_pair(records) {
        s.push_str(&format!(
            "| {} | {} | {} | {} | {:.1}% | {} | {:.1}% |\n",
            fmt(sf),
            fmt(tf),
            st.count,
            st.exact,
            rate(st.exact, st.count) * 100.0,
            st.ok,
            rate(st.ok, st.count) * 100.0,
        ));
    }
    s.push('\n');

    s.push_str("## Outcome Breakdown\n\n");
    s.push_str("| Outcome | Count |\n|---|---|\n");
    for o in [
        Outcome::Ok,
        Outcome::ReadFail,
        Outcome::WriteFail,
        Outcome::RereadFail,
        Outcome::Panic,
        Outcome::Skipped,
    ] {
        let n = cases(records).filter(|c| c.outcome == o).count();
        s.push_str(&format!("| {} | {n} |\n", out(o)));
    }
    s.push('\n');

    s.push_str("## Source Completeness by Format\n\n");
    s.push_str("| Source Format | Total | Incomplete | Incomplete % |\n|---|---|---|---|\n");
    for (f, st) in source_completeness(records) {
        s.push_str(&format!(
            "| {} | {} | {} | {:.1}% |\n",
            fmt(f),
            st.total,
            st.incomplete,
            rate(st.incomplete, st.total) * 100.0,
        ));
    }
    s.push('\n');

    s.push_str(
        "> **Note:** `BlankNodeRelabel` is a mixed bucket -- it covers both harmless \
         blank-node canonicalization artifacts (residual ordering differences the \
         canonicalizer couldn't fully resolve) *and* possible real blank-node defects. \
         It is reported as benign by default, but is worth manual spot-checking rather \
         than assuming every case in it is harmless.\n\n",
    );

    s.push_str("## Cases to Investigate (Unknown + AnnotationLoss)\n\n");
    s.push_str(
        "Both `Unknown` and `AnnotationLoss` diffs are real, reported findings -- \
         unlike the benign buckets (`InferredDeclaration`, `NaryReshape`, \
         `AnnotationNormalization`, `BlankNodeRelabel`), they are not explained away \
         by canonicalization or reshaping and warrant manual inspection.\n\n",
    );
    let tu = top_investigate(records);
    if tu.is_empty() {
        s.push_str("None. \n\n");
    } else {
        for (i, c) in tu.iter().enumerate() {
            s.push_str(&format!(
                "{}. **{}** ({} -> {}): n_unknown={}, n_annotation_loss={}\n",
                i + 1,
                c.ontology,
                fmt(c.source_format),
                fmt(c.target_format),
                n_unknown(c),
                n_annotation_loss(c),
            ));
            for d in c
                .diffs
                .iter()
                .filter(|d| {
                    d.category == Category::Unknown || d.category == Category::AnnotationLoss
                })
                .take(2)
            {
                s.push_str(&format!(
                    "   - `{:?} {:?} {}`\n",
                    d.side, d.category, d.debug
                ));
            }
        }
        s.push('\n');
    }

    s.push_str("## Slowest Ontologies (write_us + reread_us)\n\n");
    s.push_str("| Ontology | Pair | Write us | Reread us | Total us |\n|---|---|---|---|---|\n");
    let mut timed: Vec<&CaseResult> = cases(records)
        .filter(|c| c.write_us.is_some() || c.reread_us.is_some())
        .collect();
    timed.sort_by_key(|c| std::cmp::Reverse(c.write_us.unwrap_or(0) + c.reread_us.unwrap_or(0)));
    for c in timed.into_iter().take(TOP_N) {
        let total_us = c.write_us.unwrap_or(0) + c.reread_us.unwrap_or(0);
        s.push_str(&format!(
            "| {} | {} -> {} | {} | {} | {} |\n",
            c.ontology,
            fmt(c.source_format),
            fmt(c.target_format),
            opt(c.write_us),
            opt(c.reread_us),
            total_us,
        ));
    }

    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn writes_three_artifacts() {
        let recs = vec![Record::Case(CaseResult {
            ontology: "o".into(),
            source_format: Format::Ofn,
            target_format: Format::Omn,
            outcome: Outcome::Ok,
            error: None,
            exact: false,
            diffs: vec![],
            category_counts: Default::default(),
            write_us: Some(1),
            reread_us: Some(2),
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

    #[test]
    fn unknown_diff_increments_n_unknown_column_and_counts() {
        let recs = vec![Record::Case(CaseResult {
            ontology: "unk-onto".into(),
            source_format: Format::RdfXml,
            target_format: Format::Ofn,
            outcome: Outcome::Ok,
            error: None,
            exact: false,
            diffs: vec![DiffItem {
                side: Side::RoundTrip,
                component_kind: "SubClassOf".into(),
                category: Category::Unknown,
                debug: "SubClassOf(...)".into(),
            }],
            category_counts: [(Category::Unknown, 1)].into_iter().collect(),
            write_us: Some(5),
            reread_us: Some(7),
        })];
        let dir = std::env::temp_dir().join(format!("hrt-rep-unk-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        report(&recs, &dir).unwrap();

        let csv = std::fs::read_to_string(dir.join("cases.csv")).unwrap();
        let header = csv.lines().next().unwrap();
        assert_eq!(
            header,
            "ontology,source_format,target_format,outcome,source_complete,exact,\
n_lost,n_gained,benign_inferred_decl,benign_nary,benign_annotation,\
annotation_loss,benign_blanknode,n_unknown,write_us,reread_us"
        );
        let data_line = csv.lines().nth(1).unwrap();
        let cols: Vec<&str> = data_line.split(',').collect();
        assert_eq!(cols[0], "unk-onto");
        assert_eq!(cols[1], "rdf_xml");
        assert_eq!(cols[2], "ofn");
        assert_eq!(cols[6], "0"); // n_lost
        assert_eq!(cols[7], "1"); // n_gained
        assert_eq!(cols[11], "0"); // annotation_loss
        assert_eq!(cols[13], "1"); // n_unknown

        let summary: serde_json::Value =
            serde_json::from_str(&std::fs::read_to_string(dir.join("summary.json")).unwrap())
                .unwrap();
        assert_eq!(summary["category_totals"]["unknown"], 1);
        assert_eq!(summary["top_unknown"][0]["ontology"], "unk-onto");

        let md = std::fs::read_to_string(dir.join("report.md")).unwrap();
        assert!(md.contains("unk-onto"));
    }
}
