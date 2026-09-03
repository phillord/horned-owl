//! Report aggregation: turns a slice of `Record` (as loaded from a
//! `results.jsonl`) into three artifacts written to `out_dir`:
//! - `cases.csv` — one row per `Record::Case`, flat columns for spreadsheet
//!   analysis.
//! - `summary.json` — a machine-readable rollup (per-format-pair exact
//!   rates, outcome tallies, source-incompleteness, category totals, a
//!   top-N list of the worst UNKNOWN-diff cases, and per-reasoner results).
//! - `report.md` — the same rollup rendered as a scannable Markdown report.
//!
//! Handles all three sweeps. Which sections appear depends on what the
//! records actually contain, so a `reason` or `profile` run gets a report
//! about what it did rather than a round-trip report full of zeroes.
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
        Format::Obo => "obo",
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

fn reasonings(records: &[Record]) -> impl Iterator<Item = &ReasonResult> {
    records.iter().filter_map(|r| match r {
        Record::Reason(r) => Some(r),
        _ => None,
    })
}

fn profile_checks(records: &[Record]) -> impl Iterator<Item = &ProfileCheckResult> {
    records.iter().filter_map(|r| match r {
        Record::Profile(p) => Some(p),
        _ => None,
    })
}

/// Per-reasoner rollup over one run.
#[derive(Default)]
struct ReasonStats {
    ok: usize,
    inconsistent: usize,
    timeout: usize,
    failed: usize,
    /// Elapsed times of the runs that completed, for a median. Failures and
    /// timeouts are excluded: a timeout's elapsed is just the budget, and
    /// averaging that in would say more about `--timeout` than the reasoner.
    ok_ms: Vec<u64>,
}

impl ReasonStats {
    fn total(&self) -> usize {
        self.ok + self.inconsistent + self.timeout + self.failed
    }

    /// Median rather than mean: reasoning times are heavily skewed by a few
    /// large ontologies, and a mean mostly reports those.
    fn median_ok_ms(&self) -> Option<u64> {
        if self.ok_ms.is_empty() {
            return None;
        }
        let mut v = self.ok_ms.clone();
        v.sort_unstable();
        Some(v[v.len() / 2])
    }
}

fn by_reasoner(records: &[Record]) -> BTreeMap<Reasoner, ReasonStats> {
    let mut out: BTreeMap<Reasoner, ReasonStats> = BTreeMap::new();
    for r in reasonings(records) {
        let e = out.entry(r.reasoner).or_default();
        match r.outcome {
            ReasonOutcome::Ok => {
                e.ok += 1;
                e.ok_ms.push(r.elapsed_ms);
            }
            ReasonOutcome::Inconsistent => e.inconsistent += 1,
            ReasonOutcome::Timeout => e.timeout += 1,
            ReasonOutcome::Failed => e.failed += 1,
        }
    }
    out
}

/// Ontologies where two reasoners both succeeded but inferred a different
/// number of axioms.
///
/// This is the point of running more than one: they should agree, so a
/// disagreement is either a bug in one of them or -- more usefully here --
/// an ontology whose constructs they treat differently. Sorted by the size
/// of the gap.
fn axiom_disagreements(records: &[Record]) -> Vec<(String, BTreeMap<Reasoner, usize>)> {
    let mut by_ont: BTreeMap<String, BTreeMap<Reasoner, usize>> = BTreeMap::new();
    for r in reasonings(records) {
        if let (ReasonOutcome::Ok, Some(n)) = (r.outcome, r.inferred_axioms) {
            by_ont
                .entry(r.ontology.clone())
                .or_default()
                .insert(r.reasoner, n);
        }
    }
    let mut v: Vec<_> = by_ont
        .into_iter()
        .filter(|(_, counts)| counts.len() > 1 && counts.values().min() != counts.values().max())
        .collect();
    v.sort_by_key(|(ont, counts)| {
        let spread = counts.values().max().unwrap() - counts.values().min().unwrap();
        (std::cmp::Reverse(spread), ont.clone())
    });
    v.truncate(TOP_N);
    v
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
    let mut reason = serde_json::Map::new();
    for (r, st) in by_reasoner(records) {
        reason.insert(
            reasoner_name(r).to_string(),
            serde_json::json!({
                "total": st.total(),
                "ok": st.ok,
                "inconsistent": st.inconsistent,
                "timeout": st.timeout,
                "failed": st.failed,
                "median_ok_ms": st.median_ok_ms(),
            }),
        );
    }

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
        "by_reasoner": reason,
    })
}

/// The reasoning half of a report: how each reasoner got on, and where two
/// of them disagreed about an ontology.
fn render_reason_md(records: &[Record], s: &mut String) {
    s.push_str("## Reasoning\n\n");
    s.push_str(
        "Every reasoner here runs via ROBOT, so each ontology costs a JVM startup. \
         Times include that fixed overhead and are only fair compared against each \
         other.\n\n",
    );
    s.push_str("| Reasoner | Ontologies | OK | Inconsistent | Timeout | Failed | Median OK |\n");
    s.push_str("|---|---|---|---|---|---|---|\n");
    for (r, st) in by_reasoner(records) {
        s.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            reasoner_name(r),
            st.total(),
            st.ok,
            st.inconsistent,
            st.timeout,
            st.failed,
            st.median_ok_ms()
                .map(|ms| format!("{ms} ms"))
                .unwrap_or_else(|| "-".to_string()),
        ));
    }
    s.push('\n');

    let dis = axiom_disagreements(records);
    if !dis.is_empty() {
        s.push_str("### Reasoners Disagreeing on Axiom Count\n\n");
        s.push_str(
            "Both reasoners completed but inferred a different number of axioms. \
             They should agree, so each of these is either a defect in one of them or \
             an ontology whose constructs they handle differently -- worth a look either \
             way.\n\n",
        );
        s.push_str("| Ontology | Counts | Spread |\n|---|---|---|\n");
        for (ont, counts) in dis {
            let shown: Vec<String> = counts
                .iter()
                .map(|(r, n)| format!("{}: {n}", reasoner_name(*r)))
                .collect();
            let spread = counts.values().max().unwrap() - counts.values().min().unwrap();
            s.push_str(&format!(
                "| {} | {} | {} |\n",
                ont,
                shown.join(", "),
                spread
            ));
        }
        s.push('\n');
    }

    // Failures cluster on a few causes (unresolvable imports, files the OWL
    // API won't parse), so the grouped counts say more than a per-ontology
    // list would.
    let mut causes: BTreeMap<&str, usize> = BTreeMap::new();
    for r in reasonings(records) {
        if let Some(e) = r.error.as_deref() {
            *causes.entry(e).or_default() += 1;
        }
    }
    if !causes.is_empty() {
        let mut v: Vec<_> = causes.into_iter().collect();
        v.sort_by_key(|(e, n)| (std::cmp::Reverse(*n), *e));
        v.truncate(TOP_N);
        s.push_str("### Failure Causes\n\n| Count | Message |\n|---|---|\n");
        for (e, n) in v {
            s.push_str(&format!("| {n} | {} |\n", e.replace('|', "\\|")));
        }
        s.push('\n');
    }
}

/// The profile half of a report: how many ontologies fall in each OWL 2
/// profile, and whether ROBOT agreed when it was asked.
fn render_profile_md(records: &[Record], s: &mut String) {
    s.push_str("## OWL 2 Profiles\n\n");
    s.push_str("| Profile | Checked | Conformant | Conformant % |\n|---|---|---|---|\n");
    for p in [Profile::El, Profile::Ql, Profile::Rl, Profile::Dl] {
        let (checked, conformant) =
            profile_checks(records).fold((0, 0), |(c, k), pc| match pc.horned.get(&p) {
                Some(v) => (c + 1, k + usize::from(v.conformant)),
                None => (c, k),
            });
        s.push_str(&format!(
            "| {} | {checked} | {conformant} | {:.1}% |\n",
            profile_name(p),
            rate(conformant, checked) * 100.0,
        ));
    }
    s.push('\n');

    // Only meaningful when the run was given --robot-ground-truth; without
    // it every `agreement` map is empty and this section is skipped.
    let mut agree: BTreeMap<Profile, (usize, usize)> = BTreeMap::new();
    for pc in profile_checks(records) {
        for (p, ok) in &pc.agreement {
            let e = agree.entry(*p).or_default();
            e.0 += 1;
            e.1 += usize::from(*ok);
        }
    }
    if !agree.is_empty() {
        s.push_str("### Agreement with ROBOT\n\n");
        s.push_str(
            "ROBOT wraps the OWL API's own profile checker, so it is independent \
             ground truth rather than a second opinion from the same lineage. A \
             disagreement is a horned-profile bug until shown otherwise.\n\n",
        );
        s.push_str("| Profile | Compared | Agreed | Agreed % |\n|---|---|---|---|\n");
        for (p, (n, ok)) in agree {
            s.push_str(&format!(
                "| {} | {n} | {ok} | {:.1}% |\n",
                profile_name(p),
                rate(ok, n) * 100.0,
            ));
        }
        s.push('\n');
    }
}

fn reasoner_name(r: Reasoner) -> &'static str {
    match r {
        Reasoner::Elk => "ELK",
        Reasoner::HermiT => "HermiT",
        Reasoner::JFact => "JFact",
    }
}

fn profile_name(p: Profile) -> &'static str {
    match p {
        Profile::El => "EL",
        Profile::Ql => "QL",
        Profile::Rl => "RL",
        Profile::Dl => "DL",
    }
}

fn render_md(records: &[Record]) -> String {
    let mut s = String::new();
    // One report type per sweep -- name it for what the records actually
    // are, rather than calling a reasoning run a "Round-Trip Report".
    let has_cases = cases(records).next().is_some();
    let has_reason = reasonings(records).next().is_some();
    let has_profile = profile_checks(records).next().is_some();
    s.push_str(match (has_cases, has_reason, has_profile) {
        (false, true, false) => "# Reasoning Report\n\n",
        (false, false, true) => "# Profile Report\n\n",
        _ => "# Corpus Report\n\n",
    });

    if let Some(h) = header(records) {
        s.push_str(&format!(
            "horned-owl rev: `{}` | corpus: `{}` | started: {}\n\n",
            h.horned_owl_rev, h.corpus, h.started
        ));
    }

    if has_reason {
        render_reason_md(records, &mut s);
    }
    if has_profile {
        render_profile_md(records, &mut s);
    }
    // Everything below is round-trip specific; on a reason- or profile-only
    // run it would be a page of zeroes and empty tables.
    if !has_cases {
        return s;
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

    fn reason_rec(ont: &str, r: Reasoner, o: ReasonOutcome, ms: u64, ax: Option<usize>) -> Record {
        Record::Reason(ReasonResult {
            ontology: ont.into(),
            reasoner: r,
            outcome: o,
            elapsed_ms: ms,
            inferred_axioms: ax,
            error: None,
        })
    }

    #[test]
    fn reasoning_run_gets_a_reasoning_report_not_an_empty_round_trip_one() {
        // A reason-only run has no cases at all; the round-trip sections
        // would be a page of zeroes, so they are skipped entirely.
        let recs = vec![
            reason_rec("a", Reasoner::Elk, ReasonOutcome::Ok, 100, Some(5)),
            reason_rec("b", Reasoner::Elk, ReasonOutcome::Timeout, 300, None),
        ];
        let md = render_md(&recs);
        assert!(md.starts_with("# Reasoning Report"), "{md}");
        assert!(md.contains("| ELK | 2 | 1 | 0 | 1 | 0 |"), "{md}");
        assert!(!md.contains("Exact-Match Rates"), "{md}");
        assert!(!md.contains("Total cases"), "{md}");
    }

    #[test]
    fn median_ignores_timeouts_and_failures() {
        // A timeout's elapsed is just the budget, so counting it would
        // describe --timeout rather than the reasoner.
        let recs = vec![
            reason_rec("a", Reasoner::HermiT, ReasonOutcome::Ok, 10, Some(1)),
            reason_rec("b", Reasoner::HermiT, ReasonOutcome::Ok, 20, Some(1)),
            reason_rec("c", Reasoner::HermiT, ReasonOutcome::Ok, 30, Some(1)),
            reason_rec("d", Reasoner::HermiT, ReasonOutcome::Timeout, 300_000, None),
        ];
        let st = &by_reasoner(&recs)[&Reasoner::HermiT];
        assert_eq!(st.median_ok_ms(), Some(20));
        assert_eq!(st.total(), 4);
    }

    #[test]
    fn axiom_disagreement_is_reported_and_agreement_is_not() {
        let recs = vec![
            // Same ontology, two reasoners, different counts -- a finding.
            reason_rec("differs", Reasoner::Elk, ReasonOutcome::Ok, 1, Some(10)),
            reason_rec("differs", Reasoner::HermiT, ReasonOutcome::Ok, 1, Some(14)),
            // Same ontology, both agree -- not a finding.
            reason_rec("agrees", Reasoner::Elk, ReasonOutcome::Ok, 1, Some(7)),
            reason_rec("agrees", Reasoner::HermiT, ReasonOutcome::Ok, 1, Some(7)),
            // Only one reasoner ran -- nothing to disagree with.
            reason_rec("alone", Reasoner::Elk, ReasonOutcome::Ok, 1, Some(3)),
        ];
        let d = axiom_disagreements(&recs);
        assert_eq!(d.len(), 1);
        assert_eq!(d[0].0, "differs");

        let md = render_md(&recs);
        assert!(md.contains("Disagreeing on Axiom Count"), "{md}");
        assert!(md.contains("| differs |"), "{md}");
        assert!(!md.contains("| agrees |"), "{md}");
    }

    #[test]
    fn a_timed_out_reasoner_contributes_no_axiom_count_to_compare() {
        // Only completed runs are comparable; a timeout must not read as
        // "inferred nothing" and manufacture a disagreement.
        let recs = vec![
            reason_rec("o", Reasoner::Elk, ReasonOutcome::Ok, 1, Some(9)),
            reason_rec("o", Reasoner::HermiT, ReasonOutcome::Timeout, 300, None),
        ];
        assert!(axiom_disagreements(&recs).is_empty());
    }

    #[test]
    fn profile_run_gets_a_profile_report() {
        use std::collections::BTreeMap;
        let mut horned = BTreeMap::new();
        horned.insert(
            Profile::El,
            ProfileVerdict {
                conformant: true,
                violation_count: 0,
            },
        );
        horned.insert(
            Profile::Dl,
            ProfileVerdict {
                conformant: false,
                violation_count: 3,
            },
        );
        let recs = vec![Record::Profile(ProfileCheckResult {
            ontology: "o".into(),
            horned,
            robot: None,
            agreement: BTreeMap::new(),
        })];
        let md = render_md(&recs);
        assert!(md.starts_with("# Profile Report"), "{md}");
        assert!(md.contains("| EL | 1 | 1 | 100.0% |"), "{md}");
        assert!(md.contains("| DL | 1 | 0 | 0.0% |"), "{md}");
        // No --robot-ground-truth, so nothing to compare against.
        assert!(!md.contains("Agreement with ROBOT"), "{md}");
    }

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
