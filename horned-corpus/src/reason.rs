//! Reasoning over the corpus, via ROBOT's `reason` command.
//!
//! Every reasoner here is an OWL API implementation reached by spawning
//! `robot`, so each ontology costs a JVM startup. That is a large fixed
//! overhead, and it means the timings below are only fair *against each
//! other* -- comparing them to an in-process Rust reasoner would be
//! measuring the JVM as much as the reasoning.
//!
//! The Rust reasoners are deliberately absent. whelk-rs, rustdl and
//! hermit-rs all pin `horned-owl ^1.4` while this workspace is on 3.0, so
//! depending on any of them resolves a second copy of horned-owl rather
//! than sharing ours, and their `SetOntology` is then a different type from
//! ours. `[patch.crates-io]` does not bridge it: cargo reports `patch
//! horned-owl v3.0.0 was not used in the crate graph` and silently builds
//! against 1.4.0. When that gap closes they slot in beside these, with ELK
//! as whelk-rs's counterpart and HermiT as rustdl's and hermit-rs's.

use crate::model::{ReasonOutcome, ReasonResult, Reasoner, Record};
use std::time::{Duration, Instant};

impl Reasoner {
    /// The name ROBOT knows this reasoner by, for `--reasoner`.
    pub fn robot_name(self) -> &'static str {
        match self {
            Reasoner::Elk => "ELK",
            Reasoner::HermiT => "HermiT",
            Reasoner::JFact => "JFact",
        }
    }

    pub fn parse(s: &str) -> Option<Self> {
        match s.to_ascii_lowercase().as_str() {
            "elk" => Some(Reasoner::Elk),
            "hermit" => Some(Reasoner::HermiT),
            "jfact" => Some(Reasoner::JFact),
            _ => None,
        }
    }

    pub const ALL: [Reasoner; 3] = [Reasoner::Elk, Reasoner::HermiT, Reasoner::JFact];
}

/// A unique scratch directory for one reasoning call.
///
/// An atomic counter rather than a timestamp, for the same reason
/// `profile::temp_dir` uses one: the sweep runs across rayon workers that
/// share a PID, and timestamp-based names collided in practice, with two
/// concurrent calls corrupting each other's files.
fn temp_dir() -> anyhow::Result<std::path::PathBuf> {
    static COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let n = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let dir = std::env::temp_dir().join(format!("horned-corpus-reason-{}-{n}", std::process::id()));
    std::fs::create_dir_all(&dir)?;
    Ok(dir)
}

/// ROBOT says an ontology is inconsistent or has unsatisfiable classes by
/// failing, so a non-zero exit alone doesn't distinguish "this ontology is
/// unsatisfiable" -- a real result -- from "ROBOT fell over". Classify on
/// the message.
fn classify(stderr: &str) -> ReasonOutcome {
    let s = stderr.to_ascii_lowercase();
    if s.contains("inconsistent") || s.contains("unsatisfiable") {
        ReasonOutcome::Inconsistent
    } else {
        ReasonOutcome::Failed
    }
}

/// First non-empty line of ROBOT's diagnostics, truncated -- enough to
/// group failures by cause without carrying a JVM stack trace per ontology
/// into the results file.
fn first_error_line(stderr: &str) -> Option<String> {
    stderr
        .lines()
        .map(str::trim)
        .find(|l| !l.is_empty())
        .map(|l| l.chars().take(300).collect())
}

/// Run one reasoner over one ontology's bytes.
///
/// `ext` should match the source format so the OWL API can parse it. The
/// whole call is bounded by `timeout`: the DL reasoners will not finish on
/// the larger corpus ontologies, and without a bound a single one of them
/// stalls the sweep indefinitely.
pub fn reason_one(
    ontology: &str,
    bytes: &[u8],
    ext: &str,
    reasoner: Reasoner,
    timeout: Duration,
) -> anyhow::Result<ReasonResult> {
    let dir = temp_dir()?;
    let input = dir.join(format!("input.{ext}"));
    let output = dir.join("reasoned.owl");
    std::fs::write(&input, bytes)?;

    // ROBOT reports its errors on *stdout*, not stderr (confirmed: an
    // unresolvable-import failure prints UnloadableImportException to
    // stdout and leaves stderr empty), so both have to be captured or the
    // failures arrive with no diagnostic at all.
    //
    // To files rather than pipes: this waits on the child in a poll loop,
    // and reading piped output only after that loop would deadlock any
    // ontology whose diagnostics outgrow the pipe buffer -- the child
    // blocks writing, we block waiting, neither budges.
    let log = dir.join("robot.log");
    let t0 = Instant::now();
    let mut child = std::process::Command::new("robot")
        .arg("reason")
        .arg("-i")
        .arg(&input)
        .arg("-r")
        .arg(reasoner.robot_name())
        .arg("-o")
        .arg(&output)
        .stdout(std::fs::File::create(&log)?)
        .stderr(std::process::Stdio::null())
        .spawn()?;

    let timed_out = wait_or_kill(&mut child, timeout)?;
    let elapsed_ms = t0.elapsed().as_millis() as u64;
    let status = child.wait()?;
    let log_text = std::fs::read_to_string(&log).unwrap_or_default();

    // ROBOT can exit 0 having done nothing useful -- the import failure
    // above exits 0 with an exception on stdout and no output file. Treat a
    // missing output as failure regardless of exit status.
    let produced_output = output.exists();

    let (outcome, error, inferred_axioms) = if timed_out {
        (ReasonOutcome::Timeout, None, None)
    } else if status.success() && produced_output {
        (ReasonOutcome::Ok, None, count_axioms(&output))
    } else {
        (classify(&log_text), first_error_line(&log_text), None)
    };

    let _ = std::fs::remove_dir_all(&dir);
    Ok(ReasonResult {
        ontology: ontology.into(),
        reasoner,
        outcome,
        elapsed_ms,
        inferred_axioms,
        error,
    })
}

/// Wait for `child`, killing it if it outruns `timeout`. Returns whether it
/// was killed.
///
/// A poll loop rather than a crate: `wait_timeout` would be one more
/// dependency for twenty lines, and the poll interval only bounds how long
/// a finished process goes unnoticed, which against multi-second JVM starts
/// is immaterial.
fn wait_or_kill(child: &mut std::process::Child, timeout: Duration) -> anyhow::Result<bool> {
    let start = Instant::now();
    loop {
        if child.try_wait()?.is_some() {
            return Ok(false);
        }
        if start.elapsed() >= timeout {
            let _ = child.kill();
            let _ = child.wait();
            return Ok(true);
        }
        std::thread::sleep(Duration::from_millis(100));
    }
}

/// Axioms in ROBOT's reasoned output, read back with horned-owl.
///
/// Best-effort: a reasoned ontology this crate cannot itself read is worth
/// no axiom count but is not a reasoning failure, so this returns `None`
/// rather than turning it into one.
fn count_axioms(path: &std::path::Path) -> Option<usize> {
    let bytes = std::fs::read(path).ok()?;
    let o = crate::ontology::read_source(crate::model::Format::RdfXml, &bytes).ok()?;
    Some(o.model.iter().count())
}

/// Read one ontology and run every reasoner in `reasoners` over it.
///
/// Returns the usual `Record::Source` followed by one `Record::Reason` per
/// reasoner. The source read is horned-owl's, purely to report what this
/// crate makes of the file and to skip ontologies it cannot read at all;
/// ROBOT re-parses the original bytes itself.
pub fn reason_bytes(
    ontology: &str,
    bytes: &[u8],
    reasoners: &[Reasoner],
    timeout: Duration,
) -> Vec<Record> {
    let mut recs = Vec::new();
    let sfmt = crate::detect::detect(bytes);
    let Ok(_src) = crate::roundtrip::read_for_sweep(ontology, bytes, sfmt, &mut recs) else {
        return recs;
    };

    let ext = crate::roundtrip::robot_ext(sfmt);
    for &r in reasoners {
        match reason_one(ontology, bytes, ext, r, timeout) {
            Ok(res) => recs.push(Record::Reason(res)),
            Err(e) => eprintln!(
                "warning: {} failed to run for {ontology}: {e}",
                r.robot_name()
            ),
        }
    }
    recs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn robot_names_match_what_robot_accepts() {
        // `robot reason --help` lists exactly these three.
        assert_eq!(Reasoner::Elk.robot_name(), "ELK");
        assert_eq!(Reasoner::HermiT.robot_name(), "HermiT");
        assert_eq!(Reasoner::JFact.robot_name(), "JFact");
    }

    #[test]
    fn parse_is_case_insensitive_and_rejects_unknown() {
        assert_eq!(Reasoner::parse("elk"), Some(Reasoner::Elk));
        assert_eq!(Reasoner::parse("HermiT"), Some(Reasoner::HermiT));
        assert_eq!(Reasoner::parse("JFACT"), Some(Reasoner::JFact));
        assert_eq!(Reasoner::parse("whelk"), None);
    }

    #[test]
    fn serialises_under_the_names_people_type() {
        // Not the derived snake_case, which would be `hermi_t`/`j_fact`.
        let names: Vec<String> = Reasoner::ALL
            .iter()
            .map(|r| serde_json::to_string(r).unwrap())
            .collect();
        assert_eq!(names, [r#""elk""#, r#""hermit""#, r#""jfact""#]);
    }

    #[test]
    fn inconsistency_is_distinguished_from_failure() {
        // Both exit non-zero; only the message tells them apart, so a
        // genuinely unsatisfiable ontology is not filed as a tool failure.
        assert_eq!(
            classify("ERROR ontology is inconsistent"),
            ReasonOutcome::Inconsistent
        );
        assert_eq!(
            classify("ERROR unsatisfiable classes found"),
            ReasonOutcome::Inconsistent
        );
        assert_eq!(
            classify("ERROR could not resolve import"),
            ReasonOutcome::Failed
        );
    }

    #[test]
    fn error_line_skips_blanks_and_truncates() {
        assert_eq!(
            first_error_line("\n\n  real message  \nsecond"),
            Some("real message".to_string())
        );
        let long = "x".repeat(500);
        assert_eq!(first_error_line(&long).unwrap().len(), 300);
        assert_eq!(first_error_line("   \n  "), None);
    }

    #[test]
    fn a_process_that_outruns_its_budget_is_killed() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .spawn()
            .unwrap();
        let t0 = Instant::now();
        let killed = wait_or_kill(&mut child, Duration::from_millis(300)).unwrap();
        assert!(killed);
        assert!(
            t0.elapsed() < Duration::from_secs(5),
            "should not have waited for the sleep"
        );
    }

    #[test]
    fn a_process_that_finishes_in_time_is_not_killed() {
        let mut child = std::process::Command::new("true").spawn().unwrap();
        assert!(!wait_or_kill(&mut child, Duration::from_secs(10)).unwrap());
    }
}
