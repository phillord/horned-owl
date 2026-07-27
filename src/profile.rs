//! Profile-conformance checking: `horned-profile`'s own EL/QL/RL/DL checker
//! run against every corpus ontology, optionally cross-validated against
//! the OWL API's real profile checker via ROBOT's `validate-profile`
//! command. ROBOT wraps the OWL API directly, so this is a genuine
//! independent ground truth -- not another Rust implementation that could
//! share the same bugs.
//!
//! The ROBOT cross-check is opt-in (`--robot-ground-truth` on `run`): each
//! call to [`robot_verdicts`] spawns four `robot` processes (one per
//! profile), and each spawn forks a JVM -- multiple seconds of wall time
//! per *ontology*, not per corpus. Running it over a full multi-thousand
//! file corpus by default would make every `run` impractically slow; treat
//! it as a spot-check / cross-validation pass over a sample, not a routine
//! part of every sweep.

use crate::model::{Profile as RtProfile, ProfileCheckResult, ProfileVerdict};
use horned_owl::model::RcStr;
use horned_owl::ontology::set::SetOntology;
use horned_profile::Profile as HpProfile;
use std::collections::BTreeMap;

const PROFILES: [(RtProfile, HpProfile); 4] = [
    (RtProfile::Dl, HpProfile::OWL2DL),
    (RtProfile::El, HpProfile::EL),
    (RtProfile::Ql, HpProfile::QL),
    (RtProfile::Rl, HpProfile::RL),
];

/// `horned-profile`'s own verdict for every profile, against `o`.
pub fn horned_verdicts(o: &SetOntology<RcStr>) -> BTreeMap<RtProfile, ProfileVerdict> {
    PROFILES
        .iter()
        .map(|&(rp, hp)| {
            let report = horned_profile::check(o, hp);
            (
                rp,
                ProfileVerdict {
                    conformant: report.is_conformant(),
                    violation_count: report.violations().len(),
                },
            )
        })
        .collect()
}

fn profile_flag(p: RtProfile) -> &'static str {
    match p {
        RtProfile::Dl => "DL",
        RtProfile::El => "EL",
        RtProfile::Ql => "QL",
        RtProfile::Rl => "RL",
    }
}

/// `robot validate-profile`'s verdict for every profile, against the
/// original source `bytes` written out with extension `ext` -- so ROBOT/the
/// OWL API sees exactly the same file content `horned-profile` did, not a
/// horned-owl-written re-serialization. Keeps this a genuine
/// two-implementation comparison, not entangled with horned-owl's own
/// writer correctness (which the rest of this tool already checks
/// separately, in `roundtrip`).
pub fn robot_verdicts(
    bytes: &[u8],
    ext: &str,
) -> anyhow::Result<BTreeMap<RtProfile, ProfileVerdict>> {
    let dir = temp_dir()?;
    let input = dir.join(format!("input.{ext}"));
    std::fs::write(&input, bytes)?;

    let mut out = BTreeMap::new();
    for &(rp, _) in &PROFILES {
        let report_path = dir.join(format!("{}.txt", profile_flag(rp)));
        let status = std::process::Command::new("robot")
            .arg("validate-profile")
            .arg("-i")
            .arg(&input)
            .arg("-p")
            .arg(profile_flag(rp))
            .arg("-o")
            .arg(&report_path)
            .stdout(std::process::Stdio::null())
            .stderr(std::process::Stdio::null())
            .status()?;
        let report = std::fs::read_to_string(&report_path).unwrap_or_default();
        out.insert(rp, parse_robot_report(&report, status.success()));
    }
    let _ = std::fs::remove_dir_all(&dir);
    Ok(out)
}

/// Parse a `robot validate-profile` report file. A conformant report's
/// single line ends "... [Ontology and imports closure in profile]"; a
/// non-conformant one starts "... NOT in profile. The following violations
/// are present:" followed by one line per violation.
fn parse_robot_report(report: &str, exit_success: bool) -> ProfileVerdict {
    if exit_success && !report.contains("NOT in profile") {
        return ProfileVerdict {
            conformant: true,
            violation_count: 0,
        };
    }
    let violation_count = report
        .lines()
        .skip(1) // the "NOT in profile" header line itself
        .filter(|l| !l.trim().is_empty())
        .count();
    ProfileVerdict {
        conformant: false,
        // A malformed/empty report on a failing exit still means "not
        // conformant" even if no violation lines could be parsed out.
        violation_count: violation_count.max(1),
    }
}

/// A unique scratch directory for one `robot_verdicts` call.
///
/// Deliberately an atomic counter, not a nanosecond timestamp: `run`
/// processes the corpus in parallel across rayon worker threads that all
/// share one `std::process::id()`, and a timestamp-based name has a real
/// collision window between two threads racing through this function at
/// (on some platforms/under load) the same clock tick -- confirmed as a
/// real bug, not a theoretical one: an early version of this function used
/// PID+nanosecond-timestamp naming, and cross-validating against ROBOT
/// under `--jobs 4` produced different (dis)agreement results across two
/// otherwise-identical runs over the same 47-file sample, while every
/// individually-reproduced case checked in isolation was stable -- the
/// signature of two concurrent robot_verdicts calls colliding on the same
/// directory and corrupting each other's report files, not genuine ROBOT
/// non-determinism. `fetch_add` on a process-wide counter can never repeat.
fn temp_dir() -> anyhow::Result<std::path::PathBuf> {
    static COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
    let n = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    let dir =
        std::env::temp_dir().join(format!("horned-roundtrip-robot-{}-{n}", std::process::id(),));
    std::fs::create_dir_all(&dir)?;
    Ok(dir)
}

/// Build a `Record::Profile` for `ontology`: `horned-profile`'s own check
/// always runs; ROBOT's ground-truth check runs additionally when
/// `with_robot` is set (see module doc for the cost tradeoff). `ext` should
/// match the source format so ROBOT/the OWL API can actually parse the
/// file -- see `crate::model::Format`'s variants for what's meaningful here.
pub fn check(
    ontology: &str,
    o: &SetOntology<RcStr>,
    bytes: &[u8],
    ext: &str,
    with_robot: bool,
) -> ProfileCheckResult {
    let horned = horned_verdicts(o);
    let robot = if with_robot {
        match robot_verdicts(bytes, ext) {
            Ok(r) => Some(r),
            Err(e) => {
                eprintln!("warning: robot ground-truth failed for {ontology}: {e}");
                None
            }
        }
    } else {
        None
    };
    let agreement = robot
        .as_ref()
        .map(|r| {
            horned
                .iter()
                .filter_map(|(p, hv)| r.get(p).map(|rv| (*p, hv.conformant == rv.conformant)))
                .collect()
        })
        .unwrap_or_default();

    ProfileCheckResult {
        ontology: ontology.into(),
        horned,
        robot,
        agreement,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use horned_owl::model::{Build, DeclareClass, MutableOntology};

    // Regression guard for the temp_dir collision documented in temp_dir's
    // own doc comment: many calls from many threads must never produce the
    // same path. Doesn't reproduce the original race directly (that needed
    // real concurrent robot process spawns under CPU contention to surface
    // as a report-file corruption), but does assert the actual invariant
    // the fix relies on -- the counter never repeats.
    #[test]
    fn temp_dir_is_unique_across_many_concurrent_calls() {
        use std::collections::HashSet;
        use std::sync::{Arc, Mutex};

        let seen: Arc<Mutex<HashSet<std::path::PathBuf>>> = Arc::new(Mutex::new(HashSet::new()));
        let handles: Vec<_> = (0..50)
            .map(|_| {
                let seen = Arc::clone(&seen);
                std::thread::spawn(move || {
                    let dir = temp_dir().unwrap();
                    assert!(
                        seen.lock().unwrap().insert(dir.clone()),
                        "temp_dir collision: {dir:?}"
                    );
                    std::fs::remove_dir_all(&dir).ok();
                })
            })
            .collect();
        for h in handles {
            h.join().unwrap();
        }
    }

    #[test]
    fn trivial_ontology_is_horned_conformant_everywhere() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://ex/A")));

        let v = horned_verdicts(&o);
        assert_eq!(v.len(), 4);
        assert!(v.values().all(|verdict| verdict.conformant));
    }

    #[test]
    fn check_without_robot_leaves_robot_and_agreement_empty() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://ex/A")));

        let result = check("t", &o, b"irrelevant", "ofn", false);
        assert!(result.robot.is_none());
        assert!(result.agreement.is_empty());
        assert_eq!(result.horned.len(), 4);
    }

    #[test]
    fn parse_robot_report_reads_conformant() {
        let v = parse_robot_report(
            "OWL 2 DL Profile Report: [Ontology and imports closure in profile]",
            true,
        );
        assert!(v.conformant);
        assert_eq!(v.violation_count, 0);
    }

    #[test]
    fn parse_robot_report_reads_violations() {
        let report = "OWL 2 EL Profile Report: Ontology and imports closure NOT in profile. The following violations are present:\nClass expressions not allowed in profile: DataMaxCardinality [...]";
        let v = parse_robot_report(report, false);
        assert!(!v.conformant);
        assert_eq!(v.violation_count, 1);
    }
}
