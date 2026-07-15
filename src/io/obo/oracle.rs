//! ROBOT/`oboformat` oracle harness for the OBO reader (issue #181).
//!
//! For every `.obo` fixture under `src/ont/obo/`, this converts the file to OWL
//! functional syntax with ROBOT (whose OBO→OWL mapping is the OWL-API
//! `oboformat` writer), then reads BOTH the `.obo` (via [`crate::io::obo`]) and
//! ROBOT's `.ofn` (via [`crate::io::ofn`]) into the same horned-owl model and
//! diffs the component sets. Any divergence is a mapping bug or a
//! not-yet-implemented clause.
//!
//! The test is `#[ignore]`d because it needs a ROBOT install. Run it with:
//!
//! ```text
//! HORNED_ROBOT=/path/to/robot \
//!   cargo test --lib io::obo::oracle -- --ignored --nocapture
//! ```
//!
//! `HORNED_ROBOT` may be a wrapper script or the `robot` launcher; if unset the
//! harness looks for `robot` on `PATH`. With neither present the test skips.

use std::collections::BTreeSet;
use std::fs::{File, create_dir_all, read_dir};
use std::io::BufReader;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::model::RcStr;
use crate::ontology::set::SetOntology;

/// Resolve the ROBOT command: `$HORNED_ROBOT`, else `robot` if it runs.
fn robot_command() -> Option<String> {
    if let Ok(cmd) = std::env::var("HORNED_ROBOT") {
        return Some(cmd);
    }
    Command::new("robot")
        .arg("--version")
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|_| "robot".to_string())
}

/// Convert an `.obo` file to functional syntax with ROBOT.
fn robot_convert(robot: &str, obo: &Path, ofn: &Path) {
    let status = Command::new(robot)
        .args(["convert", "--input"])
        .arg(obo)
        .args(["--format", "ofn", "--output"])
        .arg(ofn)
        .status()
        .expect("failed to run ROBOT");
    assert!(status.success(), "ROBOT convert failed for {obo:?}");
}

/// Render an ontology as the set of its components' canonical debug strings.
fn components(ont: &SetOntology<RcStr>) -> BTreeSet<String> {
    ont.iter().map(|ac| format!("{ac:?}")).collect()
}

fn read_obo(path: &Path) -> BTreeSet<String> {
    let reader = BufReader::new(File::open(path).unwrap());
    let (ont, _): (SetOntology<RcStr>, _) =
        crate::io::obo::reader::read(reader, Default::default()).unwrap();
    components(&ont)
}

fn read_ofn(path: &Path) -> BTreeSet<String> {
    let reader = BufReader::new(File::open(path).unwrap());
    let (ont, _): (SetOntology<RcStr>, _) =
        crate::io::ofn::reader::read(reader, Default::default()).unwrap();
    components(&ont)
}

/// Group debug strings by their leading `Component` variant for a readable report.
fn by_kind(lines: &BTreeSet<String>) -> std::collections::BTreeMap<String, usize> {
    let mut m = std::collections::BTreeMap::new();
    for l in lines {
        // AnnotatedComponent { component: <Variant>(...), ann: {...} }
        let kind = l
            .split("component: ")
            .nth(1)
            .and_then(|s| s.split(['(', ' ', '{']).next())
            .unwrap_or("?")
            .to_string();
        *m.entry(kind).or_insert(0) += 1;
    }
    m
}

#[test]
#[ignore = "requires ROBOT: set HORNED_ROBOT or put robot on PATH; run with --ignored --nocapture"]
fn obo_matches_robot_oracle() {
    let robot = match robot_command() {
        Some(r) => r,
        None => {
            eprintln!("SKIP: ROBOT not found (set HORNED_ROBOT or add robot to PATH)");
            return;
        }
    };

    let src_dir = Path::new("./src/ont/obo");
    let tmp_dir = PathBuf::from("./tmp/obo");
    create_dir_all(&tmp_dir).unwrap();

    let mut total_missing = 0usize;
    let mut total_extra = 0usize;

    let mut fixtures: Vec<PathBuf> = read_dir(src_dir)
        .expect("src/ont/obo exists")
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| p.extension().is_some_and(|x| x == "obo"))
        .collect();
    fixtures.sort();

    for obo in &fixtures {
        let ofn = tmp_dir.join(obo.file_name().unwrap()).with_extension("ofn");
        robot_convert(&robot, obo, &ofn);

        let ours = read_obo(obo);
        let oracle = read_ofn(&ofn);

        let missing: BTreeSet<_> = oracle.difference(&ours).cloned().collect();
        let extra: BTreeSet<_> = ours.difference(&oracle).cloned().collect();
        total_missing += missing.len();
        total_extra += extra.len();

        println!("\n=== {} ===", obo.file_name().unwrap().to_string_lossy());
        println!(
            "  matched: {}   missing: {}   extra: {}",
            ours.intersection(&oracle).count(),
            missing.len(),
            extra.len()
        );
        if !missing.is_empty() {
            println!("  -- only in ROBOT (not produced by our reader), by kind:");
            for (k, n) in by_kind(&missing) {
                println!("       {n:>4}  {k}");
            }
            for l in &missing {
                println!("     - {l}");
            }
        }
        if !extra.is_empty() {
            println!("  -- only in our reader (ROBOT does not emit), by kind:");
            for (k, n) in by_kind(&extra) {
                println!("       {n:>4}  {k}");
            }
            for l in &extra {
                println!("     + {l}");
            }
        }
    }

    assert_eq!(
        (total_missing, total_extra),
        (0, 0),
        "OBO reader diverges from the ROBOT oracle ({total_missing} missing, \
         {total_extra} extra) — see the per-fixture report above"
    );
}
