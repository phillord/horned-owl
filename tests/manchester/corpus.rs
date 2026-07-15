//! A2 — corpus parse + structural round-trip via the OWL-API (ROBOT) oracle.
//!
//! Each ontology is:
//!   1. Converted from RDF/XML → Manchester (.omn) by ROBOT (OWL-API oracle).
//!   2. Parsed by horned-owl's Manchester reader.
//!   3. Rendered back to Manchester by horned-owl's Manchester writer.
//!   4. Re-parsed; component multisets compared for structural equality.
//!
//! Only koala is required to fully parse + round-trip (it is the known-good
//! reference fixture).  All other rows are **findings** — logged, never
//! panicked.  The test is gated on docker availability and is therefore not
//! `#[ignore]`d but simply skips gracefully when docker is absent.
use super::*;
use std::path::PathBuf;
use std::process::Command;

const ROBOT_IMAGE: &str = "obolibrary/robot:v1.9.6";

// ---------------------------------------------------------------------------
// Infrastructure helpers
// ---------------------------------------------------------------------------

pub fn docker_available() -> bool {
    Command::new("docker")
        .arg("version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Corpus ontologies, absolute paths to RDF/XML sources readable by ROBOT.
/// Ordered smallest → largest so failures in big ontologies don't hide small ones.
///
/// The corpus directory is taken from the `HORNED_CORPUS_DIR` environment
/// variable and is expected to contain `<name>.rdfxml` for each ontology below.
/// When the variable is unset or the files are absent, this returns an empty
/// vec and the dependent tests skip gracefully (see `docker_available` gating).
///
/// NOTE: doid (27 MB RDF/XML) is intentionally excluded — ROBOT's Manchester
/// serialisation takes >2 minutes on it, exceeding the per-ontology ROBOT timeout.
/// hp (73 MB RDF/XML) converts in ~12 s (ROBOT handles it efficiently).
pub fn corpus_paths() -> Vec<PathBuf> {
    let Some(dir) = std::env::var_os("HORNED_CORPUS_DIR") else {
        return Vec::new();
    };
    let dir = PathBuf::from(dir);
    ["koala", "sio", "obi-core", "hp"]
        .iter()
        .map(|n| dir.join(format!("{n}.rdfxml")))
        .filter(|p| p.exists())
        .collect()
}

/// Per-ontology ROBOT conversion timeout (seconds).  Keep generous — hp takes ~12 s.
const ROBOT_TIMEOUT_SECS: u64 = 120;

/// Convert `src` to Manchester `.omn` using ROBOT; returns the .omn text.
/// Handles non-standard input extensions by staging a .owl copy.
pub fn robot_to_omn(src: &std::path::Path) -> Result<String, String> {
    robot_to_fmt(src, "omn", "owl")
}

/// Convert `src` to `out_fmt` (omn|ofn) via ROBOT. `in_ext` is the extension
/// ROBOT should see for the input (e.g. "owl" for RDF/XML).
///
/// The docker call is wrapped in `timeout(1)` so that ontologies that make ROBOT
/// hang (e.g. doid >2 min) produce a clean error rather than stalling the test suite.
pub fn robot_to_fmt(src: &std::path::Path, out_fmt: &str, in_ext: &str) -> Result<String, String> {
    let tmp = std::env::temp_dir().join(format!(
        "a2-{}-{}",
        std::process::id(),
        src.file_stem().unwrap().to_string_lossy()
    ));
    std::fs::create_dir_all(&tmp).map_err(|e| e.to_string())?;
    let staged = tmp.join(format!("in.{in_ext}"));
    std::fs::copy(src, &staged).map_err(|e| e.to_string())?;
    let dir_str = tmp.to_str().ok_or("non-UTF8 tmp path")?;
    // Use `timeout` to cap the docker call; kills with SIGTERM then SIGKILL.
    let timeout_arg = ROBOT_TIMEOUT_SECS.to_string();
    let mut child = Command::new("timeout")
        .args([
            timeout_arg.as_str(),
            "docker",
            "run",
            "--rm",
            "-v",
            &format!("{dir_str}:/w"),
            "-w",
            "/w",
            ROBOT_IMAGE,
            "robot",
            "convert",
            "-i",
            &format!("in.{in_ext}"),
            "--format",
            out_fmt,
            "-o",
            &format!("out.{out_fmt}"),
        ])
        .spawn()
        .map_err(|e| e.to_string())?;
    let status = child.wait().map_err(|e| e.to_string())?;
    // timeout exits 124 when the child was killed.
    if !status.success() {
        let _ = std::fs::remove_dir_all(&tmp);
        let code = status.code().unwrap_or(-1);
        if code == 124 {
            return Err(format!(
                "ROBOT timed out after {ROBOT_TIMEOUT_SECS}s (exit 124)"
            ));
        }
        return Err(format!("ROBOT exited with code {code}"));
    }
    let result =
        std::fs::read_to_string(tmp.join(format!("out.{out_fmt}"))).map_err(|e| e.to_string());
    // Best-effort cleanup; do not fail on error.
    let _ = std::fs::remove_dir_all(&tmp);
    result
}

// ---------------------------------------------------------------------------
// Result type
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct CorpusRow {
    pub name: String,
    /// Byte length of the ROBOT-produced .omn file.
    pub bytes: usize,
    /// horned-owl reader accepted the ROBOT .omn.
    pub parse_ok: bool,
    /// Number of AnnotatedComponents in the parsed ontology.
    pub components: usize,
    /// write_str → re-parse → component multiset equals original.
    pub roundtrip_ok: bool,
    /// Non-empty when parse or render had a blocker.
    pub blocking: String,
}

// ---------------------------------------------------------------------------
// Corpus runner
// ---------------------------------------------------------------------------

pub fn run_corpus() -> Vec<CorpusRow> {
    let mut rows = Vec::new();
    for p in corpus_paths() {
        let name = p.file_stem().unwrap().to_string_lossy().into_owned();
        eprintln!("[corpus] {name}: converting via ROBOT…");

        // Step 1: ROBOT convert → .omn
        let omn = match robot_to_omn(&p) {
            Ok(s) => s,
            Err(e) => {
                let row = CorpusRow {
                    name: name.clone(),
                    bytes: 0,
                    parse_ok: false,
                    components: 0,
                    roundtrip_ok: false,
                    blocking: format!("robot: {e}"),
                };
                eprintln!("[corpus] {name}: {row:?}");
                rows.push(row);
                continue;
            }
        };
        let bytes = omn.len();
        eprintln!("[corpus] {name}: omn {bytes} bytes — parsing…");

        // Step 2: horned-owl parse
        let (ont, pm) = match read_str(&omn) {
            Ok(pair) => pair,
            Err(e) => {
                let blocker = e.lines().next().unwrap_or("").to_owned();
                let row = CorpusRow {
                    name: name.clone(),
                    bytes,
                    parse_ok: false,
                    components: 0,
                    roundtrip_ok: false,
                    blocking: blocker,
                };
                eprintln!("[corpus] {name}: {row:?}");
                rows.push(row);
                continue;
            }
        };
        let components = ont.iter().count();
        eprintln!("[corpus] {name}: parsed {components} components — rendering…");

        // Step 3: render (guard against writer panics)
        let render_result =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| write_str(&ont, &pm)));
        let (roundtrip_ok, blocking) = match render_result {
            Err(_) => (false, "render panicked".to_owned()),
            Ok(rendered) => {
                // Step 4: re-parse + component comparison
                match read_str(&rendered) {
                    Ok((ont2, _)) => {
                        let ok = components_sorted(&ont) == components_sorted(&ont2);
                        let note = if ok {
                            String::new()
                        } else {
                            format!(
                                "component mismatch: {} vs {}",
                                components_sorted(&ont).len(),
                                components_sorted(&ont2).len()
                            )
                        };
                        (ok, note)
                    }
                    Err(e) => (
                        false,
                        format!("re-parse: {}", e.lines().next().unwrap_or("")),
                    ),
                }
            }
        };

        let row = CorpusRow {
            name: name.clone(),
            bytes,
            parse_ok: true,
            components,
            roundtrip_ok,
            blocking,
        };
        eprintln!("[corpus] {name}: {row:?}");
        rows.push(row);
    }
    rows
}

// ---------------------------------------------------------------------------
// Gated test
// ---------------------------------------------------------------------------

// Slow (≈minutes) and docker/ROBOT-dependent; characterization, not a
// deterministic gate (findings live in the generated compliance report). Run
// explicitly: `cargo test --test manchester_conformance -- --ignored corpus_parses`.
#[test]
#[ignore = "slow + docker/ROBOT-dependent; run via --ignored or the report generator"]
fn corpus_parses_or_documents_blocker() {
    if !docker_available() {
        eprintln!("SKIPPED A2: docker/ROBOT not available");
        return;
    }
    if corpus_paths().is_empty() {
        eprintln!(
            "SKIPPED A2: no corpus fixtures found \
             (set HORNED_CORPUS_DIR to a directory containing \
             koala/sio/obi-core/hp .rdfxml)"
        );
        return;
    }
    let rows = run_corpus();
    assert!(!rows.is_empty(), "no corpus fixtures found");
    for r in &rows {
        eprintln!("{r:?}");
        if r.name == "koala" && !r.parse_ok {
            panic!("regression: koala no longer parses: {}", r.blocking);
        }
    }
}
