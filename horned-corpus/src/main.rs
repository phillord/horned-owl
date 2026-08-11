use anyhow::Context;
use clap::{Parser, Subcommand};
use horned_corpus::model::{Format, Outcome, Record, RunHeader, SourceReadReport};
use horned_corpus::{corpus, fetch, report, roundtrip};
use std::io::Write;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

/// What horned-owl this run tested, for provenance in the run header:
/// `"3.0.0 (2d20450)"`, or just `"3.0.0"` if the commit can't be read.
/// `--horned-owl-rev` overrides the whole string.
///
/// The version comes from [`horned_owl::VERSION`], which is
/// `CARGO_PKG_VERSION` of the crate that actually got linked, fixed at
/// compile time. The commit is a *runtime* question about the working
/// tree, so it can disagree with what was built -- build, commit, then run
/// and the hash is one commit ahead of the binary. It's still worth having,
/// because between releases the version alone doesn't change and cannot
/// tell two corpus runs apart; treat the version as authoritative and the
/// hash as a hint.
fn resolve_horned_owl_rev(manifest_dir: &std::path::Path, cli_override: Option<&str>) -> String {
    if let Some(r) = cli_override {
        return r.to_string();
    }
    match head_commit(manifest_dir) {
        Some(c) => format!("{} ({c})", horned_owl::VERSION),
        None => horned_owl::VERSION.to_string(),
    }
}

/// Short HEAD hash of the repository containing `dir`, if it is one.
fn head_commit(dir: &std::path::Path) -> Option<String> {
    // git exports GIT_DIR and friends to the processes it spawns for hooks.
    // Left in place they override `-C`, so under a hook this would report
    // the invoking repository's HEAD rather than ours.
    let output = std::process::Command::new("git")
        .env_remove("GIT_DIR")
        .env_remove("GIT_INDEX_FILE")
        .env_remove("GIT_WORK_TREE")
        .env_remove("GIT_PREFIX")
        .env_remove("GIT_CONFIG")
        .arg("-C")
        .arg(dir)
        .args(["rev-parse", "--short", "HEAD"])
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let s = String::from_utf8(output.stdout).ok()?.trim().to_string();
    (!s.is_empty()).then_some(s)
}

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Round-trip every ontology in a corpus through the requested formats.
    Roundtrip {
        /// Directory of ontologies to sweep. Read one level deep, every
        /// file tried; format is detected by content, not extension, and
        /// gzipped files are decompressed transparently.
        #[arg(long)]
        corpus: PathBuf,
        /// JSONL file to stream results into, one record per line.
        #[arg(long)]
        out: PathBuf,
        /// Which formats to write and read back, comma-separated.
        #[arg(long, default_value = "rdf,owx,ofn,omn")]
        formats: String,
        /// Worker threads (default: one per core).
        #[arg(long)]
        jobs: Option<usize>,
        /// Skip any corpus file whose byte length exceeds this cap, recording
        /// it as `Outcome::Skipped` instead of running it through the engine.
        #[arg(long = "max-bytes")]
        max_bytes: Option<u64>,
        /// Override the horned-owl version recorded in the run header.
        /// Defaults to the linked horned-owl's version plus the current
        /// commit; set it by hand when that isn't what you want recorded --
        /// e.g. a dirty tree, or a build made before the commit the working
        /// tree is now on.
        #[arg(long = "horned-owl-rev")]
        horned_owl_rev: Option<String>,
    },
    /// Check every ontology in a corpus against the OWL 2 profiles.
    Profile {
        /// Directory of ontologies to sweep, as for `roundtrip`.
        #[arg(long)]
        corpus: PathBuf,
        /// JSONL file to stream results into, one record per line.
        #[arg(long)]
        out: PathBuf,
        /// Worker threads (default: one per core).
        #[arg(long)]
        jobs: Option<usize>,
        /// Skip any corpus file whose byte length exceeds this cap, recording
        /// it as `Outcome::Skipped` instead of reading it.
        #[arg(long = "max-bytes")]
        max_bytes: Option<u64>,
        /// Override the horned-owl version recorded in the run header.
        #[arg(long = "horned-owl-rev")]
        horned_owl_rev: Option<String>,
        /// Additionally cross-validate every check against ROBOT's
        /// `validate-profile` (the OWL API's real checker). Expensive: four
        /// `robot` process spawns, each forking a JVM, per ontology -- meant
        /// for a sample, not a routine full-corpus sweep.
        #[arg(long = "robot-ground-truth")]
        robot_ground_truth: bool,
    },
    /// Aggregate a run's JSONL output into a report directory.
    Report {
        /// JSONL file produced by `roundtrip` or `profile`.
        #[arg(long = "in")]
        input: PathBuf,
        /// Directory to write cases.csv, summary.json and report.md into.
        #[arg(long = "out-dir")]
        out_dir: PathBuf,
    },
    /// Download a corpus of ontologies from BioPortal.
    Fetch {
        #[arg(long)]
        out: PathBuf,
        /// BioPortal API key; falls back to the BIOPORTAL_API_KEY env var.
        #[arg(long = "api-key")]
        api_key: Option<String>,
        #[arg(long)]
        limit: Option<usize>,
        /// Per-request timeout in seconds (applies to the ontology list,
        /// each submission lookup, and each download).
        #[arg(long, default_value_t = 180)]
        timeout: u64,
        /// Skip re-downloading any ontology whose `<acronym>.gz` already
        /// exists in `out`, reusing its manifest entry instead.
        #[arg(long = "skip-existing")]
        skip_existing: bool,
    },
}

fn parse_formats(s: &str) -> Vec<Format> {
    s.split(',')
        .filter_map(|f| match f.trim() {
            "rdf" => Some(Format::RdfXml),
            "owx" => Some(Format::OwlXml),
            "ofn" => Some(Format::Ofn),
            "omn" => Some(Format::Omn),
            _ => None,
        })
        .collect()
}

/// A cheap, dependency-free timestamp (seconds since the Unix epoch, as a
/// string) for the run header. Good enough for tracing a report back to when
/// it was produced without pulling in `chrono`.
fn timestamp() -> String {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs().to_string())
        .unwrap_or_else(|_| "0".to_string())
}

/// A `Source` record for a file skipped by the `--max-bytes` cap.
fn skipped_record(name: String, len: u64, cap: u64) -> Record {
    Record::Source(SourceReadReport {
        ontology: name,
        source_format: Format::Unknown,
        outcome: Outcome::Skipped,
        is_complete: false,
        incomplete: None,
        error: Some(format!("skipped: {len} bytes exceeds --max-bytes {cap}")),
        read_us: None,
    })
}

/// Sweep every file in `dir`, applying `per_file` to each one's raw bytes
/// and streaming the resulting records to `out` as JSONL.
///
/// Shared by `roundtrip` and `profile`, which differ only in what they do
/// with each file. Records are written and dropped as each file completes
/// rather than accumulated, so memory stays flat over a corpus of thousands
/// and partial progress survives an interruption.
fn sweep(
    dir: &std::path::Path,
    out: &std::path::Path,
    jobs: Option<usize>,
    max_bytes: Option<u64>,
    horned_owl_rev: Option<&str>,
    per_file: impl Fn(&str, &[u8]) -> Vec<Record> + Sync,
) -> anyhow::Result<()> {
    // catch_unwind inside the per-file work recovers from panics, but the
    // default hook still writes a message to stderr for each one. Across
    // ~1200 corpus files that floods the terminal, so install a no-op hook.
    // Process-wide and not restored, which is fine for a one-shot CLI but
    // would need scoping if this became reusable in a longer-lived process.
    std::panic::set_hook(Box::new(|_| {}));

    if let Some(j) = jobs {
        rayon::ThreadPoolBuilder::new()
            .num_threads(j)
            .build_global()
            .ok();
    }
    let paths = corpus::entries(dir)?;
    let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let header = Record::Header(RunHeader {
        horned_owl_rev: resolve_horned_owl_rev(manifest_dir, horned_owl_rev),
        corpus: dir.to_string_lossy().to_string(),
        started: timestamp(),
    });

    let writer = std::sync::Mutex::new(std::io::BufWriter::new(std::fs::File::create(out)?));
    {
        let mut w = writer.lock().unwrap();
        writeln!(w, "{}", serde_json::to_string(&header)?)?;
    }
    {
        use rayon::prelude::*;
        paths.par_iter().try_for_each(|p| -> anyhow::Result<()> {
            let name = p
                .file_stem()
                .unwrap_or_default()
                .to_string_lossy()
                .to_string();
            // Pre-read size gate: skip oversized files WITHOUT reading them
            // into memory (a multi-hundred-MB ontology would spike RAM even
            // if only to discard it). read_bytes still guards against a small
            // gzip that inflates past the cap.
            let over_cap_on_disk = match (max_bytes, std::fs::metadata(p)) {
                (Some(cap), Ok(meta)) if meta.len() > cap => Some((meta.len(), cap)),
                _ => None,
            };
            let recs: Vec<Record> = if let Some((len, cap)) = over_cap_on_disk {
                vec![skipped_record(name, len, cap)]
            } else {
                match corpus::read_bytes(p) {
                    Ok(bytes) => match max_bytes {
                        Some(cap) if bytes.len() as u64 > cap => {
                            vec![skipped_record(name, bytes.len() as u64, cap)]
                        }
                        // A panic outside the per-file work's own
                        // catch_unwind must not abort the sweep -- record it
                        // as one Outcome::Panic Source.
                        _ => match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                            per_file(&name, &bytes)
                        })) {
                            Ok(recs) => recs,
                            Err(_) => vec![Record::Source(SourceReadReport {
                                ontology: name,
                                source_format: Format::Unknown,
                                outcome: Outcome::Panic,
                                is_complete: false,
                                incomplete: None,
                                error: Some("panic during processing".into()),
                                read_us: None,
                            })],
                        },
                    },
                    Err(_) => vec![],
                }
            };
            if !recs.is_empty() {
                let mut w = writer.lock().unwrap();
                for r in &recs {
                    writeln!(w, "{}", serde_json::to_string(r)?)?;
                }
                w.flush()?;
            }
            Ok(())
        })?;
    }
    writer.lock().unwrap().flush()?;
    Ok(())
}

fn main() -> anyhow::Result<()> {
    match Cli::parse().cmd {
        Cmd::Roundtrip {
            corpus: dir,
            out,
            formats,
            jobs,
            max_bytes,
            horned_owl_rev,
        } => {
            let fmts = parse_formats(&formats);
            sweep(
                &dir,
                &out,
                jobs,
                max_bytes,
                horned_owl_rev.as_deref(),
                |name, bytes| roundtrip::run_bytes(name, bytes, &fmts),
            )?;
        }
        Cmd::Profile {
            corpus: dir,
            out,
            jobs,
            max_bytes,
            horned_owl_rev,
            robot_ground_truth,
        } => {
            sweep(
                &dir,
                &out,
                jobs,
                max_bytes,
                horned_owl_rev.as_deref(),
                |name, bytes| roundtrip::profile_bytes(name, bytes, robot_ground_truth),
            )?;
        }
        Cmd::Report { input, out_dir } => {
            let text = std::fs::read_to_string(&input)?;
            let recs: Vec<Record> = text
                .lines()
                .filter(|l| !l.is_empty())
                .map(serde_json::from_str)
                .collect::<Result<_, _>>()?;
            report::report(&recs, &out_dir)?;
        }
        Cmd::Fetch {
            out,
            api_key,
            limit,
            timeout,
            skip_existing,
        } => {
            let key = api_key
                .or_else(|| std::env::var("BIOPORTAL_API_KEY").ok())
                .context("BioPortal API key required: pass --api-key or set BIOPORTAL_API_KEY")?;
            fetch::fetch(&out, &key, limit, timeout, skip_existing)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::process::Command;

    fn temp_dir(name: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "horned-corpus-rev-test-{name}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn cli_override_replaces_the_whole_string() {
        // Not merely "takes priority": the override is recorded verbatim,
        // with no version prefix bolted on.
        let dir = temp_dir("override");
        assert_eq!(resolve_horned_owl_rev(&dir, Some("deadbeef")), "deadbeef");
    }

    #[test]
    fn falls_back_to_bare_version_outside_a_repository() {
        let dir = temp_dir("no-repo");
        assert_eq!(resolve_horned_owl_rev(&dir, None), horned_owl::VERSION);
    }

    #[test]
    fn reports_version_and_head_inside_a_repository() {
        let dir = temp_dir("repo");
        // git exports GIT_DIR and friends to hook subprocesses; left in
        // place they point these commands at the outer repository rather
        // than this throwaway one, and run its hooks too.
        let git = |args: &[&str]| {
            assert!(
                Command::new("git")
                    .env_remove("GIT_DIR")
                    .env_remove("GIT_INDEX_FILE")
                    .env_remove("GIT_WORK_TREE")
                    .env_remove("GIT_PREFIX")
                    .env_remove("GIT_CONFIG")
                    .arg("-C")
                    .arg(&dir)
                    .args(args)
                    .status()
                    .unwrap()
                    .success(),
                "git {args:?} failed"
            );
        };
        git(&["init", "-q"]);
        git(&["config", "user.email", "test@example.com"]);
        git(&["config", "user.name", "test"]);
        git(&["config", "core.hooksPath", "/dev/null"]);
        std::fs::write(dir.join("f"), "x").unwrap();
        git(&["add", "f"]);
        git(&["commit", "-q", "--no-verify", "-m", "c"]);

        let head = head_commit(&dir).expect("HEAD should resolve in a fresh repo");
        assert_eq!(
            resolve_horned_owl_rev(&dir, None),
            format!("{} ({head})", horned_owl::VERSION)
        );
    }
}
