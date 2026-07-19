use anyhow::Context;
use clap::{Parser, Subcommand};
use horned_roundtrip::model::{Format, Outcome, Record, RunHeader, SourceReadReport};
use horned_roundtrip::{corpus, fetch, report, roundtrip};
use std::io::Write;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

/// Determine the horned-owl commit this build was made against, for
/// provenance in the run header. Priority:
///   1. `--horned-owl-rev` on the CLI, if given -- a manual override for
///      when auto-detection can't find a real commit, or you want to
///      record something else on purpose.
///   2. Cargo.toml's `horned-owl = { path = "..." }`: run `git -C <path>
///      rev-parse HEAD` against that checkout directly. This is what
///      actually got built, unlike a hand-maintained constant that can
///      silently drift out of sync with Cargo.toml.
///   3. Cargo.toml's `horned-owl = { git = "...", rev = "..." }`: use the
///      pinned rev directly.
///   4. If none of the above resolve, "unknown" (with a warning to
///      stderr) rather than failing the run outright.
fn resolve_horned_owl_rev(manifest_dir: &std::path::Path, cli_override: Option<&str>) -> String {
    if let Some(r) = cli_override {
        return r.to_string();
    }
    match detect_horned_owl_rev(manifest_dir) {
        Some(r) => r,
        None => {
            eprintln!(
                "warning: could not determine the horned-owl commit this build used \
                 (no path or git+rev horned-owl dependency found in Cargo.toml, or \
                 `git rev-parse` failed) -- recording \"unknown\"; pass --horned-owl-rev \
                 to set it by hand"
            );
            "unknown".to_string()
        }
    }
}

/// Cargo.toml-driven half of [`resolve_horned_owl_rev`], kept separate (and
/// CLI-override-free) so it's directly testable.
fn detect_horned_owl_rev(manifest_dir: &std::path::Path) -> Option<String> {
    let manifest_path = manifest_dir.join("Cargo.toml");
    let manifest = std::fs::read_to_string(&manifest_path).ok()?;

    // Only active (non-commented) horned-owl dependency lines. A
    // commented-out `path = ...` line (the local-dev toggle in Cargo.toml)
    // must never shadow the real pinned `rev = ...` line below it, and a
    // `path` dep whose checkout can't be resolved must fall through to any
    // usable `rev` line rather than give up.
    for dep_line in manifest.lines().filter(|l| {
        !l.trim_start().starts_with('#')
            && l.contains("horned-owl")
            && (l.contains("path =") || l.contains("rev ="))
    }) {
        if let Some(path) = extract_quoted(dep_line, "path = \"") {
            let dep_dir = manifest_dir.join(&path);
            if let Ok(output) = std::process::Command::new("git")
                .arg("-C")
                .arg(&dep_dir)
                .args(["rev-parse", "HEAD"])
                .output()
            {
                if output.status.success() {
                    if let Ok(s) = String::from_utf8(output.stdout) {
                        return Some(s.trim().to_string());
                    }
                }
            }
            continue; // path unresolved -> keep scanning for a rev line
        }
        if let Some(rev) = extract_quoted(dep_line, "rev = \"") {
            return Some(rev);
        }
    }
    None
}

fn extract_quoted(line: &str, marker: &str) -> Option<String> {
    line.split(marker).nth(1)?.split('"').next().map(String::from)
}

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Round-trip every ontology in a corpus through the requested formats.
    Run {
        #[arg(long)]
        corpus: PathBuf,
        #[arg(long)]
        out: PathBuf,
        #[arg(long, default_value = "rdf,owx,ofn,omn")]
        formats: String,
        #[arg(long)]
        jobs: Option<usize>,
        /// Skip any corpus file whose byte length exceeds this cap, recording
        /// it as `Outcome::Skipped` instead of running it through the engine.
        #[arg(long = "max-bytes")]
        max_bytes: Option<u64>,
        /// Override the horned-owl commit recorded in the run header.
        /// Auto-detected by default (see `resolve_horned_owl_rev`); pass this
        /// to set it by hand when auto-detection can't or shouldn't be
        /// trusted -- e.g. a `path = "..."` dependency whose checkout is
        /// dirty/ahead of what you actually want recorded.
        #[arg(long = "horned-owl-rev")]
        horned_owl_rev: Option<String>,
    },
    /// Aggregate a run's JSONL output into a report directory.
    Report {
        #[arg(long = "in")]
        input: PathBuf,
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

fn main() -> anyhow::Result<()> {
    match Cli::parse().cmd {
        Cmd::Run {
            corpus: dir,
            out,
            formats,
            jobs,
            max_bytes,
            horned_owl_rev,
        } => {
            // catch_unwind in roundtrip::run_bytes recovers from per-file panics, but
            // the default panic hook still writes a message to stderr for each one.
            // Across ~1200 corpus files that floods the terminal, so install a no-op
            // hook for the duration of the run. This is process-wide and not restored
            // (the process exits right after `run` completes), which is fine for a
            // one-shot CLI binary but would need scoping if `run` ever became
            // reusable within a longer-lived process.
            std::panic::set_hook(Box::new(|_| {}));

            if let Some(j) = jobs {
                rayon::ThreadPoolBuilder::new()
                    .num_threads(j)
                    .build_global()
                    .ok();
            }
            let fmts = parse_formats(&formats);
            let paths = corpus::entries(&dir)?;
            let manifest_dir = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
            let header = Record::Header(RunHeader {
                horned_owl_rev: resolve_horned_owl_rev(manifest_dir, horned_owl_rev.as_deref()),
                corpus: dir.to_string_lossy().to_string(),
                started: timestamp(),
            });
            // Stream records to disk as each file completes, under a locked
            // writer. This keeps memory flat over a large corpus (each file's
            // records are written and dropped, never accumulated) and makes
            // partial progress durable if the sweep is interrupted or OOMs.
            let writer =
                std::sync::Mutex::new(std::io::BufWriter::new(std::fs::File::create(&out)?));
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
                    // Pre-read size gate: skip oversized files WITHOUT reading
                    // them into memory (a multi-hundred-MB ontology would spike
                    // RAM even if only to discard it). read_bytes still guards
                    // against a small gzip that inflates past the cap.
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
                                // A panic inside run_bytes (canonicalize/diff/
                                // categorize run outside run_bytes's own
                                // catch_unwind) must not abort the sweep --
                                // record it as one Outcome::Panic Source.
                                _ => match std::panic::catch_unwind(std::panic::AssertUnwindSafe(
                                    || roundtrip::run_bytes(&name, &bytes, &fmts),
                                )) {
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
    use std::path::Path;
    use std::process::Command;

    fn temp_dir(name: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!(
            "hrt-rev-test-{name}-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    fn write_manifest(dir: &Path, dep_line: &str) {
        std::fs::write(
            dir.join("Cargo.toml"),
            format!("[package]\nname = \"x\"\n[dependencies]\n{dep_line}\n"),
        )
        .unwrap();
    }

    #[test]
    fn cli_override_wins_over_detection() {
        // No Cargo.toml here at all -- proves the override short-circuits
        // detection entirely rather than merely taking priority when both work.
        let dir = temp_dir("override");
        assert_eq!(
            resolve_horned_owl_rev(&dir, Some("deadbeef")),
            "deadbeef"
        );
    }

    #[test]
    fn detects_rev_from_git_style_dependency() {
        let dir = temp_dir("git-dep");
        write_manifest(
            &dir,
            r#"horned-owl = { git = "https://example.com/horned-owl.git", rev = "abc123" }"#,
        );
        assert_eq!(detect_horned_owl_rev(&dir), Some("abc123".to_string()));
    }

    #[test]
    fn detects_rev_from_path_dependency_via_git_rev_parse() {
        let dir = temp_dir("path-dep");
        let sub = dir.join("horned-owl");
        std::fs::create_dir_all(&sub).unwrap();
        let git = |args: &[&str]| {
            assert!(
                Command::new("git")
                    .arg("-C")
                    .arg(&sub)
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
        std::fs::write(sub.join("f"), "x").unwrap();
        git(&["add", "f"]);
        git(&["commit", "-q", "-m", "c"]);
        let expected = String::from_utf8(
            Command::new("git")
                .arg("-C")
                .arg(&sub)
                .args(["rev-parse", "HEAD"])
                .output()
                .unwrap()
                .stdout,
        )
        .unwrap()
        .trim()
        .to_string();

        write_manifest(&dir, r#"horned-owl = { path = "./horned-owl" }"#);
        assert_eq!(detect_horned_owl_rev(&dir), Some(expected));
    }

    #[test]
    fn no_horned_owl_dependency_line_detects_nothing() {
        let dir = temp_dir("no-dep");
        write_manifest(&dir, r#"serde = "1""#);
        assert_eq!(detect_horned_owl_rev(&dir), None);
    }

    #[test]
    fn resolve_falls_back_to_unknown_when_detection_fails() {
        let dir = temp_dir("fallback");
        // No Cargo.toml, no override.
        assert_eq!(resolve_horned_owl_rev(&dir, None), "unknown");
    }

    #[test]
    fn commented_path_line_does_not_shadow_real_rev() {
        // Mirrors the real Cargo.toml: a commented-out `path = ...` dev line
        // directly above the pinned `git + rev` line. The commented line
        // must not be selected, and its unresolvable path must not suppress
        // detection of the real rev below it.
        let dir = temp_dir("commented-path");
        std::fs::write(
            dir.join("Cargo.toml"),
            "[dependencies]\n\
             # horned-owl = { path = \"./horned-owl\" }  # uncomment for local dev\n\
             horned-owl = { git = \"https://x/horned-owl.git\", rev = \"abc123\" }\n",
        )
        .unwrap();
        assert_eq!(detect_horned_owl_rev(&dir), Some("abc123".to_string()));
    }
}
