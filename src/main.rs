use anyhow::Context;
use clap::{Parser, Subcommand};
use horned_roundtrip::model::{Format, Outcome, Record, RunHeader, SourceReadReport};
use horned_roundtrip::{corpus, fetch, report, roundtrip};
use std::io::Write;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

/// The horned-owl git rev this crate is pinned to (see Cargo.toml). Recorded in
/// every run's header so a report can be traced back to the exact horned-owl
/// commit that produced it.
const HORNED_OWL_REV: &str = "0a9debdbf85243350d3d6edc0dcd617f0ed47d97";

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

fn main() -> anyhow::Result<()> {
    match Cli::parse().cmd {
        Cmd::Run {
            corpus: dir,
            out,
            formats,
            jobs,
            max_bytes,
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
            let header = Record::Header(RunHeader {
                horned_owl_rev: HORNED_OWL_REV.to_string(),
                corpus: dir.to_string_lossy().to_string(),
                started: timestamp(),
            });
            let recs: Vec<Record> = {
                use rayon::prelude::*;
                paths
                    .par_iter()
                    .flat_map(|p| {
                        let name = p
                            .file_stem()
                            .unwrap_or_default()
                            .to_string_lossy()
                            .to_string();
                        match corpus::read_bytes(p) {
                            Ok(bytes) => {
                                if let Some(cap) = max_bytes {
                                    let len = bytes.len() as u64;
                                    if len > cap {
                                        return vec![Record::Source(SourceReadReport {
                                            ontology: name,
                                            source_format: Format::Unknown,
                                            outcome: Outcome::Skipped,
                                            is_complete: false,
                                            incomplete: None,
                                            error: Some(format!(
                                                "skipped: {len} bytes exceeds --max-bytes {cap}"
                                            )),
                                            read_us: None,
                                        })];
                                    }
                                }
                                // catch_unwind scope: a panic inside
                                // run_bytes (which includes canonicalize/
                                // diff/categorize, all of which run outside
                                // run_bytes's own internal catch_unwind
                                // calls) must not abort the whole
                                // par_iter().collect() -- record it as a
                                // single Outcome::Panic Source instead.
                                match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                                    roundtrip::run_bytes(&name, &bytes, &fmts)
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
                                }
                            }
                            Err(_) => vec![],
                        }
                    })
                    .collect()
            };
            let mut f = std::fs::File::create(&out)?;
            writeln!(f, "{}", serde_json::to_string(&header)?)?;
            for r in &recs {
                writeln!(f, "{}", serde_json::to_string(r)?)?;
            }
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
        } => {
            let key = api_key
                .or_else(|| std::env::var("BIOPORTAL_API_KEY").ok())
                .context("BioPortal API key required: pass --api-key or set BIOPORTAL_API_KEY")?;
            fetch::fetch(&out, &key, limit)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::HORNED_OWL_REV;

    /// Guards against reproducibility drift: `Cargo.toml`'s pinned
    /// horned-owl `rev` and this binary's `HORNED_OWL_REV` constant (recorded
    /// in every run's header) must always agree, or reports would claim a
    /// commit that isn't actually what was built against.
    #[test]
    fn horned_owl_rev_matches_cargo_toml() {
        let manifest_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml");
        let manifest = std::fs::read_to_string(&manifest_path)
            .unwrap_or_else(|e| panic!("reading {manifest_path:?}: {e}"));
        let rev_line = manifest
            .lines()
            .find(|l| l.contains("horned-owl") && l.contains("rev ="))
            .unwrap_or_else(|| panic!("no horned-owl rev = \"...\" line found in Cargo.toml"));
        let rev = rev_line
            .split("rev = \"")
            .nth(1)
            .and_then(|s| s.split('"').next())
            .unwrap_or_else(|| panic!("could not parse rev from line: {rev_line}"));
        assert_eq!(
            rev, HORNED_OWL_REV,
            "Cargo.toml's horned-owl rev ({rev}) no longer matches HORNED_OWL_REV \
             ({HORNED_OWL_REV}) in src/main.rs -- update the constant"
        );
    }
}
