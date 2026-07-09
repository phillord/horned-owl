//! BioPortal downloader: pulls the public ontology list from
//! `data.bioontology.org`, resolves each ontology's latest submission, and
//! stores the raw ontology bytes gzip-compressed alongside a `manifest.json`
//! describing what was fetched.
//!
//! Two helpers are pure and unit-tested with no network:
//! - `ontology_list_url` builds the `/ontologies` list endpoint.
//! - `should_retry` decides whether/how long to back off for a given HTTP
//!   status and attempt count.
//!
//! `fetch()` itself performs real network I/O against BioPortal and is
//! validated only by an optional manual smoke test (see the crate's task
//! notes); per-ontology failures are logged and skipped rather than
//! propagated, so one bad ontology never aborts the whole run.

use anyhow::{Context, Result};
use flate2::write::GzEncoder;
use flate2::Compression;
use serde::Serialize;
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::io::Write;
use std::path::Path;
use std::time::Duration;

/// BioPortal REST API base URL.
const BASE_URL: &str = "https://data.bioontology.org";

/// Build the URL for the authenticated `/ontologies` list endpoint. Includes
/// `include=all` (matching the Python reference implementation) so the list
/// response embeds each ontology's full attributes rather than a bare
/// summary.
pub fn ontology_list_url(base: &str, key: &str) -> String {
    format!("{base}/ontologies?include=all&apikey={key}")
}

/// Decide whether an HTTP response should be retried, and if so, how long to
/// back off first. Retries on 429 (rate limited) and any 5xx, with
/// exponential backoff, up to a fixed maximum number of attempts.
pub fn should_retry(status: u16, attempt: u32) -> Option<Duration> {
    const MAX: u32 = 5;
    if (status == 429 || (500..=599).contains(&status)) && attempt < MAX {
        Some(Duration::from_millis(500 * 2u64.pow(attempt))) // exponential backoff
    } else {
        None
    }
}

/// One entry in `manifest.json`: what was downloaded for a single ontology.
#[derive(Serialize)]
struct ManifestEntry {
    acronym: String,
    submission_id: Option<i64>,
    reported_language: Option<String>,
    stored_path: String,
    bytes: u64,
    sha256: String,
}

/// Append `apikey=<key>` to `url`, respecting any existing query string.
fn with_api_key(url: &str, key: &str) -> String {
    if url.contains('?') {
        format!("{url}&apikey={key}")
    } else {
        format!("{url}?apikey={key}")
    }
}

/// GET `url`, retrying on 429/5xx per `should_retry` and returning the first
/// successful (2xx) response. Non-retryable error statuses and transport
/// errors are surfaced as `Err`.
fn get_with_retry(
    client: &reqwest::blocking::Client,
    url: &str,
) -> Result<reqwest::blocking::Response> {
    let mut attempt = 0u32;
    loop {
        let resp = client
            .get(url)
            .send()
            .with_context(|| format!("GET {url}"))?;
        let status = resp.status();
        if status.is_success() {
            return Ok(resp);
        }
        match should_retry(status.as_u16(), attempt) {
            Some(backoff) => {
                eprintln!("GET {url} -> {status}; retrying in {backoff:?} (attempt {attempt})");
                std::thread::sleep(backoff);
                attempt += 1;
            }
            None => {
                anyhow::bail!("GET {url} failed with status {status}");
            }
        }
    }
}

/// Find a submission's download URL. BioPortal exposes it either directly on
/// the submission (`links.download`) or nested under the submission's
/// embedded ontology object (`ontology.links.download`), depending on the
/// `include` parameters used; check both.
fn submission_download_url(sub: &Value) -> Option<String> {
    sub.get("links")
        .and_then(|l| l.get("download"))
        .and_then(|v| v.as_str())
        .or_else(|| {
            sub.get("ontology")
                .and_then(|o| o.get("links"))
                .and_then(|l| l.get("download"))
                .and_then(|v| v.as_str())
        })
        .map(str::to_string)
}

/// Resolve and download a single ontology's latest submission, gzip it to
/// `<acronym>.gz` under `out_dir`, and return its manifest entry.
fn fetch_one(
    client: &reqwest::blocking::Client,
    api_key: &str,
    ontology: &Value,
    out_dir: &Path,
) -> Result<ManifestEntry> {
    let acronym = ontology
        .get("acronym")
        .and_then(|v| v.as_str())
        .context("ontology entry missing 'acronym'")?
        .to_string();

    let latest_submission_url = ontology
        .get("links")
        .and_then(|l| l.get("latest_submission"))
        .and_then(|v| v.as_str())
        .with_context(|| format!("{acronym}: missing links.latest_submission"))?;
    let submission: Value =
        get_with_retry(client, &with_api_key(latest_submission_url, api_key))?.json()?;

    let submission_id = submission.get("submissionId").and_then(Value::as_i64);
    let reported_language = submission
        .get("hasOntologyLanguage")
        .and_then(|v| v.as_str())
        .map(str::to_string);

    // The submission's own download link is preferred; fall back to the
    // ontology-list entry's `links.download` (same endpoint in practice).
    let download_url = submission_download_url(&submission)
        .or_else(|| {
            ontology
                .get("links")
                .and_then(|l| l.get("download"))
                .and_then(|v| v.as_str())
                .map(str::to_string)
        })
        .with_context(|| format!("{acronym}: no download URL found"))?;

    let bytes = get_with_retry(client, &with_api_key(&download_url, api_key))?
        .bytes()
        .with_context(|| format!("{acronym}: reading download body"))?;

    let file_name = format!("{acronym}.gz");
    let stored_path = out_dir.join(&file_name);
    {
        let file = std::fs::File::create(&stored_path)
            .with_context(|| format!("{acronym}: creating {stored_path:?}"))?;
        let mut encoder = GzEncoder::new(file, Compression::default());
        encoder.write_all(&bytes)?;
        encoder.finish()?;
    }

    let mut hasher = Sha256::new();
    hasher.update(&bytes);
    let sha256 = format!("{:x}", hasher.finalize());

    Ok(ManifestEntry {
        acronym,
        submission_id,
        reported_language,
        stored_path: stored_path.to_string_lossy().into_owned(),
        bytes: bytes.len() as u64,
        sha256,
    })
}

/// Download up to `limit` public ontologies from BioPortal into `out_dir`,
/// writing one `<acronym>.gz` per ontology plus a `manifest.json` describing
/// what was stored. Per-ontology failures are logged to stderr and skipped;
/// only failure to reach the `/ontologies` list itself is fatal.
pub fn fetch(out_dir: &Path, api_key: &str, limit: Option<usize>) -> Result<()> {
    std::fs::create_dir_all(out_dir)?;
    let client = reqwest::blocking::Client::builder().build()?;
    let list: Value = get_with_retry(&client, &ontology_list_url(BASE_URL, api_key))?.json()?;
    let ontologies = list.as_array().cloned().unwrap_or_default();

    let mut manifest = Vec::new();
    for (i, ontology) in ontologies.iter().enumerate() {
        if let Some(l) = limit {
            if i >= l {
                break;
            }
        }
        match fetch_one(&client, api_key, ontology, out_dir) {
            Ok(entry) => manifest.push(entry),
            Err(err) => {
                let acronym = ontology
                    .get("acronym")
                    .and_then(|v| v.as_str())
                    .unwrap_or("<unknown>");
                eprintln!("skipping {acronym}: {err:#}");
            }
        }
    }

    std::fs::write(
        out_dir.join("manifest.json"),
        serde_json::to_vec_pretty(&manifest)?,
    )?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn builds_list_url_with_key() {
        assert_eq!(
            ontology_list_url("https://data.bioontology.org", "K"),
            "https://data.bioontology.org/ontologies?include=all&apikey=K"
        );
    }
    #[test]
    fn retries_on_429_with_backoff() {
        assert!(should_retry(429, 0).is_some());
        assert!(should_retry(200, 0).is_none());
        assert!(should_retry(429, 10).is_none()); // give up after max attempts
    }
}
