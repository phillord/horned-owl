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
use flate2::Compression;
use flate2::read::GzDecoder;
use flate2::write::GzEncoder;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};
use std::collections::HashMap;
use std::io::{Read, Write};
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
#[derive(Serialize, Deserialize)]
struct ManifestEntry {
    acronym: String,
    submission_id: Option<i64>,
    reported_language: Option<String>,
    stored_path: String,
    bytes: u64,
    sha256: String,
    /// When this ontology's bytes were actually last fetched from BioPortal
    /// (seconds since the Unix epoch) -- for calculating which files are
    /// stale later (e.g. `now - fetched_at > threshold`). This is the
    /// fetch time, not the time this manifest was written: a
    /// `--skip-existing` run that reuses an on-disk file (see
    /// `manifest_entry_from_disk`) carries the *original* fetch time
    /// forward rather than resetting it to now, so staleness tracking
    /// survives any number of skip-existing reruns. `#[serde(default)]`
    /// so a `manifest.json` written before this field existed still
    /// deserializes (as `None`); a fresh fetch or the mtime fallback in
    /// `manifest_entry_from_disk` backfills it going forward.
    #[serde(default)]
    fetched_at: Option<u64>,
}

/// Seconds since the Unix epoch, right now. Matches `main.rs`'s
/// `timestamp()` convention (plain epoch seconds, no `chrono` dependency)
/// -- kept as a separate small helper here rather than shared across the
/// bin/lib boundary, since it's a one-liner either way.
fn now_secs() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or(0)
}

/// `path`'s last-modified time, in epoch seconds -- the best available
/// staleness signal for a file with no prior manifest entry to carry a
/// `fetched_at` from at all (e.g. a corpus downloaded before this field
/// existed, since backfilled from disk once, not perfect but far better
/// than leaving `fetched_at` unknown forever).
fn mtime_secs(path: &Path) -> Option<u64> {
    std::fs::metadata(path)
        .and_then(|m| m.modified())
        .ok()
        .and_then(|t| t.duration_since(std::time::UNIX_EPOCH).ok())
        .map(|d| d.as_secs())
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
        fetched_at: Some(now_secs()),
    })
}

/// Rebuild a `<acronym>.gz` already on disk into a `ManifestEntry` without
/// any network access, so `--skip-existing` can skip a re-download while
/// still producing an accurate manifest. `submission_id`/`reported_language`
/// are carried over from `prior` (that ontology's entry in the manifest from
/// a previous run) when available, since recovering them requires the
/// submission-lookup request this path is meant to avoid.
///
/// `fetched_at` is carried over from `prior` too, and deliberately *not*
/// reset to now -- this file wasn't actually re-fetched, so its real fetch
/// time is whatever `prior` already recorded. When there's no `prior` entry
/// to carry a timestamp from at all (e.g. a corpus downloaded before this
/// field existed, manifest lost/rebuilt), falls back to the file's own
/// mtime as the best available signal.
fn manifest_entry_from_disk(
    acronym: &str,
    stored_path: &Path,
    prior: Option<&ManifestEntry>,
) -> Result<ManifestEntry> {
    let file = std::fs::File::open(stored_path)
        .with_context(|| format!("{acronym}: opening existing {stored_path:?}"))?;
    let mut bytes = Vec::new();
    GzDecoder::new(file)
        .read_to_end(&mut bytes)
        .with_context(|| format!("{acronym}: decompressing existing {stored_path:?}"))?;

    let mut hasher = Sha256::new();
    hasher.update(&bytes);
    let sha256 = format!("{:x}", hasher.finalize());

    let fetched_at = prior
        .and_then(|p| p.fetched_at)
        .or_else(|| mtime_secs(stored_path));

    Ok(ManifestEntry {
        acronym: acronym.to_string(),
        submission_id: prior.and_then(|p| p.submission_id),
        reported_language: prior.and_then(|p| p.reported_language.clone()),
        stored_path: stored_path.to_string_lossy().into_owned(),
        bytes: bytes.len() as u64,
        sha256,
        fetched_at,
    })
}

/// Under `--skip-existing`, decide whether the on-disk `<acronym>.gz` can be
/// reused: `Some(entry)` to reuse it, `None` to re-fetch. `None` covers both
/// "no file yet" and "file present but unreadable/truncated" (e.g. left by a
/// run killed mid-write) -- the latter must be re-downloaded, not skipped
/// forever, since an interrupted run is exactly when the last file is corrupt.
fn reusable_entry(
    acronym: &str,
    stored_path: &Path,
    prior: Option<&ManifestEntry>,
) -> Option<ManifestEntry> {
    if !stored_path.exists() {
        return None;
    }
    match manifest_entry_from_disk(acronym, stored_path, prior) {
        Ok(entry) => Some(entry),
        Err(err) => {
            eprintln!("{acronym}: existing file unusable ({err:#}), re-downloading");
            None
        }
    }
}

/// Download up to `limit` public ontologies from BioPortal into `out_dir`,
/// writing one `<acronym>.gz` per ontology plus a `manifest.json` describing
/// what was stored. Per-ontology failures are logged to stderr and skipped;
/// only failure to reach the `/ontologies` list itself is fatal.
///
/// `timeout_secs` bounds every request (list, submission lookup, download):
/// reqwest::blocking's default is 30s (unlike the async client, which has
/// none), and even the `include=all` ontology list alone is a multi-MB
/// response that routinely exceeds that against BioPortal.
///
/// When `skip_existing` is set, any ontology whose `<acronym>.gz` already
/// exists in `out_dir` is reused as-is (see `manifest_entry_from_disk`)
/// instead of re-fetched, so a rerun only downloads what's missing.
pub fn fetch(
    out_dir: &Path,
    api_key: &str,
    limit: Option<usize>,
    timeout_secs: u64,
    skip_existing: bool,
) -> Result<()> {
    std::fs::create_dir_all(out_dir)?;
    let client = reqwest::blocking::Client::builder()
        .timeout(Duration::from_secs(timeout_secs))
        .build()?;

    let prior_manifest: HashMap<String, ManifestEntry> = if skip_existing {
        std::fs::read(out_dir.join("manifest.json"))
            .ok()
            .and_then(|bytes| serde_json::from_slice::<Vec<ManifestEntry>>(&bytes).ok())
            .map(|entries| {
                entries
                    .into_iter()
                    .map(|e| (e.acronym.clone(), e))
                    .collect()
            })
            .unwrap_or_default()
    } else {
        HashMap::new()
    };

    let list: Value = get_with_retry(&client, &ontology_list_url(BASE_URL, api_key))?.json()?;
    let ontologies = list.as_array().cloned().unwrap_or_default();

    let mut manifest = Vec::new();
    for (i, ontology) in ontologies.iter().enumerate() {
        if let Some(l) = limit
            && i >= l
        {
            break;
        }
        let acronym = ontology
            .get("acronym")
            .and_then(|v| v.as_str())
            .unwrap_or("<unknown>");
        let stored_path = out_dir.join(format!("{acronym}.gz"));

        if skip_existing
            && let Some(entry) = reusable_entry(acronym, &stored_path, prior_manifest.get(acronym))
        {
            eprintln!("skipping {acronym}: already downloaded (--skip-existing)");
            manifest.push(entry);
            continue;
        }
        // else: no usable file on disk -- fall through and (re-)download.

        match fetch_one(&client, api_key, ontology, out_dir) {
            Ok(entry) => manifest.push(entry),
            Err(err) => eprintln!("skipping {acronym}: {err:#}"),
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

    fn tmp() -> std::path::PathBuf {
        let d = std::env::temp_dir().join(format!(
            "hrt-fetch-test-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        std::fs::create_dir_all(&d).unwrap();
        d
    }

    fn write_gz(path: &Path, contents: &[u8]) {
        let f = std::fs::File::create(path).unwrap();
        let mut enc = GzEncoder::new(f, Compression::default());
        enc.write_all(contents).unwrap();
        enc.finish().unwrap();
    }

    #[test]
    fn manifest_entry_from_disk_reads_valid_gz_and_carries_prior() {
        let dir = tmp();
        let p = dir.join("FOO.gz");
        write_gz(&p, b"hello ontology");
        let prior = ManifestEntry {
            acronym: "FOO".into(),
            submission_id: Some(42),
            reported_language: Some("OWL".into()),
            stored_path: "old".into(),
            bytes: 0,
            sha256: "old".into(),
            fetched_at: Some(1_000_000),
        };
        let e = manifest_entry_from_disk("FOO", &p, Some(&prior)).unwrap();
        assert_eq!(e.bytes, b"hello ontology".len() as u64);
        // sha256 is of the *uncompressed* bytes, matching fetch_one.
        let mut h = Sha256::new();
        h.update(b"hello ontology");
        assert_eq!(e.sha256, format!("{:x}", h.finalize()));
        assert_eq!(e.submission_id, Some(42)); // carried from prior
        assert_eq!(e.reported_language.as_deref(), Some("OWL"));
    }

    // The whole point of carrying fetched_at forward: reusing an on-disk
    // file under --skip-existing must not look like a fresh fetch just
    // because manifest_entry_from_disk ran again.
    #[test]
    fn manifest_entry_from_disk_preserves_original_fetched_at_not_now() {
        let dir = tmp();
        let p = dir.join("FOO.gz");
        write_gz(&p, b"hello ontology");
        let long_ago = 1_000_000; // 1970-01-12 -- nowhere near "now"
        let prior = ManifestEntry {
            acronym: "FOO".into(),
            submission_id: None,
            reported_language: None,
            stored_path: "old".into(),
            bytes: 0,
            sha256: "old".into(),
            fetched_at: Some(long_ago),
        };
        let e = manifest_entry_from_disk("FOO", &p, Some(&prior)).unwrap();
        assert_eq!(e.fetched_at, Some(long_ago));
    }

    // With no prior entry to carry a timestamp from at all (e.g. a corpus
    // predating this field, or a lost manifest), fall back to the file's
    // own mtime rather than leaving fetched_at unknown.
    #[test]
    fn manifest_entry_from_disk_falls_back_to_mtime_with_no_prior() {
        let dir = tmp();
        let p = dir.join("FOO.gz");
        write_gz(&p, b"hello ontology");
        let e = manifest_entry_from_disk("FOO", &p, None).unwrap();
        assert!(e.fetched_at.is_some());
        // Just-written file -> mtime should be very recent, not zero/bogus.
        let now = now_secs();
        let ts = e.fetched_at.unwrap();
        assert!(
            now.saturating_sub(ts) < 60,
            "expected a recent mtime, got {ts} vs now {now}"
        );
    }

    // fetch_one's success path (exercised indirectly, since fetch_one
    // itself needs real network access) is that fetched_at is a fresh
    // "now" timestamp, not carried from anywhere -- confirmed here for
    // now_secs itself, since fetch_one's own network-dependent test would
    // need the manual smoke test path documented in the module doc.
    #[test]
    fn now_secs_is_plausible_epoch_seconds() {
        // Sanity bound: some time after this was written (2026-ish), and
        // not an obviously-wrong value like 0 or something in the far
        // future from a unit mixup (e.g. accidentally returning millis).
        let ts = now_secs();
        assert!(ts > 1_700_000_000, "suspiciously small epoch seconds: {ts}");
        assert!(ts < 4_000_000_000, "suspiciously large epoch seconds: {ts}");
    }

    #[test]
    fn manifest_entry_from_disk_errors_on_truncated_gz() {
        // The exact failure an interrupted run leaves behind: bytes that
        // aren't a valid gzip stream. This is what makes a naive
        // skip-existing branch skip the file forever instead of repairing it.
        let dir = tmp();
        let p = dir.join("BAR.gz");
        std::fs::write(&p, b"not a gzip stream").unwrap();
        assert!(manifest_entry_from_disk("BAR", &p, None).is_err());
    }

    #[test]
    fn reusable_entry_routes_missing_and_corrupt_to_refetch() {
        let dir = tmp();

        // Missing file -> None (nothing to reuse; must fetch).
        let missing = dir.join("NONE.gz");
        assert!(reusable_entry("NONE", &missing, None).is_none());

        // Valid file -> Some (reuse, no re-download).
        let ok = dir.join("OK.gz");
        write_gz(&ok, b"content");
        assert!(reusable_entry("OK", &ok, None).is_some());

        // Corrupt/truncated file -> None. An interrupted run's leftover is
        // routed to re-fetch instead of skipped forever.
        let bad = dir.join("BAD.gz");
        std::fs::write(&bad, b"truncated").unwrap();
        assert!(reusable_entry("BAD", &bad, None).is_none());
    }
}
