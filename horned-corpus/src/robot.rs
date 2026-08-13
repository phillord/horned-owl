//! Ensures a pinned ROBOT release is available locally, downloading it to a
//! `dev/` cache directory on first use. Mirrors horned-owl's own
//! `bubo_ensure` (`src/io/mod.rs`): every `reason` / `--robot-ground-truth`
//! run in this crate should be pinned to a known ROBOT version, not
//! whatever (if anything) happens to be on `$PATH`.
//!
//! Downloads via `reqwest` (already a dependency, for `fetch`'s BioPortal
//! calls) rather than shelling out to `wget` or `curl` -- neither is stock
//! on Windows/macOS, and horned-corpus already pulls in one HTTP client
//! directly plus `ureq` transitively via horned-owl's `remote` feature;
//! reusing `reqwest` avoids adding a third.

use std::path::PathBuf;
use std::sync::OnceLock;

/// The ROBOT release this crate is validated against. Bump deliberately --
/// changing it changes every `reason` / `--robot-ground-truth` result.
const ROBOT_VERSION: &str = "1.9.10";

/// Ensure the pinned ROBOT jar exists at `dev/robot-<version>.jar`,
/// downloading it from the GitHub release if missing, and return its path.
pub fn robot_ensure() -> PathBuf {
    static ROBOT_PATH: OnceLock<PathBuf> = OnceLock::new();

    ROBOT_PATH
        .get_or_init(|| {
            // Anchored to the crate dir via CARGO_MANIFEST_DIR, not a
            // CWD-relative path -- unlike `cargo test` (always run from the
            // crate root), this binary is invoked from wherever a caller's
            // shell happens to be. A flat file directly under dev/ (not a
            // dev/robot-<version>/robot.jar subdirectory), matching
            // bubo_ensure's dev/bubo-0.4.0 convention exactly -- a nested
            // ignored subdirectory confuses `git ls-files --directory`'s
            // untracked-file detection (it reports the containing dev/ as
            // untracked even though its only content is gitignored).
            let local = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
                .join(format!("dev/robot-{ROBOT_VERSION}.jar"));

            if !local.exists() {
                let dir = local.parent().expect("robot jar path always has a parent");
                std::fs::create_dir_all(dir).expect("failed to create dev/");
                eprintln!("Downloading ROBOT {ROBOT_VERSION} from GitHub...");
                let url = format!(
                    "https://github.com/ontodev/robot/releases/download/v{ROBOT_VERSION}/robot.jar"
                );
                let bytes = reqwest::blocking::Client::builder()
                    .timeout(std::time::Duration::from_secs(120))
                    .build()
                    .and_then(|c| c.get(&url).send())
                    .and_then(reqwest::blocking::Response::error_for_status)
                    .and_then(|r| r.bytes())
                    .unwrap_or_else(|e| panic!("failed to download ROBOT {ROBOT_VERSION}: {e}"));

                // Written to a temp path and renamed into place, not written
                // directly to `local`: a run killed mid-download would
                // otherwise leave a truncated jar that the `local.exists()`
                // check above treats as already-downloaded on every
                // subsequent run.
                let tmp = local.with_extension("jar.part");
                std::fs::write(&tmp, &bytes)
                    .unwrap_or_else(|e| panic!("failed to write ROBOT {ROBOT_VERSION}: {e}"));
                std::fs::rename(&tmp, &local)
                    .unwrap_or_else(|e| panic!("failed to finalize ROBOT {ROBOT_VERSION}: {e}"));
            }

            local
        })
        .clone()
}

/// A `java -jar <robot.jar>` command ready for callers to append their own
/// `reason` / `validate-profile` / ... subcommand and args to.
///
/// The heap cap is fixed here rather than left to the JVM default (25% of
/// system RAM): four parallel `--jobs` workers each defaulting to several
/// GB pushed the corpus sweep into swap and got a worker OOM-killed.
pub fn robot_command() -> std::process::Command {
    let mut cmd = std::process::Command::new("java");
    cmd.arg("-Xmx4g").arg("-jar").arg(robot_ensure());
    cmd
}
