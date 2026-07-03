use std::os::unix::fs::PermissionsExt;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    let local = std::path::Path::new("dev/bubo-0.4.0");
    if !local.exists() {
        println!("cargo::warning=Downloading bubo 0.4.0 from GitHub...");
        let status = std::process::Command::new("wget")
            .args([
                "https://github.com/phillord/tawny-bubo/releases/download/0.4.0/bubo-0.4.0",
                "-O",
                "dev/bubo-0.4.0",
            ])
            .status()
            .expect("failed to run wget");
        assert!(status.success(), "failed to download bubo");

        std::fs::set_permissions(local, std::fs::Permissions::from_mode(0o755))
            .expect("failed to set bubo executable");
    }

    println!(
        "cargo::rustc-env=BUBO_LOCATION={}",
        local.canonicalize().unwrap().display()
    );
}
