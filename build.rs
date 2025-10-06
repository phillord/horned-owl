fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    println!("cargo::rustc-check-cfg=cfg(bubo)");

    let mut bubo_which = std::process::Command::new("which");
    let bubo_which_mut = bubo_which.arg("bubo");
    match bubo_which_mut.status() {
        Ok(s) if s.success() => {
            println!("cargo::rustc-cfg=bubo");
            let out = bubo_which_mut.output().unwrap();
            let out = String::from_utf8(out.stdout).unwrap();

            println!("cargo::rustc-env=BUBO_LOCATION={}", out);
        }
        _ => {}
    }
}
