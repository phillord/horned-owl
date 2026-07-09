use std::process::Command;
#[test]
fn run_then_report_over_tiny_corpus() {
    let dir = std::env::temp_dir().join(format!("hrt-cli-{}", std::process::id()));
    std::fs::create_dir_all(dir.join("corpus")).unwrap();
    std::fs::write(
        dir.join("corpus/a.ofn"),
        b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)",
    )
    .unwrap();
    let jsonl = dir.join("r.jsonl");
    let ok = Command::new(env!("CARGO_BIN_EXE_horned-roundtrip"))
        .args(["run", "--corpus"])
        .arg(dir.join("corpus"))
        .arg("--out")
        .arg(&jsonl)
        .status()
        .unwrap()
        .success();
    assert!(ok);
    assert!(jsonl.exists());
    let ok2 = Command::new(env!("CARGO_BIN_EXE_horned-roundtrip"))
        .args(["report", "--in"])
        .arg(&jsonl)
        .arg("--out-dir")
        .arg(dir.join("rep"))
        .status()
        .unwrap()
        .success();
    assert!(ok2);
    assert!(dir.join("rep/cases.csv").exists());
}
