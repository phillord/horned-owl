use horned_roundtrip::model::Record;
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

    let text = std::fs::read_to_string(&jsonl).unwrap();
    let first_line = text
        .lines()
        .next()
        .expect("jsonl should have a header line");
    let first_rec: Record = serde_json::from_str(first_line).unwrap();
    match first_rec {
        Record::Header(h) => {
            assert_eq!(
                h.horned_owl_rev, "0a9debdbf85243350d3d6edc0dcd617f0ed47d97",
                "run header should carry the pinned horned-owl rev"
            );
        }
        other => panic!("expected first jsonl line to be Record::Header, got {other:?}"),
    }

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
