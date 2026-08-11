use horned_corpus::model::Record;
use std::process::Command;

fn tiny_corpus_dir(name: &str) -> std::path::PathBuf {
    let dir = std::env::temp_dir().join(format!("hrt-cli-{name}-{}", std::process::id()));
    std::fs::create_dir_all(dir.join("corpus")).unwrap();
    std::fs::write(
        dir.join("corpus/a.ofn"),
        b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)",
    )
    .unwrap();
    dir
}

fn header_of(jsonl: &std::path::Path) -> Record {
    let text = std::fs::read_to_string(jsonl).unwrap();
    let first_line = text
        .lines()
        .next()
        .expect("jsonl should have a header line");
    serde_json::from_str(first_line).unwrap()
}

#[test]
fn run_then_report_over_tiny_corpus() {
    let dir = tiny_corpus_dir("basic");
    let jsonl = dir.join("r.jsonl");
    let ok = Command::new(env!("CARGO_BIN_EXE_horned-corpus"))
        .args(["roundtrip", "--corpus"])
        .arg(dir.join("corpus"))
        .arg("--out")
        .arg(&jsonl)
        .status()
        .unwrap()
        .success();
    assert!(ok);
    assert!(jsonl.exists());

    // Checks shape, not a specific value: the header records
    // `<version> (<short commit>)`, or a bare version outside a repository
    // (see resolve_horned_owl_rev in src/main.rs, whose unit tests cover
    // both branches).
    match header_of(&jsonl) {
        Record::Header(h) => {
            assert!(
                h.horned_owl_rev.starts_with(horned_owl::VERSION),
                "header should start with the horned-owl version, got {:?}",
                h.horned_owl_rev
            );
        }
        other => panic!("expected first jsonl line to be Record::Header, got {other:?}"),
    }

    let ok2 = Command::new(env!("CARGO_BIN_EXE_horned-corpus"))
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

#[test]
fn run_with_explicit_horned_owl_rev_override() {
    let dir = tiny_corpus_dir("override");
    let jsonl = dir.join("r.jsonl");
    let ok = Command::new(env!("CARGO_BIN_EXE_horned-corpus"))
        .args(["roundtrip", "--corpus"])
        .arg(dir.join("corpus"))
        .arg("--out")
        .arg(&jsonl)
        .args(["--horned-owl-rev", "deadbeefcafe"])
        .status()
        .unwrap()
        .success();
    assert!(ok);

    match header_of(&jsonl) {
        Record::Header(h) => {
            assert_eq!(h.horned_owl_rev, "deadbeefcafe");
        }
        other => panic!("expected first jsonl line to be Record::Header, got {other:?}"),
    }
}
