//! Differential tests: the pest Manchester reader and the nom reader in
//! `horned-owl` must accept the same documents and build the same ontologies.
use std::collections::HashSet;
use std::path::PathBuf;

use horned_owl::model::{Build, RcStr};
use horned_owl::ontology::set::SetOntology;
use rstest::rstest;

type Read = Result<(SetOntology<RcStr>, curie::PrefixMapping), horned_owl::error::HornedError>;

fn nom_read(doc: &str) -> Read {
    horned_owl::io::omn::reader::read(doc.as_bytes(), Default::default())
}

fn pest_read(doc: &str) -> Read {
    horned_pest::omn::read(doc.as_bytes(), Default::default())
}

/// Both readers must accept the same documents and build the same ontology.
#[rstest]
fn same_as_pest(#[files("../src/ont/owl-manchester/*.omn")] resource: PathBuf) {
    let doc = slurp::read_all_to_string(&resource).unwrap();
    match (nom_read(&doc), pest_read(&doc)) {
        (Ok((nom_ont, nom_pm)), Ok((pest_ont, pest_pm))) => {
            assert_eq!(
                nom_pm.mappings().collect::<HashSet<_>>(),
                pest_pm.mappings().collect::<HashSet<_>>()
            );
            pretty_assertions::assert_eq!(nom_ont, pest_ont);
        }
        (Err(_), Err(_)) => {}
        (Ok(_), Err(e)) => panic!("nom accepted what pest rejects: {e}"),
        (Err(e), Ok(_)) => panic!("nom rejected what pest accepts: {e}"),
    }
}

/// Fixtures with lines dropped, repeated or cut short: the readers must
/// still agree, on what they accept and what they build.
#[test]
fn mutated_documents_same_as_pest() {
    let mut rng = grammar::Rng(0x5eed_0fac_c0ff_ee01);
    let mut both_ok = 0;
    let mut both_err = 0;
    for entry in std::fs::read_dir("../src/ont/owl-manchester").unwrap() {
        let path = entry.unwrap().path();
        if path.extension().is_none_or(|e| e != "omn") {
            continue;
        }
        let doc = slurp::read_all_to_string(&path).unwrap();
        let lines: Vec<&str> = doc.lines().collect();
        for _ in 0..40 {
            let mut v = lines.clone();
            for _ in 0..1 + rng.next() % 3 {
                if v.is_empty() {
                    break;
                }
                let i = (rng.next() % v.len() as u64) as usize;
                match rng.next() % 4 {
                    0 => {
                        v.remove(i);
                    }
                    1 => v.insert(i, v[i]),
                    2 => {
                        let j = (rng.next() % v.len() as u64) as usize;
                        v.swap(i, j);
                    }
                    _ => {
                        let cut = (rng.next() % (v[i].len() as u64 + 1)) as usize;
                        let mut c = cut;
                        while !v[i].is_char_boundary(c) {
                            c -= 1;
                        }
                        let s: &str = &v[i][..c];
                        v[i] = s;
                    }
                }
            }
            let text = v.join("\n");
            match (nom_read(&text), pest_read(&text)) {
                (Ok((n, _)), Ok((p, _))) => {
                    both_ok += 1;
                    pretty_assertions::assert_eq!(n, p, "on {text}");
                }
                (Err(_), Err(_)) => both_err += 1,
                (Ok(_), Err(e)) => panic!("nom accepted what pest rejects ({e}):\n{text}"),
                (Err(e), Ok(_)) => panic!("nom rejected what pest accepts ({e}):\n{text}"),
            }
        }
    }
    assert!(
        both_ok > 500 && both_err > 500,
        "{both_ok} ok, {both_err} err"
    );
}

fn build() -> Build<RcStr> {
    Build::new()
}

fn prefixes() -> curie::PrefixMapping {
    let mut pm = curie::PrefixMapping::default();
    pm.add_prefix("", "http://e.com/").unwrap();
    pm.add_prefix("ex", "http://x.com/").unwrap();
    pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
        .unwrap();
    pm
}

mod grammar {
    use super::*;

    /// A small deterministic generator, so failures reproduce.
    pub struct Rng(pub u64);

    impl Rng {
        pub fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }

        fn pick<'a, T>(&mut self, items: &'a [T]) -> &'a T {
            &items[(self.next() % items.len() as u64) as usize]
        }
    }

    const TOKENS: &[&str] = &[
        "A",
        "B",
        "p",
        "q",
        "dp",
        ":a",
        ":b",
        ":p",
        "ex:c",
        "ex:r",
        "xsd:integer",
        "xsd:string",
        "<http://e.com/z>",
        "<http://www.w3.org/2001/XMLSchema#int>",
        "some",
        "only",
        "value",
        "min",
        "max",
        "exactly",
        "Self",
        "not",
        "and",
        "or",
        "that",
        "inverse",
        "(",
        ")",
        "{",
        "}",
        "[",
        "]",
        ",",
        "1",
        "2",
        "-3",
        "2.5",
        "1.5f",
        "true",
        "\"x\"",
        "\"x\"@en",
        "\"1\"^^xsd:integer",
        ">=",
        "<",
        "length",
        "pattern",
        " ",
        " ",
        " ",
        " ",
        "_:b1",
        "\n",
        "Class",
        "or",
        "notation:Foo",
        "inverse(",
        "min 1",
        "some xsd:integer",
        "{1, 2}",
    ];

    /// Class expressions built from the grammar's own tokens in random
    /// order. Almost all are invalid, which tests rejection as well.
    #[test]
    fn class_expression_same_as_pest() {
        let pm = prefixes();
        let b = build();
        let mut rng = Rng(0x0123_4567_89ab_cdef);
        let mut accepted = 0;
        for _ in 0..60_000 {
            let mut s = String::new();
            for _ in 0..1 + rng.next() % 9 {
                s.push_str(rng.pick(TOKENS));
                if !rng.next().is_multiple_of(3) {
                    s.push(' ');
                }
            }
            let pest = horned_pest::omn::parse_class_expression(&s, &pm, &b);
            let nom = horned_owl::io::omn::reader::parse_class_expression(&s, &pm, &b);
            match (&pest, &nom) {
                (Ok(p), Ok(n)) => {
                    accepted += 1;
                    assert_eq!(p, n, "different result for {s:?}");
                }
                (Err(_), Err(_)) => {}
                _ => panic!("disagree on {s:?}: pest {pest:?}, nom {nom:?}"),
            }
        }
        assert!(accepted > 1000, "only {accepted} accepted");
    }
}
