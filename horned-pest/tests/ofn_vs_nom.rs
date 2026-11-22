//! Differential tests: the pest OFN reader and the nom reader in `horned-owl`
//! must accept the same documents and build the same ontologies.
use std::collections::HashSet;
use std::path::PathBuf;

use horned_owl::model::RcStr;
use horned_owl::ontology::set::SetOntology;
use rstest::rstest;

fn nom_read(
    doc: &str,
) -> Result<(SetOntology<RcStr>, curie::PrefixMapping), horned_owl::error::HornedError> {
    horned_owl::io::ofn::reader::read(&mut doc.as_bytes(), Default::default())
}

fn pest_read(
    doc: &str,
) -> Result<(SetOntology<RcStr>, curie::PrefixMapping), horned_owl::error::HornedError> {
    horned_pest::ofn::read(&mut doc.as_bytes(), Default::default())
}

/// The nom reader must produce exactly what the pest reader does.
#[rstest]
fn same_as_pest(#[files("../src/ont/owl-functional/*.ofn")] resource: PathBuf) {
    let doc = slurp::read_all_to_string(&resource).unwrap();
    let (nom_ont, nom_pm) = nom_read(&doc).unwrap_or_else(|e| panic!("nom failed: {e}"));
    let (pest_ont, pest_pm) = pest_read(&doc).unwrap();
    assert_eq!(
        nom_pm.mappings().collect::<HashSet<_>>(),
        pest_pm.mappings().collect::<HashSet<_>>()
    );
    pretty_assertions::assert_eq!(nom_ont, pest_ont);
}

/// Inputs that pest rejects must also be rejected, and the error must point
/// at the right place.
#[rstest]
#[case("Ontology(SubClassOf(<http://e.com/a>))", "line 1, column 37")]
#[case(
    "Ontology(\n  Foo(<http://e.com/a>))",
    "unknown or unsupported axiom `Foo`"
)]
#[case("Ontology(SubClassOf(x:a x:b))", "undefined prefix")]
#[case("Ontology(EquivalentClasses(<http://e.com/a>))", "at least two")]
#[case("Ontology(Declaration(Class(<http://e.com/a>))", "expected")]
#[case("Ontology() trailing", "end of input")]
#[case(
    "Ontology(DataSomeValuesFrom(<http://e.com/a> <http://e.com/b> <http://e.com/c>))",
    "unknown or unsupported axiom"
)]
#[case(
    "Ontology(SubClassOf(DataSomeValuesFrom(<http://e.com/a> <http://e.com/b> <http://e.com/c>) <http://e.com/d>))",
    "data property chaining"
)]
fn rejects(#[case] doc: &str, #[case] message: &str) {
    let nom = nom_read(doc).map(|_| ()).expect_err("nom should fail");
    assert!(pest_read(doc).is_err(), "pest accepted {doc}");
    let text = format!("{nom}");
    let debug = format!("{nom:?}");
    assert!(
        text.contains(message) || debug.contains(message),
        "error `{text}` / `{debug}` does not mention `{message}`"
    );
}

mod grammar {
    use horned_owl::io::ofn::reader::probe;
    use horned_pest::ofn::lexer::{OwlFunctionalLexer, Rule};

    /// A small deterministic generator, so failures reproduce.
    struct Rng(u64);

    impl Rng {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }

        fn pick<'a, T>(&mut self, items: &'a [T]) -> &'a T {
            &items[(self.next() % items.len() as u64) as usize]
        }
    }

    fn pest_matches(rule: Rule, text: &str) -> bool {
        OwlFunctionalLexer::lex(rule, text)
            .ok()
            .and_then(|mut p| p.next())
            .is_some_and(|p| p.as_str() == text)
    }

    #[test]
    fn iri_same_as_pest() {
        const TOKENS: &[&str] = &[
            "http:",
            "s:",
            "x-y+z.1:",
            "//",
            "/",
            "?",
            "#",
            "@",
            "[",
            "]",
            "::",
            ":",
            "1",
            "2",
            "25",
            "255",
            "4",
            "0",
            ".",
            "a",
            "f",
            "v",
            "v1.",
            "%41",
            "%4",
            "%",
            "-",
            "_",
            "~",
            "!",
            "$",
            "&",
            "'",
            "(",
            ")",
            "*",
            "+",
            ",",
            ";",
            "=",
            "é",
            "\u{a0}",
            "\u{9f}",
            "\u{e000}",
            "\u{10000}",
            "\u{1fffe}",
            "\u{fffe}",
            "\u{fdd0}",
            "\u{e0000}",
            "\u{e1000}",
            "\u{f0000}",
            "\u{100000}",
            "x",
            "1.2.3.4",
            "1.2.3.45",
            "::1",
            "1:2:3:4:5:6:7:8",
            "1::",
            " ",
            "\"",
            "{",
            "\\",
            "^",
            "|",
            "`",
            "<",
            "\t",
            "user:pw",
            "host",
            "example.com",
        ];
        let mut rng = Rng(0x9e3779b97f4a7c15);
        let mut accepted = 0;
        for n in 0..400_000 {
            let mut s = String::new();
            if n % 4 != 0 {
                s.push_str(rng.pick(&["http:", "s:", "urn:", "a1+-.:"]));
            }
            for _ in 0..1 + rng.next() % 7 {
                s.push_str(rng.pick(TOKENS));
            }
            let doc = format!("<{s}>");
            let pest = pest_matches(Rule::FullIRI, &doc);
            let ours = probe::full_iri_matches(&doc);
            assert_eq!(pest, ours, "disagree on {doc:?}: pest {pest}, nom {ours}");
            accepted += usize::from(pest);
        }
        // Both outcomes must be well represented for the comparison to mean anything.
        assert!(accepted > 20_000, "only {accepted} accepted");
        assert!(accepted < 380_000, "only {} rejected", 400_000 - accepted);
    }

    #[test]
    fn iri_known_cases() {
        for (s, ok) in [
            ("http://example.com/a#b", true),
            ("http://example.com", true),
            ("http:", true),
            ("urn:x:y", true),
            ("http://[::1]/", false),
            ("http://1.2.3.4/", true),
            ("http://1.2.3.45/", false),
            ("http://10.0.0.1/", true),
            ("a b:c", false),
            ("no-scheme", false),
            ("http://example.com/a#b#c", false),
            ("http://example.com/%zz", false),
        ] {
            let doc = format!("<{s}>");
            assert_eq!(pest_matches(Rule::FullIRI, &doc), ok, "pest on {doc}");
            let ours = probe::full_iri_matches(&doc);
            assert_eq!(ours, ok, "nom on {doc}");
        }
    }

    #[test]
    fn prefixed_name_same_as_pest() {
        const TOKENS: &[&str] = &[
            "a",
            "Z",
            "_",
            "-",
            ".",
            "0",
            "9",
            ":",
            "é",
            "\u{b7}",
            "\u{300}",
            "\u{2040}",
            "\u{d7}",
            "\u{10000}",
            " ",
            "%",
            "#",
            "(",
            "ex",
            "owl",
            "\u{2fef}",
            "\u{3000}",
        ];
        let mut rng = Rng(0x1234_5678_9abc_def1);
        for _ in 0..200_000 {
            let mut s = String::new();
            for _ in 0..1 + rng.next() % 7 {
                s.push_str(rng.pick(TOKENS));
            }
            if s.starts_with(' ') {
                continue; // our token parsers skip leading space; the rules do not
            }
            let pest = pest_matches(Rule::AbbreviatedIRI, &s);
            let ours = probe::prefixed_name_matches(&s);
            assert_eq!(pest, ours, "disagree on {s:?}: pest {pest}, nom {ours}");
        }
    }

    #[test]
    fn blank_node_same_as_pest() {
        const TOKENS: &[&str] = &["_:", "a", "0", ".", "-", "_", ":", "é", "\u{b7}", " ", "x"];
        let mut rng = Rng(0xdead_beef_cafe_f00d);
        for _ in 0..100_000 {
            let mut s = String::from(*rng.pick(&["_:", "_:", ""]));
            for _ in 0..1 + rng.next() % 6 {
                s.push_str(rng.pick(TOKENS));
            }
            if s.starts_with(' ') {
                continue;
            }
            let pest = pest_matches(Rule::AnonymousIndividual, &s);
            let ours = probe::blank_node_matches(&s);
            assert_eq!(pest, ours, "disagree on {s:?}: pest {pest}, nom {ours}");
        }
    }

    #[test]
    fn language_tag_same_as_pest() {
        const TOKENS: &[&str] = &[
            "en",
            "zh",
            "sgn",
            "abc",
            "Hans",
            "Latn",
            "scot",
            "scotland",
            "GB",
            "US",
            "419",
            "1996",
            "a",
            "b",
            "x",
            "i",
            "u",
            "y",
            "ab",
            "abcde",
            "abcdefgh",
            "abcdefghi",
            "12",
            "1",
            "9a",
            "oed",
            "min",
            "nan",
            "ami",
            "lojban",
            "-",
            "-",
            "-",
            "-",
            "BE",
            "FR",
            "default",
            " ",
            "_",
            "é",
        ];
        let mut rng = Rng(0x0bcd_47ba_dc0f_fee1);
        for _ in 0..300_000 {
            let mut s = String::new();
            for _ in 0..1 + rng.next() % 8 {
                s.push_str(rng.pick(TOKENS));
            }
            let doc = format!("@{s}");
            let pest = OwlFunctionalLexer::lex(Rule::LanguageTag, &doc)
                .ok()
                .and_then(|mut p| p.next())
                .map(|p| p.as_str().len() - 1);
            let ours = probe::language_tag_len(&s);
            assert_eq!(pest, ours, "disagree on {s:?}: pest {pest:?}, nom {ours:?}");
        }
    }
}
