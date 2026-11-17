use std::collections::HashSet;
use std::path::PathBuf;

use rstest::rstest;

use crate::model::RcStr;
use crate::ontology::set::SetOntology;

type Read = Result<(SetOntology<RcStr>, curie::PrefixMapping), crate::error::HornedError>;

fn nom_read(doc: &str) -> Read {
    super::read(doc.as_bytes(), Default::default())
}

fn pest_read(doc: &str) -> Read {
    crate::io::obo::reader::read(&mut doc.as_bytes(), Default::default())
}

fn same(doc: &str) {
    match (nom_read(doc), pest_read(doc)) {
        (Ok((nom_ont, nom_pm)), Ok((pest_ont, pest_pm))) => {
            assert_eq!(
                nom_pm.mappings().collect::<HashSet<_>>(),
                pest_pm.mappings().collect::<HashSet<_>>()
            );
            pretty_assertions::assert_eq!(nom_ont, pest_ont, "on {doc}");
        }
        (Err(_), Err(_)) => {}
        (Ok(_), Err(e)) => panic!("nom accepted what pest rejects ({e}):\n{doc}"),
        (Err(e), Ok(_)) => panic!("nom rejected what pest accepts ({e}):\n{doc}"),
    }
}

/// Both readers must accept the same documents and build the same ontology.
#[rstest]
fn same_as_pest(#[files("src/ont/owl-obo/*.obo")] resource: PathBuf) {
    same(&slurp::read_all_to_string(&resource).unwrap());
}

struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n.max(1) as u64) as usize
    }
}

fn fixtures() -> Vec<String> {
    let mut docs = Vec::new();
    for dir in ["src/ont/owl-obo", "src/ont/owl-obo/ambiguous", "src/ont/obo"] {
        for entry in std::fs::read_dir(dir).unwrap() {
            let path = entry.unwrap().path();
            if path.extension().is_some_and(|e| e == "obo") {
                docs.push(slurp::read_all_to_string(&path).unwrap());
            }
        }
    }
    assert!(docs.len() > 50);
    docs
}

#[test]
fn other_fixtures_same_as_pest() {
    for doc in fixtures() {
        same(&doc);
    }
}

#[test]
fn crlf_same_as_pest() {
    for doc in fixtures() {
        same(&doc.replace('\n', "\r\n"));
    }
}

/// Fixtures with lines dropped, repeated or cut short, and with characters
/// that the grammar treats specially inserted: the readers must still agree.
#[test]
fn mutated_documents_same_as_pest() {
    const SPECIAL: &[&str] = &[
        "!",
        "{",
        "}",
        "\"",
        "\\",
        "[",
        "]",
        ",",
        ":",
        " ",
        "\t",
        "\r",
        "\n",
        "=",
        "http://a.b/c",
        "EXACT",
        "true",
        "xsd:string",
        "{a=\"b\"}",
        "[x]",
        "T",
        "-",
        "\u{e9}",
    ];
    // For a longer soak: OBO_NOM_MUTATIONS=2000 OBO_NOM_SEED=7.
    let rounds = std::env::var("OBO_NOM_MUTATIONS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(60);
    let seed = std::env::var("OBO_NOM_SEED")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
        .unwrap_or(0);
    let mut rng = Rng(0x0b0_5eed_c0ff_ee01 ^ seed.wrapping_mul(0x9e37_79b9_7f4a_7c15));
    let mut both_ok = 0;
    let mut both_err = 0;
    for doc in fixtures() {
        let lines: Vec<&str> = doc.lines().collect();
        for _ in 0..rounds {
            let mut v: Vec<String> = lines.iter().map(|l| l.to_string()).collect();
            for _ in 0..1 + rng.below(3) {
                if v.is_empty() {
                    break;
                }
                let i = rng.below(v.len());
                match rng.below(5) {
                    0 => {
                        v.remove(i);
                    }
                    1 => v.insert(i, v[i].clone()),
                    2 => {
                        let j = rng.below(v.len());
                        v.swap(i, j);
                    }
                    3 => {
                        let mut c = rng.below(v[i].len() + 1);
                        while !v[i].is_char_boundary(c) {
                            c -= 1;
                        }
                        v[i].truncate(c);
                    }
                    _ => {
                        let mut c = rng.below(v[i].len() + 1);
                        while !v[i].is_char_boundary(c) {
                            c -= 1;
                        }
                        let t = SPECIAL[rng.below(SPECIAL.len())];
                        v[i].insert_str(c, t);
                    }
                }
            }
            let text = v.join("\n") + if rng.below(8) == 0 { "" } else { "\n" };
            match (nom_read(&text), pest_read(&text)) {
                (Ok((n, _)), Ok((p, _))) => {
                    both_ok += 1;
                    pretty_assertions::assert_eq!(n, p, "on {text}");
                }
                (Err(_), Err(_)) => both_err += 1,
                (Ok(_), Err(e)) => panic!("nom accepted what pest rejects ({e}):\n{text:?}"),
                (Err(e), Ok(_)) => panic!("nom rejected what pest accepts ({e}):\n{text:?}"),
            }
        }
    }
    assert!(
        both_ok > 500 && both_err > 500,
        "{both_ok} ok, {both_err} err"
    );
}

/// Random documents built from clause tags and value tokens, valid and not.
#[test]
fn generated_documents_same_as_pest() {
    const HEADER_TAGS: &[&str] = &[
        "format-version:",
        "data-version:",
        "date:",
        "saved-by:",
        "auto-generated-by:",
        "import:",
        "subsetdef:",
        "synonymtypedef:",
        "default-namespace:",
        "idspace:",
        "namespace-id-rule:",
        "treat-xrefs-as-equivalent:",
        "treat-xrefs-as-genus-differentia:",
        "treat-xrefs-as-relationship:",
        "treat-xrefs-as-is_a:",
        "property_value:",
        "remark:",
        "ontology:",
        "owl-axioms:",
        "foo:",
        "x-y:",
    ];
    const TAGS: &[&str] = &[
        "is_anonymous:",
        "name:",
        "namespace:",
        "alt_id:",
        "def:",
        "comment:",
        "subset:",
        "synonym:",
        "exact_synonym:",
        "narrow_synonym:",
        "broad_synonym:",
        "related_synonym:",
        "xref:",
        "builtin:",
        "property_value:",
        "is_a:",
        "intersection_of:",
        "union_of:",
        "equivalent_to:",
        "disjoint_from:",
        "relationship:",
        "is_obsolete:",
        "replaced_by:",
        "consider:",
        "created_by:",
        "creation_date:",
        "domain:",
        "range:",
        "holds_over_chain:",
        "is_anti_symmetric:",
        "is_cyclic:",
        "is_reflexive:",
        "is_symmetric:",
        "is_asymmetric:",
        "is_transitive:",
        "is_functional:",
        "is_inverse_functional:",
        "inverse_of:",
        "transitive_over:",
        "equivalent_to_chain:",
        "disjoint_over:",
        "expand_assertion_to:",
        "expand_expression_to:",
        "is_metadata_tag:",
        "is_class_level:",
        "instance_of:",
        "bogus_tag:",
        "id:",
    ];
    const TOKENS: &[&str] = &[
        "\"text\"",
        "\"a \\\"q\\\" b\"",
        "\"unterminated",
        "\"\"",
        "GO:0001",
        "part_of",
        "R",
        "http://x.org/a",
        "http://x.org",
        "http://x.org/a?b#c",
        "[a, b]",
        "[x \"d\"]",
        "[]",
        "[a,]",
        "[a",
        "EXACT",
        "BROAD",
        "NARROW",
        "RELATED",
        "SYN",
        "true",
        "false",
        "maybe",
        "xsd:string",
        "xsd:int",
        "2020-01-01T10:00:00Z",
        "2020-01-01",
        "20200101T1000",
        "2020-01-01T10:00:00+01:00",
        "01:02:2020",
        "10:30",
        "{",
        "}",
        "{a=\"b\"}",
        "{gci_relation=\"r\", gci_filler=\"GO:2\"}",
        "{ a=\"b\" , c=\"d\" }",
        "{a=b}",
        "!",
        "! comment",
        "\\",
        "\\n",
        "x y",
        ":",
        "a:b:c",
        "ex:i1",
        "\u{e9}t\u{e9}",
        "BFO:0000050",
    ];
    let seed = std::env::var("OBO_NOM_SEED")
        .ok()
        .and_then(|v| v.parse::<u64>().ok())
        .unwrap_or(0);
    let rounds = std::env::var("OBO_NOM_GENERATED")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1500);
    let mut rng = Rng(0x6e6f_6d5f_6f62_6f31 ^ seed.wrapping_mul(0x9e37_79b9_7f4a_7c15));
    let value = |rng: &mut Rng| {
        let mut s = String::new();
        for _ in 0..rng.below(5) {
            s.push_str(TOKENS[rng.below(TOKENS.len())]);
            s.push_str(["", " ", " ", "  ", "\t"][rng.below(5)]);
        }
        s
    };
    let (mut both_ok, mut both_err) = (0, 0);
    for _ in 0..rounds {
        let mut doc = String::new();
        for _ in 0..rng.below(4) {
            doc.push_str(HEADER_TAGS[rng.below(HEADER_TAGS.len())]);
            doc.push(' ');
            doc.push_str(&value(&mut rng));
            doc.push('\n');
        }
        for _ in 0..1 + rng.below(4) {
            doc.push_str(["[Term]", "[Typedef]", "[Instance]", "[Term]"][rng.below(4)]);
            doc.push_str(["\n", "\n", " \n", "\r\n"][rng.below(4)]);
            if rng.below(6) == 0 {
                doc.push_str("! c\n");
            }
            doc.push_str(&format!(
                "id: {}\n",
                ["A", "GO:1", "part_of", "http://x.org/i", "ex:i1"][rng.below(5)]
            ));
            for _ in 0..rng.below(7) {
                doc.push_str(TAGS[rng.below(TAGS.len())]);
                doc.push(' ');
                doc.push_str(&value(&mut rng));
                doc.push('\n');
            }
            if rng.below(3) == 0 {
                doc.push('\n');
            }
        }
        match (nom_read(&doc), pest_read(&doc)) {
            (Ok((n, _)), Ok((p, _))) => {
                both_ok += 1;
                pretty_assertions::assert_eq!(n, p, "on {doc:?}");
            }
            (Err(_), Err(_)) => both_err += 1,
            (Ok(_), Err(e)) => panic!("nom accepted what pest rejects ({e}):\n{doc:?}"),
            (Err(e), Ok(_)) => panic!("nom rejected what pest accepts ({e}):\n{doc:?}"),
        }
    }
    assert!(
        both_ok > rounds / 10 && both_err > rounds / 10,
        "{both_ok} ok, {both_err} err"
    );
}

fn stream_all(
    doc: &str,
    capacity: usize,
) -> Vec<crate::io::StreamComponent<crate::model::AnnotatedComponent<RcStr>>> {
    let typedefs = super::stream::prescan(doc);
    let input = std::io::BufReader::with_capacity(capacity, doc.as_bytes());
    let mut items: Vec<_> = super::Reader::new(input, Default::default())
        .with_typedefs(typedefs)
        .map(|i| i.unwrap())
        .collect();
    // `owl-axioms:` comes back from a hash set, in any order.
    items.sort_by_cached_key(|i| format!("{i:?}"));
    items
}

/// What is found must not depend on how the input is cut into reads.
#[test]
fn chunked_same_as_whole() {
    for doc in fixtures() {
        if nom_read(&doc).is_err() {
            continue;
        }
        let whole = stream_all(&doc, 1 << 20);
        for capacity in [1, 7, 64] {
            assert_eq!(whole, stream_all(&doc, capacity), "capacity {capacity}");
        }
    }
}

/// A stream knows only the typedefs it has read: this shorthand is not
/// used as the `read` function would.
#[test]
fn streaming_sees_only_earlier_typedefs() {
    let doc = "ontology: t\n\n[Term]\nid: A\nrelationship: part_of B\n\n\
               [Typedef]\nid: part_of\nxref: BFO:0000050\n";
    let streamed: SetOntology<RcStr> = super::Reader::new(doc.as_bytes(), Default::default())
        .filter_map(|i| match i.unwrap() {
            crate::io::StreamComponent::Component(ac) => Some(ac),
            _ => None,
        })
        .collect();
    let (read, _) = nom_read(doc).unwrap();
    assert_ne!(streamed, read);
    // The pest reader reads the typedefs first, as `read` does.
    assert_eq!(read, pest_read(doc).unwrap().0);
}

/// A stanza keyword inside a quoted string that runs over lines is not the
/// end of the stanza.
#[test]
fn keyword_inside_quoted_string() {
    same("ontology: t\n\n[Term]\nid: A\ndef: \"one\n[Term]\nid: B\n two\" []\n");
}

mod writing {
    use super::*;
    use crate::io::{component_stream, prefix_stream};
    use crate::ontology::component_mapped::ComponentMappedOntology;

    type Rc_ = std::rc::Rc<crate::model::AnnotatedComponent<RcStr>>;

    fn load(path: &PathBuf) -> Option<(ComponentMappedOntology<RcStr, Rc_>, curie::PrefixMapping)> {
        let doc = slurp::read_all_to_string(path).unwrap();
        let (ont, pm) = nom_read(&doc).ok()?;
        Some((ont.into(), pm))
    }

    fn batch(ont: &ComponentMappedOntology<RcStr, Rc_>, pm: &curie::PrefixMapping) -> String {
        String::from_utf8(crate::io::obo::write(Vec::new(), ont, Some(pm)).unwrap()).unwrap()
    }

    /// With room for everything the streamed document is the batch one.
    #[rstest]
    fn one_chunk_same_as_write(#[files("src/ont/owl-obo/*.obo")] resource: PathBuf) {
        let Some((ont, pm)) = load(&resource) else {
            return;
        };
        let items = prefix_stream(&pm).chain(component_stream(&ont));
        let out = crate::io::obo::write_stream_chunked(Vec::new(), items, usize::MAX).unwrap();
        assert_eq!(batch(&ont, &pm), String::from_utf8(out).unwrap());
    }

    /// Fed in the order the reader gives, small chunks repeat stanzas,
    /// which must not change what is read.
    #[rstest]
    fn small_chunks_read_back_the_same(#[files("src/ont/owl-obo/*.obo")] resource: PathBuf) {
        let Some((ont, pm)) = load(&resource) else {
            return;
        };
        let (expected, _) = nom_read(&batch(&ont, &pm)).unwrap();
        let doc = slurp::read_all_to_string(&resource).unwrap();
        for chunk in [1, 2, 5] {
            let input = std::io::BufReader::new(doc.as_bytes());
            // The writer wants the header components first.
            let (mut items, rest): (Vec<_>, Vec<_>) =
                super::super::Reader::<RcStr, _, _>::new(input, Default::default())
                    .map(Result::unwrap)
                    .partition(|i| {
                        use crate::model::Component::*;
                        match i {
                            crate::io::StreamComponent::Prefix(..) => true,
                            crate::io::StreamComponent::Component(ac) => matches!(
                                ac.component,
                                OntologyID(_) | Import(_) | OntologyAnnotation(_)
                            ),
                        }
                    });
            items.extend(rest);
            let items = items.into_iter().map(Ok);
            let out = match crate::io::obo::write_stream_chunked(Vec::new(), items, chunk) {
                Ok(out) => out,
                // The header is out before a component with no OBO form
                // turns up: the batch writer puts these in `owl-axioms:`.
                Err(e) => {
                    assert!(
                        batch(&ont, &pm).contains("\nowl-axioms:"),
                        "chunk {chunk}: {e}"
                    );
                    continue;
                }
            };
            let text = String::from_utf8(out).unwrap();
            let (got, _) = nom_read(&text).unwrap_or_else(|e| panic!("chunk {chunk}: {e}\n{text}"));
            pretty_assertions::assert_eq!(expected, got, "chunk {chunk}:\n{text}");
        }
    }
}
