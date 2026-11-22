use std::collections::HashSet;
use std::path::PathBuf;

use rstest::rstest;

use crate::io::StreamComponent;
use crate::model::{AnnotatedComponent, Build, RcStr};
use crate::ontology::set::SetOntology;

type Read = Result<(SetOntology<RcStr>, curie::PrefixMapping), crate::error::HornedError>;

fn nom_read(doc: &str) -> Read {
    super::read(doc.as_bytes(), Default::default())
}

fn pest_read(doc: &str) -> Read {
    crate::io::omn::reader::read(doc.as_bytes(), Default::default())
}

/// Both readers must accept the same documents and build the same ontology.
#[rstest]
fn same_as_pest(#[files("src/ont/owl-manchester/*.omn")] resource: PathBuf) {
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

fn stream_all(doc: &str, chunk: usize) -> Vec<StreamComponent<AnnotatedComponent<RcStr>>> {
    let reader = super::Reader::new(doc.as_bytes(), Default::default()).with_chunk_size(chunk);
    reader.map(|i| i.unwrap()).collect()
}

/// The units found must not depend on how the input is cut into reads.
#[rstest]
fn chunked_same_as_whole(#[files("src/ont/owl-manchester/*.omn")] resource: PathBuf) {
    let doc = slurp::read_all_to_string(&resource).unwrap();
    if super::read::<_, _, SetOntology<RcStr>, _>(doc.as_bytes(), Default::default()).is_err() {
        return;
    }
    let whole = stream_all(&doc, 1 << 20);
    for chunk in [1, 7, 64] {
        assert_eq!(whole, stream_all(&doc, chunk), "chunk size {chunk}");
    }
}

/// Fixtures with lines dropped, repeated or cut short: the readers must
/// still agree, on what they accept and what they build.
#[test]
fn mutated_documents_same_as_pest() {
    let mut rng = grammar::Rng(0x5eed_0fac_c0ff_ee01);
    let mut both_ok = 0;
    let mut both_err = 0;
    for entry in std::fs::read_dir("src/ont/owl-manchester").unwrap() {
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
            let pest = crate::io::omn::reader::parse_class_expression(&s, &pm, &b);
            let nom = super::super::parse_class_expression(&s, &pm, &b);
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

mod writing {
    use super::*;
    use crate::io::{component_stream, prefix_stream};
    use crate::model::MutableOntology;
    use crate::ontology::component_mapped::ComponentMappedOntology;

    fn load(
        path: &PathBuf,
    ) -> Option<(ComponentMappedOntology<RcStr, RcStr_>, curie::PrefixMapping)> {
        let doc = slurp::read_all_to_string(path).unwrap();
        let (ont, pm): (SetOntology<RcStr>, _) = nom_read(&doc).ok()?;
        Some((ont.into(), pm))
    }

    type RcStr_ = std::rc::Rc<crate::model::AnnotatedComponent<RcStr>>;

    fn streamed(
        ont: &ComponentMappedOntology<RcStr, RcStr_>,
        pm: &curie::PrefixMapping,
        chunk: usize,
    ) -> String {
        let items = prefix_stream(pm).chain(component_stream(ont));
        let out = crate::io::omn::write_stream_chunked(Vec::new(), items, chunk).unwrap();
        String::from_utf8(out).unwrap()
    }

    /// With room for everything the streamed document is the batch one.
    #[rstest]
    fn one_chunk_same_as_write(#[files("src/ont/owl-manchester/*.omn")] resource: PathBuf) {
        let Some((ont, pm)) = load(&resource) else {
            return;
        };
        let batch = crate::io::omn::write(Vec::new(), &ont, Some(&pm)).unwrap();
        assert_eq!(
            String::from_utf8(batch).unwrap(),
            streamed(&ont, &pm, usize::MAX)
        );
    }

    /// Small chunks repeat frames, which must not change what is read.
    #[rstest]
    fn small_chunks_read_back_the_same(#[files("src/ont/owl-manchester/*.omn")] resource: PathBuf) {
        let Some((ont, pm)) = load(&resource) else {
            return;
        };
        let batch =
            String::from_utf8(crate::io::omn::write(Vec::new(), &ont, Some(&pm)).unwrap()).unwrap();
        let (expected, _) = nom_read(&batch).unwrap();
        for chunk in [1, 2, 5] {
            let text = streamed(&ont, &pm, chunk);
            let (got, _) = nom_read(&text).unwrap_or_else(|e| panic!("chunk {chunk}: {e}\n{text}"));
            pretty_assertions::assert_eq!(expected, got, "chunk {chunk}:\n{text}");
        }
    }

    /// A document written in memory can be read one item at a time, with no
    /// prescan: data properties and datatypes come before their uses.
    #[test]
    fn written_document_streams_back_the_same() {
        let ofn = "Prefix(:=<http://ex/>)\nPrefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)\nOntology(<http://ex/o>\n\
            Declaration(Class(:A))\nDeclaration(DataProperty(:p))\nDeclaration(Datatype(:dt))\n\
            DatatypeDefinition(:dt xsd:integer)\n\
            SubClassOf(:A DataSomeValuesFrom(:p :dt))\n\
            SubClassOf(:A DataAllValuesFrom(:p xsd:string))\n\
            HasKey(:A () (:p))\n)";
        let (ont, pm): (SetOntology<RcStr>, _) =
            crate::io::ofn::reader::read(&mut ofn.as_bytes(), Default::default()).unwrap();
        let cmo: ComponentMappedOntology<RcStr, RcStr_> = ont.clone().into();
        let text =
            String::from_utf8(crate::io::omn::write(Vec::new(), &cmo, Some(&pm)).unwrap()).unwrap();
        let mut got = SetOntology::new_rc();
        for item in super::super::Reader::new(text.as_bytes(), Default::default()) {
            if let StreamComponent::Component(ac) = item.unwrap() {
                got.insert(ac);
            }
        }
        pretty_assertions::assert_eq!(ont, got, "{text}");
    }

    fn orphan_assertion(
        b: &crate::model::Build<RcStr>,
        p: &str,
    ) -> crate::model::AnnotationAssertion<RcStr> {
        crate::model::AnnotationAssertion {
            subject: b.iri("http://ex/orphan").into(),
            ann: crate::model::Annotation {
                ap: b.annotation_property(p),
                av: crate::model::Literal::Simple {
                    literal: "v".to_string(),
                }
                .into(),
                ann: Default::default(),
            },
        }
    }

    /// A one-member DifferentIndividuals has no readable form; written after
    /// an orphan annotation assertion it must not take the general block with
    /// it.
    #[test]
    fn singleton_different_individuals_keeps_general_block() {
        use crate::model::*;
        let b = Build::new_rc();
        let assertion = orphan_assertion(&b, "http://ex/p");
        let one = |c: Component<RcStr>| {
            Ok(StreamComponent::Component(std::rc::Rc::new(
                AnnotatedComponent::from(c),
            )))
        };
        let items = vec![
            one(assertion.clone().into()),
            one(
                DifferentIndividuals(vec![Individual::Named(b.named_individual("http://ex/i"))])
                    .into(),
            ),
            one(SameIndividual(vec![Individual::Named(b.named_individual("http://ex/j"))]).into()),
        ];
        let out = crate::io::omn::write_stream_chunked(Vec::new(), items.into_iter(), 1).unwrap();
        let text = String::from_utf8(out).unwrap();
        let (got, _): (SetOntology<RcStr>, _) = nom_read(&text).unwrap();
        let comps: Vec<_> = got.iter().map(|ac| ac.component.clone()).collect();
        assert!(comps.contains(&assertion.into()), "{text}");
    }
}
