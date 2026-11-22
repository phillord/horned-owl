use std::path::PathBuf;

use rstest::rstest;

use crate::model::RcStr;
use crate::ontology::set::SetOntology;

type Read = Result<(SetOntology<RcStr>, curie::PrefixMapping), crate::error::HornedError>;

fn nom_read(doc: &str) -> Read {
    super::read(doc.as_bytes(), Default::default())
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

    /// A component written before its entity's id is lost, so the id
    /// arriving afterwards is an error, not a silent drop.
    #[test]
    fn late_id_is_an_error() {
        use crate::model::{AnnotationAssertion, Component};
        let doc = "ontology: t\n\n[Term]\nid: T:1\nname: a\nsynonym: \"s\" RELATED []\n";
        let (ont, pm) = nom_read(doc).unwrap();
        let ont: ComponentMappedOntology<RcStr, Rc_> = ont.into();
        let is_id = |c: &Component<RcStr>| {
            matches!(c, Component::AnnotationAssertion(AnnotationAssertion { ann, .. })
                if ann.ap.0.as_ref().ends_with("#id"))
        };
        let mut comps: Vec<_> = component_stream(&ont).map(Result::unwrap).collect();
        // Ids last.
        comps.sort_by_key(|i| match i {
            crate::io::StreamComponent::Component(ac) => is_id(&ac.component),
            _ => false,
        });
        let items = prefix_stream(&pm).chain(comps.into_iter().map(Ok));
        let err = crate::io::obo::write_stream_chunked(Vec::new(), items, 1).unwrap_err();
        assert!(err.to_string().contains("oboInOwl:id"), "{err}");
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
