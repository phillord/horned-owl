//! Diamond, cyclic and self imports: the closure is a set of documents.

use std::path::{Path, PathBuf};

use horned_owl::error::HornedError;
use horned_owl::io::closure_reader::{ClosureOntologyParser, read_to_closure};
use horned_owl::io::rdf::reader::ConcreteRcRDFOntology;
use horned_owl::io::rdf::reader::IncompleteParse;
use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::*;
use horned_owl::resolve::path_to_file_iri;

#[derive(Clone, Copy, Debug)]
enum Fmt {
    Ofn,
    Rdf,
}

impl Fmt {
    fn ext(self) -> &'static str {
        match self {
            Fmt::Ofn => "ofn",
            Fmt::Rdf => "owl",
        }
    }
}

fn config(b: &Build<RcStr>) -> RDFParserConfiguration<RcStr, &Build<RcStr>> {
    let mut config: RDFParserConfiguration<RcStr, &Build<RcStr>> =
        ParserConfiguration::new(b).into();
    config.common.local_only = true;
    config
}

/// A set of documents `a`, `b`, ... in one directory, each declaring its own
/// file IRI, and each declaring one class named after itself.
struct Docs {
    dir: tempfile::TempDir,
    build: Build<RcStr>,
    fmt: Fmt,
}

impl Docs {
    fn new(fmt: Fmt) -> Self {
        Docs {
            dir: tempfile::tempdir().unwrap(),
            build: Build::new_rc(),
            fmt,
        }
    }

    fn path(&self, name: &str) -> PathBuf {
        self.dir.path().join(format!("{name}.{}", self.fmt.ext()))
    }

    fn iri(&self, name: &str) -> String {
        path_to_file_iri(&self.build, &self.path(name)).to_string()
    }

    fn add(&self, name: &str, imports: &[&str]) {
        let me = self.iri(name);
        let imps: Vec<String> = imports.iter().map(|i| self.iri(i)).collect();
        let text = match self.fmt {
            Fmt::Ofn => {
                let imps: String = imps.iter().map(|i| format!("Import(<{i}>)\n")).collect();
                format!(
                    "Ontology(<{me}>\n{imps}Declaration(Class(<http://example.org/{name}>))\n)\n"
                )
            }
            Fmt::Rdf => {
                let imps: String = imps
                    .iter()
                    .map(|i| format!("<owl:imports rdf:resource=\"{i}\"/>"))
                    .collect();
                format!(
                    "<?xml version=\"1.0\"?>\n\
                     <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\" \
                     xmlns:owl=\"http://www.w3.org/2002/07/owl#\">\n\
                     <owl:Ontology rdf:about=\"{me}\">{imps}</owl:Ontology>\n\
                     <owl:Class rdf:about=\"http://example.org/{name}\"/>\n\
                     </rdf:RDF>\n"
                )
            }
        };
        std::fs::write(self.path(name), text).unwrap();
    }

    /// The names of the documents in the closure of `root`, in order, each
    /// with the number of class declarations it holds.
    fn closure(&self, root: &str) -> Result<Vec<(String, usize)>, HornedError> {
        let v: Vec<(ConcreteRcRDFOntology, _)> = run_bounded(self, root)?;
        Ok(v.into_iter()
            .map(|(o, _)| {
                let name = o
                    .i()
                    .the_ontology_id_or_default()
                    .iri
                    .map(|i| i.to_string())
                    .unwrap_or_default();
                let name = Path::new(&name)
                    .file_stem()
                    .unwrap()
                    .to_string_lossy()
                    .into_owned();
                (
                    name,
                    o.i()
                        .iter()
                        .filter(|c| {
                            let c: &AnnotatedComponent<RcStr> = std::borrow::Borrow::borrow(*c);
                            matches!(c.component, Component::DeclareClass(_))
                        })
                        .count(),
                )
            })
            .collect())
    }

    /// The declared IRIs that `parse_iri` reports for the closure of `root`.
    fn parse_iri(&self, root: &str) -> Vec<String> {
        let mut c: ClosureOntologyParser<RcStr, _, ConcreteRcRDFOntology, _> =
            ClosureOntologyParser::new(config(&self.build));
        c.parse_iri(&path_to_file_iri(&self.build, &self.path(root)), None)
            .unwrap()
            .into_iter()
            .map(|i| i.to_string())
            .collect()
    }
}

// A cycle that is not terminated fails here by timing out or overflowing the
// small stack, not by hanging the harness.
fn run_bounded(
    d: &Docs,
    root: &str,
) -> Result<Vec<(ConcreteRcRDFOntology, IncompleteParse<RcStr>)>, HornedError> {
    let root = path_to_file_iri(&d.build, &d.path(root));
    read_to_closure(&root, config(&d.build))
}

fn names(v: &[(String, usize)]) -> Vec<&str> {
    v.iter().map(|(n, _)| n.as_str()).collect()
}

fn both(f: fn(Fmt)) {
    for fmt in [Fmt::Ofn, Fmt::Rdf] {
        // A modest stack so unbounded recursion is a quick failure.
        std::thread::Builder::new()
            .stack_size(2 * 1024 * 1024)
            .spawn(move || f(fmt))
            .unwrap()
            .join()
            .unwrap_or_else(|_| panic!("{fmt:?}"));
    }
}

#[test]
fn diamond_contributes_once() {
    both(|fmt| {
        let d = Docs::new(fmt);
        d.add("a", &["b", "c"]);
        d.add("b", &["d"]);
        d.add("c", &["d"]);
        d.add("d", &[]);
        let v = d.closure("a").unwrap();
        assert_eq!(names(&v), ["a", "b", "d", "c"], "{fmt:?}");
        assert!(v.iter().all(|(_, n)| *n == 1), "{v:?}");
        let reported = d.parse_iri("a");
        assert_eq!(reported.len(), 4, "{reported:?}");
    });
}

#[test]
fn two_cycle() {
    both(|fmt| {
        let d = Docs::new(fmt);
        d.add("a", &["b"]);
        d.add("b", &["a"]);
        let v = d.closure("a").unwrap();
        assert_eq!(names(&v), ["a", "b"], "{fmt:?}");
        assert!(v.iter().all(|(_, n)| *n == 1), "{v:?}");
        // The same closure from the other end
        assert_eq!(names(&d.closure("b").unwrap()), ["b", "a"], "{fmt:?}");
    });
}

#[test]
fn self_import() {
    both(|fmt| {
        let d = Docs::new(fmt);
        d.add("a", &["a"]);
        let v = d.closure("a").unwrap();
        assert_eq!(names(&v), ["a"], "{fmt:?}");
        assert_eq!(v[0].1, 1);
    });
}

#[test]
fn longer_cycle_with_a_chord() {
    both(|fmt| {
        let d = Docs::new(fmt);
        d.add("a", &["b"]);
        d.add("b", &["c"]);
        d.add("c", &["d"]);
        d.add("d", &["a", "b"]);
        let v = d.closure("a").unwrap();
        assert_eq!(names(&v), ["a", "b", "c", "d"], "{fmt:?}");
        assert!(v.iter().all(|(_, n)| *n == 1), "{v:?}");
    });
}

#[test]
fn invalid_document_in_a_cycle_is_named() {
    both(|fmt| {
        let d = Docs::new(fmt);
        d.add("a", &["b"]);
        d.add("b", &["a", "c"]);
        d.add("c", &["a"]);
        let bad = match fmt {
            Fmt::Ofn => format!("Ontology(<{}> Import(<{}>) garbage", d.iri("c"), d.iri("a")),
            Fmt::Rdf => "<?xml version=\"1.0\"?><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"><unclosed>".to_string(),
        };
        std::fs::write(d.path("c"), bad).unwrap();
        let text = d.closure("a").unwrap_err().to_string();
        assert!(text.contains(&d.iri("c")), "{text}");
        assert!(!text.contains(&d.iri("b")), "{text}");
    });
}

#[test]
fn chain_is_unchanged() {
    both(|fmt| {
        let d = Docs::new(fmt);
        d.add("a", &["b"]);
        d.add("b", &["c"]);
        d.add("c", &[]);
        assert_eq!(names(&d.closure("a").unwrap()), ["a", "b", "c"], "{fmt:?}");
    });
}
