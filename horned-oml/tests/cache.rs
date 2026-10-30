//! An ontology that many others import is parsed for its terms once.

use std::cell::Cell;

use horned_oml::reader::{CachingResolver, Resolver};

struct Counting<'a>(&'a Cell<usize>);

impl Resolver for Counting<'_> {
    fn load(&self, _: &str) -> Option<String> {
        self.0.set(self.0.get() + 1);
        Some("vocabulary <http://example.com/a#> as a { concept A }".to_string())
    }
}

#[test]
fn parses_an_import_once() {
    let loads = Cell::new(0);
    let plain = Counting(&loads);
    plain.terms("http://example.com/a");
    plain.terms("http://example.com/a");
    assert_eq!(loads.get(), 2);

    loads.set(0);
    let cached = CachingResolver::new(Counting(&loads));
    let first = cached.terms("http://example.com/a").unwrap();
    let second = cached.terms("http://example.com/a").unwrap();
    assert_eq!(loads.get(), 1);
    assert_eq!(first.iri(), "http://example.com/a");
    assert!(std::rc::Rc::ptr_eq(&first, &second));
}

#[test]
fn remembers_what_could_not_be_found() {
    struct Nothing<'a>(&'a Cell<usize>);
    impl Resolver for Nothing<'_> {
        fn load(&self, _: &str) -> Option<String> {
            self.0.set(self.0.get() + 1);
            None
        }
    }
    let loads = Cell::new(0);
    let cached = CachingResolver::new(Nothing(&loads));
    assert!(cached.terms("http://example.com/b").is_none());
    assert!(cached.terms("http://example.com/b").is_none());
    assert_eq!(loads.get(), 1);
}

#[test]
fn takes_the_terms_of_a_document_already_read() {
    let loads = Cell::new(0);
    let cached = CachingResolver::new(Counting(&loads));
    let terms = cached
        .terms_of_text("vocabulary <http://example.com/c#> as c { concept C }")
        .unwrap();
    assert_eq!(terms.iri(), "http://example.com/c");
    cached.terms("http://example.com/c");
    assert_eq!(loads.get(), 0);
}
