//! Regression test for phillord/horned-owl#292.
//!
//! RFC 3986 defines `path-empty = 0<pchar>`, where `0<pchar>` is ABNF for *zero*
//! repetitions, i.e. the empty string. Any IRI with an empty path — `<urn:>`,
//! `<mailto:>`, `<tel:>` — must therefore parse.

use horned_owl::io::ParserConfiguration;
use horned_owl::io::ofn::reader::read;
use horned_owl::model::RcStr;
use horned_owl::ontology::set::SetOntology;

fn parses(iri: &str) -> bool {
    let src = format!("Prefix(:=<{iri}>)\nOntology( Declaration(Class(:A)) )\n");
    let r: Result<(SetOntology<RcStr>, _), _> =
        read(&mut src.as_bytes(), ParserConfiguration::default());
    r.is_ok()
}

#[test]
fn empty_path_iris_parse() {
    for iri in ["urn:", "mailto:", "tel:"] {
        assert!(parses(iri), "<{iri}> must parse (RFC 3986 path-empty)");
    }
}

#[test]
fn non_empty_path_iris_still_parse() {
    for iri in ["urn:x", "http://ex.org/", "http://ex.org/a/b#c", "mailto:a@b.org"] {
        assert!(parses(iri), "<{iri}> regressed");
    }
}

#[test]
fn malformed_iris_are_still_rejected() {
    for iri in ["", ":", "1http://ex.org/"] {
        assert!(!parses(iri), "<{iri}> must NOT parse");
    }
}
