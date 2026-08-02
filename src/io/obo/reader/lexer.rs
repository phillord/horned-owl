//! OBO flat-file format 1.4 lexer.
//!
//! The grammar files under `src/grammars/obo/` are vendored from
//! [`fastobo-syntax`](https://github.com/fastobo/fastobo-syntax) 0.8.1 by
//! Martin Larralde, used under the MIT licence (see `src/grammars/obo/LICENSE`).
//! They are vendored rather than taken as a crate dependency so the escaped
//! punctuation rule in xref IRIs can be relaxed locally (see issue #181) and so
//! horned-owl gains no new dependency (`pest`/`pest_derive` are already deps).
//!
//! Each `#[derive(Parser)]` type owns its own `Rule` enum, so these grammar
//! rules do not collide with the Manchester (`omn`) lexer's `bcp47`/`rfc3987`.

use pest::iterators::Pairs;
use pest_derive::Parser;

use crate::error::HornedError;

/// The OBO 1.4 lexer. Entry rule is [`Rule::OboDoc`].
#[derive(Debug, Parser)]
#[grammar = "grammars/obo/obo14.pest"]
#[grammar = "grammars/obo/bcp47.pest"]
#[grammar = "grammars/obo/iso8601.pest"]
#[grammar = "grammars/obo/rfc3987.pest"]
pub struct OboLexer;

impl OboLexer {
    /// Parse an input string using the given production rule.
    pub fn lex(rule: Rule, input: &str) -> Result<Pairs<'_, Rule>, HornedError> {
        <Self as pest::Parser<Rule>>::parse(rule, input).map_err(From::from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lexes(s: &str) -> bool {
        OboLexer::lex(Rule::OboDoc, s).is_ok()
    }

    #[test]
    fn lex_minimal_header() {
        assert!(lexes("format-version: 1.2\n"));
    }

    #[test]
    fn lex_term_stanza() {
        assert!(lexes(
            "format-version: 1.2\n\
             \n\
             [Term]\n\
             id: GO:0008150\n\
             name: biological_process\n\
             is_a: GO:0003674 ! molecular_function\n"
        ));
    }

    #[test]
    fn lex_instance_stanza() {
        // The vendored grammar already parses [Instance] frames (issue #181,
        // v1 must cover instances).
        assert!(lexes(
            "format-version: 1.2\n\
             \n\
             [Instance]\n\
             id: ex:i1\n\
             instance_of: ex:C1\n\
             property_value: ex:r ex:i2\n"
        ));
    }

    #[test]
    fn lex_typedef_stanza() {
        assert!(lexes(
            "format-version: 1.2\n\
             \n\
             [Typedef]\n\
             id: part_of\n\
             name: part of\n\
             is_transitive: true\n"
        ));
    }
}
