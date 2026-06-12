use pest::iterators::Pairs;
use pest_derive::Parser;

use crate::error::HornedError;

/// The OWL Manchester Syntax lexer.
#[derive(Debug, Parser)]
#[grammar = "grammars/bcp47.pest"]
#[grammar = "grammars/rfc3987.pest"]
#[grammar = "grammars/sparql.pest"]
#[grammar = "grammars/omn.pest"]
pub struct ManchesterLexer;

impl ManchesterLexer {
    /// Parse an input string using the given production rule.
    pub fn lex(rule: Rule, input: &str) -> Result<Pairs<'_, Rule>, HornedError> {
        <Self as pest::Parser<Rule>>::parse(rule, input).map_err(From::from)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn lexes(s: &str) -> bool {
        ManchesterLexer::lex(Rule::ClassExpressionDocument, s).is_ok()
    }

    #[test]
    fn lex_class_expressions() {
        assert!(lexes("<http://t/A>"));
        assert!(lexes("<http://t/A> and <http://t/C>"));
        assert!(lexes("<http://t/A> or <http://t/C> and <http://t/D>"));
        assert!(lexes("(<http://t/A> or <http://t/C>) and <http://t/D>"));
        assert!(lexes("not <http://t/A>"));
        assert!(lexes("<http://t/r> some <http://t/A>"));
        assert!(lexes("<http://t/r> only (<http://t/A> or <http://t/C>)"));
        assert!(lexes("<http://t/r> min 2 <http://t/A>"));
        assert!(lexes("inverse (<http://t/r>) some <http://t/A>"));
        assert!(lexes("{ <http://t/a>, <http://t/b> }"));
        assert!(!lexes("and and and")); // garbage must NOT lex
    }
}
