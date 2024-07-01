use pest::iterators::Pairs;
use pest_derive::Parser;

use crate::error::HornedError;

/// The OWL2 Functional-style Syntax lexer.
#[derive(Debug, Parser)]
#[grammar = "grammars/bcp47.pest"]
#[grammar = "grammars/rfc3987.pest"]
#[grammar = "grammars/sparql.pest"]
#[grammar = "grammars/omn.pest"]
pub struct OwlManchesterLexer;

impl OwlManchesterLexer {
    /// Parse an input string using the given production rule.
    ///
    /// This is basically a specialized version of [`pest::Parser::parse`]
    /// that only accepts [`Rule`], and does not need the `Parser` trait to
    /// be in scope.
    ///
    /// [`Rule`]: ./enum.Rule.html
    /// [`pest::Parser::parse`]: https://docs.rs/pest/latest/pest/trait.Parser.html
    pub fn lex(rule: Rule, input: &str) -> Result<Pairs<Rule>, HornedError> {
        <Self as pest::Parser<Rule>>::parse(rule, input).map_err(From::from)
    }
}

#[cfg(test)]
pub mod test {
    use super::*;

    use test_generator::test_resources;

    #[test_resources("src/ont/owl-manchester/*.omn")]
    fn lex_resource(resource: &str) {
        let ont_s = slurp::read_all_to_string(resource).unwrap();
        match OwlManchesterLexer::lex(Rule::OntologyDocument, ont_s.trim()) {
            Ok(mut pairs) => assert_eq!(pairs.next().unwrap().as_str(), ont_s.trim()),
            Err(e) => panic!("parser failed: {}", e),
        }
    }

    #[test]
    fn test_property_expression() {
        OwlManchesterLexer::lex(Rule::PropertyExpression, "o:r").unwrap();
    }

    #[test]
    fn test_has_key() {
        let _ = OwlManchesterLexer::lex(Rule::ClassHasKeyClause, "HasKey:\no:r\n").unwrap();
        let _ = OwlManchesterLexer::lex(Rule::ClassHasKeyClause, "HasKey:\no:r, o:s\n").unwrap();
    }
}
