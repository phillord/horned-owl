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

    fn lex_doc(s: &str) -> bool {
        ManchesterLexer::lex(Rule::ManchesterDocument, s).is_ok()
    }

    #[test]
    fn lex_documents() {
        assert!(lex_doc("Prefix: ex: <http://ex/>"));
        assert!(lex_doc("Prefix: : <http://ex/>")); // default prefix decl
        assert!(lex_doc("Ontology: <http://ex/o>"));
        assert!(lex_doc("Prefix: ex: <http://ex/>\nOntology: <http://ex/o>"));
        assert!(lex_doc("")); // empty document is valid
        assert!(lex_doc("Class: <http://ex/A>"));
        assert!(lex_doc(
            "Class: <http://ex/A>\n    SubClassOf: <http://ex/B>"
        ));
        assert!(lex_doc(
            "Class: <http://ex/A>\n    EquivalentTo: <http://ex/B>, <http://ex/C>"
        ));
        assert!(lex_doc(
            "ObjectProperty: <http://ex/r>\n    Characteristics: Functional\n    InverseOf: <http://ex/t>"
        ));
        assert!(lex_doc(
            "DataProperty: <http://ex/p>\n    Range: <http://ex/dt>"
        ));
        assert!(lex_doc(
            "AnnotationProperty: <http://ex/n>\n    Domain: <http://ex/A>"
        ));
        assert!(lex_doc(
            "Individual: <http://ex/a>\n    Types: <http://ex/A>\n    Facts: <http://ex/r> <http://ex/b>"
        ));
        assert!(lex_doc("Datatype: <http://ex/dt>"));
        // two frames in sequence
        assert!(lex_doc("Class: <http://ex/A>\nClass: <http://ex/B>"));
        // garbage must not lex
        assert!(!lex_doc("Class:"));
        assert!(!lex_doc("Frobnicate: <http://ex/A>"));
    }
}
