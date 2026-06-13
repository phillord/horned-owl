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
        // Genuine garbage must NOT lex. (Note: `and and and` now DOES lex since
        // bare local names are accepted — it reads as the intersection of a class
        // literally named `and` with itself; that keyword/bare-name ambiguity is
        // the documented cost of bare-name support. A dangling operator is real
        // garbage regardless.)
        assert!(!lexes("<http://t/A> and")); // trailing operator, no operand
        assert!(!lexes("(<http://t/A>")); // unclosed parenthesis
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

    #[test]
    fn keyword_curie_collisions_do_not_misparse() {
        use crate::io::omn::reader::parse_class_expression;
        use crate::model::Build;
        let b = Build::new_rc();
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("notation", "http://ex/notation#").unwrap();
        pm.add_prefix("andro", "http://ex/andro#").unwrap();
        pm.add_prefix("somers", "http://ex/somers#").unwrap();

        // prefix `not` literally registered, so `not:Foo` is a valid CURIE.
        pm.add_prefix("not", "http://ex/not#").unwrap();

        // `notation:Foo` must parse as the atomic class, NOT `not` + `ation:Foo`
        // (keyword-prefix-of-name collision, closed by the `!SPARQL_PnChars` guard).
        let ce = parse_class_expression("notation:Foo", &pm, &b).unwrap();
        assert!(
            matches!(ce, crate::model::ClassExpression::Class(_)),
            "notation:Foo must be an atomic class, got {ce:?}"
        );
        // `not:Foo` must parse as the atomic class, NOT `not` + `:Foo`
        // (keyword-EQUALS-prefix collision, closed by also guarding `:`).
        let ce = parse_class_expression("not:Foo", &pm, &b).unwrap();
        assert!(
            matches!(ce, crate::model::ClassExpression::Class(_)),
            "not:Foo must be an atomic class, got {ce:?}"
        );
        // `andro:X and somers:Y` must be a 2-way intersection of two atomic classes.
        let ce = parse_class_expression("andro:X and somers:Y", &pm, &b).unwrap();
        match ce {
            crate::model::ClassExpression::ObjectIntersectionOf(v) => assert_eq!(v.len(), 2),
            other => panic!("expected intersection of 2, got {other:?}"),
        }
    }
}
