use pest::iterators::Pairs;
use pest_derive::Parser;

use crate::error::HornedError;

/// The OWL2 Functional-style Syntax lexer.
#[derive(Debug, Parser)]
#[grammar = "grammars/bcp47.pest"]
#[grammar = "grammars/rfc3987.pest"]
#[grammar = "grammars/sparql.pest"]
#[grammar = "grammars/ofn.pest"]
pub struct OwlFunctionalLexer;

impl OwlFunctionalLexer {
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

    macro_rules! test_lexer {
        ($name:ident, $file:literal) => {
            #[test]
            fn $name() {
                let ont_s = include_str!(concat!("../../../ont/owl-functional/", $file));
                match OwlFunctionalLexer::lex(Rule::OntologyDocument, ont_s.trim()) {
                    Ok(mut pairs) => assert_eq!(pairs.next().unwrap().as_str(), ont_s.trim()),
                    Err(e) => panic!("parser failed: {}", e),
                }
            }
        };
    }

    macro_rules! generate_tests {
        ( $( $name:ident ( $file:literal ) ),* ) => {
            $( test_lexer!($name, $file); )*
        }
    }

    generate_tests!(
        and_complex("and-complex.ofn"),
        and("and.ofn"),
        annotation_domain("annotation-domain.ofn"),
        annotation_on_complex_subclass("annotation-on-complex-subclass.ofn"),
        annotation_on_subclass("annotation-on-subclass.ofn"),
        annotation_on_transitive("annotation-on-transitive.ofn"),
        annotation_property("annotation-property.ofn"),
        annotation_range("annotation-range.ofn"),
        annotation_with_annotation("annotation-with-annotation.ofn"),
        annotation_with_non_builtin_annotation("annotation-with-non-builtin-annotation.ofn"),
        annotation("annotation.ofn"),
        annotation_assertion("annotation_assertion.ofn"),
        anon_subobjectproperty("anon-subobjectproperty.ofn"),
        class_assertion("class-assertion.ofn"),
        class("class.ofn"),
        class_with_two_annotations("class_with_two_annotations.ofn"),
        comment("comment.ofn"),
        complex_equivalent_classes("complex-equivalent-classes.ofn"),
        data_exact_cardinality("data-exact-cardinality.ofn"),
        data_has_key("data-has-key.ofn"),
        data_has_value("data-has-value.ofn"),
        data_max_cardinality("data-max-cardinality.ofn"),
        data_min_cardinality("data-min-cardinality.ofn"),
        data_only("data-only.ofn"),
        data_property_assertion("data-property-assertion.ofn"),
        data_property_disjoint("data-property-disjoint.ofn"),
        data_property_domain("data-property-domain.ofn"),
        data_property_equivalent("data-property-equivalent.ofn"),
        data_property_functional("data-property-functional.ofn"),
        data_property_range("data-property-range.ofn"),
        data_property_sub("data-property-sub.ofn"),
        data_property("data-property.ofn"),
        data_some("data-some.ofn"),
        data_unqualified_exact("data-unqualified-exact.ofn"),
        datatype_alias("datatype-alias.ofn"),
        datatype_complement("datatype-complement.ofn"),
        datatype_intersection("datatype-intersection.ofn"),
        datatype_oneof("datatype-oneof.ofn"),
        datatype_union("datatype-union.ofn"),
        datatype("datatype.ofn"),
        declaration_with_annotation("declaration-with-annotation.ofn"),
        declaration_with_two_annotation("declaration-with-two-annotation.ofn"),
        different_individual("different-individual.ofn"),
        disjoint_class("disjoint-class.ofn"),
        disjoint_object_properties("disjoint-object-properties.ofn"),
        disjoint_union("disjoint-union.ofn"),
        equivalent_class("equivalent-class.ofn"),
        equivalent_object_properties("equivalent-object-properties.ofn"),
        equivalent_classes("equivalent_classes.ofn"),
        facet_restriction_complex("facet-restriction-complex.ofn"),
        facet_restriction("facet-restriction.ofn"),
        gci_and_other_class_relations("gci_and_other_class_relations.ofn"),
        happy_person("happy_person.ofn"),
        import("import.ofn"),
        intersection("intersection.ofn"),
        inverse_properties("inverse-properties.ofn"),
        inverse_transitive("inverse-transitive.ofn"),
        label("label.ofn"),
        literal_escaped("literal-escaped.ofn"),
        multi_different_individual("multi-different-individual.ofn"),
        multi_has_key("multi-has-key.ofn"),
        multiple_ontology_annotation("multiple-ontology-annotation.ofn"),
        named_individual("named-individual.ofn"),
        negative_data_property_assertion("negative-data-property-assertion.ofn"),
        negative_object_property_assertion("negative-object-property-assertion.ofn"),
        not("not.ofn"),
        o10("o10.ofn"),
        object_exact_cardinality("object-exact-cardinality.ofn"),
        object_has_key("object-has-key.ofn"),
        object_has_self("object-has-self.ofn"),
        object_has_value("object-has-value.ofn"),
        object_max_cardinality("object-max-cardinality.ofn"),
        object_min_cardinality("object-min-cardinality.ofn"),
        object_one_of("object-one-of.ofn"),
        object_property_assertion("object-property-assertion.ofn"),
        object_property_asymmetric("object-property-asymmetric.ofn"),
        object_property_domain("object-property-domain.ofn"),
        object_property_functional("object-property-functional.ofn"),
        object_property_inverse_functional("object-property-inverse-functional.ofn"),
        object_property_irreflexive("object-property-irreflexive.ofn"),
        object_property_range("object-property-range.ofn"),
        object_property_reflexive("object-property-reflexive.ofn"),
        object_property_symmetric("object-property-symmetric.ofn"),
        object_unqualified_exact("object-unqualified-exact.ofn"),
        object_unqualified_max_cardinality("object-unqualified-max-cardinality.ofn"),
        one_ontology_annotation("one-ontology-annotation.ofn"),
        only("only.ofn"),
        ont_with_bfo("ont-with-bfo.ofn"),
        ont("ont.ofn"),
        ontology_annotation("ontology-annotation.ofn"),
        oproperty("oproperty.ofn"),
        or("or.ofn"),
        other_iri("other-iri.ofn"),
        other("other.ofn"),
        recursing_class("recursing_class.ofn"),
        same_individual("same-individual.ofn"),
        some_inverse("some-inverse.ofn"),
        some_not("some-not.ofn"),
        some("some.ofn"),
        sub_annotation("sub-annotation.ofn"),
        subclass("subclass.ofn"),
        suboproperty_inverse("suboproperty-inverse.ofn"),
        suboproperty_top("suboproperty-top.ofn"),
        suboproperty("suboproperty.ofn"),
        subproperty_chain_with_inverse("subproperty-chain-with-inverse.ofn"),
        subproperty_chain("subproperty-chain.ofn"),
        transitive_properties("transitive-properties.ofn"),
        two_annotation_on_transitive("two-annotation-on-transitive.ofn"),
        type_complex("type-complex.ofn"),
        type_individual_datatype_unqualified("type-individual-datatype-unqualified.ofn"),
        type_individual_datatype("type-individual-datatype.ofn"),
        typed_individual_datatype_unqualified("typed-individual-datatype-unqualified.ofn")
    );
}
