use std::fmt::Display;
use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::ForIRI;
use crate::model::Component;
use crate::model::Kinded;
use crate::model::ComponentKind;
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;

mod as_functional;

pub use self::as_functional::AsFunctional;
pub use self::as_functional::Functional;

/// Write an Ontology to `write`, using the given `PrefixMapping`.
///
/// The ontology is written in OWL
/// [Functional-Style](https://www.w3.org/TR/2012/REC-owl2-syntax-20121211/)
/// syntax.
pub fn write<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<(), HornedError> {
    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => &default_mapper,
    };

    // Ensure we have a single OntologyID in the ontology.
    let optional_id = {
        let mut components = ont.i().component_for_kind(ComponentKind::OntologyID);
        let component = components.next(); 
        if components.next().is_some() {
            return Err(HornedError::invalid("multiple ontology IDs found"));
        }
        component.map(|c| 
            if let Component::OntologyID(ontology_id) = &c.component {
                ontology_id
            } else {
                unreachable!()
            }
        )
    };

    // Write prefixes
    write!(
        &mut write,
        "{}",
        <PrefixMapping as AsFunctional<A>>::as_functional(&mapping)
    )?;

    // Start the ontology element
    write!(write, "Ontology(")?;

    // Write the IRI and Version IRI if any
    if let Some(ontology_id) = optional_id { 
        if let Some(iri) = &ontology_id.iri {
            write!(write, "{}", iri.as_functional_with_prefixes(mapping))?;
            if let Some(viri) = &ontology_id.viri {
                writeln!(write, " {}", viri.as_functional_with_prefixes(mapping))?;
            } else {
                writeln!(write)?;
            }
        }
    }

    // Write axioms in order
    for kind in ComponentKind::all_kinds() {
        if kind != ComponentKind::OntologyID && kind != ComponentKind::DocIRI {
            let mut components = ont.i().component_for_kind(kind).collect::<Vec<_>>();
            components.sort();
            for component in components {
                writeln!(
                    &mut write,
                    "    {}",
                    component.as_functional_with_prefixes(mapping)
                )?;
            }
        }
    }

    // Close the ontology
    writeln!(write, ")").map_err(From::from)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::model::AnnotatedComponent;
    use crate::model::RcStr;
    use pretty_assertions::assert_eq;

    macro_rules! test_roundtrip {
        ($name:ident, $file:literal) => {
            #[test]
            fn $name() {
                let ont_s = include_str!(concat!("../../../ont/owl-functional/", $file));

                let reader = std::io::Cursor::new(&ont_s);
                let (ont, prefixes) =
                    crate::io::ofn::reader::read(reader, Default::default()).unwrap();

                let component_mapped: ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>> =
                    ont.clone().into();
                let mut writer = Vec::new();
                crate::io::ofn::writer::write(&mut writer, &component_mapped, Some(&prefixes))
                    .unwrap();

                let (ont2, prefixes2) = crate::io::ofn::reader::read(std::io::Cursor::new(&writer), Default::default())
                    .unwrap();

                assert_eq!(prefixes, prefixes2, "prefix mapping differ");
                assert_eq!(ont, ont2, "ontologies differ");
            }
        };
    }

    macro_rules! generate_tests {
        ( $( $name:ident ( $file:literal ) ),* ) => {
            $( test_roundtrip!($name, $file); )*
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
