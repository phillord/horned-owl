//! Plain OWL 2 DL structural conformance checking.
//!
//! Combines six checks into one `ProfileReport` for `Profile::OWL2DL`:
//! undeclared-entity usage, illegal punning, reserved-vocabulary
//! declarations, non-simple-property usage, too-few-operand n-ary class/data
//! expressions, and role-hierarchy regularity (see `regularity.rs` for that
//! one's scope note).
//!
//! [`structural_violations`] is reused by `el`/`ql`/`rl`, since each of
//! those profiles requires OWL 2 DL conformance *plus* its own extra
//! restrictions, per spec.

use std::collections::BTreeSet;

use horned_owl::model::{
    AnnotatedComponent, AnnotationProperty, Class, ClassExpression, Component, DataProperty,
    DataRange, Datatype, ForIRI, IRI, NamedEntityKind, ObjectProperty, ObjectPropertyExpression,
    Ontology,
};
use horned_owl::visitor::immutable::{Visit, Walk};

use crate::declared::DeclaredEntities;
use crate::simple_property::SimplePropertyAnalysis;
use crate::{Profile, ProfileReport, Violation, regularity};

/// Checks `o` against plain OWL 2 DL's structural (global) restrictions.
pub fn check<A: ForIRI, O: Ontology<A>>(o: &O) -> ProfileReport<A> {
    let declared = DeclaredEntities::from_ontology(o);
    ProfileReport::new(Profile::OWL2DL, structural_violations(o, &declared))
}

/// Returns the same violations as [`check`], without the `Profile::OWL2DL`
/// wrapping.
///
/// For `el`/`ql`/`rl` to fold into their own reports alongside their
/// profile-specific violations.
pub(crate) fn structural_violations<A: ForIRI, O: Ontology<A>>(
    o: &O,
    declared: &DeclaredEntities<A>,
) -> Vec<Violation<A>> {
    let mut violations = Vec::new();
    violations.extend(undeclared_violations(o, declared));
    violations.extend(illegal_punning_violations(declared));
    violations.extend(reserved_vocabulary_violations(declared));
    violations.extend(simple_property_violations(o));
    violations.extend(too_few_operands_violations(o));
    violations.extend(builtin_datatype_definition_violations(o));
    violations.extend(regularity_violations(o));
    violations
}

struct EntityUsage<A: ForIRI> {
    classes: BTreeSet<IRI<A>>,
    object_properties: BTreeSet<IRI<A>>,
    data_properties: BTreeSet<IRI<A>>,
    annotation_properties: BTreeSet<IRI<A>>,
    datatypes: BTreeSet<IRI<A>>,
}

impl<A: ForIRI> EntityUsage<A> {
    /// An empty `EntityUsage`, with no IRIs recorded for any kind yet.
    fn new() -> Self {
        EntityUsage {
            classes: BTreeSet::new(),
            object_properties: BTreeSet::new(),
            data_properties: BTreeSet::new(),
            annotation_properties: BTreeSet::new(),
            datatypes: BTreeSet::new(),
        }
    }
}

impl<A: ForIRI> Visit<A> for EntityUsage<A> {
    /// Records `c`'s IRI as a used class.
    fn visit_class(&mut self, c: &Class<A>) {
        self.classes.insert(c.0.clone());
    }
    /// Records `p`'s IRI as a used object property.
    fn visit_object_property(&mut self, p: &ObjectProperty<A>) {
        self.object_properties.insert(p.0.clone());
    }
    /// Records `p`'s IRI as a used data property.
    fn visit_data_property(&mut self, p: &DataProperty<A>) {
        self.data_properties.insert(p.0.clone());
    }
    /// Records `p`'s IRI as a used annotation property.
    fn visit_annotation_property(&mut self, p: &AnnotationProperty<A>) {
        self.annotation_properties.insert(p.0.clone());
    }
    /// Records `d`'s IRI as a used datatype.
    fn visit_datatype(&mut self, d: &Datatype<A>) {
        self.datatypes.insert(d.0.clone());
    }
}

/// Returns every named class/object-property/data-property/
/// annotation-property/datatype IRI referenced anywhere in `o`.
///
/// Declarations are included -- walking a `DeclareClass` axiom visits the
/// class it declares too, which is harmless: it's trivially "declared".
/// Named individuals aren't collected -- OWL 2 DL doesn't require them to
/// be declared, unlike the other five entity kinds (see the absence of a
/// `UseOfUndeclaredNamedIndividual` violation).
fn collect_entity_usage<A: ForIRI, O: Ontology<A>>(o: &O) -> EntityUsage<A> {
    let mut walk = Walk::new(EntityUsage::new());
    for ac in o.iter() {
        walk.annotated_component(ac);
    }
    walk.into_visit()
}

/// Returns violations for every IRI used in `o` as a class, object
/// property, data property, annotation property, or datatype without a
/// matching declaration.
fn undeclared_violations<A: ForIRI, O: Ontology<A>>(
    o: &O,
    declared: &DeclaredEntities<A>,
) -> Vec<Violation<A>> {
    let usage = collect_entity_usage(o);
    let mut violations = Vec::new();

    for iri in &usage.classes {
        if !declared.is_declared_as(iri, NamedEntityKind::Class) {
            violations.push(Violation::UseOfUndeclaredClass { iri: iri.clone() });
        }
    }
    for iri in &usage.object_properties {
        if !declared.is_declared_as(iri, NamedEntityKind::ObjectProperty) {
            violations.push(Violation::UseOfUndeclaredObjectProperty { iri: iri.clone() });
        }
    }
    for iri in &usage.data_properties {
        if !declared.is_declared_as(iri, NamedEntityKind::DataProperty) {
            violations.push(Violation::UseOfUndeclaredDataProperty { iri: iri.clone() });
        }
    }
    for iri in &usage.annotation_properties {
        if !declared.is_declared_as(iri, NamedEntityKind::AnnotationProperty) {
            violations.push(Violation::UseOfUndeclaredAnnotationProperty { iri: iri.clone() });
        }
    }
    for iri in &usage.datatypes {
        if !declared.is_declared_as(iri, NamedEntityKind::Datatype) {
            violations.push(Violation::UseOfUndeclaredDatatype { iri: iri.clone() });
        }
    }

    violations
}

/// Returns `UseOfIllegalPunning` violations for every illegally-punned IRI
/// in `declared`.
fn illegal_punning_violations<A: ForIRI>(declared: &DeclaredEntities<A>) -> Vec<Violation<A>> {
    declared
        .illegal_punnings()
        .into_iter()
        .map(|(iri, kinds)| Violation::UseOfIllegalPunning {
            iri,
            kinds: kinds.into_iter().map(kind_name).collect(),
        })
        .collect()
}

/// Returns the display name `Violation::UseOfIllegalPunning` uses for
/// `k`.
fn kind_name(k: NamedEntityKind) -> &'static str {
    match k {
        NamedEntityKind::Class => "Class",
        NamedEntityKind::Datatype => "Datatype",
        NamedEntityKind::ObjectProperty => "ObjectProperty",
        NamedEntityKind::DataProperty => "DataProperty",
        NamedEntityKind::AnnotationProperty => "AnnotationProperty",
        NamedEntityKind::NamedIndividual => "NamedIndividual",
        NamedEntityKind::Variable => "Variable",
    }
}

/// Returns `UseOfReservedVocabulary` violations for every reserved-vocabulary
/// IRI in `declared`.
fn reserved_vocabulary_violations<A: ForIRI>(declared: &DeclaredEntities<A>) -> Vec<Violation<A>> {
    declared
        .reserved_vocabulary_violations()
        .into_iter()
        .map(|iri| Violation::UseOfReservedVocabulary { iri })
        .collect()
}

/// Returns violations where a construct requiring a *simple* object
/// property is given a composite one instead.
///
/// `ObjectHasSelf`/cardinality-restriction/`DisjointObjectProperties`/
/// `Irreflexive`/`Asymmetric`/`Functional`/`InverseFunctionalObjectProperty`
/// may only use *simple* object properties -- see
/// [OWL 2 Structural Specification §11, "Global Restrictions on Axioms in OWL 2 DL"](https://www.w3.org/TR/owl2-syntax/#Global_Restrictions_on_Axioms_in_OWL_2_DL)
/// and `simple_property::SimplePropertyAnalysis`.
fn simple_property_violations<A: ForIRI, O: Ontology<A>>(o: &O) -> Vec<Violation<A>> {
    let analysis = SimplePropertyAnalysis::from_ontology(o);
    let mut violations = Vec::new();

    for ac in o.iter() {
        let usage = simple_property_usage(ac);
        for ope in &usage.has_self {
            if analysis.is_composite(ope) {
                violations.push(Violation::UseOfNonSimplePropertyInObjectHasSelf {
                    axiom: ac.clone(),
                    ope: ope.clone(),
                });
            }
        }
        for ope in &usage.cardinality {
            if analysis.is_composite(ope) {
                violations.push(Violation::UseOfNonSimplePropertyInCardinalityRestriction {
                    axiom: ac.clone(),
                    ope: ope.clone(),
                });
            }
        }

        match &ac.component {
            Component::DisjointObjectProperties(x) => {
                if x.0.iter().any(|ope| analysis.is_composite(ope)) {
                    violations.push(Violation::UseOfNonSimplePropertyInDisjointPropertiesAxiom {
                        axiom: ac.clone(),
                    });
                }
            }
            Component::IrreflexiveObjectProperty(x) if analysis.is_composite(&x.0) => {
                violations.push(
                    Violation::UseOfNonSimplePropertyInIrreflexivePropertyAxiom {
                        axiom: ac.clone(),
                    },
                );
            }
            Component::AsymmetricObjectProperty(x) if analysis.is_composite(&x.0) => {
                violations.push(Violation::UseOfNonSimplePropertyInAsymmetricPropertyAxiom {
                    axiom: ac.clone(),
                });
            }
            Component::FunctionalObjectProperty(x) if analysis.is_composite(&x.0) => {
                violations.push(Violation::UseOfNonSimplePropertyInFunctionalPropertyAxiom {
                    axiom: ac.clone(),
                });
            }
            Component::InverseFunctionalObjectProperty(x) if analysis.is_composite(&x.0) => {
                violations.push(
                    Violation::UseOfNonSimplePropertyInInverseFunctionalPropertyAxiom {
                        axiom: ac.clone(),
                    },
                );
            }
            _ => {}
        }
    }

    violations
}

struct SimplePropertyUsage<A: ForIRI> {
    has_self: Vec<ObjectPropertyExpression<A>>,
    cardinality: Vec<ObjectPropertyExpression<A>>,
}

impl<A: ForIRI> Visit<A> for SimplePropertyUsage<A> {
    /// Records `ce`'s object-property expression if it's an `ObjectHasSelf`
    /// or an object-cardinality restriction.
    fn visit_class_expression(&mut self, ce: &ClassExpression<A>) {
        match ce {
            ClassExpression::ObjectHasSelf(ope) => self.has_self.push(ope.clone()),
            ClassExpression::ObjectMinCardinality { ope, .. }
            | ClassExpression::ObjectMaxCardinality { ope, .. }
            | ClassExpression::ObjectExactCardinality { ope, .. } => {
                self.cardinality.push(ope.clone())
            }
            _ => {}
        }
    }
}

/// Returns every `ObjectHasSelf`/object-cardinality-restriction occurrence
/// within `ac`, however deeply nested inside its class expressions.
///
/// `Visit`'s `visit_class_expression` fires for every node `Walk` recurses
/// into, not just the top-level one.
fn simple_property_usage<A: ForIRI>(ac: &AnnotatedComponent<A>) -> SimplePropertyUsage<A> {
    let mut walk = Walk::new(SimplePropertyUsage {
        has_self: Vec::new(),
        cardinality: Vec::new(),
    });
    walk.annotated_component(ac);
    walk.into_visit()
}

/// Returns violations for n-ary class/data-range constructors given fewer
/// operands than OWL 2 DL requires.
///
/// Covers `ObjectUnionOf`/`ObjectIntersectionOf`/`DataUnionOf`/
/// `DataIntersectionOf`, each of which requires at least two operands --
/// see
/// [OWL 2 Structural Specification §8.1.2, "Union of Class Expressions"](https://www.w3.org/TR/owl2-syntax/#Union_of_Class_Expressions).
fn too_few_operands_violations<A: ForIRI, O: Ontology<A>>(o: &O) -> Vec<Violation<A>> {
    let mut violations = Vec::new();

    for ac in o.iter() {
        let usage = too_few_operands_usage(ac);
        for ce in usage.class_expressions {
            violations.push(Violation::UseOfClassExpressionWithTooFewOperands {
                axiom: ac.clone(),
                ce,
            });
        }
        for dr in usage.data_ranges {
            violations.push(Violation::UseOfDataRangeWithTooFewOperands {
                axiom: ac.clone(),
                dr,
            });
        }
    }

    violations
}

struct TooFewOperandsUsage<A: ForIRI> {
    class_expressions: Vec<ClassExpression<A>>,
    data_ranges: Vec<DataRange<A>>,
}

impl<A: ForIRI> Visit<A> for TooFewOperandsUsage<A> {
    /// Records `ce` if it's an under-sized `ObjectUnionOf`/
    /// `ObjectIntersectionOf`.
    fn visit_class_expression(&mut self, ce: &ClassExpression<A>) {
        match ce {
            ClassExpression::ObjectUnionOf(ops) | ClassExpression::ObjectIntersectionOf(ops)
                if ops.len() < 2 =>
            {
                self.class_expressions.push(ce.clone());
            }
            _ => {}
        }
    }
    /// Records `dr` if it's an under-sized `DataUnionOf`/
    /// `DataIntersectionOf`.
    fn visit_data_range(&mut self, dr: &DataRange<A>) {
        match dr {
            DataRange::DataUnionOf(ops) | DataRange::DataIntersectionOf(ops) if ops.len() < 2 => {
                self.data_ranges.push(dr.clone());
            }
            _ => {}
        }
    }
}

/// Returns every under-sized `ObjectUnionOf`/`ObjectIntersectionOf`/
/// `DataUnionOf`/`DataIntersectionOf` occurrence within `ac`, however
/// deeply nested.
///
/// Same `Walk`-per-axiom pattern as `simple_property_usage`.
fn too_few_operands_usage<A: ForIRI>(ac: &AnnotatedComponent<A>) -> TooFewOperandsUsage<A> {
    let mut walk = Walk::new(TooFewOperandsUsage {
        class_expressions: Vec::new(),
        data_ranges: Vec::new(),
    });
    walk.annotated_component(ac);
    walk.into_visit()
}

/// Returns violations for `DatatypeDefinition`s that redefine a genuine
/// built-in XSD/OWL 2 datatype.
///
/// `DatatypeDefinition` is for introducing a *new* custom datatype; using
/// it to redefine a built-in one is illegal -- see
/// [OWL 2 Structural Specification §9.4, "Datatype Definitions"](https://www.w3.org/TR/owl2-syntax/#Datatype_Definitions).
fn builtin_datatype_definition_violations<A: ForIRI, O: Ontology<A>>(o: &O) -> Vec<Violation<A>> {
    o.iter()
        .filter_map(|ac| match &ac.component {
            Component::DatatypeDefinition(x)
                if horned_owl::vocab::is_known_xsd_datatype(x.kind.0.as_ref()) =>
            {
                Some(Violation::UseOfBuiltinDatatypeInDatatypeDefinition { axiom: ac.clone() })
            }
            _ => None,
        })
        .collect()
}

/// Returns `UseOfPropertyInChainCausingCycle` violations for every cycle
/// `regularity::chain_cycles` finds in `o`'s role hierarchy.
fn regularity_violations<A: ForIRI, O: Ontology<A>>(o: &O) -> Vec<Violation<A>> {
    regularity::chain_cycles(o)
        .into_iter()
        .map(|cycle| Violation::UseOfPropertyInChainCausingCycle { cycle })
        .collect()
}

#[cfg(test)]
mod test {
    use super::check;
    use horned_owl::model::{Build, DeclareClass, MutableOntology, ObjectPropertyExpression};
    use horned_owl::ontology::set::SetOntology;

    #[test]
    fn fully_declared_ontology_is_conformant() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/C")));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    #[test]
    fn undeclared_class_in_subclassof_is_reported() {
        use horned_owl::model::{ClassExpression, SubClassOf};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let declared: ClassExpression<_> = b.class("http://example.com/A").into();
        let undeclared: ClassExpression<_> = b.class("http://example.com/B").into();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(SubClassOf {
            sup: undeclared,
            sub: declared,
        });

        let report = check(&o);
        assert!(!report.is_conformant());
        assert_eq!(report.violations().len(), 1);
    }

    #[test]
    fn builtin_and_xsd_vocabulary_need_no_declaration() {
        use horned_owl::model::{
            ClassExpression, DataPropertyRange, DataRange, Datatype, DeclareDataProperty,
            DeclareObjectProperty, ObjectPropertyDomain, ObjectPropertyExpression,
        };
        use horned_owl::vocab::OWL;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));

        // owl:Thing as a domain -- a built-in class, no DeclareClass needed
        // (p itself is declared, since that's not what's under test here).
        let p = b.object_property("http://example.com/p");
        o.insert(DeclareObjectProperty(p.clone()));
        let thing: ClassExpression<_> = b.class(OWL::Thing.as_ref()).into();
        o.insert(ObjectPropertyDomain {
            ope: ObjectPropertyExpression::ObjectProperty(p),
            ce: thing,
        });

        // xsd:string as a data-property range -- no DeclareDatatype needed
        // (dp itself is declared, same reasoning as p above).
        let dp = b.data_property("http://example.com/dp");
        o.insert(DeclareDataProperty(dp.clone()));
        o.insert(DataPropertyRange {
            dp,
            dr: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#string"))),
        });

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // owl:real/owl:rational need no DeclareDatatype axiom.
    #[test]
    fn owl_real_and_rational_need_no_declaration() {
        use horned_owl::model::{DataPropertyRange, DataRange, Datatype, DeclareDataProperty};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        for iri in ["http://www.w3.org/2002/07/owl#real", "http://www.w3.org/2002/07/owl#rational"]
        {
            let dp = b.data_property(format!("http://example.com/dp-{iri}"));
            o.insert(DeclareDataProperty(dp.clone()));
            o.insert(DataPropertyRange {
                dp,
                dr: DataRange::Datatype(Datatype(b.iri(iri))),
            });
        }

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // rdf:PlainLiteral/rdf:XMLLiteral need no DeclareDatatype axiom.
    #[test]
    fn rdf_plain_and_xml_literal_need_no_declaration() {
        use horned_owl::model::{DataPropertyRange, DataRange, Datatype, DeclareDataProperty};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        for iri in [
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral",
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral",
        ] {
            let dp = b.data_property(format!("http://example.com/dp-{iri}"));
            o.insert(DeclareDataProperty(dp.clone()));
            o.insert(DataPropertyRange {
                dp,
                dr: DataRange::Datatype(Datatype(b.iri(iri))),
            });
        }

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // owl:versionInfo (and the rest of vocab::is_annotation_builtin's set)
    // needs no DeclareAnnotationProperty axiom.
    #[test]
    fn owl_builtin_annotation_property_needs_no_declaration() {
        use horned_owl::model::{Annotation, AnnotationAssertion, AnnotationValue, Literal};
        use horned_owl::vocab::AnnotationBuiltIn;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(AnnotationAssertion {
            subject: b.iri("http://example.com/A").into(),
            ann: Annotation {
                ap: b.annotation_property(AnnotationBuiltIn::VersionInfo.as_ref()),
                av: AnnotationValue::Literal(Literal::Simple {
                    literal: "1.0".to_string(),
                }),
                ann: Default::default(),
            },
        });

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    #[test]
    fn class_datatype_punning_is_illegal() {
        use horned_owl::model::DeclareDatatype;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareDatatype(b.datatype("http://example.com/A")));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert_eq!(report.violations().len(), 1);
    }

    #[test]
    fn class_individual_punning_is_legal() {
        use horned_owl::model::DeclareNamedIndividual;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareNamedIndividual(
            b.named_individual("http://example.com/A"),
        ));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    #[test]
    fn object_and_data_property_punning_is_illegal() {
        use horned_owl::model::{DeclareDataProperty, DeclareObjectProperty};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareObjectProperty(
            b.object_property("http://example.com/p"),
        ));
        o.insert(DeclareDataProperty(b.data_property("http://example.com/p")));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert_eq!(report.violations().len(), 1);
    }

    // Declaring rdf:type -- a reserved RDF structural term with a fixed
    // meaning -- as an AnnotationProperty is illegal.
    #[test]
    fn declaring_rdf_type_as_annotation_property_is_illegal() {
        use horned_owl::model::DeclareAnnotationProperty;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareAnnotationProperty(b.annotation_property(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type",
        )));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfReservedVocabulary { .. }))
        );
    }

    // Redundantly declaring a *legitimately reusable* built-in (e.g.
    // owl:versionInfo, already legal without any declaration at all) must
    // not also be flagged as reserved-vocabulary misuse -- only genuinely
    // reserved *structural* vocabulary like rdf:type is illegal to declare.
    #[test]
    fn redeclaring_a_legitimate_builtin_is_not_reserved_vocabulary_misuse() {
        use horned_owl::model::DeclareAnnotationProperty;
        use horned_owl::vocab::AnnotationBuiltIn;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareAnnotationProperty(
            b.annotation_property(AnnotationBuiltIn::VersionInfo.as_ref()),
        ));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // Declaring an *invented* local name in the rdfs: namespace (not a real
    // RDFS term at all -- e.g. `rdfs:creator`, `rdfs:developer`,
    // `rdfs:SameAs`) is exactly as illegal as declaring a real one like
    // `rdfs:subClassOf`: the whole namespace is reserved, not just the
    // terms horned-owl happens to model as vocab enum variants.
    #[test]
    fn declaring_an_invented_name_in_a_reserved_namespace_is_illegal() {
        use horned_owl::model::DeclareAnnotationProperty;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareAnnotationProperty(b.annotation_property(
            "http://www.w3.org/2000/01/rdf-schema#creator",
        )));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfReservedVocabulary { .. }))
        );
    }

    // xsd:minInclusive/xsd:maxInclusive are real XSD terms, but *facet*
    // vocabulary, not datatypes -- declaring one as an AnnotationProperty
    // is exactly as illegal as the rdfs:creator case above. Also a
    // regression check for is_legitimately_reusable_builtin's loose
    // `vocab::is_xsd_datatype` namespace-prefix check silently carving
    // these back out (it matches any `xsd:`-prefixed IRI, not just real
    // datatypes).
    #[test]
    fn declaring_an_xsd_facet_term_as_annotation_property_is_illegal() {
        use horned_owl::model::DeclareAnnotationProperty;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareAnnotationProperty(b.annotation_property(
            "http://www.w3.org/2001/XMLSchema#minInclusive",
        )));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfReservedVocabulary { .. }))
        );
    }

    // A real XSD datatype itself must NOT be flagged as reserved-vocabulary
    // misuse when redundantly declared -- only non-datatype xsd: terms are
    // illegal to declare.
    #[test]
    fn redeclaring_a_real_xsd_datatype_is_not_reserved_vocabulary_misuse() {
        use horned_owl::model::DeclareDatatype;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareDatatype(
            b.datatype("http://www.w3.org/2001/XMLSchema#string"),
        ));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // Declaring an *invented/misspelled* local name in the xsd: namespace
    // as a Datatype -- xsd:'s entire purpose in OWL 2 is as a datatype
    // source, so this is fine even though it isn't a real XSD 1.0/1.1 type
    // (e.g. `xsd:datetimestamp`, wrong casing -- the real term is
    // `xsd:dateTimeStamp`), unlike the rdf:/rdfs:/owl: cases above.
    #[test]
    fn declaring_an_invented_xsd_local_name_as_datatype_is_not_reserved_vocabulary_misuse() {
        use horned_owl::model::DeclareDatatype;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareDatatype(
            b.datatype("http://www.w3.org/2001/XMLSchema#datetimestamp"),
        ));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // The same invented xsd: local name declared as a *different* kind
    // (AnnotationProperty, not Datatype) is exactly as illegal as the
    // rdf:/rdfs:/owl: cases -- xsd:'s Datatype-only exception is
    // kind-specific, not a blanket namespace carve-out.
    #[test]
    fn declaring_an_invented_xsd_local_name_as_annotation_property_is_illegal() {
        use horned_owl::model::DeclareAnnotationProperty;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareAnnotationProperty(b.annotation_property(
            "http://www.w3.org/2001/XMLSchema#datetimestamp",
        )));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfReservedVocabulary { .. }))
        );
    }

    // A composite (transitive) property used in ObjectHasSelf, or in
    // DisjointObjectProperties, must be flagged.
    #[test]
    fn non_simple_property_in_has_self_is_rejected() {
        use horned_owl::model::{ClassExpression, SubClassOf, TransitiveObjectProperty};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        let p: ObjectPropertyExpression<_> = b.object_property("http://example.com/p").into();
        o.insert(TransitiveObjectProperty(p.clone()));
        let has_self = ClassExpression::ObjectHasSelf(p);
        o.insert(SubClassOf {
            sup: has_self,
            sub: b.class("http://example.com/A").into(),
        });

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(report.violations().iter().any(|v| matches!(
            v,
            super::Violation::UseOfNonSimplePropertyInObjectHasSelf { .. }
        )));
    }

    #[test]
    fn non_simple_property_in_disjoint_object_properties_is_rejected() {
        use horned_owl::model::{
            DeclareObjectProperty, DisjointObjectProperties, TransitiveObjectProperty,
        };

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p_op = b.object_property("http://example.com/p");
        let q_op = b.object_property("http://example.com/q");
        o.insert(DeclareObjectProperty(p_op.clone()));
        o.insert(DeclareObjectProperty(q_op.clone()));
        let p: ObjectPropertyExpression<_> = p_op.into();
        let q: ObjectPropertyExpression<_> = q_op.into();
        o.insert(TransitiveObjectProperty(p.clone()));
        o.insert(DisjointObjectProperties(vec![p, q]));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(report.violations().iter().any(|v| matches!(
            v,
            super::Violation::UseOfNonSimplePropertyInDisjointPropertiesAxiom { .. }
        )));
    }

    // ObjectUnionOf with a single operand is structurally illegal in OWL 2
    // DL: it requires at least two operands.
    #[test]
    fn object_union_of_with_one_operand_is_rejected() {
        use horned_owl::model::{
            ClassExpression, DeclareObjectProperty, ObjectPropertyDomain, ObjectPropertyExpression,
        };

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        let p = b.object_property("http://example.com/p");
        o.insert(DeclareObjectProperty(p.clone()));
        let union_of_one =
            ClassExpression::ObjectUnionOf(vec![b.class("http://example.com/A").into()]);
        o.insert(ObjectPropertyDomain {
            ope: ObjectPropertyExpression::ObjectProperty(p),
            ce: union_of_one,
        });

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(report.violations().iter().any(|v| matches!(
            v,
            super::Violation::UseOfClassExpressionWithTooFewOperands { .. }
        )));
    }

    #[test]
    fn object_union_of_with_two_operands_is_conformant() {
        use horned_owl::model::{
            ClassExpression, DeclareObjectProperty, ObjectPropertyDomain, ObjectPropertyExpression,
        };

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareClass(b.class("http://example.com/B")));
        let p = b.object_property("http://example.com/p");
        o.insert(DeclareObjectProperty(p.clone()));
        let union_of_two = ClassExpression::ObjectUnionOf(vec![
            b.class("http://example.com/A").into(),
            b.class("http://example.com/B").into(),
        ]);
        o.insert(ObjectPropertyDomain {
            ope: ObjectPropertyExpression::ObjectProperty(p),
            ce: union_of_two,
        });

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    #[test]
    fn data_union_of_with_one_operand_is_rejected() {
        use horned_owl::model::{DataPropertyRange, DataRange, Datatype, DeclareDataProperty};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let dp = b.data_property("http://example.com/dp");
        o.insert(DeclareDataProperty(dp.clone()));
        let union_of_one = DataRange::DataUnionOf(vec![DataRange::Datatype(Datatype(
            b.iri("http://www.w3.org/2001/XMLSchema#string"),
        ))]);
        o.insert(DataPropertyRange {
            dp,
            dr: union_of_one,
        });

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfDataRangeWithTooFewOperands { .. }))
        );
    }

    #[test]
    fn simple_property_in_disjoint_object_properties_is_conformant() {
        use horned_owl::model::{DeclareObjectProperty, DisjointObjectProperties};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p_op = b.object_property("http://example.com/p");
        let q_op = b.object_property("http://example.com/q");
        o.insert(DeclareObjectProperty(p_op.clone()));
        o.insert(DeclareObjectProperty(q_op.clone()));
        let p: ObjectPropertyExpression<_> = p_op.into();
        let q: ObjectPropertyExpression<_> = q_op.into();
        o.insert(DisjointObjectProperties(vec![p, q]));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // A composite property declared FunctionalObjectProperty must be
    // flagged -- e.g. a property declared both TransitiveProperty and
    // FunctionalProperty.
    #[test]
    fn non_simple_property_in_functional_axiom_is_rejected() {
        use horned_owl::model::{
            DeclareObjectProperty, FunctionalObjectProperty, TransitiveObjectProperty,
        };

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p_op = b.object_property("http://example.com/p");
        o.insert(DeclareObjectProperty(p_op.clone()));
        let p: ObjectPropertyExpression<_> = p_op.into();
        o.insert(TransitiveObjectProperty(p.clone()));
        o.insert(FunctionalObjectProperty(p));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(report.violations().iter().any(|v| matches!(
            v,
            super::Violation::UseOfNonSimplePropertyInFunctionalPropertyAxiom { .. }
        )));
    }

    #[test]
    fn simple_property_in_functional_axiom_is_conformant() {
        use horned_owl::model::{DeclareObjectProperty, FunctionalObjectProperty};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p_op = b.object_property("http://example.com/p");
        o.insert(DeclareObjectProperty(p_op.clone()));
        let p: ObjectPropertyExpression<_> = p_op.into();
        o.insert(FunctionalObjectProperty(p));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // DatatypeDefinition redefining a genuine built-in XSD datatype is
    // illegal.
    #[test]
    fn datatype_definition_of_builtin_is_rejected() {
        use horned_owl::model::{DataRange, Datatype, DatatypeDefinition};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DatatypeDefinition {
            kind: Datatype(b.iri("http://www.w3.org/2001/XMLSchema#unsignedShort")),
            range: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#integer"))),
        });

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(report.violations().iter().any(|v| matches!(
            v,
            super::Violation::UseOfBuiltinDatatypeInDatatypeDefinition { .. }
        )));
    }

    // Defining a genuinely new, properly-declared custom datatype must not
    // be flagged.
    #[test]
    fn datatype_definition_of_custom_datatype_is_conformant() {
        use horned_owl::model::{DataRange, Datatype, DatatypeDefinition, DeclareDatatype};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareDatatype(Datatype(
            b.iri("http://example.com/MyDatatype"),
        )));
        o.insert(DatatypeDefinition {
            kind: Datatype(b.iri("http://example.com/MyDatatype")),
            range: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#integer"))),
        });

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }
}
