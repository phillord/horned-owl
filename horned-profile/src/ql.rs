//! OWL 2 QL profile.
//!
//! QL's class-expression grammar is deliberately asymmetric and much more
//! restrictive than EL's: subclass position allows almost nothing besides
//! an atomic class and an *unqualified* existential (`∃R.⊤`), while
//! intersection and (atomic) negation are only legal in superclass
//! position -- see [W3C OWL 2 Profiles §3](https://www.w3.org/TR/owl2-profiles/#OWL_2_QL).
//! `QL` conformance additionally requires OWL 2 DL conformance, per spec
//! (see [`check`]).
//!
//! **Scope note**: implements the class-expression grammar (sub/super
//! position, genuinely asymmetric here unlike EL) and the best-confidence
//! subset of QL's banned constructs: property chains (QL drops
//! FO-rewritability if it allows them), `TransitiveObjectProperty`,
//! `FunctionalObjectProperty`/`InverseFunctionalObjectProperty`/
//! `FunctionalDataProperty`, `HasKey`, and `DisjointUnion`. Data ranges are
//! restricted to named datatypes excluding a confirmed-rejected set (see
//! [`crate::datatypes`]) rather than a full grammar. **Not** checked, due
//! to genuine uncertainty rather than a deliberate scope decision: whether
//! `Disjoint{Object,Data}Properties`, `{A}symmetricObjectProperty`, and
//! `{Ir}reflexiveObjectProperty` are legal in QL -- recalled as
//! probably-legal from general spec knowledge, but not verified against
//! spec text or the OWL API source.

use horned_owl::model::{
    AnnotatedComponent, ClassExpression, Component, DataRange, ForIRI, Ontology,
    SubObjectPropertyExpression,
};

use crate::declared::DeclaredEntities;
use crate::owl2dl;
use crate::{Profile, ProfileReport, Violation};

/// Checks `o` against the OWL 2 QL profile (implies OWL 2 DL conformance).
pub fn check<A: ForIRI, O: Ontology<A>>(o: &O) -> ProfileReport<A> {
    let declared = DeclaredEntities::from_ontology(o);
    let mut violations = owl2dl::structural_violations(o, &declared);
    for ac in o.iter() {
        check_axiom(ac, &mut violations);
    }
    ProfileReport::new(Profile::QL, violations)
}

/// Checks `ac` against the QL profile, pushing any violations onto `out`.
fn check_axiom<A: ForIRI>(ac: &AnnotatedComponent<A>, out: &mut Vec<Violation<A>>) {
    match &ac.component {
        Component::SubClassOf(x) => {
            check_sub(&x.sub, ac, out);
            check_super(&x.sup, ac, out);
        }
        Component::EquivalentClasses(x) => {
            for ce in &x.0 {
                check_sub(ce, ac, out);
                check_super(ce, ac, out);
            }
        }
        Component::DisjointClasses(x) => {
            for ce in &x.0 {
                check_sub(ce, ac, out);
            }
        }
        // QL's ClassAssertion requires a strictly *atomic* class -- unlike
        // general superclass position, which permits a qualified
        // existential with a named-class filler (the MICRONT-derived
        // relaxation `is_ql_super_class_expression` otherwise applies).
        // EL/RL don't need the same special-casing: EL accepts the
        // qualified existential here too (consistent with its general
        // superclass grammar), and RL's general superclass grammar already
        // has no `ObjectSomeValuesFrom` arm at all, so it already rejects
        // this via the ordinary `check_super` path.
        Component::ClassAssertion(x) => {
            if !is_named_class(&x.ce) {
                out.push(Violation::UseOfNonSuperClassExpression {
                    axiom: ac.clone(),
                    ce: x.ce.clone(),
                });
            }
        }
        Component::ObjectPropertyDomain(x) => {
            check_super(&x.ce, ac, out);
        }
        Component::ObjectPropertyRange(x) => {
            check_super(&x.ce, ac, out);
        }
        Component::DataPropertyDomain(x) => {
            check_super(&x.ce, ac, out);
        }
        // Previously unchecked entirely -- `is_ql_data_range` existed and
        // was correct (used inside `DataSomeValuesFrom`'s grammar check),
        // but nothing dispatched to it for `DataPropertyRange`/
        // `DatatypeDefinition` axioms directly.
        Component::DataPropertyRange(x) => {
            if !is_ql_data_range(&x.dr) {
                out.push(illegal_dr_violation(ac));
            }
        }
        Component::DatatypeDefinition(x) => {
            if !is_ql_data_range(&x.range) {
                out.push(illegal_dr_violation(ac));
            }
        }
        // A literal's own datatype (from a `DataPropertyAssertion`) is
        // subject to the same restriction -- see `el.rs`'s equivalent arm.
        Component::DataPropertyAssertion(x) => {
            if let horned_owl::model::Literal::Datatype { datatype_iri, .. } = &x.to
                && !crate::datatypes::is_el_or_ql_datatype(datatype_iri.as_ref())
            {
                out.push(illegal_dr_violation(ac));
            }
        }
        Component::DeclareDatatype(dt)
            if !crate::datatypes::is_el_or_ql_datatype(dt.0.0.as_ref()) =>
        {
            out.push(banned(
                ac,
                "this datatype is not in QL's recognised data-range set",
            ));
        }
        Component::SubObjectPropertyOf(x) => {
            if let SubObjectPropertyExpression::ObjectPropertyChain(chain) = &x.sub
                && chain.len() >= 2
            {
                out.push(banned(
                    ac,
                    "property chains are not permitted in QL (breaks first-order rewritability)",
                ));
            }
        }
        Component::TransitiveObjectProperty(_) => {
            out.push(banned(
                ac,
                "TransitiveObjectProperty is not permitted in QL",
            ));
        }
        Component::FunctionalObjectProperty(_) => {
            out.push(banned(
                ac,
                "FunctionalObjectProperty is not permitted in QL",
            ));
        }
        Component::InverseFunctionalObjectProperty(_) => {
            out.push(banned(
                ac,
                "InverseFunctionalObjectProperty is not permitted in QL",
            ));
        }
        Component::FunctionalDataProperty(_) => {
            out.push(banned(ac, "FunctionalDataProperty is not permitted in QL"));
        }
        Component::HasKey(_) => {
            out.push(banned(ac, "HasKey is not permitted in QL"));
        }
        Component::DisjointUnion(_) => {
            out.push(banned(
                ac,
                "DisjointUnion is not permitted in QL (it requires ObjectUnionOf)",
            ));
        }
        // SWRL rules banned outright -- see el.rs's equivalent arm.
        Component::Rule(_) => {
            out.push(banned(ac, "SWRL rules are not permitted in QL"));
        }
        // SameIndividual is banned outright in QL specifically -- not a
        // general OWL 2 profile pattern: EL/RL both accept the identical
        // axiom, only QL rejects it.
        Component::SameIndividual(_) => {
            out.push(banned(ac, "SameIndividual is not permitted in QL"));
        }
        _ => {}
    }
}

/// Builds a `UseOfIllegalAxiomKind` violation for `ac`, with `reason`
/// explaining which axiom kind isn't permitted.
fn banned<A: ForIRI>(ac: &AnnotatedComponent<A>, reason: &'static str) -> Violation<A> {
    Violation::UseOfIllegalAxiomKind {
        axiom: ac.clone(),
        reason,
    }
}

/// Builds a `UseOfIllegalAxiomKind` violation for an illegal QL data range.
fn illegal_dr_violation<A: ForIRI>(ac: &AnnotatedComponent<A>) -> Violation<A> {
    Violation::UseOfIllegalAxiomKind {
        axiom: ac.clone(),
        reason: "data range is not a legal QL data range",
    }
}

/// Pushes a `UseOfNonSubClassExpression` violation onto `out` if `ce` isn't
/// legal in QL subclass position.
fn check_sub<A: ForIRI>(
    ce: &ClassExpression<A>,
    ac: &AnnotatedComponent<A>,
    out: &mut Vec<Violation<A>>,
) {
    if !is_ql_sub_class_expression(ce) {
        out.push(Violation::UseOfNonSubClassExpression {
            axiom: ac.clone(),
            ce: ce.clone(),
        });
    }
}

/// Pushes a `UseOfNonSuperClassExpression` violation onto `out` if `ce`
/// isn't legal in QL superclass position.
fn check_super<A: ForIRI>(
    ce: &ClassExpression<A>,
    ac: &AnnotatedComponent<A>,
    out: &mut Vec<Violation<A>>,
) {
    if !is_ql_super_class_expression(ce) {
        out.push(Violation::UseOfNonSuperClassExpression {
            axiom: ac.clone(),
            ce: ce.clone(),
        });
    }
}

/// Returns `true` if `ce` is a legal QL subclass-position class
/// expression.
///
/// The grammar is `owl:Thing | C | ∃R.⊤ | ∃R⁻.⊤` -- deliberately not
/// closed under intersection, unlike EL and unlike QL's own superclass
/// grammar. See
/// [W3C OWL 2 Profiles §3.2.3, "Class Expressions"](https://www.w3.org/TR/owl2-profiles/#Class_Expressions_2).
fn is_ql_sub_class_expression<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    match ce {
        ClassExpression::Class(_) => true,
        ClassExpression::ObjectSomeValuesFrom { bce, .. } => is_thing(bce),
        ClassExpression::DataSomeValuesFrom { dr, .. } => is_ql_data_range(dr),
        _ => false,
    }
}

/// Returns `true` if `ce` is a legal QL superclass-position class
/// expression.
///
/// The grammar is `owl:Thing | C | ¬C (atomic only) | ∃R.⊤ | ∃R.C | ∃R⁻.⊤
/// | ¬∃R.⊤` intersected with itself any number of times. Note the `∃R.C`
/// case (qualified, filler a plain named class rather than just
/// `owl:Thing`) is legal here but *not* in subclass position
/// (`is_ql_sub_class_expression` stays unqualified-only) -- a real
/// asymmetry between the two positions, not a simplification. See
/// [W3C OWL 2 Profiles §3.2.3, "Class Expressions"](https://www.w3.org/TR/owl2-profiles/#Class_Expressions_2).
fn is_ql_super_class_expression<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    match ce {
        ClassExpression::Class(_) => true,
        ClassExpression::ObjectIntersectionOf(ces) => ces.iter().all(is_ql_super_class_expression),
        ClassExpression::ObjectComplementOf(inner) => is_ql_negatable(inner),
        ClassExpression::ObjectSomeValuesFrom { bce, .. } => is_named_class(bce),
        ClassExpression::DataSomeValuesFrom { dr, .. } => is_ql_data_range(dr),
        _ => false,
    }
}

/// Returns `true` if `ce` is something `ObjectComplementOf` may legally
/// wrap in QL superclass position.
///
/// That's an atomic class, or an *unqualified* existential (`∃R.⊤`) --
/// unlike plain (non-negated) superclass position, the `∃R.C`
/// qualified-filler relaxation does *not* extend to negation.
fn is_ql_negatable<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    match ce {
        ClassExpression::Class(_) => true,
        ClassExpression::ObjectSomeValuesFrom { bce, .. } => is_thing(bce),
        _ => false,
    }
}

/// Returns `true` if `ce` is a plain named class (any class, including
/// `owl:Thing`).
fn is_named_class<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    matches!(ce, ClassExpression::Class(_))
}

/// Returns `true` if `ce` is specifically `owl:Thing`.
fn is_thing<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    matches!(ce, ClassExpression::Class(c) if c.is_thing())
}

/// Returns `true` if `dr` is a legal QL data range.
///
/// Named datatypes, except a confirmed-excluded set -- see
/// [`crate::datatypes`]. Still not the full precise QL data-range grammar
/// (e.g. `DataOneOf`/facet-restricted datatypes aren't modelled either
/// way), but tighter than "any named datatype" now that there's real
/// evidence some are rejected. See
/// [W3C OWL 2 Profiles §3.2.4, "Data Ranges"](https://www.w3.org/TR/owl2-profiles/#Data_Ranges_2).
fn is_ql_data_range<A: ForIRI>(dr: &DataRange<A>) -> bool {
    match dr {
        DataRange::Datatype(dt) => crate::datatypes::is_el_or_ql_datatype(dt.0.as_ref()),
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use super::check;
    use horned_owl::model::{
        Build, ClassExpression, DeclareClass, DeclareObjectProperty, MutableOntology,
        ObjectPropertyExpression, RcStr, SubClassOf, SubObjectPropertyExpression,
        SubObjectPropertyOf,
    };
    use horned_owl::ontology::set::SetOntology;
    use horned_owl::vocab::OWL;

    fn declared_ontology() -> (Build<RcStr>, SetOntology<RcStr>) {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareClass(b.class("http://example.com/B")));
        o.insert(DeclareObjectProperty(
            b.object_property("http://example.com/p"),
        ));
        (b, o)
    }

    // A ⊑ B: atomic class on both sides is always legal.
    #[test]
    fn atomic_subclassof_is_conformant() {
        let (b, mut o) = declared_ontology();
        o.insert(SubClassOf {
            sup: b.class("http://example.com/B").into(),
            sub: b.class("http://example.com/A").into(),
        });

        assert!(check(&o).is_conformant());
    }

    // ∃p.⊤ ⊑ B is legal (unqualified existential in subclass position).
    #[test]
    fn unqualified_existential_subclass_is_conformant() {
        let (b, mut o) = declared_ontology();
        let some_thing = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class(OWL::Thing.as_ref()).into()),
        };
        o.insert(SubClassOf {
            sup: b.class("http://example.com/B").into(),
            sub: some_thing,
        });

        assert!(check(&o).is_conformant());
    }

    // ∃p.B (qualified existential) is NOT legal in QL subclass position,
    // unlike EL.
    #[test]
    fn qualified_existential_subclass_is_rejected() {
        let (b, mut o) = declared_ontology();
        let some_b = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        o.insert(SubClassOf {
            sup: b.class("http://example.com/A").into(),
            sub: some_b,
        });

        assert!(!check(&o).is_conformant());
    }

    // A ⊑ ∃p.B (qualified existential, filler a plain named class) IS legal
    // in QL *super*class position. Contrast with
    // `qualified_existential_subclass_is_rejected` above -- the asymmetry
    // between sub/super position is real.
    #[test]
    fn qualified_existential_superclass_is_conformant() {
        let (b, mut o) = declared_ontology();
        let some_b = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        o.insert(SubClassOf {
            sup: some_b,
            sub: b.class("http://example.com/A").into(),
        });

        assert!(check(&o).is_conformant());
    }

    // xsd:date is rejected by ROBOT in QL data ranges; xsd:string is
    // accepted.
    #[test]
    fn excluded_datatype_in_data_some_values_from_is_rejected() {
        use horned_owl::model::{DataRange, Datatype};

        let (b, mut o) = declared_ontology();
        let dp = b.data_property("http://example.com/dp");
        o.insert(horned_owl::model::DeclareDataProperty(dp.clone()));
        let some_date = ClassExpression::DataSomeValuesFrom {
            dp,
            dr: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#date"))),
        };
        o.insert(SubClassOf {
            sup: b.class("http://example.com/A").into(),
            sub: some_date,
        });

        assert!(!check(&o).is_conformant());
    }

    #[test]
    fn ordinary_datatype_in_data_some_values_from_is_conformant() {
        use horned_owl::model::{DataRange, Datatype};

        let (b, mut o) = declared_ontology();
        let dp = b.data_property("http://example.com/dp");
        o.insert(horned_owl::model::DeclareDataProperty(dp.clone()));
        let some_string = ClassExpression::DataSomeValuesFrom {
            dp,
            dr: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#string"))),
        };
        o.insert(SubClassOf {
            sup: b.class("http://example.com/A").into(),
            sub: some_string,
        });

        assert!(check(&o).is_conformant());
    }

    // A bare declaration of an excluded datatype, never otherwise
    // referenced in a class expression, must also be flagged -- not just
    // usage sites like DataSomeValuesFrom.
    #[test]
    fn declaring_excluded_datatype_with_no_other_usage_is_rejected() {
        let (b, mut o) = declared_ontology();
        o.insert(horned_owl::model::DeclareDatatype(
            b.datatype("http://www.w3.org/2001/XMLSchema#gYear"),
        ));

        assert!(!check(&o).is_conformant());
    }

    // A ⊓ B ⊑ C: intersection is NOT legal in QL subclass position, unlike
    // EL (it's only legal on the super side).
    #[test]
    fn intersection_in_subclass_position_is_rejected() {
        let (b, mut o) = declared_ontology();
        let intersection = ClassExpression::ObjectIntersectionOf(vec![
            b.class("http://example.com/A").into(),
            b.class("http://example.com/B").into(),
        ]);
        o.insert(SubClassOf {
            sup: b.class("http://example.com/A").into(),
            sub: intersection,
        });

        assert!(!check(&o).is_conformant());
    }

    // A ⊑ B ⊓ ¬C: intersection and atomic negation ARE legal in QL
    // superclass position.
    #[test]
    fn intersection_and_negation_in_superclass_position_is_conformant() {
        let (b, mut o) = declared_ontology();
        o.insert(DeclareClass(b.class("http://example.com/C")));
        let not_c =
            ClassExpression::ObjectComplementOf(Box::new(b.class("http://example.com/C").into()));
        let sup = ClassExpression::ObjectIntersectionOf(vec![
            b.class("http://example.com/B").into(),
            not_c,
        ]);
        o.insert(SubClassOf {
            sup,
            sub: b.class("http://example.com/A").into(),
        });

        assert!(check(&o).is_conformant());
    }

    // ¬(A ⊓ B) -- negating a non-atomic expression is illegal (only atomic
    // classes and unqualified existentials may be negated in QL).
    #[test]
    fn negation_of_non_atomic_is_rejected() {
        let (b, mut o) = declared_ontology();
        let intersection = ClassExpression::ObjectIntersectionOf(vec![
            b.class("http://example.com/A").into(),
            b.class("http://example.com/B").into(),
        ]);
        let not_intersection = ClassExpression::ObjectComplementOf(Box::new(intersection));
        o.insert(SubClassOf {
            sup: not_intersection,
            sub: b.class("http://example.com/A").into(),
        });

        assert!(!check(&o).is_conformant());
    }

    // ¬∃R.C (negating a *qualified* existential) is illegal in QL --
    // unlike plain (non-negated) superclass position, which does accept
    // ∃R.C.
    #[test]
    fn negation_of_qualified_existential_is_rejected() {
        let (b, mut o) = declared_ontology();
        let existential = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        let negated = ClassExpression::ObjectComplementOf(Box::new(existential));
        o.insert(SubClassOf {
            sup: negated,
            sub: b.class("http://example.com/A").into(),
        });

        assert!(!check(&o).is_conformant());
    }

    // Property chains are banned outright in QL.
    #[test]
    fn property_chain_is_rejected() {
        let (b, mut o) = declared_ontology();
        let p: ObjectPropertyExpression<_> = b.object_property("http://example.com/p").into();
        o.insert(SubObjectPropertyOf {
            sup: p.clone(),
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![p.clone(), p]),
        });

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfIllegalAxiomKind { .. }))
        );
    }

    // TransitiveObjectProperty is banned outright in QL.
    #[test]
    fn transitive_object_property_is_rejected() {
        use horned_owl::model::TransitiveObjectProperty;

        let (b, mut o) = declared_ontology();
        o.insert(TransitiveObjectProperty(
            ObjectPropertyExpression::ObjectProperty(b.object_property("http://example.com/p")),
        ));

        assert!(!check(&o).is_conformant());
    }

    // DataOneOf is not a named datatype, so it's not a legal QL
    // DataPropertyRange.
    #[test]
    fn data_one_of_in_data_property_range_is_rejected() {
        use horned_owl::model::{DataPropertyRange, DataRange, DeclareDataProperty, Literal};

        let (b, mut o) = declared_ontology();
        let dp = b.data_property("http://example.com/dp");
        o.insert(DeclareDataProperty(dp.clone()));
        o.insert(DataPropertyRange {
            dp,
            dr: DataRange::DataOneOf(vec![Literal::Simple {
                literal: "Action".to_string(),
            }]),
        });

        assert!(!check(&o).is_conformant());
    }

    // xsd:float/xsd:double are excluded from QL's data-range set --
    // confirmed by direct `robot validate-profile -p QL` probing.
    #[test]
    fn float_in_data_property_range_is_rejected() {
        use horned_owl::model::{DataPropertyRange, DataRange, Datatype, DeclareDataProperty};

        let (b, mut o) = declared_ontology();
        let dp = b.data_property("http://example.com/dp");
        o.insert(DeclareDataProperty(dp.clone()));
        o.insert(DataPropertyRange {
            dp,
            dr: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#float"))),
        });

        assert!(!check(&o).is_conformant());
    }

    // ClassAssertion requires a strictly atomic class in QL -- stricter
    // than general superclass position, which permits a qualified
    // existential with a named-class filler.
    #[test]
    fn class_assertion_with_qualified_existential_is_rejected() {
        use horned_owl::model::{ClassAssertion, ClassExpression, DeclareNamedIndividual};

        let (b, mut o) = declared_ontology();
        o.insert(DeclareNamedIndividual(
            b.named_individual("http://example.com/i"),
        ));
        let existential = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        o.insert(ClassAssertion {
            ce: existential,
            i: b.named_individual("http://example.com/i").into(),
        });

        assert!(!check(&o).is_conformant());
    }

    #[test]
    fn class_assertion_with_atomic_class_is_conformant() {
        use horned_owl::model::{ClassAssertion, DeclareNamedIndividual};

        let (b, mut o) = declared_ontology();
        o.insert(DeclareNamedIndividual(
            b.named_individual("http://example.com/i"),
        ));
        o.insert(ClassAssertion {
            ce: b.class("http://example.com/A").into(),
            i: b.named_individual("http://example.com/i").into(),
        });

        assert!(check(&o).is_conformant(), "{:?}", check(&o).violations());
    }

    // SWRL rules are banned outright in QL -- see el.rs's equivalent test.
    #[test]
    fn swrl_rule_is_rejected() {
        use horned_owl::model::{Atom, IArgument, Rule};

        let (b, mut o) = declared_ontology();
        let var_iri = b.iri("urn:swrl:var#x");
        let atom = Atom::ClassAtom {
            pred: b.class("http://example.com/A").into(),
            arg: IArgument::Variable(var_iri.into()),
        };
        o.insert(Rule {
            head: vec![atom.clone()],
            body: vec![atom],
        });

        assert!(!check(&o).is_conformant());
    }

    // xsd:int isn't in QL's datatype allowlist -- see el.rs's equivalent
    // test.
    #[test]
    fn xsd_int_in_data_property_range_is_rejected() {
        use horned_owl::model::{DataPropertyRange, DataRange, Datatype, DeclareDataProperty};

        let (b, mut o) = declared_ontology();
        let dp = b.data_property("http://example.com/dp");
        o.insert(DeclareDataProperty(dp.clone()));
        o.insert(DataPropertyRange {
            dp,
            dr: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#int"))),
        });

        assert!(!check(&o).is_conformant());
    }

    // SameIndividual is banned outright in QL specifically -- EL/RL both
    // accept the same axiom, only QL rejects it.
    #[test]
    fn same_individual_is_rejected() {
        use horned_owl::model::{DeclareNamedIndividual, SameIndividual};

        let (b, mut o) = declared_ontology();
        o.insert(DeclareNamedIndividual(
            b.named_individual("http://example.com/i"),
        ));
        o.insert(DeclareNamedIndividual(
            b.named_individual("http://example.com/j"),
        ));
        o.insert(SameIndividual(vec![
            b.named_individual("http://example.com/i").into(),
            b.named_individual("http://example.com/j").into(),
        ]));

        assert!(!check(&o).is_conformant());
    }
}
