//! OWL 2 EL profile.
//!
//! EL's class-expression grammar is close to symmetric between subclass
//! and superclass position (unlike QL/RL), and it bans object-property
//! inverses outright -- see [W3C OWL 2 Profiles §2](https://www.w3.org/TR/owl2-profiles/#OWL_2_EL).
//! `EL` conformance additionally requires OWL 2 DL conformance, per spec
//! (see [`check`]).
//!
//! **Scope note**: implements the class-expression grammar (sub/super
//! position), the EL data-range restriction (named datatypes only -- no
//! intersection/union/complement/`DataOneOf`/facet-restricted datatypes,
//! plus an empirically-confirmed named-datatype allowlist, see
//! [`crate::datatypes`]), object-property-inverse banning, SWRL rules
//! banned outright, and the axiom kinds banned outright. Not yet checked:
//! the handful of EL-specific facet-restricted datatypes the spec does
//! actually permit (a narrow numeric-datatype allowance) -- treated
//! conservatively here as always-illegal, which can only ever over-report,
//! never under-report, a real violation.

use horned_owl::model::{
    AnnotatedComponent, ClassExpression, Component, DataRange, ForIRI, ObjectPropertyExpression,
    Ontology,
};
use horned_owl::visitor::immutable::{Visit, Walk};

use crate::declared::DeclaredEntities;
use crate::owl2dl;
use crate::{Profile, ProfileReport, Violation};

/// Checks `o` against the OWL 2 EL profile (implies OWL 2 DL conformance).
pub fn check<A: ForIRI, O: Ontology<A>>(o: &O) -> ProfileReport<A> {
    let declared = DeclaredEntities::from_ontology(o);
    let mut violations = owl2dl::structural_violations(o, &declared);
    for ac in o.iter() {
        check_axiom(ac, &mut violations);
    }
    ProfileReport::new(Profile::EL, violations)
}

/// Checks `ac` against the EL profile, pushing any violations onto `out`.
fn check_axiom<A: ForIRI>(ac: &AnnotatedComponent<A>, out: &mut Vec<Violation<A>>) {
    if axiom_uses_inverse(ac) {
        out.push(Violation::UseOfObjectPropertyInverse { axiom: ac.clone() });
    }

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
        Component::ClassAssertion(x) => {
            check_super(&x.ce, ac, out);
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
        Component::DataPropertyRange(x) => {
            if !is_el_data_range(&x.dr) {
                out.push(illegal_dr_violation(ac));
            }
        }
        // A literal's own datatype (from a `DataPropertyAssertion`, not a
        // `DataPropertyRange`) is subject to the same EL datatype
        // restriction. `Literal::Simple`/`Language` are always fine
        // (implicitly `xsd:string`/`rdf:langString`), so only the
        // explicitly-typed variant needs checking.
        Component::DataPropertyAssertion(x) => {
            if let horned_owl::model::Literal::Datatype { datatype_iri, .. } = &x.to
                && !crate::datatypes::is_el_or_ql_datatype(datatype_iri.as_ref())
            {
                out.push(illegal_dr_violation(ac));
            }
        }
        // Bare declaration of a non-EL-legal datatype, never otherwise
        // referenced in a class expression (e.g. `xsd:date a rdfs:Datatype .`
        // with no `DataPropertyRange`/`DataSomeValuesFrom` usage at all) --
        // same pattern as `ql`'s bare-`DeclareDatatype` site.
        Component::DeclareDatatype(dt)
            if !crate::datatypes::is_el_or_ql_datatype(dt.0.0.as_ref()) =>
        {
            out.push(banned_axiom(
                ac,
                "this datatype is not in EL's recognised data-range set",
            ));
        }
        Component::HasKey(x) => {
            check_sub(&x.ce, ac, out);
        }
        Component::DatatypeDefinition(x) => {
            if !is_el_data_range(&x.range) {
                out.push(illegal_dr_violation(ac));
            }
        }
        Component::DisjointObjectProperties(_) => {
            out.push(banned_axiom(
                ac,
                "DisjointObjectProperties is not permitted in EL",
            ));
        }
        Component::DisjointDataProperties(_) => {
            out.push(banned_axiom(
                ac,
                "DisjointDataProperties is not permitted in EL",
            ));
        }
        Component::FunctionalObjectProperty(_) => {
            out.push(banned_axiom(
                ac,
                "FunctionalObjectProperty is not permitted in EL",
            ));
        }
        Component::InverseFunctionalObjectProperty(_) => {
            out.push(banned_axiom(
                ac,
                "InverseFunctionalObjectProperty is not permitted in EL",
            ));
        }
        Component::IrreflexiveObjectProperty(_) => {
            out.push(banned_axiom(
                ac,
                "IrreflexiveObjectProperty is not permitted in EL",
            ));
        }
        Component::SymmetricObjectProperty(_) => {
            out.push(banned_axiom(
                ac,
                "SymmetricObjectProperty is not permitted in EL",
            ));
        }
        Component::AsymmetricObjectProperty(_) => {
            out.push(banned_axiom(
                ac,
                "AsymmetricObjectProperty is not permitted in EL",
            ));
        }
        Component::InverseObjectProperties(_) => {
            out.push(banned_axiom(
                ac,
                "InverseObjectProperties is not permitted in EL",
            ));
        }
        Component::DisjointUnion(_) => {
            out.push(banned_axiom(
                ac,
                "DisjointUnion is not permitted in EL (it requires ObjectUnionOf)",
            ));
        }
        // SWRL rules (DLSafeRule) are banned outright in EL/QL/RL, unlike
        // plain OWL 2 DL, which permits them as an extension.
        Component::Rule(_) => {
            out.push(banned_axiom(ac, "SWRL rules are not permitted in EL"));
        }
        _ => {}
    }
}

/// Builds a `UseOfIllegalAxiomKind` violation for `ac`, with `reason`
/// explaining which axiom kind isn't permitted.
fn banned_axiom<A: ForIRI>(ac: &AnnotatedComponent<A>, reason: &'static str) -> Violation<A> {
    Violation::UseOfIllegalAxiomKind {
        axiom: ac.clone(),
        reason,
    }
}

/// Builds a `UseOfIllegalAxiomKind` violation for an illegal EL data range.
///
/// `DataPropertyRange` and `DatatypeDefinition` don't have a
/// `ClassExpression` to attach to `UseOfNonSub/SuperClassExpression`, so an
/// illegal data range in either is reported via the generic
/// `UseOfIllegalAxiomKind` instead.
fn illegal_dr_violation<A: ForIRI>(ac: &AnnotatedComponent<A>) -> Violation<A> {
    Violation::UseOfIllegalAxiomKind {
        axiom: ac.clone(),
        reason: "data range is not a legal EL data range (named datatypes only)",
    }
}

/// Pushes a `UseOfNonSubClassExpression` violation onto `out` if `ce` isn't
/// legal in EL subclass position.
fn check_sub<A: ForIRI>(
    ce: &ClassExpression<A>,
    ac: &AnnotatedComponent<A>,
    out: &mut Vec<Violation<A>>,
) {
    if !is_el_sub_class_expression(ce) {
        out.push(Violation::UseOfNonSubClassExpression {
            axiom: ac.clone(),
            ce: ce.clone(),
        });
    }
}

/// Pushes a `UseOfNonSuperClassExpression` violation onto `out` if `ce`
/// isn't legal in EL superclass position.
fn check_super<A: ForIRI>(
    ce: &ClassExpression<A>,
    ac: &AnnotatedComponent<A>,
    out: &mut Vec<Violation<A>>,
) {
    if !is_el_super_class_expression(ce) {
        out.push(Violation::UseOfNonSuperClassExpression {
            axiom: ac.clone(),
            ce: ce.clone(),
        });
    }
}

/// Returns `true` if `ce` is a legal EL subclass-position class
/// expression.
///
/// See [W3C OWL 2 Profiles §2.2.3, "Class Expressions"](https://www.w3.org/TR/owl2-profiles/#Class_Expressions).
fn is_el_sub_class_expression<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    match ce {
        ClassExpression::Class(_) => true,
        ClassExpression::ObjectIntersectionOf(ces) => ces.iter().all(is_el_sub_class_expression),
        ClassExpression::ObjectSomeValuesFrom { bce, .. } => is_el_sub_class_expression(bce),
        ClassExpression::ObjectHasValue { .. } => true,
        ClassExpression::ObjectOneOf(inds) => inds.len() == 1,
        ClassExpression::DataSomeValuesFrom { dr, .. } => is_el_data_range(dr),
        ClassExpression::DataHasValue { .. } => true,
        _ => false,
    }
}

/// Returns `true` if `ce` is a legal EL superclass-position class
/// expression.
///
/// See [W3C OWL 2 Profiles §2.2.3, "Class Expressions"](https://www.w3.org/TR/owl2-profiles/#Class_Expressions).
fn is_el_super_class_expression<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    match ce {
        ClassExpression::Class(_) => true,
        ClassExpression::ObjectIntersectionOf(ces) => ces.iter().all(is_el_super_class_expression),
        ClassExpression::ObjectSomeValuesFrom { bce, .. } => is_el_super_class_expression(bce),
        ClassExpression::ObjectHasValue { .. } => true,
        ClassExpression::ObjectHasSelf(_) => true,
        // Singleton nominal, same as sub-position -- a common OBO idiom is
        // `EquivalentClasses(:C ObjectOneOf(:a))` ("C is exactly the
        // singleton {a}"), which needs this on the super side too, since
        // `EquivalentClasses` checks both. QL/RL both reject the same
        // construct, so this is EL-specific.
        ClassExpression::ObjectOneOf(inds) => inds.len() == 1,
        ClassExpression::DataSomeValuesFrom { dr, .. } => is_el_data_range(dr),
        ClassExpression::DataHasValue { .. } => true,
        _ => false,
    }
}

/// Returns `true` if `dr` is a legal EL data range.
///
/// See [W3C OWL 2 Profiles §2.2.4, "Data Ranges"](https://www.w3.org/TR/owl2-profiles/#Data_Ranges)
/// and [`crate::datatypes`] for the allowlist.
fn is_el_data_range<A: ForIRI>(dr: &DataRange<A>) -> bool {
    match dr {
        DataRange::Datatype(dt) => crate::datatypes::is_el_or_ql_datatype(dt.0.as_ref()),
        _ => false,
    }
}

/// `Visit` state for `axiom_uses_inverse`: `true` once any
/// `InverseObjectProperty` has been seen.
struct InverseUseDetector<A: ForIRI>(bool, std::marker::PhantomData<A>);

impl<A: ForIRI> Visit<A> for InverseUseDetector<A> {
    /// Sets the detector's flag if `ope` is an `InverseObjectProperty`.
    fn visit_object_property_expression(&mut self, ope: &ObjectPropertyExpression<A>) {
        if matches!(ope, ObjectPropertyExpression::InverseObjectProperty(_)) {
            self.0 = true;
        }
    }
}

/// Returns `true` if `ac` uses an object-property inverse anywhere within
/// it, however deeply nested.
fn axiom_uses_inverse<A: ForIRI>(ac: &AnnotatedComponent<A>) -> bool {
    let mut walk = Walk::new(InverseUseDetector(false, std::marker::PhantomData));
    walk.annotated_component(ac);
    walk.into_visit().0
}

#[cfg(test)]
mod test {
    use super::check;
    use horned_owl::model::{
        Build, ClassExpression, DeclareClass, DeclareObjectProperty, MutableOntology,
        ObjectPropertyExpression, RcStr, SubClassOf,
    };
    use horned_owl::ontology::set::SetOntology;

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

    // ObjectIntersectionOf(A, ObjectSomeValuesFrom(p, B)) sub B -- classic
    // EL-legal shape (intersection + existential, both sides).
    #[test]
    fn intersection_and_existential_is_conformant() {
        let (b, mut o) = declared_ontology();
        let a: ClassExpression<_> = b.class("http://example.com/A").into();
        let some_b = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        let sub = ClassExpression::ObjectIntersectionOf(vec![a, some_b]);
        o.insert(SubClassOf {
            sup: b.class("http://example.com/B").into(),
            sub,
        });

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // ObjectUnionOf is disallowed anywhere in EL.
    #[test]
    fn union_is_rejected() {
        let (b, mut o) = declared_ontology();
        let union = ClassExpression::ObjectUnionOf(vec![
            b.class("http://example.com/A").into(),
            b.class("http://example.com/B").into(),
        ]);
        o.insert(SubClassOf {
            sup: b.class("http://example.com/A").into(),
            sub: union,
        });

        let report = check(&o);
        assert!(!report.is_conformant());
    }

    // ObjectAllValuesFrom is disallowed anywhere in EL.
    #[test]
    fn all_values_from_is_rejected() {
        let (b, mut o) = declared_ontology();
        let all = ClassExpression::ObjectAllValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        o.insert(SubClassOf {
            sup: all,
            sub: b.class("http://example.com/A").into(),
        });

        let report = check(&o);
        assert!(!report.is_conformant());
    }

    // ObjectHasSelf is a legal EL *super*class-position construct but not
    // a legal *sub*class-position one.
    #[test]
    fn has_self_is_super_only() {
        let (b, mut o) = declared_ontology();
        let has_self = ClassExpression::ObjectHasSelf(ObjectPropertyExpression::ObjectProperty(
            b.object_property("http://example.com/p"),
        ));

        o.insert(SubClassOf {
            sup: has_self.clone(),
            sub: b.class("http://example.com/A").into(),
        });
        assert!(check(&o).is_conformant());

        let mut o2 = declared_ontology().1;
        o2.insert(SubClassOf {
            sup: b.class("http://example.com/A").into(),
            sub: has_self,
        });
        assert!(!check(&o2).is_conformant());
    }

    // Inverse object properties are banned outright in EL, wherever they
    // appear -- both inside a class expression and as a bare
    // InverseObjectProperties axiom.
    #[test]
    fn object_property_inverse_is_rejected() {
        let (b, mut o) = declared_ontology();
        let inv = ObjectPropertyExpression::InverseObjectProperty(
            b.object_property("http://example.com/p"),
        );
        let some = ClassExpression::ObjectSomeValuesFrom {
            ope: inv,
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        o.insert(SubClassOf {
            sup: some,
            sub: b.class("http://example.com/A").into(),
        });

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfObjectPropertyInverse { .. }))
        );
    }

    // FunctionalObjectProperty is an axiom kind banned outright in EL.
    #[test]
    fn functional_object_property_is_rejected() {
        use horned_owl::model::FunctionalObjectProperty;

        let (b, mut o) = declared_ontology();
        o.insert(FunctionalObjectProperty(
            ObjectPropertyExpression::ObjectProperty(b.object_property("http://example.com/p")),
        ));

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfIllegalAxiomKind { .. }))
        );
    }

    // Cardinality restrictions are disallowed anywhere in EL.
    #[test]
    fn cardinality_restriction_is_rejected() {
        let (b, mut o) = declared_ontology();
        let card = ClassExpression::ObjectMaxCardinality {
            n: 1,
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        o.insert(SubClassOf {
            sup: card,
            sub: b.class("http://example.com/A").into(),
        });

        let report = check(&o);
        assert!(!report.is_conformant());
    }

    // xsd:date is excluded from EL's data-range set, even when only
    // bare-declared and never referenced by a class expression.
    #[test]
    fn bare_declaration_of_excluded_datatype_is_rejected() {
        use horned_owl::model::DeclareDatatype;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareDatatype(
            b.datatype("http://www.w3.org/2001/XMLSchema#date"),
        ));

        let report = check(&o);
        assert!(!report.is_conformant());
    }

    #[test]
    fn ordinary_datatype_declaration_is_conformant() {
        use horned_owl::model::DeclareDatatype;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareDatatype(
            b.datatype("http://www.w3.org/2001/XMLSchema#string"),
        ));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // EquivalentClasses(C, ObjectOneOf(a)) -- "C is exactly the singleton
    // {a}" -- is a legal EL construct, a common OBO idiom for
    // class-as-nominal punning (unlike QL/RL, which both reject it).
    #[test]
    fn equivalent_classes_with_singleton_one_of_is_conformant() {
        use horned_owl::model::{DeclareNamedIndividual, EquivalentClasses};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/C")));
        o.insert(DeclareNamedIndividual(
            b.named_individual("http://example.com/a"),
        ));
        o.insert(EquivalentClasses(vec![
            b.class("http://example.com/C").into(),
            ClassExpression::ObjectOneOf(vec![b.named_individual("http://example.com/a").into()]),
        ]));

        let report = check(&o);
        assert!(report.is_conformant(), "{:?}", report.violations());
    }

    // An OWL 2 DL violation (undeclared entity) is still reported by the
    // EL checker too, since EL conformance requires DL conformance first.
    #[test]
    fn dl_violations_still_reported() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(SubClassOf {
            sup: b.class("http://example.com/Undeclared").into(),
            sub: b.class("http://example.com/A").into(),
        });

        let report = check(&o);
        assert!(!report.is_conformant());
        assert!(
            report
                .violations()
                .iter()
                .any(|v| matches!(v, super::Violation::UseOfUndeclaredClass { .. }))
        );
    }

    // SWRL rules are banned outright in EL (only plain DL accepts one).
    #[test]
    fn swrl_rule_is_rejected() {
        use horned_owl::model::{Atom, IArgument, Rule};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
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

    // xsd:int isn't in EL's datatype allowlist (nor is the rest of the
    // integer-restriction family).
    #[test]
    fn xsd_int_in_data_property_range_is_rejected() {
        use horned_owl::model::{DataPropertyRange, DataRange, Datatype, DeclareDataProperty};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let dp = b.data_property("http://example.com/dp");
        o.insert(DeclareDataProperty(dp.clone()));
        o.insert(DataPropertyRange {
            dp,
            dr: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#int"))),
        });

        assert!(!check(&o).is_conformant());
    }

    // A literal's own datatype (DataPropertyAssertion, not
    // DataPropertyRange) is subject to the same restriction.
    #[test]
    fn xsd_int_typed_literal_in_data_property_assertion_is_rejected() {
        use horned_owl::model::{
            DataPropertyAssertion, DeclareDataProperty, DeclareNamedIndividual, Literal,
        };

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let dp = b.data_property("http://example.com/dp");
        o.insert(DeclareDataProperty(dp.clone()));
        o.insert(DeclareNamedIndividual(
            b.named_individual("http://example.com/i"),
        ));
        o.insert(DataPropertyAssertion {
            dp,
            from: b.named_individual("http://example.com/i").into(),
            to: Literal::Datatype {
                literal: "5".to_string(),
                datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#int"),
            },
        });

        assert!(!check(&o).is_conformant());
    }
}
