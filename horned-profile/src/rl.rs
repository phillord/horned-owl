//! OWL 2 RL profile.
//!
//! RL's class-expression grammar has its own asymmetric shape, distinct
//! from both EL and QL: subclass position allows `ObjectUnionOf` (neither
//! EL nor QL do), while superclass position allows atomic negation and
//! cardinality restricted to 0/1 -- see
//! [W3C OWL 2 Profiles §4](https://www.w3.org/TR/owl2-profiles/#OWL_2_RL).
//! `RL` conformance additionally requires OWL 2 DL conformance, per spec
//! (see [`check`]).
//!
//! **Scope note -- narrower than `el`/`ql`**: this implements the
//! class-expression grammar (sub/super position) and a handful of
//! outright axiom-kind bans confirmed via real corpus evidence (see
//! `check_axiom`) -- `DisjointUnion`, SWRL rules, `ReflexiveObjectProperty`,
//! `DatatypeDefinition`. Unlike EL/QL, no broader axiom-kind banlist is
//! implemented: RL's restrictiveness comes mostly from the
//! class-expression grammar itself, and general spec recall of RL's other
//! axiom-kind restrictions isn't confident enough to encode as bans
//! without real risk of *over*-restricting -- a false "not conformant" is
//! a more actively misleading answer from a checking tool than a missed
//! violation, so the bar for adding a ban here is higher than for `el`/
//! `ql`. The grammar reconstruction below is itself a best-effort recall
//! from general OWL 2 spec knowledge, not verified against spec text or
//! the OWL API source -- treat RL results with more caution than EL/QL's
//! until checked.

use horned_owl::model::{
    AnnotatedComponent, ClassExpression, Component, DataRange, ForIRI, Ontology,
};
use horned_owl::vocab;

use crate::declared::DeclaredEntities;
use crate::owl2dl;
use crate::{Profile, ProfileReport, Violation};

/// Checks `o` against the OWL 2 RL profile (implies OWL 2 DL conformance).
pub fn check<A: ForIRI, O: Ontology<A>>(o: &O) -> ProfileReport<A> {
    let declared = DeclaredEntities::from_ontology(o);
    let mut violations = owl2dl::structural_violations(o, &declared);
    for ac in o.iter() {
        check_axiom(ac, &mut violations);
    }
    ProfileReport::new(Profile::RL, violations)
}

/// Checks `ac` against the RL profile, pushing any violations onto `out`.
fn check_axiom<A: ForIRI>(ac: &AnnotatedComponent<A>, out: &mut Vec<Violation<A>>) {
    match &ac.component {
        Component::SubClassOf(x) => {
            check_sub(&x.sub, ac, out);
            check_super(&x.sup, ac, out);
        }
        // EquivalentClasses(C1, ..., Cn) decomposes into SubClassOf(Ci, Cj)
        // for every i != j, so every operand plays both the sub role (in
        // some pair) and the super role (in another) -- each one must pass
        // *both* grammars, not just check_sub.
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
        // ClassAssertion(C, a) means "a is an instance of C" -- C plays the
        // superclass role (the thing `a` falls under), so its grammar
        // check is check_super, not check_sub.
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
        // Banned outright, regardless of the class expressions involved --
        // unlike QL (which bans it because it inherently needs
        // `ObjectUnionOf`, itself QL-illegal), RL *does* allow
        // `ObjectUnionOf` in subclass position, so this isn't banned for
        // the same reason; a real corpus rejection, not derived from the
        // grammar.
        Component::DisjointUnion(_) => {
            out.push(banned_axiom(ac, "DisjointUnion is not permitted in RL"));
        }
        // SWRL rules banned outright -- see el.rs's equivalent arm.
        Component::Rule(_) => {
            out.push(banned_axiom(ac, "SWRL rules are not permitted in RL"));
        }
        Component::ReflexiveObjectProperty(_) => {
            out.push(banned_axiom(
                ac,
                "ReflexiveObjectProperty is not permitted in RL",
            ));
        }
        // `is_rl_data_range` is also used inside the class-expression
        // grammar (`DataSomeValuesFrom`/`DataAllValuesFrom`), so this
        // dispatch handles the axiom-level case, `DataPropertyRange`,
        // separately.
        Component::DataPropertyRange(x) if !is_rl_data_range(&x.dr) => {
            out.push(banned_axiom(ac, "data range is not a legal RL data range"));
        }
        // Bare declaration of an excluded datatype, never otherwise
        // referenced in a class expression (e.g. `xsd:date a rdfs:Datatype .`),
        // same pattern as `el`'s/`ql`'s bare-`DeclareDatatype` sites.
        Component::DeclareDatatype(dt) if !is_rl_datatype(dt.0.0.as_ref()) => {
            out.push(banned_axiom(
                ac,
                "this datatype is not in RL's recognised data-range set",
            ));
        }
        Component::DatatypeDefinition(_) => {
            out.push(banned_axiom(
                ac,
                "DatatypeDefinition is not permitted in RL",
            ));
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

/// Pushes a `UseOfNonSubClassExpression` violation onto `out` if `ce` isn't
/// legal in RL subclass position.
fn check_sub<A: ForIRI>(
    ce: &ClassExpression<A>,
    ac: &AnnotatedComponent<A>,
    out: &mut Vec<Violation<A>>,
) {
    if !is_rl_sub_class_expression(ce) {
        out.push(Violation::UseOfNonSubClassExpression {
            axiom: ac.clone(),
            ce: ce.clone(),
        });
    }
}

/// Pushes a `UseOfNonSuperClassExpression` violation onto `out` if `ce`
/// isn't legal in RL superclass position.
fn check_super<A: ForIRI>(
    ce: &ClassExpression<A>,
    ac: &AnnotatedComponent<A>,
    out: &mut Vec<Violation<A>>,
) {
    if !is_rl_super_class_expression(ce) {
        out.push(Violation::UseOfNonSuperClassExpression {
            axiom: ac.clone(),
            ce: ce.clone(),
        });
    }
}

/// Returns `true` if `ce` is a legal RL subclass-position class
/// expression.
///
/// The grammar is `C | s⊓s | s⊔s | {a1,...,an} | ∃R.s | ∃R.{a} |
/// DataSomeValuesFrom | DataHasValue` (data range restricted to named
/// datatypes, see `is_rl_data_range`) -- notably includes `ObjectUnionOf`,
/// which neither EL nor QL's subclass grammar does. See
/// [W3C OWL 2 Profiles §4.2.3, "Class Expressions"](https://www.w3.org/TR/owl2-profiles/#Class_Expressions_3).
fn is_rl_sub_class_expression<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    match ce {
        ClassExpression::Class(_) => true,
        ClassExpression::ObjectIntersectionOf(ces) => ces.iter().all(is_rl_sub_class_expression),
        ClassExpression::ObjectUnionOf(ces) => ces.iter().all(is_rl_sub_class_expression),
        ClassExpression::ObjectOneOf(inds) => !inds.is_empty(),
        ClassExpression::ObjectSomeValuesFrom { bce, .. } => is_rl_sub_class_expression(bce),
        ClassExpression::ObjectHasValue { .. } => true,
        ClassExpression::DataSomeValuesFrom { dr, .. } => is_rl_data_range(dr),
        ClassExpression::DataHasValue { .. } => true,
        _ => false,
    }
}

/// Returns `true` if `ce` is a legal RL superclass-position class
/// expression.
///
/// The grammar is `C | ¬(subClassExpression) | t⊓t | ∀R.t | ∃R.{a} | ≤1
/// R.s | DataAllValuesFrom | DataHasValue | ≤1 D.s` -- notably includes
/// negation of the *sub*-grammar (not just atomic negation, unlike QL) and
/// cardinality restricted to 0/1.
///
/// `owl:Thing` specifically is *not* legal as a bare superclass here (`C`
/// means a named class other than `Thing`) -- confirmed empirically
/// against ROBOT/the OWL API; contrast with EL/QL, where `owl:Thing` in
/// superclass position is fine. Applied wherever this grammar recurses
/// (e.g. an `ObjectAllValuesFrom` filler), not just the bare top-level
/// case, on the assumption RL's real superclass grammar excludes bare
/// `owl:Thing` consistently -- an extrapolation from the confirmed
/// top-level case, not itself independently checked at every nesting
/// position. See
/// [W3C OWL 2 Profiles §4.2.3, "Class Expressions"](https://www.w3.org/TR/owl2-profiles/#Class_Expressions_3).
fn is_rl_super_class_expression<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    match ce {
        ClassExpression::Class(c) => !c.is_thing(),
        ClassExpression::ObjectIntersectionOf(ces) => ces.iter().all(is_rl_super_class_expression),
        ClassExpression::ObjectComplementOf(inner) => is_rl_sub_class_expression(inner),
        ClassExpression::ObjectAllValuesFrom { bce, .. } => is_rl_super_class_expression(bce),
        ClassExpression::ObjectHasValue { .. } => true,
        ClassExpression::ObjectMaxCardinality { n, bce, .. } => {
            *n <= 1 && is_rl_sub_class_expression(bce)
        }
        ClassExpression::DataAllValuesFrom { dr, .. } => is_rl_data_range(dr),
        ClassExpression::DataHasValue { .. } => true,
        ClassExpression::DataMaxCardinality { n, .. } => *n <= 1,
        _ => false,
    }
}

/// Returns `true` if `dr` is a legal RL data range.
///
/// Named datatypes only -- `DataOneOf` is confirmed rejected by ROBOT/the
/// OWL API for RL, despite the more permissive shape of RL's
/// class-expression grammar elsewhere. RL's actual data-range grammar is
/// somewhat richer still (a limited set of facet-restricted datatypes),
/// not modelled here. See
/// [W3C OWL 2 Profiles §4.2.4, "Data Ranges"](https://www.w3.org/TR/owl2-profiles/#Data_Ranges_3).
fn is_rl_data_range<A: ForIRI>(dr: &DataRange<A>) -> bool {
    match dr {
        DataRange::Datatype(dt) => is_rl_datatype(dt.0.as_ref()),
        _ => false,
    }
}

/// The XSD datatypes RL rejects.
///
/// Unlike EL/QL (where the accepted set turned out small and fixed, so
/// those use an allowlist -- see [`crate::datatypes`]), RL's accepted set
/// is broad and its rejected set stays small even after fairly wide
/// empirical probing against ROBOT/the OWL API, so this is a denylist: RL
/// accepts `boolean`/`float`/`double`/`int`/`short`/`negativeInteger`/
/// `nonNegativeInteger`/`language`/`unsignedLong`/`unsignedByte` (all
/// EL/QL-rejected), while rejecting `duration`/`unsignedInt`/
/// `unsignedShort` in addition to the partial-date/time family below. The
/// `unsignedInt`/`unsignedShort` rejected but `unsignedLong`/`unsignedByte`
/// accepted asymmetry is real, empirically confirmed, not a transcription
/// error -- XSD's derived numeric facets aren't implemented uniformly.
const RL_EXCLUDED_DATATYPES: [&str; 10] = [
    "http://www.w3.org/2001/XMLSchema#date",
    "http://www.w3.org/2001/XMLSchema#time",
    "http://www.w3.org/2001/XMLSchema#gYear",
    "http://www.w3.org/2001/XMLSchema#gMonth",
    "http://www.w3.org/2001/XMLSchema#gDay",
    "http://www.w3.org/2001/XMLSchema#gYearMonth",
    "http://www.w3.org/2001/XMLSchema#gMonthDay",
    "http://www.w3.org/2001/XMLSchema#duration",
    "http://www.w3.org/2001/XMLSchema#unsignedInt",
    "http://www.w3.org/2001/XMLSchema#unsignedShort",
];

/// Returns `true` if `iri` is a legal RL datatype.
///
/// Any non-XSD datatype is legal; an XSD one is legal unless it's in
/// [`RL_EXCLUDED_DATATYPES`].
fn is_rl_datatype(iri: &str) -> bool {
    if vocab::is_xsd_datatype(iri) {
        !RL_EXCLUDED_DATATYPES.contains(&iri)
    } else {
        true
    }
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

    // A ⊑ owl:Thing (bare) is NOT legal in RL superclass position.
    #[test]
    fn bare_thing_in_superclass_position_is_rejected() {
        use horned_owl::vocab::OWL;

        let (b, mut o) = declared_ontology();
        o.insert(SubClassOf {
            sup: b.class(OWL::Thing.as_ref()).into(),
            sub: b.class("http://example.com/A").into(),
        });

        assert!(!check(&o).is_conformant());
    }

    // A ⊔ B ⊑ C: union is legal in RL subclass position, unlike EL/QL.
    #[test]
    fn union_in_subclass_position_is_conformant() {
        let (b, mut o) = declared_ontology();
        o.insert(DeclareClass(b.class("http://example.com/C")));
        let union = ClassExpression::ObjectUnionOf(vec![
            b.class("http://example.com/A").into(),
            b.class("http://example.com/B").into(),
        ]);
        o.insert(SubClassOf {
            sup: b.class("http://example.com/C").into(),
            sub: union,
        });

        assert!(check(&o).is_conformant());
    }

    // A ⊔ B ⊑ ... is illegal in *super*class position -- union is
    // sub-grammar-only in RL, same asymmetry principle as QL's
    // intersection/negation being super-only.
    #[test]
    fn union_in_superclass_position_is_rejected() {
        let (b, mut o) = declared_ontology();
        let union = ClassExpression::ObjectUnionOf(vec![
            b.class("http://example.com/A").into(),
            b.class("http://example.com/B").into(),
        ]);
        o.insert(SubClassOf {
            sup: union,
            sub: b.class("http://example.com/A").into(),
        });

        assert!(!check(&o).is_conformant());
    }

    // A ⊑ ¬(B ⊓ C): negation of a non-atomic (but sub-grammar-legal)
    // expression is fine in RL superclass position, unlike QL's
    // atomic-only negation.
    #[test]
    fn negation_of_intersection_in_superclass_is_conformant() {
        let (b, mut o) = declared_ontology();
        o.insert(DeclareClass(b.class("http://example.com/C")));
        let intersection = ClassExpression::ObjectIntersectionOf(vec![
            b.class("http://example.com/B").into(),
            b.class("http://example.com/C").into(),
        ]);
        let negated = ClassExpression::ObjectComplementOf(Box::new(intersection));
        o.insert(SubClassOf {
            sup: negated,
            sub: b.class("http://example.com/A").into(),
        });

        assert!(check(&o).is_conformant());
    }

    // <=1 p.B is legal in RL superclass position (cardinality 0/1 only).
    #[test]
    fn max_cardinality_one_is_conformant() {
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

        assert!(check(&o).is_conformant());
    }

    // <=2 p.B exceeds RL's 0/1-only cardinality restriction.
    #[test]
    fn max_cardinality_two_is_rejected() {
        let (b, mut o) = declared_ontology();
        let card = ClassExpression::ObjectMaxCardinality {
            n: 2,
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        o.insert(SubClassOf {
            sup: card,
            sub: b.class("http://example.com/A").into(),
        });

        assert!(!check(&o).is_conformant());
    }

    // ObjectMinCardinality is not part of RL's superclass grammar at all
    // (only max, capped at 1, is).
    #[test]
    fn min_cardinality_is_rejected() {
        let (b, mut o) = declared_ontology();
        let card = ClassExpression::ObjectMinCardinality {
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

        assert!(!check(&o).is_conformant());
    }

    // ClassAssertion(owl:Thing, a) -- bare Thing must be checked as a
    // *superclass* expression, not subclass.
    #[test]
    fn class_assertion_of_bare_thing_is_rejected() {
        use horned_owl::model::{ClassAssertion, DeclareNamedIndividual};
        use horned_owl::vocab::OWL;

        let (b, mut o) = declared_ontology();
        o.insert(DeclareNamedIndividual(
            b.named_individual("http://example.com/i"),
        ));
        o.insert(ClassAssertion {
            ce: b.class(OWL::Thing.as_ref()).into(),
            i: b.named_individual("http://example.com/i").into(),
        });

        assert!(!check(&o).is_conformant());
    }

    // EquivalentClasses(A, ObjectSomeValuesFrom(p, B)) -- the existential
    // is a legal RL *subclass* expression but not a legal *superclass* one
    // (RL restricts existentials in superclass position to the unqualified
    // owl:Thing filler only), so it must be rejected here even though a
    // plain SubClassOf with the same existential as *subclass* would be
    // fine.
    #[test]
    fn equivalent_classes_with_qualified_existential_is_rejected() {
        use horned_owl::model::EquivalentClasses;

        let (b, mut o) = declared_ontology();
        let existential = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(
                b.object_property("http://example.com/p"),
            ),
            bce: Box::new(b.class("http://example.com/B").into()),
        };
        o.insert(EquivalentClasses(vec![
            b.class("http://example.com/A").into(),
            existential,
        ]));

        assert!(!check(&o).is_conformant());
    }

    // DisjointUnion is banned outright in RL, even though RL's own grammar
    // otherwise permits `ObjectUnionOf` in subclass position.
    #[test]
    fn disjoint_union_is_rejected() {
        use horned_owl::model::DisjointUnion;

        let (b, mut o) = declared_ontology();
        o.insert(DeclareClass(b.class("http://example.com/C")));
        o.insert(DisjointUnion(
            b.class("http://example.com/A"),
            vec![b.class("http://example.com/B").into(), b.class("http://example.com/C").into()],
        ));

        assert!(!check(&o).is_conformant());
    }

    // SWRL rules are banned outright in RL -- see el.rs's equivalent test.
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

    // ReflexiveObjectProperty is banned outright in RL.
    #[test]
    fn reflexive_object_property_is_rejected() {
        use horned_owl::model::{ObjectPropertyExpression, ReflexiveObjectProperty};

        let (b, mut o) = declared_ontology();
        o.insert(ReflexiveObjectProperty(
            ObjectPropertyExpression::ObjectProperty(b.object_property("http://example.com/p")),
        ));

        assert!(!check(&o).is_conformant());
    }

    // DataOneOf is not a legal RL data range, even in DataPropertyRange
    // (a separate dispatch site from the class-expression grammar).
    #[test]
    fn data_one_of_in_data_property_range_is_rejected() {
        use horned_owl::model::{DataPropertyRange, DataRange, DeclareDataProperty, Literal};

        let (b, mut o) = declared_ontology();
        let dp = b.data_property("http://example.com/dp");
        o.insert(DeclareDataProperty(dp.clone()));
        o.insert(DataPropertyRange {
            dp,
            dr: DataRange::DataOneOf(vec![Literal::Simple {
                literal: "a".to_string(),
            }]),
        });

        assert!(!check(&o).is_conformant());
    }

    // DatatypeDefinition is banned outright in RL.
    #[test]
    fn datatype_definition_is_rejected() {
        use horned_owl::model::{DataRange, Datatype, DatatypeDefinition};

        let (b, mut o) = declared_ontology();
        o.insert(DatatypeDefinition {
            kind: Datatype(b.iri("http://example.com/MyDatatype")),
            range: DataRange::Datatype(Datatype(b.iri("http://www.w3.org/2001/XMLSchema#integer"))),
        });

        assert!(!check(&o).is_conformant());
    }

    // xsd:date isn't in RL's datatype set, even bare-declared and never
    // otherwise referenced.
    #[test]
    fn bare_declaration_of_excluded_datatype_is_rejected() {
        use horned_owl::model::DeclareDatatype;

        let (b, mut o) = declared_ontology();
        o.insert(DeclareDatatype(
            b.datatype("http://www.w3.org/2001/XMLSchema#date"),
        ));

        assert!(!check(&o).is_conformant());
    }

    // RL's excluded set is much narrower than EL/QL's -- xsd:boolean and
    // xsd:int, both EL/QL-rejected, are RL-conformant.
    #[test]
    fn ordinary_datatype_declaration_is_conformant() {
        use horned_owl::model::DeclareDatatype;

        let (b, mut o) = declared_ontology();
        o.insert(DeclareDatatype(
            b.datatype("http://www.w3.org/2001/XMLSchema#int"),
        ));

        assert!(check(&o).is_conformant(), "{:?}", check(&o).violations());
    }
}
