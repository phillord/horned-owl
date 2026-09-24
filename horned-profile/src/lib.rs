//! OWL 2 profile (EL/QL/RL/DL) conformance checking.
//!
//! See `docs/horned-profile-plan.md` at the repository root for the design
//! rationale and phased implementation plan, including per-module scope
//! notes and open questions -- particularly `ql`'s and `rl`'s, which carry
//! genuine unverified-recall caveats, not just deliberate scope cuts.
//!
//! All four checkers are implemented: [`owl2dl::check`] (plain OWL 2 DL),
//! [`el::check`], [`ql::check`], [`rl::check`]. [`check`] and
//! [`conformant_profiles`] dispatch across all four by [`Profile`].

mod datatypes;
pub mod declared;
pub mod el;
pub mod owl2dl;
pub mod ql;
pub mod regularity;
pub mod rl;
pub mod simple_property;

pub use declared::DeclaredEntities;
pub use simple_property::SimplePropertyAnalysis;

use horned_owl::model::{
    AnnotatedComponent, ClassExpression, DataRange, ForIRI, IRI, ObjectProperty,
    ObjectPropertyExpression, Ontology,
};

/// Checks `o` against `profile`.
pub fn check<A: ForIRI, O: Ontology<A>>(o: &O, profile: Profile) -> ProfileReport<A> {
    match profile {
        Profile::OWL2DL => owl2dl::check(o),
        Profile::EL => el::check(o),
        Profile::QL => ql::check(o),
        Profile::RL => rl::check(o),
    }
}

/// Returns every profile `o` conforms to, in `Profile`'s declaration order
/// (`OWL2DL, EL, QL, RL`).
///
/// An ontology can conform to more than one of EL/QL/RL/DL at once -- they
/// overlap, they aren't mutually exclusive tiers.
pub fn conformant_profiles<A: ForIRI, O: Ontology<A>>(o: &O) -> Vec<Profile> {
    [Profile::OWL2DL, Profile::EL, Profile::QL, Profile::RL]
        .into_iter()
        .filter(|&p| check(o, p).is_conformant())
        .collect()
}

/// Which OWL 2 profile a `ProfileReport` checks conformance against.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Profile {
    /// Plain OWL 2 DL -- not one of the three tractable sub-profiles, but
    /// a real conformance question in its own right (global restrictions:
    /// role-hierarchy regularity, declaration/punning constraints). Also
    /// a prerequisite for EL/QL/RL conformance, per spec.
    OWL2DL,
    /// The OWL 2 EL profile -- see [`el`].
    EL,
    /// The OWL 2 QL profile -- see [`ql`].
    QL,
    /// The OWL 2 RL profile -- see [`rl`].
    RL,
}

/// One way an ontology fails to conform to a `Profile`.
///
/// Mirrors the OWL API's per-profile violation class hierarchy
/// (`org.semanticweb.owlapi.profiles.violations`) as variants of a single
/// enum rather than a class-per-violation hierarchy. `UseOfNonAtomicClassExpression`
/// and `UseOfIllegalClassExpression` are currently unconstructed --
/// subsumed in practice by the coarser class-expression/data-range checks
/// `el`/`ql`/`rl` actually do.
#[derive(Clone, Debug)]
pub enum Violation<A: ForIRI> {
    /// A class expression isn't *atomic* where OWL 2 DL requires one to
    /// be. Currently unconstructed -- see this enum's doc comment.
    UseOfNonAtomicClassExpression {
        axiom: AnnotatedComponent<A>,
        ce: ClassExpression<A>,
    },
    /// `ce` isn't legal in the profile's subclass-position grammar.
    UseOfNonSubClassExpression {
        axiom: AnnotatedComponent<A>,
        ce: ClassExpression<A>,
    },
    /// `ce` isn't legal in the profile's superclass-position grammar.
    UseOfNonSuperClassExpression {
        axiom: AnnotatedComponent<A>,
        ce: ClassExpression<A>,
    },
    /// A class expression isn't legal in OWL 2 DL at all, in any position.
    /// Currently unconstructed -- see this enum's doc comment.
    UseOfIllegalClassExpression {
        axiom: AnnotatedComponent<A>,
        ce: ClassExpression<A>,
    },
    /// `ObjectUnionOf`/`ObjectIntersectionOf` built with fewer than the two
    /// operands OWL 2 DL requires for these n-ary constructors --
    /// `horned_owl::model`'s `Vec`-backed representation doesn't enforce
    /// arity, so this is representable and needs an explicit runtime check.
    UseOfClassExpressionWithTooFewOperands {
        axiom: AnnotatedComponent<A>,
        ce: ClassExpression<A>,
    },
    /// The `DataUnionOf`/`DataIntersectionOf` counterpart of
    /// `UseOfClassExpressionWithTooFewOperands`, for the same OWL 2 DL
    /// n-ary-constructor arity requirement applied to `DataRange` instead
    /// of `ClassExpression`.
    UseOfDataRangeWithTooFewOperands {
        axiom: AnnotatedComponent<A>,
        dr: DataRange<A>,
    },
    /// A `DatatypeDefinition` whose subject (`kind`) is itself a genuine
    /// built-in XSD/OWL 2 datatype -- illegal, since `DatatypeDefinition`
    /// is for introducing a *new* custom datatype, not redefining an
    /// existing built-in one.
    UseOfBuiltinDatatypeInDatatypeDefinition { axiom: AnnotatedComponent<A> },
    /// `ObjectHasSelf` is given a composite (non-simple) object property.
    UseOfNonSimplePropertyInObjectHasSelf {
        axiom: AnnotatedComponent<A>,
        ope: ObjectPropertyExpression<A>,
    },
    /// An object-cardinality restriction is given a composite (non-simple)
    /// object property.
    UseOfNonSimplePropertyInCardinalityRestriction {
        axiom: AnnotatedComponent<A>,
        ope: ObjectPropertyExpression<A>,
    },
    /// `DisjointObjectProperties` includes a composite (non-simple) object
    /// property.
    UseOfNonSimplePropertyInDisjointPropertiesAxiom { axiom: AnnotatedComponent<A> },
    /// `IrreflexiveObjectProperty` is given a composite (non-simple) object
    /// property.
    UseOfNonSimplePropertyInIrreflexivePropertyAxiom { axiom: AnnotatedComponent<A> },
    /// `AsymmetricObjectProperty` is given a composite (non-simple) object
    /// property.
    UseOfNonSimplePropertyInAsymmetricPropertyAxiom { axiom: AnnotatedComponent<A> },
    /// `FunctionalObjectProperty` requires a simple property, per the same
    /// OWL 2 DL table (`ObjectHasSelf`/cardinality/`Disjoint`/`Irreflexive`/
    /// `Asymmetric`) the other `UseOfNonSimpleProperty*` variants cover.
    UseOfNonSimplePropertyInFunctionalPropertyAxiom { axiom: AnnotatedComponent<A> },
    /// The `InverseFunctionalObjectProperty` counterpart of
    /// `UseOfNonSimplePropertyInFunctionalPropertyAxiom`, for the same OWL
    /// 2 DL simple-property requirement.
    UseOfNonSimplePropertyInInverseFunctionalPropertyAxiom { axiom: AnnotatedComponent<A> },
    /// A cycle in the role hierarchy's property-chain graph -- see
    /// `regularity`'s scope note. Spans potentially many chain axioms, so
    /// (unlike most other variants) this doesn't point at one offending
    /// axiom; `cycle` is the sequence of properties forming the cycle,
    /// first repeated as last to close the loop.
    UseOfPropertyInChainCausingCycle { cycle: Vec<ObjectProperty<A>> },
    /// `iri` is used as a class without a `DeclareClass` axiom.
    UseOfUndeclaredClass { iri: IRI<A> },
    /// `iri` is used as an object property without a
    /// `DeclareObjectProperty` axiom.
    UseOfUndeclaredObjectProperty { iri: IRI<A> },
    /// `iri` is used as a data property without a `DeclareDataProperty`
    /// axiom.
    UseOfUndeclaredDataProperty { iri: IRI<A> },
    /// `iri` is used as an annotation property without a
    /// `DeclareAnnotationProperty` axiom.
    UseOfUndeclaredAnnotationProperty { iri: IRI<A> },
    /// `iri` is used as a datatype without a `DeclareDatatype` axiom.
    UseOfUndeclaredDatatype { iri: IRI<A> },
    /// `iri` is declared as more than one mutually-exclusive entity kind;
    /// `kinds` names the violating kinds (see `DeclaredEntities::illegal_punnings`).
    UseOfIllegalPunning {
        iri: IRI<A>,
        kinds: Vec<&'static str>,
    },
    /// A reserved `rdf:`/`rdfs:`/`owl:` structural vocabulary term (e.g.
    /// `rdf:type`, `rdfs:subClassOf`, `owl:sameAs`) is the subject of a
    /// `Declare*` axiom.
    UseOfReservedVocabulary { iri: IRI<A> },
    /// EL-specific: `DataOneOf` with more than one literal.
    UseOfDataOneOfWithMultipleLiterals { axiom: AnnotatedComponent<A> },
    /// EL-specific: object property inverses are not permitted at all.
    UseOfObjectPropertyInverse { axiom: AnnotatedComponent<A> },
    /// This whole axiom *kind* isn't permitted in the profile, regardless
    /// of its content -- e.g. `FunctionalObjectProperty` in EL. A catch-all
    /// rather than one variant per banned axiom kind across three profiles.
    UseOfIllegalAxiomKind {
        axiom: AnnotatedComponent<A>,
        reason: &'static str,
    },
}

/// The result of checking one ontology against one `Profile`.
pub struct ProfileReport<A: ForIRI> {
    profile: Profile,
    violations: Vec<Violation<A>>,
}

impl<A: ForIRI> ProfileReport<A> {
    /// Builds a `ProfileReport` for `profile` from a checker's `violations`.
    pub fn new(profile: Profile, violations: Vec<Violation<A>>) -> Self {
        ProfileReport {
            profile,
            violations,
        }
    }

    /// Returns the `Profile` this report checks conformance against.
    pub fn profile(&self) -> Profile {
        self.profile
    }

    /// Returns `true` if the checked ontology has no violations.
    pub fn is_conformant(&self) -> bool {
        self.violations.is_empty()
    }

    /// Returns every violation found.
    pub fn violations(&self) -> &[Violation<A>] {
        &self.violations
    }
}

#[cfg(test)]
mod test {
    use super::{Profile, check, conformant_profiles};
    use horned_owl::model::{Build, DeclareClass, MutableOntology};
    use horned_owl::ontology::set::SetOntology;

    // An ontology with only a bare class declaration conforms to
    // everything -- OWL 2 DL and all three sub-profiles.
    #[test]
    fn trivial_ontology_conforms_to_everything() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));

        assert!(check(&o, Profile::OWL2DL).is_conformant());
        assert!(check(&o, Profile::EL).is_conformant());
        assert!(check(&o, Profile::QL).is_conformant());
        assert!(check(&o, Profile::RL).is_conformant());
        assert_eq!(
            conformant_profiles(&o),
            vec![Profile::OWL2DL, Profile::EL, Profile::QL, Profile::RL]
        );
    }

    // ObjectUnionOf conforms to RL but not EL/QL -- conformant_profiles
    // should report exactly that split, demonstrating the profiles
    // genuinely aren't a linear "stricter than" ladder.
    #[test]
    fn union_conforms_to_rl_only_among_the_sub_profiles() {
        use horned_owl::model::{ClassExpression, SubClassOf};

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        o.insert(DeclareClass(b.class("http://example.com/B")));
        o.insert(DeclareClass(b.class("http://example.com/C")));
        let union = ClassExpression::ObjectUnionOf(vec![
            b.class("http://example.com/A").into(),
            b.class("http://example.com/B").into(),
        ]);
        o.insert(SubClassOf {
            sup: b.class("http://example.com/C").into(),
            sub: union,
        });

        assert!(check(&o, Profile::OWL2DL).is_conformant());
        assert!(!check(&o, Profile::EL).is_conformant());
        assert!(!check(&o, Profile::QL).is_conformant());
        assert!(check(&o, Profile::RL).is_conformant());
        assert_eq!(conformant_profiles(&o), vec![Profile::OWL2DL, Profile::RL]);
    }

    // check/conformant_profiles are generic over Ontology<A>, not hardcoded
    // to SetOntology -- proven here with a different implementor rather
    // than just asserting it compiles for SetOntology alone.
    #[test]
    fn works_with_a_non_set_ontology_implementor() {
        use horned_owl::model::RcAnnotatedComponent;
        use horned_owl::ontology::component_mapped::ComponentMappedOntology;

        let b = Build::new_rc();
        let mut o = SetOntology::new();
        o.insert(DeclareClass(b.class("http://example.com/A")));
        let indexed: ComponentMappedOntology<_, RcAnnotatedComponent> = o.into();

        assert!(check(&indexed, Profile::OWL2DL).is_conformant());
    }
}
