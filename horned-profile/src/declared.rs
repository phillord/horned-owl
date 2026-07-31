//! Declared-entity lookup.
//!
//! Reuses `horned_owl`'s own declaration/punning-resolution index
//! (`DeclarationMappedIndex`) rather than re-deriving that logic --
//! see `docs/horned-profile-plan.md`'s "Shared infrastructure" section.
//!
//! Also tracks, separately, the *full* set of declared kinds per IRI
//! (`all_kinds`). `DeclarationMappedIndex::kind` deliberately collapses an
//! IRI to a single "most useful" kind when it's punned (e.g. an
//! ObjectProperty/AnnotationProperty pun always resolves to ObjectProperty,
//! per issue #228) -- correct for its own purpose, but wrong for phase 3's
//! needs: checking "was this IRI declared as an AnnotationProperty at all"
//! or "is this IRI illegally punned" both need the full set, not the
//! collapsed single answer.

use std::collections::{BTreeSet, HashMap, HashSet};
use std::rc::Rc;

use horned_owl::model::{AnnotatedComponent, Component, ForIRI, IRI, NamedEntityKind, Ontology};
use horned_owl::ontology::declaration_mapped::DeclarationMappedIndex;
use horned_owl::ontology::indexed::OntologyIndex;
use horned_owl::vocab::{self, Namespace, OWL2Datatype};

pub struct DeclaredEntities<A: ForIRI> {
    index: DeclarationMappedIndex<A, Rc<AnnotatedComponent<A>>>,
    all_kinds: HashMap<IRI<A>, BTreeSet<NamedEntityKind>>,
}

impl<A: ForIRI> DeclaredEntities<A> {
    /// Builds a `DeclaredEntities` from every `Declare*` axiom in `o`.
    ///
    /// Indexes both the collapsed kind (via `DeclarationMappedIndex`, for
    /// `kind`/`is_kind`) and the full, uncollapsed per-IRI kind set (via
    /// `all_kinds`, for `is_declared_as`/`illegal_punnings`) in one pass.
    pub fn from_ontology<O: Ontology<A>>(o: &O) -> Self {
        let mut index = DeclarationMappedIndex::new();
        let mut all_kinds: HashMap<IRI<A>, BTreeSet<NamedEntityKind>> = HashMap::new();

        for ac in o.iter() {
            index.index_insert(Rc::new(ac.clone()));

            let entry = match &ac.component {
                Component::DeclareClass(dc) => Some((dc.0.0.clone(), NamedEntityKind::Class)),
                Component::DeclareObjectProperty(dp) => {
                    Some((dp.0.0.clone(), NamedEntityKind::ObjectProperty))
                }
                Component::DeclareDataProperty(dp) => {
                    Some((dp.0.0.clone(), NamedEntityKind::DataProperty))
                }
                Component::DeclareAnnotationProperty(dp) => {
                    Some((dp.0.0.clone(), NamedEntityKind::AnnotationProperty))
                }
                Component::DeclareDatatype(dt) => Some((dt.0.0.clone(), NamedEntityKind::Datatype)),
                Component::DeclareNamedIndividual(ni) => {
                    Some((ni.0.0.clone(), NamedEntityKind::NamedIndividual))
                }
                _ => None,
            };
            if let Some((iri, kind)) = entry {
                all_kinds.entry(iri).or_default().insert(kind);
            }
        }

        DeclaredEntities { index, all_kinds }
    }

    /// Returns the declared kind of `iri`, if declared.
    ///
    /// Collapsed to a single answer when punned (see
    /// `DeclarationMappedIndex::kind`). Built-in vocabulary (`owl:Thing`,
    /// `owl:topObjectProperty`, ...) resolves without an explicit
    /// declaration axiom.
    pub fn kind(&self, iri: &IRI<A>) -> Option<NamedEntityKind> {
        self.index.kind(iri)
    }

    /// Returns `true` if `iri`'s collapsed declared kind (see `kind`) is
    /// `kind`.
    pub fn is_kind(&self, iri: &IRI<A>, kind: NamedEntityKind) -> bool {
        self.index.is_kind(iri, kind)
    }

    /// Returns every kind `iri` is declared as, uncollapsed.
    ///
    /// Empty if undeclared.
    pub fn all_kinds(&self, iri: &IRI<A>) -> BTreeSet<NamedEntityKind> {
        self.all_kinds.get(iri).cloned().unwrap_or_default()
    }

    /// Returns `true` if `iri` is declared as `kind`, specifically.
    ///
    /// Correct even when the IRI is also (illegally) punned as something
    /// else, unlike `is_kind`. Also accepts built-in vocabulary without
    /// requiring a declaration axiom: `owl:Thing`/`owl:topObjectProperty`/etc. (via
    /// `vocab::to_built_in_entity`), the built-in annotation properties
    /// (`rdfs:label`/`comment`/`seeAlso`/`isDefinedBy`,
    /// `owl:deprecated`/`versionInfo`/`priorVersion`/
    /// `backwardCompatibleWith`/`incompatibleWith`, via
    /// `vocab::is_annotation_builtin`), and the XSD datatypes plus
    /// `owl:real`/`owl:rational`/`rdf:PlainLiteral`/`rdf:XMLLiteral` (the
    /// last three matched by IRI string -- none has a named `vocab`
    /// constant in this codebase).
    pub fn is_declared_as(&self, iri: &IRI<A>, kind: NamedEntityKind) -> bool {
        if self
            .all_kinds
            .get(iri)
            .is_some_and(|kinds| kinds.contains(&kind))
        {
            return true;
        }
        if kind == NamedEntityKind::Datatype
            && (vocab::is_xsd_datatype(iri.as_ref())
                || iri.as_ref() == OWL2Datatype::Literal.as_ref()
                || is_owl_real_or_rational(iri)
                || is_rdf_plain_or_xml_literal(iri))
        {
            return true;
        }
        if kind == NamedEntityKind::AnnotationProperty && vocab::is_annotation_builtin(iri.as_ref())
        {
            return true;
        }
        vocab::to_built_in_entity(iri).map(NamedEntityKind::from) == Some(kind)
    }

    /// Returns declared IRIs that are OWL 2 DL "reserved vocabulary".
    ///
    /// Reserved vocabulary is core `rdf:`/`rdfs:`/`owl:`/`xsd:` terms
    /// (`rdf:type`, `rdfs:subClassOf`, `owl:sameAs`, ...) that have a fixed
    /// built-in meaning and can never legally be the subject of a
    /// `Declare*` axiom, as *any* entity kind. See
    /// [OWL 2 Structural Specification §2.4, "IRIs"](https://www.w3.org/TR/owl2-syntax/#IRIs).
    /// Distinct from the legitimately-reusable built-ins `is_declared_as`
    /// already carves out (`owl:Thing`, the XSD datatypes, the built-in
    /// annotation properties, ...) -- explicitly declaring one of *those*
    /// again is redundant, not illegal, so they're excluded here.
    ///
    /// `xsd:` is a special case: unlike `rdf:`/`rdfs:`/`owl:` (a
    /// heterogeneous mix of term kinds with no single "natural" role), the
    /// entire purpose of the `xsd:` namespace in OWL 2 is as a source of
    /// datatypes -- so `Declaration(Datatype(xsd:anything))` is always
    /// legal, even for an invented/misspelled local name that isn't a real
    /// XSD 1.0/1.1 type, while `Declaration(AnyOtherKind(xsd:anything))` is
    /// exactly as illegal as the `rdf:`/`rdfs:`/`owl:` cases.
    pub fn reserved_vocabulary_violations(&self) -> Vec<IRI<A>> {
        let mut out: Vec<IRI<A>> = self
            .all_kinds
            .iter()
            .filter(|(iri, kinds)| {
                is_reserved_vocabulary(iri)
                    && !is_legitimately_reusable_builtin(iri)
                    && !is_xsd_iri_used_only_as_datatype(iri, kinds)
            })
            .map(|(iri, _)| iri.clone())
            .collect();
        out.sort();
        out
    }

    /// Returns IRIs that are more than one kind of named entity, unless
    /// explicitly allowed.
    ///
    /// "Explicitly allowed" means the OWL 2 DL type-separation rules:
    /// Class and Datatype must be disjoint, and ObjectProperty/DataProperty/
    /// AnnotationProperty must be pairwise disjoint from each other.
    /// (`Class`/`NamedIndividual` punning is explicitly legal and excluded
    /// -- see `class_individual_puns`.)
    ///
    /// Each returned set is exactly the violating kinds for that IRI, not
    /// its full declared-kind set (e.g. an IRI declared Class, Datatype,
    /// *and* NamedIndividual reports only `{Class, Datatype}` here).
    pub fn illegal_punnings(&self) -> Vec<(IRI<A>, BTreeSet<NamedEntityKind>)> {
        const PROPERTY_KINDS: [NamedEntityKind; 3] = [
            NamedEntityKind::ObjectProperty,
            NamedEntityKind::DataProperty,
            NamedEntityKind::AnnotationProperty,
        ];

        let mut out = Vec::new();
        for (iri, kinds) in &self.all_kinds {
            let mut violating: BTreeSet<NamedEntityKind> = BTreeSet::new();

            if kinds.contains(&NamedEntityKind::Class) && kinds.contains(&NamedEntityKind::Datatype)
            {
                violating.insert(NamedEntityKind::Class);
                violating.insert(NamedEntityKind::Datatype);
            }

            let present_properties: BTreeSet<NamedEntityKind> = PROPERTY_KINDS
                .into_iter()
                .filter(|k| kinds.contains(k))
                .collect();
            if present_properties.len() >= 2 {
                violating.extend(present_properties);
            }

            if !violating.is_empty() {
                out.push((iri.clone(), violating));
            }
        }
        out.sort_by(|a, b| a.0.cmp(&b.0));
        out
    }

    /// Returns IRIs declared as both a `Class` and a `NamedIndividual`
    /// (legal punning under OWL 2 DL).
    pub fn class_individual_puns(&self) -> &HashSet<IRI<A>> {
        self.index.puns()
    }
}

/// Returns `true` if `iri` is the built-in `owl:real` or `owl:rational`
/// datatype.
///
/// Matched by IRI string since neither has a named `vocab` constant in
/// this codebase.
fn is_owl_real_or_rational<A: ForIRI>(iri: &IRI<A>) -> bool {
    matches!(
        iri.as_ref().strip_prefix(Namespace::OWL.as_ref()),
        Some("real") | Some("rational")
    )
}

/// Returns `true` if `iri` is the built-in `rdf:PlainLiteral` or
/// `rdf:XMLLiteral` datatype.
///
/// Matched by IRI string since neither has a named `vocab` constant in
/// this codebase (same reasoning as `is_owl_real_or_rational`).
fn is_rdf_plain_or_xml_literal<A: ForIRI>(iri: &IRI<A>) -> bool {
    matches!(
        iri.as_ref().strip_prefix(Namespace::RDF.as_ref()),
        Some("PlainLiteral") | Some("XMLLiteral")
    )
}

/// Returns `true` if `iri` is in the `rdf:`/`rdfs:`/`owl:`/`xsd:` namespace
/// at all -- not just the specific local names horned-owl happens to model
/// as `RDF`/`RDFS`/`OWL` enum variants.
///
/// The whole namespace is reserved per
/// OWL 2 DL, not just its "real" terms: an ontology author writing
/// `rdfs:creator` (not a real RDFS property -- the real term with this
/// meaning would be `dc:creator`/`dc:terms:creator`) is exactly as illegal
/// as writing `rdfs:subClassOf`, since both are declaring something in a
/// reserved namespace. A narrower check based on `RDF`/`RDFS`/`OWL`'s
/// `FromStr` impls (real, enumerated vocabulary terms only) misses this
/// case, since `FromStr` only succeeds for local names horned-owl's vocab
/// enums actually contain -- an invented local name in a reserved
/// namespace is arguably the more common mistake in practice.
fn is_reserved_vocabulary<A: ForIRI>(iri: &IRI<A>) -> bool {
    let s = iri.as_ref();
    s.starts_with(Namespace::RDF.as_ref())
        || s.starts_with(Namespace::RDFS.as_ref())
        || s.starts_with(Namespace::OWL.as_ref())
        || s.starts_with(Namespace::XSD.as_ref())
}

/// Returns `true` if `iri` is in the `xsd:` namespace and only ever
/// declared as a `Datatype`.
///
/// See `reserved_vocabulary_violations`'s doc comment: `xsd:` is reserved
/// against every kind *except* `Datatype`, unlike `rdf:`/`rdfs:`/`owl:`
/// which have no such natural-kind exception.
fn is_xsd_iri_used_only_as_datatype<A: ForIRI>(
    iri: &IRI<A>,
    kinds: &BTreeSet<NamedEntityKind>,
) -> bool {
    iri.as_ref().starts_with(Namespace::XSD.as_ref())
        && kinds.len() == 1
        && kinds.contains(&NamedEntityKind::Datatype)
}

/// Returns `true` for any IRI that's legitimately reusable without a
/// declaration axiom at all, regardless of kind.
///
/// The kind-agnostic version of `DeclaredEntities::is_declared_as`'s
/// built-in carve-outs (so *redundantly* declaring one of them is
/// harmless, not the reserved-vocabulary violation
/// `reserved_vocabulary_violations` is checking for).
fn is_legitimately_reusable_builtin<A: ForIRI>(iri: &IRI<A>) -> bool {
    vocab::is_known_xsd_datatype(iri.as_ref())
        || iri.as_ref() == OWL2Datatype::Literal.as_ref()
        || is_owl_real_or_rational(iri)
        || is_rdf_plain_or_xml_literal(iri)
        || vocab::is_annotation_builtin(iri.as_ref())
        || vocab::to_built_in_entity(iri).is_some()
}
