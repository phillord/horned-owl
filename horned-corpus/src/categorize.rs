//! Diff categorization: classify each `RawDiff` item into a benign bucket
//! (`AnnotationNormalization`, `InferredDeclaration`, `NaryReshape`,
//! `Normalization`, `BlankNodeRelabel`) or a reported-defect bucket (`AnnotationLoss`,
//! `Unknown`) — the real-defect signal.
//!
//! Rules are applied in order, first match wins per item:
//! 1. A lost item and a gained item share a component-minus-annotations key
//!    (same `.component`). If their `.ann` sets are also equal, the pairing
//!    is a spurious/degenerate match and both items are tagged
//!    `AnnotationNormalization` (benign). If the `.ann` sets differ, the
//!    round-trip genuinely changed or dropped an annotation, so both items
//!    are tagged `AnnotationLoss` (a reported, non-benign difference).
//! 2. `InferredDeclaration` — a gained `Declare*` whose declared entity is
//!    used, with a matching entity kind, by some non-declaration component
//!    already present in `src`.
//! 3. `NaryReshape` — a gained binary n-ary axiom (EquivalentClasses /
//!    DisjointClasses / SameIndividual / DifferentIndividuals with exactly
//!    2 members) whose members are a subset of a same-variant axiom in
//!    `src`.
//! 4. `Normalization` — a pair (or lone item) that differs only by a
//!    deliberate model normalisation: a typed `xsd:string` literal read back
//!    as a simple one, a one-member `ObjectIntersectionOf`/`ObjectUnionOf`
//!    collapsed to its member, an n-ary axiom with fewer than two members
//!    dropped, or the empty `OntologyID` an OFN/OWX/OMN reader adds when the
//!    source had no ontology header.
//! 5. `BlankNodeRelabel` — any item still unmatched by rules 1-3 whose
//!    component mentions an anonymous individual. Task 6's blank-node
//!    canonicalization is a deterministic first cut that can leave residual
//!    ordering differences for models with 2+ asymmetric anonymous
//!    individuals; those must not pollute the `Unknown` signal.
//! 6. Everything else → `Unknown`.

use crate::diff::{RawDiff, kind_of};
use crate::model::{Category, DiffItem, Side};
use horned_owl::model::{
    AnnotatedComponent, AnnotationProperty, AnonymousIndividual, Class, ClassExpression, Component,
    DataProperty, Datatype, DeclareAnnotationProperty, DeclareClass, DeclareDataProperty,
    DeclareDatatype, DeclareNamedIndividual, DeclareObjectProperty, DifferentIndividuals,
    DisjointClasses, EquivalentClasses, IRI, Individual, Literal, NamedIndividual, ObjectProperty,
    OntologyID, RcStr, SameIndividual,
};
use horned_owl::ontology::set::SetOntology;
use horned_owl::visitor::immutable::{Visit, Walk};
use horned_owl::visitor::mutable::{VisitMut, WalkMut};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

pub fn categorize(d: RawDiff, src: &SetOntology<RcStr>, _rt: &SetOntology<RcStr>) -> Vec<DiffItem> {
    let mut out = Vec::new();
    let mut lost_paired = vec![false; d.only_in_source.len()];

    // Built once, up front: rules 2 and 3 consult these for every gained
    // item, so they must not be recomputed per item.
    let used = used_entities(src);
    let nary = nary_members(src);

    // Rule 1 pairing index: component (annotations stripped — two
    // `AnnotatedComponent`s with the same `.component` but different
    // `.ann` share a key) -> lost-item indices with that component, in
    // index order, consumed front-to-back as gained items pair with them.
    let mut lost_by_key: HashMap<&Component<RcStr>, VecDeque<usize>> = HashMap::new();
    for (i, s) in d.only_in_source.iter().enumerate() {
        lost_by_key.entry(&s.component).or_default().push_back(i);
    }

    // Rule 4 pairing index over the lost items, keyed on the normalised
    // form (annotations included).
    let mut lost_by_norm: HashMap<AnnotatedComponent<RcStr>, VecDeque<usize>> = HashMap::new();
    for (i, s) in d.only_in_source.iter().enumerate() {
        lost_by_norm.entry(normalised(s)).or_default().push_back(i);
    }
    let src_has_id = src
        .iter()
        .any(|c| matches!(c.component, Component::OntologyID(_)));

    for g in &d.only_in_roundtrip {
        // Rule 1: pair with an unpaired lost item that shares the
        // annotation-stripped key. Equal `.ann` sets -> benign
        // AnnotationNormalization; differing `.ann` sets -> a real
        // AnnotationLoss.
        if let Some(i) = lost_by_key
            .get_mut(&g.component)
            .and_then(|q| q.pop_front())
        {
            lost_paired[i] = true;
            let cat = if d.only_in_source[i].ann == g.ann {
                Category::AnnotationNormalization
            } else {
                Category::AnnotationLoss
            };
            out.push(item(Side::RoundTrip, g, cat));
            out.push(item(Side::Source, &d.only_in_source[i], cat));
            continue;
        }
        // Rule 2: InferredDeclaration.
        if is_inferred_declaration(g, &used) {
            out.push(item(Side::RoundTrip, g, Category::InferredDeclaration));
            continue;
        }
        // Rule 3: NaryReshape.
        if is_nary_reshape(g, &nary) {
            out.push(item(Side::RoundTrip, g, Category::NaryReshape));
            continue;
        }
        // Rule 4: Normalization.
        if let Some(i) = lost_by_norm.get_mut(&normalised(g)).and_then(|q| {
            q.iter()
                .position(|&i| !lost_paired[i])
                .and_then(|p| q.remove(p))
        }) {
            lost_paired[i] = true;
            out.push(item(Side::RoundTrip, g, Category::Normalization));
            out.push(item(
                Side::Source,
                &d.only_in_source[i],
                Category::Normalization,
            ));
            continue;
        }
        if !src_has_id && is_empty_ontology_id(g) {
            out.push(item(Side::RoundTrip, g, Category::Normalization));
            continue;
        }
        // Rule 5 (fallback): BlankNodeRelabel, else Unknown.
        out.push(item(Side::RoundTrip, g, fallback_category(g)));
    }
    for (i, s) in d.only_in_source.iter().enumerate() {
        if !lost_paired[i] {
            let cat = if is_degenerate_nary(&s.component) {
                Category::Normalization
            } else {
                fallback_category(s)
            };
            out.push(item(Side::Source, s, cat));
        }
    }
    out
}

fn item(side: Side, c: &AnnotatedComponent<RcStr>, category: Category) -> DiffItem {
    DiffItem {
        side,
        component_kind: kind_of(c),
        category,
        debug: format!("{c:?}"),
    }
}

/// `Visit` that flags whether the walk reached an `AnonymousIndividual`
/// anywhere — as an individual operand, nested inside a class expression,
/// or as an annotation subject/value.
#[derive(Default)]
struct HasAnon(bool);

impl Visit<RcStr> for HasAnon {
    fn visit_anonymous_individual(&mut self, _: &AnonymousIndividual<RcStr>) {
        self.0 = true;
    }
}

/// Rule 4: any component (including its annotations) that still mentions an
/// anonymous individual after rules 1-3 have failed to match is a
/// canonicalization residual, not a genuine defect.
fn fallback_category(c: &AnnotatedComponent<RcStr>) -> Category {
    let mut walk = Walk::new(HasAnon::default());
    walk.annotated_component(c);
    if walk.into_visit().0 {
        Category::BlankNodeRelabel
    } else {
        Category::Unknown
    }
}

const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";

/// Rewrites the deliberate normalisations of rule 4: `"x"^^xsd:string`
/// becomes a simple literal, and a one-member `ObjectIntersectionOf` or
/// `ObjectUnionOf` becomes its member.
struct Normalise;

impl VisitMut<RcStr> for Normalise {
    fn visit_literal(&mut self, l: &mut Literal<RcStr>) {
        if let Literal::Datatype {
            literal,
            datatype_iri,
        } = l
            && datatype_iri.as_ref() == XSD_STRING
        {
            *l = Literal::Simple {
                literal: std::mem::take(literal),
            };
        }
    }

    fn visit_class_expression(&mut self, e: &mut ClassExpression<RcStr>) {
        while let ClassExpression::ObjectIntersectionOf(v) | ClassExpression::ObjectUnionOf(v) = e
            && v.len() == 1
        {
            *e = v.pop().unwrap();
        }
    }
}

fn normalised(c: &AnnotatedComponent<RcStr>) -> AnnotatedComponent<RcStr> {
    let mut c = c.clone();
    let mut walk = WalkMut::new(Normalise);
    walk.annotated_component(&mut c);
    c
}

/// An n-ary axiom with fewer than two members, which the OFN writer drops.
fn is_degenerate_nary(c: &Component<RcStr>) -> bool {
    match c {
        Component::EquivalentClasses(EquivalentClasses(v)) => v.len() < 2,
        Component::DisjointClasses(DisjointClasses(v)) => v.len() < 2,
        Component::SameIndividual(SameIndividual(v)) => v.len() < 2,
        Component::DifferentIndividuals(DifferentIndividuals(v)) => v.len() < 2,
        _ => false,
    }
}

fn is_empty_ontology_id(c: &AnnotatedComponent<RcStr>) -> bool {
    matches!(
        &c.component,
        Component::OntologyID(OntologyID {
            iri: None,
            viri: None
        })
    )
}

/// True iff `c` is one of the six `Declare*` axiom kinds.
fn is_declaration(c: &Component<RcStr>) -> bool {
    matches!(
        c,
        Component::DeclareClass(_)
            | Component::DeclareObjectProperty(_)
            | Component::DeclareAnnotationProperty(_)
            | Component::DeclareDataProperty(_)
            | Component::DeclareNamedIndividual(_)
            | Component::DeclareDatatype(_)
    )
}

/// The set of IRIs used, per entity kind, by non-declaration components in
/// the source ontology. Built with a single `Walk` pass over `src`; each
/// gained declaration is then a constant-time lookup. This replaces a
/// per-declaration Debug-string scan over the whole source ontology, which
/// went quadratic on inputs that gain a declaration per entity on
/// round-trip (RDFS-heavy ontologies like GEXO: ~166K gained declarations
/// x ~1M source components).
#[derive(Default)]
struct UsedEntities {
    class: HashSet<IRI<RcStr>>,
    object_property: HashSet<IRI<RcStr>>,
    annotation_property: HashSet<IRI<RcStr>>,
    data_property: HashSet<IRI<RcStr>>,
    named_individual: HashSet<IRI<RcStr>>,
    datatype: HashSet<IRI<RcStr>>,
}

impl Visit<RcStr> for UsedEntities {
    fn visit_class(&mut self, c: &Class<RcStr>) {
        self.class.insert(c.0.clone());
    }
    fn visit_object_property(&mut self, p: &ObjectProperty<RcStr>) {
        self.object_property.insert(p.0.clone());
    }
    fn visit_annotation_property(&mut self, p: &AnnotationProperty<RcStr>) {
        self.annotation_property.insert(p.0.clone());
    }
    fn visit_data_property(&mut self, p: &DataProperty<RcStr>) {
        self.data_property.insert(p.0.clone());
    }
    fn visit_named_individual(&mut self, i: &NamedIndividual<RcStr>) {
        self.named_individual.insert(i.0.clone());
    }
    fn visit_datatype(&mut self, d: &Datatype<RcStr>) {
        self.datatype.insert(d.0.clone());
    }
}

/// Walk every non-declaration component in `src` (component only, not its
/// annotations — a use inside an axiom annotation doesn't justify a
/// declaration) and collect the entity IRIs it uses, in their typed
/// positions, into per-kind sets.
fn used_entities(src: &SetOntology<RcStr>) -> UsedEntities {
    let mut walk = Walk::new(UsedEntities::default());
    for c in src.iter() {
        if !is_declaration(&c.component) {
            walk.component(&c.component);
        }
    }
    walk.into_visit()
}

/// Rule 2: `gained` is a `Declare*` whose declared entity is used, with a
/// matching entity kind, by some non-declaration component in `src`. The
/// per-kind sets keep a punning/kind-mismatch out: an IRI used only as a
/// Class never appears in `named_individual`, so a gained
/// `DeclareNamedIndividual` for it is not treated as inferred.
fn is_inferred_declaration(gained: &AnnotatedComponent<RcStr>, used: &UsedEntities) -> bool {
    match &gained.component {
        Component::DeclareClass(DeclareClass(Class(iri))) => used.class.contains(iri),
        Component::DeclareObjectProperty(DeclareObjectProperty(ObjectProperty(iri))) => {
            used.object_property.contains(iri)
        }
        Component::DeclareAnnotationProperty(DeclareAnnotationProperty(AnnotationProperty(
            iri,
        ))) => used.annotation_property.contains(iri),
        Component::DeclareDataProperty(DeclareDataProperty(DataProperty(iri))) => {
            used.data_property.contains(iri)
        }
        Component::DeclareNamedIndividual(DeclareNamedIndividual(NamedIndividual(iri))) => {
            used.named_individual.contains(iri)
        }
        Component::DeclareDatatype(DeclareDatatype(Datatype(iri))) => used.datatype.contains(iri),
        _ => false,
    }
}

/// The member sets of every n-ary axiom in the source ontology, bucketed
/// by variant. Built once per `categorize` call; rule 3 then answers each
/// gained binary axiom with two `contains` probes per source axiom of the
/// same variant, instead of re-Debug-formatting the whole ontology.
#[derive(Default)]
struct NaryMembers<'a> {
    equivalent_classes: Vec<BTreeSet<&'a ClassExpression<RcStr>>>,
    disjoint_classes: Vec<BTreeSet<&'a ClassExpression<RcStr>>>,
    same_individual: Vec<BTreeSet<&'a Individual<RcStr>>>,
    different_individuals: Vec<BTreeSet<&'a Individual<RcStr>>>,
}

fn nary_members(src: &SetOntology<RcStr>) -> NaryMembers<'_> {
    let mut n = NaryMembers::default();
    for c in src.iter() {
        match &c.component {
            Component::EquivalentClasses(EquivalentClasses(v)) => {
                n.equivalent_classes.push(v.iter().collect())
            }
            Component::DisjointClasses(DisjointClasses(v)) => {
                n.disjoint_classes.push(v.iter().collect())
            }
            Component::SameIndividual(SameIndividual(v)) => {
                n.same_individual.push(v.iter().collect())
            }
            Component::DifferentIndividuals(DifferentIndividuals(v)) => {
                n.different_individuals.push(v.iter().collect())
            }
            _ => {}
        }
    }
    n
}

/// True iff `v` has exactly 2 members and some source member set of the
/// same variant contains both.
fn binary_subset<T: Ord>(v: &[T], sets: &[BTreeSet<&T>]) -> bool {
    if v.len() != 2 {
        return false;
    }
    sets.iter().any(|s| s.contains(&v[0]) && s.contains(&v[1]))
}

/// Rule 3: `gained` is a binary EquivalentClasses/DisjointClasses/
/// SameIndividual/DifferentIndividuals axiom whose 2 members are a subset
/// of a same-variant axiom already in `src`.
fn is_nary_reshape(gained: &AnnotatedComponent<RcStr>, nary: &NaryMembers) -> bool {
    match &gained.component {
        Component::EquivalentClasses(EquivalentClasses(v)) => {
            binary_subset(v, &nary.equivalent_classes)
        }
        Component::DisjointClasses(DisjointClasses(v)) => binary_subset(v, &nary.disjoint_classes),
        Component::SameIndividual(SameIndividual(v)) => binary_subset(v, &nary.same_individual),
        Component::DifferentIndividuals(DifferentIndividuals(v)) => {
            binary_subset(v, &nary.different_individuals)
        }
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diff::diff;
    use crate::model::Format;
    use crate::ontology::read_source;
    use horned_owl::model::MutableOntology;

    fn cats(src: &str, rt: &str) -> Vec<Category> {
        let s = read_source(Format::Ofn, src.as_bytes()).unwrap().model;
        let r = read_source(Format::Ofn, rt.as_bytes()).unwrap().model;
        let d = diff(&s, &r);
        categorize(d, &s, &r)
            .into_iter()
            .map(|x| x.category)
            .collect()
    }

    #[test]
    fn inferred_declaration_is_benign() {
        // rt gains DeclareClass(A); A is used by a SubClassOf present in src
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
        let rt = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\nDeclaration(Class(<http://ex/A>))\n)";
        assert!(
            cats(src, rt)
                .iter()
                .all(|c| *c == Category::InferredDeclaration)
        );
    }

    #[test]
    fn punning_declaration_is_unknown() {
        // rt gains DeclareNamedIndividual(A) but A is used only as a Class in src
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
        let rt = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\nDeclaration(NamedIndividual(<http://ex/A>))\n)";
        assert!(cats(src, rt).contains(&Category::Unknown));
    }

    #[test]
    fn dropped_annotation_is_annotation_loss() {
        // same axiom, annotation present in src, dropped in rt -> paired, but
        // since the .ann sets differ (one has the annotation, the other
        // doesn't) this is a real AnnotationLoss, not benign normalization.
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(Annotation(<http://ex/p> \"x\") <http://ex/A> <http://ex/B>)\n)";
        let rt = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
        assert!(cats(src, rt).iter().all(|c| *c == Category::AnnotationLoss));
    }

    #[test]
    fn residual_anonymous_individual_diff_is_blank_node_relabel_not_unknown() {
        // Two asymmetric anonymous individuals in a SameIndividual axiom:
        // Task 6's canonicalization can leave a residual ordering
        // difference here (neither side is a strict superset of the
        // other's members, so rule 3 does not apply either) — such a diff
        // item must be tagged BlankNodeRelabel, never Unknown.
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSameIndividual(<http://ex/A> _:b0)\nDeclaration(NamedIndividual(<http://ex/A>))\n)";
        let rt = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSameIndividual(<http://ex/A> _:b1)\nDeclaration(NamedIndividual(<http://ex/A>))\n)";
        let cs = cats(src, rt);
        assert!(!cs.is_empty());
        assert!(cs.iter().all(|c| *c == Category::BlankNodeRelabel));
        assert!(!cs.contains(&Category::Unknown));
    }

    #[test]
    fn anonymous_individual_in_annotation_value_is_blank_node_relabel() {
        // Anonymous individual appears only as an annotation VALUE (not as an
        // individual operand). Such items must be tagged BlankNodeRelabel, not
        // Unknown. This test verifies that the fallback_category function
        // correctly detects anonymous individuals in annotation positions
        // (AnnotationValue::AnonymousIndividual), which render as
        // `AnonymousIndividual(AnonymousIndividual("..."))` and match the
        // `AnonymousIndividual(` marker.
        //
        // We create an axiom that differs in structure (ObjectPropertyAssertion
        // vs SubClassOf) so rule 1 doesn't match on annotation stripping alone.
        // The rt version has an anonymous individual only in an annotation value.
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
        let rt = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\nObjectPropertyAssertion(Annotation(<http://ex/p> _:b0) <http://ex/r> <http://ex/x> <http://ex/y>)\n)";
        let s = read_source(Format::Ofn, src.as_bytes()).unwrap().model;
        let r = read_source(Format::Ofn, rt.as_bytes()).unwrap().model;
        let d = diff(&s, &r);
        let items = categorize(d, &s, &r);
        let cs: Vec<Category> = items.iter().map(|x| x.category).collect();
        assert!(!cs.is_empty());
        assert!(cs.iter().all(|c| *c == Category::BlankNodeRelabel));
        assert!(!cs.contains(&Category::Unknown));
    }

    fn cats_of(
        src: Vec<AnnotatedComponent<RcStr>>,
        rt: Vec<AnnotatedComponent<RcStr>>,
    ) -> Vec<Category> {
        let mut s = SetOntology::new();
        for c in src {
            s.insert(c);
        }
        let mut r = SetOntology::new();
        for c in rt {
            r.insert(c);
        }
        let d = diff(&s, &r);
        categorize(d, &s, &r)
            .into_iter()
            .map(|x| x.category)
            .collect()
    }

    fn assert_all_normalization(cs: Vec<Category>) {
        assert!(!cs.is_empty());
        assert!(cs.iter().all(|c| *c == Category::Normalization), "{cs:?}");
    }

    #[test]
    fn xsd_string_literal_read_back_as_simple_is_normalization() {
        use horned_owl::model::{
            Annotation, AnnotationAssertion, AnnotationSubject, AnnotationValue, Build,
        };
        let b = Build::new_rc();
        let mk = |literal: Literal<RcStr>| -> AnnotatedComponent<RcStr> {
            AnnotationAssertion {
                subject: AnnotationSubject::IRI(b.iri("http://ex/s")),
                ann: Annotation {
                    ap: b.annotation_property("http://ex/p"),
                    av: AnnotationValue::Literal(literal),
                    ann: Default::default(),
                },
            }
            .into()
        };
        let typed = mk(Literal::Datatype {
            literal: "x".to_string(),
            datatype_iri: b.iri(XSD_STRING),
        });
        let simple = mk(Literal::Simple {
            literal: "x".to_string(),
        });
        assert_all_normalization(cats_of(vec![typed], vec![simple]));
    }

    #[test]
    fn xsd_integer_literal_change_is_unknown() {
        use horned_owl::model::{
            Annotation, AnnotationAssertion, AnnotationSubject, AnnotationValue, Build,
        };
        let b = Build::new_rc();
        let mk = |literal: Literal<RcStr>| -> AnnotatedComponent<RcStr> {
            AnnotationAssertion {
                subject: AnnotationSubject::IRI(b.iri("http://ex/s")),
                ann: Annotation {
                    ap: b.annotation_property("http://ex/p"),
                    av: AnnotationValue::Literal(literal),
                    ann: Default::default(),
                },
            }
            .into()
        };
        let typed = mk(Literal::Datatype {
            literal: "1".to_string(),
            datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
        });
        let simple = mk(Literal::Simple {
            literal: "1".to_string(),
        });
        assert!(cats_of(vec![typed], vec![simple]).contains(&Category::Unknown));
    }

    #[test]
    fn singleton_intersection_collapsed_to_member_is_normalization() {
        use horned_owl::model::{Build, SubClassOf};
        let b = Build::new_rc();
        let a = ClassExpression::Class(b.class("http://ex/A"));
        let bc = ClassExpression::Class(b.class("http://ex/B"));
        let mk = |sup: ClassExpression<RcStr>| -> AnnotatedComponent<RcStr> {
            SubClassOf {
                sub: a.clone(),
                sup,
            }
            .into()
        };
        assert_all_normalization(cats_of(
            vec![mk(ClassExpression::ObjectIntersectionOf(vec![bc.clone()]))],
            vec![mk(bc)],
        ));
    }

    #[test]
    fn singleton_different_individuals_dropped_is_normalization() {
        use horned_owl::model::Build;
        let b = Build::new_rc();
        let c: AnnotatedComponent<RcStr> =
            DifferentIndividuals(vec![Individual::Named(b.named_individual("http://ex/i"))]).into();
        assert_all_normalization(cats_of(vec![c], vec![]));
    }

    #[test]
    fn gained_empty_ontology_id_is_normalization_only_when_source_has_none() {
        let empty: AnnotatedComponent<RcStr> = OntologyID {
            iri: None,
            viri: None,
        }
        .into();
        assert_all_normalization(cats_of(vec![], vec![empty.clone()]));

        let b = horned_owl::model::Build::new_rc();
        let named: AnnotatedComponent<RcStr> = OntologyID {
            iri: Some(b.iri("http://ex/o")),
            viri: None,
        }
        .into();
        assert!(cats_of(vec![named], vec![empty]).contains(&Category::Unknown));
    }
}
