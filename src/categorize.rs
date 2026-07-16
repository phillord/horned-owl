//! Diff categorization: classify each `RawDiff` item into a benign bucket
//! (`AnnotationNormalization`, `InferredDeclaration`, `NaryReshape`,
//! `BlankNodeRelabel`) or a reported-defect bucket (`AnnotationLoss`,
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
//! 4. `BlankNodeRelabel` — any item still unmatched by rules 1-3 whose
//!    component mentions an anonymous individual. Task 6's blank-node
//!    canonicalization is a deterministic first cut that can leave residual
//!    ordering differences for models with 2+ asymmetric anonymous
//!    individuals; those must not pollute the `Unknown` signal.
//! 5. Everything else → `Unknown`.

use crate::diff::{kind_of, RawDiff};
use crate::model::{Category, DiffItem, Side};
use horned_owl::model::{
    AnnotatedComponent, AnnotationProperty, Class, Component, DataProperty, Datatype,
    DeclareAnnotationProperty, DeclareClass, DeclareDataProperty, DeclareDatatype,
    DeclareNamedIndividual, DeclareObjectProperty, DifferentIndividuals, DisjointClasses,
    AnonymousIndividual, EquivalentClasses, NamedIndividual, ObjectProperty, RcStr,
    SameIndividual, IRI,
};
use horned_owl::ontology::set::SetOntology;
use horned_owl::visitor::immutable::{Visit, Walk};
use std::collections::{BTreeSet, HashMap, HashSet, VecDeque};

pub fn categorize(d: RawDiff, src: &SetOntology<RcStr>, _rt: &SetOntology<RcStr>) -> Vec<DiffItem> {
    let mut out = Vec::new();
    let mut lost_paired = vec![false; d.only_in_source.len()];

    // Built once, up front: rule 2 consults this for every gained
    // declaration, so it must not be recomputed per item.
    let used = used_entities(src);

    // key = component with its annotations stripped (Debug of component
    // sans ann set). Two `AnnotatedComponent`s with the same `.component`
    // but different `.ann` share a key.
    let key = |c: &AnnotatedComponent<RcStr>| format!("{:?}", c.component);

    // Rule 1 pairing index: key -> lost-item indices with that key, in
    // index order, consumed front-to-back as gained items pair with them.
    // Each key is formatted exactly once per item; the old per-comparison
    // `find` reformatted both sides for every (gained, lost) pair.
    let mut lost_by_key: HashMap<String, VecDeque<usize>> = HashMap::new();
    for (i, s) in d.only_in_source.iter().enumerate() {
        lost_by_key.entry(key(s)).or_default().push_back(i);
    }

    for g in &d.only_in_roundtrip {
        // Rule 1: pair with an unpaired lost item that shares the
        // annotation-stripped key. Equal `.ann` sets -> benign
        // AnnotationNormalization; differing `.ann` sets -> a real
        // AnnotationLoss.
        if let Some(i) = lost_by_key.get_mut(&key(g)).and_then(|q| q.pop_front()) {
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
        if is_nary_reshape(g, src) {
            out.push(item(Side::RoundTrip, g, Category::NaryReshape));
            continue;
        }
        // Rule 4 (fallback): BlankNodeRelabel, else Unknown.
        out.push(item(Side::RoundTrip, g, fallback_category(g)));
    }
    for (i, s) in d.only_in_source.iter().enumerate() {
        if !lost_paired[i] {
            out.push(item(Side::Source, s, fallback_category(s)));
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

/// If `c` is one of the four binary-capable n-ary axiom kinds, return a
/// variant tag plus the `Debug` string of each member (used as a
/// content-equality key that is robust to member ordering).
fn nary_key_and_members(c: &Component<RcStr>) -> Option<(&'static str, Vec<String>)> {
    match c {
        Component::EquivalentClasses(EquivalentClasses(v)) => Some((
            "EquivalentClasses",
            v.iter().map(|x| format!("{x:?}")).collect(),
        )),
        Component::DisjointClasses(DisjointClasses(v)) => Some((
            "DisjointClasses",
            v.iter().map(|x| format!("{x:?}")).collect(),
        )),
        Component::SameIndividual(SameIndividual(v)) => Some((
            "SameIndividual",
            v.iter().map(|x| format!("{x:?}")).collect(),
        )),
        Component::DifferentIndividuals(DifferentIndividuals(v)) => Some((
            "DifferentIndividuals",
            v.iter().map(|x| format!("{x:?}")).collect(),
        )),
        _ => None,
    }
}

/// Rule 3: `gained` is a binary EquivalentClasses/DisjointClasses/
/// SameIndividual/DifferentIndividuals axiom whose 2 members are a subset
/// of a same-variant axiom already in `src`.
fn is_nary_reshape(gained: &AnnotatedComponent<RcStr>, src: &SetOntology<RcStr>) -> bool {
    let Some((kind, members)) = nary_key_and_members(&gained.component) else {
        return false;
    };
    if members.len() != 2 {
        return false;
    }
    let gained_set: BTreeSet<String> = members.into_iter().collect();
    src.iter().any(|s| {
        nary_key_and_members(&s.component)
            .map(|(k, m)| k == kind && gained_set.is_subset(&m.into_iter().collect()))
            .unwrap_or(false)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diff::diff;
    use crate::model::Format;
    use crate::ontology::read_source;

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
        let rt  = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\nDeclaration(Class(<http://ex/A>))\n)";
        assert!(cats(src, rt)
            .iter()
            .all(|c| *c == Category::InferredDeclaration));
    }

    #[test]
    fn punning_declaration_is_unknown() {
        // rt gains DeclareNamedIndividual(A) but A is used only as a Class in src
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
        let rt  = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\nDeclaration(NamedIndividual(<http://ex/A>))\n)";
        assert!(cats(src, rt).contains(&Category::Unknown));
    }

    #[test]
    fn dropped_annotation_is_annotation_loss() {
        // same axiom, annotation present in src, dropped in rt -> paired, but
        // since the .ann sets differ (one has the annotation, the other
        // doesn't) this is a real AnnotationLoss, not benign normalization.
        let src = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(Annotation(<http://ex/p> \"x\") <http://ex/A> <http://ex/B>)\n)";
        let rt  = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\n)";
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
        let rt  = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSameIndividual(<http://ex/A> _:b1)\nDeclaration(NamedIndividual(<http://ex/A>))\n)";
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
        let rt  = "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nSubClassOf(<http://ex/A> <http://ex/B>)\nObjectPropertyAssertion(Annotation(<http://ex/p> _:b0) <http://ex/r> <http://ex/x> <http://ex/y>)\n)";
        let s = read_source(Format::Ofn, src.as_bytes()).unwrap().model;
        let r = read_source(Format::Ofn, rt.as_bytes()).unwrap().model;
        let d = diff(&s, &r);
        let items = categorize(d, &s, &r);
        let cs: Vec<Category> = items.iter().map(|x| x.category).collect();
        assert!(!cs.is_empty());
        assert!(cs.iter().all(|c| *c == Category::BlankNodeRelabel));
        assert!(!cs.contains(&Category::Unknown));
    }
}
