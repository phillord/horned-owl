//! Blank-node (anonymous-individual) canonicalization.
//!
//! horned-owl's `AnonymousIndividual<A>(pub A)` carries its node-id string as
//! part of its identity (it derives `Eq`/`Hash`/`Ord` over the tuple field),
//! so two structurally-identical ontologies that merely used different
//! blank-node labels (`_:x1` vs `_:y9`) compare as unequal. `canonicalize`
//! neutralizes that by rewriting every anonymous-individual id to a
//! deterministic, content-derived label (`_:c0`, `_:c1`, …) so structurally
//! equal models become `Eq`-equal regardless of their original labels.
//!
//! ## Ordering strategy
//!
//! There is no canonical *graph* ordering available cheaply here (that would
//! require full RDF-style graph canonicalization, e.g. blank-node
//! neighborhood hashing), so this takes the simplest deterministic scheme
//! that satisfies "same content -> same labels":
//!
//! 1. Sort all `AnnotatedComponent`s by their derived `Ord`. This is a
//!    total order that depends only on component content (see the caveat
//!    below for the one place original blank-node labels leak into it).
//! 2. Walk the sorted components in order (immutable visitor) and assign
//!    `_:c{n}` to each previously-unseen anonymous-individual id, in
//!    first-visit order.
//! 3. Rewrite every `AnonymousIndividual` in place via horned-owl's mutable
//!    visitor (`horned_owl::visitor::mutable`), which reaches anon ids
//!    wherever they occur -- as an `Individual` operand (class/property
//!    assertions, `ObjectOneOf`, `SameIndividual`, …) or as an
//!    `AnnotationSubject`/`AnnotationValue`.
//!
//! ### Caveat
//!
//! The derived `Ord` includes the anonymous individuals' pre-canon id
//! strings, so it is not a *pure* content order: if a single component
//! contains two distinct anonymous individuals whose original labels order
//! differently relative to the rest of the component, the pre-canon label
//! can (in adversarial cases) perturb the component's sort position, which
//! in turn can perturb *first-visit* order and thus the final canonical
//! labels. In practice, for the common case this task targets -- a single
//! anonymous individual's label differing between two otherwise-identical
//! models, or several anon ids whose relative order is determined by
//! surrounding non-anon content -- this still produces identical output.
//! This is a deliberate first cut (per the task brief) and not full RDF
//! blank-node canonicalization; revisit if the round-trip harness surfaces
//! false mismatches traceable to this.

use horned_owl::model::{AnnotatedComponent, AnonymousIndividual, Build, MutableOntology, RcStr};
use horned_owl::ontology::set::SetOntology;
use horned_owl::visitor::immutable::{Visit, Walk};
use horned_owl::visitor::mutable::{VisitMut, WalkMut};
use std::collections::HashMap;

/// Rewrite every anonymous-individual id in `model` to a deterministic,
/// content-derived label so structurally-equal models become `Eq`-equal
/// regardless of their original blank-node labels.
pub fn canonicalize(model: SetOntology<RcStr>) -> SetOntology<RcStr> {
    // Deterministic order: the components' derived Ord.
    let mut comps: Vec<AnnotatedComponent<RcStr>> = model.iter().cloned().collect();
    comps.sort();

    // First pass: assign canonical ids in first-visit order.
    let mut map: HashMap<String, String> = HashMap::new();
    {
        let mut walk = Walk::new(AnonCollector { map: &mut map });
        for c in &comps {
            walk.annotated_component(c);
        }
    }

    // Second pass: rewrite via a mutable visitor.
    let build: Build<RcStr> = Build::new();
    let rewriter = AnonRewriter {
        map: &map,
        build: &build,
    };
    let mut walk: WalkMut<RcStr, AnonRewriter> = WalkMut::new(rewriter);

    let mut out = SetOntology::new();
    for mut c in comps {
        walk.annotated_component(&mut c);
        out.insert(c);
    }
    out
}

/// `Visit` that assigns each previously-unseen anonymous-individual id the
/// next canonical label (`_:c0`, `_:c1`, …) as the walk reaches it.
struct AnonCollector<'a> {
    map: &'a mut HashMap<String, String>,
}

impl<'a> Visit<RcStr> for AnonCollector<'a> {
    fn visit_anonymous_individual(&mut self, ai: &AnonymousIndividual<RcStr>) {
        let n = self.map.len();
        self.map
            .entry(ai.as_ref().to_string())
            .or_insert_with(|| format!("_:c{n}"));
    }
}

/// `VisitMut` that rewrites each `AnonymousIndividual` it reaches according
/// to `map` (old id -> new canonical id), using `build` so the replacement
/// goes through horned-owl's normal anon-individual construction path.
/// Holding the ids that aren't in `map` unchanged is intentional (there
/// should be none, since `map` is built from exactly this component set),
/// but leaving it a no-op rather than panicking keeps `canonicalize` total
/// over any input.
struct AnonRewriter<'a> {
    map: &'a HashMap<String, String>,
    build: &'a Build<RcStr>,
}

impl<'a> VisitMut<RcStr> for AnonRewriter<'a> {
    fn visit_anonymous_individual(&mut self, ai: &mut AnonymousIndividual<RcStr>) {
        let current: &str = ai.as_ref();
        if let Some(new_id) = self.map.get(current) {
            *ai = self.build.anon(new_id.as_str());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Format;
    use crate::ontology::read_source;

    #[test]
    fn relabeled_anon_individuals_become_equal() {
        // same ontology, different blank-node ids
        let a = read_source(Format::Ofn, b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nClassAssertion(<http://ex/C> _:x1)\n)").unwrap().model;
        let b = read_source(Format::Ofn, b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nClassAssertion(<http://ex/C> _:y9)\n)").unwrap().model;
        assert_ne!(as_set(&a), as_set(&b)); // differ before canon
        assert_eq!(as_set(&canonicalize(a)), as_set(&canonicalize(b))); // equal after
    }

    #[test]
    fn canon_is_idempotent_and_preserves_component_count() {
        let a = read_source(Format::Ofn, b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nClassAssertion(<http://ex/C> _:x1)\nClassAssertion(<http://ex/D> _:x1)\n)").unwrap().model;
        let count_before = a.iter().count();
        let once = canonicalize(a);
        let count_after = once.iter().count();
        assert_eq!(count_before, count_after);
        // Both assertions referenced the SAME anon individual _:x1; that
        // must still be true (same canonical id) after rewriting -- i.e.
        // canonicalization must not accidentally split one anon individual
        // into two different labels.
        let ids = anon_id_set(&once);
        assert_eq!(
            ids.len(),
            1,
            "expected a single canonical anon id, got {ids:?}"
        );

        let twice = canonicalize(once.clone());
        assert_eq!(as_set(&once), as_set(&twice));
    }

    fn as_set(o: &SetOntology<RcStr>) -> std::collections::BTreeSet<String> {
        o.iter().map(|c| format!("{:?}", c)).collect()
    }

    /// The distinct anonymous-individual ids in `o`, gathered by visitor.
    fn anon_id_set(o: &SetOntology<RcStr>) -> std::collections::BTreeSet<String> {
        struct Ids(std::collections::BTreeSet<String>);
        impl Visit<RcStr> for Ids {
            fn visit_anonymous_individual(&mut self, ai: &AnonymousIndividual<RcStr>) {
                self.0.insert(ai.as_ref().to_string());
            }
        }
        let mut walk = Walk::new(Ids(Default::default()));
        for c in o.iter() {
            walk.annotated_component(c);
        }
        walk.into_visit().0
    }
}
