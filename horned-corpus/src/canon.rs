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
//! Labels must not depend on the original ids, or two reads of the same
//! document (whose readers number blank nodes differently) canonicalize
//! differently. So the order is derived from structure alone:
//!
//! 1. Colour refinement: each anonymous id starts with one shared colour;
//!    every round, each component is rendered with its ids replaced by
//!    their colours, and an id's new colour hashes its old colour with the
//!    sorted renderings of the components mentioning it. Rounds repeat until
//!    the number of distinct colours stops growing.
//! 2. Components containing anonymous ids are sorted by their final
//!    rendering, with the derived `Ord` only breaking ties.
//! 3. Walk that order (immutable visitor) and assign `_:c{n}` to each
//!    previously-unseen id, in first-visit order.
//! 4. Rewrite every `AnonymousIndividual` via horned-owl's mutable visitor,
//!    which reaches ids wherever they occur -- as an `Individual` operand
//!    or as an `AnnotationSubject`/`AnnotationValue`.
//!
//! ### Caveat
//!
//! Ids that refinement cannot tell apart (structurally symmetric nodes) are
//! ordered by the original labels, as this is not full RDF blank-node
//! canonicalization.

use horned_owl::model::{AnnotatedComponent, AnonymousIndividual, Build, MutableOntology, RcStr};
use horned_owl::ontology::set::SetOntology;
use horned_owl::visitor::immutable::{Visit, Walk};
use horned_owl::visitor::mutable::{VisitMut, WalkMut};
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::hash::{Hash, Hasher};

/// Rewrite every anonymous-individual id in `model` to a deterministic,
/// content-derived label so structurally-equal models become `Eq`-equal
/// regardless of their original blank-node labels.
pub fn canonicalize(model: SetOntology<RcStr>) -> SetOntology<RcStr> {
    let mut comps: Vec<AnnotatedComponent<RcStr>> = model.iter().cloned().collect();
    comps.sort();

    let order = structural_order(&comps);

    // Assign canonical ids in first-visit order.
    let mut map: HashMap<String, String> = HashMap::new();
    {
        let mut walk = Walk::new(AnonCollector { map: &mut map });
        for &i in &order {
            walk.annotated_component(&comps[i]);
        }
    }

    let build: Build<RcStr> = Build::new();
    let mut out = SetOntology::new();
    for mut c in comps {
        rewrite(&mut c, &map, &build);
        out.insert(c);
    }
    out
}

/// Indices into `comps` of the components containing anonymous ids, sorted
/// by structure rather than by the ids' original labels.
fn structural_order(comps: &[AnnotatedComponent<RcStr>]) -> Vec<usize> {
    let anon: Vec<usize> = (0..comps.len())
        .filter(|&i| !anon_ids(&comps[i]).is_empty())
        .collect();
    let ids: Vec<Vec<String>> = anon.iter().map(|&i| anon_ids(&comps[i])).collect();

    let mut colour: HashMap<String, String> = ids
        .iter()
        .flatten()
        .map(|id| (id.clone(), String::new()))
        .collect();
    let build: Build<RcStr> = Build::new();
    let render = |colour: &HashMap<String, String>| -> Vec<String> {
        anon.iter()
            .map(|&i| {
                let mut c = comps[i].clone();
                rewrite(&mut c, colour, &build);
                format!("{c:?}")
            })
            .collect()
    };

    let mut keys = render(&colour);
    let mut classes = 1;
    // Each round can only split colour classes, so this stops after at most
    // one round per id; the cap bounds the cost on pathological inputs.
    for _ in 0..32 {
        let mut seen: HashMap<&str, Vec<&str>> = HashMap::new();
        for (key, ids) in keys.iter().zip(&ids) {
            for id in ids {
                seen.entry(id.as_str()).or_default().push(key);
            }
        }
        let next: HashMap<String, String> = seen
            .into_iter()
            .map(|(id, mut ks)| {
                ks.sort();
                let mut h = DefaultHasher::new();
                colour[id].hash(&mut h);
                ks.hash(&mut h);
                (id.to_string(), format!("_:h{:016x}", h.finish()))
            })
            .collect();
        let n = next.values().collect::<HashSet<_>>().len();
        colour = next;
        keys = render(&colour);
        if n <= classes {
            break;
        }
        classes = n;
    }

    let mut order: Vec<usize> = (0..anon.len()).collect();
    order.sort_by(|&a, &b| keys[a].cmp(&keys[b]).then(anon[a].cmp(&anon[b])));
    order.into_iter().map(|k| anon[k]).collect()
}

fn anon_ids(c: &AnnotatedComponent<RcStr>) -> Vec<String> {
    struct Ids(BTreeSet<String>);
    impl Visit<RcStr> for Ids {
        fn visit_anonymous_individual(&mut self, ai: &AnonymousIndividual<RcStr>) {
            self.0.insert(ai.as_ref().to_string());
        }
    }
    let mut walk = Walk::new(Ids(BTreeSet::new()));
    walk.annotated_component(c);
    walk.into_visit().0.into_iter().collect()
}

fn rewrite(c: &mut AnnotatedComponent<RcStr>, map: &HashMap<String, String>, build: &Build<RcStr>) {
    let mut walk: WalkMut<RcStr, AnonRewriter> = WalkMut::new(AnonRewriter { map, build });
    walk.annotated_component(c);
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

    #[test]
    fn label_order_does_not_decide_canonical_labels() {
        // Two anonymous nodes that differ only in their annotation property:
        // swapping which original label each carries must not change the result.
        let src = |x: &str, y: &str| {
            format!(
                "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\n\
                 AnnotationAssertion(:p _:{x} \"v\")\nAnnotationAssertion(:q _:{y} \"v\")\n)"
            )
        };
        let a = read_source(Format::Ofn, src("a", "b").as_bytes())
            .unwrap()
            .model;
        let b = read_source(Format::Ofn, src("b", "a").as_bytes())
            .unwrap()
            .model;
        assert_eq!(as_set(&canonicalize(a)), as_set(&canonicalize(b)));
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
