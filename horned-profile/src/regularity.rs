//! Role-hierarchy (RBox) regularity.
//!
//! OWL 2 DL requires the object-property hierarchy to be *regular*: there
//! must exist a strict partial order `≺` (consistent with inverses) such
//! that every `SubObjectPropertyOf(ObjectPropertyChain(R1, ..., Rn), R)`
//! axiom (n >= 2) satisfies one of six shapes from the spec -- the same
//! regularity condition originally from Horrocks & Sattler's role
//! hierarchies work that SROIQ (and so OWL 2 DL) is built on.
//!
//! **Scope note**: this module does not implement the full six-shape
//! grammar check. It implements the part of it that's unconditionally
//! correct on its own: build a graph with an edge from each chain member
//! `Ri` to `R` (for `Ri != R`), and detect cycles. A cycle in this graph
//! can never be satisfied by *any* strict partial order, so reporting one
//! is always a genuine violation, regardless of the finer shape rules.
//! What's not yet checked is whether a *non*-cyclic chain still fails one
//! of the six shapes in some other way -- a real but narrower gap than "no
//! regularity checking at all", left for a follow-up once this is
//! exercised against real ontologies.
//!
//! Each property and its declared inverse are folded to the same graph
//! node (the underlying named `ObjectProperty`), matching the spec's
//! requirement that `≺` be consistent with inverses -- a cycle through a
//! mix of a property and its inverse is exactly as real a violation as one
//! that never uses an inverse.

use std::collections::{HashMap, HashSet};

use horned_owl::model::{
    Component, ForIRI, ObjectProperty, ObjectPropertyExpression, Ontology,
    SubObjectPropertyExpression,
};

/// Returns every cycle found in the property-chain graph, each as the
/// sequence of properties forming it (first element repeated as the last,
/// closing the loop).
///
/// Empty if the role hierarchy's chain axioms are acyclic.
pub fn chain_cycles<A: ForIRI, O: Ontology<A>>(o: &O) -> Vec<Vec<ObjectProperty<A>>> {
    let mut edges: HashMap<ObjectProperty<A>, HashSet<ObjectProperty<A>>> = HashMap::new();

    for ac in o.iter() {
        if let Component::SubObjectPropertyOf(sopo) = &ac.component
            && let SubObjectPropertyExpression::ObjectPropertyChain(chain) = &sopo.sub
            && chain.len() >= 2
        {
            let sup = base(&sopo.sup);
            for member in chain {
                let member = base(member);
                if member != sup {
                    edges.entry(member).or_default().insert(sup.clone());
                }
            }
        }
    }

    find_cycles(&edges)
}

/// Returns the underlying named `ObjectProperty` for `ope`, folding an
/// inverse to the same property (see this module's doc comment on why
/// inverses share a graph node).
fn base<A: ForIRI>(ope: &ObjectPropertyExpression<A>) -> ObjectProperty<A> {
    match ope {
        ObjectPropertyExpression::ObjectProperty(p) => p.clone(),
        ObjectPropertyExpression::InverseObjectProperty(p) => p.clone(),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Colour {
    Gray,
    Black,
}

/// Returns every cycle in the directed graph described by `edges` (source
/// property -> set of target properties), via depth-first search from
/// each unvisited node.
fn find_cycles<A: ForIRI>(
    edges: &HashMap<ObjectProperty<A>, HashSet<ObjectProperty<A>>>,
) -> Vec<Vec<ObjectProperty<A>>> {
    let mut colour: HashMap<ObjectProperty<A>, Colour> = HashMap::new();
    let mut cycles: Vec<Vec<ObjectProperty<A>>> = Vec::new();

    // Every node that appears anywhere (as a source or a target), not just
    // sources -- a leaf target with no outgoing edges of its own still
    // needs to be visited (harmlessly finds nothing) so it's not skipped
    // as if unreachable.
    let mut nodes: Vec<ObjectProperty<A>> = edges.keys().cloned().collect();
    for succs in edges.values() {
        for s in succs {
            if !edges.contains_key(s) {
                nodes.push(s.clone());
            }
        }
    }

    for start in &nodes {
        if colour.contains_key(start) {
            continue;
        }
        let mut stack = Vec::new();
        visit(start, edges, &mut colour, &mut stack, &mut cycles);
    }

    cycles
}

/// Depth-first-searches from `node`, recording any cycle found onto
/// `cycles`.
///
/// Standard gray/black DFS cycle detection: a gray node reached again is a
/// cycle, closed by taking the stack from that node's position onward.
fn visit<A: ForIRI>(
    node: &ObjectProperty<A>,
    edges: &HashMap<ObjectProperty<A>, HashSet<ObjectProperty<A>>>,
    colour: &mut HashMap<ObjectProperty<A>, Colour>,
    stack: &mut Vec<ObjectProperty<A>>,
    cycles: &mut Vec<Vec<ObjectProperty<A>>>,
) {
    colour.insert(node.clone(), Colour::Gray);
    stack.push(node.clone());

    if let Some(succs) = edges.get(node) {
        for succ in succs {
            match colour.get(succ) {
                None => visit(succ, edges, colour, stack, cycles),
                Some(Colour::Gray) => {
                    let pos = stack
                        .iter()
                        .position(|n| n == succ)
                        .expect("a gray node must be on the current stack");
                    let mut cycle = stack[pos..].to_vec();
                    cycle.push(succ.clone());
                    cycles.push(cycle);
                }
                Some(Colour::Black) => {}
            }
        }
    }

    stack.pop();
    colour.insert(node.clone(), Colour::Black);
}

#[cfg(test)]
mod test {
    use super::chain_cycles;
    use horned_owl::model::{
        Build, MutableOntology, ObjectPropertyExpression, SubObjectPropertyExpression,
        SubObjectPropertyOf,
    };
    use horned_owl::ontology::set::SetOntology;

    // The classic, always-fine shape: R = R1 = ... = Rn (this is exactly
    // how TransitiveObjectProperty(R) can be expressed as a chain axiom
    // instead) -- no edges should be added at all since every chain
    // member equals the super-property.
    #[test]
    fn self_chain_is_not_a_cycle() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let r: ObjectPropertyExpression<_> = b.object_property("http://example.com/r").into();
        o.insert(SubObjectPropertyOf {
            sup: r.clone(),
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![r.clone(), r.clone()]),
        });

        assert!(chain_cycles(&o).is_empty());
    }

    // A single chain axiom (R1, R2) -> R, with R1/R2 distinct from R and
    // from each other, is acyclic (R1 -> R, R2 -> R, no path back).
    #[test]
    fn simple_chain_is_acyclic() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p1: ObjectPropertyExpression<_> = b.object_property("http://example.com/p1").into();
        let p2: ObjectPropertyExpression<_> = b.object_property("http://example.com/p2").into();
        let q: ObjectPropertyExpression<_> = b.object_property("http://example.com/q").into();
        o.insert(SubObjectPropertyOf {
            sup: q,
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![p1, p2]),
        });

        assert!(chain_cycles(&o).is_empty());
    }

    // A genuine cycle: R is a chain member for S, and S is (via another
    // chain axiom) a chain member for R -- unsatisfiable by any strict
    // partial order.
    #[test]
    fn two_property_cycle_is_detected() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let r: ObjectPropertyExpression<_> = b.object_property("http://example.com/r").into();
        let s: ObjectPropertyExpression<_> = b.object_property("http://example.com/s").into();
        let x: ObjectPropertyExpression<_> = b.object_property("http://example.com/x").into();
        // R o X <= S
        o.insert(SubObjectPropertyOf {
            sup: s.clone(),
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![r.clone(), x.clone()]),
        });
        // S o X <= R
        o.insert(SubObjectPropertyOf {
            sup: r,
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![s, x]),
        });

        let cycles = chain_cycles(&o);
        assert_eq!(cycles.len(), 1);
        assert!(cycles[0].len() >= 2);
    }

    // A cycle expressed through an inverse must still be caught -- R and
    // its inverse are the same graph node.
    #[test]
    fn cycle_through_inverse_is_detected() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let r_op = b.object_property("http://example.com/r");
        let s_op = b.object_property("http://example.com/s");
        let r: ObjectPropertyExpression<_> = r_op.clone().into();
        let inv_r = ObjectPropertyExpression::InverseObjectProperty(r_op);
        let s: ObjectPropertyExpression<_> = s_op.clone().into();
        let x: ObjectPropertyExpression<_> = b.object_property("http://example.com/x").into();

        o.insert(SubObjectPropertyOf {
            sup: s.clone(),
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![r, x.clone()]),
        });
        // Using inv(R) here instead of R still closes the same cycle,
        // since inv(R) and R share a graph node.
        o.insert(SubObjectPropertyOf {
            sup: inv_r,
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![s, x]),
        });

        assert_eq!(chain_cycles(&o).len(), 1);
    }
}
