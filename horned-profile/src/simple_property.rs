//! Simple/composite object-property analysis.
//!
//! Several OWL 2 DL global restrictions (cardinality restrictions,
//! `ObjectHasSelf`, disjointness, and a handful of property-characteristic
//! axioms) may only use *simple* object properties. Per the OWL 2 spec, an
//! object property expression is **composite** (non-simple) if:
//!
//! - it's declared `TransitiveObjectProperty`; or
//! - it's the super-property in a `SubObjectPropertyOf(ObjectPropertyChain(...),
//!   P)` axiom whose chain has length >= 2; or
//! - any of its declared *sub*-properties is composite.
//!
//! Compositeness propagates from sub-property up to super-property only,
//! never the reverse -- a property is not made composite merely by being a
//! sub-property of something transitive (see this module's tests).
//! `ObjectPropertyExpression::InverseObjectProperty` mirrors its base
//! property's composite status automatically, and
//! `InverseObjectProperties`/`EquivalentObjectProperties` axioms unify the
//! composite status of the properties they relate.

use std::collections::{HashMap, HashSet};

use horned_owl::model::{
    Component, ForIRI, ObjectPropertyExpression, Ontology, SubObjectPropertyExpression,
};

/// Which object-property expressions in an ontology are composite
/// (non-simple), per this module's rules.
pub struct SimplePropertyAnalysis<A: ForIRI>(HashSet<ObjectPropertyExpression<A>>);

impl<A: ForIRI> SimplePropertyAnalysis<A> {
    /// Builds a `SimplePropertyAnalysis` for `o`, by finding directly
    /// composite properties and propagating compositeness up the
    /// sub-property hierarchy to a fixpoint.
    pub fn from_ontology<O: Ontology<A>>(o: &O) -> Self {
        let mut composite: HashSet<ObjectPropertyExpression<A>> = HashSet::new();
        // sub-property -> declared super-properties, from plain
        // (non-chain) SubObjectPropertyOf and both directions of
        // EquivalentObjectProperties/InverseObjectProperties.
        let mut edges: HashMap<ObjectPropertyExpression<A>, Vec<ObjectPropertyExpression<A>>> =
            HashMap::new();

        for ac in o.iter() {
            match &ac.component {
                Component::TransitiveObjectProperty(top) => {
                    mark_composite(&mut composite, &top.0);
                }
                Component::SubObjectPropertyOf(sopo) => match &sopo.sub {
                    SubObjectPropertyExpression::ObjectPropertyChain(chain) if chain.len() >= 2 => {
                        mark_composite(&mut composite, &sopo.sup);
                    }
                    // A degenerate length-<=1 chain carries no
                    // composition; treat length 1 as a plain sub-property
                    // edge (length 0 shouldn't occur in practice).
                    SubObjectPropertyExpression::ObjectPropertyChain(chain) => {
                        if let Some(single) = chain.first() {
                            add_edge(&mut edges, single.clone(), sopo.sup.clone());
                        }
                    }
                    SubObjectPropertyExpression::ObjectPropertyExpression(sub_ope) => {
                        add_edge(&mut edges, sub_ope.clone(), sopo.sup.clone());
                    }
                },
                Component::EquivalentObjectProperties(eq) => {
                    for a in &eq.0 {
                        for b in &eq.0 {
                            if a != b {
                                add_edge(&mut edges, a.clone(), b.clone());
                            }
                        }
                    }
                }
                Component::InverseObjectProperties(iop) => {
                    // p and inv(q) denote the same relation; likewise q
                    // and inv(p) -- unify their compositeness both ways,
                    // same as an EquivalentObjectProperties pair.
                    let p: ObjectPropertyExpression<A> = iop.0.clone().into();
                    let q: ObjectPropertyExpression<A> = iop.1.clone().into();
                    let inv_p = inverse_of(&p);
                    let inv_q = inverse_of(&q);
                    add_edge(&mut edges, p.clone(), inv_q.clone());
                    add_edge(&mut edges, inv_q, p);
                    add_edge(&mut edges, q.clone(), inv_p.clone());
                    add_edge(&mut edges, inv_p, q);
                }
                _ => {}
            }
        }

        // Fixpoint: propagate compositeness from sub-property to
        // super-property until nothing changes.
        loop {
            let mut changed = false;
            for (sub, sups) in &edges {
                if composite.contains(sub) {
                    for sup in sups {
                        if !composite.contains(sup) {
                            mark_composite(&mut composite, sup);
                            changed = true;
                        }
                    }
                }
            }
            if !changed {
                break;
            }
        }

        SimplePropertyAnalysis(composite)
    }

    /// Returns `true` if `ope` is simple (not composite).
    pub fn is_simple(&self, ope: &ObjectPropertyExpression<A>) -> bool {
        !self.0.contains(ope)
    }

    /// Returns `true` if `ope` is composite (not simple).
    pub fn is_composite(&self, ope: &ObjectPropertyExpression<A>) -> bool {
        !self.is_simple(ope)
    }
}

/// Records a sub-property -> super-property edge from `sub` to `sup` in
/// `edges`, for later fixpoint propagation.
fn add_edge<A: ForIRI>(
    edges: &mut HashMap<ObjectPropertyExpression<A>, Vec<ObjectPropertyExpression<A>>>,
    sub: ObjectPropertyExpression<A>,
    sup: ObjectPropertyExpression<A>,
) {
    edges.entry(sub).or_default().push(sup);
}

/// Marks `ope`, and its inverse, as composite in `composite`.
fn mark_composite<A: ForIRI>(
    composite: &mut HashSet<ObjectPropertyExpression<A>>,
    ope: &ObjectPropertyExpression<A>,
) {
    composite.insert(ope.clone());
    composite.insert(inverse_of(ope));
}

/// Returns the inverse of `ope` (`ObjectProperty` <-> `InverseObjectProperty`).
fn inverse_of<A: ForIRI>(ope: &ObjectPropertyExpression<A>) -> ObjectPropertyExpression<A> {
    match ope {
        ObjectPropertyExpression::ObjectProperty(p) => {
            ObjectPropertyExpression::InverseObjectProperty(p.clone())
        }
        ObjectPropertyExpression::InverseObjectProperty(p) => {
            ObjectPropertyExpression::ObjectProperty(p.clone())
        }
    }
}

#[cfg(test)]
mod test {
    use super::SimplePropertyAnalysis;
    use horned_owl::model::{
        Build, InverseObjectProperties, MutableOntology, ObjectPropertyExpression,
        SubObjectPropertyExpression, SubObjectPropertyOf, TransitiveObjectProperty,
    };
    use horned_owl::ontology::set::SetOntology;

    // Direct transitive: a directly-transitive property is composite.
    #[test]
    fn direct_transitive_is_composite() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p: ObjectPropertyExpression<_> = b.object_property("http://example.com/p").into();
        o.insert(TransitiveObjectProperty(p.clone()));

        let a = SimplePropertyAnalysis::from_ontology(&o);
        assert!(a.is_composite(&p));
        assert!(!a.is_simple(&p));
    }

    // Transitive-via-chain: a property that is the super-property of a
    // property chain of length >= 2 is composite, even though it's never
    // itself declared TransitiveObjectProperty.
    #[test]
    fn super_of_chain_is_composite() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p1: ObjectPropertyExpression<_> = b.object_property("http://example.com/p1").into();
        let p2: ObjectPropertyExpression<_> = b.object_property("http://example.com/p2").into();
        let q: ObjectPropertyExpression<_> = b.object_property("http://example.com/q").into();
        o.insert(SubObjectPropertyOf {
            sup: q.clone(),
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![p1.clone(), p2.clone()]),
        });

        let a = SimplePropertyAnalysis::from_ontology(&o);
        assert!(a.is_composite(&q));
        // The chain members themselves are plain, unrelated properties --
        // nothing about being used inside someone else's chain makes them
        // composite.
        assert!(a.is_simple(&p1));
        assert!(a.is_simple(&p2));
    }

    // Transitive-via-sub-property-of-transitive: being a *sub*-property of
    // a transitive property must NOT make a property composite --
    // compositeness only propagates from sub-property up to
    // super-property, never downward. This is the natural bug to write by
    // accident (propagating the wrong direction), so it's tested
    // explicitly rather than just being implied by the other two cases.
    #[test]
    fn sub_property_of_transitive_stays_simple() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let has_ancestor: ObjectPropertyExpression<_> =
            b.object_property("http://example.com/hasAncestor").into();
        let has_parent: ObjectPropertyExpression<_> =
            b.object_property("http://example.com/hasParent").into();
        o.insert(TransitiveObjectProperty(has_ancestor.clone()));
        o.insert(SubObjectPropertyOf {
            sup: has_ancestor.clone(),
            sub: SubObjectPropertyExpression::ObjectPropertyExpression(has_parent.clone()),
        });

        let a = SimplePropertyAnalysis::from_ontology(&o);
        assert!(a.is_composite(&has_ancestor));
        assert!(a.is_simple(&has_parent));
    }

    // A property declared composite propagates that status up through
    // multiple levels of plain SubObjectPropertyOf, and to its own inverse.
    #[test]
    fn compositeness_propagates_transitively_up_and_to_inverse() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p: ObjectPropertyExpression<_> = b.object_property("http://example.com/p").into();
        let q: ObjectPropertyExpression<_> = b.object_property("http://example.com/q").into();
        let r: ObjectPropertyExpression<_> = b.object_property("http://example.com/r").into();
        o.insert(TransitiveObjectProperty(p.clone()));
        o.insert(SubObjectPropertyOf {
            sup: q.clone(),
            sub: SubObjectPropertyExpression::ObjectPropertyExpression(p.clone()),
        });
        o.insert(SubObjectPropertyOf {
            sup: r.clone(),
            sub: SubObjectPropertyExpression::ObjectPropertyExpression(q.clone()),
        });

        let a = SimplePropertyAnalysis::from_ontology(&o);
        assert!(a.is_composite(&p));
        assert!(a.is_composite(&q));
        assert!(a.is_composite(&r));

        let inv_p = match &p {
            ObjectPropertyExpression::ObjectProperty(op) => {
                ObjectPropertyExpression::InverseObjectProperty(op.clone())
            }
            _ => unreachable!(),
        };
        assert!(a.is_composite(&inv_p));
    }

    // InverseObjectProperties(p, q) unifies p and q's compositeness: if p
    // is composite, so is q, since q denotes the same relation as inv(p).
    #[test]
    fn inverse_object_properties_unifies_compositeness() {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let p = b.object_property("http://example.com/p");
        let q = b.object_property("http://example.com/q");
        o.insert(TransitiveObjectProperty(
            ObjectPropertyExpression::ObjectProperty(p.clone()),
        ));
        o.insert(InverseObjectProperties(p.clone(), q.clone()));

        let a = SimplePropertyAnalysis::from_ontology(&o);
        assert!(a.is_composite(&ObjectPropertyExpression::ObjectProperty(q)));
    }

    // An ordinary, unrelated property with no transitive/chain involvement
    // at all is simple.
    #[test]
    fn unrelated_property_is_simple() {
        let b = Build::new_rc();
        let o: SetOntology<_> = SetOntology::new();
        let p: ObjectPropertyExpression<_> = b.object_property("http://example.com/p").into();

        let a = SimplePropertyAnalysis::from_ontology(&o);
        assert!(a.is_simple(&p));
    }
}
