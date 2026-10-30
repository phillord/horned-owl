//! The closed world of a vocabulary bundle.
//!
//! oml2owl makes the classes of a bundle disjoint wherever they share no
//! subclass. The classes form a hierarchy; a class with several parents is
//! split so that the hierarchy becomes a tree, by replacing each parent P
//! with "P and not the class". Then every class gets a subclass axiom for
//! its place in the tree and its children are made disjoint.
//!
//! This follows `io.opencaesar.closeworld` (the owl-close-world library).

use std::collections::{BTreeMap, BTreeSet};

/// A class expression, simplified as the algebra of the library does.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Ce {
    Universal,
    Empty,
    Unit(String),
    Complement(Box<Ce>),
    Difference(Box<Ce>, Box<Ce>),
    Intersection(BTreeSet<Ce>),
    Union(BTreeSet<Ce>),
}

impl Ce {
    pub fn complement(&self) -> Ce {
        match self {
            Ce::Universal => Ce::Empty,
            Ce::Empty => Ce::Universal,
            Ce::Complement(e) => (**e).clone(),
            other => Ce::Complement(Box::new(other.clone())),
        }
    }

    pub fn difference(&self, e: &Ce) -> Ce {
        if matches!(self, Ce::Empty) {
            return Ce::Empty;
        }
        let s = if matches!(e, Ce::Empty) {
            self.clone()
        } else if matches!(e, Ce::Universal) || self == e {
            Ce::Empty
        } else {
            Ce::Difference(Box::new(self.clone()), Box::new(e.clone()))
        };
        match self {
            Ce::Difference(a, b) if !matches!(s, Ce::Empty) && s != *self => {
                a.difference(&b.union(e))
            }
            _ => s,
        }
    }

    pub fn intersection(&self, e: &Ce) -> Ce {
        match (self, e) {
            (Ce::Universal, _) => e.clone(),
            (Ce::Empty, _) => Ce::Empty,
            _ if self == e => self.clone(),
            (Ce::Intersection(s), Ce::Intersection(t)) => {
                Ce::Intersection(s.union(t).cloned().collect())
            }
            (_, Ce::Intersection(_) | Ce::Empty | Ce::Universal) => e.intersection(self),
            (_, Ce::Difference(_, b)) if **b == *self => Ce::Empty,
            (Ce::Intersection(s), _) => {
                let mut s = s.clone();
                match e {
                    Ce::Intersection(t) => s.extend(t.iter().cloned()),
                    _ => {
                        s.insert(e.clone());
                    }
                }
                Ce::Intersection(s)
            }
            _ => Ce::Intersection([self.clone(), e.clone()].into()),
        }
    }

    pub fn union(&self, e: &Ce) -> Ce {
        match (self, e) {
            (Ce::Universal, _) => Ce::Universal,
            (Ce::Empty, _) => e.clone(),
            _ if self == e => self.clone(),
            (Ce::Union(s), Ce::Union(t)) => Ce::Union(s.union(t).cloned().collect()),
            (_, Ce::Union(_) | Ce::Empty | Ce::Universal) => e.union(self),
            (_, Ce::Difference(a, b)) if **b == *self => self.union(a),
            (Ce::Union(s), _) => {
                let mut s = s.clone();
                match e {
                    Ce::Union(t) => s.extend(t.iter().cloned()),
                    _ => {
                        s.insert(e.clone());
                    }
                }
                Ce::Union(s)
            }
            _ => Ce::Union([self.clone(), e.clone()].into()),
        }
    }
}

/// The axioms that close the world of a taxonomy.
#[derive(Default)]
pub struct Closure {
    /// `(subclass, superclass)`.
    pub subclasses: Vec<(Ce, Ce)>,
    /// Sets of classes that are made disjoint.
    pub disjoint: Vec<BTreeSet<Ce>>,
    /// A class and the disjoint classes that together are that class.
    pub unions: Vec<(Ce, BTreeSet<Ce>)>,
}

/// A hierarchy of class expressions, with edges from general to specific.
#[derive(Clone, Default)]
pub struct Taxonomy {
    children: BTreeMap<Ce, BTreeSet<Ce>>,
    parents: BTreeMap<Ce, BTreeSet<Ce>>,
}

impl Taxonomy {
    fn add_vertex(&mut self, v: Ce) {
        self.children.entry(v.clone()).or_default();
        self.parents.entry(v).or_default();
    }

    fn add_edge(&mut self, parent: Ce, child: Ce) {
        if parent == child {
            return;
        }
        self.add_vertex(parent.clone());
        self.add_vertex(child.clone());
        self.children
            .get_mut(&parent)
            .unwrap()
            .insert(child.clone());
        self.parents.get_mut(&child).unwrap().insert(parent);
    }

    /// A taxonomy of the named classes with an edge from each super class to
    /// its subclass. Classes that are each other's ancestors are one class.
    pub fn new(classes: &[String], edges: &[(String, String)]) -> Taxonomy {
        let mut representative: BTreeMap<String, String> =
            classes.iter().map(|c| (c.clone(), c.clone())).collect();
        // Collapse cycles: all of a strongly connected set take its first name.
        let reach = reachability(classes, edges);
        for a in classes {
            for b in classes {
                if a < b && reach[a].contains(b) && reach[b].contains(a) {
                    let r = representative[a].clone();
                    representative.insert(b.clone(), r);
                }
            }
        }
        let mut t = Taxonomy::default();
        for r in representative.values() {
            t.add_vertex(Ce::Unit(r.clone()));
        }
        for (p, c) in edges {
            if let (Some(p), Some(c)) = (representative.get(p), representative.get(c)) {
                t.add_edge(Ce::Unit(p.clone()), Ce::Unit(c.clone()));
            }
        }
        t
    }

    fn vertices(&self) -> Vec<Ce> {
        self.children.keys().cloned().collect()
    }

    fn ancestors_of(&self, v: &Ce) -> BTreeSet<Ce> {
        let mut out = BTreeSet::new();
        let mut todo: Vec<Ce> = self.parents[v].iter().cloned().collect();
        while let Some(p) = todo.pop() {
            if out.insert(p.clone()) {
                todo.extend(self.parents[&p].iter().cloned());
            }
        }
        out
    }

    fn descendants_of(&self, v: &Ce) -> BTreeSet<Ce> {
        let mut out = BTreeSet::new();
        let mut todo: Vec<Ce> = self.children[v].iter().cloned().collect();
        while let Some(c) = todo.pop() {
            if out.insert(c.clone()) {
                todo.extend(self.children[&c].iter().cloned());
            }
        }
        out
    }

    /// Parents that are not an ancestor of another parent.
    fn direct_parents_of(&self, v: &Ce) -> BTreeSet<Ce> {
        let covered: BTreeSet<Ce> = self.parents[v]
            .iter()
            .flat_map(|p| self.ancestors_of(p))
            .collect();
        self.parents[v].difference(&covered).cloned().collect()
    }

    pub fn transitive_reduction(&self) -> Taxonomy {
        let mut t = Taxonomy::default();
        for v in self.vertices() {
            t.add_vertex(v);
        }
        for (p, cs) in &self.children {
            for c in cs {
                let redundant = cs
                    .iter()
                    .any(|other| other != c && self.descendants_of(other).contains(c));
                if !redundant {
                    t.add_edge(p.clone(), c.clone());
                }
            }
        }
        t
    }

    /// Add `root` above every class that has no parent.
    pub fn root_at(&self, root: Ce) -> Taxonomy {
        let mut t = self.clone();
        t.add_vertex(root.clone());
        for (v, ps) in &self.parents {
            if ps.is_empty() {
                t.add_edge(root.clone(), v.clone());
            }
        }
        t
    }

    /// The first class, in depth first postorder, with several direct parents.
    fn multi_parent_child(&self) -> Option<Ce> {
        let mut seen = BTreeSet::new();
        let mut order = vec![];
        for start in self.vertices() {
            self.postorder(&start, &mut seen, &mut order);
        }
        order
            .into_iter()
            .find(|v| self.direct_parents_of(v).len() > 1)
    }

    fn postorder(&self, v: &Ce, seen: &mut BTreeSet<Ce>, order: &mut Vec<Ce>) {
        if !seen.insert(v.clone()) {
            return;
        }
        for c in &self.children[v] {
            self.postorder(c, seen, order);
        }
        order.push(v.clone());
    }

    /// Replace each parent P of `child` with "P and not child", and put the
    /// child under its grandparents.
    fn bypass_isolate(&self, child: &Ce) -> Taxonomy {
        let parents = self.parents[child].clone();
        let grandparents: BTreeSet<Ce> = parents
            .iter()
            .flat_map(|p| self.parents[p].iter().cloned())
            .collect();
        let replace: BTreeMap<Ce, Ce> = parents
            .iter()
            .map(|p| (p.clone(), p.difference(child)))
            .collect();
        let mut t = Taxonomy::default();
        for v in self.vertices().into_iter().filter(|v| !parents.contains(v)) {
            t.add_vertex(v);
        }
        for r in replace.values() {
            t.add_vertex(r.clone());
        }
        for (s, cs) in &self.children {
            for c in cs {
                if parents.contains(s) {
                    if c != child {
                        t.add_edge(replace[s].clone(), c.clone());
                    }
                } else if parents.contains(c) {
                    t.add_edge(s.clone(), replace[c].clone());
                } else {
                    t.add_edge(s.clone(), c.clone());
                }
            }
        }
        for gp in grandparents {
            t.add_edge(gp, child.clone());
        }
        t
    }

    /// Drop the edges into `child` that are redundant.
    fn reduce_child(&self, child: &Ce) -> Taxonomy {
        let mut t = Taxonomy::default();
        for v in self.vertices() {
            t.add_vertex(v);
        }
        for (p, cs) in &self.children {
            for c in cs.iter().filter(|c| *c != child) {
                t.add_edge(p.clone(), c.clone());
            }
        }
        for p in self.direct_parents_of(child) {
            t.add_edge(p, child.clone());
        }
        t
    }

    /// A tree with the same classes, in which no class has several parents.
    pub fn treeify(&self) -> Taxonomy {
        let mut t = self.clone();
        while let Some(child) = t.multi_parent_child() {
            t = t.bypass_isolate(&child).reduce_child(&child);
        }
        t
    }

    /// The subclass axioms `(subclass, superclass)`, and the sets of classes
    /// that are made disjoint.
    pub fn closure(&self) -> (Vec<(Ce, Ce)>, Vec<BTreeSet<Ce>>) {
        let c = self.closure_with_unions();
        (c.subclasses, c.disjoint)
    }

    /// As [`closure`](Self::closure), also giving each named class the set of
    /// children that partition it.
    pub fn closure_with_unions(&self) -> Closure {
        let tree = self.treeify();
        let mut closure = Closure::default();
        for (parent, children) in &tree.children {
            for c in children {
                closure.subclasses.push((c.clone(), parent.clone()));
            }
            if children.len() > 1 {
                closure.disjoint.push(children.clone());
            }
            if matches!(parent, Ce::Unit(_)) && !children.is_empty() {
                closure.unions.push((parent.clone(), children.clone()));
            }
        }
        closure
    }
}

fn reachability(
    classes: &[String],
    edges: &[(String, String)],
) -> BTreeMap<String, BTreeSet<String>> {
    let mut reach: BTreeMap<String, BTreeSet<String>> = classes
        .iter()
        .map(|c| (c.clone(), BTreeSet::new()))
        .collect();
    for (p, c) in edges {
        if let Some(s) = reach.get_mut(p) {
            s.insert(c.clone());
        }
    }
    // Transitive closure.
    for k in classes {
        let via_k = reach[k].clone();
        for i in classes {
            if reach[i].contains(k) {
                reach.get_mut(i).unwrap().extend(via_k.iter().cloned());
            }
        }
    }
    reach
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unit(s: &str) -> Ce {
        Ce::Unit(s.to_string())
    }

    fn taxonomy(classes: &[&str], edges: &[(&str, &str)]) -> Taxonomy {
        let classes: Vec<String> = classes.iter().map(|s| s.to_string()).collect();
        let edges: Vec<(String, String)> = edges
            .iter()
            .map(|(a, b)| (a.to_string(), b.to_string()))
            .collect();
        Taxonomy::new(&classes, &edges)
            .transitive_reduction()
            .root_at(Ce::Universal)
    }

    #[test]
    fn single_inheritance_makes_siblings_disjoint() {
        let (sub, disjoint) = taxonomy(&["A", "B", "C"], &[("A", "B"), ("A", "C")]).closure();
        assert!(sub.contains(&(unit("B"), unit("A"))));
        assert!(sub.contains(&(unit("A"), Ce::Universal)));
        assert_eq!(disjoint, vec![[unit("B"), unit("C")].into()]);
    }

    #[test]
    fn a_class_with_two_parents_is_split() {
        // C is a B and an A, so A and B may overlap, but only within C.
        let (sub, disjoint) = taxonomy(&["A", "B", "C"], &[("A", "C"), ("B", "C")]).closure();
        let a_not_c = unit("A").difference(&unit("C"));
        let b_not_c = unit("B").difference(&unit("C"));
        assert!(sub.contains(&(unit("C"), Ce::Universal)));
        assert!(sub.contains(&(a_not_c.clone(), Ce::Universal)));
        assert!(sub.contains(&(b_not_c.clone(), Ce::Universal)));
        assert!(disjoint.contains(&[a_not_c, b_not_c, unit("C")].into()));
    }

    #[test]
    fn a_shortcut_edge_is_redundant() {
        let t = taxonomy(&["A", "B", "C"], &[("A", "B"), ("B", "C"), ("A", "C")]);
        let (sub, disjoint) = t.closure();
        assert!(sub.contains(&(unit("C"), unit("B"))));
        assert!(!sub.contains(&(unit("C"), unit("A"))));
        assert!(disjoint.is_empty());
    }

    #[test]
    fn algebra() {
        let (a, b) = (unit("A"), unit("B"));
        assert_eq!(a.difference(&a), Ce::Empty);
        assert_eq!(a.difference(&Ce::Empty), a);
        assert_eq!(
            a.intersection(&a.complement()),
            Ce::Intersection([a.clone(), a.complement()].into())
        );
        assert_eq!(a.intersection(&Ce::Universal), a);
        assert_eq!(
            Ce::Universal.difference(&a),
            Ce::Difference(Box::new(Ce::Universal), Box::new(a.clone()))
        );
        assert_eq!(a.union(&b), Ce::Union([a, b].into()));
    }
}
