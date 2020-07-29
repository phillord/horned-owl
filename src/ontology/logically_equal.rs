use crate::model::{
    AnnotatedAxiom,
    Axiom
};

use std::collections::HashMap;
use std::rc::Rc;
use super::indexed::{rc_unwrap_or_clone, OntologyIndex};

#[derive(Debug,Default)]
pub struct LogicallyEqualIndex(HashMap<Axiom, Rc<AnnotatedAxiom>>);

impl OntologyIndex for LogicallyEqualIndex {
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom>) -> bool {
        self.0.insert(ax.axiom.clone(), ax).is_some()
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.0.remove(&ax.axiom).map(rc_unwrap_or_clone)
    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom) -> bool {
        self.0.remove(&ax.axiom).is_some()
    }
}

impl LogicallyEqualIndex {
    pub fn logical_contains(&self, ax: &AnnotatedAxiom) -> bool {
        self.0.contains_key(&ax.axiom)
    }

    pub fn logical_get(&self, ax: &AnnotatedAxiom) -> Option<&AnnotatedAxiom> {
        self.0.get(&ax.axiom).map(|rcax| &**rcax)
    }

    pub fn logical_get_rc(&self, ax: &AnnotatedAxiom) -> Option<Rc<AnnotatedAxiom>> {
        self.0.get(&ax.axiom).cloned()
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::model::*;
    use crate::ontology::indexed::*;
    use crate::ontology::set::*;

    #[test]
    fn cons() {
        let _lei = LogicallyEqualIndex::default();
        assert!(true);
    }

    #[test]
    fn equal_retrieve() {
        // Setup
        let build = Build::new();
        let mut o = LogicallyEqualIndex::default();
        let decl1:AnnotatedAxiom =
            DeclareClass(build.class("http://www.example.com#a")).into();
        let decl2:AnnotatedAxiom =
            DeclareClass(build.class("http://www.example.com#b")).into();
        let decl3:AnnotatedAxiom =
            DeclareClass(build.class("http://www.example.com#c")).into();

        o.index_insert(Rc::new(decl1.clone()));
        o.index_insert(Rc::new(decl2.clone()));
        o.index_insert(Rc::new(decl3.clone()));

        assert!(o.logical_contains(&decl1));
        assert!(o.logical_contains(&decl2));
        assert!(o.logical_contains(&decl3));
    }

    #[test]
    fn annotation_not_equal_retrieve() {
        // Setup
        let b = Build::new();
        let mut o:TwoIndexedOntology<SetIndex,LogicallyEqualIndex> = Default::default();

        let ann = Annotation {
            ap: b.annotation_property("http://www.example.com/ap"),
            av: b.iri("http://www.example.com/av").into()
        };

        let decl1:AnnotatedAxiom =
            DeclareClass(b.class("http://www.example.com#a")).into();
        let decl2:AnnotatedAxiom =
            DeclareClass(b.class("http://www.example.com#b")).into();
        let decl3:AnnotatedAxiom =
            DeclareClass(b.class("http://www.example.com#c")).into();


        let mut decl1_a = decl1.clone();
        decl1_a.ann.insert(ann.clone());

        let mut decl2_a = decl2.clone();
        decl2_a.ann.insert(ann);

        o.insert(decl1_a.clone());
        o.insert(decl2_a.clone());
        o.insert(decl3.clone());

        assert!(!o.i().contains(&decl1));
        assert!(!o.i().contains(&decl2));
        assert!(o.i().contains(&decl3));

        assert!(o.j().logical_contains(&decl1));
        assert!(o.j().logical_contains(&decl2));
        assert!(o.j().logical_contains(&decl3));
    }

}
