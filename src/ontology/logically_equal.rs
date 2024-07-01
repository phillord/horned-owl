//! An ontology that allows rapid lookup by logically equality.
use crate::model::{AnnotatedComponent, Component, ForIRI, MutableOntology, RcStr};
use crate::ontology::indexed::ForIndex;

use super::indexed::{OntologyIndex, ThreeIndexedOntology, TwoIndexedOntology};
use std::collections::HashMap;
use std::convert::AsRef;
use std::rc::Rc;

#[derive(Debug)]
pub struct LogicallyEqualIndex<A, AA>(HashMap<Component<A>, AA>);

impl<A: ForIRI, AA: ForIndex<A>> Default for LogicallyEqualIndex<A, AA> {
    fn default() -> Self {
        LogicallyEqualIndex(HashMap::new())
    }
}

impl<A: ForIRI, AA: ForIndex<A>> LogicallyEqualIndex<A, AA> {
    pub fn new() -> Self {
        LogicallyEqualIndex(HashMap::new())
    }
}

impl LogicallyEqualIndex<RcStr, Rc<AnnotatedComponent<RcStr>>> {
    pub fn new_rc() -> Self {
        LogicallyEqualIndex::new()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> OntologyIndex<A, AA> for LogicallyEqualIndex<A, AA> {
    fn index_insert(&mut self, cmp: AA) -> bool {
        self.0.insert(cmp.borrow().component.clone(), cmp).is_some()
    }

    fn index_remove(&mut self, cmp: &AnnotatedComponent<A>) -> bool {
        self.0.remove(&cmp.component).is_some()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> LogicallyEqualIndex<A, AA> {
    pub fn logical_contains(&self, cmp: &AnnotatedComponent<A>) -> bool {
        self.0.contains_key(&cmp.component)
    }

    pub fn logical_get(&self, cmp: &AnnotatedComponent<A>) -> Option<&AnnotatedComponent<A>> {
        self.0.get(&cmp.component).map(|fi| fi.borrow())
    }

    pub fn logical_get_rc(&self, cmp: &AnnotatedComponent<A>) -> Option<AA> {
        self.0.get(&cmp.component).cloned()
    }
}

impl<A: ForIRI, AA: ForIndex<A>, I: OntologyIndex<A, AA>> AsRef<LogicallyEqualIndex<A, AA>>
    for TwoIndexedOntology<A, AA, I, LogicallyEqualIndex<A, AA>>
{
    fn as_ref(&self) -> &LogicallyEqualIndex<A, AA> {
        self.j()
    }
}

impl<A: ForIRI, AA: ForIndex<A>, I, J> AsRef<LogicallyEqualIndex<A, AA>>
    for ThreeIndexedOntology<A, AA, I, J, LogicallyEqualIndex<A, AA>>
where
    I: OntologyIndex<A, AA>,
    J: OntologyIndex<A, AA>,
{
    fn as_ref(&self) -> &LogicallyEqualIndex<A, AA> {
        self.k()
    }
}

pub fn update_or_insert_logically_equal_component<A: ForIRI, AA: ForIndex<A>, O>(
    o: &mut O,
    cmp: AnnotatedComponent<A>,
) where
    O: MutableOntology<A> + AsRef<LogicallyEqualIndex<A, AA>>,
{
    if let Some(cmp) = update_logically_equal_axiom(o, cmp) {
        o.insert(cmp);
    }
}

pub fn update_logically_equal_axiom<A: ForIRI, AA: ForIndex<A>, O>(
    o: &mut O,
    mut cmp: AnnotatedComponent<A>,
) -> Option<AnnotatedComponent<A>>
where
    O: MutableOntology<A> + AsRef<LogicallyEqualIndex<A, AA>>,
{
    let lei: &LogicallyEqualIndex<_, _> = o.as_ref();
    let src = lei.logical_get_rc(&cmp);
    // Does the logically equal axiom exist
    if let Some(fi) = src {
        // Remove the rc from everywhere
        o.remove(fi.borrow());
        //dbg!(&rc);
        //dbg!(Rc::strong_count(&rc));

        // Un-rc
        let mut logical_axiom = fi.unwrap();
        // Extend it
        logical_axiom.ann.append(&mut cmp.ann);
        // Insert it
        o.insert(logical_axiom);
        None
    } else {
        // Otherwise put the one we have in
        Some(cmp)
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
        let _lei = LogicallyEqualIndex::new_rc();
        assert!(true);
    }

    #[test]
    fn equal_retrieve() {
        // Setup
        let build = Build::new_rc();
        let mut o = LogicallyEqualIndex::new();
        let decl1: AnnotatedComponent<_> =
            DeclareClass(build.class("http://www.example.com#a")).into();
        let decl2: AnnotatedComponent<_> =
            DeclareClass(build.class("http://www.example.com#b")).into();
        let decl3: AnnotatedComponent<_> =
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
        let b = Build::new_rc();
        let mut o = TwoIndexedOntology::new(SetIndex::new_rc(), LogicallyEqualIndex::new());

        let ann = Annotation {
            ap: b.annotation_property("http://www.example.com/ap"),
            av: b.iri("http://www.example.com/av").into(),
        };

        let decl1: AnnotatedComponent<_> = DeclareClass(b.class("http://www.example.com#a")).into();
        let decl2: AnnotatedComponent<_> = DeclareClass(b.class("http://www.example.com#b")).into();
        let decl3: AnnotatedComponent<_> = DeclareClass(b.class("http://www.example.com#c")).into();

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

    #[test]
    fn test_update_equal_axiom() {
        let b = Build::new_rc();
        {
            let mut o = TwoIndexedOntology::new(SetIndex::new_rc(), LogicallyEqualIndex::new());
            let ne: NamedOWLEntity<_> = b.class("http://www.example.com").into();
            let ax: Component<_> = ne.into();
            let mut dec: AnnotatedComponent<_> = ax.into();

            dec.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a1").into(),
            });

            let ne: NamedOWLEntity<_> = b.class("http://www.example.com").into();
            let ax: Component<_> = ne.into();
            let mut dec2: AnnotatedComponent<_> = ax.into();

            dec2.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a2").into(),
            });

            o.insert(dec);
            o.insert(dec2);
            assert_eq!(o.i().into_iter().count(), 2);
        }

        {
            let mut o = TwoIndexedOntology::new(SetIndex::new_rc(), LogicallyEqualIndex::new());
            let ne: NamedOWLEntity<_> = b.class("http://www.example.com").into();
            let ax: Component<_> = ne.into();
            let mut dec: AnnotatedComponent<_> = ax.into();
            dec.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a1").into(),
            });

            let ne: NamedOWLEntity<_> = b.class("http://www.example.com").into();
            let ax: Component<_> = ne.into();
            let mut dec2: AnnotatedComponent<_> = ax.into();
            dec2.ann.insert(Annotation {
                ap: b.annotation_property("http://www.example.com/p1"),
                av: b.iri("http://www.example.com/a2").into(),
            });

            o.insert(dec);
            update_logically_equal_axiom(&mut o, dec2);
            assert_eq!(o.i().into_iter().count(), 1);

            let aa = o.i().into_iter().next().unwrap();

            assert_eq!(aa.ann.len(), 2);
        }
    }
}
