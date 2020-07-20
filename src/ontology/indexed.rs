use crate::model::{AnnotatedAxiom, MutableOntology, OntologyID, Ontology};
use std::rc::Rc;

pub trait OntologyIndex {
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom>) -> bool;
    fn index_remove(&mut self, ax: &AnnotatedAxiom) -> bool {
        self.index_take(ax).is_some()
    }
    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom>;
}

#[derive(Default, Debug, Eq, PartialEq)]
pub struct OneIndexedOntology<I:OntologyIndex>(I, OntologyID);

impl<I:OntologyIndex> OneIndexedOntology<I> {
    pub fn new(i: I) -> Self {
        OneIndexedOntology(i, Default::default())
    }

    pub fn i(&self) -> &I {
        &self.0
    }

    pub fn index(self) -> I {
        self.0
    }
}

impl<I:OntologyIndex> Ontology for OneIndexedOntology<I> {
    fn id(&self) -> &OntologyID {
        &self.1
    }

    fn mut_id(&mut self) -> &mut OntologyID {
        &mut self.1
    }
}

impl<I:OntologyIndex> MutableOntology for OneIndexedOntology<I> {
    fn insert<A:Into<AnnotatedAxiom>>(&mut self, ax: A) -> bool
    {
        let rc = Rc::new(ax.into());
        self.0.index_insert(rc)
    }

    fn take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.0.index_take(ax)
    }
}

pub struct TwoIndexedOntology<I:OntologyIndex, J: OntologyIndex> (I, J, OntologyID);

impl<I:OntologyIndex, J: OntologyIndex> TwoIndexedOntology<I, J> {
    pub fn new(i: I, j: J) -> Self {
        TwoIndexedOntology(i, j, Default::default())
    }

    pub fn i(&self) -> &I {
        &self.0
    }

    pub fn j(&self) -> &J {
        &self.1
    }
}

impl<I:OntologyIndex, J:OntologyIndex> Ontology for TwoIndexedOntology<I, J> {
    fn id(&self) -> &OntologyID {
        &self.2
    }

    fn mut_id(&mut self) -> &mut OntologyID {
        &mut self.2
    }
}

impl<I:OntologyIndex, J:OntologyIndex> MutableOntology for TwoIndexedOntology<I, J> {
    fn insert<A:Into<AnnotatedAxiom>>(&mut self, ax: A) -> bool {
        let rc = Rc::new(ax.into());
        self.index_insert(rc)
    }

    fn take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        self.index_take(ax)
    }
}

impl<I: OntologyIndex, J: OntologyIndex> OntologyIndex for TwoIndexedOntology<I, J> {
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom>) -> bool {
        let rtn = self.0.index_insert(ax.clone());
        // Don't short cirtuit
        self.1.index_insert(ax) || rtn
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        let rtn = self.0.index_take(ax);
        self.1.index_take(ax).or(rtn)
    }
}

/*
pub struct ThreeIndexedOntology<I:OntologyIndex,
                                J: OntologyIndex,
                                K: OntologyIndex>
    (TwoIndexedOntology<I, TwoIndexedOntology<J, K>> );
*/
