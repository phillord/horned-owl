//! An index that provides rapid look up via declaration kind

use crate::model::{AnnotatedAxiom, Axiom, AxiomKind, Kinded, NamedEntityKind, IRI, ForIRI};

use super::indexed::ForIndex;
use super::indexed::OntologyIndex;

use std::{collections::HashMap, rc::Rc};

#[derive(Debug, Default)]
pub struct DeclarationMappedIndex<A: ForIRI, AA: ForIndex<A>>(HashMap<IRI<A>, NamedEntityKind>);

impl<A: ForIRI, AA: ForIndex<A>> DeclarationMappedIndex<A, AA> {
    pub fn new() -> DeclarationMappedIndex<A, AA> {
        DeclarationMappedIndex(HashMap::new())
    }

    pub fn is_annotation_property(&self, iri: &IRI<A>) -> bool {
        match self.declaration_kind(iri) {
            Some(NamedEntityKind::AnnotationProperty) => true,
            _ => false,
        }
    }

    pub fn declaration_kind(&self, iri: &IRI<A>) -> Option<NamedEntityKind> {
        self.0
            .get(iri)
            .cloned()
            .or_else(|| crate::vocab::to_built_in_entity(iri))
    }

    fn aa_to_ne(&self, ax: &AnnotatedAxiom<A>) -> Option<NamedEntityKind> {
        match ax.kind() {
            AxiomKind::DeclareClass
            | AxiomKind::DeclareObjectProperty
            | AxiomKind::DeclareAnnotationProperty
            | AxiomKind::DeclareDataProperty
            | AxiomKind::DeclareDatatype
            | AxiomKind::DeclareNamedIndividual => match ax.clone().axiom {
                Axiom::DeclareClass(dc) => Some(dc.0.into()),
                Axiom::DeclareObjectProperty(op) => Some(op.0.into()),
                Axiom::DeclareAnnotationProperty(ap) => Some(ap.0.into()),
                Axiom::DeclareDataProperty(dp) => Some(dp.0.into()),
                Axiom::DeclareDatatype(dt) => Some(dt.0.into()),
                Axiom::DeclareNamedIndividual(ni) => Some(ni.0.into()),
                _ => None,
            },
            _ => None,
        }
    }

    fn aa_to_iri(&self, ax: &AnnotatedAxiom<A>) -> Option<IRI<A>> {
        match ax.kind() {
            AxiomKind::DeclareClass
            | AxiomKind::DeclareObjectProperty
            | AxiomKind::DeclareAnnotationProperty
            | AxiomKind::DeclareDataProperty
            | AxiomKind::DeclareDatatype
            | AxiomKind::DeclareNamedIndividual => match ax.clone().axiom {
                Axiom::DeclareClass(dc) => Some(dc.0.into()),
                Axiom::DeclareObjectProperty(op) => Some(op.0.into()),
                Axiom::DeclareAnnotationProperty(ap) => Some(ap.0.into()),
                Axiom::DeclareDataProperty(dp) => Some(dp.0.into()),
                Axiom::DeclareDatatype(dt) => Some(dt.0.into()),
                Axiom::DeclareNamedIndividual(ni) => Some(ni.0.into()),
                _ => None,
            },
            _ => None,
        }
    }
}

macro_rules! some {
    ($body:expr) => {
        (|| Some($body))()
    };
}

impl<A: ForIRI, AA: ForIndex<A>> OntologyIndex<A, AA> for DeclarationMappedIndex<A, AA> {
    fn index_insert(&mut self, ax: AA) -> bool {
        let s = some! {
            self.0.insert(self.aa_to_iri(&*ax)?,
                          self.aa_to_ne(&*ax)?)
        };
        s.is_some()
    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom<A>) -> bool {
        let s = some! {
            self.0.remove(&self.aa_to_iri(&*ax)?)
        };

        s.is_some()
    }
}

impl DeclarationMappedIndex<Rc<str>, Rc<AnnotatedAxiom<Rc<str>>>> {
    pub fn new_rc() -> Self {
        Self::new()
    }
}


#[cfg(test)]
mod test {
    use std::rc::Rc;

    use super::DeclarationMappedIndex;
    use crate::model::{AnnotatedAxiom, Build, NamedEntity, NamedEntityKind};
    use crate::ontology::indexed::OntologyIndex;
    use crate::vocab::{WithIRI, OWL};
    fn stuff() -> (AnnotatedAxiom<Rc<str>>, AnnotatedAxiom<Rc<str>>, AnnotatedAxiom<Rc<str>>) {
        let b = Build::new_rc();
        let c: NamedEntity<_> = b.class("http://www.example.com/c").into();
        let o: NamedEntity<_> = b.object_property("http://www.example.com/p").into();
        let b: NamedEntity<_> = b.data_property("http://www.example.com/d").into();

        (c.into(), o.into(), b.into())
    }

    #[test]
    fn test_cons() {
        let _d:DeclarationMappedIndex<Rc<str>> = DeclarationMappedIndex::new();
        assert!(true);
    }

    #[test]
    fn test_insert() {
        let mut d = DeclarationMappedIndex::new();
        let s = stuff();
        assert!(d.index_insert(s.0.into()));
        assert!(d.index_insert(s.1.into()));
        assert!(d.index_insert(s.2.into()));
    }

    #[test]
    fn test_declaration() {
        let mut d = DeclarationMappedIndex::new();
        let s = stuff();
        assert!(d.index_insert(s.0.into()));
        assert!(d.index_insert(s.1.into()));
        assert!(d.index_insert(s.2.into()));

        let b = Build::new();
        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/c")),
            Some(NamedEntityKind::Class)
        );
        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/p")),
            Some(NamedEntityKind::ObjectProperty)
        );
        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/d")),
            Some(NamedEntityKind::DataProperty)
        );
    }

    #[test]
    fn test_declaration_builtin() {
        let d = DeclarationMappedIndex::new();
        let b = Build::new_rc();
        assert_eq!(
            d.declaration_kind(&b.iri(OWL::TopDataProperty.iri_str())),
            Some(NamedEntityKind::DataProperty)
        );
    }
}
