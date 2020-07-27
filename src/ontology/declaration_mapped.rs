//! An index that provides rapid look up via declaration kind

use crate::model::{
    AnnotatedAxiom,
    AxiomKind, Axiom, Kinded, IRI, NamedEntityKind
};

use super::indexed::OntologyIndex;

use std::{
    collections::HashMap,
    rc::Rc,
};

#[derive(Debug, Default)]
pub struct DeclarationMappedIndex(HashMap<IRI, NamedEntityKind>);

impl DeclarationMappedIndex {
    pub fn is_annotation_property(&self, iri: &IRI) -> bool {
        match self.declaration_kind(iri) {
            Some(NamedEntityKind::AnnotationProperty) => true,
            _ => false,
        }
    }

    pub fn declaration_kind(&self, iri: &IRI) -> Option<NamedEntityKind>{
        self.0.get(iri).cloned()
            .or_else(|| crate::vocab::to_built_in_entity(iri))
    }

    fn aa_to_ne(&self, ax: &AnnotatedAxiom) -> Option<NamedEntityKind> {
        match ax.kind() {
            AxiomKind::DeclareClass |
            AxiomKind::DeclareObjectProperty |
            AxiomKind::DeclareAnnotationProperty |
            AxiomKind::DeclareDataProperty |
            AxiomKind::DeclareDatatype |
            AxiomKind::DeclareNamedIndividual => {
                match ax.clone().axiom {
                    Axiom::DeclareClass(dc) => Some(dc.0.into()),
                    Axiom::DeclareObjectProperty(op) => Some(op.0.into()),
                    Axiom::DeclareAnnotationProperty(ap) => Some(ap.0.into()),
                    Axiom::DeclareDataProperty(dp) => Some(dp.0.into()),
                    Axiom::DeclareDatatype(dt) => Some(dt.0.into()),
                    Axiom::DeclareNamedIndividual(ni) => Some(ni.0.into()),
                    _ => None
                }
            }
            _ => None
       }
    }

    fn aa_to_iri(&self, ax: &AnnotatedAxiom) -> Option<IRI> {
        match ax.kind() {
            AxiomKind::DeclareClass |
            AxiomKind::DeclareObjectProperty |
            AxiomKind::DeclareAnnotationProperty |
            AxiomKind::DeclareDataProperty |
            AxiomKind::DeclareDatatype |
            AxiomKind::DeclareNamedIndividual => {
                match ax.clone().axiom {
                    Axiom::DeclareClass(dc) => Some(dc.0.into()),
                    Axiom::DeclareObjectProperty(op) => Some(op.0.into()),
                    Axiom::DeclareAnnotationProperty(ap) => Some(ap.0.into()),
                    Axiom::DeclareDataProperty(dp) => Some(dp.0.into()),
                    Axiom::DeclareDatatype(dt) => Some(dt.0.into()),
                    Axiom::DeclareNamedIndividual(ni) => Some(ni.0.into()),
                    _ => None
                }
            }
            _ => None
        }
    }
}

macro_rules! some {
    ($body:expr) => {
        (|| Some($body))()
    };
}


impl OntologyIndex for DeclarationMappedIndex {
    fn index_insert(&mut self, ax: Rc<AnnotatedAxiom>) -> bool {
        let s = some!{
            self.0.insert(self.aa_to_iri(&*ax)?,
                          self.aa_to_ne(&*ax)?)
        };
        s.is_some()
    }

    fn index_take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom> {
        let s = some! {
            self.0.remove(&self.aa_to_iri(&*ax)?)
        };

        if s.is_some() {
            Some(ax.clone())
        }
        else {
            None
        }
    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom) -> bool {
        let s = some!{
            self.0.remove(&self.aa_to_iri(&*ax)?)
        };

        s.is_some()
    }
}


#[cfg(test)]
mod test{
    use crate::model::{AnnotatedAxiom, Build, NamedEntity, NamedEntityKind};
    use crate::ontology::indexed::OntologyIndex;
    use crate::vocab::{OWL, WithIRI};
    use super::DeclarationMappedIndex;
    fn stuff() -> (AnnotatedAxiom, AnnotatedAxiom, AnnotatedAxiom) {
        let b = Build::new();
        let c: NamedEntity = b.class("http://www.example.com/c").into();
        let o: NamedEntity = b.object_property("http://www.example.com/p").into();
        let b: NamedEntity = b.data_property("http://www.example.com/d").into();

        (c.into(), o.into(), b.into())
    }

    #[test]
    fn test_cons() {
        let _d = DeclarationMappedIndex::default();
        assert!(true);
    }

    #[test]
    fn test_insert() {
        let mut d = DeclarationMappedIndex::default();
        let s = stuff();
        assert!(d.index_insert(s.0.into()));
        assert!(d.index_insert(s.1.into()));
        assert!(d.index_insert(s.2.into()));
    }

    #[test]
    fn test_declaration() {
        let mut d = DeclarationMappedIndex::default();
        let s = stuff();
        assert!(d.index_insert(s.0.into()));
        assert!(d.index_insert(s.1.into()));
        assert!(d.index_insert(s.2.into()));

        let b = Build::new();
        assert_eq!(d.declaration_kind(&b.iri("http://www.example.com/c")),
                   Some(NamedEntityKind::Class));
        assert_eq!(d.declaration_kind(&b.iri("http://www.example.com/p")),
                   Some(NamedEntityKind::ObjectProperty));
        assert_eq!(d.declaration_kind(&b.iri("http://www.example.com/d")),
                   Some(NamedEntityKind::DataProperty));
    }

    #[test]
    fn test_declaration_builtin() {
        let d = DeclarationMappedIndex::default();
        let b = Build::new();
        assert_eq!(
            d.declaration_kind(&b.iri(OWL::TopDataProperty.iri_s())),
            Some(NamedEntityKind::DataProperty)
        );


    }
}
