//! An index that provides rapid look up via declaration kind

use crate::model::{
    AnnotatedComponent, Component, ComponentKind, ForIRI, Kinded, NamedEntityKind,
    NamedOWLEntityKind, RcAnnotatedComponent, RcStr, IRI,
};

use super::indexed::ForIndex;
use super::indexed::OntologyIndex;

use std::collections::HashMap;
use std::collections::HashSet;
use std::marker::PhantomData;

#[derive(Debug)]
pub struct DeclarationMappedIndex<A, AA>(
    HashMap<IRI<A>, NamedEntityKind>,
    HashSet<IRI<A>>,
    PhantomData<AA>,
);

impl<A: ForIRI, AA: ForIndex<A>> DeclarationMappedIndex<A, AA> {
    pub fn new() -> DeclarationMappedIndex<A, AA> {
        DeclarationMappedIndex(HashMap::new(), HashSet::new(), Default::default())
    }

    pub fn is_annotation_property(&self, iri: &IRI<A>) -> bool {
        matches! {
            self.declaration_kind(iri),
            Some(NamedOWLEntityKind::AnnotationProperty)
        }
    }

    pub fn declaration_kind(&self, iri: &IRI<A>) -> Option<NamedOWLEntityKind> {
        self.kind(iri).map(|e| e.as_owl()).flatten()
    }

    pub fn kind(&self, iri: &IRI<A>) -> Option<NamedEntityKind> {
        self.0
            .get(iri)
            .cloned()
            .or_else(|| crate::vocab::to_built_in_entity(iri).map(|e| e.into()))
    }

    pub fn puns(&self) -> &HashSet<IRI<A>> {
        &self.1
    }

    fn aa_to_ne(&self, ax: &AnnotatedComponent<A>) -> Option<NamedEntityKind> {
        match ax.kind() {
            ComponentKind::DeclareClass
            | ComponentKind::DeclareObjectProperty
            | ComponentKind::DeclareAnnotationProperty
            | ComponentKind::DeclareDataProperty
            | ComponentKind::DeclareDatatype
            | ComponentKind::DeclareNamedIndividual => match ax.clone().component {
                Component::DeclareClass(dc) => Some(dc.0.into()),
                Component::DeclareObjectProperty(op) => Some(op.0.into()),
                Component::DeclareAnnotationProperty(ap) => Some(ap.0.into()),
                Component::DeclareDataProperty(dp) => Some(dp.0.into()),
                Component::DeclareDatatype(dt) => Some(dt.0.into()),
                Component::DeclareNamedIndividual(ni) => Some(ni.0.into()),
                _ => None,
            },
            _ => None,
        }
    }

    fn aa_to_iri(&self, ax: &AnnotatedComponent<A>) -> Option<IRI<A>> {
        match ax.kind() {
            ComponentKind::DeclareClass
            | ComponentKind::DeclareObjectProperty
            | ComponentKind::DeclareAnnotationProperty
            | ComponentKind::DeclareDataProperty
            | ComponentKind::DeclareDatatype
            | ComponentKind::DeclareNamedIndividual => match ax.clone().component {
                Component::DeclareClass(dc) => Some(dc.0.into()),
                Component::DeclareObjectProperty(op) => Some(op.0.into()),
                Component::DeclareAnnotationProperty(ap) => Some(ap.0.into()),
                Component::DeclareDataProperty(dp) => Some(dp.0.into()),
                Component::DeclareDatatype(dt) => Some(dt.0.into()),
                Component::DeclareNamedIndividual(ni) => Some(ni.0.into()),
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

impl<A, AA> Default for DeclarationMappedIndex<A, AA> {
    fn default() -> Self {
        DeclarationMappedIndex(Default::default(), Default::default(), Default::default())
    }
}

impl<A: ForIRI, AA: ForIndex<A>> OntologyIndex<A, AA> for DeclarationMappedIndex<A, AA> {
    fn index_insert(&mut self, ax: AA) -> bool {
        some! {
            {
                let ne = self.aa_to_ne(ax.borrow())?;
                let iri = self.aa_to_iri(ax.borrow())?;

                // If this is a individual and we already have a
                // class, this is a pun, and we ignore the NI
                if ne == NamedEntityKind::NamedIndividual &&
                    self.0.get(&iri) == Some(&NamedEntityKind::Class)
                {
                    self.1.insert(iri.clone());
                    return None;
                }

                // Save the kind
                let s = self.0.insert(iri.clone(), ne);

                // If we have replaced an NI with a class, we have a pun
                if ne == NamedEntityKind::Class &&
                    s == Some(NamedEntityKind::NamedIndividual)
                {
                    self.1.insert(iri);
                }

                s
            }
        }
        .is_some()
    }

    fn index_remove(&mut self, ax: &AnnotatedComponent<A>) -> bool {
        let s = some! {
            self.0.remove(&self.aa_to_iri(&*ax)?)
        };

        s.is_some()
    }
}

impl DeclarationMappedIndex<RcStr, RcAnnotatedComponent> {
    pub fn new_rc() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::DeclarationMappedIndex;
    use crate::model::{AnnotatedComponent, Build, NamedOWLEntity, NamedOWLEntityKind, RcStr};
    use crate::ontology::indexed::OntologyIndex;
    use crate::vocab::OWL;
    fn stuff() -> (
        AnnotatedComponent<RcStr>,
        AnnotatedComponent<RcStr>,
        AnnotatedComponent<RcStr>,
    ) {
        let b = Build::new_rc();
        let c: NamedOWLEntity<_> = b.class("http://www.example.com/c").into();
        let o: NamedOWLEntity<_> = b.object_property("http://www.example.com/p").into();
        let b: NamedOWLEntity<_> = b.data_property("http://www.example.com/d").into();

        (c.into(), o.into(), b.into())
    }

    #[test]
    fn test_cons() {
        let _d = DeclarationMappedIndex::new_rc();
        assert!(true);
    }

    #[test]
    fn test_insert() {
        let mut d = DeclarationMappedIndex::new_rc();
        let s = stuff();
        assert!(d.index_insert(s.0.into()));
        assert!(d.index_insert(s.1.into()));
        assert!(d.index_insert(s.2.into()));
    }

    #[test]
    fn test_declaration() {
        let mut d = DeclarationMappedIndex::new_rc();
        let s = stuff();
        assert!(d.index_insert(s.0.into()));
        assert!(d.index_insert(s.1.into()));
        assert!(d.index_insert(s.2.into()));

        let b = Build::new();
        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/c")),
            Some(NamedOWLEntityKind::Class)
        );
        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/p")),
            Some(NamedOWLEntityKind::ObjectProperty)
        );
        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/d")),
            Some(NamedOWLEntityKind::DataProperty)
        );
    }

    #[test]
    fn test_declaration_builtin() {
        let d = DeclarationMappedIndex::new_rc();
        let b = Build::new_rc();
        assert_eq!(
            d.declaration_kind(&b.iri(OWL::TopDataProperty.as_ref())),
            Some(NamedOWLEntityKind::DataProperty)
        );
    }

    #[test]
    fn test_pun_support() {
        let mut d = DeclarationMappedIndex::new_rc();
        let b = Build::new_rc();

        assert_eq!(d.puns().len(), 0);

        let iri = b.iri("http://www.example.com/p");
        let c: NamedOWLEntity<_> = b.class("http://www.example.com/p").into();
        let c: AnnotatedComponent<_> = c.into();
        let ni: NamedOWLEntity<_> = b.named_individual("http://www.example.com/p").into();
        let ni: AnnotatedComponent<_> = ni.into();

        d.index_insert(c.clone().into());
        d.index_insert(ni.clone().into());

        assert_eq!(d.puns().len(), 1);
        assert_eq!(d.puns().iter().next(), Some(&iri));
        assert_eq!(d.declaration_kind(&iri), Some(NamedOWLEntityKind::Class));

        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(ni.clone().into());
        d.index_insert(c.clone().into());

        assert_eq!(d.puns().len(), 1);
        assert_eq!(d.puns().iter().next(), Some(&iri));
        assert_eq!(d.declaration_kind(&iri), Some(NamedOWLEntityKind::Class));
    }
}
