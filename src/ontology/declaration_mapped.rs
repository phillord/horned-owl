//! An index that provides rapid look up via declaration kind

use crate::model::{AnnotatedAxiom, Axiom, AxiomKind, ForIRI, Kinded, NamedEntityKind, IRI, RcAnnotatedAxiom, RcStr};

use super::indexed::ForIndex;
use super::indexed::OntologyIndex;

use std::collections::HashSet;
use std::marker::PhantomData;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct DeclarationMappedIndex<A, AA>(HashMap<IRI<A>, NamedEntityKind>,
                                         HashSet<IRI<A>>,
                                         PhantomData<AA>);

impl<A: ForIRI, AA: ForIndex<A>> DeclarationMappedIndex<A, AA> {
    pub fn new() -> DeclarationMappedIndex<A, AA> {
        DeclarationMappedIndex(HashMap::new(), HashSet::new(), Default::default())
    }

    pub fn is_annotation_property(&self, iri: &IRI<A>) -> bool {
        matches!{
            self.declaration_kind(iri),
            Some(NamedEntityKind::AnnotationProperty)
        }
    }

    pub fn declaration_kind(&self, iri: &IRI<A>) -> Option<NamedEntityKind> {
        self.0
            .get(iri)
            .cloned()
            .or_else(|| crate::vocab::to_built_in_entity(iri))
    }

    pub fn puns(&self) -> &HashSet<IRI<A>> {
        &self.1
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
        }.is_some()
    }

    fn index_remove(&mut self, ax: &AnnotatedAxiom<A>) -> bool {
        let s = some! {
            self.0.remove(&self.aa_to_iri(&*ax)?)
        };

        s.is_some()
    }
}

impl DeclarationMappedIndex<RcStr, RcAnnotatedAxiom> {
    pub fn new_rc() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod test {
    use super::DeclarationMappedIndex;
    use crate::model::{AnnotatedAxiom, Build, NamedEntity, NamedEntityKind, RcStr};
    use crate::ontology::indexed::OntologyIndex;
    use crate::vocab::{WithIRI, OWL};
    fn stuff() -> (
        AnnotatedAxiom<RcStr>,
        AnnotatedAxiom<RcStr>,
        AnnotatedAxiom<RcStr>,
    ) {
        let b = Build::new_rc();
        let c: NamedEntity<_> = b.class("http://www.example.com/c").into();
        let o: NamedEntity<_> = b.object_property("http://www.example.com/p").into();
        let b: NamedEntity<_> = b.data_property("http://www.example.com/d").into();

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
        let d = DeclarationMappedIndex::new_rc();
        let b = Build::new_rc();
        assert_eq!(
            d.declaration_kind(&b.iri(OWL::TopDataProperty.iri_str())),
            Some(NamedEntityKind::DataProperty)
        );
    }


    #[test]
    fn test_pun_support() {
        let mut d = DeclarationMappedIndex::new_rc();
        let b = Build::new_rc();

        assert_eq!(d.puns().len(), 0);

        let iri = b.iri("http://www.example.com/p");
        let c: NamedEntity<_> = b.class("http://www.example.com/p").into();
        let c: AnnotatedAxiom<_> = c.into();
        let ni: NamedEntity<_> = b.named_individual("http://www.example.com/p").into();
        let ni: AnnotatedAxiom<_> = ni.into();

        d.index_insert(c.clone().into());
        d.index_insert(ni.clone().into());

        assert_eq!(d.puns().len(), 1);
        assert_eq!(d.puns().iter().next(), Some(&iri));
        assert_eq!(d.declaration_kind(&iri), Some(NamedEntityKind::Class));

        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(ni.clone().into());
        d.index_insert(c.clone().into());

        assert_eq!(d.puns().len(), 1);
        assert_eq!(d.puns().iter().next(), Some(&iri));
        assert_eq!(d.declaration_kind(&iri), Some(NamedEntityKind::Class));
    }
}
