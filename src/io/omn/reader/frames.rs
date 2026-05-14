use std::collections::BTreeSet;

use crate::model::{
    AnnotatedComponent, Annotation, AnnotationProperty, ClassExpression, Component, DataProperty, Datatype, DeclareAnnotationProperty, DeclareClass, DeclareDataProperty, DeclareDatatype, DeclareNamedIndividual, DeclareObjectProperty, ForIRI, Individual, ObjectPropertyExpression
};

// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub struct Frame<A: ForIRI, T> {
    pub entity: T,
    pub components: Vec<AnnotatedComponent<A>>,
}

impl<A: ForIRI, T> Frame<A, T> {
    #[allow(unused)]
    pub fn with_components(entity: T, components: Vec<AnnotatedComponent<A>>) -> Self {
        Self { entity, components }
    }

    #[allow(unused)]
    pub fn into_components(self) -> Vec<AnnotatedComponent<A>> {
        self.components
    }
}

// ---------------------------------------------------------------------------

macro_rules! impl_new {
    ($ty:ident, $entity:ident, $complex_entity:ident, $declare:ident) => {
        impl<A: ForIRI> $ty<A> {
            pub fn new(entity: $complex_entity<A>, annotations: BTreeSet<Annotation<A>>) -> Self {
                let components = if let $complex_entity::$entity(ref c) = entity {
                    vec![AnnotatedComponent::new(
                        Component::$declare($declare(c.clone()).into()),
                        annotations,
                    )]
                } else {
                    vec![]
                };
                Self { entity, components }
            }
        }
    };
    ($ty:ident, $entity:ident, $declare:ident) => {
        impl<A: ForIRI> $ty<A> {
            pub fn new(entity: $entity<A>, annotations: BTreeSet<Annotation<A>>) -> Self {
                let components = vec![
                    AnnotatedComponent::new(
                        Component::$declare($declare(entity.clone()).into()),
                        annotations,
                    ),
                ];
                Self { entity, components }
            }
        }
    };
}

// ---------------------------------------------------------------------------

pub type DatatypeFrame<A> = Frame<A, Datatype<A>>;

impl_new!(DatatypeFrame, Datatype, DeclareDatatype);

// ---------------------------------------------------------------------------

pub type ClassFrame<A> = Frame<A, ClassExpression<A>>;

impl_new!(ClassFrame, Class, ClassExpression, DeclareClass);

// ---------------------------------------------------------------------------

pub type ObjectPropertyFrame<A> = Frame<A, ObjectPropertyExpression<A>>;

impl_new!(ObjectPropertyFrame, ObjectProperty, ObjectPropertyExpression, DeclareObjectProperty);

// ---------------------------------------------------------------------------

pub type DataPropertyFrame<A> = Frame<A, DataProperty<A>>;

impl_new!(DataPropertyFrame, DataProperty, DeclareDataProperty);

// ---------------------------------------------------------------------------

pub type AnnotationPropertyFrame<A> = Frame<A, AnnotationProperty<A>>;

impl_new!(
    AnnotationPropertyFrame,
    AnnotationProperty,
    DeclareAnnotationProperty
);

// ---------------------------------------------------------------------------

pub type IndividualFrame<A> = Frame<A, Individual<A>>;

// Need manual implementation because anonymous individuals must not be declared.
impl<A: ForIRI> IndividualFrame<A> {
    pub fn new(entity: Individual<A>, annotations: BTreeSet<Annotation<A>>) -> Self {
        let components = match &entity {
            Individual::Anonymous(_) => Vec::new(),
            Individual::Named(ni) => vec![
                AnnotatedComponent::new(
                    Component::DeclareNamedIndividual(DeclareNamedIndividual(ni.clone())),
                    annotations,
                )
            ],
        };
        Self { entity, components }
    }
}

// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct MiscClause<A: ForIRI>(pub Option<AnnotatedComponent<A>>);

impl<A: ForIRI> MiscClause<A> {
    pub fn new(component: AnnotatedComponent<A>) -> Self {
        MiscClause(Some(component))
    }
}

impl<A: ForIRI> From<AnnotatedComponent<A>> for MiscClause<A> {
    fn from(component: AnnotatedComponent<A>) -> Self {
        MiscClause::new(component)
    }
}

// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct InverseObjectPropertyFrame<A: ForIRI> {
    pub entity: ObjectPropertyExpression<A>,
    pub components: Vec<AnnotatedComponent<A>>,
}

impl<A: ForIRI> InverseObjectPropertyFrame<A> {
    pub fn new(entity: ObjectPropertyExpression<A>) -> Self {
        let components = Vec::new();
        Self { entity, components }
    }

    pub fn into_components(self) -> Vec<AnnotatedComponent<A>> {
        self.components
    }
}

impl<A: ForIRI> From<ObjectPropertyExpression<A>> for InverseObjectPropertyFrame<A> {
    fn from(ope: ObjectPropertyExpression<A>) -> Self {
        Self::new(ope)
    }
}
