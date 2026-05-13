use crate::model::{
    AnnotatedComponent, AnnotationProperty, Class, DataProperty, Datatype,
    DeclareAnnotationProperty, DeclareClass, DeclareDataProperty, DeclareDatatype,
    DeclareNamedIndividual, DeclareObjectProperty, ForIRI, Individual, ObjectProperty, ObjectPropertyExpression,
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

macro_rules! impl_from {
    ($ty:ident, $entity:ident, $declare:ident) => {
        impl<A: ForIRI> From<$entity<A>> for $ty<A> {
            fn from(entity: $entity<A>) -> Self {
                let components = vec![$declare(entity.clone()).into()];
                Self { entity, components }
            }
        }
    };
}

// ---------------------------------------------------------------------------

pub type DatatypeFrame<A> = Frame<A, Datatype<A>>;

impl_from!(DatatypeFrame, Datatype, DeclareDatatype);

// ---------------------------------------------------------------------------

pub type ClassFrame<A> = Frame<A, Class<A>>;

impl_from!(ClassFrame, Class, DeclareClass);

// ---------------------------------------------------------------------------

pub type ObjectPropertyFrame<A> = Frame<A, ObjectProperty<A>>;

impl_from!(ObjectPropertyFrame, ObjectProperty, DeclareObjectProperty);

// ---------------------------------------------------------------------------

pub type DataPropertyFrame<A> = Frame<A, DataProperty<A>>;

impl_from!(DataPropertyFrame, DataProperty, DeclareDataProperty);

// ---------------------------------------------------------------------------

pub type AnnotationPropertyFrame<A> = Frame<A, AnnotationProperty<A>>;

impl_from!(
    AnnotationPropertyFrame,
    AnnotationProperty,
    DeclareAnnotationProperty
);

// ---------------------------------------------------------------------------

pub type IndividualFrame<A> = Frame<A, Individual<A>>;

// Need manual implementation because anonymous individuals must not be declared.
impl<A: ForIRI> From<Individual<A>> for IndividualFrame<A> {
    fn from(entity: Individual<A>) -> Self {
        let components = match &entity {
            Individual::Anonymous(_) => Vec::new(),
            Individual::Named(ni) => vec![DeclareNamedIndividual(ni.clone()).into()],
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

    pub fn empty() -> Self {
        MiscClause(None)
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
