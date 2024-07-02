use std::collections::BTreeSet;
use std::marker::PhantomData;
use std::str::FromStr;

use curie::Curie;
use curie::PrefixMapping;
use enum_meta::Meta;
use pest::iterators::Pair;

use crate::error::HornedError;
use crate::model::*;
use crate::vocab::{Facet, OWL2Datatype, OWL};

use super::Context;
use super::Rule;

// ---------------------------------------------------------------------------

type Result<T> = std::result::Result<T, HornedError>;

/// A trait for OWL elements that can be obtained from OWL Functional tokens.
///
/// `Pair<Rule>` values can be obtained from the `OwlFunctionalParser` struct
/// after parsing a document.
pub trait FromPair<A: ForIRI>: Sized {
    /// The valid production rule for the implementor.
    const RULE: Rule;

    /// Create a new instance from a `Pair`.
    #[inline]
    fn from_pair(pair: Pair<Rule>, context: &Context<'_, A>) -> Result<Self> {
        if cfg!(debug_assertions) && pair.as_rule() != Self::RULE {
            return Err(HornedError::from(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::ParsingError {
                    positives: vec![pair.as_rule()],
                    negatives: vec![Self::RULE],
                },
                pair.as_span(),
            )));
        }
        Self::from_pair_unchecked(pair, context)
    }

    /// Create a new instance from a `Pair` without checking the PEG rule.
    fn from_pair_unchecked(pair: Pair<Rule>, context: &Context<'_, A>) -> Result<Self>;
}

// ---------------------------------------------------------------------------

macro_rules! impl_wrapper {
    ($ty:ident, $rule:path) => {
        impl<A: ForIRI> FromPair<A> for $ty<A> {
            const RULE: Rule = $rule;
            fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
                FromPair::from_pair(pair.into_inner().next().unwrap(), ctx).map($ty)
            }
        }
    };
}

impl_wrapper!(Class, Rule::Class);
impl_wrapper!(Import, Rule::Import);
impl_wrapper!(Datatype, Rule::Datatype);
impl_wrapper!(ObjectProperty, Rule::ObjectProperty);
impl_wrapper!(DataProperty, Rule::DataProperty);
impl_wrapper!(AnnotationProperty, Rule::AnnotationProperty);

impl_wrapper!(DeclareClass, Rule::ClassDeclaration);
impl_wrapper!(DeclareDatatype, Rule::DatatypeDeclaration);
impl_wrapper!(DeclareObjectProperty, Rule::ObjectPropertyDeclaration);
impl_wrapper!(DeclareDataProperty, Rule::DataPropertyDeclaration);
impl_wrapper!(
    DeclareAnnotationProperty,
    Rule::AnnotationPropertyDeclaration
);
impl_wrapper!(DeclareNamedIndividual, Rule::NamedIndividualDeclaration);

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for AnnotatedComponent<A> {
    const RULE: Rule = Rule::Axiom;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            // Declaration
            Rule::Declaration => {
                let mut inner = pair.into_inner();

                let ann = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let decl = inner.next().unwrap().into_inner().next().unwrap();
                let component = match decl.as_rule() {
                    Rule::ClassDeclaration => DeclareClass::from_pair(decl, ctx)?.into(),
                    Rule::DatatypeDeclaration => DeclareDatatype::from_pair(decl, ctx)?.into(),
                    Rule::ObjectPropertyDeclaration => {
                        DeclareObjectProperty::from_pair(decl, ctx)?.into()
                    }
                    Rule::DataPropertyDeclaration => {
                        DeclareDataProperty::from_pair(decl, ctx)?.into()
                    }
                    Rule::AnnotationPropertyDeclaration => {
                        DeclareAnnotationProperty::from_pair(decl, ctx)?.into()
                    }
                    Rule::NamedIndividualDeclaration => {
                        DeclareNamedIndividual::from_pair(decl, ctx)?.into()
                    }
                    rule => {
                        unreachable!(
                            "unexpected rule in AnnotatedComponent::Declaration: {:?}",
                            rule
                        )
                    }
                };

                Ok(Self { component, ann })
            }

            // ClassAxiom
            Rule::SubClassOf => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let subcls = ClassExpression::from_pair(inner.next().unwrap(), ctx)?;
                let supercls = ClassExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(SubClassOf::new(supercls, subcls), annotations))
            }
            Rule::EquivalentClasses => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ce = inner
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(EquivalentClasses(ce), annotations))
            }
            Rule::DisjointClasses => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ce = inner
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(DisjointClasses(ce), annotations))
            }
            Rule::DisjointUnion => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let cls = Class::from_pair(inner.next().unwrap(), ctx)?;
                let ce = inner
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(DisjointUnion(cls, ce), annotations))
            }

            // ObjectPropertyAxiom
            Rule::SubObjectPropertyOf => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let sub = SubObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                let sup = ObjectPropertyExpression::from_pair(
                    inner.next().unwrap().into_inner().next().unwrap(),
                    ctx,
                )?;
                Ok(Self::new(SubObjectPropertyOf { sup, sub }, annotations))
            }
            Rule::EquivalentObjectProperties => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ops = inner
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(EquivalentObjectProperties(ops), annotations))
            }
            Rule::DisjointObjectProperties => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ops = inner
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(DisjointObjectProperties(ops), annotations))
            }
            Rule::ObjectPropertyDomain => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ope = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ce = ClassExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(ObjectPropertyDomain::new(ope, ce), annotations))
            }
            Rule::ObjectPropertyRange => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ope = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                let ce = ClassExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(ObjectPropertyRange::new(ope, ce), annotations))
            }
            Rule::InverseObjectProperties => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let r1 = ObjectProperty::from_pair(inner.next().unwrap(), ctx)?;
                let r2 = ObjectProperty::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(InverseObjectProperties(r1, r2), annotations))
            }
            Rule::FunctionalObjectProperty => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let r = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(FunctionalObjectProperty(r), annotations))
            }
            Rule::InverseFunctionalObjectProperty => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let r = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(InverseFunctionalObjectProperty(r), annotations))
            }
            Rule::ReflexiveObjectProperty => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let r = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(ReflexiveObjectProperty(r), annotations))
            }
            Rule::IrreflexiveObjectProperty => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let r = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(IrreflexiveObjectProperty(r), annotations))
            }
            Rule::SymmetricObjectProperty => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let r = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(SymmetricObjectProperty(r), annotations))
            }
            Rule::AsymmetricObjectProperty => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let r = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(AsymmetricObjectProperty(r), annotations))
            }
            Rule::TransitiveObjectProperty => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let r = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(TransitiveObjectProperty(r), annotations))
            }

            // DataPropertyAxiom
            Rule::SubDataPropertyOf => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let sub = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let sup = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(SubDataPropertyOf { sub, sup }, annotations))
            }
            Rule::EquivalentDataProperties => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let dps = inner
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(EquivalentDataProperties(dps), annotations))
            }
            Rule::DisjointDataProperties => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let dps = inner
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(DisjointDataProperties(dps), annotations))
            }
            Rule::DataPropertyDomain => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let dp = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ce = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(DataPropertyDomain::new(dp, ce), annotations))
            }
            Rule::DataPropertyRange => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let dp = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ce = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(DataPropertyRange::new(dp, ce), annotations))
            }
            Rule::FunctionalDataProperty => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let dp = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(FunctionalDataProperty(dp), annotations))
            }
            Rule::DatatypeDefinition => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let k = Datatype::from_pair(inner.next().unwrap(), ctx)?;
                let r = DataRange::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(DatatypeDefinition::new(k, r), annotations))
            }

            // HasKey
            Rule::HasKey => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ce = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let vpe = inner
                    .map(|pair| match pair.as_rule() {
                        Rule::ObjectPropertyExpression => FromPair::from_pair(pair, ctx)
                            .map(PropertyExpression::ObjectPropertyExpression),
                        Rule::DataProperty => {
                            FromPair::from_pair(pair, ctx).map(PropertyExpression::DataProperty)
                        }
                        _ => unreachable!(),
                    })
                    .collect::<Result<_>>()?;
                Ok(Self::new(HasKey::new(ce, vpe), annotations))
            }

            // Assertion
            Rule::SameIndividual => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let individuals = inner
                    .map(|pair| Individual::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(SameIndividual(individuals), annotations))
            }
            Rule::DifferentIndividuals => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let individuals = inner
                    .map(|pair| Individual::from_pair(pair, ctx))
                    .collect::<Result<_>>()?;
                Ok(Self::new(DifferentIndividuals(individuals), annotations))
            }
            Rule::ClassAssertion => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ce = ClassExpression::from_pair(inner.next().unwrap(), ctx)?;
                let i = Individual::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(ClassAssertion::new(ce, i), annotations))
            }
            Rule::ObjectPropertyAssertion => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ope = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                let from = Individual::from_pair(inner.next().unwrap(), ctx)?;
                let to = Individual::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(
                    ObjectPropertyAssertion { ope, from, to },
                    annotations,
                ))
            }
            Rule::NegativeObjectPropertyAssertion => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ope = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
                let from = Individual::from_pair(inner.next().unwrap(), ctx)?;
                let to = Individual::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(
                    NegativeObjectPropertyAssertion::new(ope, from, to),
                    annotations,
                ))
            }
            Rule::DataPropertyAssertion => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ope = DataProperty::from_pair(inner.next().unwrap(), ctx)?;
                let from = Individual::from_pair(inner.next().unwrap(), ctx)?;
                let to = Literal::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(
                    DataPropertyAssertion::new(ope, from, to),
                    annotations,
                ))
            }
            Rule::NegativeDataPropertyAssertion => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ope = DataProperty::from_pair(inner.next().unwrap(), ctx)?;
                let from = Individual::from_pair(inner.next().unwrap(), ctx)?;
                let to = Literal::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(
                    NegativeDataPropertyAssertion::new(ope, from, to),
                    annotations,
                ))
            }

            // AnnotationAxiom
            Rule::AnnotationAssertion => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ap = AnnotationProperty::from_pair(inner.next().unwrap(), ctx)?;
                let subject = AnnotationSubject::from_pair(inner.next().unwrap(), ctx)?;
                let av = AnnotationValue::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(
                    AnnotationAssertion::new(subject, Annotation { ap, av }),
                    annotations,
                ))
            }
            Rule::SubAnnotationPropertyOf => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let sub =
                    FromPair::from_pair(inner.next().unwrap().into_inner().next().unwrap(), ctx)?;
                let sup =
                    FromPair::from_pair(inner.next().unwrap().into_inner().next().unwrap(), ctx)?;
                Ok(Self::new(SubAnnotationPropertyOf { sub, sup }, annotations))
            }
            Rule::AnnotationPropertyDomain => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ap = AnnotationProperty::from_pair(inner.next().unwrap(), ctx)?;
                let iri = IRI::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(
                    AnnotationPropertyDomain::new(ap, iri),
                    annotations,
                ))
            }
            Rule::AnnotationPropertyRange => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let ap = AnnotationProperty::from_pair(inner.next().unwrap(), ctx)?;
                let iri = IRI::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Self::new(
                    AnnotationPropertyRange::new(ap, iri),
                    annotations,
                ))
            }

            // Rule
            Rule::DLSafeRule => {
                let mut inner = pair.into_inner();
                let annotations = FromPair::from_pair(inner.next().unwrap(), ctx)?;
                let body = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .rev()
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<Vec<_>>>()?;
                let head = inner
                    .next()
                    .unwrap()
                    .into_inner()
                    .rev()
                    .map(|pair| FromPair::from_pair(pair, ctx))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Self::new(crate::model::Rule::new(head, body), annotations))
            }

            _ => unreachable!("unexpected rule in AnnotatedAxiom::from_pair"),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for Annotation<A> {
    const RULE: Rule = Rule::Annotation;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let mut inner = pair.into_inner();
        let _annotations: BTreeSet<Annotation<A>> =
            FromPair::from_pair(inner.next().unwrap(), ctx)?;

        Ok(Annotation {
            ap: FromPair::from_pair(inner.next().unwrap(), ctx)?,
            av: FromPair::from_pair(inner.next().unwrap(), ctx)?,
        })
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for AnnotationSubject<A> {
    const RULE: Rule = Rule::AnnotationSubject;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::IRI => FromPair::from_pair(inner, ctx).map(AnnotationSubject::IRI),
            // .map(Individual::Named)?,
            Rule::AnonymousIndividual => {
                FromPair::from_pair(inner, ctx).map(AnnotationSubject::AnonymousIndividual)
            }
            rule => {
                unreachable!(
                    "unexpected rule in AnnotationSubject::from_pair: {:?}",
                    rule
                )
            }
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for AnnotationValue<A> {
    const RULE: Rule = Rule::AnnotationValue;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::IRI => IRI::from_pair(inner, ctx).map(AnnotationValue::IRI),
            Rule::Literal => Literal::from_pair(inner, ctx).map(AnnotationValue::Literal),
            Rule::AnonymousIndividual => {
                AnonymousIndividual::from_pair(inner, ctx).map(AnnotationValue::AnonymousIndividual)
            }
            _ => unreachable!(),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for AnonymousIndividual<A> {
    const RULE: Rule = Rule::AnonymousIndividual;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let nodeid = pair.into_inner().next().unwrap();
        let inner = nodeid;
        let iri = ctx.build.iri(inner.as_str());
        Ok(AnonymousIndividual(iri.underlying()))
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for Atom<A> {
    const RULE: Rule = Rule::Atom;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::AtomClass => {
                let mut pairs = inner.into_inner();
                let pred = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let arg = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                Ok(Atom::ClassAtom { pred, arg })
            }
            Rule::AtomDataRange => {
                let mut pairs = inner.into_inner();
                let pred = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let arg = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                Ok(Atom::DataRangeAtom { pred, arg })
            }
            Rule::AtomObjectProperty => {
                let mut pairs = inner.into_inner();
                let pred = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let i1 = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let i2 = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let args = (i1, i2);
                Ok(Atom::ObjectPropertyAtom { pred, args })
            }
            Rule::AtomDataProperty => {
                let mut pairs = inner.into_inner();
                let pred = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let d1 = DArgument::from_pair(pairs.next().unwrap(), ctx)?;
                let d2 = DArgument::from_pair(pairs.next().unwrap(), ctx)?;
                let args = (d1, d2);
                Ok(Atom::DataPropertyAtom { pred, args })
            }
            Rule::AtomBuiltIn => {
                let mut pairs = inner.into_inner();
                let pred = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let mut args = Vec::with_capacity(pairs.len());
                for pair in pairs {
                    args.push(FromPair::from_pair(pair, ctx)?);
                }
                Ok(Atom::BuiltInAtom { pred, args })
            }
            Rule::AtomSameIndividual => {
                let mut pairs = inner.into_inner();
                let i1 = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let i2 = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                Ok(Atom::SameIndividualAtom(i1, i2))
            }
            Rule::AtomDifferentIndividuals => {
                let mut pairs = inner.into_inner();
                let i1 = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                let i2 = FromPair::from_pair(pairs.next().unwrap(), ctx)?;
                Ok(Atom::DifferentIndividualsAtom(i1, i2))
            }
            _ => unreachable!(),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for Component<A> {
    const RULE: Rule = Rule::Axiom;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        AnnotatedComponent::from_pair_unchecked(pair, ctx).map(|ac| ac.component)
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for BTreeSet<Annotation<A>> {
    const RULE: Rule = Rule::Annotations;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        pair.into_inner()
            .map(|pair| Annotation::from_pair(pair, ctx))
            .collect()
    }
}

// ---------------------------------------------------------------------------

macro_rules! impl_ce_data_cardinality {
    ($ctx:ident, $inner:ident, $dt:ident) => {{
        let mut pair = $inner.into_inner();
        let n = u32::from_pair(pair.next().unwrap(), $ctx)?;
        let dp = DataProperty::from_pair(pair.next().unwrap(), $ctx)?;
        let dr = match pair.next() {
            Some(pair) => DataRange::from_pair(pair, $ctx)?,
            // No data range is equivalent to `rdfs:Literal` as a data range.
            // see https://www.w3.org/TR/owl2-syntax/#Data_Property_Cardinality_Restrictions
            None => Datatype($ctx.build.iri(OWL2Datatype::Literal)).into(),
        };
        Ok(ClassExpression::$dt { n, dp, dr })
    }};
}

macro_rules! impl_ce_obj_cardinality {
    ($ctx:ident, $inner:ident, $card:ident) => {{
        let mut pair = $inner.into_inner();
        let n = u32::from_pair(pair.next().unwrap(), $ctx)?;
        let ope = ObjectPropertyExpression::from_pair(pair.next().unwrap(), $ctx)?;
        let bce = match pair.next() {
            Some(x) => Self::from_pair(x, $ctx).map(Box::new)?,
            // Missing class expression is equivalent to `owl:Thing` as class expression.
            // see https://www.w3.org/TR/owl2-syntax/#Object_Property_Cardinality_Restrictions
            None => Box::new(ClassExpression::Class(Class($ctx.build.iri(OWL::Thing)))),
        };
        Ok(ClassExpression::$card { n, ope, bce })
    }};
}

impl<A: ForIRI> FromPair<A> for ClassExpression<A> {
    const RULE: Rule = Rule::ClassExpression;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::Class => Class::from_pair(inner, ctx).map(ClassExpression::Class),
            Rule::ObjectIntersectionOf => inner
                .into_inner()
                .map(|pair| Self::from_pair(pair, ctx))
                .collect::<Result<_>>()
                .map(ClassExpression::ObjectIntersectionOf),
            Rule::ObjectUnionOf => inner
                .into_inner()
                .map(|pair| Self::from_pair(pair, ctx))
                .collect::<Result<_>>()
                .map(ClassExpression::ObjectUnionOf),
            Rule::ObjectComplementOf => Self::from_pair(inner.into_inner().next().unwrap(), ctx)
                .map(Box::new)
                .map(ClassExpression::ObjectComplementOf),
            Rule::ObjectOneOf => inner
                .into_inner()
                .map(|pair| Individual::from_pair(pair, ctx))
                .collect::<Result<_>>()
                .map(ClassExpression::ObjectOneOf),
            Rule::ObjectSomeValuesFrom => {
                let mut pairs = inner.into_inner();
                let ope = ObjectPropertyExpression::from_pair(pairs.next().unwrap(), ctx)?;
                let bce = Self::from_pair(pairs.next().unwrap(), ctx).map(Box::new)?;
                Ok(ClassExpression::ObjectSomeValuesFrom { ope, bce })
            }
            Rule::ObjectAllValuesFrom => {
                let mut pairs = inner.into_inner();
                let ope = ObjectPropertyExpression::from_pair(pairs.next().unwrap(), ctx)?;
                let bce = Self::from_pair(pairs.next().unwrap(), ctx).map(Box::new)?;
                Ok(ClassExpression::ObjectAllValuesFrom { ope, bce })
            }
            Rule::ObjectHasValue => {
                let mut pairs = inner.into_inner();
                let ope = ObjectPropertyExpression::from_pair(pairs.next().unwrap(), ctx)?;
                let i = Individual::from_pair(pairs.next().unwrap(), ctx)?;
                Ok(ClassExpression::ObjectHasValue { ope, i })
            }
            Rule::ObjectHasSelf => {
                let pair = inner.into_inner().next().unwrap();
                let expr = ObjectPropertyExpression::from_pair(pair, ctx)?;
                Ok(ClassExpression::ObjectHasSelf(expr))
            }
            Rule::ObjectMinCardinality => {
                impl_ce_obj_cardinality!(ctx, inner, ObjectMinCardinality)
            }
            Rule::ObjectMaxCardinality => {
                impl_ce_obj_cardinality!(ctx, inner, ObjectMaxCardinality)
            }
            Rule::ObjectExactCardinality => {
                impl_ce_obj_cardinality!(ctx, inner, ObjectExactCardinality)
            }
            Rule::DataSomeValuesFrom => {
                let mut pair = inner.into_inner();
                let dp = DataProperty::from_pair(pair.next().unwrap(), ctx)?;
                let next = pair.next().unwrap();
                if next.as_rule() == Rule::DataProperty {
                    Err(HornedError::invalid_at(
                        "horned-owl does not support data property chaining in `DataSomeValuesFrom`",
                        next.as_span(),
                    ))
                } else {
                    let dr = DataRange::from_pair(next, ctx)?;
                    Ok(ClassExpression::DataSomeValuesFrom { dp, dr })
                }
            }
            Rule::DataAllValuesFrom => {
                let mut pair = inner.into_inner();
                let dp = DataProperty::from_pair(pair.next().unwrap(), ctx)?;
                let next = pair.next().unwrap();
                if next.as_rule() == Rule::DataProperty {
                    Err(HornedError::invalid_at(
                        "horned-owl does not support data property chaining in `DataAllValuesFrom`",
                        next.as_span(),
                    ))
                } else {
                    let dr = DataRange::from_pair(next, ctx)?;
                    Ok(ClassExpression::DataAllValuesFrom { dp, dr })
                }
            }
            Rule::DataHasValue => {
                let mut pair = inner.into_inner();
                let dp = DataProperty::from_pair(pair.next().unwrap(), ctx)?;
                let l = Literal::from_pair(pair.next().unwrap(), ctx)?;
                Ok(ClassExpression::DataHasValue { dp, l })
            }
            Rule::DataMinCardinality => {
                impl_ce_data_cardinality!(ctx, inner, DataMinCardinality)
            }
            Rule::DataMaxCardinality => {
                impl_ce_data_cardinality!(ctx, inner, DataMaxCardinality)
            }
            Rule::DataExactCardinality => {
                impl_ce_data_cardinality!(ctx, inner, DataExactCardinality)
            }
            rule => unreachable!("unexpected rule in ClassExpression::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for DArgument<A> {
    const RULE: Rule = Rule::DArg;
    fn from_pair_unchecked(pair: Pair<Rule>, context: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::Literal => FromPair::from_pair(inner, context).map(DArgument::Literal),
            Rule::Variable => FromPair::from_pair(inner, context).map(DArgument::Variable),
            _ => unreachable!(),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for DataRange<A> {
    const RULE: Rule = Rule::DataRange;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::Datatype => Datatype::from_pair(inner, ctx).map(DataRange::Datatype),
            Rule::DataIntersectionOf => inner
                .into_inner()
                .map(|pair| Self::from_pair(pair, ctx))
                .collect::<Result<_>>()
                .map(DataRange::DataIntersectionOf),
            Rule::DataUnionOf => inner
                .into_inner()
                .map(|pair| Self::from_pair(pair, ctx))
                .collect::<Result<_>>()
                .map(DataRange::DataUnionOf),
            Rule::DataComplementOf => Self::from_pair(inner.into_inner().next().unwrap(), ctx)
                .map(Box::new)
                .map(DataRange::DataComplementOf),
            Rule::DataOneOf => inner
                .into_inner()
                .map(|pair| Literal::from_pair(pair, ctx))
                .collect::<Result<_>>()
                .map(DataRange::DataOneOf),
            Rule::DatatypeRestriction => {
                let mut pairs = inner.into_inner();
                Ok(DataRange::DatatypeRestriction(
                    Datatype::from_pair(pairs.next().unwrap(), ctx)?,
                    pairs
                        .map(|pair| FacetRestriction::from_pair(pair, ctx))
                        .collect::<Result<_>>()?,
                ))
            }
            rule => unreachable!("unexpected rule in DataRange::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for Facet {
    const RULE: Rule = Rule::ConstrainingFacet;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let pair = pair.into_inner().next().unwrap();
        let span = pair.as_span();
        let iri = IRI::from_pair(pair, ctx)?;
        Facet::all()
            .into_iter()
            .find(|facet| iri.to_string() == facet.as_ref())
            .ok_or_else(|| HornedError::invalid_at("invalid facet", span))
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for FacetRestriction<A> {
    const RULE: Rule = Rule::FacetRestriction;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let mut inner = pair.into_inner();
        let f = Facet::from_pair(inner.next().unwrap(), ctx)?;
        let l = Literal::from_pair(inner.next().unwrap(), ctx)?;
        Ok(FacetRestriction { f, l })
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for IArgument<A> {
    const RULE: Rule = Rule::IArg;
    fn from_pair_unchecked(pair: Pair<Rule>, context: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::Individual => FromPair::from_pair(inner, context).map(IArgument::Individual),
            Rule::Variable => FromPair::from_pair(inner, context).map(IArgument::Variable),
            _ => unreachable!(),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for Individual<A> {
    const RULE: Rule = Rule::Individual;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::NamedIndividual => NamedIndividual::from_pair(inner, ctx).map(Individual::Named),
            Rule::AnonymousIndividual => {
                AnonymousIndividual::from_pair(inner, ctx).map(Individual::Anonymous)
            }
            rule => unreachable!("unexpected rule in Individual::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for IRI<A> {
    const RULE: Rule = Rule::IRI;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::AbbreviatedIRI => {
                let span = inner.as_span();
                let mut pname = inner.into_inner().next().unwrap().into_inner();
                let prefix = pname.next().unwrap().into_inner().next();
                let local = pname.next().unwrap();
                let curie = Curie::new(
                    Some(prefix.map(|p| p.as_str()).unwrap_or_default()),
                    local.as_str(),
                );
                match ctx.mapping.expand_curie(&curie) {
                    Ok(s) => Ok(ctx.build.iri(s)),
                    Err(curie::ExpansionError::Invalid) => {
                        Err(HornedError::invalid_at("undefined prefix", span))
                    }
                    Err(curie::ExpansionError::MissingDefault) => {
                        Err(HornedError::invalid_at("missing default prefix", span))
                    }
                }
            }
            Rule::FullIRI => {
                let iri = inner.into_inner().next().unwrap();
                Ok(ctx.build.iri(iri.as_str()))
            }
            rule => unreachable!("unexpected rule in IRI::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for NamedIndividual<A> {
    const RULE: Rule = Rule::NamedIndividual;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        IRI::from_pair(pair.into_inner().next().unwrap(), ctx).map(NamedIndividual)
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for Literal<A> {
    const RULE: Rule = Rule::Literal;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let pair = pair.into_inner().next().unwrap();
        match pair.as_rule() {
            Rule::Literal => Self::from_pair(pair.into_inner().next().unwrap(), ctx),
            Rule::TypedLiteral => {
                let mut inner = pair.into_inner();
                let literal = String::from_pair(inner.next().unwrap(), ctx)?;
                let dty = Datatype::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Literal::Datatype {
                    literal,
                    datatype_iri: dty.0,
                })
            }
            Rule::StringLiteralWithLanguage => {
                let mut inner = pair.into_inner();
                let literal = String::from_pair(inner.next().unwrap(), ctx)?;
                let lang = inner.next().unwrap().as_str()[1..].trim().to_string();
                Ok(Literal::Language { literal, lang })
            }
            Rule::StringLiteralNoLanguage => {
                let mut inner = pair.into_inner();
                let literal = String::from_pair(inner.next().unwrap(), ctx)?;
                Ok(Literal::Simple { literal })
            }
            rule => unreachable!("unexpected rule in Literal::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for ObjectPropertyExpression<A> {
    const RULE: Rule = Rule::ObjectPropertyExpression;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::ObjectProperty => {
                ObjectProperty::from_pair(inner, ctx).map(ObjectPropertyExpression::ObjectProperty)
            }
            Rule::InverseObjectProperty => {
                ObjectProperty::from_pair(inner.into_inner().next().unwrap(), ctx)
                    .map(ObjectPropertyExpression::InverseObjectProperty)
            }
            rule => unreachable!(
                "unexpected rule in ObjectPropertyExpression::from_pair: {:?}",
                rule
            ),
        }
    }
}

// ---------------------------------------------------------------------------
pub(crate) struct MutableOntologyWrapper<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default>(
    pub(crate) O,
    PhantomData<A>,
);

impl<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default> FromPair<A>
    for MutableOntologyWrapper<A, O>
{
    const RULE: Rule = Rule::Ontology;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        debug_assert!(pair.as_rule() == Rule::Ontology);
        let mut pairs = pair.into_inner();
        let mut pair = pairs.next().unwrap();

        let mut ontology: O = Default::default();
        let mut ontology_id = OntologyID::default();

        // Parse ontology IRI and Version IRI if any
        if pair.as_rule() == Rule::OntologyIRI {
            let inner = pair.into_inner().next().unwrap();
            ontology_id.iri = Some(IRI::from_pair(inner, ctx)?);
            pair = pairs.next().unwrap();
            if pair.as_rule() == Rule::VersionIRI {
                let inner = pair.into_inner().next().unwrap();
                ontology_id.viri = Some(IRI::from_pair(inner, ctx)?);
                pair = pairs.next().unwrap();
            }
        }
        ontology.insert(ontology_id);

        // Process imports
        for p in pair.into_inner() {
            ontology.insert(Import::from_pair(p, ctx)?);
        }

        // Process ontology annotations
        for pair in pairs.next().unwrap().into_inner() {
            ontology.insert(OntologyAnnotation::from_pair(pair, ctx)?);
        }

        // Process axioms, ignore SWRL rules
        for pair in pairs.next().unwrap().into_inner() {
            let inner = pair.into_inner().next().unwrap();
            match inner.as_rule() {
                Rule::Axiom => {
                    ontology.insert(AnnotatedComponent::from_pair(inner, ctx)?);
                }
                rule => {
                    unreachable!("unexpected rule in Ontology::from_pair: {:?}", rule);
                }
            }
        }

        Ok(MutableOntologyWrapper(ontology, Default::default()))
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for OntologyAnnotation<A> {
    const RULE: Rule = Rule::Annotation;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        Annotation::from_pair(pair, ctx).map(OntologyAnnotation)
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for PrefixMapping {
    const RULE: Rule = Rule::PrefixDeclarations;
    fn from_pair_unchecked(pair: Pair<Rule>, _ctx: &Context<'_, A>) -> Result<Self> {
        // Build the prefix mapping and use it to build the ontology
        let mut prefixes = Self::default();
        for inner in pair.into_inner() {
            let mut decl = inner.into_inner();
            let mut pname = decl.next().unwrap().into_inner();
            let iri = decl.next().unwrap().into_inner().next().unwrap();
            if let Some(prefix) = pname.next().unwrap().into_inner().next() {
                prefixes
                    .add_prefix(prefix.as_str(), iri.as_str())
                    .expect("grammar does not allow invalid prefixes");
            } else {
                prefixes
                    .add_prefix("", iri.as_str())
                    .expect("empty prefix shouldn't fail")
            }
        }
        Ok(prefixes)
    }
}

// ---------------------------------------------------------------------------

impl<A, O> FromPair<A> for (MutableOntologyWrapper<A, O>, PrefixMapping)
where
    A: ForIRI,
    O: Default + MutableOntology<A> + Ontology<A>,
{
    const RULE: Rule = Rule::OntologyDocument;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();
        let prefixes = PrefixMapping::from_pair(pairs.next().unwrap(), ctx)?;
        let context = Context::new(ctx.build, &prefixes);
        MutableOntologyWrapper::from_pair(pairs.next().unwrap(), &context)
            .map(|ont| (ont, prefixes))
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for String {
    const RULE: Rule = Rule::QuotedString;
    fn from_pair_unchecked(pair: Pair<Rule>, _ctx: &Context<'_, A>) -> Result<Self> {
        let l = pair.as_str().len();
        let s = &pair.as_str()[1..l - 1];
        if s.contains(r"\\") || s.contains(r#"\""#) {
            Ok(s.replace(r"\\", r"\").replace(r#"\""#, r#"""#))
        } else {
            Ok(s.to_string())
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for SubObjectPropertyExpression<A> {
    const RULE: Rule = Rule::SubObjectPropertyExpression;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::ObjectPropertyExpression => ObjectPropertyExpression::from_pair(inner, ctx)
                .map(SubObjectPropertyExpression::ObjectPropertyExpression),
            Rule::PropertyExpressionChain => {
                let mut objs = Vec::new();
                for pair in inner.into_inner() {
                    objs.push(ObjectPropertyExpression::from_pair(pair, ctx)?);
                }
                Ok(SubObjectPropertyExpression::ObjectPropertyChain(objs))
            }
            rule => unreachable!(
                "unexpected rule in SubObjectProperty::from_pair: {:?}",
                rule
            ),
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for u32 {
    const RULE: Rule = Rule::NonNegativeInteger;
    fn from_pair_unchecked(pair: Pair<Rule>, _ctx: &Context<'_, A>) -> Result<Self> {
        Ok(Self::from_str(pair.as_str()).expect("cannot fail with the right rule"))
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for Variable<A> {
    const RULE: Rule = Rule::Variable;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        FromPair::from_pair(inner, ctx).map(Variable)
    }
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use std::collections::HashSet;
    use std::io::Cursor;
    use std::rc::Rc;

    use super::*;
    use crate::io::ofn::reader::lexer::OwlFunctionalLexer;
    use crate::ontology::set::SetOntology;

    use test_generator::test_resources;

    macro_rules! assert_parse_into {
        ($ty:ty, $rule:path, $build:ident, $prefixes:ident, $doc:expr, $expected:expr) => {
            let doc = $doc.trim();
            let ctx = Context::new(&$build, &$prefixes);
            match OwlFunctionalLexer::lex($rule, doc) {
                Ok(mut pairs) => {
                    let res = <$ty as FromPair<_>>::from_pair(pairs.next().unwrap(), &ctx);
                    assert_eq!(res.unwrap(), $expected);
                }
                Err(e) => panic!(
                    "parsing using {:?}:\n{}\nfailed with: {}",
                    $rule,
                    doc.trim(),
                    e
                ),
            }
        };
    }

    #[test]
    fn has_key() {
        let build = Build::default();
        let mut prefixes = PrefixMapping::default();
        prefixes
            .add_prefix("owl", "http://www.w3.org/2002/07/owl#")
            .unwrap();

        assert_parse_into!(
            AnnotatedComponent<String>,
            Rule::Axiom,
            build,
            prefixes,
            "HasKey( owl:Thing () (<http://www.example.com/issn>) )",
            AnnotatedComponent::from(HasKey::new(
                ClassExpression::Class(build.class("http://www.w3.org/2002/07/owl#Thing")),
                vec![PropertyExpression::DataProperty(
                    build.data_property("http://www.example.com/issn")
                )],
            ))
        );
    }

    #[test]
    fn declare_class() {
        let build = Build::default();
        let mut prefixes = PrefixMapping::default();
        prefixes
            .add_prefix("owl", "http://www.w3.org/2002/07/owl#")
            .unwrap();

        assert_parse_into!(
            DeclareClass<String>,
            Rule::ClassDeclaration,
            build,
            prefixes,
            "Class( owl:Thing )",
            DeclareClass(build.class("http://www.w3.org/2002/07/owl#Thing"))
        );

        assert_parse_into!(
            Component<String>,
            Rule::Axiom,
            build,
            prefixes,
            "Declaration(Class(owl:Thing))",
            Component::DeclareClass(DeclareClass(
                build.class("http://www.w3.org/2002/07/owl#Thing")
            ))
        );

        assert_parse_into!(
            AnnotatedComponent<String>,
            Rule::Axiom,
            build,
            prefixes,
            "Declaration(Class(owl:Thing))",
            AnnotatedComponent::from(DeclareClass(
                build.class("http://www.w3.org/2002/07/owl#Thing")
            ))
        );
    }

    #[test]
    fn iri() {
        let build = Build::default();
        let mut prefixes = PrefixMapping::default();
        prefixes
            .add_prefix("ex", "http://example.com/path#")
            .unwrap();

        assert_parse_into!(
            IRI<String>,
            Rule::IRI,
            build,
            prefixes,
            "<http://example.com/path#ref>",
            build.iri("http://example.com/path#ref")
        );

        assert_parse_into!(
            IRI<String>,
            Rule::IRI,
            build,
            prefixes,
            "ex:ref",
            build.iri("http://example.com/path#ref")
        );
    }

    #[test]
    fn ontology_document() {
        let build = Build::default();
        let prefixes = PrefixMapping::default();
        let txt = "Prefix(ex:=<http://example.com/>) Prefix(:=<http://default.com/>) Ontology()";

        let mut expected = PrefixMapping::default();
        expected.add_prefix("", "http://default.com/").unwrap();
        expected.add_prefix("ex", "http://example.com/").unwrap();

        let pair = OwlFunctionalLexer::lex(Rule::OntologyDocument, txt)
            .unwrap()
            .next()
            .unwrap();

        let doc: (
            MutableOntologyWrapper<_, SetOntology<String>>,
            PrefixMapping,
        ) = FromPair::from_pair(pair, &Context::new(&build, &prefixes)).unwrap();
        assert_eq!(
            doc.1.mappings().collect::<HashSet<_>>(),
            expected.mappings().collect::<HashSet<_>>()
        );
    }

    #[test]
    fn data_property_atom() {
        let build = Build::<String>::new();
        let mut mapping = PrefixMapping::default();
        mapping.add_prefix("o", "https://example.com/").unwrap();
        let txt = "DataPropertyAtom(o:d Variable(o:x) \"Literal String\")";

        let expected = Atom::DataPropertyAtom {
            pred: build.data_property("https://example.com/d"),
            args: (
                DArgument::Variable(build.variable("https://example.com/x")),
                DArgument::Literal(Literal::Simple {
                    literal: String::from("Literal String"),
                }),
            ),
        };
        let pair = OwlFunctionalLexer::lex(Rule::Atom, txt)
            .unwrap()
            .next()
            .unwrap();
        let actual = Atom::from_pair(pair, &Context::new(&build, &mapping)).unwrap();
        pretty_assertions::assert_eq!(actual, expected);
    }

    #[test_resources("src/ont/owl-functional/*.ofn")]
    fn from_pair_resource(resource: &str) {
        let text = &slurp::read_all_to_string(resource).unwrap();
        let pair = match OwlFunctionalLexer::lex(Rule::OntologyDocument, text.trim()) {
            Err(e) => panic!("parser failed: {}", e),
            Ok(mut pairs) => {
                let pair = pairs.next().unwrap();
                assert_eq!(pair.as_str(), text.trim());
                pair
            }
        };

        let build = Build::new();
        let prefixes = PrefixMapping::default();
        let ctx = Context::new(&build, &prefixes);
        let item: (MutableOntologyWrapper<_, SetOntology<Rc<str>>>, _) =
            FromPair::from_pair(pair, &ctx).unwrap();

        let path = resource
            .replace("owl-functional", "owl-xml")
            .replace(".ofn", ".owx");
        let owx = &slurp::read_all_to_string(path).unwrap();
        let expected =
            crate::io::owx::reader::read(&mut Cursor::new(&owx), Default::default()).unwrap();

        // pretty_assertions::assert_eq!(item.1, expected.1);
        pretty_assertions::assert_eq!(item.0 .0, expected.0);
    }
}
