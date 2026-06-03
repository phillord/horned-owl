use std::collections::BTreeSet;
use std::marker::PhantomData;
use std::num::NonZeroU32;
use std::str::FromStr;

use curie::Curie;
use curie::PrefixMapping;
use pest::Span;
use pest::iterators::Pair;
use pest::iterators::Pairs;

use crate::error::HornedError;
use crate::io::omn::reader::ambiguity::data_range_to_class_expression;
use crate::model::Rule as SWRLRule;
use crate::model::*;
use crate::vocab::Facet;

use super::Context;
use super::frames::AnnotationPropertyFrame;
use super::frames::ClassFrame;
use super::frames::DataPropertyFrame;
use super::frames::DatatypeFrame;
use super::frames::IndividualFrame;
use super::frames::MiscClause;
use super::frames::ObjectPropertyFrame;
use super::lexer::Rule;

type Result<T> = std::result::Result<T, HornedError>;
// Alias for better readability
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ParseResult<'a, T> {
    Success(T),
    Ambiguous(T, Span<'a>),
}
use ParseResult::*;

fn collect_parse_result<'a, T>(
    results: impl Iterator<Item = ParseResult<'a, T>>,
) -> ParseResult<'a, Vec<T>> {
    let mut values = Vec::new();
    let mut ambiguous = None;

    for result in results {
        match result {
            Success(value) => values.push(value),
            Ambiguous(value, span) => {
                values.push(value);
                ambiguous.get_or_insert(span);
            }
        }
    }

    if let Some(span) = ambiguous {
        Ambiguous(values, span)
    } else {
        Success(values)
    }
}

impl<'a, T> ParseResult<'a, T> {
    fn map<U, F: FnOnce(T) -> U>(self, f: F) -> ParseResult<'a, U> {
        match self {
            Success(value) => Success(f(value)),
            Ambiguous(value, span) => Ambiguous(f(value), span),
        }
    }
}

impl<'a, T, S> From<(ParseResult<'a, T>, ParseResult<'a, S>)> for ParseResult<'a, (T, S)> {
    fn from((result, suffix): (ParseResult<'a, T>, ParseResult<'a, S>)) -> Self {
        match (result, suffix) {
            (Success(value), Success(suffix)) => Success((value, suffix)),
            (Success(value), Ambiguous(suffix, span))
            | (Ambiguous(value, span), Success(suffix)) => Ambiguous((value, suffix), span),
            (Ambiguous(value, span), Ambiguous(suffix, _)) => Ambiguous((value, suffix), span),
        }
    }
}

impl<'a, T> From<T> for ParseResult<'a, T> {
    fn from(value: T) -> Self {
        Success(value)
    }
}

impl<'a, T> From<ParseResult<'a, Result<T>>> for Result<ParseResult<'a, T>> {
    fn from(result: ParseResult<'a, Result<T>>) -> Self {
        match result {
            Success(value) => value.map(Success),
            Ambiguous(value, span) => value.map(|v| Ambiguous(v, span)),
        }
    }
}

fn pair_or_err<'a>(pair: Option<Pair<'a, Rule>>, message: &str) -> Result<Pair<'a, Rule>> {
    match pair {
        Some(pair) => Ok(pair),
        None => Err(HornedError::invalid(message.to_string())),
    }
}

macro_rules! next_or_err {
    ($pairs:expr, $message:expr) => {
        pair_or_err($pairs.next(), $message)
    };
    ($pairs:expr) => {
        pair_or_err($pairs.next(), "Unexpected end of parse tree")
    };
}
macro_rules! last_or_err {
    ($pairs:expr, $message:expr) => {{
        let mut pairs = $pairs;
        let pair = pair_or_err(pairs.next(), $message);
        if pairs.next().is_some() {
            Err(HornedError::invalid(format!(
                "Expected exactly one pair, but found more: {}",
                $message
            )))
        } else {
            pair
        }
    }};
    ($pairs:expr) => {
        last_or_err!($pairs, "Unexpected tokens in parse tree")
    };
}

macro_rules! empty_or_err {
    ($pairs:expr) => {
        if let Some(pair) = $pairs.next() {
            Err(HornedError::invalid(format!(
                "Expected no more pairs, but found: {}",
                pair.as_str()
            )))
        } else {
            Ok(())
        }
    };
}

macro_rules! parse_error {
    ($message:expr, $span:expr) => {
        Err(HornedError::invalid_at($message.to_string(), $span))
    };
    ($message:expr) => {
        Err(HornedError::invalid($message.to_string()))
    };
}

// ---------------------------------------------------------------------------

/// A trait for OWL elements that can be obtained from OWL Manchester tokens.
///
/// `Pair<Rule>` values can be obtained from the `OwlManchesterParser` struct
/// after parsing a document.
pub trait FromPair<'a, A: ForIRI>: Sized {
    /// The valid production rule for the implementor.
    const RULE: Rule;

    /// Create a new instance from a `Pair`.
    #[inline]
    fn from_pair(pair: Pair<'a, Rule>, context: &mut Context<'a, A>) -> Result<Self> {
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
    fn from_pair_unchecked(pair: Pair<'a, Rule>, context: &mut Context<'a, A>) -> Result<Self>;
}

// --- Helpers ---------------------------------------------------------------

/// Mark some rules as unreachable when performing rule matching.
macro_rules! unexpected_rule {
    ($type:ident, $rule:expr) => {
        parse_error!(format!(
            "unexpected rule in {}::from_pair: {:?}",
            stringify!($type),
            $rule
        ))
    };
}

/// Descend one layer of depth into a pair which rule contains a single rule.
fn descend(pair: Pair<Rule>) -> Result<Pair<Rule>> {
    last_or_err!(pair.into_inner())
}

/// Parse optional `Annotations` into a `BTreeSet` to use with `AnnotatedComponents`.
fn component_annotations<'a, A: ForIRI>(
    pair: &mut Pair<'a, Rule>,
    pairs: &mut Pairs<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<BTreeSet<Annotation<A>>> {
    if pair.as_rule() == Rule::Annotations {
        let p = std::mem::replace(pair, next_or_err!(pairs)?);
        let anns = BTreeSet::from_pair(descend(p)?, ctx)?;
        Ok(anns)
    } else {
        Ok(BTreeSet::new())
    }
}

// --- IRI Wrappers ----------------------------------------------------------

macro_rules! impl_wrapper {
    ($ty:ident, $rule:path) => {
        impl<'a, A: ForIRI> FromPair<'a, A> for $ty<A> {
            const RULE: Rule = $rule;
            fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
                FromPair::<'a, A>::from_pair(descend(pair)?, ctx).map($ty)
            }
        }
    };
}

impl_wrapper!(Class, Rule::ClassIRI);
impl_wrapper!(Import, Rule::Import);
impl_wrapper!(NamedIndividual, Rule::IndividualIRI);
impl_wrapper!(ObjectProperty, Rule::ObjectPropertyIRI);
impl_wrapper!(DataProperty, Rule::DataPropertyIRI);
impl_wrapper!(AnnotationProperty, Rule::AnnotationPropertyIRI);

// --- Raw Item Lists --------------------------------------------------------

macro_rules! impl_vector {
    ($A:ident, $ty:ty, $rule:path) => {
        impl<'a, $A: ForIRI> FromPair<'a, $A> for $ty {
            const RULE: Rule = $rule;
            fn from_pair_unchecked(
                pair: Pair<'a, Rule>,
                ctx: &mut Context<'a, $A>,
            ) -> Result<Self> {
                pair.into_inner()
                    .map(|pair| FromPair::<'a, $A>::from_pair(pair, ctx))
                    .collect()
            }
        }
    };
}

impl_vector!(A, Vec<Literal<A>>, Rule::LiteralList);
impl_vector!(A, Vec<Individual<A>>, Rule::IndividualList);
impl_vector!(A, Vec<FacetRestriction<A>>, Rule::FacetRestrictionList);

// --- Annotation ------------------------------------------------------------

impl<'a, A: ForIRI> FromPair<'a, A> for Annotation<A> {
    const RULE: Rule = Rule::Annotation;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut inner = pair.into_inner();
        let ap = FromPair::<'a, A>::from_pair(next_or_err!(inner)?, ctx)?;
        let av = FromPair::<'a, A>::from_pair(next_or_err!(inner)?, ctx)?;
        Ok(Annotation { ap, av })
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for AnnotationValue<A> {
    const RULE: Rule = Rule::AnnotationTarget;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::NodeID => {
                let individual = FromPair::<'a, A>::from_pair(inner, ctx)?;
                Ok(AnnotationValue::AnonymousIndividual(individual))
            }
            Rule::IRI => {
                let iri = FromPair::<'a, A>::from_pair(inner, ctx)?;
                Ok(AnnotationValue::IRI(iri))
            }
            Rule::Literal => {
                let literal = FromPair::<'a, A>::from_pair(inner, ctx)?;
                Ok(AnnotationValue::Literal(literal))
            }
            rule => unexpected_rule!(AnnotationValue, rule),
        }
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for BTreeSet<Annotation<A>> {
    const RULE: Rule = Rule::AnnotationAnnotatedList;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = pair.into_inner();

        inner
            .map(|pair| match pair.as_rule() {
                Rule::Annotation => Annotation::<A>::from_pair(pair, ctx),
                Rule::Annotations => {
                    parse_error!(
                        "nested annotation lists should not be encountered",
                        pair.as_span()
                    )
                }
                rule => unexpected_rule!(BTreeSet, rule),
            })
            .collect::<Result<BTreeSet<_>>>()
    }
}

// --- ClassExpression -------------------------------------------------------

// Class expressions have several production rules in the Manchester syntax
// because they may be more restrictive in some cases; we parse all these
// different rules into a ClassExpression anyway, but we still need to
// support the different rules.

// Returns None if the restriction contains an undeclared property
fn from_restriction_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<ParseResult<'a, ClassExpression<A>>> {
    debug_assert!(pair.as_rule() == Rule::Restriction);

    macro_rules! ensure_property_kind {
        (Object, $ope:expr, $span:ident, $result:expr) => {{ ensure_property_kind!($ope.as_property(), $span, $result, ObjectProperty) }};
        (Data, $dp:expr, $span:ident, $result:expr) => {{ ensure_property_kind!(Some(&$dp), $span, $result, DataProperty) }};
        ($prop:expr, $span:ident, $result:expr, $kind:ident) => {{
            match $result {
                Success(r) => {
                    let kind = $prop
                        .map(|p| ctx.get_property_kind(&p))
                        .unwrap_or(Some(NamedEntityKind::ObjectProperty));

                    match kind {
                        Some(NamedEntityKind::$kind) => Ok(Success(r)),
                        Some(_) => parse_error!(
                            format!(
                                "Expected '{:?}' as {:?}Property but is declared as {:?}Property",
                                $prop,
                                NamedEntityKind::$kind,
                                kind
                            ),
                            $span
                        ),
                        None => Ok(Ambiguous(r, $span)),
                    }
                }
                Ambiguous(r, span) => Ok(Ambiguous(r, span)),
            }
        }};
    }

    macro_rules! data_cardinality {
        ($pairs:ident, $span:ident, $ctx:ident, ClassExpression:: $variant:ident) => {{
            let span = $span;
            let mut pairs = $pairs;

            let dp = DataProperty::from_pair(descend(next_or_err!(pairs)?)?, $ctx)?;
            let n = u32::from_pair(next_or_err!(pairs)?, $ctx)?;

            let dr = if let Some(pair) = pairs.next() {
                empty_or_err!(pairs)?;
                DataRange::from_pair(pair, $ctx)?
            } else {
                // FIXME: currently unsupported in `horned-owl`
                return parse_error!(
                    concat!(
                        stringify!($variant),
                        " without data range are not supported"
                    ),
                    span
                );
            };

            let ce = ClassExpression::$variant {
                n,
                dp: dp.clone(),
                dr,
            };

            ensure_property_kind!(Data, dp, span, Success(ce))
        }};
    }

    macro_rules! object_cardinality {
        ($pairs:ident, $span:ident, $ctx:ident, ClassExpression:: $variant:ident) => {{
            let span = $span;
            let mut pairs = $pairs;

            let ope = ope_from_pair(next_or_err!(pairs)?, $ctx)?;
            let n = u32::from_pair(next_or_err!(pairs)?, $ctx)?;

            let bce = if let Some(pair) = pairs.next() {
                ce_from_primary_or_data_primary_pair(pair, $ctx)?.map(Box::new)
            } else {
                // FIXME: currently unsupported in `horned-owl`
                return parse_error!(
                    concat!(
                        stringify!($variant),
                        " without class expression are not supported"
                    ),
                    span
                );
            };

            let ce = bce.map(|bce| ClassExpression::$variant {
                n,
                ope: ope.clone(),
                bce,
            });

            ensure_property_kind!(Object, ope, span, ce)
        }};
    }

    let inner = descend(pair)?;
    let rule = inner.as_rule();
    let span = inner.as_span();
    let mut pairs = inner.into_inner();

    let property_kind = pairs.peek().and_then(|p| {
        let iri = DataProperty::from_pair(descend(p.clone()).ok()?, ctx)
            .ok()?
            .0;
        ctx.get_property_kind(&iri)
    });

    macro_rules! dp {
        () => {
            Some(NamedEntityKind::DataProperty) | None
        };
    }

    // Resolve ambiguity between object and data property restrictions by first checking if the property is declared as a data property.
    // For object properties we then
    match (rule, property_kind) {
        (Rule::DataSomeValuesFromRestriction, dp!()) => {
            let dp = DataProperty::from_pair(descend(next_or_err!(pairs)?)?, ctx)?;
            let dr = FromPair::from_pair(last_or_err!(pairs)?, ctx)?;
            let ce = ClassExpression::DataSomeValuesFrom { dp: dp.clone(), dr };

            ensure_property_kind!(Data, dp, span, Success(ce))
        }
        (Rule::DataAllValuesFromRestriction, dp!()) => {
            let dp =
                DataProperty::from_pair(last_or_err!(next_or_err!(pairs)?.into_inner())?, ctx)?;
            let dr = FromPair::from_pair(last_or_err!(pairs)?, ctx)?;
            let ce = ClassExpression::DataAllValuesFrom { dp: dp.clone(), dr };

            ensure_property_kind!(Data, dp, span, Success(ce))
        }
        (Rule::DataHasValueRestriction, dp!()) => {
            let dp = DataProperty::from_pair(descend(next_or_err!(pairs)?)?, ctx)?;
            let l = FromPair::from_pair(last_or_err!(pairs)?, ctx)?;
            let ce = ClassExpression::DataHasValue { dp: dp.clone(), l };

            ensure_property_kind!(Data, dp, span, Success(ce))
        }
        (Rule::DataMinCardinalityRestriction, dp!()) => {
            data_cardinality!(pairs, span, ctx, ClassExpression::DataMinCardinality)
        }
        (Rule::DataMaxCardinalityRestriction, dp!()) => {
            data_cardinality!(pairs, span, ctx, ClassExpression::DataMaxCardinality)
        }
        (Rule::DataExactCardinalityRestriction, dp!()) => {
            data_cardinality!(pairs, span, ctx, ClassExpression::DataExactCardinality)
        }
        (Rule::ObjectSomeValuesFromRestriction | Rule::DataSomeValuesFromRestriction, _) => {
            let ope: ObjectPropertyExpression<A> = ope_from_pair(next_or_err!(pairs)?, ctx)?;

            let bce =
                ce_from_primary_or_data_primary_pair(next_or_err!(pairs)?, ctx)?.map(Box::new);

            ensure_property_kind!(
                Object,
                ope,
                span,
                bce.map(|bce| ClassExpression::ObjectSomeValuesFrom {
                    ope: ope.clone(),
                    bce
                })
            )
        }
        (Rule::ObjectAllValuesFromRestriction | Rule::DataAllValuesFromRestriction, _) => {
            let ope: ObjectPropertyExpression<A> = ope_from_pair(next_or_err!(pairs)?, ctx)?;

            let bce =
                ce_from_primary_or_data_primary_pair(next_or_err!(pairs)?, ctx)?.map(Box::new);

            ensure_property_kind!(
                Object,
                ope,
                span,
                bce.map(|bce| ClassExpression::ObjectAllValuesFrom {
                    ope: ope.clone(),
                    bce
                })
            )
        }
        (Rule::ObjectHasValueRestriction | Rule::DataHasValueRestriction, _) => {
            let ope: ObjectPropertyExpression<A> = ope_from_pair(next_or_err!(pairs)?, ctx)?;

            let i = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;

            ensure_property_kind!(
                Object,
                ope,
                span,
                Success(ClassExpression::ObjectHasValue {
                    ope: ope.clone(),
                    i
                })
            )
        }
        (Rule::ObjectHasSelfRestriction, _) => {
            let ope = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
            Ok(Success(ClassExpression::ObjectHasSelf(ope)))
        }
        (Rule::ObjectMinCardinalityRestriction | Rule::DataMinCardinalityRestriction, _) => {
            object_cardinality!(pairs, span, ctx, ClassExpression::ObjectMinCardinality)
        }
        (Rule::ObjectMaxCardinalityRestriction | Rule::DataMaxCardinalityRestriction, _) => {
            object_cardinality!(pairs, span, ctx, ClassExpression::ObjectMaxCardinality)
        }
        (Rule::ObjectExactCardinalityRestriction | Rule::DataExactCardinalityRestriction, _) => {
            object_cardinality!(pairs, span, ctx, ClassExpression::ObjectExactCardinality)
        }
        rule => unexpected_rule!(ClassExpression, rule),
    }
}

fn from_atomic_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<ParseResult<'a, ClassExpression<A>>> {
    debug_assert!(pair.as_rule() == Rule::Atomic);

    let inner = descend(pair)?;
    match inner.as_rule() {
        Rule::Description => FromPair::<'a, A>::from_pair(inner, ctx),
        Rule::ClassIRI => FromPair::<'a, A>::from_pair(inner, ctx)
            .map(ClassExpression::Class)
            .map(Success),
        Rule::IndividualList => FromPair::<'a, A>::from_pair(inner, ctx)
            .map(ClassExpression::ObjectOneOf)
            .map(Success),
        rule => unexpected_rule!(ClassExpression, rule),
    }
}

fn from_primary_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<ParseResult<'a, ClassExpression<A>>> {
    debug_assert!(pair.as_rule() == Rule::Primary);

    let mut inner = pair.into_inner();
    let mut pair = next_or_err!(inner)?;

    let mut is_complement = false;

    if pair.as_rule() == Rule::NOT {
        is_complement = true;
        pair = next_or_err!(inner)?;
    }

    let ce = match pair.as_rule() {
        Rule::Restriction => from_restriction_pair(pair, ctx),
        Rule::Atomic => from_atomic_pair(pair, ctx),
        rule => unexpected_rule!(ClassExpression, rule),
    };

    ce.map(|ce| {
        if is_complement {
            ce.map(Box::new).map(ClassExpression::ObjectComplementOf)
        } else {
            ce
        }
    })
}

/// Parse an `ObjectPropertyExpression` from either an `ObjectPropertyExpression`
/// or `DataPropertyExpression` pair. Used when a `Data*Restriction` is matched
/// but the property is known to be an object property.
fn ope_from_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<ObjectPropertyExpression<A>> {
    match pair.as_rule() {
        Rule::ObjectPropertyExpression => ObjectPropertyExpression::from_pair(pair, ctx),
        Rule::DataPropertyExpression => {
            let dp = DataProperty::from_pair(descend(pair)?, ctx)?;
            Ok(ObjectPropertyExpression::ObjectProperty(ObjectProperty(
                dp.0,
            )))
        }
        rule => parse_error!(format!(
            "Unexpected rule for object property expression: {:?}",
            rule
        )),
    }
}

/// Parse a `ClassExpression` from either a `Primary` or `DataPrimary` pair.
/// Used when a `Data*Restriction` is matched but the property is known to be an object property.
fn ce_from_primary_or_data_primary_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<ParseResult<'a, ClassExpression<A>>> {
    if pair.as_rule() == Rule::Primary {
        return from_primary_pair(pair, ctx);
    }
    debug_assert!(pair.as_rule() == Rule::DataPrimary);
    let span = pair.as_span();
    let dr = DataRange::from_pair(pair, ctx)?;
    data_range_to_class_expression(dr.clone())
        .map(Success)
        .ok_or_else(|| {
            HornedError::invalid_at(
                format!(
                    "Cannot reinterpret data range '{:?}' as class expression",
                    dr
                ),
                span,
            )
        })
}

fn from_conjunction_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<ParseResult<'a, ClassExpression<A>>> {
    debug_assert!(pair.as_rule() == Rule::Conjunction);

    let span = pair.as_span();

    let mut inner = pair.into_inner();
    inner
        .peek()
        .map(|r| match r.as_rule() {
            Rule::ClassIRI => {
                let class = Class::from_pair(next_or_err!(inner)?, ctx)?;
                let mut intersection: Vec<ParseResult<ClassExpression<A>>> =
                    vec![Success(ClassExpression::Class(class))];

                while let Some(pair) = inner.next() {
                    let cexp = if pair.as_rule() == Rule::NOT {
                        from_restriction_pair(next_or_err!(inner)?, ctx)?
                            .map(|c| ClassExpression::ObjectComplementOf(Box::new(c)))
                    } else {
                        from_restriction_pair(pair, ctx)?
                    };
                    intersection.push(cexp);
                }

                let intersection = collect_parse_result(intersection.into_iter());
                Ok(intersection.map(ClassExpression::ObjectIntersectionOf))
            }
            Rule::Primary => {
                let primaries: Vec<ParseResult<ClassExpression<A>>> = inner
                    .map(|pair| from_primary_pair(pair, ctx))
                    .collect::<Result<Vec<_>>>()?;

                if primaries.len() == 1 {
                    Ok(primaries.into_iter().next().unwrap())
                } else {
                    Ok(collect_parse_result(primaries.into_iter())
                        .map(ClassExpression::ObjectIntersectionOf))
                }
            }
            rule => unexpected_rule!(ClassExpression, rule),
        })
        .unwrap_or_else(|| {
            parse_error!(
                "Expected class expression, but found empty conjunction",
                span
            )
        })
}

impl<'a, A: ForIRI> FromPair<'a, A> for ParseResult<'a, ClassExpression<A>> {
    const RULE: Rule = Rule::Description;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut inner = pair.into_inner();
        if inner.len() == 1 {
            from_conjunction_pair(next_or_err!(inner)?, ctx)
        } else {
            let union_of = inner
                .map(|pair| from_conjunction_pair(pair, ctx))
                .collect::<Result<Vec<_>>>()?;
            Ok(collect_parse_result(union_of.into_iter()).map(ClassExpression::ObjectUnionOf))
        }
    }
}

// --- DataRange -------------------------------------------------------------

// Similarly to class expressions, data ranges can be parsed from several
// production rules.

fn from_data_conjunction_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<DataRange<A>> {
    debug_assert!(pair.as_rule() == Rule::DataConjunction);

    let mut ranges = pair
        .into_inner()
        .map(|pair| DataRange::from_pair(pair, ctx))
        .collect::<Result<Vec<_>>>()?;
    if ranges.len() == 1 {
        Ok(ranges.pop().unwrap())
    } else {
        Ok(DataRange::DataIntersectionOf(ranges))
    }
}

fn from_data_range_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<DataRange<A>> {
    debug_assert!(pair.as_rule() == Rule::DataRange);

    let mut ranges = pair
        .into_inner()
        .map(|pair| from_data_conjunction_pair(pair, ctx))
        .collect::<Result<Vec<_>>>()?;
    if ranges.len() == 1 {
        Ok(ranges.pop().unwrap())
    } else {
        Ok(DataRange::DataUnionOf(ranges))
    }
}

fn from_data_atomic_pair<'a, A: ForIRI>(
    pair: Pair<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<DataRange<A>> {
    debug_assert!(pair.as_rule() == Rule::DataAtomic);

    let inner = descend(pair)?;
    match inner.as_rule() {
        Rule::DataRange => from_data_range_pair(inner, ctx),
        Rule::DatatypeRestriction => {
            let mut pairs = inner.into_inner();
            let dt = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
            let restrictions = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
            Ok(DataRange::DatatypeRestriction(dt, restrictions))
        }
        Rule::Datatype => {
            let dt = FromPair::from_pair(inner, ctx)?;
            Ok(DataRange::Datatype(dt))
        }
        Rule::LiteralList => {
            let literals = FromPair::from_pair(inner, ctx)?;
            Ok(DataRange::DataOneOf(literals))
        }
        rule => unexpected_rule!(DataRange, rule),
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for DataRange<A> {
    const RULE: Rule = Rule::DataPrimary;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::DataAtomic => from_data_atomic_pair(inner, ctx),
            Rule::DataAtomicComplement => {
                let pair = descend(inner)?;
                from_data_atomic_pair(pair, ctx)
                    .map(Box::new)
                    .map(DataRange::DataComplementOf)
            }
            rule => unexpected_rule!(DataRange, rule),
        }
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for Facet {
    const RULE: Rule = Rule::Facet;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, _ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        let facet = match inner.as_rule() {
            Rule::FacetLength => Facet::Length,
            Rule::FacetMinLength => Facet::MinLength,
            Rule::FacetMaxLength => Facet::MaxLength,
            Rule::FacetPattern => Facet::Pattern,
            Rule::FacetLangRange => Facet::LangRange,
            Rule::FacetMinInclusive => Facet::MinInclusive,
            Rule::FacetMinExclusive => Facet::MinExclusive,
            Rule::FacetMaxInclusive => Facet::MaxInclusive,
            Rule::FacetMaxExclusive => Facet::MaxExclusive,
            rule => unexpected_rule!(Facet, rule)?,
        };
        Ok(facet)
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for FacetRestriction<A> {
    const RULE: Rule = Rule::FacetRestriction;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut inner = pair.into_inner();
        let f = FromPair::from_pair(next_or_err!(inner)?, ctx)?;
        let l = FromPair::from_pair(descend(next_or_err!(inner)?)?, ctx)?;
        Ok(FacetRestriction { f, l })
    }
}

// --- Datatype --------------------------------------------------------------

// The Manchester syntax has builtin support for the `float`, `integer`,
// `decimal` and `string` datatypes, which are translated into their `xsd`
// equivalent. The `xsd` prefix should always be available as mandated by
// the specification, so CURIE expansion should always work, but as a backend
// we can use hardcoded IRIs.
// (see https://www.w3.org/TR/owl2-manchester-syntax/#Ontologies_and_Annotations)

impl<'a, A: ForIRI> FromPair<'a, A> for Datatype<A> {
    const RULE: Rule = Rule::Datatype;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        macro_rules! xsd_datatype {
            ($ctx:ident, xsd: $datatype:expr) => {{
                Ok($ctx.build.datatype(concat!(
                    "http://www.w3.org/2001/XMLSchema#",
                    stringify!($datatype)
                )))
            }};
        }

        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::IntegerDatatype => xsd_datatype!(ctx, xsd:integer),
            Rule::DecimalDatatype => xsd_datatype!(ctx, xsd:decimal),
            Rule::FloatDatatype => xsd_datatype!(ctx, xsd:float),
            Rule::StringDatatype => xsd_datatype!(ctx, xsd:string),
            Rule::DatatypeIRI => FromPair::from_pair(descend(inner)?, ctx).map(Datatype),
            rule => unexpected_rule!(ClassFrame, rule),
        }
    }
}

// ---------------------------------------------------------------------------

impl<'a, A: ForIRI> FromPair<'a, A> for Individual<A> {
    const RULE: Rule = Rule::Individual;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::IndividualIRI => FromPair::from_pair(inner, ctx).map(Individual::Named),
            Rule::NodeID => FromPair::from_pair(inner, ctx).map(Individual::Anonymous),
            rule => unexpected_rule!(Individual, rule),
        }
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for AnonymousIndividual<A> {
    const RULE: Rule = Rule::NodeID;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let iri = ctx.build.iri(pair.as_str());
        Ok(AnonymousIndividual(iri.underlying()))
    }
}

// ---------------------------------------------------------------------------

impl<'a, A: ForIRI> FromPair<'a, A> for Literal<A> {
    const RULE: Rule = Rule::Literal;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        macro_rules! xsd_literal {
            ($pair:ident, $ctx:ident, xsd: $datatype:expr) => {{
                let literal = $pair.as_str().to_string();
                let datatype_iri = $ctx.build.iri(concat!(
                    "http://www.w3.org/2001/XMLSchema#",
                    stringify!($datatype)
                ));
                Ok(Literal::Datatype {
                    literal,
                    datatype_iri,
                })
            }};
        }

        let pair = descend(pair)?;
        match pair.as_rule() {
            Rule::TypedLiteral => {
                let mut inner = pair.into_inner();
                let literal = String::from_pair(next_or_err!(inner)?, ctx)?;
                let dty = Datatype::from_pair(next_or_err!(inner)?, ctx)?;
                Ok(Literal::Datatype {
                    literal,
                    datatype_iri: dty.0,
                })
            }
            Rule::StringLiteralWithLanguage => {
                let mut inner = pair.into_inner();
                let literal = String::from_pair(next_or_err!(inner)?, ctx)?;
                let lang = next_or_err!(inner)?.as_str()[1..].trim().to_string();
                Ok(Literal::Language { literal, lang })
            }
            Rule::StringLiteralNoLanguage => {
                let mut inner = pair.into_inner();
                let literal = String::from_pair(next_or_err!(inner)?, ctx)?;
                Ok(Literal::Simple { literal })
            }
            Rule::IntegerLiteral => xsd_literal!(pair, ctx, xsd:integer),
            Rule::DecimalLiteral => xsd_literal!(pair, ctx, xsd:decimal),
            Rule::FloatingPointLiteral => xsd_literal!(pair, ctx, xsd:float),
            Rule::BooleanLiteral => xsd_literal!(pair, ctx, xsd:boolean),
            rule => unexpected_rule!(Literal, rule),
        }
    }
}

// ---------------------------------------------------------------------------

#[derive(Default)]
pub(crate) struct MutableOntologyWrapper<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default>(
    pub(crate) O,
    PhantomData<A>,
);

impl<'a, A: ForIRI> FromPair<'a, A> for OntologyID<A> {
    const RULE: Rule = Rule::OntologyID;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        let iri = Some(FromPair::<A>::from_pair(next_or_err!(pairs)?, ctx)?);
        let viri = match pairs.next() {
            Some(pair) => Some(FromPair::<A>::from_pair(pair, ctx)?),
            None => None,
        };

        Ok(OntologyID { iri, viri })
    }
}

impl<'a, A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default> FromPair<'a, A>
    for MutableOntologyWrapper<A, O>
{
    const RULE: Rule = Rule::Ontology;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        let mut pair = next_or_err!(&mut pairs)?;

        let mut ontology: O = Default::default();

        // Parse ontology IRI and Version IRI if any
        if pair.as_rule() == Rule::OntologyID {
            let mut inners = pair.into_inner();
            let iri = Some(IRI::from_pair(next_or_err!(inners)?, ctx)?);

            let viri = if let Some(inner) = inners.next() {
                Some(IRI::from_pair(inner, ctx)?)
            } else {
                None
            };

            ontology.insert(OntologyID::new(iri, viri));

            pair = next_or_err!(&mut pairs)?;
        }

        // Process imports
        for p in pair.into_inner() {
            ontology.insert(Import::from_pair(p, ctx)?);
        }

        // Process ontology annotations
        for annotations_pair in next_or_err!(pairs)?.into_inner() {
            let mut annotated_list = descend(annotations_pair)?.into_inner();
            while let Some(mut item) = annotated_list.next() {
                let ann = component_annotations(&mut item, &mut annotated_list, ctx)?;
                let annotation = OntologyAnnotation::from_pair(item, ctx)?;
                ontology.insert(AnnotatedComponent {
                    ann,
                    component: Component::OntologyAnnotation(annotation),
                });
            }
        }

        let frames = pairs;

        // Process frames
        for pair in frames {
            debug_assert!(pair.as_rule() == Rule::Frame);
            let inner = descend(pair)?;
            let components = match inner.as_rule() {
                Rule::DatatypeFrame => DatatypeFrame::from_pair(inner, ctx)?.into_components(),
                Rule::ClassFrame => {
                    let class_components: ParseResult<ClassFrame<A>> =
                        FromPair::from_pair(inner, ctx)?;
                    match class_components {
                        Success(frame) => frame.into_components(),
                        Ambiguous(frame, span) => {
                            for component in frame.into_components() {
                                ctx.add_ambiguous_component(component, span);
                            }
                            vec![]
                        }
                    }
                }
                Rule::ObjectPropertyFrame => {
                    ObjectPropertyFrame::from_pair(inner, ctx)?.into_components()
                }
                Rule::DataPropertyFrame => {
                    DataPropertyFrame::from_pair(inner, ctx)?.into_components()
                }
                Rule::AnnotationPropertyFrame => {
                    AnnotationPropertyFrame::from_pair(inner, ctx)?.into_components()
                }
                Rule::IndividualFrame => IndividualFrame::from_pair(inner, ctx)?.into_components(),
                Rule::MiscClause => {
                    let clause: ParseResult<MiscClause<A>> = FromPair::from_pair(inner, ctx)?;

                    match clause {
                        Success(c) => c.0.map(|component| vec![component]).unwrap_or_default(),
                        Ambiguous(MiscClause(Some(c)), span) => {
                            ctx.add_ambiguous_component(c, span);
                            vec![]
                        }
                        _ => vec![],
                    }
                }
                rule => unexpected_rule!(Frame, rule)?,
            };
            for component in components {
                ontology.insert(component);
            }
        }

        Ok(MutableOntologyWrapper(ontology, Default::default()))
    }
}

impl<'a, A, O> FromPair<'a, A> for (MutableOntologyWrapper<A, O>, PrefixMapping)
where
    A: ForIRI,
    O: Ontology<A> + MutableOntology<A> + Default,
{
    const RULE: Rule = Rule::OntologyDocument;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        // Register prefix declarations into ctx so IRI expansion works during parsing
        for inner in next_or_err!(pairs)?.into_inner() {
            debug_assert!(inner.as_rule() == Rule::PrefixDeclaration);
            let mut decl = inner.into_inner();
            let mut pname = next_or_err!(decl)?.into_inner();
            let iri = descend(last_or_err!(decl)?)?;

            if let Some(prefix) = next_or_err!(pname)?.into_inner().next() {
                ctx.mapping
                    .add_prefix(prefix.as_str(), iri.as_str())
                    .map_err(|_| {
                        HornedError::invalid_at("The prefix `\"_\"` is reserved.", prefix.as_span())
                    })?;
            } else {
                ctx.mapping.set_default(iri.as_str());
            }
        }

        let prefixes = ctx.mapping.clone();
        MutableOntologyWrapper::from_pair(next_or_err!(pairs)?, ctx).map(|ont| (ont, prefixes))
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for OntologyAnnotation<A> {
    const RULE: Rule = Rule::Annotation;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        Annotation::from_pair(pair, ctx).map(OntologyAnnotation)
    }
}

// ---------------------------------------------------------------------------

macro_rules! annotated_component {
    ($pair:ident, $inner:ident, $ctx:ident, $frame:ident, component = $component:expr) => {{
        let mut annotated_list = descend($inner)?.into_inner();
        while let Some(mut $pair) = annotated_list.next() {
            let ann = component_annotations(&mut $pair, &mut annotated_list, $ctx)?;
            let component = $component;
            match component {
                Success(component) => $frame
                    .components
                    .push(AnnotatedComponent { ann, component }),
                Ambiguous(component, span) => {
                    $ctx.add_ambiguous_component(AnnotatedComponent { ann, component }, span);
                }
            }
        }
    }};
}

macro_rules! simple_component {
    ($variant:ident($($arg:expr),*)) => {
        Component::$variant($variant($($arg),*))
    };
    ($variant:ident { $($field:ident),* $(,)? }) => {
        Component::$variant($variant { $($field),* })
    };
    ($variant:ident { $($field:ident: $val:expr),* }) => {
        Component::$variant($variant { $($field: $val),* })
    };
}

fn parse_optional_annotations<'a, A: ForIRI>(
    pairs: &mut Pairs<'a, Rule>,
    ctx: &mut Context<'a, A>,
) -> Result<BTreeSet<Annotation<A>>> {
    if pairs.peek().map(|p| p.as_rule()) == Some(Rule::Annotations) {
        let pair = next_or_err!(pairs)?;
        return BTreeSet::from_pair(descend(pair)?, ctx);
    }

    Ok(BTreeSet::new())
}

impl<'a, A: ForIRI> FromPair<'a, A> for DatatypeFrame<A> {
    const RULE: Rule = Rule::DatatypeFrame;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        let annotations = parse_optional_annotations(&mut pairs, ctx)?;
        let datatype = Datatype::from_pair(next_or_err!(pairs)?, ctx)?;

        ctx.record_entity_kind(datatype.clone(), NamedEntityKind::Datatype);

        let mut frame = DatatypeFrame::new(datatype, annotations);

        for pair in pairs {
            debug_assert!(pair.as_rule() == Rule::DatatypeClause);
            let inner = descend(pair)?;
            match inner.as_rule() {
                Rule::DatatypeAnnotationsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ann = FromPair::from_pair(pair, ctx)?;
                            let subject = AnnotationSubject::IRI(frame.entity.0.clone());
                            Success(AnnotationAssertion { subject, ann }.into())
                        }
                    )
                }
                Rule::DatatypeEquivalentToClause => {
                    let mut pairs = inner.into_inner();
                    let mut pair = next_or_err!(pairs)?;
                    let ann = component_annotations(&mut pair, &mut pairs, ctx)?;

                    let range = from_data_range_pair(pair, ctx)?;
                    let kind = frame.entity.clone();

                    let component = DatatypeDefinition { kind, range }.into();
                    frame.components.push(AnnotatedComponent { ann, component });
                }
                rule => unexpected_rule!(ClassFrame, rule)?,
            }
        }

        Ok(frame)
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for ParseResult<'a, ClassFrame<A>> {
    const RULE: Rule = Rule::ClassFrame;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        let annotations = parse_optional_annotations(&mut pairs, ctx)?;
        let class_expr: ParseResult<ClassExpression<A>> =
            FromPair::from_pair(next_or_err!(pairs)?, ctx)?;

        if let Success(ClassExpression::Class(c)) = &class_expr {
            ctx.record_entity_kind(c.clone(), NamedEntityKind::Class);
        }

        class_expr.map(|class_expr| {

        let mut frame = ClassFrame::new(class_expr, annotations);

        for pair in pairs {
            debug_assert!(pair.as_rule() == Rule::ClassClause);
            let inner = descend(pair)?;
            match inner.as_rule() {
                Rule::ClassAnnotationsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ann = FromPair::from_pair(pair, ctx)?;
                            let subject = match &frame.entity {
                                ClassExpression::Class(c) => AnnotationSubject::IRI(c.0.clone()),
                                _ => {
                                    return Err(HornedError::invalid(
                                        "AnnotationAssertion requires a named class subject",
                                    ));
                                }
                            };
                            Success(AnnotationAssertion { subject, ann }.into())
                        }
                    )
                }
                Rule::ClassSubClassOfClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let sup: ParseResult<ClassExpression<A>> =
                                FromPair::from_pair(pair, ctx)?;
                            sup.map(|sup| {
                                SubClassOf {
                                    sup: sup,
                                    sub: frame.entity.clone(),
                                }
                                .into()
                            })
                        }
                    )
                }
                Rule::ClassEquivalentToClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ce: ParseResult<ClassExpression<A>> =
                                FromPair::from_pair(pair, ctx)?;
                            ce.map(|ce| EquivalentClasses(vec![frame.entity.clone(), ce]).into())
                        }
                    )
                }
                Rule::ClassDisjointWithClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ce: ParseResult<ClassExpression<A>> =
                                FromPair::from_pair(pair, ctx)?;
                            ce.map(|ce| DisjointClasses(vec![frame.entity.clone(), ce]).into())
                        }
                    )
                }
                Rule::ClassDisjointUnionOfClause => {
                    let mut value = inner.into_inner();
                    let mut pair = value.next().unwrap();
                    let ann = component_annotations(&mut pair, &mut value, ctx)?;
                    let descriptions = pair
                        .into_inner()
                        .map(|pair| FromPair::from_pair(pair, ctx))
                        .collect::<Result<Vec<ParseResult<ClassExpression<A>>>>>()?;
                    let descriptions = collect_parse_result(descriptions.into_iter());
                    let class = match &frame.entity {
                        ClassExpression::Class(c) => c.clone(),
                        _ => {
                            return Err(HornedError::invalid(
                                "DisjointUnion requires a named class",
                            ));
                        }
                    };

                    let component =
                        descriptions.map(|descriptions| DisjointUnion(class, descriptions).into());

                    match component {
                        Success(component) => {
                            frame.components.push(AnnotatedComponent { component, ann })
                        }
                        Ambiguous(component, span) => ctx.add_ambiguous_component(AnnotatedComponent { ann, component }, span),
                    }
                }
                Rule::ClassHasKeyClause => {
                    let mut value = inner.into_inner().peekable();
                    let ann = if value.peek().unwrap().as_rule() == Rule::Annotations {
                        FromPair::from_pair(value.next().unwrap(), ctx)?
                    } else {
                        Default::default()
                    };
                    let ce = frame.entity.clone();
                    let vpe = value
                        .map(|pair| FromPair::from_pair(pair, ctx))
                        .collect::<Result<Vec<ParseResult<PropertyExpression<A>>>>>()?;
                    let component = collect_parse_result(vpe.into_iter()).map(|vpe| HasKey { ce, vpe }.into());

                    match component {
                        Success(component) => {
                            frame.components.push(AnnotatedComponent { component, ann });
                        }
                        Ambiguous(component, span) => {
                            ctx.add_ambiguous_component(AnnotatedComponent { ann, component }, span);
                        }
                    }
                }
                rule => unexpected_rule!(ClassFrame, rule)?,
            }
        }

        Ok(frame)
    }).into()
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for ObjectPropertyFrame<A> {
    const RULE: Rule = Rule::ObjectPropertyFrame;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        let annotations = parse_optional_annotations(&mut pairs, ctx)?;
        let op = ObjectPropertyExpression::from_pair(next_or_err!(pairs)?, ctx)?;

        if let ObjectPropertyExpression::ObjectProperty(op) = &op {
            ctx.record_entity_kind(op.clone(), NamedEntityKind::ObjectProperty);
        }

        let mut frame = ObjectPropertyFrame::new(op, annotations);

        for pair in pairs {
            debug_assert!(pair.as_rule() == Rule::ObjectPropertyClause);
            let inner = descend(pair)?;
            match inner.as_rule() {
                Rule::ObjectPropertyAnnotationsClause
                    if let Some(frame_entity) = frame.entity.as_property() =>
                {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ann = FromPair::from_pair(pair, ctx)?;
                            let subject = AnnotationSubject::IRI(frame_entity.0.clone());
                            Success(AnnotationAssertion { subject, ann }.into())
                        }
                    )
                }
                Rule::ObjectPropertyDomainClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ce: ParseResult<ClassExpression<A>> =
                                FromPair::from_pair(pair, ctx)?;
                            let ope = frame.entity.clone().into();
                            ce.map(|ce| ObjectPropertyDomain { ope, ce }.into())
                        }
                    )
                }
                Rule::ObjectPropertyRangeClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ce: ParseResult<ClassExpression<A>> =
                                FromPair::from_pair(pair, ctx)?;
                            let ope = frame.entity.clone().into();
                            ce.map(|ce| ObjectPropertyRange { ope, ce }.into())
                        }
                    )
                }
                Rule::ObjectPropertyCharacteristicsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let op = frame.entity.clone();
                            Success(match descend(pair)?.as_rule() {
                                Rule::FunctionalCharacteristic => {
                                    simple_component!(FunctionalObjectProperty(op))
                                }
                                Rule::InverseFunctionalCharacteristic => {
                                    simple_component!(InverseFunctionalObjectProperty(op))
                                }
                                Rule::ReflexiveCharacteristic => {
                                    simple_component!(ReflexiveObjectProperty(op))
                                }
                                Rule::IrreflexiveCharacteristic => {
                                    simple_component!(IrreflexiveObjectProperty(op))
                                }
                                Rule::SymmetricCharacteristic => {
                                    simple_component!(SymmetricObjectProperty(op))
                                }
                                Rule::AsymmetricCharacteristic => {
                                    simple_component!(AsymmetricObjectProperty(op))
                                }
                                Rule::TransitiveCharacteristic => {
                                    simple_component!(TransitiveObjectProperty(op))
                                }
                                rule => unexpected_rule!(ObjectPropertyFrame, rule)?,
                            })
                        }
                    )
                }
                Rule::ObjectPropertySubPropertyOfClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            Success(
                                SubObjectPropertyOf {
                                    sup: ObjectPropertyExpression::from_pair(pair, ctx)?,
                                    sub: SubObjectPropertyExpression::ObjectPropertyExpression(
                                        frame.entity.clone().into(),
                                    ),
                                }
                                .into(),
                            )
                        }
                    )
                }
                Rule::ObjectPropertyEquivalentToClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            Success(
                                EquivalentObjectProperties(vec![
                                    frame.entity.clone(),
                                    ObjectPropertyExpression::from_pair(pair, ctx)?,
                                ])
                                .into(),
                            )
                        }
                    )
                }
                Rule::ObjectPropertyDisjointWithClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            Success(
                                DisjointObjectProperties(vec![
                                    frame.entity.clone(),
                                    ObjectPropertyExpression::from_pair(pair, ctx)?,
                                ])
                                .into(),
                            )
                        }
                    )
                }
                Rule::ObjectPropertyInverseOfClause
                    if let Some(frame_entity) = frame.entity.as_property() =>
                {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let pair = descend(pair)?;
                            let op: ObjectProperty<A> = match pair.as_rule() {
                                Rule::ObjectPropertyIRI => {
                                    IRI::from_pair(descend(pair)?, ctx)?.into()
                                }
                                Rule::InverseObjectProperty => {
                                    // FIXME: currently unsupported in `horned-owl`
                                    return parse_error!(
                                        "InverseOf cannot contain inverse object property expression",
                                        pair.as_span()
                                    );
                                }
                                rule => unexpected_rule!(ObjectPropertyExpression, rule)?,
                            };
                            Success(InverseObjectProperties(op, frame_entity.clone()).into())
                        }
                    )
                }
                Rule::ObjectPropertySubPropertyChainClause => {
                    let mut chainlist = inner.into_inner();
                    let mut pair = next_or_err!(chainlist)?;
                    let ann = component_annotations(&mut pair, &mut chainlist, ctx)?;
                    let first = ObjectPropertyExpression::from_pair(pair, ctx)?;
                    let mut chain = chainlist
                        .map(|pair| FromPair::from_pair(pair, ctx))
                        .collect::<Result<Vec<_>>>()?;
                    chain.insert(0, first);

                    let component = SubObjectPropertyOf {
                        sup: frame.entity.clone().into(),
                        sub: SubObjectPropertyExpression::ObjectPropertyChain(chain),
                    }
                    .into();
                    frame.components.push(AnnotatedComponent { ann, component });
                }
                rule => unexpected_rule!(ObjectPropertyFrame, rule)?,
            }
        }

        Ok(frame)
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for DataPropertyFrame<A> {
    const RULE: Rule = Rule::DataPropertyFrame;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        let annotations = parse_optional_annotations(&mut pairs, ctx)?;
        let dt = DataProperty::from_pair(next_or_err!(pairs)?, ctx)?;

        ctx.record_entity_kind(dt.clone(), NamedEntityKind::DataProperty);

        let mut frame = DataPropertyFrame::new(dt, annotations);

        for pair in pairs {
            debug_assert!(pair.as_rule() == Rule::DataPropertyClause);
            let inner = descend(pair)?;
            match inner.as_rule() {
                Rule::DataPropertyAnnotationsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ann = FromPair::from_pair(pair, ctx)?;
                            let subject = AnnotationSubject::IRI(frame.entity.0.clone());
                            Success(AnnotationAssertion { subject, ann }.into())
                        }
                    )
                }
                Rule::DataPropertyDomainClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let dp = frame.entity.clone();
                            let ce: ParseResult<ClassExpression<A>> =
                                FromPair::from_pair(pair, ctx)?;
                            ce.map(|ce| DataPropertyDomain { dp, ce }.into())
                        }
                    )
                }
                Rule::DataPropertyRangeClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let dp = frame.entity.clone();
                            let dr = from_data_range_pair(pair, ctx)?;
                            Success(DataPropertyRange { dp, dr }.into())
                        }
                    )
                }
                Rule::DataPropertyCharacteristicsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let dp = frame.entity.clone();
                            match descend(pair)?.as_rule() {
                                Rule::FunctionalCharacteristic => {
                                    Success(simple_component!(FunctionalDataProperty(dp)))
                                }
                                rule => unexpected_rule!(ObjectPropertyFrame, rule)?,
                            }
                        }
                    )
                }
                Rule::DataPropertySubPropertyOfClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let inner = descend(pair)?;
                            Success(
                                SubDataPropertyOf {
                                    sup: DataProperty::from_pair(inner, ctx)?,
                                    sub: frame.entity.clone(),
                                }
                                .into(),
                            )
                        }
                    )
                }
                Rule::DataPropertyEquivalentToClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            Success(
                                EquivalentDataProperties(vec![
                                    frame.entity.clone(),
                                    DataProperty::from_pair(descend(pair)?, ctx)?,
                                ])
                                .into(),
                            )
                        }
                    )
                }
                Rule::DataPropertyDisjointWithClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            Success(
                                DisjointDataProperties(vec![
                                    frame.entity.clone(),
                                    DataProperty::from_pair(descend(pair)?, ctx)?,
                                ])
                                .into(),
                            )
                        }
                    )
                }
                rule => unexpected_rule!(DataPropertyFrame, rule)?,
            }
        }

        Ok(frame)
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for AnnotationPropertyFrame<A> {
    const RULE: Rule = Rule::AnnotationPropertyFrame;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        let annotations = parse_optional_annotations(&mut pairs, ctx)?;
        let ap = AnnotationProperty::from_pair(next_or_err!(pairs)?, ctx)?;

        ctx.record_entity_kind(ap.clone(), NamedEntityKind::AnnotationProperty);

        let mut frame = AnnotationPropertyFrame::new(ap, annotations);

        for pair in pairs {
            debug_assert!(pair.as_rule() == Rule::AnnotationPropertyClause);
            let inner = descend(pair)?;
            match inner.as_rule() {
                Rule::AnnotationPropertyAnnotationsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ann = FromPair::from_pair(pair, ctx)?;
                            let subject = AnnotationSubject::IRI(frame.entity.0.clone());
                            Success(AnnotationAssertion { subject, ann }.into())
                        }
                    )
                }
                Rule::AnnotationPropertyDomainClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let iri = FromPair::from_pair(pair, ctx)?;
                            let ap = frame.entity.clone();
                            Success(AnnotationPropertyDomain { ap, iri }.into())
                        }
                    )
                }
                Rule::AnnotationPropertyRangeClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let iri = FromPair::from_pair(pair, ctx)?;
                            let ap = frame.entity.clone();
                            Success(AnnotationPropertyRange { ap, iri }.into())
                        }
                    )
                }
                Rule::AnnotationPropertySubPropertyOfClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            Success(
                                SubAnnotationPropertyOf {
                                    sup: AnnotationProperty::from_pair(pair, ctx)?,
                                    sub: frame.entity.clone(),
                                }
                                .into(),
                            )
                        }
                    )
                }
                rule => unexpected_rule!(AnnotationPropertyFrame, rule)?,
            }
        }

        Ok(frame)
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for IndividualFrame<A> {
    const RULE: Rule = Rule::IndividualFrame;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let mut pairs = pair.into_inner();

        let annotations = parse_optional_annotations(&mut pairs, ctx)?;
        let individual = Individual::from_pair(next_or_err!(pairs)?, ctx)?;

        if let Individual::Named(named) = &individual {
            ctx.record_entity_kind(named.clone(), NamedEntityKind::NamedIndividual);
        }

        let mut frame = IndividualFrame::new(individual, annotations);

        for pair in pairs {
            debug_assert!(pair.as_rule() == Rule::IndividualClause);
            let inner = descend(pair)?;
            match inner.as_rule() {
                Rule::IndividualAnnotationsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let ann = FromPair::from_pair(pair, ctx)?;
                            let subject = match &frame.entity {
                                Individual::Anonymous(anon) => {
                                    AnnotationSubject::AnonymousIndividual(anon.clone())
                                }
                                Individual::Named(anon) => AnnotationSubject::IRI(anon.0.clone()),
                            };
                            Success(AnnotationAssertion { subject, ann }.into())
                        }
                    )
                }
                Rule::IndividualTypesClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let i = frame.entity.clone();
                            let ce: ParseResult<ClassExpression<A>> =
                                FromPair::from_pair(pair, ctx)?;
                            ce.map(|ce| ClassAssertion { ce, i }.into())
                        }
                    )
                }
                Rule::IndividualSameAsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let i1 = frame.entity.clone();
                            let i2 = Individual::from_pair(pair, ctx)?;
                            Success(SameIndividual(vec![i1, i2]).into())
                        }
                    )
                }
                Rule::IndividualDifferentFromClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let i1 = frame.entity.clone();
                            let i2 = Individual::from_pair(pair, ctx)?;
                            Success(DifferentIndividuals(vec![i1, i2]).into())
                        }
                    )
                }
                Rule::IndividualFactsClause => {
                    annotated_component!(
                        pair,
                        inner,
                        ctx,
                        frame,
                        component = {
                            let mut fact = descend(pair)?;
                            let negative = if fact.as_rule() == Rule::InverseFact {
                                fact = descend(fact)?;
                                true
                            } else {
                                false
                            };

                            let from = frame.entity.clone();
                            Success(match fact.as_rule() {
                                Rule::ObjectPropertyFact => {
                                    let mut pairs = fact.into_inner();
                                    let op = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
                                    let to = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
                                    let ope = ObjectPropertyExpression::ObjectProperty(op);
                                    if negative {
                                        simple_component!(NegativeObjectPropertyAssertion {
                                            ope,
                                            from,
                                            to
                                        })
                                    } else {
                                        ObjectPropertyAssertion { ope, from, to }.into()
                                    }
                                }
                                Rule::DataPropertyFact => {
                                    let mut pairs = fact.into_inner();
                                    let dp = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
                                    let to = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
                                    if negative {
                                        NegativeDataPropertyAssertion { dp, from, to }.into()
                                    } else {
                                        DataPropertyAssertion { dp, from, to }.into()
                                    }
                                }
                                rule => unexpected_rule!(AnnotationPropertyFrame, rule)?,
                            })
                        }
                    )
                }
                rule => unexpected_rule!(AnnotationPropertyFrame, rule)?,
            }
        }

        Ok(frame)
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for ParseResult<'a, MiscClause<A>> {
    const RULE: Rule = Rule::MiscClause;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        macro_rules! entity_list {
            ($inner:ident, $ctx:ident, $clause:ident) => {{
                entity_list!($inner, $ctx, $clause, |pair| FromPair::from_pair(
                    pair, $ctx
                ))
            }};
            ($inner:ident, $ctx:ident, $clause:ident, $closure:expr) => {{
                let mut pairs = $inner.into_inner();
                let mut pair = next_or_err!(pairs)?;
                let ann = component_annotations(&mut pair, &mut pairs, $ctx)?;
                let entities = pair
                    .into_inner()
                    .map($closure)
                    .collect::<Result<Vec<ParseResult<_>>>>()?;
                let entities = collect_parse_result(entities.into_iter());
                let component: ParseResult<Component<A>> = entities.map(|e| $clause(e).into());
                Ok(component.map(|c| MiscClause::new(AnnotatedComponent { ann, component: c })))
            }};
        }

        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::MiscEquivalentClassesClause => {
                entity_list!(inner, ctx, EquivalentClasses)
            }
            Rule::MiscDisjointClassesClause => {
                entity_list!(inner, ctx, DisjointClasses)
            }
            Rule::MiscEquivalentObjectPropertiesClause => {
                entity_list!(inner, ctx, EquivalentObjectProperties)
            }
            Rule::MiscDisjointObjectPropertiesClause => {
                entity_list!(inner, ctx, DisjointObjectProperties)
            }
            Rule::MiscEquivalentDataPropertiesClause => {
                entity_list!(inner, ctx, EquivalentDataProperties, |pair| {
                    FromPair::from_pair(descend(pair)?, ctx)
                })
            }
            Rule::MiscDisjointDataPropertiesClause => {
                entity_list!(inner, ctx, DisjointDataProperties, |pair| {
                    FromPair::from_pair(descend(pair)?, ctx)
                })
            }
            Rule::MiscSameIndividualClause => {
                entity_list!(inner, ctx, SameIndividual)
            }
            Rule::MiscDifferentIndividualsClause => {
                entity_list!(inner, ctx, DifferentIndividuals)
            }
            Rule::MiscRuleClause => {
                let rule = ParseResult::<SWRLRule<A>>::from_pair(inner, ctx)?;
                Ok(rule.map(|r| MiscClause(Some(r.into()))))
            }
            rule => unexpected_rule!(MiscClause, rule),
        }
    }
}

// ---------------------------------------------------------------------------

impl<'a, A: ForIRI> FromPair<'a, A> for ParseResult<'a, SWRLRule<A>> {
    const RULE: Rule = Rule::MiscRuleClause;

    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        debug_assert!(inner.as_rule() == Rule::SWRLRule);

        let mut inner = inner.into_inner();

        // parse antecedent list
        let pair = next_or_err!(inner)?;
        debug_assert!(pair.as_rule() == Rule::SWRLAntecedentList);
        let body = pair
            .into_inner()
            .map(|pair| {
                debug_assert!(pair.as_rule() == Rule::SWRLAntecedent);
                ParseResult::<Atom<A>>::from_pair(descend(pair)?, ctx)
            })
            .collect::<Result<Vec<_>>>()?;
        let body = collect_parse_result(body.into_iter());

        // parse consequent list
        let pair = next_or_err!(inner)?;
        debug_assert!(pair.as_rule() == Rule::SWRLConsequentList);
        let head = pair
            .into_inner()
            .map(|pair| {
                debug_assert!(pair.as_rule() == Rule::SWRLConsequent);
                ParseResult::<Atom<A>>::from_pair(descend(pair)?, ctx)
            })
            .collect::<Result<Vec<_>>>()?;
        let head = collect_parse_result(head.into_iter());

        Ok(ParseResult::<(Vec<_>, Vec<_>)>::from((head, body))
            .map(|(head, body)| SWRLRule { head, body }))
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for ParseResult<'a, Atom<A>> {
    const RULE: Rule = Rule::SWRLAtom;

    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        let span = inner.as_span();
        match inner.as_rule() {
            // Unary
            Rule::SWRLAmbiguousUnaryAtom => {
                let mut pairs = inner.into_inner();
                let iri = IRI::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg = Variable::from_pair(next_or_err!(pairs)?, ctx)?;

                match ctx.get_property_kind(&iri) {
                    Some(NamedEntityKind::Class) => Ok(Success(Atom::ClassAtom {
                        pred: iri.into(),
                        arg: arg.into(),
                    })),
                    Some(NamedEntityKind::Datatype) => Ok(Success(Atom::DataRangeAtom {
                        pred: iri.into(),
                        arg: DArgument::Variable(arg),
                    })),
                    None => Ok(Ambiguous(
                        Atom::ClassAtom {
                            pred: iri.into(),
                            arg: arg.into(),
                        },
                        span,
                    )),
                    Some(kind) => parse_error!(
                        format!(
                            "property used in atom has incompatible kind: expected object or data property, found {:?}",
                            kind
                        ),
                        span
                    ),
                }
            }
            Rule::SWRLClassAtom => {
                let mut pairs = inner.into_inner();
                let ce: ParseResult<ClassExpression<A>> =
                    FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
                let i = FromPair::from_pair(next_or_err!(pairs)?, ctx)?;
                Ok(ce.map(|pred| Atom::ClassAtom { pred, arg: i }))
            }
            Rule::SWRLDataRangeAtom => {
                let mut pairs = inner.into_inner();
                let dr = from_data_range_pair(next_or_err!(pairs)?, ctx)?;
                let arg = DArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                Ok(Success(Atom::DataRangeAtom { pred: dr, arg }))
            }
            // Binary
            Rule::SWRLAmbiguousBinaryAtom => {
                let mut pairs = inner.into_inner();
                let iri = IRI::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg1 = IArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg2 = Variable::from_pair(next_or_err!(pairs)?, ctx)?;

                match ctx.get_property_kind(&iri) {
                    Some(NamedEntityKind::ObjectProperty) => {
                        Ok(Success(Atom::ObjectPropertyAtom {
                            pred: iri.into(),
                            args: (arg1.into(), arg2.into()),
                        }))
                    }
                    Some(NamedEntityKind::DataProperty) => Ok(Success(Atom::DataPropertyAtom {
                        pred: iri.into(),
                        args: (arg1.into(), arg2.into()),
                    })),
                    None => Ok(Ambiguous(
                        Atom::DataPropertyAtom {
                            pred: iri.into(),
                            args: (arg1.into(), arg2.into()),
                        },
                        span,
                    )),
                    Some(kind) => parse_error!(
                        format!(
                            "property used in atom has incompatible kind: expected object or data property, found {:?}",
                            kind
                        ),
                        span
                    ),
                }
            }
            Rule::SWRLObjectPropertyAtom => {
                let mut pairs = inner.into_inner();
                let op = ObjectPropertyExpression::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg1 = IArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg2 = DArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                Ok(Success(Atom::ObjectPropertyAtom {
                    pred: op,
                    args: (arg1.into(), arg2.into()),
                }))
            }
            Rule::SWRLDataPropertyAtom => {
                let mut pairs = inner.into_inner();
                let dp = DataProperty::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg1 = IArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg2 = DArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                Ok(Success(Atom::DataPropertyAtom {
                    pred: dp,
                    args: (arg1.into(), arg2.into()),
                }))
            }
            Rule::SWRLSameIndividualAtom => {
                let mut pairs = inner.into_inner();
                let arg1 = IArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg2 = IArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                Ok(Success(Atom::SameIndividualAtom(arg1.into(), arg2.into())))
            }
            Rule::SWRLDifferentIndividualsAtom => {
                let mut pairs = inner.into_inner();
                let arg1 = IArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                let arg2 = IArgument::from_pair(next_or_err!(pairs)?, ctx)?;
                Ok(Success(Atom::DifferentIndividualsAtom(
                    arg1.into(),
                    arg2.into(),
                )))
            }
            // Built-in
            Rule::SWRLBuiltInAtom => {
                let mut pairs = inner.into_inner();
                let iri = IRI::from_pair(descend(next_or_err!(pairs)?)?, ctx)?;
                let args = pairs
                    .map(|pair| DArgument::from_pair(pair, ctx))
                    .collect::<Result<Vec<_>>>()?;
                Ok(Success(Atom::BuiltInAtom {
                    pred: iri.into(),
                    args,
                }))
            }
            rule => unexpected_rule!(Atom, rule),
        }
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for Variable<A> {
    const RULE: Rule = Rule::SWRLVariable;

    fn from_pair_unchecked(pair: Pair<'a, Rule>, context: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        let iri = IRI::from_pair(inner, context)?;
        Ok(Variable(iri))
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for IArgument<A> {
    const RULE: Rule = Rule::SWRLIObject;

    fn from_pair_unchecked(pair: Pair<'a, Rule>, context: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::SWRLVariable => Variable::from_pair(inner, context).map(IArgument::Variable),
            Rule::Individual => Individual::from_pair(inner, context).map(IArgument::Individual),
            rule => unexpected_rule!(IArgument, rule),
        }
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for DArgument<A> {
    const RULE: Rule = Rule::SWRLDObject;

    fn from_pair_unchecked(pair: Pair<'a, Rule>, context: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::SWRLVariable => Variable::from_pair(inner, context).map(DArgument::Variable),
            Rule::Literal => Literal::from_pair(inner, context).map(DArgument::Literal),
            rule => unexpected_rule!(DArgument, rule),
        }
    }
}
// ---------------------------------------------------------------------------

impl<'a, A: ForIRI> FromPair<'a, A> for ParseResult<'a, PropertyExpression<A>> {
    const RULE: Rule = Rule::PropertyExpression;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::ObjectPropertyExpression => ObjectPropertyExpression::from_pair(inner, ctx)
                .map(PropertyExpression::ObjectPropertyExpression)
                .map(Success),
            Rule::DataPropertyExpression => {
                let pair = descend(inner)?;
                let span = pair.as_span();
                let dp = DataProperty::from_pair(pair, ctx)?;
                match ctx.get_property_kind(&dp.0) {
                    Some(NamedEntityKind::DataProperty) => Ok(Success(dp.into())),
                    Some(NamedEntityKind::ObjectProperty) => Ok(Success(ObjectProperty::from(dp.0).into())),
                    Some(kind) => parse_error!(
                        format!(
                            "property used in data property expression has incompatible kind: expected data property, found {:?}",
                            kind
                        ),
                        span
                    ),
                    None => Ok(Ambiguous(
                        PropertyExpression::DataProperty(dp),
                        span,
                    )),
                }
            }
            rule => unexpected_rule!(PropertyExpression, rule),
        }
    }
}

macro_rules! impl_from_pair_for_parse_result {
    ($typ:ident, $rule:expr) => {
        impl<'a, A: ForIRI> FromPair<'a, A> for ParseResult<'a, $typ<A>> {
            const RULE: Rule = $rule;
            fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
                $typ::from_pair(pair, ctx).map(Success)
            }
        }
    };
    ($typ:ident) => {
        impl_from_pair_for_parse_result!($typ, Rule::$typ);
    };
}

impl<'a, A: ForIRI> FromPair<'a, A> for ObjectPropertyExpression<A> {
    const RULE: Rule = Rule::ObjectPropertyExpression;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        match inner.as_rule() {
            Rule::ObjectPropertyIRI => {
                FromPair::from_pair(inner, ctx).map(ObjectPropertyExpression::ObjectProperty)
            }
            Rule::InverseObjectProperty => {
                let pair = descend(inner)?;
                let op = match pair.as_rule() {
                    Rule::BracketedObjectPropertyIRI => {
                        ObjectProperty::from_pair(descend(pair)?, ctx)?
                    }
                    Rule::ObjectPropertyIRI => ObjectProperty::from_pair(pair, ctx)?,
                    rule => unexpected_rule!(ObjectPropertyExpression, rule)?,
                };
                Ok(ObjectPropertyExpression::InverseObjectProperty(op))
            }
            rule => unexpected_rule!(ObjectPropertyExpression, rule),
        }
    }
}

impl_from_pair_for_parse_result!(ObjectPropertyExpression);
impl_from_pair_for_parse_result!(Individual);
impl_from_pair_for_parse_result!(DataProperty, Rule::DataPropertyIRI);

// ---------------------------------------------------------------------------

fn expand_iri<'a, A: ForIRI>(
    curie: &Curie,
    ctx: &mut Context<'a, A>,
    span: Span<'_>,
) -> Result<IRI<A>> {
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

impl<'a, A: ForIRI> FromPair<'a, A> for IRI<A> {
    const RULE: Rule = Rule::IRI;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, ctx: &mut Context<'a, A>) -> Result<Self> {
        let inner = descend(pair)?;
        let span = inner.as_span();
        match inner.as_rule() {
            Rule::SimpleIRI => {
                let local = descend(inner)?;
                let curie = Curie::new(None, local.as_str());
                expand_iri(&curie, ctx, span)
            }
            Rule::AbbreviatedIRI => {
                let mut pname = descend(inner)?.into_inner();
                let prefix = pname.next().unwrap().into_inner().next();
                let local = pname.next().unwrap();
                let curie = Curie::new(prefix.map(|p| p.as_str()), local.as_str());
                expand_iri(&curie, ctx, span)
            }
            Rule::FullIRI => {
                let iri = descend(inner)?;
                Ok(ctx.build.iri(iri.as_str()))
            }
            rule => unexpected_rule!(IRI, rule),
        }
    }
}

// ---------------------------------------------------------------------------

impl<'a, A: ForIRI> FromPair<'a, A> for String {
    const RULE: Rule = Rule::QuotedString;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, _ctx: &mut Context<'a, A>) -> Result<Self> {
        let l = pair.as_str().len();
        let s = &pair.as_str()[1..l - 1];
        if s.contains(r"\\") || s.contains(r#"\""#) {
            Ok(s.replace(r"\\", r"\").replace(r#"\""#, r#"""#))
        } else {
            Ok(s.to_string())
        }
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for u32 {
    const RULE: Rule = Rule::NonNegativeInteger;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, _ctx: &mut Context<'a, A>) -> Result<Self> {
        Ok(Self::from_str(pair.as_str()).expect("cannot fail with the right rule"))
    }
}

impl<'a, A: ForIRI> FromPair<'a, A> for NonZeroU32 {
    const RULE: Rule = Rule::PositiveInteger;
    fn from_pair_unchecked(pair: Pair<'a, Rule>, _ctx: &mut Context<'a, A>) -> Result<Self> {
        let n = u32::from_str(pair.as_str()).expect("cannot fail with the right rule");
        Ok(Self::new(n).expect("cannot be zero with the right rule"))
    }
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {

    use std::{collections::HashSet, io::Cursor, rc::Rc};

    use enum_meta::Meta;
    use test_generator::test_resources;

    use super::*;
    use crate::{
        io::omn::reader::lexer::OwlManchesterLexer, ontology::set::SetOntology, vocab::Namespace,
    };

    impl<'a> FromPair<'a, String> for SetOntology<String> {
        const RULE: Rule = Rule::Ontology;

        fn from_pair_unchecked(
            pair: Pair<'a, Rule>,
            context: &mut Context<'a, String>,
        ) -> Result<Self> {
            MutableOntologyWrapper::<String, SetOntology<String>>::from_pair(pair, context)
                .map(|wrapper| wrapper.0)
        }
    }

    macro_rules! assert_parse_into {
        ($ty:ty, $rule:path, $build:ident, $prefixes:ident, $doc:expr, $expected:expr) => {{
            assert_parse_into!(
                $ty,
                $rule,
                &mut Context::new(&$build, $prefixes.clone()),
                $doc,
                $expected
            )
        }};
        ($ty:ty, $rule:path, $ctx:expr, $doc:expr, $expected:expr) => {{
            let doc = $doc.trim();
            match OwlManchesterLexer::lex($rule, doc) {
                Ok(mut pairs) => {
                    let res = <$ty as FromPair<_>>::from_pair(pairs.next().unwrap(), $ctx).unwrap();
                    pretty_assertions::assert_eq!(res, $expected);
                }
                Err(e) => panic!(
                    "parsing using {:?}:\n{}\nfailed with: {}",
                    $rule,
                    doc.trim(),
                    e
                ),
            }
        }};
    }

    #[test]
    fn annotation() {
        let build = Build::new();
        let mut prefixes = PrefixMapping::default();
        prefixes.set_default("http://example.com/owl/families#");
        prefixes
            .add_prefix("owl", "http://www.w3.org/2002/07/owl#")
            .unwrap();
        prefixes
            .add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();

        assert_parse_into!(
            Annotation<String>,
            Rule::Annotation,
            build,
            prefixes,
            r#"owl:deprecated true"#,
            Annotation {
                ap: build.annotation_property("http://www.w3.org/2002/07/owl#deprecated"),
                av: AnnotationValue::Literal(Literal::Datatype {
                    literal: "true".into(),
                    datatype_iri: build.iri("http://www.w3.org/2001/XMLSchema#boolean"),
                }),
            }
        );
    }

    #[test]
    fn annotation_property_frame() {
        let build = Build::new();
        let mut prefixes = PrefixMapping::default();
        prefixes.set_default("http://example.com/owl/families#");
        prefixes
            .add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
            .unwrap();

        assert_parse_into!(
            AnnotationPropertyFrame<String>,
            Rule::AnnotationPropertyFrame,
            build,
            prefixes,
            r#"
            AnnotationProperty: <http://purl.obolibrary.org/obo/IAO_0000115>
            "#,
            AnnotationPropertyFrame::with_components(
                build.annotation_property("http://purl.obolibrary.org/obo/IAO_0000115"),
                vec![
                    DeclareAnnotationProperty(
                        build.annotation_property("http://purl.obolibrary.org/obo/IAO_0000115")
                    )
                    .into()
                ]
            )
        );

        assert_parse_into!(
            AnnotationPropertyFrame<String>,
            Rule::AnnotationPropertyFrame,
            build,
            prefixes,
            r#"
            AnnotationProperty: <http://purl.obolibrary.org/obo/IAO_0000115>

                Annotations:
                    rdfs:label "definition"
            "#,
            AnnotationPropertyFrame::with_components(
                build.annotation_property("http://purl.obolibrary.org/obo/IAO_0000115"),
                vec![
                    DeclareAnnotationProperty(
                        build.annotation_property("http://purl.obolibrary.org/obo/IAO_0000115")
                    )
                    .into(),
                    AnnotationAssertion {
                        subject: AnnotationSubject::IRI(
                            build.iri("http://purl.obolibrary.org/obo/IAO_0000115")
                        ),
                        ann: Annotation {
                            ap: build
                                .annotation_property("http://www.w3.org/2000/01/rdf-schema#label"),
                            av: AnnotationValue::Literal(Literal::Simple {
                                literal: String::from("definition")
                            })
                        }
                    }
                    .into()
                ]
            )
        );
    }

    #[test]
    fn class_frame() {
        let build = Build::new();
        let mut prefixes = PrefixMapping::default();
        prefixes.set_default("http://example.com/owl/families#");
        prefixes
            .add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
            .unwrap();

        assert_parse_into!(
            ParseResult<ClassFrame<String>>,
            Rule::ClassFrame,
            build,
            prefixes,
            r#"
            Class: <http://purl.obolibrary.org/obo/APO_0000098>

                Annotations:
                    rdfs:label "utilization of carbon source"

                SubClassOf:
                    <http://purl.obolibrary.org/obo/APO_0000096>
            "#,
            Success(ClassFrame::with_components(
                ClassExpression::Class(build.class("http://purl.obolibrary.org/obo/APO_0000098")),
                vec![
                    DeclareClass(build.class("http://purl.obolibrary.org/obo/APO_0000098"),).into(),
                    AnnotationAssertion {
                        subject: AnnotationSubject::IRI(
                            build.iri("http://purl.obolibrary.org/obo/APO_0000098")
                        ),
                        ann: Annotation {
                            ap: build
                                .annotation_property("http://www.w3.org/2000/01/rdf-schema#label"),
                            av: AnnotationValue::Literal(Literal::Simple {
                                literal: String::from("utilization of carbon source")
                            })
                        }
                    }
                    .into(),
                    SubClassOf {
                        sub: ClassExpression::Class(
                            build.class("http://purl.obolibrary.org/obo/APO_0000098")
                        ),
                        sup: ClassExpression::Class(
                            build.class("http://purl.obolibrary.org/obo/APO_0000096"),
                        )
                    }
                    .into(),
                ]
            ))
        );

        assert_parse_into!(
            ParseResult<ClassFrame<String>>,
            Rule::ClassFrame,
            build,
            prefixes,
            r#"
            Class: <http://purl.obolibrary.org/obo/BFO_0000002>

            DisjointWith:
                <http://purl.obolibrary.org/obo/BFO_0000003>
            "#,
            Success(ClassFrame::with_components(
                ClassExpression::Class(build.class("http://purl.obolibrary.org/obo/BFO_0000002")),
                vec![
                    DeclareClass(build.class("http://purl.obolibrary.org/obo/BFO_0000002"),).into(),
                    DisjointClasses(vec![
                        ClassExpression::Class(
                            build.class("http://purl.obolibrary.org/obo/BFO_0000002")
                        ),
                        ClassExpression::Class(
                            build.class("http://purl.obolibrary.org/obo/BFO_0000003")
                        ),
                    ])
                    .into(),
                ]
            ))
        );
    }

    #[test]
    fn iri() {
        let build = Build::new();
        let mut prefixes = PrefixMapping::default();
        prefixes.set_default("http://example.com/owl/families#");

        assert_parse_into!(
            IRI<String>,
            Rule::IRI,
            build,
            prefixes,
            r#"<http://example.com/owl/families>"#,
            build.iri("http://example.com/owl/families")
        );
        assert_parse_into!(
            IRI<String>,
            Rule::IRI,
            build,
            prefixes,
            r#"John"#,
            build.iri("http://example.com/owl/families#John")
        );
        assert_parse_into!(
            IRI<String>,
            Rule::IRI,
            build,
            prefixes,
            r#"<http://purl.obolibrary.org/obo/ms.owl>"#,
            build.iri("http://purl.obolibrary.org/obo/ms.owl")
        );
    }

    #[test]
    fn object_property_expression() {
        let build = Build::new();
        let mut prefixes = PrefixMapping::default();
        prefixes.set_default("http://example.com/owl/families#");
        prefixes
            .add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
            .unwrap();

        assert_parse_into!(
            ObjectPropertyExpression<String>,
            Rule::ObjectPropertyExpression,
            build,
            prefixes,
            r#"inverse hasSpouse"#,
            ObjectPropertyExpression::InverseObjectProperty(
                build.object_property("http://example.com/owl/families#hasSpouse")
            )
        );
    }

    #[test]
    fn object_property_frame() {
        let build = Build::new();
        let mut prefixes = PrefixMapping::default();
        prefixes
            .add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
            .unwrap();

        assert_parse_into!(
            ObjectPropertyFrame<String>,
            Rule::ObjectPropertyFrame,
            build,
            prefixes,
            r#"
            ObjectProperty: <http://purl.obolibrary.org/obo/RO_0000052>

            Annotations:
                <http://www.geneontology.org/formats/oboInOwl#hasDbXref> "RO:0000052",
                rdfs:label "inheres in"

            "#,
            ObjectPropertyFrame::with_components(
                build
                    .object_property("http://purl.obolibrary.org/obo/RO_0000052")
                    .into(),
                vec![
                    DeclareObjectProperty(
                        build.object_property("http://purl.obolibrary.org/obo/RO_0000052")
                    )
                    .into(),
                    AnnotationAssertion {
                        subject: AnnotationSubject::IRI(
                            build.iri("http://purl.obolibrary.org/obo/RO_0000052")
                        ),
                        ann: Annotation {
                            ap: build.annotation_property(
                                "http://www.geneontology.org/formats/oboInOwl#hasDbXref"
                            ),
                            av: AnnotationValue::Literal(Literal::Simple {
                                literal: String::from("RO:0000052")
                            })
                        }
                    }
                    .into(),
                    AnnotationAssertion {
                        subject: AnnotationSubject::IRI(
                            build.iri("http://purl.obolibrary.org/obo/RO_0000052")
                        ),
                        ann: Annotation {
                            ap: build
                                .annotation_property("http://www.w3.org/2000/01/rdf-schema#label"),
                            av: AnnotationValue::Literal(Literal::Simple {
                                literal: String::from("inheres in")
                            })
                        }
                    }
                    .into(),
                ]
            )
        );
    }

    #[test]
    fn ontology() {
        let build = Build::new();
        let mut prefixes = PrefixMapping::default();
        prefixes.set_default("http://www.example.com/owl/families#");
        prefixes
            .add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
            .unwrap();

        assert_parse_into!(
            SetOntology<String>,
            Rule::Ontology,
            build,
            prefixes,
            r#"
            Ontology:
            "#,
            SetOntology::<String>::new()
        );

        let mut ont = SetOntology::new();
        let id = OntologyID::new(
            Some(build.iri("http://purl.obolibrary.org/obo/ms.owl")),
            Some(build.iri("http://purl.obolibrary.org/obo/ms/4.1.29/ms.owl")),
        );
        ont.insert(id);
        assert_parse_into!(
            SetOntology<String>,
            Rule::Ontology,
            build,
            prefixes,
            r#"Ontology: <http://purl.obolibrary.org/obo/ms.owl>
                <http://purl.obolibrary.org/obo/ms/4.1.29/ms.owl>
            "#,
            ont
        );

        let mut ont = SetOntology::new();
        ont.insert(AnnotatedComponent {
            ann: BTreeSet::from_iter(vec![Annotation {
                ap: build.annotation_property("http://www.example.com/owl/families#creator"),
                av: AnnotationValue::IRI(build.iri("http://www.example.com/owl/families#John")),
            }]),
            component: Component::OntologyAnnotation(OntologyAnnotation(Annotation {
                ap: build.annotation_property(
                    "http://www.geneontology.org/formats/oboInOwl#hasOBOFormatVersion",
                ),
                av: AnnotationValue::Literal(Literal::Simple {
                    literal: String::from("1.2"),
                }),
            })),
        });
        ont.insert(AnnotatedComponent {
            ann: BTreeSet::new(),
            component: Component::OntologyAnnotation(OntologyAnnotation(Annotation {
                ap: build
                    .annotation_property("http://www.geneontology.org/formats/oboInOwl#saved-by"),
                av: AnnotationValue::Literal(Literal::Simple {
                    literal: String::from("cooperl"),
                }),
            })),
        });
        assert_parse_into!(
            SetOntology<String>,
            Rule::Ontology,
            build,
            prefixes,
            r#"Ontology:

            Annotations:
                Annotations:
                    creator John
                <http://www.geneontology.org/formats/oboInOwl#hasOBOFormatVersion> "1.2",
                <http://www.geneontology.org/formats/oboInOwl#saved-by> "cooperl"
            "#,
            ont
        );
    }

    #[test]
    fn quoted_string() {
        let build = Build::new_string();
        let prefixes = PrefixMapping::default();

        assert_parse_into!(
            String,
            Rule::QuotedString,
            build,
            prefixes,
            r#""\"Hello, there\", he said""#,
            String::from(r#""Hello, there", he said"#)
        );
    }

    #[test]
    fn ambiguous_component() {
        let build = Build::new();
        let prefixes = PrefixMapping::default();

        let input = r#"
            Class: <http://example.com/ontology/classB>

            SubClassOf:
                <http://example.com/ontology/propA> some <http://example.com/ontology/classA>
            "#
        .trim();

        let mut ctx = Context::new(&build, prefixes);

        assert_parse_into!(
            ParseResult<ClassFrame<String>>,
            Rule::ClassFrame,
            &mut ctx,
            input,
            Success(ClassFrame::with_components(
                ClassExpression::Class(build.class("http://example.com/ontology/classB")),
                vec![DeclareClass(build.class("http://example.com/ontology/classB")).into()]
            ))
        );

        let expected: HashSet<(AnnotatedComponent<_>, _)> = HashSet::from_iter(vec![(
            Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(build.class("http://example.com/ontology/classB")),
                sup: ClassExpression::DataSomeValuesFrom {
                    dp: build
                        .data_property("http://example.com/ontology/propA")
                        .into(),
                    dr: build.datatype("http://example.com/ontology/classA").into(),
                },
            })
            .into(),
            Span::new(input, 85, 162).unwrap(),
        )]);

        assert_eq!(ctx.ambiguous_components, expected);
    }

    #[test]
    fn explicit_property_type() {
        let build = Build::<String>::new();
        let mut prefixes = PrefixMapping::default();
        prefixes.set_default("http://example.com/ontology/");

        let mut ctx = Context::new(&build, prefixes);
        ctx.record_entity_kind(
            build.iri("http://example.com/ontology/propA"),
            NamedEntityKind::ObjectProperty,
        );
        ctx.record_entity_kind(
            build.iri("http://example.com/ontology/propB"),
            NamedEntityKind::ObjectProperty,
        );

        let input = r#"
        Ontology:
            Class: classB

            SubClassOf:
                propA some classA
            "#
        .trim();

        let expected = SetOntology::from_iter(vec![
            DeclareClass(build.class("http://example.com/ontology/classB")).into(),
            SubClassOf {
                sub: ClassExpression::Class(build.class("http://example.com/ontology/classB")),
                sup: ClassExpression::ObjectSomeValuesFrom {
                    ope: build
                        .object_property("http://example.com/ontology/propA")
                        .into(),
                    bce: build.class("http://example.com/ontology/classA").into(),
                },
            }
            .into(),
        ]);

        assert_parse_into!(
            SetOntology<String>,
            Rule::Ontology,
            &mut ctx,
            input,
            expected
        );

        let expected = HashSet::new();

        assert_eq!(ctx.ambiguous_components, expected);
    }

    #[test_resources("src/ont/owl-manchester/*.omn")]
    fn from_pair_resource(resource: &str) {
        pub fn is_built_in(iri: &IRI<RcStr>) -> bool {
            Namespace::all()
                .iter()
                .any(|ns| iri.to_string().starts_with(&ns.to_string()))
        }

        let text = &slurp::read_all_to_string(resource).unwrap();
        let pair = match OwlManchesterLexer::lex(Rule::OntologyDocument, text.trim()) {
            Err(e) => panic!("parser failed: {e}"),
            Ok(mut pairs) => {
                let pair = pairs.next().unwrap();
                assert_eq!(pair.as_str(), text.trim());
                pair
            }
        };

        let build = Build::new();
        let mut ctx = Context::new(&build, PrefixMapping::default());
        let (wrapper, actual_prefixes): (MutableOntologyWrapper<_, SetOntology<Rc<str>>>, _) =
            FromPair::from_pair(pair, &mut ctx).unwrap();

        let path = resource
            .replace("owl-manchester", "owl-xml")
            .replace(".omn", ".owx");
        let owx = &slurp::read_all_to_string(path).unwrap();
        let (owx_ontology, expected_prefixes): (SetOntology<Rc<str>>, PrefixMapping) =
            crate::io::owx::reader::read(&mut Cursor::new(&owx), Default::default()).unwrap();

        // The OWL API includes built-in entities in the OMN output but not always in the OWX
        // output. Filter them from both sides for a consistent comparison.
        let filter_built_in = |c: &AnnotatedComponent<Rc<str>>| match c {
            AnnotatedComponent {
                component: Component::DeclareClass(DeclareClass(c)),
                ann: _,
            } => !is_built_in(&c.0),
            AnnotatedComponent {
                component: Component::DeclareObjectProperty(DeclareObjectProperty(p)),
                ann: _,
            } => !is_built_in(&p.0),
            AnnotatedComponent {
                component: Component::DeclareDataProperty(DeclareDataProperty(p)),
                ann: _,
            } => !is_built_in(&p.0),
            AnnotatedComponent {
                component: Component::DeclareAnnotationProperty(DeclareAnnotationProperty(p)),
                ann: _,
            } => !is_built_in(&p.0),
            AnnotatedComponent {
                component: Component::DeclareDatatype(DeclareDatatype(d)),
                ann: _,
            } => !is_built_in(&d.0),
            _ => true,
        };

        // Manchester syntax expresses symmetric n-ary axioms once per member frame, so
        // `SameIndividual(o:r o:s)` is written as both `Individual: o:r SameAs: o:s` and
        // `Individual: o:s SameAs: o:r`, yielding two axioms that differ only in member order.
        // Normalise member order and drop the resulting duplicates before comparing.
        fn normalize(c: &mut AnnotatedComponent<Rc<str>>) {
            match &mut c.component {
                Component::EquivalentClasses(EquivalentClasses(v)) => v.sort(),
                Component::DisjointClasses(DisjointClasses(v)) => v.sort(),
                Component::EquivalentObjectProperties(EquivalentObjectProperties(v)) => v.sort(),
                Component::DisjointObjectProperties(DisjointObjectProperties(v)) => v.sort(),
                Component::EquivalentDataProperties(EquivalentDataProperties(v)) => v.sort(),
                Component::DisjointDataProperties(DisjointDataProperties(v)) => v.sort(),
                Component::SameIndividual(SameIndividual(v)) => v.sort(),
                Component::DifferentIndividuals(DifferentIndividuals(v)) => v.sort(),
                Component::InverseObjectProperties(InverseObjectProperties(a, b)) => {
                    if a > b {
                        std::mem::swap(a, b);
                    }
                }
                Component::Rule(SWRLRule{ head, body }) => {
                    head.sort();
                    body.sort();
                }
                _ => {}
            }
        }

        let mut actual: Vec<AnnotatedComponent<Rc<str>>> =
            wrapper.0.into_iter().filter(filter_built_in).collect();
        actual.iter_mut().for_each(normalize);
        actual.sort();
        actual.dedup();

        let mut expected: Vec<AnnotatedComponent<Rc<str>>> =
            owx_ontology.into_iter().filter(filter_built_in).collect();
        expected.iter_mut().for_each(normalize);
        expected.sort();
        expected.dedup();

        pretty_assertions::assert_eq!(actual_prefixes, expected_prefixes);
        pretty_assertions::assert_eq!(actual, expected);
    }
}
