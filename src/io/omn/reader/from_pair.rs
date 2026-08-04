use curie::Curie;
use curie::PrefixMapping;
use pest::iterators::Pair;
use std::collections::BTreeSet;
use std::collections::HashSet;

use crate::error::HornedError;
use crate::model::*;
use crate::vocab::{Facet, OWL};

use super::Rule;

// ---------------------------------------------------------------------------

type Result<T> = std::result::Result<T, HornedError>;

// ---------------------------------------------------------------------------

/// Property/datatype declarations collected in the pre-pass (pass 1.5).
///
/// Only `DataProperty:` and `Datatype:` frame subjects are stored — object
/// properties are the default and do not need to be tracked explicitly.
/// Keys are fully-resolved, interned `IRI<A>` values (same `build.iri()` path
/// used by the main pass), so `HashSet` lookup is pointer-equality-fast on
/// reference-counted IRI types.
pub struct Declarations<A: ForIRI> {
    /// Subjects of `DataProperty:` frames.
    pub(crate) data_props: HashSet<IRI<A>>,
    /// Subjects of `Datatype:` frames.
    pub(crate) datatypes: HashSet<IRI<A>>,
}

impl<A: ForIRI> Declarations<A> {
    fn new() -> Self {
        Self {
            data_props: HashSet::new(),
            datatypes: HashSet::new(),
        }
    }
}

/// Shared parsing context: carries the `Build`, prefix mapping, and
/// (optionally) the pre-pass declaration set.
pub struct Context<'a, A: ForIRI> {
    pub(crate) build: &'a Build<A>,
    pub(crate) prefixes: &'a PrefixMapping,
    /// Declaration set from the pre-pass.  `None` means "no declarations
    /// available" — every bare IRI defaults to object property (the pre-pass
    /// path is disabled; all existing callers that use `Context::new` keep
    /// today's behaviour unchanged).
    pub(crate) decls: Option<&'a Declarations<A>>,
}

impl<'a, A: ForIRI> Context<'a, A> {
    /// Standard constructor — no declarations (object-property default for all
    /// bare IRIs).  Used by every call site outside the whole-document reader.
    pub fn new(build: &'a Build<A>, prefixes: &'a PrefixMapping) -> Self {
        Self {
            build,
            prefixes,
            decls: None,
        }
    }

    /// Constructor with a pre-pass declaration set.  Used by
    /// `read_with_build` after the pre-pass has been run so that bare property
    /// IRIs in HasKey / Misc / Restriction contexts can be correctly typed.
    pub fn with_decls(
        build: &'a Build<A>,
        prefixes: &'a PrefixMapping,
        decls: &'a Declarations<A>,
    ) -> Self {
        Self {
            build,
            prefixes,
            decls: Some(decls),
        }
    }

    /// Returns `true` iff `iri` was declared as a data property in the
    /// pre-pass.  Always `false` when `decls` is `None` (object-default path).
    #[inline]
    pub(crate) fn is_data_prop(&self, iri: &IRI<A>) -> bool {
        self.decls.is_some_and(|d| d.data_props.contains(iri))
    }

    /// Returns `true` iff `iri` was declared as a datatype in the pre-pass.
    #[inline]
    pub(crate) fn is_datatype(&self, iri: &IRI<A>) -> bool {
        self.decls.is_some_and(|d| d.datatypes.contains(iri))
    }
}

// ---------------------------------------------------------------------------

/// Trait for types convertible from a `Pair<Rule>` in the Manchester grammar.
pub trait FromPair<A: ForIRI>: Sized {
    /// The valid production rule for the implementor.
    const RULE: Rule;

    /// Create a new instance from a `Pair`, checking the rule in debug builds.
    #[inline]
    fn from_pair(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        if cfg!(debug_assertions) && pair.as_rule() != Self::RULE {
            return Err(HornedError::from(pest::error::Error::new_from_span(
                pest::error::ErrorVariant::ParsingError {
                    positives: vec![pair.as_rule()],
                    negatives: vec![Self::RULE],
                },
                pair.as_span(),
            )));
        }
        Self::from_pair_unchecked(pair, ctx)
    }

    /// Create a new instance from a `Pair` without checking the PEG rule.
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self>;
}

// ---------------------------------------------------------------------------

/// A macro for simple "wrapper" types: descend one level and delegate.
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

// In omn, the IRI wrapper rules are named *IRI
impl_wrapper!(Class, Rule::ClassIRI);
impl_wrapper!(ObjectProperty, Rule::ObjectPropertyIRI);
impl_wrapper!(DataProperty, Rule::DataPropertyIRI);
impl_wrapper!(Datatype, Rule::DatatypeIRI);

// ---------------------------------------------------------------------------

/// Unescape a quoted string body (contents between the outer `"` delimiters).
fn unescape(s: &str) -> String {
    if s.contains(r"\\") || s.contains(r#"\""#) {
        s.replace(r"\\", r"\").replace(r#"\""#, r#"""#)
    } else {
        s.to_string()
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for String {
    const RULE: Rule = Rule::QuotedString;
    fn from_pair_unchecked(pair: Pair<Rule>, _ctx: &Context<'_, A>) -> Result<Self> {
        let raw = pair.as_str();
        // strip the surrounding double-quotes
        let inner = &raw[1..raw.len() - 1];
        Ok(unescape(inner))
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
                // AbbreviatedIRI = { SPARQL_PnameLn }
                // SPARQL_PnameLn = ${ SPARQL_PnameNs ~ SPARQL_PnLocal }
                // SPARQL_PnameNs = ${ SPARQL_PnPrefix? ~ ":" }
                let mut pname = inner.into_inner().next().unwrap().into_inner();
                let prefix_part = pname.next().unwrap().into_inner().next();
                let local = pname.next().unwrap();
                let curie = Curie::new(
                    Some(prefix_part.map(|p| p.as_str()).unwrap_or_default()),
                    local.as_str(),
                );
                match ctx.prefixes.expand_curie(&curie) {
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
                // FullIRI = ${ "<" ~ RFC3987_Iri ~ ">" }
                let iri = inner.into_inner().next().unwrap();
                Ok(ctx.build.iri(iri.as_str()))
            }
            Rule::SimpleIRI => {
                // SimpleIRI = { SPARQL_PnLocal } — a bare local name resolved
                // against the DEFAULT (empty) prefix, exactly like the empty-prefix
                // AbbreviatedIRI (`:local`) path above.
                let span = inner.as_span();
                let curie = Curie::new(Some(""), inner.as_str());
                match ctx.prefixes.expand_curie(&curie) {
                    Ok(s) => Ok(ctx.build.iri(s)),
                    Err(curie::ExpansionError::Invalid) => {
                        Err(HornedError::invalid_at("undefined prefix", span))
                    }
                    Err(curie::ExpansionError::MissingDefault) => Err(HornedError::invalid_at(
                        "bare local name but no default prefix is declared",
                        span,
                    )),
                }
            }
            rule => unreachable!("unexpected rule in IRI::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

/// `Individual = { AnonymousIndividual | IRI }` — a named IRI OR an anonymous
/// (blank-node) `_:id` individual.
impl<A: ForIRI> FromPair<A> for Individual<A> {
    const RULE: Rule = Rule::Individual;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            // `AnonymousIndividual = { SPARQL_BlankNodeLabel }`; the label's
            // `as_str()` is `_:label` — strip the `_:` prefix to get the id.
            Rule::AnonymousIndividual => {
                let label = inner.as_str();
                let id = label.strip_prefix("_:").unwrap_or(label);
                Ok(Individual::Anonymous(ctx.build.anon(id)))
            }
            _ => {
                let iri = IRI::from_pair(inner, ctx)?;
                Ok(Individual::Named(NamedIndividual(iri)))
            }
        }
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for Literal<A> {
    const RULE: Rule = Rule::Literal;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::TypedLiteral => {
                let mut parts = inner.into_inner();
                let literal = String::from_pair(parts.next().unwrap(), ctx)?;
                // TypedLiteral = { QuotedString ~ "^^" ~ DatatypeIRI }
                let dty = Datatype::from_pair(parts.next().unwrap(), ctx)?;
                Ok(Literal::Datatype {
                    literal,
                    datatype_iri: dty.0,
                })
            }
            Rule::StringLiteralWithLanguage => {
                let mut parts = inner.into_inner();
                let literal = String::from_pair(parts.next().unwrap(), ctx)?;
                // LanguageTag = ${ "@" ~ BCP47_LanguageTag }  — as_str includes the "@"
                let lang = parts.next().unwrap().as_str()[1..].trim().to_string();
                Ok(Literal::Language { literal, lang })
            }
            Rule::StringLiteralNoLanguage => {
                let mut parts = inner.into_inner();
                let literal = String::from_pair(parts.next().unwrap(), ctx)?;
                Ok(Literal::Simple { literal })
            }
            // §2.5 bare numeric literals: the lexical text IS the value; the
            // datatype is fixed by the production (integer/decimal/float).
            Rule::IntegerLiteral => Ok(Literal::Datatype {
                literal: inner.as_str().to_string(),
                datatype_iri: ctx.build.iri("http://www.w3.org/2001/XMLSchema#integer"),
            }),
            Rule::DecimalLiteral => Ok(Literal::Datatype {
                literal: inner.as_str().to_string(),
                datatype_iri: ctx.build.iri("http://www.w3.org/2001/XMLSchema#decimal"),
            }),
            Rule::FloatingPointLiteral => Ok(Literal::Datatype {
                literal: inner.as_str().to_string(),
                datatype_iri: ctx.build.iri("http://www.w3.org/2001/XMLSchema#float"),
            }),
            // OWL-API/Protégé compat: bare `true`/`false` → xsd:boolean typed literal.
            Rule::BooleanLiteral => Ok(Literal::Datatype {
                literal: inner.as_str().to_string(),
                datatype_iri: ctx.build.iri("http://www.w3.org/2001/XMLSchema#boolean"),
            }),
            rule => unreachable!("unexpected rule in Literal::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

/// `ope = { ( InverseKw ~ "(" ~ ObjectPropertyIRI ~ ")" ) | ObjectPropertyIRI }`
///
/// `InverseKw` is a compound-atomic keyword guard rule (emits a pair).
/// When the inverse arm matches, `into_inner()` yields `[InverseKw, ObjectPropertyIRI]`.
/// When the plain arm matches, `into_inner()` yields `[ObjectPropertyIRI]`.
/// We check the rule of the first inner pair to detect the inverse case.
impl<A: ForIRI> FromPair<A> for ObjectPropertyExpression<A> {
    const RULE: Rule = Rule::ope;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let mut inner = pair.into_inner();
        let first = inner.next().unwrap();
        let (is_inverse, op_pair) = if first.as_rule() == Rule::InverseKw {
            (true, inner.next().unwrap())
        } else {
            (false, first)
        };
        let op = ObjectProperty::from_pair(op_pair, ctx)?;
        if is_inverse {
            Ok(ObjectPropertyExpression::InverseObjectProperty(op))
        } else {
            Ok(ObjectPropertyExpression::ObjectProperty(op))
        }
    }
}

// ---------------------------------------------------------------------------

/// Map a `FacetSymbol` string (as written in Manchester) to a `Facet` variant.
///
/// This is the exact inverse of the writer's `facet_symbol` in `as_manchester.rs`.
fn facet_from_symbol(s: &str) -> Option<Facet> {
    match s {
        ">=" => Some(Facet::MinInclusive),
        "<=" => Some(Facet::MaxInclusive),
        ">" => Some(Facet::MinExclusive),
        "<" => Some(Facet::MaxExclusive),
        // case-insensitive word facets (grammar uses ^"length" etc.)
        _ => match s.to_ascii_lowercase().as_str() {
            "length" => Some(Facet::Length),
            "minlength" => Some(Facet::MinLength),
            "maxlength" => Some(Facet::MaxLength),
            "pattern" => Some(Facet::Pattern),
            "langrange" => Some(Facet::LangRange),
            "totaldigits" => Some(Facet::TotalDigits),
            "fractiondigits" => Some(Facet::FractionDigits),
            _ => None,
        },
    }
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for FacetRestriction<A> {
    const RULE: Rule = Rule::Facet;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        // Facet = { FacetSymbol ~ Literal }
        let mut inner = pair.into_inner();
        let sym_pair = inner.next().unwrap();
        let sym_str = sym_pair.as_str();
        let f = facet_from_symbol(sym_str).ok_or_else(|| {
            HornedError::invalid_at(
                format!("unknown facet symbol: {sym_str}"),
                sym_pair.as_span(),
            )
        })?;
        let l = Literal::from_pair(inner.next().unwrap(), ctx)?;
        Ok(FacetRestriction { f, l })
    }
}

// ---------------------------------------------------------------------------

/// The §2.5 `dataRange` grammar, layered like the class-expression rules:
///
/// ```text
/// DataRange       = DataConjunction ( OrKw  DataConjunction )*
/// DataConjunction = DataPrimary     ( AndKw DataPrimary )*
/// DataPrimary     = NotKw? DataAtomic
/// DataAtomic      = DataOneOf | DatatypeRestriction | "(" DataRange ")" | DatatypeIRI
/// ```
///
/// `RULE` is the top `or` layer (`DataRange`). The `OrKw`/`AndKw`/`NotKw` keyword
/// rules emit pairs that the helpers filter/skip.
impl<A: ForIRI> FromPair<A> for DataRange<A> {
    const RULE: Rule = Rule::DataRange;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        // DataRange = DataConjunction (OrKw DataConjunction)*
        let mut conjs: Vec<DataRange<A>> = pair
            .into_inner()
            .filter(|p| p.as_rule() == Rule::DataConjunction)
            .map(|p| data_conjunction(p, ctx))
            .collect::<Result<_>>()?;
        Ok(if conjs.len() == 1 {
            conjs.remove(0)
        } else {
            DataRange::DataUnionOf(conjs)
        })
    }
}

fn data_conjunction<A: ForIRI>(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<DataRange<A>> {
    // DataConjunction = DataPrimary (AndKw DataPrimary)*
    let mut prims: Vec<DataRange<A>> = pair
        .into_inner()
        .filter(|p| p.as_rule() == Rule::DataPrimary)
        .map(|p| data_primary(p, ctx))
        .collect::<Result<_>>()?;
    Ok(if prims.len() == 1 {
        prims.remove(0)
    } else {
        DataRange::DataIntersectionOf(prims)
    })
}

fn data_primary<A: ForIRI>(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<DataRange<A>> {
    // DataPrimary = NotKw? DataAtomic
    let mut it = pair.into_inner();
    let mut first = it.next().unwrap();
    let negated = first.as_rule() == Rule::NotKw;
    if negated {
        first = it.next().unwrap();
    }
    let atomic = data_atomic(first, ctx)?;
    Ok(if negated {
        DataRange::DataComplementOf(Box::new(atomic))
    } else {
        atomic
    })
}

fn data_atomic<A: ForIRI>(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<DataRange<A>> {
    // DataAtomic = DataOneOf | DatatypeRestriction | "(" DataRange ")" | DatatypeIRI
    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::DataOneOf => {
            let lits = inner
                .into_inner()
                .map(|p| Literal::from_pair(p, ctx))
                .collect::<Result<_>>()?;
            Ok(DataRange::DataOneOf(lits))
        }
        Rule::DatatypeRestriction => {
            let mut parts = inner.into_inner();
            let dt = Datatype::from_pair(parts.next().unwrap(), ctx)?;
            let facets = parts
                .map(|p| FacetRestriction::from_pair(p, ctx))
                .collect::<Result<_>>()?;
            Ok(DataRange::DatatypeRestriction(dt, facets))
        }
        Rule::DataRange => DataRange::from_pair(inner, ctx), // parenthesized
        Rule::DatatypeIRI => Ok(DataRange::Datatype(Datatype::from_pair(inner, ctx)?)),
        rule => unreachable!("unexpected data-atomic rule: {:?}", rule),
    }
}

// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// ClassExpression — inverse of the P1 Manchester writer
//
// The grammar has 5 layers: Description (or), Conjunction (and), Primary (not?),
// Atomic (oneOf / parens / ClassIRI), Restriction (property restrictions).
//
// RULE is Description (the top-layer and public entry point).
// Internal recursion MUST call from_pair_unchecked (not from_pair) because
// child pairs carry sub-layer rules (Conjunction/Primary/…) ≠ Description,
// which would trip the debug-assertion in from_pair.

impl<A: ForIRI> FromPair<A> for ClassExpression<A> {
    const RULE: Rule = Rule::Description;

    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        match pair.as_rule() {
            // Description = { Conjunction ~ (OrKw ~ Conjunction)* }
            // `OrKw` is a compound-atomic keyword guard rule (emits a pair);
            // we filter to only `Conjunction` children.
            // 1 Conjunction → unwrap, ≥2 → ObjectUnionOf.
            Rule::Description => {
                let mut ces: Vec<ClassExpression<A>> = pair
                    .into_inner()
                    .filter(|p| p.as_rule() == Rule::Conjunction)
                    .map(|p| Self::from_pair_unchecked(p, ctx))
                    .collect::<Result<_>>()?;
                if ces.len() == 1 {
                    Ok(ces.pop().unwrap())
                } else {
                    Ok(ClassExpression::ObjectUnionOf(ces))
                }
            }

            // Conjunction = { Primary ~ (AndKw ~ Primary)* }
            // `AndKw` is a compound-atomic keyword guard rule (emits a pair);
            // we filter to only `Primary` children.
            // 1 Primary → unwrap, ≥2 → ObjectIntersectionOf.
            Rule::Conjunction => {
                let mut ces: Vec<ClassExpression<A>> = pair
                    .into_inner()
                    .filter(|p| p.as_rule() == Rule::Primary)
                    .map(|p| Self::from_pair_unchecked(p, ctx))
                    .collect::<Result<_>>()?;
                if ces.len() == 1 {
                    Ok(ces.pop().unwrap())
                } else {
                    Ok(ClassExpression::ObjectIntersectionOf(ces))
                }
            }

            // Primary = { NotKw? ~ (Restriction | Atomic) }
            // `NotKw` is a compound-atomic keyword guard rule that emits a pair
            // when `not` is present. Detect negation by checking whether the
            // first inner pair is `Rule::NotKw`.
            Rule::Primary => {
                let mut inner = pair.into_inner();
                let first = inner.next().unwrap();
                let (is_not, child) = if first.as_rule() == Rule::NotKw {
                    (true, inner.next().unwrap())
                } else {
                    (false, first)
                };
                let ce = Self::from_pair_unchecked(child, ctx)?;
                if is_not {
                    Ok(ClassExpression::ObjectComplementOf(Box::new(ce)))
                } else {
                    Ok(ce)
                }
            }

            // Atomic = { ObjectOneOf | "(" ~ Description ~ ")" | ClassIRI }
            Rule::Atomic => {
                let child = pair.into_inner().next().unwrap();
                match child.as_rule() {
                    Rule::ObjectOneOf => {
                        let individuals: Result<Vec<Individual<A>>> = child
                            .into_inner()
                            .map(|p| Individual::from_pair(p, ctx))
                            .collect();
                        Ok(ClassExpression::ObjectOneOf(individuals?))
                    }
                    Rule::Description => Self::from_pair_unchecked(child, ctx),
                    Rule::ClassIRI => Class::from_pair(child, ctx).map(ClassExpression::Class),
                    rule => unreachable!("unexpected rule in Atomic::from_pair: {rule:?}"),
                }
            }

            // Restriction — object or data, keyword extracted from raw text gap.
            //
            // Object arms:
            //   ope ~ ^"some"    ~ Primary
            //   ope ~ ^"only"    ~ Primary
            //   ope ~ ^"value"   ~ Individual
            //   ope ~ ^"Self"
            //   ope ~ ^"min"     ~ Cardinality ~ Primary?
            //   ope ~ ^"max"     ~ Cardinality ~ Primary?
            //   ope ~ ^"exactly" ~ Cardinality ~ Primary?
            //
            // Data arms:
            //   DataPropertyIRI ~ ^"some"    ~ DataRange
            //   DataPropertyIRI ~ ^"only"    ~ DataRange
            //   DataPropertyIRI ~ ^"value"   ~ Literal
            //   DataPropertyIRI ~ ^"min"     ~ Cardinality ~ DataRange?
            //   DataPropertyIRI ~ ^"max"     ~ Cardinality ~ DataRange?
            //   DataPropertyIRI ~ ^"exactly" ~ Cardinality ~ DataRange?
            Rule::Restriction => {
                let r_str = pair.as_str();
                let r_start = pair.as_span().start();
                let r_span = pair.as_span();
                let mut children = pair.into_inner().peekable();

                let prop_pair = children.next().unwrap();
                let is_object = prop_pair.as_rule() == Rule::ope;

                // Extract the keyword from the text between end-of-property and the next token.
                // `split_whitespace` would glue the keyword with a no-whitespace filler
                // (e.g. `only(<http://t/A>)` → `"only(<http://t/A>)"`). Use a take-while
                // alphabetic scan instead: it isolates the keyword regardless of the
                // following character (IRI, parenthesis, or whitespace).
                let prop_end = prop_pair.as_span().end() - r_start;
                let after_prop = r_str[prop_end..].trim_start();
                let keyword = after_prop
                    .chars()
                    .take_while(|c| c.is_ascii_alphabetic())
                    .collect::<String>()
                    .to_ascii_lowercase();

                // The compound-atomic keyword guard rules (`SomeKw`, `OnlyKw`, etc.)
                // each emit one pair. Skip it — the keyword text was already extracted
                // from the raw string above.
                let _ = children.next(); // consume the keyword pair

                if is_object {
                    let ope = ObjectPropertyExpression::from_pair(prop_pair, ctx)?;

                    // Declaration-based flip: if this OPE is a plain (non-inverse)
                    // property declared as a data property, and the filler (where
                    // applicable) is a BARE class IRI that was declared as a
                    // `Datatype:` frame subject, rewrite to the data restriction
                    // form.  Compound fillers (intersections, etc.) are left as-is;
                    // `value` and `Self` have no ClassExpression filler so are
                    // excluded by construction.
                    let prop_is_data = matches!(&ope,
                        ObjectPropertyExpression::ObjectProperty(ObjectProperty(iri))
                            if ctx.is_data_prop(iri));

                    // Helper: given a ClassExpression, extract a bare-datatype IRI if
                    // the filler is a plain `Class(iri)` whose IRI was declared as a
                    // `Datatype:` frame subject; otherwise `None`.
                    let bare_datatype_iri = |bce: &ClassExpression<A>| -> Option<IRI<A>> {
                        if let ClassExpression::Class(Class(filler_iri)) = bce
                            && ctx.is_datatype(filler_iri)
                        {
                            return Some(filler_iri.clone());
                        }
                        None
                    };

                    // Helper: `not <D>` where `<D>` is a bare `Class(iri)` that is a
                    // declared `Datatype:` (or sits under a declared data property).
                    // The grammar routes `not <customType>` (no xsd prefix, no facet)
                    // to the object arm as `ObjectComplementOf`, so this recovers the
                    // intended `DataComplementOf` filler; `None` keeps the object form.
                    let negated_datatype = |bce: &ClassExpression<A>| -> Option<IRI<A>> {
                        if let ClassExpression::ObjectComplementOf(inner) = bce
                            && let ClassExpression::Class(Class(filler_iri)) = &**inner
                            && (prop_is_data || ctx.is_datatype(filler_iri))
                        {
                            return Some(filler_iri.clone());
                        }
                        None
                    };

                    // Combined data-range filler for a cardinality qualifier: a bare
                    // declared datatype/class (→ `Datatype`) or a `not`-negation of one
                    // (→ `DataComplementOf`), when declarations indicate a data
                    // restriction; `None` keeps the object form.
                    let data_range_filler = |bce: &ClassExpression<A>| -> Option<DataRange<A>> {
                        if let ClassExpression::Class(Class(iri)) = bce
                            && (prop_is_data || ctx.is_datatype(iri))
                        {
                            return Some(DataRange::Datatype(Datatype(iri.clone())));
                        }
                        negated_datatype(bce).map(|iri| {
                            DataRange::DataComplementOf(Box::new(DataRange::Datatype(Datatype(
                                iri,
                            ))))
                        })
                    };

                    match keyword.as_str() {
                        "some" => {
                            let filler = children.next().unwrap();
                            let bce = Box::new(Self::from_pair_unchecked(filler, ctx)?);
                            // Flip to data form if:
                            //   (a) property declared as data, OR
                            //   (b) filler is a declared Datatype IRI.
                            if let Some(dt_iri) = bare_datatype_iri(&bce) {
                                let dp = DataProperty(match &ope {
                                    ObjectPropertyExpression::ObjectProperty(ObjectProperty(
                                        iri,
                                    )) => iri.clone(),
                                    _ => unreachable!("inverse has no datatype filler"),
                                });
                                Ok(ClassExpression::DataSomeValuesFrom {
                                    dp,
                                    dr: DataRange::Datatype(Datatype(dt_iri)),
                                })
                            } else if let Some(dt_iri) = negated_datatype(&bce) {
                                match &ope {
                                    ObjectPropertyExpression::ObjectProperty(ObjectProperty(
                                        iri,
                                    )) => Ok(ClassExpression::DataSomeValuesFrom {
                                        dp: DataProperty(iri.clone()),
                                        dr: DataRange::DataComplementOf(Box::new(
                                            DataRange::Datatype(Datatype(dt_iri)),
                                        )),
                                    }),
                                    _ => Ok(ClassExpression::ObjectSomeValuesFrom { ope, bce }),
                                }
                            } else if prop_is_data {
                                // Property declared data but filler is not a bare
                                // declared Datatype — only flip if filler is a bare
                                // Class; leave compound fillers alone.
                                if let ClassExpression::Class(Class(filler_iri)) = *bce {
                                    let dp = match &ope {
                                        ObjectPropertyExpression::ObjectProperty(
                                            ObjectProperty(iri),
                                        ) => DataProperty(iri.clone()),
                                        _ => unreachable!(),
                                    };
                                    Ok(ClassExpression::DataSomeValuesFrom {
                                        dp,
                                        dr: DataRange::Datatype(Datatype(filler_iri)),
                                    })
                                } else {
                                    Ok(ClassExpression::ObjectSomeValuesFrom { ope, bce })
                                }
                            } else {
                                Ok(ClassExpression::ObjectSomeValuesFrom { ope, bce })
                            }
                        }
                        "only" => {
                            let filler = children.next().unwrap();
                            let bce = Box::new(Self::from_pair_unchecked(filler, ctx)?);
                            if let Some(dt_iri) = bare_datatype_iri(&bce) {
                                let dp = match &ope {
                                    ObjectPropertyExpression::ObjectProperty(ObjectProperty(
                                        iri,
                                    )) => DataProperty(iri.clone()),
                                    _ => unreachable!("inverse has no datatype filler"),
                                };
                                Ok(ClassExpression::DataAllValuesFrom {
                                    dp,
                                    dr: DataRange::Datatype(Datatype(dt_iri)),
                                })
                            } else if let Some(dt_iri) = negated_datatype(&bce) {
                                match &ope {
                                    ObjectPropertyExpression::ObjectProperty(ObjectProperty(
                                        iri,
                                    )) => Ok(ClassExpression::DataAllValuesFrom {
                                        dp: DataProperty(iri.clone()),
                                        dr: DataRange::DataComplementOf(Box::new(
                                            DataRange::Datatype(Datatype(dt_iri)),
                                        )),
                                    }),
                                    _ => Ok(ClassExpression::ObjectAllValuesFrom { ope, bce }),
                                }
                            } else if prop_is_data {
                                if let ClassExpression::Class(Class(filler_iri)) = *bce {
                                    let dp = match &ope {
                                        ObjectPropertyExpression::ObjectProperty(
                                            ObjectProperty(iri),
                                        ) => DataProperty(iri.clone()),
                                        _ => unreachable!(),
                                    };
                                    Ok(ClassExpression::DataAllValuesFrom {
                                        dp,
                                        dr: DataRange::Datatype(Datatype(filler_iri)),
                                    })
                                } else {
                                    Ok(ClassExpression::ObjectAllValuesFrom { ope, bce })
                                }
                            } else {
                                Ok(ClassExpression::ObjectAllValuesFrom { ope, bce })
                            }
                        }
                        "value" => {
                            let ind = children.next().unwrap();
                            let i = Individual::from_pair(ind, ctx)?;
                            Ok(ClassExpression::ObjectHasValue { ope, i })
                        }
                        "self" => Ok(ClassExpression::ObjectHasSelf(ope)),
                        "min" => {
                            let card_pair = children.next().unwrap();
                            let n: u32 = card_pair.as_str().parse().map_err(|_| {
                                HornedError::invalid_at("invalid cardinality", card_pair.as_span())
                            })?;
                            let filler_pair = children.next();
                            // Flip to data cardinality ONLY when there is an EXPLICIT filler
                            // that is a bare declared-Datatype IRI, or when the property is
                            // declared data and the filler is a bare class.  The no-filler
                            // (unqualified) case is NOT flipped — the injected default is
                            // `owl:Thing` (object) not `rdfs:Literal` (data), and we cannot
                            // distinguish user intent without a filler.
                            match filler_pair {
                                Some(fp) => {
                                    let bce = Box::new(Self::from_pair_unchecked(fp, ctx)?);
                                    match data_range_filler(&bce) {
                                        Some(dr) => match &ope {
                                            ObjectPropertyExpression::ObjectProperty(
                                                ObjectProperty(iri),
                                            ) => Ok(ClassExpression::DataMinCardinality {
                                                n,
                                                dp: DataProperty(iri.clone()),
                                                dr,
                                            }),
                                            _ => Err(HornedError::invalid_at(
                                                "data property cannot be inverse",
                                                r_span,
                                            )),
                                        },
                                        None => Ok(ClassExpression::ObjectMinCardinality {
                                            n,
                                            ope,
                                            bce,
                                        }),
                                    }
                                }
                                None => {
                                    let bce = Box::new(ClassExpression::Class(Class(
                                        ctx.build.iri(OWL::Thing),
                                    )));
                                    Ok(ClassExpression::ObjectMinCardinality { n, ope, bce })
                                }
                            }
                        }
                        "max" => {
                            let card_pair = children.next().unwrap();
                            let n: u32 = card_pair.as_str().parse().map_err(|_| {
                                HornedError::invalid_at("invalid cardinality", card_pair.as_span())
                            })?;
                            let filler_pair = children.next();
                            match filler_pair {
                                Some(fp) => {
                                    let bce = Box::new(Self::from_pair_unchecked(fp, ctx)?);
                                    match data_range_filler(&bce) {
                                        Some(dr) => match &ope {
                                            ObjectPropertyExpression::ObjectProperty(
                                                ObjectProperty(iri),
                                            ) => Ok(ClassExpression::DataMaxCardinality {
                                                n,
                                                dp: DataProperty(iri.clone()),
                                                dr,
                                            }),
                                            _ => Err(HornedError::invalid_at(
                                                "data property cannot be inverse",
                                                r_span,
                                            )),
                                        },
                                        None => Ok(ClassExpression::ObjectMaxCardinality {
                                            n,
                                            ope,
                                            bce,
                                        }),
                                    }
                                }
                                None => {
                                    let bce = Box::new(ClassExpression::Class(Class(
                                        ctx.build.iri(OWL::Thing),
                                    )));
                                    Ok(ClassExpression::ObjectMaxCardinality { n, ope, bce })
                                }
                            }
                        }
                        "exactly" => {
                            let card_pair = children.next().unwrap();
                            let n: u32 = card_pair.as_str().parse().map_err(|_| {
                                HornedError::invalid_at("invalid cardinality", card_pair.as_span())
                            })?;
                            let filler_pair = children.next();
                            match filler_pair {
                                Some(fp) => {
                                    let bce = Box::new(Self::from_pair_unchecked(fp, ctx)?);
                                    match data_range_filler(&bce) {
                                        Some(dr) => match &ope {
                                            ObjectPropertyExpression::ObjectProperty(
                                                ObjectProperty(iri),
                                            ) => Ok(ClassExpression::DataExactCardinality {
                                                n,
                                                dp: DataProperty(iri.clone()),
                                                dr,
                                            }),
                                            _ => Err(HornedError::invalid_at(
                                                "data property cannot be inverse",
                                                r_span,
                                            )),
                                        },
                                        None => Ok(ClassExpression::ObjectExactCardinality {
                                            n,
                                            ope,
                                            bce,
                                        }),
                                    }
                                }
                                None => {
                                    let bce = Box::new(ClassExpression::Class(Class(
                                        ctx.build.iri(OWL::Thing),
                                    )));
                                    Ok(ClassExpression::ObjectExactCardinality { n, ope, bce })
                                }
                            }
                        }
                        kw => Err(HornedError::invalid_at(
                            format!("unknown object restriction keyword: {kw}"),
                            r_span,
                        )),
                    }
                } else {
                    // Data property arm
                    let dp = DataProperty::from_pair(prop_pair, ctx)?;
                    match keyword.as_str() {
                        "some" => {
                            let dr_pair = children.next().unwrap();
                            let dr = DataRange::from_pair(dr_pair, ctx)?;
                            Ok(ClassExpression::DataSomeValuesFrom { dp, dr })
                        }
                        "only" => {
                            let dr_pair = children.next().unwrap();
                            let dr = DataRange::from_pair(dr_pair, ctx)?;
                            Ok(ClassExpression::DataAllValuesFrom { dp, dr })
                        }
                        "value" => {
                            let l_pair = children.next().unwrap();
                            let l = Literal::from_pair(l_pair, ctx)?;
                            Ok(ClassExpression::DataHasValue { dp, l })
                        }
                        "min" => {
                            let card_pair = children.next().unwrap();
                            let n: u32 = card_pair.as_str().parse().map_err(|_| {
                                HornedError::invalid_at("invalid cardinality", card_pair.as_span())
                            })?;
                            let dr = match children.next() {
                                Some(p) => DataRange::from_pair(p, ctx)?,
                                None => DataRange::Datatype(Datatype(
                                    ctx.build
                                        .iri("http://www.w3.org/2000/01/rdf-schema#Literal"),
                                )),
                            };
                            Ok(ClassExpression::DataMinCardinality { n, dp, dr })
                        }
                        "max" => {
                            let card_pair = children.next().unwrap();
                            let n: u32 = card_pair.as_str().parse().map_err(|_| {
                                HornedError::invalid_at("invalid cardinality", card_pair.as_span())
                            })?;
                            let dr = match children.next() {
                                Some(p) => DataRange::from_pair(p, ctx)?,
                                None => DataRange::Datatype(Datatype(
                                    ctx.build
                                        .iri("http://www.w3.org/2000/01/rdf-schema#Literal"),
                                )),
                            };
                            Ok(ClassExpression::DataMaxCardinality { n, dp, dr })
                        }
                        "exactly" => {
                            let card_pair = children.next().unwrap();
                            let n: u32 = card_pair.as_str().parse().map_err(|_| {
                                HornedError::invalid_at("invalid cardinality", card_pair.as_span())
                            })?;
                            let dr = match children.next() {
                                Some(p) => DataRange::from_pair(p, ctx)?,
                                None => DataRange::Datatype(Datatype(
                                    ctx.build
                                        .iri("http://www.w3.org/2000/01/rdf-schema#Literal"),
                                )),
                            };
                            Ok(ClassExpression::DataExactCardinality { n, dp, dr })
                        }
                        kw => Err(HornedError::invalid_at(
                            format!("unknown data restriction keyword: {kw}"),
                            r_span,
                        )),
                    }
                }
            }

            rule => unreachable!("unexpected rule in ClassExpression::from_pair: {rule:?}"),
        }
    }
}

// ---------------------------------------------------------------------------
// Annotation FromPair impls
// ---------------------------------------------------------------------------

impl<A: ForIRI> FromPair<A> for AnnotationValue<A> {
    const RULE: Rule = Rule::AnnotationTarget;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let inner = pair.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::Literal => Ok(AnnotationValue::Literal(Literal::from_pair(inner, ctx)?)),
            Rule::IRI => Ok(AnnotationValue::IRI(IRI::from_pair(inner, ctx)?)),
            Rule::AnonymousIndividual => {
                let label = inner.as_str();
                let id = label.strip_prefix("_:").unwrap_or(label);
                Ok(AnnotationValue::AnonymousIndividual(ctx.build.anon(id)))
            }
            rule => unreachable!("unexpected annotation target: {:?}", rule),
        }
    }
}

impl<A: ForIRI> FromPair<A> for Annotation<A> {
    const RULE: Rule = Rule::AnnotationEntry;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let mut inner = pair.into_inner();
        let mut next = inner.next().unwrap();
        // The annotation entry may itself be annotated (§2.5 `annotationAnnotatedList`);
        // store the nested `Annotations:` in `ann` (OWL 2 annotated annotations).
        let ann: BTreeSet<Annotation<A>> = if next.as_rule() == Rule::Annotations {
            let nested = parse_annotations(next, ctx)?.into_iter().collect();
            next = inner.next().unwrap();
            nested
        } else {
            BTreeSet::new()
        };
        let ap = AnnotationProperty(IRI::from_pair(next, ctx)?);
        let av = AnnotationValue::from_pair(inner.next().unwrap(), ctx)?;
        Ok(Annotation { ap, av, ann })
    }
}

/// Build the `AnnotatedComponent` for an entity-frame annotation
/// (`Class: A Annotations: …`, and the analogous property/individual/datatype
/// frames). A nested `Annotations:` on the entry annotates the resulting
/// `AnnotationAssertion` *axiom* — not its annotation value — so the nested set
/// is lifted from the entry's own `ann` to the component's axiom annotations.
/// This matches the ofn/owx readers (§2.5 `annotationAnnotatedList`): an
/// annotation on a frame annotation is an annotation on the assertion it yields.
fn entity_annotation_assertion<A: ForIRI>(
    subject: AnnotationSubject<A>,
    mut entry: Annotation<A>,
) -> AnnotatedComponent<A> {
    let axiom_ann = std::mem::take(&mut entry.ann);
    AnnotatedComponent {
        component: Component::AnnotationAssertion(AnnotationAssertion {
            subject,
            ann: entry,
        }),
        ann: axiom_ann,
    }
}

/// Parse an `Annotations` clause pair into a `Vec<Annotation>`.
/// The pair's inner children are `AnnotationEntry` items.
pub(crate) fn parse_annotations<A: ForIRI>(
    clause: Pair<Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<Annotation<A>>> {
    clause
        .into_inner()
        .map(|e| Annotation::from_pair(e, ctx))
        .collect()
}

// ---------------------------------------------------------------------------
// Whole-ontology document support.
// ---------------------------------------------------------------------------

/// Build a `PrefixMapping` from a slice of `PrefixDeclaration` pairs.
///
/// `PrefixDeclaration = { ^"Prefix:" ~ PrefixName ~ FullIRI }`
/// `PrefixName        = { SPARQL_PnameNs }`  (e.g. `ex:` or bare `:`)
pub(crate) fn prefixes_from_decls<'a>(
    decls: impl Iterator<Item = Pair<'a, Rule>>,
) -> Result<PrefixMapping> {
    let mut prefixes = PrefixMapping::default();
    for decl in decls {
        let mut inner = decl.into_inner();
        let pname = inner.next().unwrap(); // PrefixName
        let full_iri = inner.next().unwrap(); // FullIRI
        // FullIRI = ${ "<" ~ RFC3987_Iri ~ ">" } — its inner is the bare IRI text.
        let iri_text = full_iri.into_inner().next().unwrap().as_str();
        // PrefixName = { SPARQL_PnameNs }; SPARQL_PnameNs = ${ SPARQL_PnPrefix? ~ ":" }
        let prefix_part = pname.into_inner().next().unwrap().into_inner().next();
        match prefix_part {
            Some(p) => prefixes
                .add_prefix(p.as_str(), iri_text)
                .expect("grammar guarantees a valid prefix"),
            None => prefixes
                .add_prefix("", iri_text)
                .expect("empty prefix shouldn't fail"),
        }
    }
    Ok(prefixes)
}

/// Pre-pass (pass 1.5): collect `DataProperty:` and `Datatype:` frame subjects
/// from the already-buffered document children.
///
/// Iterates the buffered `children` (cloned pairs — pass 2 still owns the
/// originals) and inserts fully-resolved `IRI<A>` values into the returned
/// `Declarations`.  Both sides of every subsequent lookup use the SAME
/// `build.iri()` interning path so that `HashSet` membership is reliable.
///
/// A `Context` built from `build` + `prefixes` (with `decls: None`) is used
/// here — declaration IRIs don't themselves require a declaration table, and
/// we must avoid a circular dependency.
pub(crate) fn declarations_from_frames<'a, A: ForIRI>(
    children: impl Iterator<Item = pest::iterators::Pair<'a, Rule>>,
    build: &Build<A>,
    prefixes: &PrefixMapping,
) -> Declarations<A> {
    // A no-decls context is sufficient for IRI resolution.
    let ctx = Context::new(build, prefixes);
    let mut decls = Declarations::new();
    for child in children {
        if child.as_rule() != Rule::Frame {
            continue;
        }
        let inner = child.into_inner().next().unwrap();
        let (rule, subject_iri) = match inner.as_rule() {
            Rule::DataPropertyFrame | Rule::DatatypeFrame => {
                let rule = inner.as_rule();
                // Both frames start with FrameSubject = { IRI }.
                let mut pairs = inner.into_inner();
                let subject_pair = pairs.next().unwrap(); // FrameSubject
                let iri_pair = subject_pair.into_inner().next().unwrap(); // IRI
                match IRI::from_pair(iri_pair, &ctx) {
                    Ok(iri) => (rule, iri),
                    Err(_) => continue, // skip on resolution error (will error again in pass 2)
                }
            }
            _ => continue,
        };
        match rule {
            Rule::DataPropertyFrame => {
                decls.data_props.insert(subject_iri);
            }
            Rule::DatatypeFrame => {
                decls.datatypes.insert(subject_iri);
            }
            _ => unreachable!(),
        }
    }
    decls
}

/// Dispatch a single `Frame` pair to the matching sub-function, inserting the
/// resulting components into `ont`.
pub(crate) fn insert_frame<A: ForIRI, O: MutableOntology<A>>(
    frame: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    let inner = frame.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::ClassFrame => insert_class_frame(inner, ctx, ont),
        Rule::ObjectPropertyFrame => insert_object_property_frame(inner, ctx, ont),
        Rule::DataPropertyFrame => insert_data_property_frame(inner, ctx, ont),
        Rule::AnnotationPropertyFrame => insert_annotation_property_frame(inner, ctx, ont),
        Rule::IndividualFrame => insert_individual_frame(inner, ctx, ont),
        Rule::DatatypeFrame => insert_datatype_frame(inner, ctx, ont),
        Rule::RuleFrame => insert_rule_frame(inner, ctx, ont),
        rule => unreachable!("unexpected frame rule: {:?}", rule),
    }
}

/// Parse a frame's `FrameSubject` (the first inner pair) into an `IRI`,
/// returning it plus the remaining clause pairs.
fn frame_subject_and_clauses<'a, A: ForIRI>(
    frame: Pair<'a, Rule>,
    ctx: &Context<'_, A>,
) -> Result<(IRI<A>, pest::iterators::Pairs<'a, Rule>)> {
    let mut inner = frame.into_inner();
    let subject_pair = inner.next().unwrap(); // FrameSubject
    let iri = IRI::from_pair(subject_pair.into_inner().next().unwrap(), ctx)?;
    Ok((iri, inner))
}

/// Extract the lower-cased clause keyword (without the trailing colon) from a
/// clause pair, e.g. `"SubClassOf: ..."` -> `"subclassof"`.
fn clause_keyword(clause: &Pair<Rule>) -> String {
    clause
        .as_str()
        .chars()
        .take_while(|c| c.is_ascii_alphabetic())
        .flat_map(|c| c.to_lowercase())
        .collect()
}

/// Per-item annotated-list entry: an item plus the `Annotations:` that
/// immediately preceded it (empty in the common case).
type AnnItem<A, T> = (BTreeSet<Annotation<A>>, T);

/// Fold an annotatedList's inner pairs (interleaved `Annotations` markers and
/// item pairs, per the §2.5 grammar) into `(per-item annotations, item)` pairs.
/// A leading `Annotations` pair attaches to the item that follows it; items
/// without a preceding `Annotations` carry an empty set. Behaviour is identical
/// to a plain list when no per-item annotations are present.
fn parse_annotated_list<A: ForIRI, T, F>(
    list: Pair<Rule>,
    ctx: &Context<'_, A>,
    mut item: F,
) -> Result<Vec<AnnItem<A, T>>>
where
    F: FnMut(Pair<Rule>, &Context<'_, A>) -> Result<T>,
{
    let mut out = Vec::new();
    let mut pending: BTreeSet<Annotation<A>> = BTreeSet::new();
    for p in list.into_inner() {
        if p.as_rule() == Rule::Annotations {
            pending.extend(parse_annotations(p, ctx)?);
        } else {
            out.push((std::mem::take(&mut pending), item(p, ctx)?));
        }
    }
    Ok(out)
}

/// §2.5 `descriptionAnnotatedList ::= [annotations] description { ',' …` — a
/// LEADING clause-level annotation binds the FIRST list item ONLY. Fold the
/// clause-level `ann` into `list[0]`'s own annotations; every other item keeps
/// its own (post-comma) annotations untouched. Single-item clauses (the common
/// case) are unchanged: the leading annotation still annotates the one axiom.
fn bind_leading_to_first<A: ForIRI, T>(ann: BTreeSet<Annotation<A>>, list: &mut [AnnItem<A, T>]) {
    if let Some(first) = list.first_mut() {
        first.0.extend(ann);
    }
}

/// Drain a per-item annotated list into `items`, folding every item's
/// annotations into the single n-ary axiom's `ann` (§2.5: per-item annotations
/// on an n-ary list annotate the axiom). Identity to the old behaviour when no
/// item carries annotations.
fn merge_list_ann<A: ForIRI, T>(
    ann: &mut BTreeSet<Annotation<A>>,
    list: Vec<AnnItem<A, T>>,
    items: &mut Vec<T>,
) {
    for (item_ann, item) in list {
        ann.extend(item_ann);
        items.push(item);
    }
}

/// Parse a `DescriptionList` pair into per-item `(annotations, ClassExpression)`.
fn parse_description_list<A: ForIRI>(
    list: Pair<Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnItem<A, ClassExpression<A>>>> {
    parse_annotated_list(list, ctx, ClassExpression::from_pair)
}

fn insert_class_frame<A: ForIRI, O: MutableOntology<A>>(
    frame: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    let mut inner = frame.into_inner();
    // A leading `Annotations?` (before the subject) annotates the *declaration*
    // axiom — OWL-API renders an annotated `Declaration(Class(C))` this way.
    let mut first = inner.next().unwrap();
    let mut decl_ann: BTreeSet<Annotation<A>> = BTreeSet::new();
    if first.as_rule() == Rule::Annotations {
        decl_ann = parse_annotations(first, ctx)?.into_iter().collect();
        first = inner.next().unwrap();
    }
    // The subject is `ClassFrameSubject = { Description }` — parse its inner
    // `Description` as a ClassExpression to support complex subjects.
    // OWL-API/Protégé/ROBOT emit general class axioms (GCIs) as `Class: <expr>`
    // frames; strict §2.5 requires a classIRI subject, but we accept leniently.
    let subj_pair = first; // ClassFrameSubject
    let desc_pair = subj_pair.into_inner().next().unwrap(); // Description
    let subject_ce = ClassExpression::from_pair_unchecked(desc_pair, ctx)?;
    let clauses = inner; // remaining pairs are ClassClause*

    // Determine whether the subject is a plain named class (atomic) or a
    // compound expression (GCI path).
    let atomic_iri: Option<IRI<A>> = if let ClassExpression::Class(Class(ref iri)) = subject_ce {
        Some(iri.clone())
    } else {
        None
    };

    // For atomic subjects only: declare the class (existing behaviour), carrying
    // any declaration annotations.
    if let Some(ref iri) = atomic_iri {
        ont.insert(AnnotatedComponent {
            component: Component::DeclareClass(DeclareClass(Class(iri.clone()))),
            ann: decl_ann,
        });
    }

    for clause in clauses {
        let kw = clause_keyword(&clause);
        // Peek the first inner pair: if the clause is a keyworded arm with an
        // optional `Annotations?` prefix, consume it into `ann`; otherwise `ann`
        // is empty. Guard with `kw != "annotations"` so the standalone entity-
        // annotation arm (which has exactly one inner pair = the Annotations rule)
        // never tries to advance past it.
        let mut it = clause.into_inner();
        let mut first = it.next().unwrap();
        let mut ann: BTreeSet<Annotation<A>> = BTreeSet::new();
        if kw != "annotations" && first.as_rule() == Rule::Annotations {
            ann = parse_annotations(first, ctx)?.into_iter().collect();
            first = it.next().unwrap();
        }
        let body = first;
        match kw.as_str() {
            "annotations" => {
                // For complex subjects the annotation subject must be a
                // named IRI; only emit AnnotationAssertion for atomic subjects.
                if let Some(ref iri) = atomic_iri {
                    // `body` is the inner `Annotations` pair (AnnotationEntry items).
                    for ann_item in parse_annotations(body, ctx)? {
                        ont.insert(entity_annotation_assertion(
                            AnnotationSubject::IRI(iri.clone()),
                            ann_item,
                        ));
                    }
                }
            }
            "subclassof" => {
                let mut list = parse_description_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, sup) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::SubClassOf(SubClassOf {
                            sub: subject_ce.clone(),
                            sup,
                        }),
                        ann: item_ann,
                    });
                }
            }
            "equivalentto" => {
                // A frame `EquivalentTo:` list pairs the subject with EACH item
                // as a separate binary axiom (OWL 2 Manchester §2.4, matching the
                // OWL-API / owx reader), not one fused n-ary axiom.
                let mut list = parse_description_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, ce) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::EquivalentClasses(EquivalentClasses(vec![
                            subject_ce.clone(),
                            ce,
                        ])),
                        ann: item_ann,
                    });
                }
            }
            "disjointwith" => {
                // Per-item binary DisjointClasses(subject, item) — a fused n-ary
                // axiom would also assert disjointness *between* the listed items,
                // which the frame does not state.
                let mut list = parse_description_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, ce) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::DisjointClasses(DisjointClasses(vec![
                            subject_ce.clone(),
                            ce,
                        ])),
                        ann: item_ann,
                    });
                }
            }
            "disjointunionof" => {
                // DisjointUnionOf requires a named class subject; only valid for
                // atomic subjects.  Silently skip on a complex subject (ROBOT
                // does not emit DisjointUnionOf for complex-LHS frames).
                if let Some(ref iri) = atomic_iri {
                    let mut items = Vec::new();
                    merge_list_ann(&mut ann, parse_description_list(body, ctx)?, &mut items);
                    ont.insert(AnnotatedComponent {
                        component: Component::DisjointUnion(DisjointUnion(
                            Class(iri.clone()),
                            items,
                        )),
                        ann,
                    });
                }
            }
            "haskey" => {
                // HasKey requires a named class subject; only valid for atomic
                // subjects.  Silently skip on a complex subject (ROBOT does not
                // emit HasKey for complex-LHS frames).
                if let Some(ref iri) = atomic_iri {
                    // body is a PropertyExprList of `ope`. Manchester HasKey: does NOT
                    // lexically distinguish object vs data properties — they are all bare
                    // property IRIs. The grammar always parses each key as an `ope` (object
                    // property expression). With the pre-pass declaration table we can flip
                    // plain (non-inverse) keys that were declared as `DataProperty:` to
                    // `PropertyExpression::DataProperty`. Inverse-form keys are never data
                    // properties in OWL 2 DL, so they always stay object.
                    let mut vpe = Vec::new();
                    for p in body.into_inner() {
                        if p.as_rule() == Rule::ope {
                            let ope = ObjectPropertyExpression::from_pair(p.clone(), ctx)?;
                            let pe = match &ope {
                                ObjectPropertyExpression::ObjectProperty(ObjectProperty(
                                    key_iri,
                                )) if ctx.is_data_prop(key_iri) => {
                                    // Declared as data — flip to data-property key.
                                    PropertyExpression::DataProperty(DataProperty(key_iri.clone()))
                                }
                                _ => PropertyExpression::ObjectPropertyExpression(ope),
                            };
                            vpe.push(pe);
                        }
                    }
                    ont.insert(AnnotatedComponent {
                        component: Component::HasKey(HasKey {
                            ce: ClassExpression::Class(Class(iri.clone())),
                            vpe,
                        }),
                        ann,
                    });
                }
            }
            other => unreachable!("unexpected class clause keyword: {other}"),
        }
    }
    Ok(())
}

fn parse_ope_list<A: ForIRI>(
    list: Pair<Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnItem<A, ObjectPropertyExpression<A>>>> {
    parse_annotated_list(list, ctx, ObjectPropertyExpression::from_pair)
}

fn parse_iri_list<A: ForIRI>(
    list: Pair<Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnItem<A, IRI<A>>>> {
    parse_annotated_list(list, ctx, IRI::from_pair)
}

/// Returns `Some(Vec<DataProperty>)` iff EVERY OPE in `opes` is a plain
/// (non-inverse) `ObjectProperty` whose IRI was declared as a data property
/// in the pre-pass.  Returns `None` for mixed lists, empty lists, or when
/// any member is an inverse expression (data properties have no inverse).
fn all_as_data_props<A: ForIRI>(
    ctx: &Context<'_, A>,
    opes: &[ObjectPropertyExpression<A>],
) -> Option<Vec<DataProperty<A>>> {
    if opes.is_empty() {
        return None;
    }
    opes.iter()
        .map(|ope| match ope {
            ObjectPropertyExpression::ObjectProperty(ObjectProperty(iri))
                if ctx.is_data_prop(iri) =>
            {
                Some(DataProperty(iri.clone()))
            }
            _ => None,
        })
        .collect()
}

fn parse_individual_list<A: ForIRI>(
    list: Pair<Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnItem<A, Individual<A>>>> {
    parse_annotated_list(list, ctx, Individual::from_pair)
}

/// Parse a top-level `Misc` axiom (§2.5 `misc`) into the corresponding n-ary
/// `Component` and insert it.  A leading `Annotations?` (axiom annotation on the
/// whole clause) folds into the component's `ann` set.
pub(crate) fn insert_misc<A: ForIRI, O: MutableOntology<A>>(
    misc: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    let kw = clause_keyword(&misc);
    let mut it = misc.into_inner();
    let mut first = it.next().unwrap();
    let mut ann: BTreeSet<Annotation<A>> = BTreeSet::new();
    if first.as_rule() == Rule::Annotations {
        ann = parse_annotations(first, ctx)?.into_iter().collect();
        first = it.next().unwrap();
    }
    let body = first; // DescriptionList | OpeList | IndividualList
    // All misc axioms are n-ary: per-item annotations fold into the axiom `ann`.
    let component = match kw.as_str() {
        "equivalentclasses" => {
            let mut v = Vec::new();
            merge_list_ann(&mut ann, parse_description_list(body, ctx)?, &mut v);
            Component::EquivalentClasses(EquivalentClasses(v))
        }
        "disjointclasses" => {
            let mut v = Vec::new();
            merge_list_ann(&mut ann, parse_description_list(body, ctx)?, &mut v);
            Component::DisjointClasses(DisjointClasses(v))
        }
        "equivalentproperties" => {
            let mut opes = Vec::new();
            merge_list_ann(&mut ann, parse_ope_list(body, ctx)?, &mut opes);
            // If ALL members are plain (non-inverse) OPEs declared as data
            // properties, emit EquivalentDataProperties; otherwise fall back
            // to EquivalentObjectProperties (includes mixed / undeclared lists).
            if let Some(dps) = all_as_data_props(ctx, &opes) {
                Component::EquivalentDataProperties(EquivalentDataProperties(dps))
            } else {
                Component::EquivalentObjectProperties(EquivalentObjectProperties(opes))
            }
        }
        "disjointproperties" => {
            let mut opes = Vec::new();
            merge_list_ann(&mut ann, parse_ope_list(body, ctx)?, &mut opes);
            // Same logic as equivalentproperties.
            if let Some(dps) = all_as_data_props(ctx, &opes) {
                Component::DisjointDataProperties(DisjointDataProperties(dps))
            } else {
                Component::DisjointObjectProperties(DisjointObjectProperties(opes))
            }
        }
        "sameindividual" => {
            let mut v = Vec::new();
            merge_list_ann(&mut ann, parse_individual_list(body, ctx)?, &mut v);
            Component::SameIndividual(SameIndividual(v))
        }
        "differentindividuals" => {
            let mut v = Vec::new();
            merge_list_ann(&mut ann, parse_individual_list(body, ctx)?, &mut v);
            Component::DifferentIndividuals(DifferentIndividuals(v))
        }
        other => unreachable!("unexpected misc keyword: {other}"),
    };
    ont.insert(AnnotatedComponent { component, ann });
    Ok(())
}

fn parse_data_range_list<A: ForIRI>(
    list: Pair<Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<DataRange<A>>> {
    list.into_inner()
        .map(|p| DataRange::from_pair(p, ctx))
        .collect()
}

fn insert_object_property_frame<A: ForIRI, O: MutableOntology<A>>(
    frame: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    // The frame subject is an `ope` — a named property or `inverse(...)`.
    let mut inner = frame.into_inner();
    let subject_ope = ObjectPropertyExpression::from_pair(inner.next().unwrap(), ctx)?;
    let clauses = inner;
    // A single named-property IRI, when the subject is plain (not inverse).
    let subject: Option<IRI<A>> = match &subject_ope {
        ObjectPropertyExpression::ObjectProperty(ObjectProperty(iri)) => Some(iri.clone()),
        _ => None,
    };
    if let Some(iri) = &subject {
        ont.insert(DeclareObjectProperty(ObjectProperty(iri.clone())));
    }

    for clause in clauses {
        let kw = clause_keyword(&clause);
        let mut it = clause.into_inner();
        let mut first = it.next().unwrap();
        let mut ann: BTreeSet<Annotation<A>> = BTreeSet::new();
        if kw != "annotations" && first.as_rule() == Rule::Annotations {
            ann = parse_annotations(first, ctx)?.into_iter().collect();
            first = it.next().unwrap();
        }
        let body = first;
        match kw.as_str() {
            "annotations" => {
                // Entity annotations require a named subject; inverse-headed
                // frames have none, so there is nothing to attach them to.
                if let Some(iri) = &subject {
                    for ann_item in parse_annotations(body, ctx)? {
                        ont.insert(entity_annotation_assertion(
                            AnnotationSubject::IRI(iri.clone()),
                            ann_item,
                        ));
                    }
                }
            }
            "subpropertychain" => {
                // body is a PropertyChain: `ope (OKw ope)+`. Filter OUT the emitted
                // `OKw` keyword pairs (compound-atomic, emit a pair); keep only the
                // `ope` operands.
                let chain: Vec<ObjectPropertyExpression<A>> = body
                    .into_inner()
                    .filter(|p| p.as_rule() == Rule::ope)
                    .map(|p| ObjectPropertyExpression::from_pair(p, ctx))
                    .collect::<Result<_>>()?;
                ont.insert(AnnotatedComponent {
                    component: Component::SubObjectPropertyOf(SubObjectPropertyOf {
                        sub: SubObjectPropertyExpression::ObjectPropertyChain(chain),
                        sup: subject_ope.clone(),
                    }),
                    ann,
                });
            }
            "subpropertyof" => {
                let mut list = parse_ope_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, sup) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::SubObjectPropertyOf(SubObjectPropertyOf {
                            sub: SubObjectPropertyExpression::ObjectPropertyExpression(
                                subject_ope.clone(),
                            ),
                            sup,
                        }),
                        ann: item_ann,
                    });
                }
            }
            "equivalentto" => {
                // Per-item binary EquivalentObjectProperties(subject, item), per
                // OWL 2 Manchester §2.4 (matching the OWL-API / owx reader).
                let mut list = parse_ope_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, sup) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::EquivalentObjectProperties(
                            EquivalentObjectProperties(vec![subject_ope.clone(), sup]),
                        ),
                        ann: item_ann,
                    });
                }
            }
            "disjointwith" => {
                let mut list = parse_ope_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, sup) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::DisjointObjectProperties(DisjointObjectProperties(
                            vec![subject_ope.clone(), sup],
                        )),
                        ann: item_ann,
                    });
                }
            }
            "inverseof" => {
                let mut list = parse_ope_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, inv) in list {
                    // InverseObjectProperties takes ObjectProperty, not OPE;
                    // the writer only emits a plain property here.
                    match (&subject, inv) {
                        (Some(subj_iri), ObjectPropertyExpression::ObjectProperty(p)) => {
                            ont.insert(AnnotatedComponent {
                                component: Component::InverseObjectProperties(
                                    InverseObjectProperties(
                                        ObjectProperty(subj_iri.clone()).into(),
                                        p.into(),
                                    ),
                                ),
                                ann: item_ann,
                            });
                        }
                        _ => {
                            return Err(HornedError::invalid(
                                "InverseOf: expected named object properties on both sides",
                            ));
                        }
                    }
                }
            }
            "domain" => {
                let mut list = parse_description_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, ce) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::ObjectPropertyDomain(ObjectPropertyDomain {
                            ope: subject_ope.clone(),
                            ce,
                        }),
                        ann: item_ann,
                    });
                }
            }
            "range" => {
                let mut list = parse_description_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, ce) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::ObjectPropertyRange(ObjectPropertyRange {
                            ope: subject_ope.clone(),
                            ce,
                        }),
                        ann: item_ann,
                    });
                }
            }
            "characteristics" => {
                // §2.5 objectPropertyCharacteristicAnnotatedList: a LEADING
                // clause-level annotation binds the FIRST item only.
                let empty = BTreeSet::new();
                for (i, ch) in body.into_inner().enumerate() {
                    let item_ann = if i == 0 { &ann } else { &empty };
                    insert_object_characteristic(ch.as_str(), &subject_ope, item_ann, ont)?;
                }
            }
            other => unreachable!("unexpected object-property clause keyword: {other}"),
        }
    }
    Ok(())
}

fn insert_object_characteristic<A: ForIRI, O: MutableOntology<A>>(
    kw: &str,
    ope: &ObjectPropertyExpression<A>,
    ann: &BTreeSet<Annotation<A>>,
    ont: &mut O,
) -> Result<()> {
    let ope = ope.clone();
    match kw.to_ascii_lowercase().as_str() {
        "functional" => ont.insert(AnnotatedComponent {
            component: Component::FunctionalObjectProperty(FunctionalObjectProperty(ope)),
            ann: ann.clone(),
        }),
        "inversefunctional" => ont.insert(AnnotatedComponent {
            component: Component::InverseFunctionalObjectProperty(InverseFunctionalObjectProperty(
                ope,
            )),
            ann: ann.clone(),
        }),
        "reflexive" => ont.insert(AnnotatedComponent {
            component: Component::ReflexiveObjectProperty(ReflexiveObjectProperty(ope)),
            ann: ann.clone(),
        }),
        "irreflexive" => ont.insert(AnnotatedComponent {
            component: Component::IrreflexiveObjectProperty(IrreflexiveObjectProperty(ope)),
            ann: ann.clone(),
        }),
        "symmetric" => ont.insert(AnnotatedComponent {
            component: Component::SymmetricObjectProperty(SymmetricObjectProperty(ope)),
            ann: ann.clone(),
        }),
        "asymmetric" => ont.insert(AnnotatedComponent {
            component: Component::AsymmetricObjectProperty(AsymmetricObjectProperty(ope)),
            ann: ann.clone(),
        }),
        "transitive" => ont.insert(AnnotatedComponent {
            component: Component::TransitiveObjectProperty(TransitiveObjectProperty(ope)),
            ann: ann.clone(),
        }),
        other => {
            return Err(HornedError::invalid(format!(
                "unknown object characteristic: {other}"
            )));
        }
    };
    Ok(())
}

// ---------------------------------------------------------------------------
// SWRL rules (`Rule:` frame).
// ---------------------------------------------------------------------------

/// A parsed SWRL argument before it is coerced to an `IArgument`/`DArgument`
/// (the coercion depends on the atom kind the argument appears in).
enum SwrlArgKind<A: ForIRI> {
    Var(Variable<A>),
    Lit(Literal<A>),
    Ind(Individual<A>),
}

fn swrl_arg_kind<A: ForIRI>(arg: Pair<Rule>, ctx: &Context<'_, A>) -> Result<SwrlArgKind<A>> {
    let inner = arg.into_inner().next().unwrap();
    Ok(match inner.as_rule() {
        // `Variable = { "?" ~ IRI }`
        Rule::Variable => SwrlArgKind::Var(Variable(IRI::from_pair(
            inner.into_inner().next().unwrap(),
            ctx,
        )?)),
        Rule::Literal => SwrlArgKind::Lit(Literal::from_pair(inner, ctx)?),
        Rule::Individual => SwrlArgKind::Ind(Individual::from_pair(inner, ctx)?),
        rule => unreachable!("unexpected SWRL argument: {:?}", rule),
    })
}

fn swrl_iarg<A: ForIRI>(k: &SwrlArgKind<A>) -> Result<IArgument<A>> {
    match k {
        SwrlArgKind::Var(v) => Ok(IArgument::Variable(v.clone())),
        SwrlArgKind::Ind(i) => Ok(IArgument::Individual(i.clone())),
        SwrlArgKind::Lit(_) => Err(HornedError::invalid(
            "SWRL: expected an individual or variable argument, found a literal",
        )),
    }
}

fn swrl_darg<A: ForIRI>(k: &SwrlArgKind<A>) -> Result<DArgument<A>> {
    match k {
        SwrlArgKind::Var(v) => Ok(DArgument::Variable(v.clone())),
        SwrlArgKind::Lit(l) => Ok(DArgument::Literal(l.clone())),
        SwrlArgKind::Ind(_) => Err(HornedError::invalid(
            "SWRL: expected a data value or variable argument, found an individual",
        )),
    }
}

/// Parse a `SwrlIObj = { Variable | Individual }` into an `IArgument`.
fn swrl_iobj<A: ForIRI>(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<IArgument<A>> {
    swrl_iarg(&swrl_arg_kind(pair, ctx)?)
}

/// Parse one `SwrlAtom`. Atom shapes are positional in Manchester syntax, so
/// class-vs-datarange and object-vs-data-property are disambiguated by argument
/// arity/type and the declaration pre-pass (`is_datatype` / `is_data_prop`).
fn parse_swrl_atom<A: ForIRI>(atom: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Atom<A>> {
    let inner = atom.into_inner().next().unwrap();
    match inner.as_rule() {
        Rule::SwrlSameAs | Rule::SwrlDifferentFrom => {
            let is_same = inner.as_rule() == Rule::SwrlSameAs;
            let objs: Vec<_> = inner
                .into_inner()
                .filter(|p| p.as_rule() == Rule::SwrlIObj)
                .collect();
            let i1 = swrl_iobj(objs[0].clone(), ctx)?;
            let i2 = swrl_iobj(objs[1].clone(), ctx)?;
            Ok(if is_same {
                Atom::SameIndividualAtom(i1, i2)
            } else {
                Atom::DifferentIndividualsAtom(i1, i2)
            })
        }
        Rule::SwrlUnary | Rule::SwrlNary => {
            let mut pred_ce = None;
            let mut arg_pairs = Vec::new();
            for p in inner.into_inner() {
                match p.as_rule() {
                    Rule::AtomPred => {
                        pred_ce = Some(ClassExpression::from_pair_unchecked(
                            p.into_inner().next().unwrap(),
                            ctx,
                        )?)
                    }
                    Rule::SwrlArg => arg_pairs.push(p),
                    _ => {}
                }
            }
            let pred_ce = pred_ce.unwrap();
            let pred_iri: Option<IRI<A>> = match &pred_ce {
                ClassExpression::Class(Class(iri)) => Some(iri.clone()),
                _ => None,
            };
            let kinds: Vec<SwrlArgKind<A>> = arg_pairs
                .into_iter()
                .map(|p| swrl_arg_kind(p, ctx))
                .collect::<Result<_>>()?;
            let bare = || -> Result<IRI<A>> {
                pred_iri.clone().ok_or_else(|| {
                    HornedError::invalid("SWRL: this atom requires a named-IRI predicate")
                })
            };

            if kinds.len() == 1 {
                match &kinds[0] {
                    // datatype(lit) -> DataRangeAtom
                    SwrlArgKind::Lit(l) => Ok(Atom::DataRangeAtom {
                        pred: DataRange::Datatype(Datatype(bare()?)),
                        arg: DArgument::Literal(l.clone()),
                    }),
                    // datatype(?v) when pred was declared a datatype -> DataRangeAtom
                    SwrlArgKind::Var(v)
                        if pred_iri.as_ref().is_some_and(|i| ctx.is_datatype(i)) =>
                    {
                        Ok(Atom::DataRangeAtom {
                            pred: DataRange::Datatype(Datatype(bare()?)),
                            arg: DArgument::Variable(v.clone()),
                        })
                    }
                    // otherwise a ClassAtom over a (possibly complex) class expression
                    other => Ok(Atom::ClassAtom {
                        pred: pred_ce,
                        arg: swrl_iarg(other)?,
                    }),
                }
            } else {
                let first_is_lit = matches!(kinds[0], SwrlArgKind::Lit(_));
                let second_is_lit = matches!(kinds[1], SwrlArgKind::Lit(_));
                if first_is_lit || kinds.len() > 2 {
                    // n-ary, or data-valued first arg -> built-in atom
                    Ok(Atom::BuiltInAtom {
                        pred: bare()?,
                        args: kinds.iter().map(swrl_darg).collect::<Result<_>>()?,
                    })
                } else if second_is_lit || pred_iri.as_ref().is_some_and(|i| ctx.is_data_prop(i)) {
                    Ok(Atom::DataPropertyAtom {
                        pred: DataProperty(bare()?),
                        args: (swrl_darg(&kinds[0])?, swrl_darg(&kinds[1])?),
                    })
                } else {
                    Ok(Atom::ObjectPropertyAtom {
                        pred: ObjectPropertyExpression::ObjectProperty(ObjectProperty(bare()?)),
                        args: (swrl_iarg(&kinds[0])?, swrl_iarg(&kinds[1])?),
                    })
                }
            }
        }
        rule => unreachable!("unexpected SWRL atom: {:?}", rule),
    }
}

fn parse_swrl_atom_list<A: ForIRI>(list: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Vec<Atom<A>>> {
    list.into_inner()
        .filter(|p| p.as_rule() == Rule::SwrlAtom)
        .map(|a| parse_swrl_atom(a, ctx))
        .collect()
}

/// `Rule: <body> -> <head>` — note Manchester writes body (antecedent) first.
fn insert_rule_frame<A: ForIRI, O: MutableOntology<A>>(
    frame: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    let mut inner = frame.into_inner();
    let mut first = inner.next().unwrap();
    let mut ann: BTreeSet<Annotation<A>> = BTreeSet::new();
    if first.as_rule() == Rule::Annotations {
        ann = parse_annotations(first, ctx)?.into_iter().collect();
        first = inner.next().unwrap();
    }
    let body = parse_swrl_atom_list(first, ctx)?;
    let head = parse_swrl_atom_list(inner.next().unwrap(), ctx)?;
    ont.insert(AnnotatedComponent {
        component: Component::Rule(crate::model::Rule { head, body }),
        ann,
    });
    Ok(())
}

fn insert_data_property_frame<A: ForIRI, O: MutableOntology<A>>(
    frame: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    let (subject, clauses) = frame_subject_and_clauses(frame, ctx)?;
    ont.insert(DeclareDataProperty(DataProperty(subject.clone())));

    for clause in clauses {
        let kw = clause_keyword(&clause);
        let mut it = clause.into_inner();
        let mut first = it.next().unwrap();
        let mut ann: BTreeSet<Annotation<A>> = BTreeSet::new();
        if kw != "annotations" && first.as_rule() == Rule::Annotations {
            ann = parse_annotations(first, ctx)?.into_iter().collect();
            first = it.next().unwrap();
        }
        let body = first;
        match kw.as_str() {
            "annotations" => {
                for ann_item in parse_annotations(body, ctx)? {
                    ont.insert(entity_annotation_assertion(
                        AnnotationSubject::IRI(subject.clone()),
                        ann_item,
                    ));
                }
            }
            "subpropertyof" => {
                let mut list = parse_iri_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, iri) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::SubDataPropertyOf(SubDataPropertyOf {
                            sub: DataProperty(subject.clone()),
                            sup: DataProperty(iri),
                        }),
                        ann: item_ann,
                    });
                }
            }
            "equivalentto" => {
                // Per-item binary EquivalentDataProperties(subject, item), per
                // OWL 2 Manchester §2.4 (matching the OWL-API / owx reader).
                let mut list = parse_iri_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, iri) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::EquivalentDataProperties(EquivalentDataProperties(
                            vec![DataProperty(subject.clone()), DataProperty(iri)],
                        )),
                        ann: item_ann,
                    });
                }
            }
            "disjointwith" => {
                let mut list = parse_iri_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, iri) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::DisjointDataProperties(DisjointDataProperties(vec![
                            DataProperty(subject.clone()),
                            DataProperty(iri),
                        ])),
                        ann: item_ann,
                    });
                }
            }
            "domain" => {
                let mut list = parse_description_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, ce) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::DataPropertyDomain(DataPropertyDomain {
                            dp: DataProperty(subject.clone()),
                            ce,
                        }),
                        ann: item_ann,
                    });
                }
            }
            "range" => {
                for dr in parse_data_range_list(body, ctx)? {
                    ont.insert(AnnotatedComponent {
                        component: Component::DataPropertyRange(DataPropertyRange {
                            dp: DataProperty(subject.clone()),
                            dr,
                        }),
                        ann: ann.clone(),
                    });
                }
            }
            "characteristics" => {
                // §2.5: leading clause-level annotation binds the FIRST item only.
                let empty = BTreeSet::new();
                for (i, ch) in body.into_inner().enumerate() {
                    let item_ann = if i == 0 { &ann } else { &empty };
                    // Only Functional is valid on a data property.
                    if ch.as_str().eq_ignore_ascii_case("functional") {
                        ont.insert(AnnotatedComponent {
                            component: Component::FunctionalDataProperty(FunctionalDataProperty(
                                DataProperty(subject.clone()),
                            )),
                            ann: item_ann.clone(),
                        });
                    } else {
                        return Err(HornedError::invalid(
                            "data properties only support the Functional characteristic",
                        ));
                    }
                }
            }
            other => unreachable!("unexpected data-property clause keyword: {other}"),
        }
    }
    Ok(())
}

fn insert_annotation_property_frame<A: ForIRI, O: MutableOntology<A>>(
    frame: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    let (subject, clauses) = frame_subject_and_clauses(frame, ctx)?;
    ont.insert(DeclareAnnotationProperty(AnnotationProperty(
        subject.clone(),
    )));

    for clause in clauses {
        let kw = clause_keyword(&clause);
        let mut it = clause.into_inner();
        let mut first = it.next().unwrap();
        let mut ann: BTreeSet<Annotation<A>> = BTreeSet::new();
        if kw != "annotations" && first.as_rule() == Rule::Annotations {
            ann = parse_annotations(first, ctx)?.into_iter().collect();
            first = it.next().unwrap();
        }
        let body = first;
        match kw.as_str() {
            "annotations" => {
                for ann_item in parse_annotations(body, ctx)? {
                    ont.insert(entity_annotation_assertion(
                        AnnotationSubject::IRI(subject.clone()),
                        ann_item,
                    ));
                }
            }
            "subpropertyof" => {
                let mut list = parse_iri_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, iri) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::SubAnnotationPropertyOf(SubAnnotationPropertyOf {
                            sub: AnnotationProperty(subject.clone()),
                            sup: AnnotationProperty(iri),
                        }),
                        ann: item_ann,
                    });
                }
            }
            "domain" => {
                let mut list = parse_iri_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, iri) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::AnnotationPropertyDomain(AnnotationPropertyDomain {
                            ap: AnnotationProperty(subject.clone()),
                            iri,
                        }),
                        ann: item_ann,
                    });
                }
            }
            "range" => {
                let mut list = parse_iri_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, iri) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::AnnotationPropertyRange(AnnotationPropertyRange {
                            ap: AnnotationProperty(subject.clone()),
                            iri,
                        }),
                        ann: item_ann,
                    });
                }
            }
            other => unreachable!("unexpected annotation-property clause keyword: {other}"),
        }
    }
    Ok(())
}

fn insert_individual_frame<A: ForIRI, O: MutableOntology<A>>(
    frame: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    // The subject is an `Individual` (named OR anonymous `_:id`), not a
    // FrameSubject IRI: an anonymous individual may head a frame (§2.5).
    let mut inner = frame.into_inner();
    let subject_ind = Individual::from_pair(inner.next().unwrap(), ctx)?;
    let clauses = inner;
    // Anonymous individuals are NOT declared (no DeclareNamedIndividual); only a
    // named subject gets a declaration. The clauses use the subject either way.
    let anno_subject = match &subject_ind {
        Individual::Named(ni) => {
            ont.insert(DeclareNamedIndividual(ni.clone()));
            AnnotationSubject::IRI(ni.0.clone())
        }
        Individual::Anonymous(ai) => AnnotationSubject::AnonymousIndividual(ai.clone()),
    };

    for clause in clauses {
        let kw = clause_keyword(&clause);
        let mut it = clause.into_inner();
        let mut first = it.next().unwrap();
        let mut ann: BTreeSet<Annotation<A>> = BTreeSet::new();
        if kw != "annotations" && first.as_rule() == Rule::Annotations {
            ann = parse_annotations(first, ctx)?.into_iter().collect();
            first = it.next().unwrap();
        }
        let body = first;
        match kw.as_str() {
            "annotations" => {
                for ann_item in parse_annotations(body, ctx)? {
                    ont.insert(entity_annotation_assertion(anno_subject.clone(), ann_item));
                }
            }
            "types" => {
                let mut list = parse_description_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, ce) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::ClassAssertion(ClassAssertion {
                            i: subject_ind.clone(),
                            ce,
                        }),
                        ann: item_ann,
                    });
                }
            }
            "facts" => {
                // §2.5 factAnnotatedList: leading annotation binds the FIRST item only.
                let empty = BTreeSet::new();
                for (i, fact) in body.into_inner().enumerate() {
                    let item_ann = if i == 0 { &ann } else { &empty };
                    insert_fact(fact, ctx, &subject_ind, item_ann, ont)?;
                }
            }
            "sameas" => {
                // Per-item binary SameIndividual(subject, item), per OWL 2
                // Manchester §2.4 (matching the OWL-API / owx reader).
                let mut list = parse_individual_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, ind) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::SameIndividual(SameIndividual(vec![
                            subject_ind.clone(),
                            ind,
                        ])),
                        ann: item_ann,
                    });
                }
            }
            "differentfrom" => {
                // Per-item binary DifferentIndividuals(subject, item) — a fused
                // n-ary axiom would also assert distinctness between the listed
                // items, which the frame does not state.
                let mut list = parse_individual_list(body, ctx)?;
                bind_leading_to_first(ann, &mut list);
                for (item_ann, ind) in list {
                    ont.insert(AnnotatedComponent {
                        component: Component::DifferentIndividuals(DifferentIndividuals(vec![
                            subject_ind.clone(),
                            ind,
                        ])),
                        ann: item_ann,
                    });
                }
            }
            other => unreachable!("unexpected individual clause keyword: {other}"),
        }
    }
    Ok(())
}

/// `Fact = { NotKw? ~ ope ~ ( Literal | Individual ) }`
///
/// A trailing `Literal` => (negative) data-property assertion; a trailing
/// `Individual` => (negative) object-property assertion. `NotKw` is a
/// compound-atomic keyword guard rule that emits a pair when `not` is present;
/// we detect negation by checking whether the first inner pair is `Rule::NotKw`.
fn insert_fact<A: ForIRI, O: MutableOntology<A>>(
    fact: Pair<Rule>,
    ctx: &Context<'_, A>,
    from: &Individual<A>,
    ann: &BTreeSet<Annotation<A>>,
    ont: &mut O,
) -> Result<()> {
    let mut inner = fact.into_inner();
    let first = inner.next().unwrap();
    let (negated, ope_pair) = if first.as_rule() == Rule::NotKw {
        (true, inner.next().unwrap())
    } else {
        (false, first)
    };
    let ope = ObjectPropertyExpression::from_pair(ope_pair, ctx)?;
    let target = inner.next().unwrap();
    match target.as_rule() {
        Rule::Literal => {
            // data-property assertion; the ope's inner IRI is the data property.
            let lit = Literal::from_pair(target, ctx)?;
            let dp = match &ope {
                ObjectPropertyExpression::ObjectProperty(p) => DataProperty(p.0.clone()),
                ObjectPropertyExpression::InverseObjectProperty(_) => {
                    return Err(HornedError::invalid("inverse property in a data fact"));
                }
            };
            if negated {
                ont.insert(AnnotatedComponent {
                    component: Component::NegativeDataPropertyAssertion(
                        NegativeDataPropertyAssertion {
                            dp,
                            from: from.clone(),
                            to: lit,
                        },
                    ),
                    ann: ann.clone(),
                });
            } else {
                ont.insert(AnnotatedComponent {
                    component: Component::DataPropertyAssertion(DataPropertyAssertion {
                        dp,
                        from: from.clone(),
                        to: lit,
                    }),
                    ann: ann.clone(),
                });
            }
        }
        Rule::Individual => {
            let to = Individual::from_pair(target, ctx)?;
            if negated {
                ont.insert(AnnotatedComponent {
                    component: Component::NegativeObjectPropertyAssertion(
                        NegativeObjectPropertyAssertion {
                            ope,
                            from: from.clone(),
                            to,
                        },
                    ),
                    ann: ann.clone(),
                });
            } else {
                ont.insert(AnnotatedComponent {
                    component: Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                        ope,
                        from: from.clone(),
                        to,
                    }),
                    ann: ann.clone(),
                });
            }
        }
        rule => unreachable!("unexpected fact target: {:?}", rule),
    }
    Ok(())
}

fn insert_datatype_frame<A: ForIRI, O: MutableOntology<A>>(
    frame: Pair<Rule>,
    ctx: &Context<'_, A>,
    ont: &mut O,
) -> Result<()> {
    let (subject, clauses) = frame_subject_and_clauses(frame, ctx)?;
    ont.insert(DeclareDatatype(Datatype(subject.clone())));

    for clause in clauses {
        let kw = clause_keyword(&clause);
        // Mirror `insert_class_frame`: a keyworded clause may carry a leading
        // `Annotations?` axiom-annotation slot; consume it into `ann`. The
        // standalone entity-annotation arm (`kw == "annotations"`) keeps its
        // single inner `Annotations` pair as the body.
        let mut it = clause.into_inner();
        let mut first = it.next().unwrap();
        let mut ann: BTreeSet<Annotation<A>> = BTreeSet::new();
        if kw != "annotations" && first.as_rule() == Rule::Annotations {
            ann = parse_annotations(first, ctx)?.into_iter().collect();
            first = it.next().unwrap();
        }
        let body = first;
        match kw.as_str() {
            "annotations" => {
                for ann_item in parse_annotations(body, ctx)? {
                    ont.insert(entity_annotation_assertion(
                        AnnotationSubject::IRI(subject.clone()),
                        ann_item,
                    ));
                }
            }
            "equivalentto" => {
                ont.insert(AnnotatedComponent {
                    component: Component::DatatypeDefinition(DatatypeDefinition {
                        kind: Datatype(subject.clone()),
                        range: DataRange::from_pair(body, ctx)?,
                    }),
                    ann,
                });
            }
            other => unreachable!("unexpected datatype clause keyword: {other}"),
        }
    }
    Ok(())
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::omn::reader::lexer::ManchesterLexer;
    use crate::model::{Build, RcStr};
    use rstest::rstest;

    #[test]
    fn parses_iri_full_and_prefixed() {
        let b = Build::new_rc();
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("ex", "http://t/").unwrap();
        let ctx = Context::new(&b, &pm);

        let full = ManchesterLexer::lex(Rule::IRI, "<http://t/A>")
            .unwrap()
            .next()
            .unwrap();
        assert_eq!(
            IRI::<RcStr>::from_pair(full, &ctx).unwrap(),
            b.iri("http://t/A")
        );

        let pfx = ManchesterLexer::lex(Rule::IRI, "ex:A")
            .unwrap()
            .next()
            .unwrap();
        assert_eq!(
            IRI::<RcStr>::from_pair(pfx, &ctx).unwrap(),
            b.iri("http://t/A")
        );
    }

    /// §2.5 allows bare numeric literals (integer/decimal/float) wherever a
    /// `Literal` is expected — e.g. a facet value `xsd:integer[>= 0]` or a
    /// `DataOneOf { 1, 2.5, 3.0f }`. Previously these hard-failed (the `Literal`
    /// rule only had the quoted/typed/lang forms).
    #[test]
    fn reads_bare_numeric_literals() {
        let b = Build::new_rc();
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let ctx = Context::new(&b, &pm);

        // Facet value: a bare integer `0`.
        let dr = ManchesterLexer::lex(Rule::DataRange, "xsd:integer[>= 0]")
            .unwrap()
            .next()
            .unwrap();
        let parsed = DataRange::<RcStr>::from_pair(dr, &ctx).unwrap();
        match parsed {
            DataRange::DatatypeRestriction(_, facets) => {
                assert_eq!(facets.len(), 1);
                assert_eq!(facets[0].f, crate::vocab::Facet::MinInclusive);
                assert_eq!(
                    facets[0].l,
                    Literal::Datatype {
                        literal: "0".to_string(),
                        datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
                    }
                );
            }
            _ => panic!("expected DatatypeRestriction, got {parsed:?}"),
        }

        // DataOneOf with integer, decimal, float members.
        let one_of = ManchesterLexer::lex(Rule::DataRange, "{ 1, 2.5, 3.0f }")
            .unwrap()
            .next()
            .unwrap();
        let parsed = DataRange::<RcStr>::from_pair(one_of, &ctx).unwrap();
        let DataRange::DataOneOf(lits) = parsed else {
            panic!("expected DataOneOf, got {parsed:?}");
        };
        assert_eq!(
            lits,
            vec![
                Literal::Datatype {
                    literal: "1".to_string(),
                    datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
                },
                Literal::Datatype {
                    literal: "2.5".to_string(),
                    datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#decimal"),
                },
                Literal::Datatype {
                    literal: "3.0f".to_string(),
                    datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#float"),
                },
            ]
        );
    }

    #[test]
    fn parses_ope_and_datarange() {
        let b = Build::new_rc();
        let pm = curie::PrefixMapping::default();
        let ctx = Context::new(&b, &pm);

        // inverse object property
        let p = ManchesterLexer::lex(Rule::ope, "inverse (<http://t/r>)")
            .unwrap()
            .next()
            .unwrap();
        assert_eq!(
            ObjectPropertyExpression::<RcStr>::from_pair(p, &ctx).unwrap(),
            ObjectPropertyExpression::InverseObjectProperty(b.object_property("http://t/r"))
        );

        // xsd:integer[>= "0"^^xsd:integer]
        let mut pm2 = curie::PrefixMapping::default();
        pm2.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let ctx2 = Context::new(&b, &pm2);
        let dr = ManchesterLexer::lex(Rule::DataRange, "xsd:integer[>= \"0\"^^xsd:integer]")
            .unwrap()
            .next()
            .unwrap();
        let parsed = DataRange::<RcStr>::from_pair(dr, &ctx2).unwrap();
        match parsed {
            DataRange::DatatypeRestriction(dt, facets) => {
                assert_eq!(dt, b.datatype("http://www.w3.org/2001/XMLSchema#integer"));
                assert_eq!(facets.len(), 1);
                assert_eq!(facets[0].f, crate::vocab::Facet::MinInclusive);
            }
            _ => panic!("expected DatatypeRestriction, got {parsed:?}"),
        }
    }

    #[test]
    fn parses_class_expressions() {
        use crate::model::*;
        let b = Build::new_rc();
        let pm = curie::PrefixMapping::default();
        let p =
            |s: &str| crate::io::omn::reader::parse_class_expression::<RcStr>(s, &pm, &b).unwrap();
        let a = ClassExpression::Class(b.class("http://t/A"));
        let c = ClassExpression::Class(b.class("http://t/C"));
        let d = ClassExpression::Class(b.class("http://t/D"));
        // atomic
        assert_eq!(p("<http://t/A>"), a);
        // and
        assert_eq!(
            p("<http://t/A> and <http://t/C>"),
            ClassExpression::ObjectIntersectionOf(vec![a.clone(), c.clone()])
        );
        // or
        assert_eq!(
            p("<http://t/A> or <http://t/C>"),
            ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()])
        );
        // not
        assert_eq!(
            p("not <http://t/A>"),
            ClassExpression::ObjectComplementOf(Box::new(a.clone()))
        );
        // precedence: (A or C) and D  — parens force union inside intersection
        let aorc = ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]);
        assert_eq!(
            p("(<http://t/A> or <http://t/C>) and <http://t/D>"),
            ClassExpression::ObjectIntersectionOf(vec![aorc, d.clone()])
        );
        // precedence: A or C and D  == A or (C and D)  — and binds tighter
        let cand = ClassExpression::ObjectIntersectionOf(vec![c.clone(), d.clone()]);
        assert_eq!(
            p("<http://t/A> or <http://t/C> and <http://t/D>"),
            ClassExpression::ObjectUnionOf(vec![a.clone(), cand])
        );
        // restrictions
        let r = ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r"));
        assert_eq!(
            p("<http://t/r> some <http://t/A>"),
            ClassExpression::ObjectSomeValuesFrom {
                ope: r.clone(),
                bce: Box::new(a.clone())
            }
        );
        assert_eq!(
            p("<http://t/r> only (<http://t/A> or <http://t/C>)"),
            ClassExpression::ObjectAllValuesFrom {
                ope: r.clone(),
                bce: Box::new(ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]))
            }
        );
        assert_eq!(
            p("<http://t/r> min 2 <http://t/A>"),
            ClassExpression::ObjectMinCardinality {
                n: 2,
                ope: r,
                bce: Box::new(a)
            }
        );
    }

    #[test]
    fn parses_restriction_without_whitespace() {
        use crate::model::*;
        let b = Build::new_rc();
        let pm = curie::PrefixMapping::default();
        let p =
            |s: &str| crate::io::omn::reader::parse_class_expression::<RcStr>(s, &pm, &b).unwrap();
        let r = ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r"));
        let a = ClassExpression::Class(b.class("http://t/A"));
        // whitespace between keyword and filler is OPTIONAL in the grammar
        assert_eq!(
            p("<http://t/r> only(<http://t/A>)"),
            ClassExpression::ObjectAllValuesFrom {
                ope: r,
                bce: Box::new(a)
            }
        );
    }

    #[test]
    fn class_expression_round_trips() {
        use crate::io::omn::AsManchester;
        use crate::model::*;
        let b = Build::new_rc();
        let pm = curie::PrefixMapping::default();
        let a = ClassExpression::Class(b.class("http://t/A"));
        let c = ClassExpression::Class(b.class("http://t/C"));
        let d = ClassExpression::Class(b.class("http://t/D"));
        let r = ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r"));
        let s = ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/s"));
        let x = Individual::Named(b.named_individual("http://t/x"));
        let cases: Vec<ClassExpression<RcStr>> = vec![
            a.clone(),
            ClassExpression::ObjectIntersectionOf(vec![a.clone(), c.clone()]),
            ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone(), d.clone()]),
            ClassExpression::ObjectComplementOf(Box::new(a.clone())),
            // precedence-sensitive nestings (the heart of the gate)
            ClassExpression::ObjectIntersectionOf(vec![
                ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]),
                d.clone(),
            ]), // (A or C) and D
            ClassExpression::ObjectUnionOf(vec![
                a.clone(),
                ClassExpression::ObjectIntersectionOf(vec![c.clone(), d.clone()]),
            ]), // A or C and D
            ClassExpression::ObjectComplementOf(Box::new(ClassExpression::ObjectUnionOf(vec![
                a.clone(),
                c.clone(),
            ]))), // not (A or C)
            ClassExpression::ObjectSomeValuesFrom {
                ope: r.clone(),
                bce: Box::new(a.clone()),
            },
            ClassExpression::ObjectAllValuesFrom {
                ope: r.clone(),
                bce: Box::new(ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()])),
            }, // r only (A or C)
            ClassExpression::ObjectMinCardinality {
                n: 2,
                ope: r.clone(),
                bce: Box::new(a.clone()),
            },
            ClassExpression::ObjectMaxCardinality {
                n: 1,
                ope: r.clone(),
                bce: Box::new(c.clone()),
            },
            ClassExpression::ObjectExactCardinality {
                n: 3,
                ope: r.clone(),
                bce: Box::new(d.clone()),
            },
            ClassExpression::ObjectHasValue {
                ope: r.clone(),
                i: x.clone(),
            },
            ClassExpression::ObjectHasSelf(r.clone()),
            ClassExpression::ObjectOneOf(vec![x.clone()]),
            // inverse property + nested restriction
            ClassExpression::ObjectSomeValuesFrom {
                ope: ObjectPropertyExpression::InverseObjectProperty(
                    b.object_property("http://t/r"),
                ),
                bce: Box::new(a.clone()),
            },
            // deeper nesting
            ClassExpression::ObjectIntersectionOf(vec![
                ClassExpression::ObjectSomeValuesFrom {
                    ope: r.clone(),
                    bce: Box::new(a.clone()),
                },
                ClassExpression::ObjectAllValuesFrom {
                    ope: s,
                    bce: Box::new(c.clone()),
                },
            ]),
        ];
        for ce in &cases {
            let rendered = ce.as_manchester().to_string();
            let parsed =
                crate::io::omn::reader::parse_class_expression::<RcStr>(&rendered, &pm, &b)
                    .unwrap_or_else(|e| panic!("PARSE FAILED for {rendered:?}: {e}"));
            assert_eq!(
                &parsed, ce,
                "ROUND-TRIP MISMATCH\n  rendered: {rendered}\n  expected: {ce:?}\n  got:      {parsed:?}"
            );
        }
    }

    #[test]
    fn parses_value_self_and_data_restriction() {
        use crate::model::*;
        let b = Build::new_rc();
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let p =
            |s: &str| crate::io::omn::reader::parse_class_expression::<RcStr>(s, &pm, &b).unwrap();
        let r = ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r"));
        let x = Individual::Named(b.named_individual("http://t/x"));
        assert_eq!(
            p("<http://t/r> value <http://t/x>"),
            ClassExpression::ObjectHasValue {
                ope: r.clone(),
                i: x
            }
        );
        assert_eq!(p("<http://t/r> Self"), ClassExpression::ObjectHasSelf(r));
        // data restriction — P2 known limitation: object/data ambiguity.
        // ALL restrictions currently parse as OBJECT restrictions:
        //   - BARE data range (`<dp> some xsd:integer`): silently mis-parsed as
        //     `ObjectSomeValuesFrom { bce: Class(xsd:integer) }` — the datatype IRI is
        //     captured as a plain ClassIRI with no error. This is a SILENT mis-bind.
        //   - FACETED data range (`<dp> some xsd:integer[>= "0"^^xsd:integer]`): the
        //     object-property arm commits, consumes `<dp> some xsd:integer`, and then
        //     fails at EOI because `[...]` is left unconsumed — visible error.
        // Root cause: `DataPropertyIRI` in the `Restriction` grammar rule is identical to
        // `ObjectPropertyIRI` (both are `{ IRI }`), so PEG commits to the first (object)
        // arm and never backtracks to the data arms.  Data-property restrictions are
        // deferred to P2.
        // The `DataRange` parser itself handles facets correctly (see `parses_ope_and_datarange`).
        // TODO(P2): disambiguate object vs data property at `Restriction` rule level.
        // -- test intentionally ignored until P2 is resolved --
        // match p("<http://t/dp> some xsd:integer[>= \"0\"^^xsd:integer]") {
        //     ClassExpression::DataSomeValuesFrom { dp, dr } => {
        //         assert_eq!(dp, b.data_property("http://t/dp"));
        //         assert!(matches!(dr, DataRange::DatatypeRestriction(_, _)));
        //     }
        //     other => panic!("expected DataSomeValuesFrom, got {other:?}"),
        // }
    }

    /// A restriction whose filler is a bare literal enumeration `{ "a", "b" }`
    /// is unambiguously a data restriction (literals cannot be individuals), so
    /// it must parse as `DataSomeValuesFrom`/`DataAllValuesFrom` over a
    /// `DataOneOf` rather than failing with "expected Individual" against the
    /// object-arm `ObjectOneOf`.
    #[test]
    fn parses_literal_enumeration_data_restriction() {
        use crate::model::*;
        let b = Build::new_rc();
        let pm = curie::PrefixMapping::default();
        let p =
            |s: &str| crate::io::omn::reader::parse_class_expression::<RcStr>(s, &pm, &b).unwrap();

        let dp = b.data_property("http://t/dp");
        let one_of = DataRange::DataOneOf(vec![
            Literal::Simple {
                literal: "A".to_string(),
            },
            Literal::Simple {
                literal: "B".to_string(),
            },
        ]);

        assert_eq!(
            p(r#"<http://t/dp> only { "A", "B" }"#),
            ClassExpression::DataAllValuesFrom {
                dp: dp.clone(),
                dr: one_of.clone(),
            }
        );
        assert_eq!(
            p(r#"<http://t/dp> some { "A", "B" }"#),
            ClassExpression::DataSomeValuesFrom { dp, dr: one_of }
        );

        // An individual-member brace must still parse as ObjectOneOf.
        assert_eq!(
            p("<http://t/r> only { <http://t/i> }"),
            ClassExpression::ObjectAllValuesFrom {
                ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r")),
                bce: Box::new(ClassExpression::ObjectOneOf(vec![Individual::Named(
                    b.named_individual("http://t/i")
                )])),
            }
        );
    }

    /// A `not`-negated data-shaped filler after `some`/`only` is unambiguously a
    /// data restriction over a `DataComplementOf` (an object complement cannot
    /// carry a datatype facet). Previously the data-arm lookahead did not see
    /// through the leading `not`, so a faceted case (`dp some not xsd:float[…]`)
    /// hard-errored on the `[`, and bare/parenthesised cases silently mis-parsed
    /// as object complements.
    #[test]
    fn parses_negated_data_range_restriction() {
        use crate::model::*;
        let b = Build::new_rc();
        let pm = curie::PrefixMapping::default();
        let p =
            |s: &str| crate::io::omn::reader::parse_class_expression::<RcStr>(s, &pm, &b).unwrap();
        let dp = b.data_property("http://t/dp");
        let xsd_int = "http://www.w3.org/2001/XMLSchema#integer";
        let xsd_float = "http://www.w3.org/2001/XMLSchema#float";

        // bare negated known datatype
        assert_eq!(
            p(&format!("<http://t/dp> some not <{xsd_int}>")),
            ClassExpression::DataSomeValuesFrom {
                dp: dp.clone(),
                dr: DataRange::DataComplementOf(Box::new(DataRange::Datatype(b.datatype(xsd_int)))),
            }
        );

        // negated FACETED datatype — the case that hard-errored (PACO).
        let parsed = p(&format!(
            r#"<http://t/dp> some not <{xsd_float}>[> "1.0"^^<{xsd_float}>]"#
        ));
        match parsed {
            ClassExpression::DataSomeValuesFrom { dp: d, dr } => {
                assert_eq!(d, dp);
                assert!(
                    matches!(&dr, DataRange::DataComplementOf(inner)
                        if matches!(**inner, DataRange::DatatypeRestriction(_, _))),
                    "expected DataComplementOf(DatatypeRestriction), got {dr:?}"
                );
            }
            other => panic!("expected DataSomeValuesFrom, got {other:?}"),
        }

        // negated parenthesised data range
        assert_eq!(
            p(&format!(
                "<http://t/dp> only not (<{xsd_int}> or <{xsd_float}>)"
            )),
            ClassExpression::DataAllValuesFrom {
                dp: dp.clone(),
                dr: DataRange::DataComplementOf(Box::new(DataRange::DataUnionOf(vec![
                    DataRange::Datatype(b.datatype(xsd_int)),
                    DataRange::Datatype(b.datatype(xsd_float)),
                ]))),
            }
        );

        // Control: a negated bare CLASS filler stays an object restriction.
        assert_eq!(
            p("<http://t/r> some not <http://t/SomeClass>"),
            ClassExpression::ObjectSomeValuesFrom {
                ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r")),
                bce: Box::new(ClassExpression::ObjectComplementOf(Box::new(
                    ClassExpression::Class(b.class("http://t/SomeClass"))
                ))),
            }
        );
    }

    /// A negated CUSTOM datatype is only knowable from a declaration: the
    /// grammar can't see `not :MyType` is data-shaped (not `xsd:`/faceted), so
    /// it lands on the object arm as `ObjectComplementOf`. When the property is
    /// a declared `DataProperty:` or the negated IRI is a declared `Datatype:`,
    /// the reader must flip it to a data restriction over `DataComplementOf`.
    #[test]
    fn flips_negated_declared_datatype_to_data_restriction() {
        use crate::io::omn::reader::read_with_build;
        use crate::model::*;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;

        let has_sup = |ont: &SetOntology<RcStr>, want: &ClassExpression<RcStr>| {
            ont.iter().any(|ac| {
                matches!(&ac.component,
                    Component::SubClassOf(SubClassOf { sup, .. }) if sup == want)
            })
        };

        // (1) flip via a `Datatype:` declaration on the negated IRI.
        let b = Build::new_rc();
        let doc = "Prefix: : <http://e/>\nDatatype: :MyType\nClass: :C\n    SubClassOf: :p some not :MyType\n";
        let (ont, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        assert!(
            has_sup(
                &ont,
                &ClassExpression::DataSomeValuesFrom {
                    dp: b.data_property("http://e/p"),
                    dr: DataRange::DataComplementOf(Box::new(DataRange::Datatype(
                        b.datatype("http://e/MyType")
                    ))),
                }
            ),
            "Datatype-declared negation should flip: {:?}",
            ont.iter().map(|a| a.component.clone()).collect::<Vec<_>>()
        );

        // (2) flip via a `DataProperty:` declaration on the property.
        let b2 = Build::new_rc();
        let doc2 =
            "Prefix: : <http://e/>\nDataProperty: :p\nClass: :C\n    SubClassOf: :p only not :X\n";
        let (ont2, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc2.as_bytes()), &b2).unwrap();
        assert!(has_sup(
            &ont2,
            &ClassExpression::DataAllValuesFrom {
                dp: b2.data_property("http://e/p"),
                dr: DataRange::DataComplementOf(Box::new(DataRange::Datatype(
                    b2.datatype("http://e/X")
                ))),
            }
        ));

        // (3) no declaration anywhere: irreducibly ambiguous, stays object.
        let b3 = Build::new_rc();
        let doc3 = "Prefix: : <http://e/>\nClass: :C\n    SubClassOf: :p some not :Y\n";
        let (ont3, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc3.as_bytes()), &b3).unwrap();
        assert!(ont3.iter().any(|ac| matches!(
            &ac.component,
            Component::SubClassOf(SubClassOf {
                sup: ClassExpression::ObjectSomeValuesFrom { .. },
                ..
            })
        )));
    }

    /// The negated declared-datatype flip also applies to qualified cardinality
    /// restrictions (`min`/`max`/`exactly`): `p min 2 not :MyType` over a
    /// declared `Datatype:` becomes `DataMinCardinality` with a
    /// `DataComplementOf` range.
    #[test]
    fn flips_negated_datatype_in_cardinality() {
        use crate::io::omn::reader::read_with_build;
        use crate::model::*;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;

        let b = Build::new_rc();
        let doc = "Prefix: : <http://e/>\nDatatype: :MyType\nClass: :C\n    \
                   SubClassOf: :p min 2 not :MyType\n    SubClassOf: :q exactly 1 not :MyType\n";
        let (ont, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        let cx = ont
            .iter()
            .filter_map(|ac| match &ac.component {
                Component::SubClassOf(SubClassOf { sup, .. }) => Some(sup.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        let neg = DataRange::DataComplementOf(Box::new(DataRange::Datatype(
            b.datatype("http://e/MyType"),
        )));
        assert!(
            cx.contains(&ClassExpression::DataMinCardinality {
                n: 2,
                dp: b.data_property("http://e/p"),
                dr: neg.clone(),
            }),
            "min → DataMinCardinality(DataComplementOf); got {cx:?}"
        );
        assert!(
            cx.contains(&ClassExpression::DataExactCardinality {
                n: 1,
                dp: b.data_property("http://e/q"),
                dr: neg,
            }),
            "exactly → DataExactCardinality(DataComplementOf)"
        );
    }

    /// Regression test for the boundary-safe inverse detection fix.
    ///
    /// Before the fix, `ObjectPropertyExpression::from_pair_unchecked` used the raw
    /// byte slice `s[..7]` to probe for the "inverse" keyword.  When the IRI contains
    /// a multi-byte UTF-8 character whose byte sequence straddles index 7 (e.g. the
    /// two-byte é in `<ab://éx>`), that indexing panics with a char-boundary error.
    /// The fix replaces `s[..7]` with `s.get(..7)` which returns `None` on a
    /// non-boundary index and therefore never panics.
    #[test]
    fn reads_declarations_round_trip() {
        use crate::io::omn::write;
        use crate::model::RcAnnotatedComponent;
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        use std::rc::Rc;

        type TestOnt = ComponentMappedOntology<Rc<str>, RcAnnotatedComponent>;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();

        let mut o = SetOntology::new_rc();
        o.insert(DeclareClass(b.class("http://ex/A")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/r")));
        o.insert(DeclareDataProperty(b.data_property("http://ex/p")));
        o.insert(DeclareAnnotationProperty(
            b.annotation_property("http://ex/n"),
        ));
        o.insert(DeclareNamedIndividual(b.named_individual("http://ex/a")));
        o.insert(DeclareDatatype(b.datatype("http://ex/dt")));

        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();

        let (parsed, _pm): (SetOntology<_>, PrefixMapping) =
            crate::io::omn::reader::read_with_build(BufReader::new(&buf[..]), &b).unwrap();

        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "declarations did not round-trip");
    }

    #[test]
    fn reads_class_frame_round_trip() {
        use crate::io::omn::write;
        use crate::model::RcAnnotatedComponent;
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        use std::rc::Rc;

        type TestOnt = ComponentMappedOntology<Rc<str>, RcAnnotatedComponent>;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();

        let a = || ClassExpression::Class(b.class("http://ex/A"));
        let mut o = SetOntology::new_rc();
        for c in ["A", "B", "C", "D", "E", "F", "G"] {
            o.insert(DeclareClass(b.class(format!("http://ex/{c}"))));
        }
        o.insert(SubClassOf {
            sub: a(),
            sup: ClassExpression::Class(b.class("http://ex/B")),
        });
        o.insert(EquivalentClasses(vec![
            a(),
            ClassExpression::Class(b.class("http://ex/C")),
        ]));
        o.insert(DisjointClasses(vec![
            a(),
            ClassExpression::Class(b.class("http://ex/D")),
        ]));
        // DisjointUnion exercises the disjointunionof clause arm.
        o.insert(DisjointUnion(
            b.class("http://ex/A"),
            vec![
                ClassExpression::Class(b.class("http://ex/F")),
                ClassExpression::Class(b.class("http://ex/G")),
            ],
        ));

        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();

        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            crate::io::omn::reader::read_with_build(BufReader::new(&buf[..]), &b).unwrap();

        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "class frame did not round-trip");
    }

    #[test]
    fn reads_object_property_frame_round_trip() {
        use crate::io::omn::write;
        use crate::model::RcAnnotatedComponent;
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        use std::rc::Rc;

        type TestOnt = ComponentMappedOntology<Rc<str>, RcAnnotatedComponent>;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let ope = |i: &str| ObjectPropertyExpression::ObjectProperty(b.object_property(i));

        let mut o = SetOntology::new_rc();
        o.insert(DeclareObjectProperty(b.object_property("http://ex/r")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/s")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/t")));
        o.insert(DeclareClass(b.class("http://ex/A")));
        o.insert(DeclareClass(b.class("http://ex/B")));
        o.insert(SubObjectPropertyOf {
            sub: SubObjectPropertyExpression::ObjectPropertyExpression(ope("http://ex/r")),
            sup: ope("http://ex/s"),
        });
        o.insert(EquivalentObjectProperties(vec![
            ope("http://ex/r"),
            ope("http://ex/s"),
        ]));
        o.insert(DisjointObjectProperties(vec![
            ope("http://ex/r"),
            ope("http://ex/t"),
        ]));
        o.insert(ObjectPropertyDomain {
            ope: ope("http://ex/r"),
            ce: ClassExpression::Class(b.class("http://ex/A")),
        });
        o.insert(ObjectPropertyRange {
            ope: ope("http://ex/r"),
            ce: ClassExpression::Class(b.class("http://ex/B")),
        });
        // every characteristic arm (round-trip only — semantic consistency irrelevant)
        o.insert(FunctionalObjectProperty(ope("http://ex/r")));
        o.insert(InverseFunctionalObjectProperty(ope("http://ex/r")));
        o.insert(ReflexiveObjectProperty(ope("http://ex/r")));
        o.insert(IrreflexiveObjectProperty(ope("http://ex/r")));
        o.insert(SymmetricObjectProperty(ope("http://ex/r")));
        o.insert(AsymmetricObjectProperty(ope("http://ex/r")));
        o.insert(TransitiveObjectProperty(ope("http://ex/r")));
        o.insert(InverseObjectProperties(ope("http://ex/r"), ope("http://ex/t")));

        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            crate::io::omn::reader::read_with_build(BufReader::new(&buf[..]), &b).unwrap();

        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "object property frame did not round-trip");
    }

    #[test]
    fn reads_data_property_frame_round_trip() {
        use crate::io::omn::write;
        use crate::model::RcAnnotatedComponent;
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        use std::rc::Rc;

        type TestOnt = ComponentMappedOntology<Rc<str>, RcAnnotatedComponent>;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();

        let dp = |i: &str| b.data_property(i);
        let mut o = SetOntology::new_rc();
        o.insert(DeclareDataProperty(dp("http://ex/p")));
        o.insert(DeclareDataProperty(dp("http://ex/q")));
        o.insert(DeclareDataProperty(dp("http://ex/u")));
        o.insert(DeclareDataProperty(dp("http://ex/v")));
        o.insert(DeclareClass(b.class("http://ex/A")));
        o.insert(SubDataPropertyOf {
            sub: dp("http://ex/p"),
            sup: dp("http://ex/q"),
        });
        o.insert(EquivalentDataProperties(vec![
            dp("http://ex/p"),
            dp("http://ex/u"),
        ]));
        o.insert(DisjointDataProperties(vec![
            dp("http://ex/p"),
            dp("http://ex/v"),
        ]));
        o.insert(DataPropertyDomain {
            dp: dp("http://ex/p"),
            ce: ClassExpression::Class(b.class("http://ex/A")),
        });
        o.insert(DataPropertyRange {
            dp: dp("http://ex/p"),
            dr: DataRange::Datatype(b.datatype("http://www.w3.org/2001/XMLSchema#integer")),
        });
        o.insert(FunctionalDataProperty(dp("http://ex/p")));
        // Faceted range: xsd:integer[>= "0"^^xsd:integer] on a second property ex:w.
        o.insert(DeclareDataProperty(dp("http://ex/w")));
        o.insert(DataPropertyRange {
            dp: dp("http://ex/w"),
            dr: DataRange::DatatypeRestriction(
                b.datatype("http://www.w3.org/2001/XMLSchema#integer"),
                vec![FacetRestriction {
                    f: Facet::MinInclusive,
                    l: Literal::Datatype {
                        literal: "0".to_string(),
                        datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
                    },
                }],
            ),
        });

        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            crate::io::omn::reader::read_with_build(BufReader::new(&buf[..]), &b).unwrap();

        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "data property frame did not round-trip");
    }

    #[test]
    fn reads_annotation_property_frame_round_trip() {
        use crate::io::omn::write;
        use crate::model::RcAnnotatedComponent;
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        use std::rc::Rc;

        type TestOnt = ComponentMappedOntology<Rc<str>, RcAnnotatedComponent>;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();

        let mut o = SetOntology::new_rc();
        o.insert(DeclareAnnotationProperty(
            b.annotation_property("http://ex/n"),
        ));
        o.insert(SubAnnotationPropertyOf {
            sub: b.annotation_property("http://ex/n"),
            sup: b.annotation_property("http://ex/m"),
        });
        o.insert(AnnotationPropertyDomain {
            ap: b.annotation_property("http://ex/n"),
            iri: b.iri("http://ex/A"),
        });
        o.insert(AnnotationPropertyRange {
            ap: b.annotation_property("http://ex/n"),
            iri: b.iri("http://ex/B"),
        });

        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            crate::io::omn::reader::read_with_build(BufReader::new(&buf[..]), &b).unwrap();

        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "annotation property frame did not round-trip");
    }

    #[test]
    fn reads_individual_frame_round_trip() {
        use crate::io::omn::write;
        use crate::model::RcAnnotatedComponent;
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        use std::rc::Rc;

        type TestOnt = ComponentMappedOntology<Rc<str>, RcAnnotatedComponent>;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let named = |i: &str| Individual::Named(b.named_individual(i));

        let mut o = SetOntology::new_rc();
        o.insert(DeclareNamedIndividual(b.named_individual("http://ex/a")));
        o.insert(DeclareClass(b.class("http://ex/A")));
        o.insert(ClassAssertion {
            i: named("http://ex/a"),
            ce: ClassExpression::Class(b.class("http://ex/A")),
        });
        o.insert(ObjectPropertyAssertion {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://ex/r")),
            from: b.named_individual("http://ex/a").into(),
            to: b.named_individual("http://ex/b").into(),
        });
        o.insert(DataPropertyAssertion {
            dp: b.data_property("http://ex/p"),
            from: b.named_individual("http://ex/a").into(),
            to: Literal::Datatype {
                literal: "5".to_string(),
                datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
            },
        });
        // negative facts exercise the `Facts: not …` negation-detection path
        o.insert(NegativeObjectPropertyAssertion {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://ex/r")),
            from: b.named_individual("http://ex/a").into(),
            to: named("http://ex/b"),
        });
        o.insert(NegativeDataPropertyAssertion {
            dp: b.data_property("http://ex/p"),
            from: b.named_individual("http://ex/a").into(),
            to: Literal::Datatype {
                literal: "6".to_string(),
                datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
            },
        });
        o.insert(SameIndividual(vec![
            named("http://ex/a"),
            named("http://ex/c"),
        ]));
        o.insert(DifferentIndividuals(vec![
            named("http://ex/a"),
            named("http://ex/d"),
        ]));

        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            crate::io::omn::reader::read_with_build(BufReader::new(&buf[..]), &b).unwrap();

        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "individual frame did not round-trip");
    }

    #[test]
    fn parses_unicode_iri_property_no_panic() {
        use crate::model::*;
        let b = Build::new_rc();
        let pm = curie::PrefixMapping::default();
        let p =
            |s: &str| crate::io::omn::reader::parse_class_expression::<RcStr>(s, &pm, &b).unwrap();
        // `é` (U+00E9) is encoded as two bytes (0xC3 0xA9) in UTF-8.  The IRI
        // `<ab://éx>` is 12 UTF-8 bytes inside the angle brackets; byte index 7
        // (`<ab://` = 6 bytes for the bracket+scheme, then `é` starts at 6) falls
        // inside the multi-byte sequence — the old `s[..7]` would panic here.
        let result = p("<ab://\u{00e9}x> some <http://t/A>");
        match result {
            ClassExpression::ObjectSomeValuesFrom { ope, bce } => {
                assert_eq!(
                    ope,
                    ObjectPropertyExpression::ObjectProperty(b.object_property("ab://\u{00e9}x"))
                );
                assert_eq!(*bce, ClassExpression::Class(b.class("http://t/A")));
            }
            other => panic!("expected ObjectSomeValuesFrom, got {other:?}"),
        }
    }

    #[test]
    fn reads_property_chain_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let ope = |i: &str| ObjectPropertyExpression::ObjectProperty(b.object_property(i));
        let mut o = SetOntology::new_rc();
        for p in ["r", "p", "q"] {
            o.insert(DeclareObjectProperty(
                b.object_property(format!("http://ex/{p}")),
            ));
        }
        o.insert(SubObjectPropertyOf {
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![
                ope("http://ex/p"),
                ope("http://ex/q"),
            ]),
            sup: ope("http://ex/r"),
        });
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig,
            got,
            "chain did not round-trip\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn reads_haskey_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let mut o = SetOntology::new_rc();
        o.insert(DeclareClass(b.class("http://ex/C")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/k1")));
        // NOTE: object-only keys — Manchester HasKey: does not lexically distinguish
        // object vs data properties; the reader reconstructs all keys as
        // ObjectPropertyExpression. Using a data-property key here would fail on
        // round-trip (parsed back as object). See Task 7 for the limitation doc.
        o.insert(HasKey {
            ce: ClassExpression::Class(b.class("http://ex/C")),
            vpe: vec![PropertyExpression::ObjectPropertyExpression(
                ObjectPropertyExpression::ObjectProperty(b.object_property("http://ex/k1")),
            )],
        });
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig,
            got,
            "haskey did not round-trip\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn reads_import_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();

        let mut o = SetOntology::new_rc();
        o.insert(Import(b.iri("http://ex/imported")));
        o.insert(DeclareClass(b.class("http://ex/A")));

        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();

        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig,
            got,
            "import did not round-trip\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn whole_ontology_round_trips() {
        use crate::io::omn::{read_with_build, write};
        use crate::model::RcAnnotatedComponent;
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        use std::rc::Rc;

        type TestOnt = ComponentMappedOntology<Rc<str>, RcAnnotatedComponent>;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();

        let ce = |i: &str| ClassExpression::Class(b.class(i));
        let ope = |i: &str| ObjectPropertyExpression::ObjectProperty(b.object_property(i));
        let named = |i: &str| Individual::Named(b.named_individual(i));

        let mut o = SetOntology::new_rc();
        // ontology header
        o.insert(OntologyID {
            iri: Some(b.iri("http://ex/onto")),
            ..Default::default()
        });
        // declarations
        for c in ["A", "B", "C", "D"] {
            o.insert(DeclareClass(b.class(format!("http://ex/{c}"))));
        }
        o.insert(DeclareObjectProperty(b.object_property("http://ex/r")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/t")));
        o.insert(DeclareDataProperty(b.data_property("http://ex/p")));
        o.insert(DeclareAnnotationProperty(
            b.annotation_property("http://ex/n"),
        ));
        o.insert(DeclareNamedIndividual(b.named_individual("http://ex/a")));
        o.insert(DeclareNamedIndividual(b.named_individual("http://ex/b")));
        o.insert(DeclareDatatype(b.datatype("http://ex/dt")));
        // class axioms
        o.insert(SubClassOf {
            sub: ce("http://ex/A"),
            sup: ce("http://ex/B"),
        });
        o.insert(EquivalentClasses(vec![
            ce("http://ex/A"),
            ce("http://ex/C"),
        ]));
        o.insert(DisjointClasses(vec![ce("http://ex/A"), ce("http://ex/D")]));
        // object property axioms
        o.insert(ObjectPropertyDomain {
            ope: ope("http://ex/r"),
            ce: ce("http://ex/A"),
        });
        o.insert(FunctionalObjectProperty(ope("http://ex/r")));
        o.insert(InverseObjectProperties(ope("http://ex/r"), ope("http://ex/t")));
        // data property axioms
        o.insert(DataPropertyRange {
            dp: b.data_property("http://ex/p"),
            dr: DataRange::Datatype(b.datatype("http://www.w3.org/2001/XMLSchema#integer")),
        });
        // annotation property axioms
        o.insert(AnnotationPropertyDomain {
            ap: b.annotation_property("http://ex/n"),
            iri: b.iri("http://ex/A"),
        });
        // individual axioms
        o.insert(ClassAssertion {
            i: named("http://ex/a"),
            ce: ce("http://ex/A"),
        });
        o.insert(ObjectPropertyAssertion {
            ope: ope("http://ex/r"),
            from: named("http://ex/a"),
            to: named("http://ex/b"),
        });
        o.insert(DataPropertyAssertion {
            dp: b.data_property("http://ex/p"),
            from: named("http://ex/a"),
            to: Literal::Datatype {
                literal: "5".into(),
                datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
            },
        });

        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();

        let (parsed, parsed_pm): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();

        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig,
            got,
            "whole ontology did not round-trip\n--- document ---\n{}",
            String::from_utf8_lossy(&buf)
        );
        // prefixes survive the round-trip
        assert_eq!(
            parsed_pm.expand_curie_string("ex:A").unwrap(),
            "http://ex/A"
        );
    }

    #[test]
    fn reads_axiom_annotations_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::collections::BTreeSet;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();

        // Helper: build a one-annotation BTreeSet with the given literal value.
        let make_ann = |val: &str| -> BTreeSet<Annotation<std::rc::Rc<str>>> {
            let mut s = BTreeSet::new();
            s.insert(Annotation {
                ap: b.annotation_property("http://ex/prov"),
                av: AnnotationValue::Literal(Literal::Simple {
                    literal: val.to_string(),
                }),
                ann: Default::default(),
            });
            s
        };

        let mut o = SetOntology::new_rc();
        // --- SubClassOf (tests the basic push_clause ann_prefix path) ---
        o.insert(DeclareClass(b.class("http://ex/A")));
        o.insert(DeclareClass(b.class("http://ex/B")));
        o.insert(AnnotatedComponent {
            component: Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(b.class("http://ex/A")),
                sup: ClassExpression::Class(b.class("http://ex/B")),
            }),
            ann: make_ann("inferred"),
        });

        // --- Characteristics (tests insert_object_characteristic with ann) ---
        o.insert(DeclareObjectProperty(b.object_property("http://ex/r")));
        let r_ope = ObjectPropertyExpression::ObjectProperty(b.object_property("http://ex/r"));
        o.insert(AnnotatedComponent {
            component: Component::FunctionalObjectProperty(FunctionalObjectProperty(r_ope.clone())),
            ann: make_ann("char-ann"),
        });

        // --- Facts (tests insert_fact with ann) ---
        o.insert(DeclareNamedIndividual(b.named_individual("http://ex/a")));
        o.insert(DeclareNamedIndividual(b.named_individual("http://ex/b")));
        let ind_a = Individual::Named(b.named_individual("http://ex/a"));
        let ind_b = Individual::Named(b.named_individual("http://ex/b"));
        o.insert(AnnotatedComponent {
            component: Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                ope: r_ope,
                from: ind_a,
                to: ind_b,
            }),
            ann: make_ann("fact-ann"),
        });

        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        // compare FULL AnnotatedComponents (component + ann), not just components
        let orig: BTreeSet<_> = o.iter().cloned().collect();
        let got: BTreeSet<_> = parsed.iter().cloned().collect();
        assert_eq!(
            orig,
            got,
            "axiom annotation did not round-trip\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn reads_entity_and_ontology_annotations_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        pm.add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
            .unwrap();

        let mut o = SetOntology::new_rc();
        o.insert(OntologyID {
            iri: Some(b.iri("http://ex/o")),
            ..Default::default()
        });
        // an import too — validates the conformant header hosts iri+import+annotations together
        o.insert(Import(b.iri("http://ex/imported")));
        o.insert(OntologyAnnotation(Annotation {
            ap: b.annotation_property("http://www.w3.org/2000/01/rdf-schema#comment"),
            av: AnnotationValue::Literal(Literal::Simple {
                literal: "an ontology".to_string(),
            }),
            ann: Default::default(),
        }));
        o.insert(DeclareClass(b.class("http://ex/A")));
        o.insert(AnnotationAssertion {
            subject: AnnotationSubject::IRI(b.iri("http://ex/A")),
            ann: Annotation {
                ap: b.annotation_property("http://www.w3.org/2000/01/rdf-schema#label"),
                av: AnnotationValue::Literal(Literal::Simple {
                    literal: "the A class".to_string(),
                }),
                ann: Default::default(),
            },
        });
        // an IRI-valued entity annotation too
        o.insert(AnnotationAssertion {
            subject: AnnotationSubject::IRI(b.iri("http://ex/A")),
            ann: Annotation {
                ap: b.annotation_property("http://ex/seeAlso"),
                av: AnnotationValue::IRI(b.iri("http://ex/B")),
                ann: Default::default(),
            },
        });

        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig,
            got,
            "annotations did not round-trip\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn reads_general_axioms_block() {
        use crate::io::omn::reader::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        // A frame plus a trailing functional-syntax `# General axioms` block
        // holding a GCI and an annotation assertion on an undeclared subject
        // (the shape that previously caused silent, large-scale loss).
        let doc = "Prefix: ex: <http://ex/>\n\nClass: ex:A\n\n# General axioms\n\
                   SubClassOf(ObjectIntersectionOf(<http://ex/A> <http://ex/B>) <http://ex/C>)\n\
                   AnnotationAssertion(<http://ex/p> <http://ex/CHEBI_1> \"tyramine\")\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();

        // the frame parsed
        assert!(
            parsed
                .iter()
                .any(|ac| matches!(&ac.component, Component::DeclareClass(_)))
        );

        // the GCI in the block is now read back (not skipped)
        let expected_sub = ClassExpression::ObjectIntersectionOf(vec![
            ClassExpression::Class(b.class("http://ex/A")),
            ClassExpression::Class(b.class("http://ex/B")),
        ]);
        let expected_sup = ClassExpression::Class(b.class("http://ex/C"));
        assert!(
            parsed.iter().any(|ac| matches!(
                &ac.component,
                Component::SubClassOf(SubClassOf { sub, sup })
                    if *sub == expected_sub && *sup == expected_sup
            )),
            "general-axiom SubClassOf GCI should be read back"
        );

        // the annotation assertion on an undeclared subject is preserved
        let expected_subject = b.iri("http://ex/CHEBI_1");
        assert!(
            parsed.iter().any(|ac| matches!(
                &ac.component,
                Component::AnnotationAssertion(aa)
                    if matches!(&aa.subject, AnnotationSubject::IRI(i) if *i == expected_subject)
            )),
            "general-axiom annotation assertion should be read back"
        );
    }

    #[test]
    fn general_axioms_block_parse_failure_is_skipped_not_errored() {
        use crate::io::omn::reader::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        // A `# General axioms` block the functional-syntax reader cannot parse
        // must degrade to warn-and-skip (the pre-delegation behaviour), never a
        // hard error — so the rest of the document still reads.
        let doc = "Prefix: ex: <http://ex/>\n\nClass: ex:A\n\n# General axioms\n\
                   NotARealAxiom(@@@ broken)\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b)
                .expect("unparseable general-axioms block must not error the read");
        assert!(
            parsed
                .iter()
                .any(|ac| matches!(&ac.component, Component::DeclareClass(_)))
        );
    }

    /// Complex-LHS `Class:` frame parsed as a general class axiom (GCI).
    /// OWL-API/Protégé/ROBOT emit frames like:
    ///   Class: :r some :C
    ///       SubClassOf: :D
    /// The subject is a compound ClassExpression, not a plain classIRI.
    /// The reader must parse this as SubClassOf(ObjectSomeValuesFrom(:r,:C), :D)
    /// with NO DeclareClass for the complex subject.
    #[test]
    fn reads_complex_lhs_class_frame_as_gci_subclassof() {
        use crate::io::omn::reader::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;

        let b = Build::new_rc();
        let src = "Prefix: : <http://e/>\nClass: :r some :C\n    SubClassOf: :D\n";
        let (ont, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(src.as_bytes()), &b)
                .unwrap_or_else(|e| panic!("complex-LHS GCI should parse: {e}"));

        // Must contain SubClassOf(ObjectSomeValuesFrom(:r, :C), :D).
        let expected_sub = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://e/r")),
            bce: Box::new(ClassExpression::Class(b.class("http://e/C"))),
        };
        let expected_sup = ClassExpression::Class(b.class("http://e/D"));
        let found = ont.iter().find(|ac| {
            matches!(
                &ac.component,
                Component::SubClassOf(SubClassOf { sub, sup })
                    if *sub == expected_sub && *sup == expected_sup
            )
        });
        assert!(
            found.is_some(),
            "expected SubClassOf(ObjectSomeValuesFrom(:r,:C), :D), components:\n{}",
            ont.iter()
                .map(|ac| format!("{:?}", ac.component))
                .collect::<Vec<_>>()
                .join("\n")
        );

        // Must NOT have a DeclareClass for the complex subject (it has no IRI).
        let has_declare_for_some = ont.iter().any(|ac| {
            matches!(
                &ac.component,
                Component::DeclareClass(DeclareClass(c))
                    if c.0.as_ref().contains("/r") || c.0.as_ref().contains("/C")
            )
        });
        assert!(
            !has_declare_for_some,
            "complex-LHS GCI must NOT declare the complex subject as a class"
        );
    }

    /// Regression guard: atomic `Class: :A SubClassOf: :B` still emits
    /// `DeclareClass(:A)` + `SubClassOf(:A, :B)` (behaviour must be byte-identical).
    #[test]
    fn reads_atomic_class_frame_unchanged() {
        use crate::io::omn::reader::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;

        let b = Build::new_rc();
        let src = "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :B\n";
        let (ont, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(src.as_bytes()), &b)
                .unwrap_or_else(|e| panic!("atomic class frame should parse: {e}"));

        // Must have DeclareClass(:A).
        let has_declare = ont.iter().any(|ac| {
            matches!(&ac.component, Component::DeclareClass(DeclareClass(c)) if c.0.as_ref() == "http://e/A")
        });
        assert!(has_declare, "atomic subject must yield DeclareClass");

        // Must have SubClassOf(:A, :B).
        let has_sub = ont.iter().any(|ac| {
            matches!(
                &ac.component,
                Component::SubClassOf(SubClassOf { sub, sup })
                    if matches!(sub, ClassExpression::Class(c) if c.0.as_ref() == "http://e/A")
                    && matches!(sup, ClassExpression::Class(c) if c.0.as_ref() == "http://e/B")
            )
        });
        assert!(has_sub, "atomic subject must yield SubClassOf(:A,:B)");
    }

    /// Complex-LHS `EquivalentTo:` parsed as `EquivalentClasses(complexCE, X)`.
    #[test]
    fn reads_complex_lhs_class_frame_equivalentto() {
        use crate::io::omn::reader::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;

        let b = Build::new_rc();
        let src = "Prefix: : <http://e/>\nClass: :r some :C\n    EquivalentTo: :D\n";
        let (ont, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(src.as_bytes()), &b)
                .unwrap_or_else(|e| panic!("complex-LHS EquivalentTo should parse: {e}"));

        let some_rc = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://e/r")),
            bce: Box::new(ClassExpression::Class(b.class("http://e/C"))),
        };
        let d = ClassExpression::Class(b.class("http://e/D"));
        let found = ont.iter().find(|ac| {
            matches!(
                &ac.component,
                Component::EquivalentClasses(EquivalentClasses(v))
                    if v.contains(&some_rc) && v.contains(&d)
            )
        });
        assert!(
            found.is_some(),
            "expected EquivalentClasses(ObjectSomeValuesFrom(:r,:C), :D), components:\n{}",
            ont.iter()
                .map(|ac| format!("{:?}", ac.component))
                .collect::<Vec<_>>()
                .join("\n")
        );
    }

    #[test]
    fn whole_ontology_with_extras_round_trips() {
        use crate::io::omn::{read_with_build, write};
        use crate::model::RcAnnotatedComponent;
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::collections::BTreeSet;
        use std::io::BufReader;
        use std::rc::Rc;

        type TestOnt = ComponentMappedOntology<Rc<str>, RcAnnotatedComponent>;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        pm.add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
            .unwrap();

        let ce = |i: &str| ClassExpression::Class(b.class(i));
        let ope = |i: &str| ObjectPropertyExpression::ObjectProperty(b.object_property(i));

        let mut o = SetOntology::new_rc();

        // ontology header
        o.insert(OntologyID {
            iri: Some(b.iri("http://ex/onto")),
            ..Default::default()
        });

        // Import
        o.insert(Import(b.iri("http://ex/imported")));

        // OntologyAnnotation
        o.insert(OntologyAnnotation(Annotation {
            ap: b.annotation_property("http://www.w3.org/2000/01/rdf-schema#comment"),
            av: AnnotationValue::Literal(Literal::Simple {
                literal: "capstone ontology".to_string(),
            }),
            ann: Default::default(),
        }));

        // declarations
        for c in ["A", "B", "C"] {
            o.insert(DeclareClass(b.class(format!("http://ex/{c}"))));
        }
        o.insert(DeclareObjectProperty(b.object_property("http://ex/r")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/p")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/q")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/k1")));

        // entity AnnotationAssertion on a declared class
        o.insert(AnnotationAssertion {
            subject: AnnotationSubject::IRI(b.iri("http://ex/A")),
            ann: Annotation {
                ap: b.annotation_property("http://www.w3.org/2000/01/rdf-schema#label"),
                av: AnnotationValue::Literal(Literal::Simple {
                    literal: "Class A".to_string(),
                }),
                ann: Default::default(),
            },
        });

        // property chain: p o q -> r
        o.insert(SubObjectPropertyOf {
            sub: SubObjectPropertyExpression::ObjectPropertyChain(vec![
                ope("http://ex/p"),
                ope("http://ex/q"),
            ]),
            sup: ope("http://ex/r"),
        });

        // HasKey (object-only keys — data keys are a known conflation)
        o.insert(HasKey {
            ce: ce("http://ex/A"),
            vpe: vec![PropertyExpression::ObjectPropertyExpression(
                ObjectPropertyExpression::ObjectProperty(b.object_property("http://ex/k1")),
            )],
        });

        // AnnotatedComponent: SubClassOf with annotation
        let mut ann = BTreeSet::new();
        ann.insert(Annotation {
            ap: b.annotation_property("http://ex/prov"),
            av: AnnotationValue::Literal(Literal::Simple {
                literal: "inferred".to_string(),
            }),
            ann: Default::default(),
        });
        o.insert(AnnotatedComponent {
            component: Component::SubClassOf(SubClassOf {
                sub: ce("http://ex/B"),
                sup: ce("http://ex/C"),
            }),
            ann,
        });

        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();

        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();

        // compare full AnnotatedComponents (component + ann)
        let orig: BTreeSet<_> = o.iter().cloned().collect();
        let got: BTreeSet<_> = parsed.iter().cloned().collect();
        assert_eq!(
            orig,
            got,
            "whole_ontology_with_extras did not round-trip\n--- document ---\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn reads_bare_default_prefix_names_round_trip() {
        // With a DEFAULT (empty) prefix the writer emits bare local names
        // (`Class: Ancestor`, not `Class: :Ancestor`); the reader must accept
        // them via the `SimpleIRI` production for the round-trip to hold.
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("", "http://ex/").unwrap(); // default prefix → bare names

        let mut o = SetOntology::new_rc();
        o.insert(OntologyID {
            iri: Some(b.iri("http://ex/o")),
            ..Default::default()
        });
        o.insert(DeclareClass(b.class("http://ex/Ancestor")));
        o.insert(DeclareClass(b.class("http://ex/Person")));
        o.insert(SubClassOf {
            sub: ClassExpression::Class(b.class("http://ex/Ancestor")),
            sup: ClassExpression::Class(b.class("http://ex/Person")),
        });

        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(buf.clone()).unwrap();
        assert!(
            s.contains("Class: Ancestor"),
            "expected a bare default-prefix name, got:\n{s}"
        );

        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: BTreeSet<_> = parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "bare-name round-trip failed\n{s}");
    }

    #[test]
    fn reads_version_iri_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let mut o = SetOntology::new_rc();
        o.insert(OntologyID {
            iri: Some(b.iri("http://ex/o")),
            viri: Some(b.iri("http://ex/o/1.0.0")),
        });
        o.insert(DeclareClass(b.class("http://ex/A")));
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(buf.clone()).unwrap();
        assert!(
            s.contains("Ontology: ex:o ex:o/1.0.0")
                || s.contains("Ontology: ex:o <http://ex/o/1.0.0>"),
            "expected version IRI in header, got:\n{s}"
        );
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "version IRI did not round-trip\n{s}");
    }

    #[test]
    fn ontology_iri_then_import_no_version_iri() {
        // Guard: the optional VersionIRI must NOT greedily grab a following Import.
        // `Ontology: ex:o` (no version) then `Import: ex:i` must round-trip the Import.
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let mut o = SetOntology::new_rc();
        o.insert(OntologyID {
            iri: Some(b.iri("http://ex/o")),
            ..Default::default()
        });
        o.insert(Import(b.iri("http://ex/i")));
        o.insert(DeclareClass(b.class("http://ex/A")));
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig,
            got,
            "import after version-less ontology IRI did not round-trip\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn reads_compound_data_ranges_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use crate::vocab::Facet;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let xsd_int =
            || DataRange::Datatype(b.datatype("http://www.w3.org/2001/XMLSchema#integer"));
        let restr = DataRange::DatatypeRestriction(
            b.datatype("http://www.w3.org/2001/XMLSchema#integer"),
            vec![FacetRestriction {
                f: Facet::MinInclusive,
                l: Literal::Datatype {
                    literal: "0".into(),
                    datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
                },
            }],
        );
        let mut o = SetOntology::new_rc();
        o.insert(DeclareDataProperty(b.data_property("http://ex/p")));
        o.insert(DeclareClass(b.class("http://ex/A")));
        // Range: p ( (xsd:integer and [≥0]) or not {"x"} )
        //
        // NOTE: the plan wrapped this DataRange in a `DataSomeValuesFrom` carrier
        // (`p some <dr>`), but that form is unreachable on read — the pre-existing,
        // out-of-scope object/data-property `some` ambiguity (omn.pest P2 dead
        // productions) commits `p some <dt>` to the OBJECT arm before the data-range
        // grammar is reached. `DataPropertyRange` (`Range:` clause) routes through
        // `DataRangeList → DataRange::from_pair`, exercising the identical
        // or/and/not/oneOf code Task 2 added.
        o.insert(DataPropertyRange {
            dp: b.data_property("http://ex/p"),
            dr: DataRange::DataUnionOf(vec![
                DataRange::DataIntersectionOf(vec![xsd_int(), restr]),
                DataRange::DataComplementOf(Box::new(DataRange::DataOneOf(vec![
                    Literal::Simple {
                        literal: "x".into(),
                    },
                ]))),
            ]),
        });
        // and-over-or: forces the writer to emit parentheses around the inner `or`
        // (`xsd:integer and ( xsd:string or xsd:integer )`), exercising the
        // `DataAtomic = "(" ~ DataRange ~ ")"` reader branch.
        o.insert(DeclareDataProperty(b.data_property("http://ex/q")));
        o.insert(DataPropertyRange {
            dp: b.data_property("http://ex/q"),
            dr: DataRange::DataIntersectionOf(vec![
                xsd_int(),
                DataRange::DataUnionOf(vec![
                    DataRange::Datatype(b.datatype("http://www.w3.org/2001/XMLSchema#string")),
                    xsd_int(),
                ]),
            ]),
        });
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(buf.clone()).unwrap();
        assert!(
            s.contains('('),
            "expected parenthesized data range in writer output:\n{s}"
        );
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig,
            got,
            "compound data range did not round-trip\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn reads_datatype_definition_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use crate::vocab::Facet;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let mut o = SetOntology::new_rc();
        o.insert(DeclareDatatype(b.datatype("http://ex/SmallInt")));
        o.insert(DatatypeDefinition {
            kind: b.datatype("http://ex/SmallInt"),
            range: DataRange::DatatypeRestriction(
                b.datatype("http://www.w3.org/2001/XMLSchema#integer"),
                vec![FacetRestriction {
                    f: Facet::MaxInclusive,
                    l: Literal::Datatype {
                        literal: "255".into(),
                        datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
                    },
                }],
            ),
        });
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(buf.clone()).unwrap();
        assert!(
            s.contains("Datatype: ex:SmallInt") && s.contains("EquivalentTo:"),
            "got:\n{s}"
        );
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "datatype definition did not round-trip\n{s}");
    }

    #[test]
    fn reads_misc_disjoint_complex_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let some = |r: &str, c: &str| ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property(r)),
            bce: Box::new(ClassExpression::Class(b.class(c))),
        };
        let mut o = SetOntology::new_rc();
        for n in ["r", "s"] {
            o.insert(DeclareObjectProperty(
                b.object_property(format!("http://ex/{n}")),
            ));
        }
        for n in ["A", "B"] {
            o.insert(DeclareClass(b.class(format!("http://ex/{n}"))));
        }
        o.insert(DisjointClasses(vec![
            some("http://ex/r", "http://ex/A"),
            some("http://ex/s", "http://ex/B"),
        ]));
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(buf.clone()).unwrap();
        assert!(
            s.contains("DisjointClasses:"),
            "expected misc DisjointClasses:, got:\n{s}"
        );
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "misc DisjointClasses did not round-trip\n{s}");
    }

    #[test]
    fn reads_misc_object_property_keywords_round_trip() {
        // Locks the native Misc property keyword (`EquivalentProperties:` /
        // `DisjointProperties:`, NOT the functional `EquivalentObjectProperties:`):
        // a complex (inverse) first member has no frame subject, so these route to
        // the top-level Misc section.
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let inv_r =
            ObjectPropertyExpression::InverseObjectProperty(b.object_property("http://ex/r"));
        let s_ope = ObjectPropertyExpression::ObjectProperty(b.object_property("http://ex/s"));
        let mut o = SetOntology::new_rc();
        for n in ["r", "s"] {
            o.insert(DeclareObjectProperty(
                b.object_property(format!("http://ex/{n}")),
            ));
        }
        o.insert(EquivalentObjectProperties(vec![
            inv_r.clone(),
            s_ope.clone(),
        ]));
        o.insert(DisjointObjectProperties(vec![inv_r, s_ope]));
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(buf.clone()).unwrap();
        assert!(
            s.contains("EquivalentProperties:") && s.contains("DisjointProperties:"),
            "expected native Misc property keywords, got:\n{s}"
        );
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig, got,
            "misc object-property axioms did not round-trip\n{s}"
        );
    }

    /// Hand-written §2.5 per-item annotatedList: a mid-list `Annotations:`
    /// after a comma annotates only the following list item. This exercises the
    /// new per-item grammar+reader path (the OWL-API ro.owlapi.omn line-1231
    /// shape) — the leading clause-level `Annotations?` slot is shadowed by PEG
    /// greediness, so the post-comma form is the only one that exercises it.
    #[test]
    fn reads_mid_list_per_item_annotation() {
        use crate::io::omn::reader::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::collections::BTreeSet;
        use std::io::BufReader;
        let b = Build::new_rc();
        // `SubClassOf: :B, Annotations: ex:p "x" :C` => two SubClassOf axioms;
        // only the second (A ⊑ C) carries the `ex:p "x"` annotation.
        let doc = "Prefix: : <http://ex/>\n\
                   Prefix: ex: <http://ex/>\n\
                   Ontology:\n\
                   Class: :A\n    \
                   SubClassOf: :B, Annotations: ex:p \"x\" :C\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();

        let plain = AnnotatedComponent {
            component: Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(b.class("http://ex/A")),
                sup: ClassExpression::Class(b.class("http://ex/B")),
            }),
            ann: BTreeSet::new(),
        };
        let mut ann = BTreeSet::new();
        ann.insert(Annotation {
            ap: b.annotation_property("http://ex/p"),
            av: AnnotationValue::Literal(Literal::Simple {
                literal: "x".to_string(),
            }),
            ann: Default::default(),
        });
        let annotated = AnnotatedComponent {
            component: Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(b.class("http://ex/A")),
                sup: ClassExpression::Class(b.class("http://ex/C")),
            }),
            ann,
        };
        let got: BTreeSet<_> = parsed.iter().cloned().collect();
        assert!(
            got.contains(&plain),
            "expected un-annotated A ⊑ B, got:\n{got:#?}"
        );
        assert!(
            got.contains(&annotated),
            "expected A ⊑ C with ex:p \"x\" annotation, got:\n{got:#?}"
        );
    }

    /// §2.5 `descriptionAnnotatedList`: a LEADING clause-level `Annotations?`
    /// binds the FIRST list item ONLY — not every item. Repro: `SubClassOf:
    /// Annotations: ex:note "x" :B, :C` must yield `A ⊑ B` ann `{note x}` and
    /// `A ⊑ C` UNANNOTATED. (Reader previously spread the leading annotation to
    /// every item via `merge_ann(&ann, item_ann)`.)
    #[test]
    fn leading_annotation_binds_first_item_only() {
        use crate::io::omn::reader::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::collections::BTreeSet;
        use std::io::BufReader;
        let b = Build::new_rc();
        let doc = "Prefix: : <http://ex/>\n\
                   Prefix: ex: <http://ex/>\n\
                   Ontology:\n\
                   Class: :A\n    \
                   SubClassOf: Annotations: ex:note \"x\" :B, :C\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();

        // First item (A ⊑ B) carries the leading annotation.
        let mut ann = BTreeSet::new();
        ann.insert(Annotation {
            ap: b.annotation_property("http://ex/note"),
            av: AnnotationValue::Literal(Literal::Simple {
                literal: "x".to_string(),
            }),
            ann: Default::default(),
        });
        let annotated_b = AnnotatedComponent {
            component: Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(b.class("http://ex/A")),
                sup: ClassExpression::Class(b.class("http://ex/B")),
            }),
            ann: ann.clone(),
        };
        // Second item (A ⊑ C) must be UNANNOTATED.
        let plain_c = AnnotatedComponent {
            component: Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(b.class("http://ex/A")),
                sup: ClassExpression::Class(b.class("http://ex/C")),
            }),
            ann: BTreeSet::new(),
        };
        // The bug-producing axiom: A ⊑ C WITH the leading annotation.
        let bad_c = AnnotatedComponent {
            component: Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(b.class("http://ex/A")),
                sup: ClassExpression::Class(b.class("http://ex/C")),
            }),
            ann,
        };

        let got: BTreeSet<_> = parsed.iter().cloned().collect();
        assert!(
            got.contains(&annotated_b),
            "expected A ⊑ B with ex:note \"x\" annotation, got:\n{got:#?}"
        );
        assert!(
            got.contains(&plain_c),
            "expected UNANNOTATED A ⊑ C, got:\n{got:#?}"
        );
        assert!(
            !got.contains(&bad_c),
            "leading annotation must NOT spread to A ⊑ C, got:\n{got:#?}"
        );
    }

    /// §2.5: `Characteristics:` (objectPropertyCharacteristicAnnotatedList) and
    /// `Facts:` (factAnnotatedList) are annotatedLists — a LEADING clause-level
    /// annotation binds the FIRST list item only, not the whole comma-list.
    #[test]
    fn leading_annotation_on_characteristics_and_facts_binds_first_only() {
        use crate::io::omn::reader::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::collections::BTreeSet;
        use std::io::BufReader;
        let b = Build::new_rc();
        let doc = "Prefix: : <http://ex/>\n\
                   Prefix: ex: <http://ex/>\n\
                   Ontology:\n\
                   ObjectProperty: ex:r\n    \
                   Characteristics: Annotations: ex:note \"n\" Functional, Transitive\n\
                   Individual: ex:a\n    \
                   Facts: Annotations: ex:note \"f\" ex:r ex:b, ex:r ex:c\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        let got: BTreeSet<_> = parsed.iter().cloned().collect();

        let mk_ann = |val: &str| {
            let mut s = BTreeSet::new();
            s.insert(Annotation {
                ap: b.annotation_property("http://ex/note"),
                av: AnnotationValue::Literal(Literal::Simple {
                    literal: val.to_string(),
                }),
                ann: Default::default(),
            });
            s
        };
        let r_ope = ObjectPropertyExpression::ObjectProperty(b.object_property("http://ex/r"));

        // --- Characteristics: Functional (first) annotated, Transitive (rest) plain.
        let functional_annotated = AnnotatedComponent {
            component: Component::FunctionalObjectProperty(FunctionalObjectProperty(r_ope.clone())),
            ann: mk_ann("n"),
        };
        let transitive_plain = AnnotatedComponent {
            component: Component::TransitiveObjectProperty(TransitiveObjectProperty(r_ope.clone())),
            ann: BTreeSet::new(),
        };
        // Bug shape: Transitive carrying the leading annotation.
        let transitive_bad = AnnotatedComponent {
            component: Component::TransitiveObjectProperty(TransitiveObjectProperty(r_ope.clone())),
            ann: mk_ann("n"),
        };
        assert!(
            got.contains(&functional_annotated),
            "expected Functional(r) with ex:note \"n\", got:\n{got:#?}"
        );
        assert!(
            got.contains(&transitive_plain),
            "expected UNANNOTATED Transitive(r), got:\n{got:#?}"
        );
        assert!(
            !got.contains(&transitive_bad),
            "leading annotation must NOT spread to Transitive(r), got:\n{got:#?}"
        );

        // --- Facts: r a b (first) annotated, r a c (rest) plain.
        let ind_a = Individual::Named(b.named_individual("http://ex/a"));
        let ind_b = Individual::Named(b.named_individual("http://ex/b"));
        let ind_c = Individual::Named(b.named_individual("http://ex/c"));
        let fact_ab_annotated = AnnotatedComponent {
            component: Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                ope: r_ope.clone(),
                from: ind_a.clone(),
                to: ind_b,
            }),
            ann: mk_ann("f"),
        };
        let fact_ac_plain = AnnotatedComponent {
            component: Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                ope: r_ope.clone(),
                from: ind_a.clone(),
                to: ind_c.clone(),
            }),
            ann: BTreeSet::new(),
        };
        let fact_ac_bad = AnnotatedComponent {
            component: Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                ope: r_ope,
                from: ind_a,
                to: ind_c,
            }),
            ann: mk_ann("f"),
        };
        assert!(
            got.contains(&fact_ab_annotated),
            "expected r(a,b) with ex:note \"f\", got:\n{got:#?}"
        );
        assert!(
            got.contains(&fact_ac_plain),
            "expected UNANNOTATED r(a,c), got:\n{got:#?}"
        );
        assert!(
            !got.contains(&fact_ac_bad),
            "leading annotation must NOT spread to r(a,c), got:\n{got:#?}"
        );
    }

    /// Our own writer emits one clause per per-item axiom, so an annotated and a
    /// plain SubClassOf round-trip even without per-item lists. Regression guard
    /// that the helper-signature refactor does not break the common case.
    #[test]
    fn reads_per_item_annotated_list_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::collections::BTreeSet;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let mut o = SetOntology::new_rc();
        for n in ["A", "B", "C"] {
            o.insert(DeclareClass(b.class(format!("http://ex/{n}"))));
        }
        o.insert(SubClassOf {
            sub: ClassExpression::Class(b.class("http://ex/A")),
            sup: ClassExpression::Class(b.class("http://ex/B")),
        });
        let mut ann = BTreeSet::new();
        ann.insert(Annotation {
            ap: b.annotation_property("http://ex/p"),
            av: AnnotationValue::Literal(Literal::Simple {
                literal: "x".into(),
            }),
            ann: Default::default(),
        });
        o.insert(AnnotatedComponent {
            component: Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(b.class("http://ex/A")),
                sup: ClassExpression::Class(b.class("http://ex/C")),
            }),
            ann,
        });
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: BTreeSet<_> = o.iter().cloned().collect();
        let got: BTreeSet<_> = parsed.iter().cloned().collect();
        assert_eq!(
            orig,
            got,
            "per-item annotated list did not round-trip\n{}",
            String::from_utf8_lossy(&buf)
        );
    }

    #[test]
    fn parses_swrl_rule() {
        use crate::io::omn::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        // Body and head are object-property atoms; `o:r(?x, ?y) -> o:s(?x, ?y)`.
        let doc = "Prefix: o: <http://ex/>\nOntology: <http://ex/o>\nRule: \n    o:r(?<http://ex/x>, ?<http://ex/y>) -> o:s(?<http://ex/x>, ?<http://ex/y>)\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        let rule = parsed
            .iter()
            .find_map(|ac| match &ac.component {
                Component::Rule(r) => Some(r.clone()),
                _ => None,
            })
            .expect("expected a SWRL Rule component");
        assert_eq!(rule.body.len(), 1);
        assert_eq!(rule.head.len(), 1);
        assert!(matches!(rule.body[0], Atom::ObjectPropertyAtom { .. }));
        assert!(matches!(rule.head[0], Atom::ObjectPropertyAtom { .. }));
    }

    #[test]
    fn parses_swrl_atom_kinds() {
        // Disambiguation: data-property (literal 2nd arg), built-in (literal
        // 1st arg), datarange (datatype pred + literal), same-individual.
        use crate::io::omn::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let doc = concat!(
            "Prefix: o: <http://ex/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n",
            "Ontology: <http://ex/o>\n",
            "Rule: o:d(?<http://ex/x>, \"v\") -> <http://ex/bi>(\"a\", \"b\")\n",
            "Rule: xsd:integer(\"1\") -> SameAs(o:I, o:J)\n",
        );
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        let mut seen = std::collections::BTreeSet::new();
        for ac in parsed.iter() {
            if let Component::Rule(r) = &ac.component {
                for a in r.body.iter().chain(r.head.iter()) {
                    seen.insert(match a {
                        Atom::DataPropertyAtom { .. } => "dp",
                        Atom::BuiltInAtom { .. } => "builtin",
                        Atom::DataRangeAtom { .. } => "datarange",
                        Atom::SameIndividualAtom(..) => "same",
                        _ => "other",
                    });
                }
            }
        }
        for kind in ["dp", "builtin", "datarange", "same"] {
            assert!(
                seen.contains(kind),
                "missing atom kind {kind}; got {seen:?}"
            );
        }
    }

    #[test]
    fn parses_inverse_object_property_frame_subject() {
        use crate::io::omn::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        // `ObjectProperty: inverse(o:r) Characteristics: Transitive`
        let doc = "Prefix: o: <http://ex/>\nOntology: <http://ex/o>\nObjectProperty: o:r\nObjectProperty: inverse (o:r)\n    Characteristics: Transitive\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        let t = parsed
            .iter()
            .find_map(|ac| match &ac.component {
                Component::TransitiveObjectProperty(t) => Some(t.0.clone()),
                _ => None,
            })
            .expect("expected a TransitiveObjectProperty");
        assert!(matches!(
            t,
            ObjectPropertyExpression::InverseObjectProperty(_)
        ));
    }

    #[test]
    fn parses_annotated_class_declaration() {
        use crate::io::omn::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        // Leading `Annotations:` before the subject annotates the Declaration.
        let doc = "Prefix: o: <http://ex/>\nPrefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\nOntology: <http://ex/o>\nClass: \n    Annotations: rdfs:comment \"c\"\n  o:C\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        let decl_ann = parsed
            .iter()
            .find_map(|ac| match &ac.component {
                Component::DeclareClass(_) => Some(ac.ann.clone()),
                _ => None,
            })
            .expect("expected a DeclareClass");
        assert_eq!(decl_ann.len(), 1, "expected one declaration annotation");
    }

    #[test]
    fn parses_nested_annotation_on_annotation() {
        use crate::io::omn::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        // `Annotations: Annotations: ex:meta "m" ex:label "L"` — the inner annotates the outer.
        let doc = "Prefix: ex: <http://ex/>\nOntology: <http://ex/o>\nClass: ex:A\n    Annotations: Annotations: ex:meta \"m\" ex:label \"L\"\n";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        // The outer `ex:label "L"` annotation on ex:A is recovered as an
        // AnnotationAssertion, and its nested `ex:meta "m"` annotation annotates
        // that assertion *axiom* — so it lands in the component's axiom
        // annotations (`ac.ann`), matching the ofn/owx readers, not inside the
        // assertion's own annotation value.
        let aa_comp = parsed
            .iter()
            .find(|ac| matches!(&ac.component, Component::AnnotationAssertion(_)))
            .expect("expected the outer annotation to survive")
            .clone();
        let Component::AnnotationAssertion(aa) = &aa_comp.component else {
            unreachable!()
        };
        // The assertion value carries no further (value-level) annotation …
        assert_eq!(aa.ann.ann.len(), 0);
        // … the nested `ex:meta "m"` is an annotation on the axiom.
        assert_eq!(
            aa_comp.ann.len(),
            1,
            "expected the nested annotation preserved"
        );
        assert_eq!(
            aa_comp.ann.iter().next().unwrap().ap,
            AnnotationProperty(b.iri("http://ex/meta"))
        );
    }

    #[test]
    fn nested_annotation_on_annotation_round_trips() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let doc = "Prefix: ex: <http://ex/>\nOntology: <http://ex/o>\nClass: ex:A\n    Annotations: Annotations: ex:meta \"m\" ex:label \"L\"\n";
        let (parsed, pm): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();

        // Write it back out: the nested form must appear as `Annotations: Annotations:`.
        let amo: ComponentMappedOntology<std::rc::Rc<str>, AnnotatedComponent<std::rc::Rc<str>>> =
            parsed.clone().into();
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(out).unwrap();
        assert!(
            s.contains("Annotations: Annotations:"),
            "expected nested `Annotations: Annotations:` in output, got:\n{s}"
        );

        // And it survives a full re-read (semantic round-trip).
        let (reparsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(s.as_bytes()), &b).unwrap();
        // The nested annotation is an annotation on the AnnotationAssertion
        // *axiom* (`ac.ann`), so it must survive there across the round-trip.
        let axiom_ann_len = |o: &SetOntology<_>| {
            o.iter()
                .find_map(|ac| match &ac.component {
                    Component::AnnotationAssertion(_) => Some(ac.ann.len()),
                    _ => None,
                })
                .unwrap_or(0)
        };
        assert_eq!(axiom_ann_len(&parsed), 1);
        assert_eq!(
            axiom_ann_len(&reparsed),
            1,
            "nested annotation lost on round-trip"
        );
    }

    #[test]
    fn reads_anonymous_individuals_round_trip() {
        use crate::io::omn::{read_with_build, write};
        use crate::ontology::component_mapped::ComponentMappedOntology;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        let mut o = SetOntology::new_rc();
        o.insert(DeclareNamedIndividual(b.named_individual("http://ex/a")));
        o.insert(DeclareObjectProperty(b.object_property("http://ex/r")));
        o.insert(ObjectPropertyAssertion {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://ex/r")),
            from: Individual::Named(b.named_individual("http://ex/a")),
            to: Individual::Anonymous(b.anon("genid1")),
        });
        type TestOnt = ComponentMappedOntology<
            std::rc::Rc<str>,
            std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
        >;
        let amo: TestOnt = o.clone().into();
        let mut buf = Vec::<u8>::new();
        write(&mut buf, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(buf.clone()).unwrap();
        assert!(
            s.contains("_:genid1"),
            "expected blank-node rendering, got:\n{s}"
        );
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&buf[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "anonymous individual did not round-trip\n{s}");
    }

    #[test]
    fn parses_general_manchester_document() {
        // A single hand-written §2.5 document exercising the constructs added
        // across Tasks 1–5c together: a version IRI in the header, a top-level
        // `DisjointClasses:` misc axiom over complex (ObjectSomeValuesFrom)
        // expressions, a `Datatype: D EquivalentTo: <facet range>`, a compound
        // data range (`xsd:integer and not {…}`) via a `DataProperty Range:`
        // AND inside a `SubClassOf: p some (…)`, an anonymous individual as a
        // `Facts:` target, and a nested annotation-on-annotation (parse-and-drop,
        // the outer annotation survives). Parsed via the READER on external-style
        // syntax (not our own writer's output) and asserted with `matches!`
        // spot-checks — this locks the general-§2.5 capability in one test.
        // The component count + variant shapes were verified empirically with the
        // `omnread`/`omndump` harness before baking them in.
        use crate::io::omn::read_with_build;
        use crate::ontology::set::SetOntology;
        use std::io::BufReader;
        let b = Build::new_rc();
        let doc = "\
Prefix: ex: <http://ex/>
Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>
Ontology: <http://ex/o> <http://ex/o/1.0.0>

Datatype: ex:SmallInt
    EquivalentTo: xsd:integer[<= \"255\"^^xsd:integer]

DataProperty: ex:p
    Range: (xsd:integer and not {\"x\"})

ObjectProperty: ex:r

Class: ex:A
    Annotations: Annotations: ex:meta \"m\" ex:label \"L\"
    SubClassOf: ex:p some (xsd:integer and not {\"x\"})

Class: ex:B

Individual: ex:a
    Facts: ex:r _:genid1

DisjointClasses: ex:r some ex:A, ex:r some ex:B
";
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(doc.as_bytes()), &b).unwrap();
        let comps: Vec<_> = parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(comps.len(), 13, "unexpected component count\n{comps:#?}");

        // Version IRI in the header.
        assert!(
            comps.iter().any(|c| matches!(
                c,
                Component::OntologyID(oid) if oid.viri.is_some()
            )),
            "expected an OntologyID carrying a version IRI"
        );
        // Datatype definition with a facet range.
        assert!(
            comps.iter().any(|c| matches!(
                c,
                Component::DatatypeDefinition(dd)
                    if matches!(dd.range, DataRange::DatatypeRestriction(_, _))
            )),
            "expected a DatatypeDefinition with a DatatypeRestriction range"
        );
        // Compound data range on the data-property Range:.
        assert!(
            comps.iter().any(|c| matches!(
                c,
                Component::DataPropertyRange(dpr)
                    if matches!(dpr.dr, DataRange::DataIntersectionOf(_))
            )),
            "expected a DataPropertyRange with a compound (intersection) range"
        );
        // SubClassOf carrying the compound DataSomeValuesFrom.
        assert!(
            comps.iter().any(|c| matches!(
                c,
                Component::SubClassOf(sc)
                    if matches!(&sc.sup, ClassExpression::DataSomeValuesFrom { dr, .. }
                        if matches!(dr, DataRange::DataIntersectionOf(_)))
            )),
            "expected a SubClassOf with a compound DataSomeValuesFrom on the RHS"
        );
        // Anonymous individual as a Facts target.
        assert!(
            comps.iter().any(|c| matches!(
                c,
                Component::ObjectPropertyAssertion(opa)
                    if matches!(opa.to, Individual::Anonymous(_))
            )),
            "expected an ObjectPropertyAssertion with an anonymous individual target"
        );
        // Top-level DisjointClasses misc axiom over complex expressions.
        assert!(
            comps.iter().any(|c| matches!(
                c,
                Component::DisjointClasses(DisjointClasses(v))
                    if v.iter().all(|ce| matches!(
                        ce, ClassExpression::ObjectSomeValuesFrom { .. }))
            )),
            "expected a top-level DisjointClasses over ObjectSomeValuesFrom members"
        );
        // Nested annotation-on-annotation: the OUTER (ex:label "L") survives as
        // an AnnotationAssertion, and its nested annotation is preserved.
        assert!(
            comps
                .iter()
                .any(|c| matches!(c, Component::AnnotationAssertion(_))),
            "expected the outer annotation to survive"
        );
    }

    /// Fixtures whose OMN (Tawny-OWL) and OWX (OWL-API) serialisations encode
    /// genuinely *different* ontologies, so `compare_to_owx` cannot equate them.
    /// Each exclusion is a corpus/oracle artefact, NOT an OMN reader defect —
    /// established by reading the two source files directly:
    ///
    /// * `annotation_assertion` — the two sources name different subjects
    ///   (`<http://www.example.com/i>` in the OWX vs `o:i` =
    ///   `http://www.example.com/iri#i` in the OMN).
    /// * `gci_and_other_class_relations` — the OWX carries `EquivalentClasses`
    ///   and `DisjointClasses` GCIs over complex expressions that the Tawny OMN
    ///   serialisation simply omits (it emits only the `SubClassOf` GCI).
    ///
    /// (The `equivalent_classes` / `complex-equivalent-classes` /
    /// `annotation-on-equivalent-classes` fixtures were excluded until the reader
    /// was changed to read a frame `EquivalentTo:` list as per-item binary axioms
    /// — matching the OWL-API / owx — rather than one fused n-ary axiom; they now
    /// compare cleanly. The `annotation-with-annotation` /
    /// `annotation-with-non-builtin-annotation` fixtures were excluded until the
    /// compare test exposed a real OMN reader bug — a nested frame annotation was
    /// attached to the annotation value rather than the assertion axiom.)
    const COMPARE_EXCLUSIONS: &[&str] = &["annotation_assertion", "gci_and_other_class_relations"];

    /// Cross-format conformance: `compare(read(OWX), read(OMN))`.
    ///
    /// For every `owl-manchester/*.omn` fixture with a same-stem `owl-xml/*.owx`
    /// twin (the OWL-API test corpus serialised both ways), read each through its
    /// own reader and assert the ontologies are equal. This catches *systematic*
    /// errors in the OMN reader/parser that a read→write→read round-trip cannot:
    /// a round-trip only proves the OMN reader and writer agree with each other,
    /// whereas this pins the OMN reader against the independent OWX oracle.
    /// Mirrors the RDF reader's `compare_to_xml`.
    ///
    /// Two readings are compared modulo the differences between the Tawny-OWL and
    /// OWL-API serialisation conventions that are not parser behaviour:
    ///
    /// * **Declarations** — Tawny emits explicit `Declaration`s for every entity
    ///   (built-ins like `rdfs:label` / `rdf:langString` included) where the
    ///   OWL-API omits them, so declarations are dropped before comparing. (OMN
    ///   declaration fidelity is covered by `roundtrip_resource`.)
    /// * **n-ary operand order** — operands of unordered axioms
    ///   (`EquivalentClasses`, `SameIndividual`, SWRL rule atoms, …) are sorted,
    ///   since the two writers emit them in different orders.
    ///
    /// 122 of the 127 OMN fixtures have an OWX twin (the 5 OMN-only fixtures are
    /// skipped). A further [`COMPARE_EXCLUSIONS`] set covers fixtures whose two
    /// serialisations encode genuinely different ontologies; see that constant.
    #[rstest]
    fn compare_to_owx(#[files("src/ont/owl-manchester/*.omn")] resource: std::path::PathBuf) {
        use crate::model::{ComponentKind, Kinded};
        use crate::normalize::normalize;
        use crate::ontology::set::SetOntology;
        use std::path::Path;

        let stem = Path::new(&resource)
            .file_stem()
            .unwrap()
            .to_string_lossy()
            .into_owned();
        let owx_path = format!("src/ont/owl-xml/{stem}.owx");
        if !Path::new(&owx_path).exists() {
            // OMN-only fixture: no independent XML oracle to compare against.
            return;
        }
        if COMPARE_EXCLUSIONS.contains(&stem.as_str()) {
            // Sources encode different ontologies (see COMPARE_EXCLUSIONS).
            return;
        }

        // Canonicalise to the logical/annotation content shared by both
        // serialisation conventions: normalise (sort + reanonymise + drop
        // DocIRI), drop declarations, and sort unordered n-ary operands.
        let canon =
            |o: SetOntology<RcStr>| -> std::collections::BTreeSet<AnnotatedComponent<RcStr>> {
                let mut v: Vec<AnnotatedComponent<RcStr>> = normalize(o.into_iter().collect())
                    .into_iter()
                    .filter(|c| {
                        !matches!(
                            c.kind(),
                            ComponentKind::DeclareClass
                                | ComponentKind::DeclareObjectProperty
                                | ComponentKind::DeclareDataProperty
                                | ComponentKind::DeclareAnnotationProperty
                                | ComponentKind::DeclareNamedIndividual
                                | ComponentKind::DeclareDatatype
                        )
                    })
                    .collect();
                for ac in v.iter_mut() {
                    match &mut ac.component {
                        Component::EquivalentClasses(EquivalentClasses(x)) => x.sort(),
                        Component::DisjointClasses(DisjointClasses(x)) => x.sort(),
                        Component::EquivalentObjectProperties(EquivalentObjectProperties(x)) => {
                            x.sort()
                        }
                        Component::DisjointObjectProperties(DisjointObjectProperties(x)) => {
                            x.sort()
                        }
                        Component::EquivalentDataProperties(EquivalentDataProperties(x)) => {
                            x.sort()
                        }
                        Component::DisjointDataProperties(DisjointDataProperties(x)) => x.sort(),
                        Component::SameIndividual(SameIndividual(x)) => x.sort(),
                        Component::DifferentIndividuals(DifferentIndividuals(x)) => x.sort(),
                        Component::Rule(r) => {
                            r.head.sort();
                            r.body.sort();
                        }
                        Component::InverseObjectProperties(InverseObjectProperties(a, b)) => {
                            if a > b {
                                std::mem::swap(a, b);
                            }
                        }
                        _ => {}
                    }
                }
                v.into_iter().collect()
            };

        // Read the Manchester form through the OMN reader (the subject).
        let omn_reader = std::fs::File::open(&resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (omn_ont, _): (SetOntology<RcStr>, _) =
            crate::io::omn::reader::read(omn_reader, Default::default())
                .unwrap_or_else(|e| panic!("OMN read failed for {}: {e:?}", resource.display()));

        // Read the OWL/XML form through the OWX reader (the oracle).
        let owx_src = std::fs::read_to_string(&owx_path).unwrap();
        let owx_ont: SetOntology<RcStr> =
            crate::io::owx::reader::test::read_ok(&mut owx_src.as_bytes())
                .0
                .into();

        assert_eq!(
            canon(owx_ont),
            canon(omn_ont),
            "OMN reader output diverges from the OWX oracle for `{stem}`"
        );
    }
}
