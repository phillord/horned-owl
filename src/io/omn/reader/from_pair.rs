use curie::Curie;
use curie::PrefixMapping;
use pest::iterators::Pair;

use crate::error::HornedError;
use crate::model::*;
use crate::vocab::Facet;

use super::Rule;

// ---------------------------------------------------------------------------

type Result<T> = std::result::Result<T, HornedError>;

// ---------------------------------------------------------------------------

/// Shared parsing context: carries the `Build` and prefix mapping.
pub struct Context<'a, A: ForIRI> {
    pub(crate) build: &'a Build<A>,
    pub(crate) prefixes: &'a PrefixMapping,
}

impl<'a, A: ForIRI> Context<'a, A> {
    pub fn new(build: &'a Build<A>, prefixes: &'a PrefixMapping) -> Self {
        Self { build, prefixes }
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
            rule => unreachable!("unexpected rule in IRI::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

/// `Individual` in omn is `{ IRI }` — always named (no blank node syntax).
impl<A: ForIRI> FromPair<A> for Individual<A> {
    const RULE: Rule = Rule::Individual;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let iri = IRI::from_pair(pair.into_inner().next().unwrap(), ctx)?;
        Ok(Individual::Named(NamedIndividual(iri)))
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
            rule => unreachable!("unexpected rule in Literal::from_pair: {:?}", rule),
        }
    }
}

// ---------------------------------------------------------------------------

/// `ope = { ( ^"inverse" ~ "(" ~ ObjectPropertyIRI ~ ")" ) | ObjectPropertyIRI }`
///
/// The `inverse` keyword and the parentheses are bare literals — pest does NOT
/// emit them as child pairs. So `into_inner()` always yields `[ObjectPropertyIRI]`
/// regardless of which arm matched. We discriminate on the raw string instead.
impl<A: ForIRI> FromPair<A> for ObjectPropertyExpression<A> {
    const RULE: Rule = Rule::ope;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let s = pair.as_str().trim_start();
        // "inverse" arm: starts with the keyword followed by '(' (after optional whitespace).
        // Guard against a CURIE whose prefix is "inverse:" (rare but possible).
        let is_inverse = s.len() >= 7
            && s[..7].eq_ignore_ascii_case("inverse")
            && s[7..].trim_start().starts_with('(');
        let op_pair = pair.into_inner().next().unwrap();
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

/// `DataRange = { DatatypeIRI ~ ( "[" ~ Facet ~ ( "," ~ Facet )* ~ "]" )? }`
///
/// No brackets → `DataRange::Datatype`; with brackets → `DataRange::DatatypeRestriction`.
/// (Composite data ranges—`DataOneOf`, `DataIntersectionOf`, etc.—are not in the
/// Manchester grammar and are therefore not handled here.)
impl<A: ForIRI> FromPair<A> for DataRange<A> {
    const RULE: Rule = Rule::DataRange;
    fn from_pair_unchecked(pair: Pair<Rule>, ctx: &Context<'_, A>) -> Result<Self> {
        let mut inner = pair.into_inner();
        let dt_pair = inner.next().unwrap();
        let dt = Datatype::from_pair(dt_pair, ctx)?;
        // Collect any Facet children
        let facets: Result<Vec<FacetRestriction<A>>> =
            inner.map(|p| FacetRestriction::from_pair(p, ctx)).collect();
        let facets = facets?;
        if facets.is_empty() {
            Ok(DataRange::Datatype(dt))
        } else {
            Ok(DataRange::DatatypeRestriction(dt, facets))
        }
    }
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::io::omn::reader::lexer::ManchesterLexer;
    use crate::model::{Build, RcStr};

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
}
