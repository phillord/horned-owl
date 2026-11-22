//! An OWL Functional Syntax reader written with [`nom`].
//!
//! This builds model values directly while parsing, rather than going via a
//! parse tree as the original pest reader (the `horned-pest` crate) does. It accepts the
//! same language.
use std::collections::BTreeSet;
use std::io::BufRead;

use curie::{Curie, PrefixMapping};
use enum_meta::Meta;
use nom::Err;

use crate::error::HornedError;
use crate::io::{ParserConfiguration, StreamComponent};
use crate::model::*;
use crate::vocab::{Facet, OWL, OWL2Datatype};

pub(crate) mod bcp47;
pub(crate) mod error;
pub(crate) mod lex;
pub(crate) mod rfc3987;
mod stream;

pub use stream::{Reader, read_to_stream};

/// Entry points to the token-level parsers, for the differential tests in
/// `horned-pest`. Not part of the API.
#[doc(hidden)]
pub mod probe {
    /// Whether `s` is entirely one `<...>` IRI.
    pub fn full_iri_matches(s: &str) -> bool {
        super::lex::full_iri(s).is_ok_and(|(rest, _)| rest.is_empty())
    }

    /// Whether `s` is entirely one prefixed name.
    pub fn prefixed_name_matches(s: &str) -> bool {
        super::lex::raw_iri(s).is_ok_and(|(rest, raw)| {
            rest.is_empty() && matches!(raw, super::lex::RawIri::Prefixed(..))
        })
    }

    /// Whether `s` is entirely one blank node label.
    pub fn blank_node_matches(s: &str) -> bool {
        super::lex::blank_node(s).is_ok_and(|(rest, _)| rest.is_empty())
    }

    /// The length of the longest language tag at the start of `s`.
    pub fn language_tag_len(s: &str) -> Option<usize> {
        super::bcp47::language_tag_len(s)
    }
}

use error::PErr;
use lex::*;

pub(super) struct Ctx<'c, A: ForIRI> {
    pub(super) build: &'c Build<A>,
    pub(super) mapping: &'c PrefixMapping,
}

/// Read an ontology in OWL Functional Syntax by draining a [`Reader`].
pub fn read<
    A: ForIRI,
    B: AsRef<Build<A>>,
    O: MutableOntology<A> + Ontology<A> + Default,
    R: BufRead,
>(
    bufread: &mut R,
    config: ParserConfiguration<A, B>,
) -> Result<(O, PrefixMapping), HornedError> {
    let mut reader = Reader::new(bufread, config);
    let mut ont = O::default();
    for item in reader.by_ref() {
        if let StreamComponent::Component(ac) = item? {
            ont.insert(ac);
        }
    }
    Ok((ont, reader.into_prefixes()))
}

/// Read an ontology in OWL Functional Syntax, parsing the whole document
/// from memory in one go.
pub fn read_in_memory<
    A: ForIRI,
    B: AsRef<Build<A>>,
    O: MutableOntology<A> + Ontology<A> + Default,
    R: BufRead,
>(
    bufread: &mut R,
    config: ParserConfiguration<A, B>,
) -> Result<(O, PrefixMapping), HornedError> {
    let mut doc = String::new();
    crate::io::skip_bom(bufread)?;
    bufread.read_to_string(&mut doc)?;
    let doc = doc.trim();
    document(config.build.as_ref(), doc).map_err(|e| match e {
        Err::Error(e) | Err::Failure(e) => e.into_horned(doc, error::Origin::default()),
        Err::Incomplete(_) => HornedError::invalid("incomplete input"),
    })
}

// ---------------------------------------------------------------------------

pub(super) type R<'a, T> = PResult<'a, T>;

type ObjectCardinality<A> =
    fn(u32, ObjectPropertyExpression<A>, Box<ClassExpression<A>>) -> ClassExpression<A>;

/// Make a failure unrecoverable, so it is not masked by an alternative.
fn fatal<T>(r: R<'_, T>) -> R<'_, T> {
    r.map_err(|e| match e {
        Err::Error(e) => Err::Failure(e),
        e => e,
    })
}

/// Record the construct being parsed on a failure.
pub(super) fn within<'a, T>(kw: &'a str, r: R<'a, T>) -> R<'a, T> {
    r.map_err(|e| match e {
        Err::Error(e) | Err::Failure(e) => Err::Failure(e.within(kw)),
        e => e,
    })
}

fn unknown<'a, T>(what: &str, kw: &'a str) -> R<'a, T> {
    Err(Err::Failure(PErr::semantic(
        kw,
        kw.len(),
        format!("unknown or unsupported {what} `{kw}`"),
    )))
}

/// Parse items until the closing `)`, which is consumed. At least `min`
/// items are required.
fn list<'a, T>(
    mut i: &'a str,
    min: usize,
    what: &'static str,
    mut item: impl FnMut(&'a str) -> R<'a, T>,
) -> R<'a, Vec<T>> {
    let mut v = Vec::new();
    loop {
        let j = sp(i);
        if let Some(rest) = j.strip_prefix(')') {
            if v.len() < min {
                return Err(Err::Failure(PErr::expected(j, what)));
            }
            return Ok((rest, v));
        }
        let (r, x) = item(j)?;
        v.push(x);
        i = r;
    }
}

// ---------------------------------------------------------------------------
// Document

fn document<'a, A, O>(build: &Build<A>, doc: &'a str) -> Result<(O, PrefixMapping), Err<PErr<'a>>>
where
    A: ForIRI,
    O: MutableOntology<A> + Ontology<A> + Default,
{
    let mut prefixes = PrefixMapping::default();
    let mut i = doc;
    while peek_kw(i) == Some("Prefix") {
        let at = sp(i);
        let (r, (name, iri)) = prefix_decl(i)?;
        prefixes.add_prefix(name, iri).map_err(|_| {
            Err::Failure(PErr::semantic(
                at,
                r.len().abs_diff(at.len()),
                "invalid prefix declaration",
            ))
        })?;
        i = r;
    }

    let ctx = Ctx {
        build,
        mapping: &prefixes,
    };
    let (r, ontology) = fatal(ontology(&ctx, i))?;
    let r = sp(r);
    if !r.is_empty() {
        return Err(Err::Failure(PErr::expected(r, "end of input")));
    }
    Ok((ontology, prefixes))
}

/// `Prefix(name:=<iri>)`, returning the name and the IRI.
pub(super) fn prefix_decl(i: &str) -> R<'_, (&str, &str)> {
    let (r, kw) = keyword(i)?;
    let (r, p) = within(kw, fatal(pname_ns(r)))?;
    let (r, _) = within(kw, fatal(equals(r)))?;
    let (r, iri) = within(kw, fatal(full_iri(r)))?;
    let (r, _) = within(kw, fatal(rparen(r)))?;
    Ok((r, (p, iri)))
}

pub(super) fn import<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Import<A>> {
    let (r, kw) = keyword(i)?;
    let (r, x) = within(kw, iri(c, r))?;
    let (r, _) = within(kw, rparen(r))?;
    Ok((r, Import(x)))
}

pub(super) fn ontology_annotation<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    i: &'a str,
) -> R<'a, OntologyAnnotation<A>> {
    annotation(c, i).map(|(r, a)| (r, OntologyAnnotation(a)))
}

fn ontology<'a, A, O>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, O>
where
    A: ForIRI,
    O: MutableOntology<A> + Ontology<A> + Default,
{
    let (mut r, _) = expect_kw(i, "Ontology")?;
    let mut ontology = O::default();
    let mut id = OntologyID::default();

    if !at_rparen(r) && peek_kw(r).is_none() {
        let (r1, iri_) = within("Ontology", iri(c, r))?;
        id.iri = Some(iri_);
        r = r1;
        if !at_rparen(r) && peek_kw(r).is_none() {
            let (r1, v) = within("Ontology", iri(c, r))?;
            id.viri = Some(v);
            r = r1;
        }
    }
    ontology.insert(id);

    while peek_kw(r) == Some("Import") {
        let (r1, x) = import(c, r)?;
        ontology.insert(x);
        r = r1;
    }

    while peek_kw(r) == Some("Annotation") {
        let (r1, a) = ontology_annotation(c, r)?;
        ontology.insert(a);
        r = r1;
    }

    loop {
        if at_rparen(r) {
            let (r, _) = rparen(r)?;
            return Ok((r, ontology));
        }
        let (r1, ax) = within("Ontology", axiom(c, r))?;
        ontology.insert(ax);
        r = r1;
    }
}

// ---------------------------------------------------------------------------
// IRIs, individuals, literals

pub(super) fn iri<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, IRI<A>> {
    let start = sp(i);
    let (r, raw) = raw_iri(i)?;
    match raw {
        RawIri::Full(s) => Ok((r, c.build.iri(s))),
        RawIri::Prefixed(prefix, local, whole) => {
            match c.mapping.expand_curie(&Curie::new(Some(prefix), local)) {
                Ok(s) => Ok((r, c.build.iri(s))),
                Err(curie::ExpansionError::Invalid) => Err(Err::Failure(PErr::semantic(
                    start,
                    whole.len(),
                    "undefined prefix",
                ))),
                Err(curie::ExpansionError::MissingDefault) => Err(Err::Failure(PErr::semantic(
                    start,
                    whole.len(),
                    "missing default prefix",
                ))),
            }
        }
    }
}

fn class<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Class<A>> {
    iri(c, i).map(|(r, x)| (r, Class(x)))
}

fn datatype<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Datatype<A>> {
    iri(c, i).map(|(r, x)| (r, Datatype(x)))
}

fn object_property<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, ObjectProperty<A>> {
    iri(c, i).map(|(r, x)| (r, ObjectProperty(x)))
}

fn data_property<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, DataProperty<A>> {
    iri(c, i).map(|(r, x)| (r, DataProperty(x)))
}

fn annotation_property<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, AnnotationProperty<A>> {
    iri(c, i).map(|(r, x)| (r, AnnotationProperty(x)))
}

fn anonymous<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, AnonymousIndividual<A>> {
    let (r, s) = blank_node(i)?;
    Ok((r, AnonymousIndividual(c.build.iri(s).underlying())))
}

fn individual<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Individual<A>> {
    if at_blank_node(i) {
        anonymous(c, i).map(|(r, x)| (r, Individual::Anonymous(x)))
    } else {
        iri(c, i).map(|(r, x)| (r, Individual::Named(NamedIndividual(x))))
    }
}

fn literal<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Literal<A>> {
    let (r, literal) = quoted_string(i)?;
    let j = sp(r);
    if let Some(r) = j.strip_prefix('^') {
        let (r, _) = fatal(
            sp(r)
                .strip_prefix('^')
                .map(|r| (r, ()))
                .ok_or_else(|| Err::Error(PErr::expected(sp(r), "`^^`"))),
        )?;
        let (r, dt) = fatal(datatype(c, r))?;
        Ok((
            r,
            Literal::Datatype {
                literal,
                datatype_iri: dt.0,
            },
        ))
    } else if j.starts_with('@') {
        let (r, lang) = language_tag(j)?;
        Ok((
            r,
            Literal::Language {
                literal,
                lang: lang.to_string(),
            },
        ))
    } else {
        Ok((r, Literal::Simple { literal }))
    }
}

// ---------------------------------------------------------------------------
// Annotations

fn annotations<'a, A: ForIRI>(c: &Ctx<'_, A>, mut i: &'a str) -> R<'a, BTreeSet<Annotation<A>>> {
    let mut set = BTreeSet::new();
    while peek_kw(i) == Some("Annotation") {
        let (r, a) = annotation(c, i)?;
        set.insert(a);
        i = r;
    }
    Ok((i, set))
}

pub(super) fn annotation<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Annotation<A>> {
    let (r, kw) = keyword(i)?;
    within(kw, annotation_body(c, r))
}

fn annotation_body<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Annotation<A>> {
    let (r, ann) = annotations(c, i)?;
    let (r, ap) = annotation_property(c, r)?;
    let (r, av) = annotation_value(c, r)?;
    let (r, _) = rparen(r)?;
    Ok((r, Annotation { ap, av, ann }))
}

fn annotation_value<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, AnnotationValue<A>> {
    if at_blank_node(i) {
        anonymous(c, i).map(|(r, x)| (r, AnnotationValue::AnonymousIndividual(x)))
    } else if sp(i).starts_with('"') {
        literal(c, i).map(|(r, x)| (r, AnnotationValue::Literal(x)))
    } else {
        iri(c, i).map(|(r, x)| (r, AnnotationValue::IRI(x)))
    }
}

fn annotation_subject<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, AnnotationSubject<A>> {
    if at_blank_node(i) {
        anonymous(c, i).map(|(r, x)| (r, AnnotationSubject::AnonymousIndividual(x)))
    } else {
        iri(c, i).map(|(r, x)| (r, AnnotationSubject::IRI(x)))
    }
}

// ---------------------------------------------------------------------------
// Property expressions

fn object_property_expression<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    i: &'a str,
) -> R<'a, ObjectPropertyExpression<A>> {
    if peek_kw(i) == Some("ObjectInverseOf") {
        let (r, kw) = keyword(i)?;
        let (r, p) = within(kw, object_property(c, r))?;
        let (r, _) = within(kw, rparen(r))?;
        Ok((r, ObjectPropertyExpression::InverseObjectProperty(p)))
    } else {
        object_property(c, i).map(|(r, p)| (r, ObjectPropertyExpression::ObjectProperty(p)))
    }
}

// ---------------------------------------------------------------------------
// Data ranges

fn data_range<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, DataRange<A>> {
    let i = sp(i);
    let Some(kw) = peek_kw(i) else {
        return datatype(c, i).map(|(r, x)| (r, DataRange::Datatype(x)));
    };
    let (r, _) = keyword(i)?;
    within(kw, data_range_body(c, kw, r, i))
}

fn data_range_body<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    kw: &'a str,
    r: &'a str,
    start: &'a str,
) -> R<'a, DataRange<A>> {
    match kw {
        "DataIntersectionOf" => list(r, 2, "data range", |i| data_range(c, i))
            .map(|(r, v)| (r, DataRange::DataIntersectionOf(v))),
        "DataUnionOf" => list(r, 2, "data range", |i| data_range(c, i))
            .map(|(r, v)| (r, DataRange::DataUnionOf(v))),
        "DataComplementOf" => {
            let (r, x) = data_range(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, DataRange::DataComplementOf(Box::new(x))))
        }
        "DataOneOf" => {
            list(r, 1, "literal", |i| literal(c, i)).map(|(r, v)| (r, DataRange::DataOneOf(v)))
        }
        "DatatypeRestriction" => {
            let (r, dt) = datatype(c, r)?;
            let (r, v) = list(r, 1, "facet restriction", |i| facet_restriction(c, i))?;
            Ok((r, DataRange::DatatypeRestriction(dt, v)))
        }
        _ => unknown("data range", &start[..kw.len()]),
    }
}

fn facet_restriction<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, FacetRestriction<A>> {
    let start = sp(i);
    let (r, iri_) = iri(c, i)?;
    let f = Facet::all()
        .into_iter()
        .find(|facet| iri_.as_ref() == facet.as_ref())
        .ok_or_else(|| {
            Err::Failure(PErr::semantic(
                start,
                start.len() - r.len(),
                "invalid facet",
            ))
        })?;
    let (r, l) = literal(c, r)?;
    Ok((r, FacetRestriction { f, l }))
}

// ---------------------------------------------------------------------------
// Class expressions

fn class_expression<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, ClassExpression<A>> {
    let i = sp(i);
    let Some(kw) = peek_kw(i) else {
        return class(c, i).map(|(r, x)| (r, ClassExpression::Class(x)));
    };
    let (r, _) = keyword(i)?;
    within(kw, class_expression_body(c, kw, r, i))
}

fn object_cardinality<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    r: &'a str,
    make: ObjectCardinality<A>,
) -> R<'a, ClassExpression<A>> {
    let (r, n) = non_negative_integer(r)?;
    let (r, ope) = object_property_expression(c, r)?;
    // A missing class expression is `owl:Thing`.
    let (r, bce) = if at_rparen(r) {
        (
            r,
            Box::new(ClassExpression::Class(Class(c.build.iri(OWL::Thing)))),
        )
    } else {
        let (r, ce) = class_expression(c, r)?;
        (r, Box::new(ce))
    };
    let (r, _) = rparen(r)?;
    Ok((r, make(n, ope, bce)))
}

fn data_cardinality<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    r: &'a str,
    make: fn(u32, DataProperty<A>, DataRange<A>) -> ClassExpression<A>,
) -> R<'a, ClassExpression<A>> {
    let (r, n) = non_negative_integer(r)?;
    let (r, dp) = data_property(c, r)?;
    // A missing data range is `rdfs:Literal`.
    let (r, dr) = if at_rparen(r) {
        (r, Datatype(c.build.iri(OWL2Datatype::Literal)).into())
    } else {
        data_range(c, r)?
    };
    let (r, _) = rparen(r)?;
    Ok((r, make(n, dp, dr)))
}

/// `DataSomeValuesFrom` and `DataAllValuesFrom`: a data property then a
/// data range.
fn data_values_from<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    r: &'a str,
    make: fn(DataProperty<A>, DataRange<A>) -> ClassExpression<A>,
    name: &'static str,
) -> R<'a, ClassExpression<A>> {
    let (r, dp) = data_property(c, r)?;
    let j = sp(r);
    let (r, dr) = if peek_kw(j).is_some() || j.starts_with(')') {
        data_range(c, j)?
    } else {
        let (after, x) = iri(c, j)?;
        if at_rparen(after) {
            (after, DataRange::Datatype(Datatype(x)))
        } else {
            return Err(Err::Failure(PErr::semantic(
                j,
                j.len() - after.len(),
                format!("horned-owl does not support data property chaining in `{name}`"),
            )));
        }
    };
    let (r, _) = rparen(r)?;
    Ok((r, make(dp, dr)))
}

fn class_expression_body<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    kw: &'a str,
    r: &'a str,
    start: &'a str,
) -> R<'a, ClassExpression<A>> {
    use ClassExpression as C;
    match kw {
        "ObjectIntersectionOf" => list(r, 2, "class expression", |i| class_expression(c, i))
            .map(|(r, v)| (r, C::ObjectIntersectionOf(v))),
        "ObjectUnionOf" => list(r, 2, "class expression", |i| class_expression(c, i))
            .map(|(r, v)| (r, C::ObjectUnionOf(v))),
        "ObjectComplementOf" => {
            let (r, x) = class_expression(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, C::ObjectComplementOf(Box::new(x))))
        }
        "ObjectOneOf" => {
            list(r, 1, "individual", |i| individual(c, i)).map(|(r, v)| (r, C::ObjectOneOf(v)))
        }
        "ObjectSomeValuesFrom" => {
            let (r, ope) = object_property_expression(c, r)?;
            let (r, ce) = class_expression(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((
                r,
                C::ObjectSomeValuesFrom {
                    ope,
                    bce: Box::new(ce),
                },
            ))
        }
        "ObjectAllValuesFrom" => {
            let (r, ope) = object_property_expression(c, r)?;
            let (r, ce) = class_expression(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((
                r,
                C::ObjectAllValuesFrom {
                    ope,
                    bce: Box::new(ce),
                },
            ))
        }
        "ObjectHasValue" => {
            let (r, ope) = object_property_expression(c, r)?;
            let (r, i) = individual(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, C::ObjectHasValue { ope, i }))
        }
        "ObjectHasSelf" => {
            let (r, ope) = object_property_expression(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, C::ObjectHasSelf(ope)))
        }
        "ObjectMinCardinality" => {
            object_cardinality(c, r, |n, ope, bce| C::ObjectMinCardinality { n, ope, bce })
        }
        "ObjectMaxCardinality" => {
            object_cardinality(c, r, |n, ope, bce| C::ObjectMaxCardinality { n, ope, bce })
        }
        "ObjectExactCardinality" => object_cardinality(c, r, |n, ope, bce| {
            C::ObjectExactCardinality { n, ope, bce }
        }),
        "DataSomeValuesFrom" => data_values_from(
            c,
            r,
            |dp, dr| C::DataSomeValuesFrom { dp, dr },
            "DataSomeValuesFrom",
        ),
        "DataAllValuesFrom" => data_values_from(
            c,
            r,
            |dp, dr| C::DataAllValuesFrom { dp, dr },
            "DataAllValuesFrom",
        ),
        "DataHasValue" => {
            let (r, dp) = data_property(c, r)?;
            let (r, l) = literal(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, C::DataHasValue { dp, l }))
        }
        "DataMinCardinality" => {
            data_cardinality(c, r, |n, dp, dr| C::DataMinCardinality { n, dp, dr })
        }
        "DataMaxCardinality" => {
            data_cardinality(c, r, |n, dp, dr| C::DataMaxCardinality { n, dp, dr })
        }
        "DataExactCardinality" => {
            data_cardinality(c, r, |n, dp, dr| C::DataExactCardinality { n, dp, dr })
        }
        _ => unknown("class expression", &start[..kw.len()]),
    }
}

// ---------------------------------------------------------------------------
// Axioms

fn entity<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Component<A>> {
    let start = sp(i);
    let (r, kw) = fatal(keyword(i))?;
    let (r, comp): (_, Component<A>) = within(
        kw,
        match kw {
            "Class" => class(c, r).map(|(r, x)| (r, DeclareClass(x).into())),
            "Datatype" => datatype(c, r).map(|(r, x)| (r, DeclareDatatype(x).into())),
            "ObjectProperty" => {
                object_property(c, r).map(|(r, x)| (r, DeclareObjectProperty(x).into()))
            }
            "DataProperty" => data_property(c, r).map(|(r, x)| (r, DeclareDataProperty(x).into())),
            "AnnotationProperty" => {
                annotation_property(c, r).map(|(r, x)| (r, DeclareAnnotationProperty(x).into()))
            }
            "NamedIndividual" => {
                iri(c, r).map(|(r, x)| (r, DeclareNamedIndividual(NamedIndividual(x)).into()))
            }
            _ => return unknown("entity", &start[..kw.len()]),
        },
    )?;
    let (r, _) = within(kw, rparen(r))?;
    Ok((r, comp))
}

fn individual_list<'a, A: ForIRI>(c: &Ctx<'_, A>, r: &'a str) -> R<'a, Vec<Individual<A>>> {
    list(r, 2, "individual (at least two required)", |i| {
        individual(c, i)
    })
}

fn class_list<'a, A: ForIRI>(c: &Ctx<'_, A>, r: &'a str) -> R<'a, Vec<ClassExpression<A>>> {
    list(r, 2, "class expression (at least two required)", |i| {
        class_expression(c, i)
    })
}

fn ope_list<'a, A: ForIRI>(c: &Ctx<'_, A>, r: &'a str) -> R<'a, Vec<ObjectPropertyExpression<A>>> {
    list(r, 2, "object property (at least two required)", |i| {
        object_property_expression(c, i)
    })
}

fn dp_list<'a, A: ForIRI>(c: &Ctx<'_, A>, r: &'a str) -> R<'a, Vec<DataProperty<A>>> {
    list(r, 2, "data property (at least two required)", |i| {
        data_property(c, i)
    })
}

pub(super) fn axiom<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, AnnotatedComponent<A>> {
    let i = sp(i);
    if peek_kw(i).is_none() {
        return Err(Err::Failure(PErr::expected(i, "axiom or `)`")));
    }
    let (r, kw) = keyword(i)?;
    within(kw, axiom_body(c, kw, r, i))
}

fn axiom_body<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    kw: &'a str,
    r: &'a str,
    start: &'a str,
) -> R<'a, AnnotatedComponent<A>> {
    // Every axiom starts with its annotations, so check the keyword first.
    if !is_axiom_keyword(kw) {
        return unknown("axiom", &start[..kw.len()]);
    }
    let (r, ann) = annotations(c, r)?;
    let (r, comp): (_, Component<A>) = match kw {
        "Declaration" => {
            let (r, x) = entity(c, r)?;
            let (r, _) = rparen(r)?;
            (r, x)
        }
        "SubClassOf" => {
            let (r, sub) = class_expression(c, r)?;
            let (r, sup) = class_expression(c, r)?;
            let (r, _) = rparen(r)?;
            (r, SubClassOf::new(sup, sub).into())
        }
        "EquivalentClasses" => {
            let (r, v) = class_list(c, r)?;
            (r, EquivalentClasses(v).into())
        }
        "DisjointClasses" => {
            let (r, v) = class_list(c, r)?;
            (r, DisjointClasses(v).into())
        }
        "DisjointUnion" => {
            let (r, cls) = class(c, r)?;
            let (r, v) = class_list(c, r)?;
            (r, DisjointUnion(cls, v).into())
        }
        "SubObjectPropertyOf" => {
            let (r, sub) = if peek_kw(r) == Some("ObjectPropertyChain") {
                let (r, kw) = keyword(r)?;
                let (r, v) = within(kw, ope_list(c, r))?;
                (r, SubObjectPropertyExpression::ObjectPropertyChain(v))
            } else {
                let (r, x) = object_property_expression(c, r)?;
                (r, SubObjectPropertyExpression::ObjectPropertyExpression(x))
            };
            let (r, sup) = object_property_expression(c, r)?;
            let (r, _) = rparen(r)?;
            (r, SubObjectPropertyOf { sup, sub }.into())
        }
        "EquivalentObjectProperties" => {
            let (r, v) = ope_list(c, r)?;
            (r, EquivalentObjectProperties(v).into())
        }
        "DisjointObjectProperties" => {
            let (r, v) = ope_list(c, r)?;
            (r, DisjointObjectProperties(v).into())
        }
        "ObjectPropertyDomain" => {
            let (r, ope) = object_property_expression(c, r)?;
            let (r, ce) = class_expression(c, r)?;
            let (r, _) = rparen(r)?;
            (r, ObjectPropertyDomain::new(ope, ce).into())
        }
        "ObjectPropertyRange" => {
            let (r, ope) = object_property_expression(c, r)?;
            let (r, ce) = class_expression(c, r)?;
            let (r, _) = rparen(r)?;
            (r, ObjectPropertyRange::new(ope, ce).into())
        }
        "InverseObjectProperties" => {
            let (r, a) = object_property(c, r)?;
            let (r, b) = object_property(c, r)?;
            let (r, _) = rparen(r)?;
            (r, InverseObjectProperties(a, b).into())
        }
        "FunctionalObjectProperty"
        | "InverseFunctionalObjectProperty"
        | "ReflexiveObjectProperty"
        | "IrreflexiveObjectProperty"
        | "SymmetricObjectProperty"
        | "AsymmetricObjectProperty"
        | "TransitiveObjectProperty" => {
            let (r, p) = object_property_expression(c, r)?;
            let (r, _) = rparen(r)?;
            let comp: Component<A> = match kw {
                "FunctionalObjectProperty" => FunctionalObjectProperty(p).into(),
                "InverseFunctionalObjectProperty" => InverseFunctionalObjectProperty(p).into(),
                "ReflexiveObjectProperty" => ReflexiveObjectProperty(p).into(),
                "IrreflexiveObjectProperty" => IrreflexiveObjectProperty(p).into(),
                "SymmetricObjectProperty" => SymmetricObjectProperty(p).into(),
                "AsymmetricObjectProperty" => AsymmetricObjectProperty(p).into(),
                _ => TransitiveObjectProperty(p).into(),
            };
            (r, comp)
        }
        "SubDataPropertyOf" => {
            let (r, sub) = data_property(c, r)?;
            let (r, sup) = data_property(c, r)?;
            let (r, _) = rparen(r)?;
            (r, SubDataPropertyOf { sub, sup }.into())
        }
        "EquivalentDataProperties" => {
            let (r, v) = dp_list(c, r)?;
            (r, EquivalentDataProperties(v).into())
        }
        "DisjointDataProperties" => {
            let (r, v) = dp_list(c, r)?;
            (r, DisjointDataProperties(v).into())
        }
        "DataPropertyDomain" => {
            let (r, dp) = data_property(c, r)?;
            let (r, ce) = class_expression(c, r)?;
            let (r, _) = rparen(r)?;
            (r, DataPropertyDomain::new(dp, ce).into())
        }
        "DataPropertyRange" => {
            let (r, dp) = data_property(c, r)?;
            let (r, dr) = data_range(c, r)?;
            let (r, _) = rparen(r)?;
            (r, DataPropertyRange::new(dp, dr).into())
        }
        "FunctionalDataProperty" => {
            let (r, dp) = data_property(c, r)?;
            let (r, _) = rparen(r)?;
            (r, FunctionalDataProperty(dp).into())
        }
        "DatatypeDefinition" => {
            let (r, k) = datatype(c, r)?;
            let (r, dr) = data_range(c, r)?;
            let (r, _) = rparen(r)?;
            (r, DatatypeDefinition::new(k, dr).into())
        }
        "HasKey" => {
            let (r, ce) = class_expression(c, r)?;
            let (r, _) = lparen(r)?;
            let (r, opes) = list(r, 0, "object property", |i| {
                object_property_expression(c, i)
            })?;
            let (r, _) = lparen(r)?;
            let (r, dps) = list(r, 0, "data property", |i| data_property(c, i))?;
            let (r, _) = rparen(r)?;
            let vpe = opes
                .into_iter()
                .map(PropertyExpression::ObjectPropertyExpression)
                .chain(dps.into_iter().map(PropertyExpression::DataProperty))
                .collect();
            (r, HasKey::new(ce, vpe).into())
        }
        "SameIndividual" => {
            let (r, v) = individual_list(c, r)?;
            (r, SameIndividual(v).into())
        }
        "DifferentIndividuals" => {
            let (r, v) = individual_list(c, r)?;
            (r, DifferentIndividuals(v).into())
        }
        "ClassAssertion" => {
            let (r, ce) = class_expression(c, r)?;
            let (r, i) = individual(c, r)?;
            let (r, _) = rparen(r)?;
            (r, ClassAssertion::new(ce, i).into())
        }
        "ObjectPropertyAssertion" | "NegativeObjectPropertyAssertion" => {
            let (r, ope) = object_property_expression(c, r)?;
            let (r, from) = individual(c, r)?;
            let (r, to) = individual(c, r)?;
            let (r, _) = rparen(r)?;
            if kw == "ObjectPropertyAssertion" {
                (r, ObjectPropertyAssertion { ope, from, to }.into())
            } else {
                (
                    r,
                    NegativeObjectPropertyAssertion::new(ope, from, to).into(),
                )
            }
        }
        "DataPropertyAssertion" | "NegativeDataPropertyAssertion" => {
            let (r, dp) = data_property(c, r)?;
            let (r, from) = individual(c, r)?;
            let (r, to) = literal(c, r)?;
            let (r, _) = rparen(r)?;
            if kw == "DataPropertyAssertion" {
                (r, DataPropertyAssertion::new(dp, from, to).into())
            } else {
                (r, NegativeDataPropertyAssertion::new(dp, from, to).into())
            }
        }
        "AnnotationAssertion" => {
            let (r, ap) = annotation_property(c, r)?;
            let (r, subject) = annotation_subject(c, r)?;
            let (r, av) = annotation_value(c, r)?;
            let (r, _) = rparen(r)?;
            let a = Annotation {
                ap,
                av,
                ann: Default::default(),
            };
            (r, AnnotationAssertion::new(subject, a).into())
        }
        "SubAnnotationPropertyOf" => {
            let (r, sub) = annotation_property(c, r)?;
            let (r, sup) = annotation_property(c, r)?;
            let (r, _) = rparen(r)?;
            (r, SubAnnotationPropertyOf { sub, sup }.into())
        }
        "AnnotationPropertyDomain" => {
            let (r, ap) = annotation_property(c, r)?;
            let (r, x) = iri(c, r)?;
            let (r, _) = rparen(r)?;
            (r, AnnotationPropertyDomain::new(ap, x).into())
        }
        "AnnotationPropertyRange" => {
            let (r, ap) = annotation_property(c, r)?;
            let (r, x) = iri(c, r)?;
            let (r, _) = rparen(r)?;
            (r, AnnotationPropertyRange::new(ap, x).into())
        }
        "DLSafeRule" => {
            let (r, _) = expect_kw(r, "Body")?;
            let (r, body) = list(r, 0, "atom", |i| atom(c, i))?;
            let (r, _) = expect_kw(r, "Head")?;
            let (r, head) = list(r, 0, "atom", |i| atom(c, i))?;
            let (r, _) = rparen(r)?;
            (r, crate::model::Rule::new(head, body).into())
        }
        _ => unreachable!("is_axiom_keyword admitted {kw}"),
    };
    Ok((r, AnnotatedComponent::new(comp, ann)))
}

fn is_axiom_keyword(kw: &str) -> bool {
    matches!(
        kw,
        "Declaration"
            | "SubClassOf"
            | "EquivalentClasses"
            | "DisjointClasses"
            | "DisjointUnion"
            | "SubObjectPropertyOf"
            | "EquivalentObjectProperties"
            | "DisjointObjectProperties"
            | "ObjectPropertyDomain"
            | "ObjectPropertyRange"
            | "InverseObjectProperties"
            | "FunctionalObjectProperty"
            | "InverseFunctionalObjectProperty"
            | "ReflexiveObjectProperty"
            | "IrreflexiveObjectProperty"
            | "SymmetricObjectProperty"
            | "AsymmetricObjectProperty"
            | "TransitiveObjectProperty"
            | "SubDataPropertyOf"
            | "EquivalentDataProperties"
            | "DisjointDataProperties"
            | "DataPropertyDomain"
            | "DataPropertyRange"
            | "FunctionalDataProperty"
            | "DatatypeDefinition"
            | "HasKey"
            | "SameIndividual"
            | "DifferentIndividuals"
            | "ClassAssertion"
            | "ObjectPropertyAssertion"
            | "NegativeObjectPropertyAssertion"
            | "DataPropertyAssertion"
            | "NegativeDataPropertyAssertion"
            | "AnnotationAssertion"
            | "SubAnnotationPropertyOf"
            | "AnnotationPropertyDomain"
            | "AnnotationPropertyRange"
            | "DLSafeRule"
    )
}

// ---------------------------------------------------------------------------
// SWRL

fn variable<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Variable<A>> {
    let (r, kw) = expect_kw(i, "Variable").map(|(r, _)| (r, "Variable"))?;
    let (r, x) = within(kw, iri(c, r))?;
    let (r, _) = within(kw, rparen(r))?;
    Ok((r, Variable(x)))
}

fn iarg<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, IArgument<A>> {
    if peek_kw(i) == Some("Variable") {
        variable(c, i).map(|(r, x)| (r, IArgument::Variable(x)))
    } else {
        individual(c, i).map(|(r, x)| (r, IArgument::Individual(x)))
    }
}

fn darg<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, DArgument<A>> {
    if peek_kw(i) == Some("Variable") {
        variable(c, i).map(|(r, x)| (r, DArgument::Variable(x)))
    } else {
        literal(c, i).map(|(r, x)| (r, DArgument::Literal(x)))
    }
}

fn atom<'a, A: ForIRI>(c: &Ctx<'_, A>, i: &'a str) -> R<'a, Atom<A>> {
    let i = sp(i);
    let (r, kw) = keyword(i)?;
    within(kw, atom_body(c, kw, r, i))
}

fn atom_body<'a, A: ForIRI>(
    c: &Ctx<'_, A>,
    kw: &'a str,
    r: &'a str,
    start: &'a str,
) -> R<'a, Atom<A>> {
    match kw {
        "ClassAtom" => {
            let (r, pred) = class_expression(c, r)?;
            let (r, arg) = iarg(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, Atom::ClassAtom { pred, arg }))
        }
        "DataRangeAtom" => {
            let (r, pred) = data_range(c, r)?;
            let (r, arg) = darg(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, Atom::DataRangeAtom { pred, arg }))
        }
        "ObjectPropertyAtom" => {
            let (r, pred) = object_property_expression(c, r)?;
            let (r, a) = iarg(c, r)?;
            let (r, b) = iarg(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, Atom::ObjectPropertyAtom { pred, args: (a, b) }))
        }
        "DataPropertyAtom" => {
            let (r, pred) = data_property(c, r)?;
            let (r, a) = darg(c, r)?;
            let (r, b) = darg(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, Atom::DataPropertyAtom { pred, args: (a, b) }))
        }
        "BuiltInAtom" => {
            let (r, pred) = iri(c, r)?;
            let (r, args) = list(r, 1, "argument", |i| darg(c, i))?;
            Ok((r, Atom::BuiltInAtom { pred, args }))
        }
        "SameIndividualAtom" => {
            let (r, a) = iarg(c, r)?;
            let (r, b) = iarg(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, Atom::SameIndividualAtom(a, b)))
        }
        "DifferentIndividualsAtom" => {
            let (r, a) = iarg(c, r)?;
            let (r, b) = iarg(c, r)?;
            let (r, _) = rparen(r)?;
            Ok((r, Atom::DifferentIndividualsAtom(a, b)))
        }
        _ => unknown("atom", &start[..kw.len()]),
    }
}

#[cfg(test)]
mod tests;
