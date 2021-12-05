use curie::PrefixMapping;

use crate::model::*;
use crate::vocab::Namespace::*;
use crate::vocab::OWL2Datatype;
use crate::vocab::WithIRI;
use crate::{ontology::set::SetOntology, vocab::OWL};

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::io::BufRead;

use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::Event;
use quick_xml::Reader;

use failure::Error;

#[derive(Debug, Fail)]
enum ReadError {
    #[fail(display = "End Tag Arrived Unexpectedly: {} at {}", tag, pos)]
    UnexpectedEndTag { tag: String, pos: usize },

    #[fail(display = "Missing End Tag: expected {} after {}", tag, pos)]
    MissingEndTag { tag: String, pos: usize },

    #[fail(display = "Missing element: Expected {} at {}", tag, pos)]
    MissingElement { tag: String, pos: usize },

    #[fail(display = "Missing attribute: Expected {} at {}", attribute, pos)]
    MissingAttribute { attribute: String, pos: usize },

    #[fail(
        display = "Unknown Entity: Expected Kind of {}, found {} at {}",
        kind, found, pos
    )]
    UnknownEntity {
        kind: String,
        found: String,
        pos: usize,
    },

    #[fail(display = "Unexpected Tag: found {} at {}", tag, pos)]
    UnexpectedTag { tag: String, pos: usize },

    #[fail(display = "Unexpected End of File: {}", pos)]
    UnexpectedEof { pos: usize },
}

struct Read<'a, R>
where
    R: BufRead,
{
    build: &'a Build,
    mapping: PrefixMapping,
    reader: Reader<R>,
    buf: Vec<u8>,
    ns_buf: Vec<u8>,
}

pub fn read<R: BufRead>(bufread: &mut R) -> Result<(SetOntology, PrefixMapping), Error> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

pub fn read_with_build<R: BufRead>(
    bufread: &mut R,
    build: &Build,
) -> Result<(SetOntology, PrefixMapping), Error> {
    let reader: Reader<&mut R> = Reader::from_reader(bufread);
    let mut ont = SetOntology::default();
    let mapping = PrefixMapping::default();

    let mut r = Read {
        reader,
        build,
        mapping,
        buf: Vec::new(),
        ns_buf: Vec::new(),
    };

    loop {
        match read_event(&mut r)? {
            (ref ns, Event::Start(ref e)) | (ref ns, Event::Empty(ref e))
                if *ns == b"http://www.w3.org/2002/07/owl#" =>
            {
                match e.local_name() {
                    b"Ontology" => {
                        let s = attrib_value(&mut r, e, b"ontologyIRI")?;
                        if s.is_some() {
                            r.mapping.set_default(&s.unwrap());
                        }

                        ont.mut_id().iri = read_a_iri_attr(&mut r, e, b"ontologyIRI")?;
                        ont.mut_id().viri = read_a_iri_attr(&mut r, e, b"versionIRI")?;
                    }
                    b"Prefix" => {
                        let iri = attrib_value(&mut r, e, b"IRI")?;
                        let prefix = attrib_value(&mut r, e, b"name")?;
                        match (prefix, iri) {
                            (Some(p), Some(i)) => {
                                r.mapping.add_prefix(&p, &i).ok();
                            }
                            (None, _) => {
                                return Err(error_missing_attribute("IRI", &mut r));
                            }
                            (Some(_), None) => {
                                return Err(error_missing_attribute("name", &mut r));
                            }
                        }
                    }
                    b"Import" => {
                        ont.insert(Import(IRI::from_xml(&mut r, b"Import")?));
                    }
                    _ => {
                        let aa = AnnotatedAxiom::from_start(&mut r, e)?;
                        ont.insert(aa);
                    }
                }
            }
            (ref ns, Event::End(ref e)) if is_owl_name(ns, e, b"Ontology") => {
                break;
            }
            _ => {}
        }
    }
    Ok((ont, r.mapping))
}

/// Read an event from the reader, which is unowned.
///
/// This method is here because it allows me to nest self called,
/// inside a match on the event; the event otherwise keeps a
/// mutable borrow out which prevents these calls. This problem
/// happens because of the design of quick-xml. Nested calls
/// cannot work because we have to pass `buf` and `ns_buf`.
///
/// So, we use this solution instead; the worry is that this will
/// be inefficient because it removes the zero-copy promise of
/// quick-xml. I will not worry about this, however, because when
/// non-lexical lifetimes appears, it should be possible to make
/// this a straight alias for `read_namespaced_event`, and still
/// have it all work.
fn read_event<R: BufRead>(read: &mut Read<R>) -> Result<(Vec<u8>, Event<'static>), Error> {
    let r = read
        .reader
        .read_namespaced_event(&mut read.buf, &mut read.ns_buf);

    match r {
        Ok((_, Event::Eof)) => Err(ReadError::UnexpectedEof {
            pos: read.reader.buffer_position(),
        }
        .into()),
        Ok((option_ns, event)) => Ok((option_ns.unwrap_or(b"").to_owned(), event.into_owned())),
        Err(r) => Err(r.into()),
    }
}

fn decode_expand_curie_maybe<R: BufRead>(r: &mut Read<R>, val: &[u8]) -> String {
    let s = r.reader.decode(val).into_owned();
    expand_curie_maybe(r, s)
}

/// Expand a curie if there is an appropriate prefix
fn expand_curie_maybe<R: BufRead>(r: &mut Read<R>, val: String) -> String {
    match r.mapping.expand_curie_string(&val) {
        // If we expand use this
        Ok(n) => n,
        // Else assume it's a complete URI
        Err(_e) => val,
    }
}

fn attrib_value_b<'a>(event: &'a BytesStart, tag: &[u8]) -> Result<Option<Cow<'a, [u8]>>, Error> {
    for res in event.attributes() {
        let attrib = res?;
        if attrib.key == tag {
            return Ok(Some(attrib.value));
        }
    }

    Ok(None)
}

fn attrib_value<R: BufRead>(
    r: &mut Read<R>,
    event: &BytesStart,
    tag: &[u8],
) -> Result<Option<String>, Error> {
    attrib_value_b(event, tag).map(|res| res.map(|val| r.reader.decode(&val).into_owned()))
}

fn read_iri_attr<R: BufRead>(r: &mut Read<R>, event: &BytesStart) -> Result<Option<IRI>, Error> {
    let iri = read_a_iri_attr(r, event, b"IRI")?;
    Ok(if iri.is_some() {
        iri
    } else {
        read_a_iri_attr(r, event, b"abbreviatedIRI")?
    })
}

fn read_a_iri_attr<R: BufRead>(
    r: &mut Read<R>,
    event: &BytesStart,
    tag: &[u8],
) -> Result<Option<IRI>, Error> {
    Ok(
        // check for the attrib, if malformed return
        attrib_value(r, event, tag)?.
        // or transform the some String
            map(|st|
                // Into an iri
                r.build.iri(
                    // or a curie
                    expand_curie_maybe(r, st))),
    )
}

fn error_missing_end_tag<R: BufRead>(tag: &[u8], r: &mut Read<R>, pos: usize) -> Error {
    ReadError::MissingEndTag {
        tag: r.reader.decode(tag).into_owned(),
        pos,
    }
    .into()
}

fn error_missing_attribute<A: Into<String>, R: BufRead>(attribute: A, r: &mut Read<R>) -> Error {
    ReadError::MissingAttribute {
        attribute: attribute.into(),
        pos: r.reader.buffer_position(),
    }
    .into()
}

fn error_unexpected_tag<R: BufRead>(tag: &[u8], r: &mut Read<R>) -> Error {
    ReadError::UnexpectedTag {
        tag: r.reader.decode(tag).into_owned(),
        pos: r.reader.buffer_position(),
    }
    .into()
}

fn error_unexpected_end_tag<R: BufRead>(tag: &[u8], r: &mut Read<R>) -> Error {
    ReadError::UnexpectedEndTag {
        tag: r.reader.decode(tag).into_owned(),
        pos: r.reader.buffer_position(),
    }
    .into()
}

fn error_unknown_entity<A: Into<String>, R: BufRead>(
    kind: A,
    found: &[u8],
    r: &mut Read<R>,
) -> Error {
    ReadError::UnknownEntity {
        kind: kind.into(),
        found: r.reader.decode(found).into_owned(),
        pos: r.reader.buffer_position(),
    }
    .into()
}

fn error_missing_element<R: BufRead>(tag: &[u8], r: &mut Read<R>) -> Error {
    ReadError::MissingElement {
        tag: r.reader.decode(tag).into_owned(),
        pos: r.reader.buffer_position(),
    }
    .into()
}

fn is_owl(ns: &[u8]) -> bool {
    ns == OWL.iri_b()
}

fn is_owl_name(ns: &[u8], e: &BytesEnd, tag: &[u8]) -> bool {
    is_owl(ns) && e.local_name() == tag
}

trait FromStart: Sized {
    fn from_start<R: BufRead>(r: &mut Read<R>, e: &BytesStart) -> Result<Self, Error>;
}

macro_rules! from_start {
    ($type:ident, $r:ident, $e:ident, $body:tt) => {
        impl FromStart for $type {
            fn from_start<R: BufRead>($r: &mut Read<R>, $e: &BytesStart) -> Result<$type, Error> {
                $body
            }
        }
    };
}

/// Potentially unbalanced
fn named_entity_from_start<R, T>(r: &mut Read<R>, e: &BytesStart, tag: &[u8]) -> Result<T, Error>
where
    R: BufRead,
    T: From<IRI>,
{
    if let Some(iri) = read_iri_attr(r, e)? {
        if e.local_name() == tag {
            return Ok(T::from(iri));
        } else {
            return Err(error_unknown_entity(
                ::std::str::from_utf8(tag).unwrap(),
                e.local_name(),
                r,
            ));
        }
    }

    Err(error_missing_element(b"IRI", r))
}

fn from_start<R: BufRead, T: FromStart>(r: &mut Read<R>, e: &BytesStart) -> Result<T, Error> {
    T::from_start(r, e)
}

from_start! {
    Literal, r, e,
    {
        let datatype_iri = read_a_iri_attr(r, e, b"datatypeIRI")?;
        let lang = attrib_value(r, e, b"xml:lang")?;

        let literal = r.reader.read_text(b"Literal", &mut Vec::new())?;
        Ok(
            match (datatype_iri, lang, literal) {
                (None, None, literal) =>
                    Literal::Simple{literal},
                (Some(ref datatype_iri), None, literal)
                    if **datatype_iri == *"http://www.w3.org/2001/XMLSchema#string" =>
                    Literal::Simple{literal},
                (None, Some(lang), literal) =>
                    Literal::Language{literal, lang},
                (Some(ref datatype_iri), Some(ref lang), ref literal)
                    if **datatype_iri == *"http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral"
                    => Literal::Language{literal:literal.to_string(), lang:lang.to_string()},
                (Some(_), Some(_), _)
                    => bail!("Literal with language tag and incorrect datatype"),
                (Some(datatype_iri), None, literal)
                    => Literal::Datatype{literal, datatype_iri},
            })
    }
}

from_start! {
    AnnotationValue, r, e, {
        Ok(
            match e.local_name() {
                b"Literal" => {
                    Literal::from_start(r, e)?
                    .into()
                }
                b"AbbreviatedIRI"|b"IRI" => {
                    IRI::from_xml(r, e.local_name())?
                    .into()
                }
                _ => {
                    return Err
                        (error_unexpected_tag(e.local_name(), r));
                }
            }
        )
    }
}

from_start! {
    AnnotationProperty, r, e,
    {
        named_entity_from_start(r, e, b"AnnotationProperty")
    }
}

fn axiom_from_start<R: BufRead>(
    r: &mut Read<R>,
    e: &BytesStart,
    axiom_kind: &[u8],
) -> Result<Axiom, Error> {
    Ok(match axiom_kind {
        b"Annotation" => OntologyAnnotation(Annotation {
            ap: from_start(r, e)?,
            av: from_next(r)?,
        })
        .into(),
        b"Declaration" => {
            let ne: NamedEntity = from_start(r, e)?;
            ne.into()
        }
        b"SubClassOf" => SubClassOf {
            sub: from_start(r, e)?,
            sup: from_next(r)?,
        }
        .into(),
        b"EquivalentClasses" => {
            EquivalentClasses(from_start_to_end(r, e, b"EquivalentClasses")?).into()
        }
        b"DisjointClasses" => DisjointClasses(from_start_to_end(r, e, b"DisjointClasses")?).into(),
        b"DisjointUnion" => DisjointUnion(from_start(r, e)?, till_end(r, b"DisjointUnion")?).into(),
        b"SubObjectPropertyOf" => SubObjectPropertyOf {
            sub: from_start(r, e)?,
            sup: from_next(r)?,
        }
        .into(),
        b"EquivalentObjectProperties" => {
            EquivalentObjectProperties(from_start_to_end(r, e, b"EquivalentObjectProperties")?)
                .into()
        }
        b"DisjointObjectProperties" => {
            DisjointObjectProperties(from_start_to_end(r, e, b"DisjointObjectProperties")?).into()
        }
        b"InverseObjectProperties" => {
            InverseObjectProperties(from_start(r, e)?, from_next(r)?).into()
        }
        b"ObjectPropertyDomain" => ObjectPropertyDomain {
            ope: from_start(r, e)?,
            ce: from_next(r)?,
        }
        .into(),
        b"ObjectPropertyRange" => ObjectPropertyRange {
            ope: from_start(r, e)?,
            ce: from_next(r)?,
        }
        .into(),
        b"FunctionalObjectProperty" => FunctionalObjectProperty(from_start(r, e)?).into(),
        b"InverseFunctionalObjectProperty" => {
            InverseFunctionalObjectProperty(from_start(r, e)?).into()
        }
        b"ReflexiveObjectProperty" => ReflexiveObjectProperty(from_start(r, e)?).into(),
        b"IrreflexiveObjectProperty" => IrreflexiveObjectProperty(from_start(r, e)?).into(),
        b"SymmetricObjectProperty" => SymmetricObjectProperty(from_start(r, e)?).into(),
        b"AsymmetricObjectProperty" => AsymmetricObjectProperty(from_start(r, e)?).into(),
        b"TransitiveObjectProperty" => TransitiveObjectProperty(from_start(r, e)?).into(),
        b"SubDataPropertyOf" => SubDataPropertyOf {
            sub: from_start(r, e)?,
            sup: from_next(r)?,
        }
        .into(),
        b"EquivalentDataProperties" => {
            EquivalentDataProperties(from_start_to_end(r, e, b"EquivalentDataProperties")?).into()
        }
        b"DisjointDataProperties" => {
            DisjointDataProperties(from_start_to_end(r, e, b"DisjointDataProperties")?).into()
        }
        b"DataPropertyDomain" => DataPropertyDomain {
            dp: from_start(r, e)?,
            ce: from_next(r)?,
        }
        .into(),
        b"DataPropertyRange" => DataPropertyRange {
            dp: from_start(r, e)?,
            dr: from_next(r)?,
        }
        .into(),
        b"FunctionalDataProperty" => FunctionalDataProperty(from_start(r, e)?).into(),
        b"DatatypeDefinition" => DatatypeDefinition {
            kind: from_start(r, e)?,
            range: from_next(r)?,
        }
        .into(),
        b"HasKey" => HasKey {
            ce: from_start(r, e)?,
            vpe: till_end(r, b"HasKey")?,
        }
        .into(),
        b"SameIndividual" => SameIndividual(from_start_to_end(r, e, b"SameIndividual")?).into(),
        b"DifferentIndividuals" => {
            DifferentIndividuals(from_start_to_end(r, e, b"DifferentIndividuals")?).into()
        }
        b"ClassAssertion" => ClassAssertion {
            ce: from_start(r, e)?,
            i: from_next(r)?,
        }
        .into(),
        b"ObjectPropertyAssertion" => ObjectPropertyAssertion {
            ope: from_start(r, e)?,
            from: from_next(r)?,
            to: from_next(r)?,
        }
        .into(),
        b"NegativeObjectPropertyAssertion" => NegativeObjectPropertyAssertion {
            ope: from_start(r, e)?,
            from: from_next(r)?,
            to: from_next(r)?,
        }
        .into(),
        b"DataPropertyAssertion" => DataPropertyAssertion {
            dp: from_start(r, e)?,
            from: from_next(r)?,
            to: from_next(r)?,
        }
        .into(),
        b"NegativeDataPropertyAssertion" => NegativeDataPropertyAssertion {
            dp: from_start(r, e)?,
            from: from_next(r)?,
            to: from_next(r)?,
        }
        .into(),
        b"AnnotationAssertion" => {
            let ap = from_start(r, e)?;
            let subject = from_next(r)?;
            let av = from_next(r)?;

            AnnotationAssertion {
                subject,
                ann: Annotation { ap, av },
            }
            .into()
        }
        b"SubAnnotationPropertyOf" => SubAnnotationPropertyOf {
            sub: from_start(r, e)?,
            sup: from_next(r)?,
        }
        .into(),
        b"AnnotationPropertyDomain" => AnnotationPropertyDomain {
            ap: from_start(r, e)?,
            iri: from_next(r)?,
        }
        .into(),
        b"AnnotationPropertyRange" => AnnotationPropertyRange {
            ap: from_start(r, e)?,
            iri: from_next(r)?,
        }
        .into(),
        _ => {
            return Err(error_unexpected_tag(axiom_kind, r));
        }
    })
}

fn from_start_to_end<R: BufRead, T: FromStart + std::fmt::Debug>(
    r: &mut Read<R>,
    e: &BytesStart,
    end_tag: &[u8],
) -> Result<Vec<T>, Error> {
    let mut v = Vec::new();
    v.push(from_start(r, e)?);
    till_end_with(r, end_tag, v)
}

// Keep reading entities, till end_tag is reached
fn till_end<R: BufRead, T: FromStart + std::fmt::Debug>(r: &mut Read<R>, end_tag: &[u8]) -> Result<Vec<T>, Error> {
    let operands: Vec<T> = Vec::new();
    till_end_with(r, end_tag, operands)
}

// Keep reading entities, till end_tag is reached
fn till_end_with<R: BufRead, T: FromStart + std::fmt::Debug>(
    r: &mut Read<R>,
    end_tag: &[u8],
    mut operands: Vec<T>,
) -> Result<Vec<T>, Error> {
    loop {
        let e = read_event(r)?;
        match e {
            (ref ns, Event::Empty(ref e)) if is_owl(ns) => {
                let op = from_start(r, e)?;
                operands.push(op);
            }
            (ref ns, Event::Start(ref e)) if is_owl(ns) => {
                let op = from_start(r, e)?;
                operands.push(op);
            }
            (ref ns, Event::End(ref e)) if is_owl_name(ns, e, end_tag) => {
                return Ok(operands);
            }
            _ => {}
        }
    }
}

fn object_cardinality_restriction<R: BufRead>(
    r: &mut Read<R>,
    e: &BytesStart,
    end_tag: &[u8],
) -> Result<(u32, ObjectPropertyExpression, Box<ClassExpression>), Error> {
    let n = attrib_value(r, e, b"cardinality")?;
    let n = n.ok_or_else(|| error_missing_attribute("cardinality", r))?;

    let ope = from_next(r)?;
    let mut vce: Vec<ClassExpression> = till_end(r, end_tag)?;

    Ok((
        n.parse::<u32>()?,
        ope,
        Box::new(match vce.len() {
            0 => r.build.class(OWL::Thing.iri_s().to_string()).into(),
            1 => vce.remove(0),
            _ => Err(error_unexpected_tag(end_tag, r))?,
        }),
    ))
}

fn data_cardinality_restriction<R: BufRead>(
    r: &mut Read<R>,
    e: &BytesStart,
    end_tag: &[u8],
) -> Result<(u32, DataProperty, DataRange), Error> {
    let n = attrib_value(r, e, b"cardinality")?;
    let n = n.ok_or_else(|| (error_missing_attribute("cardinality", r)))?;

    let dp = from_next(r)?;
    let mut vdr: Vec<DataRange> = till_end(r, end_tag)?;

    Ok((
        n.parse::<u32>()?,
        dp,
        match vdr.len() {
            0 => r
                .build
                .datatype(OWL2Datatype::RDFSLiteral.iri_s().to_string())
                .into(),
            1 => vdr.remove(0),
            _ => Err(error_unexpected_tag(end_tag, r))?,
        },
    ))
}

from_start! {
    PropertyExpression, r, e,
    {
        Ok(
            match e.local_name() {
                b"ObjectProperty" |
                b"ObjectInverseOf" => {
                    PropertyExpression::ObjectPropertyExpression
                        (from_start(r, e)?)
                }
                b"DataProperty" => {
                    PropertyExpression::DataProperty(
                        from_start(r, e)?
                    )
                }
                _ => {
                    return Err(error_unknown_entity("PropertyExpression",
                                                    e.local_name(), r))
                }
            })
    }
}

from_start! {
    ClassExpression, r, e, {
        Ok(
            match e.local_name() {
                b"Class" => {
                    Class::from_start(r, e)?.into()
                }
                b"ObjectSomeValuesFrom" => {
                    let ope = from_next(r)?;
                    let bce = Box::new(from_next(r)?);
                    ClassExpression::ObjectSomeValuesFrom{ope,bce}
                }
                b"ObjectAllValuesFrom" => {
                    let ope = from_next(r)?;
                    let bce = Box::new(from_next(r)?);
                    ClassExpression::ObjectAllValuesFrom{ope,bce}
                }
                b"ObjectIntersectionOf" => {
                    let o = till_end(r, b"ObjectIntersectionOf")?;
                    ClassExpression::ObjectIntersectionOf(o)
                }
                b"ObjectUnionOf" => {
                    let o = till_end(r, b"ObjectUnionOf")?;
                    ClassExpression::ObjectUnionOf(o)
                }
                b"ObjectComplementOf" => {
                    ClassExpression::ObjectComplementOf
                        (Box::new(from_next(r)?))
                }
                b"ObjectHasValue" => {
                    ClassExpression::ObjectHasValue
                    {
                        ope:from_next(r)?,
                        i:from_next(r)?
                    }
                }
                b"ObjectOneOf" => {
                    ClassExpression::ObjectOneOf(till_end(r, b"ObjectOneOf")?)
                }
                b"ObjectHasSelf" => {
                    ClassExpression::ObjectHasSelf
                        (from_next(r)?)
                }
                b"ObjectMinCardinality" => {
                    let (n, ope, bce) = object_cardinality_restriction
                        (r, e, b"ObjectMinCardinality")?;
                    ClassExpression::ObjectMinCardinality{n, ope, bce}
                }
                b"ObjectMaxCardinality" => {
                    let (n, ope, bce) = object_cardinality_restriction
                        (r, e, b"ObjectMaxCardinality")?;
                    ClassExpression::ObjectMaxCardinality{n, ope, bce}
                }
                b"ObjectExactCardinality" => {
                    let (n, ope, bce) = object_cardinality_restriction
                        (r, e, b"ObjectExactCardinality")?;
                    ClassExpression::ObjectExactCardinality{n, ope, bce}
                }
                b"DataSomeValuesFrom" => {
                    ClassExpression::DataSomeValuesFrom{
                        dp:from_next(r)?,
                        dr:from_next(r)?
                    }
                }
                b"DataAllValuesFrom" => {
                    ClassExpression::DataAllValuesFrom{
                        dp:from_next(r)?,
                        dr:from_next(r)?
                    }
                }
                b"DataHasValue" => {
                    ClassExpression::DataHasValue {
                        dp:from_next(r)?,
                        l:from_next(r)?
                    }
                }
                b"DataMinCardinality" => {
                    let (n, dp, dr) = data_cardinality_restriction
                        (r, e, b"DataMinCardinality")?;
                    ClassExpression::DataMinCardinality{n, dp, dr}
                }
                b"DataMaxCardinality" => {
                    let (n, dp, dr) = data_cardinality_restriction
                        (r, e, b"DataMaxCardinality")?;
                    ClassExpression::DataMaxCardinality{n, dp, dr}
                }
                b"DataExactCardinality" => {
                    let (n, dp, dr) = data_cardinality_restriction
                        (r, e, b"DataExactCardinality")?;
                    ClassExpression::DataExactCardinality{n, dp, dr}
                }
                _ => {
                    return Err(error_unexpected_tag(e.local_name(), r));
                }
            }
        )
    }
}

from_start! {
    AnnotatedAxiom, r, e,
    {
        let mut annotation: BTreeSet<Annotation> = BTreeSet::new();
        let axiom_kind:&[u8] = e.local_name();

        loop {
            let e = read_event(r)?;
            match e {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if is_owl(ns) =>
                {
                    match e.local_name() {
                        b"Annotation" => {
                            annotation.insert
                                (Annotation::from_xml(r, b"Annotation")?);
                        }
                        _ => {
                            return Ok(AnnotatedAxiom{
                                ann:annotation,
                                axiom:axiom_from_start(r,e,axiom_kind)?
                            });
                        }
                    }
                }
                (ref ns, Event::End(ref e))
                    if is_owl_name(ns, e, axiom_kind) =>
                {
                    return Err(error_unexpected_end_tag(axiom_kind, r));
                },
                _=>{
                }
            }
        }
    }
}

from_start! {
    Class, r, e,
    {
        named_entity_from_start(r, e, b"Class")
    }
}

from_start! {
    ObjectProperty, r, e,
    {
        named_entity_from_start(r, e, b"ObjectProperty")
    }
}

from_start! {
    DataProperty, r, e,
    {
        named_entity_from_start(r, e, b"DataProperty")
    }
}

from_start! {
    Individual,r, e,
    {
        match e.local_name() {
            b"AnonymousIndividual" =>{
                eprintln!("About to read anonymous");
                let ai:AnonymousIndividual = from_start(r, e)?;
                eprintln!("Read anonymous");
                Ok(ai.into())
            }
            b"NamedIndividual" =>{
                let ni:NamedIndividual = from_start(r, e)?;
                Ok(ni.into())
            }
            b"IRI" | b"AbbreviatedIRI" => {
                let iri:IRI = from_start(r, e)?;
                let ni:NamedIndividual = iri.into();
                Ok(ni.into())
            }
            l => {
                todo!("{:?}",std::str::from_utf8(l))
            }
        }
    }
}

from_start! {
    AnonymousIndividual, r, e,
    {
        let ai:AnonymousIndividual =
            attrib_value(r, e, b"nodeID")?.ok_or(
                error_missing_attribute("nodeID Expected", r)
            )?.into();
        Ok(ai)
    }
}

from_start! {
    NamedIndividual, r, e,
    {
        named_entity_from_start(r, e, b"NamedIndividual")
    }
}

from_start! {
    Datatype, r, e,
    {
        named_entity_from_start(r, e, b"Datatype")
    }
}

from_start! {
    ObjectPropertyExpression, r, e,
    {
        Ok(
            match e.local_name() {
                b"ObjectProperty" => {
                    ObjectPropertyExpression::ObjectProperty
                        (from_start(r, e)?)
                }
                b"ObjectInverseOf" => {
                    ObjectPropertyExpression::InverseObjectProperty
                        (from_next(r)?)
                }
                _ => {
                    return Err(error_unknown_entity
                                ("Object Property Expression",
                                 e.local_name(), r));
                }
            }
        )
    }
}

from_start! {
    SubObjectPropertyExpression, r, e,
    {
        Ok(
            match e.local_name() {
                b"ObjectPropertyChain" => {
                    let o = till_end(r, b"ObjectPropertyChain")?;
                    SubObjectPropertyExpression::ObjectPropertyChain(o)

                }
                b"ObjectProperty" => {
                    SubObjectPropertyExpression::
                    ObjectPropertyExpression(from_start(r, e)?)
                }
                _ => {
                    return Err(error_unknown_entity("Sub Object Property",
                                                    e.local_name(),
                                                    r));
                }
            }
        )
    }
}

from_start! {
    FacetRestriction, r, e,
    {
        let f = attrib_value_b(e, b"facet")?;
        let f = f.ok_or_else(
            || error_missing_attribute("facet", r)
        )?;

        Ok(
            FacetRestriction {
                f: Facet::var_b(&f)
                    .ok_or_else(
                        || error_unknown_entity("facet", &f, r))?,
                l: from_next(r)?
            }
        )
    }
}

from_start! {
    DataRange, r, e,
    {
        Ok(
            match e.local_name() {
                b"Datatype" => {
                    DataRange::Datatype(
                        from_start(r, e)?
                    )
                }
                b"DataIntersectionOf" => {
                    DataRange::DataIntersectionOf(
                        till_end(r, b"DataIntersectionOf")?
                    )
                }
                b"DataComplementOf" => {
                    DataRange::DataComplementOf(
                        Box::new(from_next(r)?)
                    )
                }
                b"DataOneOf" => {
                    DataRange::DataOneOf(
                        till_end(r, b"DataOneOf")?
                    )
                }
                b"DatatypeRestriction" => {
                    DataRange::DatatypeRestriction (
                        from_next(r)?,
                        till_end(r, b"DatatypeRestriction")?
                    )
                }
                _=> {
                    return Err(error_unknown_entity("DataRange",
                                                    e.local_name(),r ));
                }
            }
        )
    }
}

from_start! {
    NamedEntity, r, e,
    {
        Ok(
            match e.local_name() {
                b"Class" => {
                    Class::from_start(r, e)?.into()
                },
                b"ObjectProperty" => {
                    ObjectProperty::from_start(r, e)?.into()
                }
                b"AnnotationProperty" => {
                    AnnotationProperty::from_start(r, e)?.into()
                }
                b"DataProperty" => {
                    DataProperty::from_start(r, e)?.into()
                }
                b"NamedIndividual" => {
                    NamedIndividual::from_start(r, e)?.into()
                }
                b"Datatype" => {
                    Datatype::from_start(r, e)?.into()
                }
                _=> {
                    return Err(error_unknown_entity("NamedEntity",
                                                    e.local_name(),r ));
                }
            }
        )
    }
}

trait FromXML: Sized {
    fn from_xml<R: BufRead>(newread: &mut Read<R>, end_tag: &[u8]) -> Result<Self, Error> {
        let s = Self::from_xml_nc(newread, end_tag);
        newread.buf.clear();
        s
    }

    fn from_xml_nc<R: BufRead>(newread: &mut Read<R>, end_tag: &[u8]) -> Result<Self, Error>;
}

macro_rules! from_xml {
    ($type:ident, $r:ident, $end:ident, $body:tt) => {
        impl FromXML for $type {
            fn from_xml_nc<R: BufRead>($r: &mut Read<R>, $end: &[u8]) -> Result<$type, Error> {
                $body
            }
        }
    };
}

from_xml! {
    Annotation, r, end,
    {

        let mut ap:Option<AnnotationProperty> = None;
        let mut av:Option<AnnotationValue> = None;

        loop {
            let e = read_event(r)?;
            match e {
                (ref ns, Event::Start(ref e))
                |
                (ref ns, Event::Empty(ref e))
                    if is_owl(ns) =>
                {
                    match e.local_name() {
                        b"AnnotationProperty" =>
                            ap = Some(from_start(r, e)?),
                        _ =>
                            av = Some(from_start(r, e)?),
                    }
                }
                (ref ns, Event::End(ref e))
                    if is_owl_name(ns, e, end) =>
                {
                    if ap.is_none() || av.is_none() {
                        return Err(error_unexpected_end_tag(end, r));
                    }
                    return Ok(Annotation{
                        ap:ap.unwrap(),
                        av:av.unwrap()
                    });
                },
                _ =>{}
            }
        }
    }

}

fn from_next<R: BufRead, T: FromStart>(r: &mut Read<R>) -> Result<T, Error> {
    loop {
        let e = read_event(r)?;
        match e {
            (ref ns, Event::Empty(ref e)) | (ref ns, Event::Start(ref e)) if is_owl(ns) => {
                return from_start(r, e);
            }
            _ => {}
        }
    }
}

fn discard_till<R: BufRead>(r: &mut Read<R>, end: &[u8]) -> Result<(), Error> {
    let pos = r.reader.buffer_position();
    loop {
        let e = read_event(r)?;

        match e {
            (ref ns, Event::End(ref e)) if is_owl_name(ns, e, end) => {
                return Ok(());
            }
            (_, Event::Eof) => {
                return Err(error_missing_end_tag(end, r, pos));
            }
            _ => {}
        }
    }
}

from_xml! {
    NamedEntity,r, end,
    {
        let ne = from_next(r);
        discard_till(r, end)?;
        ne
    }
}

from_start! {
    IRI, r, e,
    {
        Self::from_xml(r, e.local_name())
    }
}

from_xml! {IRI, r, end,
        {
            let mut iri: Option<IRI> = None;
            loop {
                let e = read_event(r)?;
                match e {
                    (ref _ns,Event::Text(ref e)) => {
                        iri = Some(r.build.iri
                                   (decode_expand_curie_maybe(r, e)));
                    },
                    (ref ns, Event::End(ref e))
                        if is_owl_name(ns, e, end) =>
                    {
                        return iri.ok_or_else(
                            || error_unexpected_end_tag(end, r)                        );
                    },
                    _=>{}
                }
            }
        }
}

#[cfg(test)]
pub mod test {
    use super::*;
    use crate::ontology::axiom_mapped::AxiomMappedOntology;
    use std::collections::HashMap;

    pub fn read_ok<R: BufRead>(bufread: &mut R) -> (AxiomMappedOntology, PrefixMapping) {
        let r = read(bufread);
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        let (o, m) = r.ok().unwrap();
        (o.into(), m)
    }

    #[test]
    fn test_simple_ontology_prefix() {
        let ont_s = include_str!("../../ont/owl-xml/ont.owx");
        let (_, mapping) = read_ok(&mut ont_s.as_bytes());

        let hash_map: HashMap<&String, &String> = mapping.mappings().collect();
        assert_eq!(6, hash_map.len());
    }

    #[test]
    fn test_simple_ontology() {
        let ont_s = include_str!("../../ont/owl-xml/ont.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        assert_eq!(
            ont.id().iri.as_ref().unwrap().as_ref(),
            "http://www.example.com/iri"
        );
    }

    #[test]
    fn test_simple_ontology_rendered_by_horned() {
        let ont_s = include_str!("../../ont/owl-xml/one-ont-from-horned.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(
            ont.id().iri.as_ref().unwrap().as_ref(),
            "http://example.com/iri"
        );
    }

    #[test]
    fn test_class() {
        let ont_s = include_str!("../../ont/owl-xml/class.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_class().count(), 1);
        assert_eq!(
            String::from(&ont.i().declare_class().next().unwrap().0),
            "http://www.example.com/iri#C"
        );
    }

    #[test]
    fn test_class_with_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/declaration-with-annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_class().count(), 1);

        let aa = ont
            .i()
            .annotated_axiom(AxiomKind::DeclareClass)
            .next()
            .unwrap();

        assert_eq!(aa.ann.len(), 1);

        let ann = aa.ann.iter().next().unwrap();
        assert_eq!(
            String::from(&ann.ap),
            "http://www.w3.org/2000/01/rdf-schema#comment"
        );
    }

    #[test]
    fn class_with_two_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/class_with_two_annotations.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_class().count(), 1);

        assert_eq!(ont.i().annotation_assertion().count(), 2);

        let aa = ont.i().annotation_assertion().next().unwrap();
        assert_eq!(*(aa.subject), *"http://www.example.com/iri#C");

        assert_eq!(
            String::from(&aa.ann.ap),
            "http://www.w3.org/2000/01/rdf-schema#comment"
        );
    }

    #[test]
    fn test_one_class_fqn() {
        let ont_s = include_str!("../../ont/owl-xml/one-class-fully-qualified.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_class().count(), 1);
        assert_eq!(
            String::from(&ont.i().declare_class().next().unwrap().0),
            "http://www.russet.org.uk/#C"
        );
    }

    #[test]
    fn test_ten_class() {
        let ont_s = include_str!("../../ont/owl-xml/o10.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_class().count(), 10);
    }

    #[test]
    fn test_oproperty() {
        let ont_s = include_str!("../../ont/owl-xml/oproperty.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_object_property().count(), 1);
    }

    #[test]
    fn test_subclass() {
        let ont_s = include_str!("../../ont/owl-xml/subclass.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
    }

    #[test]
    fn annotated_subclass() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-on-subclass.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let annotated_axiom = ont
            .i()
            .annotated_axiom(AxiomKind::SubClassOf)
            .next()
            .unwrap();
        assert_eq!(annotated_axiom.ann.len(), 1);
    }

    #[test]
    fn test_some() {
        let ont_s = include_str!("../../ont/owl-xml/some.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
        assert_eq!(ont.i().declare_object_property().count(), 1);
    }

    #[test]
    fn test_some_not() {
        let ont_s = include_str!("../../ont/owl-xml/some-not.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
        let sc: &SubClassOf = ont.i().sub_class_of().next().unwrap();
        match &sc.sup {
            ClassExpression::ObjectSomeValuesFrom { ope: _, bce } => {
                matches!(**bce, ClassExpression::ObjectComplementOf(_));
            }
            _ => panic!(),
        }

        assert_eq!(ont.i().declare_object_property().count(), 1);
    }

    #[test]
    fn test_only() {
        let ont_s = include_str!("../../ont/owl-xml/only.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
        assert_eq!(ont.i().declare_class().count(), 2);
        assert_eq!(ont.i().declare_object_property().count(), 1);
    }

    #[test]
    fn test_and() {
        let ont_s = include_str!("../../ont/owl-xml/and.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);

        let sc = ont.i().sub_class_of().next().unwrap();
        assert!(matches!(&sc.sup, ClassExpression::ObjectIntersectionOf(_)));
    }

    #[test]
    fn test_or() {
        let ont_s = include_str!("../../ont/owl-xml/or.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);

        let sc = ont.i().sub_class_of().next().unwrap();
        assert!(matches!(&sc.sup, ClassExpression::ObjectUnionOf(_)));
    }

    #[test]
    fn test_not() {
        let ont_s = include_str!("../../ont/owl-xml/not.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
    }

    #[test]
    fn test_annotation_property() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-property.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        assert_eq!(ont.i().declare_annotation_property().count(), 1);
    }

    #[test]
    fn test_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        assert_eq!(ont.i().declare_annotation_property().count(), 1);
        assert_eq!(ont.i().annotation_assertion().count(), 1);
    }

    #[test]
    fn test_one_label_non_abbreviated() {
        let ont_s = include_str!("../../ont/owl-xml/manual/one-label-non-abbreviated-iri.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().annotation_assertion().count(), 1);
    }

    #[test]
    fn test_label() {
        let ont_s = include_str!("../../ont/owl-xml/label.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().annotation_assertion().count(), 1);
    }

    #[test]
    fn test_one_ontology_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/one-ontology-annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().ontology_annotation().count(), 1);
    }

    #[test]
    fn test_equivalent_class() {
        let ont_s = include_str!("../../ont/owl-xml/equivalent-class.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().equivalent_class().count(), 1);
    }

    #[test]
    fn test_disjoint_class() {
        let ont_s = include_str!("../../ont/owl-xml/disjoint-class.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().disjoint_class().count(), 1);
    }

    #[test]
    fn test_disjoint_union() {
        let ont_s = include_str!("../../ont/owl-xml/disjoint-union.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().disjoint_union().count(), 1);
    }

    #[test]
    fn test_sub_oproperty() {
        let ont_s = include_str!("../../ont/owl-xml/suboproperty.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_object_property().count(), 1);
    }

    #[test]
    fn test_sub_oproperty_inverse() {
        let ont_s = include_str!("../../ont/owl-xml/suboproperty-inverse.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_object_property().count(), 1);
    }

    #[test]
    fn test_one_inverse_property() {
        let ont_s = include_str!("../../ont/owl-xml/inverse-properties.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().inverse_object_properties().count(), 1);
    }

    #[test]
    fn test_one_transitive_property() {
        let ont_s = include_str!("../../ont/owl-xml/transitive-properties.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().transitive_object_property().count(), 1);
    }

    #[test]
    fn test_inverse_transitive() {
        let ont_s = include_str!("../../ont/owl-xml/inverse-transitive.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().transitive_object_property().count(), 1);
    }

    #[test]
    fn test_subproperty_chain() {
        let ont_s = include_str!("../../ont/owl-xml/subproperty-chain.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_object_property().count(), 1);
    }

    #[test]
    fn test_subproperty_chain_with_inverse() {
        let ont_s = include_str!("../../ont/owl-xml/subproperty-chain-with-inverse.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_object_property().count(), 1);
    }

    #[test]
    fn test_annotation_on_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-with-annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let mut ann_i = ont.i().annotated_axiom(AxiomKind::AnnotationAssertion);
        let ann: &AnnotatedAxiom = ann_i.next().unwrap();
        assert_eq!(ann.ann.len(), 1);
    }

    #[test]
    fn annotated_transitive() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-on-transitive.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let annotated_axiom = ont
            .i()
            .annotated_axiom(AxiomKind::TransitiveObjectProperty)
            .next()
            .unwrap();
        assert_eq!(annotated_axiom.ann.len(), 1);
    }

    #[test]
    fn two_annotated_transitive() {
        let ont_s = include_str!("../../ont/owl-xml/two-annotation-on-transitive.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let annotated_axiom = ont
            .i()
            .annotated_axiom(AxiomKind::TransitiveObjectProperty)
            .next()
            .unwrap();

        assert_eq!(annotated_axiom.ann.len(), 2);
    }
    #[test]
    fn test_sub_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/sub-annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_annotation_property_of().count(), 1);
    }

    #[test]
    fn test_annotation_domain() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-domain.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().annotation_property_domain().count(), 1);
    }

    #[test]
    fn test_annotation_range() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-range.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().annotation_property_range().count(), 1);
    }

    #[test]
    fn test_data_property() {
        let ont_s = include_str!("../../ont/owl-xml/data-property.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_data_property().count(), 1);
    }

    #[test]
    fn test_literal_escaped() {
        let ont_s = include_str!("../../ont/owl-xml/literal-escaped.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let aa = ont.i().annotation_assertion().next().unwrap();
        match &aa.ann.av {
            AnnotationValue::Literal(l) => assert_eq!(l.literal(), &String::from("A --> B")),
            _ => panic!("expected literal annotation value"),
        }
    }

    #[test]
    fn test_named_individual() {
        let ont_s = include_str!("../../ont/owl-xml/named-individual.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_named_individual().count(), 1);
    }

    #[test]
    fn test_import() {
        let ont_s = include_str!("../../ont/owl-xml/import.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().import().count(), 1);
    }

    #[test]
    fn test_datatype() {
        let ont_s = include_str!("../../ont/owl-xml/datatype.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_datatype().count(), 1);
    }

    #[test]
    fn test_has_value() {
        let ont_s = include_str!("../../ont/owl-xml/object-has-value.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let ss = ont.i().sub_class_of().next().unwrap();
        if let ClassExpression::ObjectHasValue { ope: _, i: _ } = ss.sup {
            return;
        }
        assert!(false);
    }

    #[test]
    fn test_one_of() {
        let ont_s = include_str!("../../ont/owl-xml/object-one-of.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let ss = ont.i().sub_class_of().next().unwrap();
        if let ClassExpression::ObjectOneOf(ref o) = ss.sub {
            assert_eq!(o.len(), 2);
        }
    }

    #[test]
    fn test_has_self() {
        let ont_s = include_str!("../../ont/owl-xml/object-has-self.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let ss = ont.i().sub_class_of().next().unwrap();
        match ss.sup {
            ClassExpression::ObjectHasSelf(ObjectPropertyExpression::ObjectProperty(ref op)) => {
                assert_eq!(String::from(op), "http://example.com/iri#o")
            }
            _ => {
                panic!("Expecting ObjectProperty");
            }
        }
    }

    #[test]
    fn test_inverse() {
        let ont_s = include_str!("../../ont/owl-xml/some-inverse.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
        assert_eq!(ont.i().declare_object_property().count(), 1);

        let sc = ont.i().sub_class_of().next().unwrap();
        let some = &sc.sup;

        assert_eq!(
            match some {
                ClassExpression::ObjectSomeValuesFrom {
                    ope: ObjectPropertyExpression::InverseObjectProperty(op),
                    bce: _,
                } => String::from(op),
                _ => "It didn't match".to_string(),
            },
            "http://www.example.com#r"
        );
    }

    #[test]
    fn test_min_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/object-min-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
        assert_eq!(ont.i().declare_object_property().count(), 1);

        let sc = ont.i().sub_class_of().next().unwrap();
        let some = &sc.sup;

        let (n, o, c) = match some {
            ClassExpression::ObjectMinCardinality {
                n,
                ope: ObjectPropertyExpression::ObjectProperty(o),
                bce,
            } => match **bce {
                ClassExpression::Class(ref c) => (n, String::from(o), String::from(c)),
                _ => {
                    panic!("1:Unexpected Class");
                }
            },
            _ => {
                panic!("2:Unexpected Class");
            }
        };

        assert!(n == &1);
        assert_eq!(o, "http://www.example.com/iri#r");
        assert_eq!(c, "http://www.example.com/iri#D")
    }

    #[test]
    fn test_unqualified_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/object-unqualified-max-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
    }

    #[test]
    fn test_max_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/object-max-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
        assert_eq!(ont.i().declare_object_property().count(), 1);

        let sc = ont.i().sub_class_of().next().unwrap();
        let some = &sc.sup;

        let (n, o, c) = match some {
            ClassExpression::ObjectMaxCardinality {
                n,
                ope: ObjectPropertyExpression::ObjectProperty(o),
                bce,
            } => match **bce {
                ClassExpression::Class(ref c) => (n, String::from(o), String::from(c)),
                _ => {
                    panic!("1:Unexpected Class");
                }
            },
            _ => {
                panic!("2:Unexpected Class");
            }
        };

        assert!(n == &1);
        assert_eq!(o, "http://www.example.com/iri#r");
        assert_eq!(c, "http://www.example.com/iri#D")
    }

    #[test]
    fn test_exact_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/object-exact-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
        assert_eq!(ont.i().declare_object_property().count(), 1);

        let sc = ont.i().sub_class_of().next().unwrap();
        let some = &sc.sup;

        let (n, o, c) = match some {
            ClassExpression::ObjectExactCardinality {
                n,
                ope: ObjectPropertyExpression::ObjectProperty(o),
                bce,
            } => match **bce {
                ClassExpression::Class(ref c) => (n, String::from(o), String::from(c)),
                _ => {
                    panic!("1:Unexpected Class");
                }
            },
            _ => {
                panic!("2:Unexpected Class");
            }
        };

        assert!(n == &1);
        assert_eq!(o, "http://www.example.com/iri#r");
        assert_eq!(c, "http://www.example.com/iri#D")
    }

    #[test]
    fn datatype_alias() {
        let ont_s = include_str!("../../ont/owl-xml/datatype-alias.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().datatype_definition().count(), 1);
        let dd = ont.i().datatype_definition().next().unwrap();

        match dd {
            DatatypeDefinition { kind, range } => {
                assert_eq!(String::from(kind), "http://www.example.com/D");

                match range {
                    DataRange::Datatype(real) => {
                        assert_eq!(String::from(real), "http://www.w3.org/2002/07/owl#real");
                    }
                    _ => {
                        panic!("Unexpected type from test");
                    }
                }
            }
        }
    }

    #[test]
    fn datatype_intersection() {
        let ont_s = include_str!("../../ont/owl-xml/datatype-intersection.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().datatype_definition().count(), 1);
    }

    #[test]
    fn datatype_union() {
        let ont_s = include_str!("../../ont/owl-xml/datatype-union.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().datatype_definition().count(), 1);
    }

    #[test]
    fn datatype_complement() {
        let ont_s = include_str!("../../ont/owl-xml/datatype-complement.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().datatype_definition().count(), 1);
    }

    #[test]
    fn datatype_oneof() {
        let ont_s = include_str!("../../ont/owl-xml/datatype-oneof.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().datatype_definition().count(), 1);
    }

    #[test]
    fn data_some() {
        let ont_s = include_str!("../../ont/owl-xml/data-some.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
    }

    #[test]
    fn facet_restriction() {
        let ont_s = include_str!("../../ont/owl-xml/facet-restriction.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
    }

    #[test]
    fn data_only() {
        let ont_s = include_str!("../../ont/owl-xml/data-only.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let cl = &ont.i().sub_class_of().next().unwrap().sup;
        assert_eq!(ont.i().sub_class_of().count(), 1);
        if let ClassExpression::DataAllValuesFrom {
            dp: ref _dp,
            dr: ref _dr,
        } = cl
        {
            assert!(true);
        } else {
            panic!("Expecting DataAllValuesFrom");
        }
    }

    #[test]
    fn data_exact_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/data-exact-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let cl = &ont.i().sub_class_of().next().unwrap().sup;
        assert_eq!(ont.i().sub_class_of().count(), 1);
        if let ClassExpression::DataExactCardinality {
            n: ref _n,
            dp: ref _dp,
            dr: ref _dr,
        } = cl
        {
            assert!(true);
        } else {
            panic!("Expecting DataExactCardinality");
        }
    }

    #[test]
    fn data_unqualified_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/data-unqualified-exact.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        let cl = &ont.i().sub_class_of().next().unwrap().sup;
        assert_eq!(ont.i().sub_class_of().count(), 1);
        if let ClassExpression::DataExactCardinality {
            n: ref _n,
            dp: ref _dp,
            ref dr,
        } = cl
        {
            assert!(match dr {
                DataRange::Datatype(dt) => dt.is_s(&OWL2Datatype::RDFSLiteral.iri_s()[..]),
                _ => false,
            });
        } else {
            panic!("Expecting DataExactCardinality");
        }
    }

    #[test]
    fn data_min_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/data-min-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let cl = &ont.i().sub_class_of().next().unwrap().sup;
        assert_eq!(ont.i().sub_class_of().count(), 1);
        if let ClassExpression::DataMinCardinality {
            n: ref _n,
            dp: ref _dp,
            dr: ref _dr,
        } = cl
        {
            assert!(true);
        } else {
            panic!("Expecting DataMinCardinality");
        }
    }

    #[test]
    fn data_max_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/data-max-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let cl = &ont.i().sub_class_of().next().unwrap().sup;
        assert_eq!(ont.i().sub_class_of().count(), 1);
        if let ClassExpression::DataMaxCardinality {
            n: ref _n,
            dp: ref _dp,
            dr: ref _dr,
        } = cl
        {
            assert!(true);
        } else {
            panic!("Expecting DataMaxCardinality");
        }
    }

    #[test]
    fn data_has_value() {
        let ont_s = include_str!("../../ont/owl-xml/data-has-value.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(1, ont.i().sub_class_of().count());
    }

    #[test]
    fn class_assertion() {
        let ont_s = include_str!("../../ont/owl-xml/class-assertion.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(1, ont.i().class_assertion().count());
    }

    #[test]
    fn different_individuals() {
        let ont_s = include_str!("../../ont/owl-xml/different-individual.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(1, ont.i().different_individuals().count());

        let di = ont.i().different_individuals().next().unwrap();
        assert_eq!(2, di.0.len());
    }

    #[test]
    fn annotation_with_anonymous() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-with-anonymous.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().annotation_assertion().count(), 1);

        let _aa = ont.i().annotation_assertion().next();
    }

    #[test]
    fn type_complex() {
        let ont_s = include_str!("../../ont/owl-xml/type-complex.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(1, ont.i().class_assertion().count());
        let ca = ont.i().class_assertion().next().unwrap();
        assert!{
            matches!{
                &ca.ce, ClassExpression::ObjectComplementOf(_c)
            }
        }
    }

    #[test]
    fn type_individual_datatype() {
        let ont_s = include_str!("../../ont/owl-xml/type-individual-datatype.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(1, ont.i().class_assertion().count());
        let ca = ont.i().class_assertion().next().unwrap();

        assert!{
            matches!{
                &ca.ce, ClassExpression::ObjectMinCardinality{n:_, ope:_, bce:_}
            }
        };
    }

    #[test]
    fn family() {
        let ont_s = include_str!("../../ont/owl-xml/family.owx");
        let (_, _) = read_ok(&mut ont_s.as_bytes());
    }
}
