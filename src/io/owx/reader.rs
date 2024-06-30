use curie::PrefixMapping;
use quick_xml::escape::unescape;
use quick_xml::name::ResolveResult;
use quick_xml::name::ResolveResult::Bound;

use crate::error::*;
use crate::io::ParserConfiguration;
use crate::model::*;
use crate::vocab::Facet;
use crate::vocab::Namespace::*;
use crate::vocab::OWL2Datatype;
use crate::vocab::OWL;

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::convert::TryFrom;
use std::io::BufRead;

use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::Event;
use quick_xml::NsReader;

struct Read<'a, A: ForIRI, R>
where
    R: BufRead,
{
    build: &'a Build<A>,
    mapping: PrefixMapping,
    reader: NsReader<R>,
}

pub fn read<A: ForIRI, O: MutableOntology<A> + Default, R: BufRead>(
    bufread: &mut R,
    _config: ParserConfiguration,
) -> Result<(O, PrefixMapping), HornedError> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

pub fn read_with_build<A: ForIRI, O: MutableOntology<A> + Default, R: BufRead>(
    bufread: R,
    build: &Build<A>,
) -> Result<(O, PrefixMapping), HornedError> {
    let reader: NsReader<R> = NsReader::from_reader(bufread);
    let mut ont: O = Default::default();
    let mapping = PrefixMapping::default();
    let mut buf = Vec::new();

    let mut r = Read {
        reader,
        build,
        mapping,
    };

    loop {
        match r.reader.read_resolved_event_into(&mut buf)? {
            (ref ns, Event::Start(ref e)) | (ref ns, Event::Empty(ref e)) if is_owl(ns) => {
                match e.local_name().as_ref() {
                    b"Ontology" => {
                        let s = get_attr_value_str(&mut r.reader, e, b"ontologyIRI")?;
                        if let Some(s) = s {
                            r.mapping.set_default(&s);
                        }

                        ont.insert(OntologyID {
                            iri: get_iri_value_for(&mut r, e, b"ontologyIRI")?,
                            viri: get_iri_value_for(&mut r, e, b"versionIRI")?,
                        });
                    }
                    b"Prefix" => {
                        let iri = get_attr_value_str(&mut r.reader, e, b"IRI")?;
                        let prefix = get_attr_value_str(&mut r.reader, e, b"name")?;
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
                        let aa = AnnotatedComponent::from_start(&mut r, e)?;
                        ont.insert(aa);
                    }
                }
            }
            (ref ns, Event::End(ref e)) if is_owl_name(ns, e, b"Ontology") => {
                break;
            }
            // this initially was in `read_event`.
            (_, Event::Eof) => {
                let pos = &r.reader.buffer_position();
                return Err(invalid! {
                    "Unexpected EoF at {}", pos
                });
            }
            _ => {}
        }
    }
    Ok((ont, r.mapping))
}

fn decode_expand_curie_maybe<'a, A: ForIRI, R: BufRead>(
    r: &mut Read<A, R>,
    val: &'a [u8],
) -> Result<Cow<'a, str>, HornedError> {
    // Okay, so a lot of matching, but without this the borrow checker
    // is gonna complain. This let's us do the following:
    // - if the CURIE can be decoded without replacement, and if it is
    //   already a complete IRI, we don't have to copy it, and we can
    //   return it as as borrowed string.
    // - in any other case, we need to perform a copy, otherwise the decoded
    //   string / unabbreviated IRI is not going to live long enough.
    #[cfg(feature = "quick-xml/encoding")]
    match r.reader.decode(val) {
        Cow::Borrowed(b) => expand_curie_maybe(r, b),
        Cow::Owned(o) => match expand_curie_maybe(r, &o) {
            Cow::Borrowed(b) => Cow::Owned(b.to_string()),
            Cow::Owned(o) => Cow::Owned(o),
        },
    }

    #[cfg(not(feature = "quick-xml/encoding"))]
    match r.reader.decoder().decode(val) {
        Ok(curie) => {
            let cur = expand_curie_maybe(r, curie);
            Ok(cur)
        }
        Err(e) => Err(HornedError::from(e)),
    }
}

/// Expand a curie if there is an appropriate prefix
fn expand_curie_maybe<'a, A: ForIRI, R: BufRead>(
    r: &mut Read<A, R>,
    val: Cow<'a, str>,
) -> Cow<'a, str> {
    match r.mapping.expand_curie_string(&val) {
        // If we expand use this
        Ok(n) => Cow::Owned(n),
        // Else assume it's a complete URI
        Err(_e) => val,
    }
}

/// Returns, if present, the byte slice corresponding to the value of the given attribute within opening tag.
///
/// ## Errors
///
/// Errors are generated by `quick-xml` and as such they are converted into a `ParserError`.
fn get_attr_value_bytes<'a>(
    event: &'a BytesStart,
    attr_key: &[u8],
) -> Result<Option<Cow<'a, [u8]>>, HornedError> {
    event
        .try_get_attribute(attr_key)
        .map_err(|err| HornedError::ParserError(Box::new(err), Location::Unknown))
        .map(|opt_attr| opt_attr.map(|attr| attr.value))
}

/// Returns, if present, the string corresponding to the value of the given attribute within opening tag.
///
/// ## Errors
///
/// Errors are generated by `quick-xml` and as such they are converted into a `ParserError`.
fn get_attr_value_str<R: BufRead>(
    reader: &mut NsReader<R>,
    event: &BytesStart,
    attr_key: &[u8],
) -> Result<Option<String>, HornedError> {
    // First, get the byte slice containing the attribute value
    get_attr_value_bytes(event, attr_key)?
        .as_ref()
        .map(|val|
        // Next, decode it to obtain a `str`.
        reader.decoder().decode(val)
        .map_err(|err| HornedError::ParserError(Box::new(err), Location::Unknown)))
        .transpose()
        .map(|opt| opt.map(|s| s.to_string()))
}

/// Returns, if present, the IRI for the given opening tag.
fn get_iri_value<A: ForIRI, R: BufRead>(
    r: &mut Read<A, R>,
    event: &BytesStart,
) -> Result<Option<IRI<A>>, HornedError> {
    let iri = get_iri_value_for(r, event, b"IRI")?;
    if iri.is_none() {
        get_iri_value_for(r, event, b"abbreviatedIRI")
    } else {
        Ok(iri)
    }
}

/// Returns, if present, the IRI included in the given attribute for the given opening tag.
fn get_iri_value_for<A: ForIRI, R: BufRead>(
    r: &mut Read<A, R>,
    event: &BytesStart,
    iri_attr: &[u8],
) -> Result<Option<IRI<A>>, HornedError> {
    Ok(
        // check for the attrib, if malformed return
        get_attr_value_str(&mut r.reader, event, iri_attr)?
            // or transform the some String
            .map(|st| {
                let cow = Cow::Owned(st);
                let x = expand_curie_maybe(r, cow);
                // Into an iri
                r.build.iri(
                    // or a curie
                    x,
                )
            }),
    )
}

fn decode_tag<A: ForIRI, R: BufRead>(
    tag: &[u8],
    r: &mut Read<A, R>,
) -> Result<String, HornedError> {
    Ok(r.reader.decoder().decode(tag)?.to_string())
}

fn error_missing_end_tag<A: ForIRI, R: BufRead>(
    tag: &[u8],
    r: &mut Read<A, R>,
    pos: usize,
) -> HornedError {
    match decode_tag(tag, r) {
        Ok(tag) => invalid! {"Missing End Tag: expected {tag} after {pos}"},
        Err(e) => e,
    }
}

fn error_missing_attribute<A: ForIRI, AT: Into<String>, R: BufRead>(
    attribute: AT,
    r: &mut Read<A, R>,
) -> HornedError {
    let attribute = attribute.into();
    let pos = r.reader.buffer_position();
    invalid! {
        "Missing Attribute: expected {attribute} at {pos}"
    }
}

fn error_unexpected_tag<A: ForIRI, R: BufRead>(tag: &[u8], r: &mut Read<A, R>) -> HornedError {
    match decode_tag(tag, r) {
        Ok(tag) => invalid! {
            "Unexpected tag: found {tag} at {}", r.reader.buffer_position()
        },
        Err(e) => e,
    }
}

fn error_unexpected_end_tag<A: ForIRI, R: BufRead>(tag: &[u8], r: &mut Read<A, R>) -> HornedError {
    match decode_tag(tag, r) {
        Ok(tag) => invalid! {
            "Unexpected end tag: expected {tag} at {}", r.reader.buffer_position()
        },
        Err(e) => e,
    }
}

fn error_unknown_entity<A: ForIRI, AA: Into<String>, R: BufRead>(
    kind: AA,
    found: &[u8],
    r: &mut Read<A, R>,
) -> HornedError {
    match decode_tag(found, r) {
        Ok(found) => invalid! {
            "Unknown Entity: expected kind of {}, found {found} at {}",
            kind.into(),
            r.reader.buffer_position()
        },
        Err(e) => e,
    }
}

fn error_missing_element<A: ForIRI, R: BufRead>(tag: &[u8], r: &mut Read<A, R>) -> HornedError {
    match decode_tag(tag, r) {
        Ok(tag) => invalid! {
            "Missing Element: expected {tag} at {}",
                r.reader.buffer_position()
        },
        Err(e) => e,
    }
}

fn is_owl(res: &ResolveResult) -> bool {
    if let Bound(ns) = res {
        ns.as_ref() == OWL.as_bytes()
    } else {
        false
    }
}

fn is_owl_name(res: &ResolveResult, e: &BytesEnd, tag: &[u8]) -> bool {
    is_owl(res) && e.local_name().as_ref() == tag
}

trait FromStart<A: ForIRI>: Sized {
    fn from_start<R: BufRead>(r: &mut Read<A, R>, e: &BytesStart) -> Result<Self, HornedError>;
}

macro_rules! from_start {
    ($type:ident, $r:ident, $e:ident, $body:tt) => {
        impl<A: ForIRI> FromStart<A> for $type<A> {
            fn from_start<R: BufRead>(
                $r: &mut Read<A, R>,
                $e: &BytesStart,
            ) -> Result<$type<A>, HornedError>
                $body
        }
    };
}

/// Potentially unbalanced
fn named_entity_from_start<A, R, T>(
    r: &mut Read<A, R>,
    e: &BytesStart,
    tag: &[u8],
) -> Result<T, HornedError>
where
    A: ForIRI,
    R: BufRead,
    T: From<IRI<A>>,
{
    if let Some(iri) = get_iri_value(r, e)? {
        if e.local_name().as_ref() == tag {
            return Ok(T::from(iri));
        } else {
            return Err(error_unknown_entity(
                ::std::str::from_utf8(tag).unwrap(),
                e.local_name().as_ref(),
                r,
            ));
        }
    }

    Err(error_missing_element(b"IRI", r))
}

fn from_start<A: ForIRI, R: BufRead, T: FromStart<A>>(
    r: &mut Read<A, R>,
    e: &BytesStart,
) -> Result<T, HornedError> {
    T::from_start(r, e)
}

from_start! {
    Literal, r, e,
    {
        let datatype_iri = get_iri_value_for(r, e, b"datatypeIRI")?;
        let lang = get_attr_value_str(&mut r.reader, e, b"xml:lang")?;

        // quick-xml only offers `r.reader.read_text()` for NsReader<'i &u8> as
        // of version 0.26.0.
        // So, we need to work around it.
        //
        // # Assumptions
        // The first closing `Literal` tag that is encountered is the one that
        // matches the opening tag we are considering.
        let mut literal = String::new();
        let mut buf = Vec::new();
        loop {
            if let Event::End(event) = r.reader.read_event_into(&mut buf)? {
                if let b"Literal" = event.local_name().as_ref() { break; }
            }

            // This decoding step is not sufficient on its own.
            // For instance, "A --> B" would yield "A --&gt; B".
            let escaped_str = r.reader.decoder().decode(&buf)?;
            // Hence this next step.
            let unescaped_str = unescape(&escaped_str)
                .map_err(|e| HornedError::ParserError(Box::new(e), Location::BytePosition(r.reader.buffer_position())))?;
            // Finally, we add the unescaped string to the literal we are building.
            literal.push_str(&unescaped_str);
        }
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
                    => return Err(invalid!("Broken literal at {}", r.reader.buffer_position())),
                (Some(datatype_iri), None, literal)
                    => Literal::Datatype{literal, datatype_iri},
            })
    }
}

from_start! {
    AnnotationValue, r, e, {
        Ok(
            match e.local_name().as_ref() {
                b"Literal" => {
                    Literal::from_start(r, e)?
                    .into()
                }
                b"AbbreviatedIRI"|b"IRI" => {
                    IRI::from_xml(r, e.local_name().as_ref())?
                    .into()
                }
                b"AnonymousIndividual" => {
                    AnonymousIndividual::from_start(r, e)?
                    .into()
                }
                _ => {
                    return Err
                        (error_unexpected_tag(e.local_name().as_ref(), r));
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

fn axiom_from_start<A: ForIRI, R: BufRead>(
    r: &mut Read<A, R>,
    e: &BytesStart,
    axiom_kind: &[u8],
) -> Result<Component<A>, HornedError> {
    Ok(match axiom_kind {
        b"Annotation" => OntologyAnnotation(Annotation {
            ap: from_start(r, e)?,
            av: from_next(r)?,
        })
        .into(),
        b"Declaration" => {
            let ne: NamedOWLEntity<_> = from_start(r, e)?;
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
        b"DLSafeRule" => {
            let body = till_end(r, b"Body")?;
            discard_till_start(r, b"Head")?;
            let head = till_end(r, b"Head")?;
            discard_till(r, b"DLSafeRule")?;
            Rule { body, head }
        }
        .into(),
        _ => {
            return Err(error_unexpected_tag(axiom_kind, r));
        }
    })
}

fn from_start_to_end<A: ForIRI, R: BufRead, T: FromStart<A> + std::fmt::Debug>(
    r: &mut Read<A, R>,
    e: &BytesStart,
    end_tag: &[u8],
) -> Result<Vec<T>, HornedError> {
    let v = vec![from_start(r, e)?];
    till_end_with(r, end_tag, v)
}

// Keep reading entities, till end_tag is reached
fn till_end<A: ForIRI, R: BufRead, T: FromStart<A> + std::fmt::Debug>(
    r: &mut Read<A, R>,
    end_tag: &[u8],
) -> Result<Vec<T>, HornedError> {
    let operands: Vec<T> = Vec::new();
    till_end_with(r, end_tag, operands)
}

// Keep reading entities, till end_tag is reached
fn till_end_with<A: ForIRI, R: BufRead, T: FromStart<A> + std::fmt::Debug>(
    r: &mut Read<A, R>,
    end_tag: &[u8],
    mut operands: Vec<T>,
) -> Result<Vec<T>, HornedError> {
    let mut buf = Vec::new();
    loop {
        let e = r.reader.read_resolved_event_into(&mut buf)?;
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

#[allow(clippy::type_complexity)]
fn object_cardinality_restriction<A: ForIRI, R: BufRead>(
    r: &mut Read<A, R>,
    e: &BytesStart,
    end_tag: &[u8],
) -> Result<(u32, ObjectPropertyExpression<A>, Box<ClassExpression<A>>), HornedError> {
    let n = get_attr_value_str(&mut r.reader, e, b"cardinality")?;
    let n = n.ok_or_else(|| error_missing_attribute("cardinality", r))?;

    let ope = from_next(r)?;
    let mut vce: Vec<ClassExpression<_>> = till_end(r, end_tag)?;

    Ok((
        n.parse::<u32>()
            .map_err(|_s| HornedError::invalid("Failed to parse int"))?,
        ope,
        Box::new(match vce.len() {
            0 => r.build.class(OWL::Thing.as_ref()).into(),
            1 => vce.remove(0),
            _ => return Err(error_unexpected_tag(end_tag, r)),
        }),
    ))
}

fn data_cardinality_restriction<A: ForIRI, R: BufRead>(
    r: &mut Read<A, R>,
    e: &BytesStart,
    end_tag: &[u8],
) -> Result<(u32, DataProperty<A>, DataRange<A>), HornedError> {
    let n = get_attr_value_str(&mut r.reader, e, b"cardinality")?;
    let n = n.ok_or_else(|| (error_missing_attribute("cardinality", r)))?;

    let dp = from_next(r)?;
    let mut vdr: Vec<DataRange<_>> = till_end(r, end_tag)?;

    Ok((
        n.parse::<u32>()
            .map_err(|_s| HornedError::invalid("Failed to parse int"))?,
        dp,
        match vdr.len() {
            0 => r.build.datatype(OWL2Datatype::Literal.as_ref()).into(),
            1 => vdr.remove(0),
            _ => return Err(error_unexpected_tag(end_tag, r)),
        },
    ))
}

from_start! {
    PropertyExpression, r, e,
    {
        Ok(
            match e.local_name().as_ref() {
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
                                                    e.local_name().as_ref(), r))
                }
            })
    }
}

from_start! {
    ClassExpression, r, e, {
        Ok(
            match e.local_name().as_ref() {
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
                    return Err(error_unexpected_tag(e.local_name().as_ref(), r));
                }
            }
        )
    }
}

from_start! {
    AnnotatedComponent, r, e,
    {
        let mut annotation: BTreeSet<Annotation<_>> = BTreeSet::new();
        let axiom_kind = e.local_name();
        let mut buf = Vec::new();

        loop {
            let e = r.reader.read_resolved_event_into(&mut buf)?;
            match e {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if is_owl(ns) =>
                {
                    match e.local_name().as_ref() {
                        b"Annotation" => {
                            annotation.insert
                                (Annotation::from_xml(r, b"Annotation")?);
                        }
                        _ => {
                            return Ok(AnnotatedComponent{
                                ann:annotation,
                                component:axiom_from_start(r,e,axiom_kind.as_ref())?
                            });
                        }
                    }
                }
                (ref ns, Event::End(ref e))
                    if is_owl_name(ns, e, axiom_kind.as_ref()) =>
                {
                    return Err(error_unexpected_end_tag(axiom_kind.as_ref(), r));
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
        match e.local_name().as_ref() {
            b"AnonymousIndividual" =>{
                let ai:AnonymousIndividual<_> = from_start(r, e)?;
                Ok(ai.into())
            }
            b"NamedIndividual" =>{
                let ni:NamedIndividual<_> = from_start(r, e)?;
                Ok(ni.into())
            }
            b"IRI" | b"AbbreviatedIRI" => {
                let iri:IRI<_> = from_start(r, e)?;
                let ni:NamedIndividual<_> = iri.into();
                Ok(ni.into())
            }
            l => {
                todo!("{:?}",std::str::from_utf8(l))
            }
        }
    }
}

from_start! {
    AnnotationSubject, r, e,
    {
        match e.local_name().as_ref() {
            b"AnonymousIndividual" =>{
                let ai:AnonymousIndividual<_> = from_start(r, e)?;
                Ok(ai.into())
            }
            b"IRI" | b"AbbreviatedIRI" => {
                let iri:IRI<_> = from_start(r, e)?;
                Ok(iri.into())
            }
            l => {
                todo!("{:?}",std::str::from_utf8(l))
            }
        }

    }
}

impl<A: ForIRI> FromStart<A> for AnonymousIndividual<A> {
    fn from_start<R: BufRead>(
        r: &mut Read<A, R>,
        e: &BytesStart,
    ) -> Result<AnonymousIndividual<A>, HornedError> {
        let ai: AnonymousIndividual<_> = r.build.anon(
            get_attr_value_str(&mut r.reader, e, b"nodeID")?
                .ok_or_else(|| error_missing_attribute("nodeID Expected", r))?,
        );
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
            match e.local_name().as_ref() {
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
                                 e.local_name().as_ref(), r));
                }
            }
        )
    }
}

from_start! {
    SubObjectPropertyExpression, r, e,
    {
        Ok(
            match e.local_name().as_ref() {
                b"ObjectPropertyChain" => {
                    let o = till_end(r, b"ObjectPropertyChain")?;
                    SubObjectPropertyExpression::ObjectPropertyChain(o)

                }
                b"ObjectProperty" | b"ObjectInverseOf" => {
                    SubObjectPropertyExpression::
                    ObjectPropertyExpression(from_start(r, e)?)
                }
                _ => {
                    return Err(error_unknown_entity("Sub Object Property",
                                                    e.local_name().as_ref(),
                                                    r));
                }
            }
        )
    }
}

from_start! {
    FacetRestriction, r, e,
    {
        let f = get_attr_value_bytes(e, b"facet")?
            .ok_or_else(|| error_missing_attribute("facet", r))?;

        Ok(
            FacetRestriction {
                f: Facet::try_from(f.as_ref())
                    .map_err(|_| error_unknown_entity("facet", &f, r))?,
                    // .ok_or_else(
                    //     || error_unknown_entity("facet", &f, r))?,
                l: from_next(r)?
            }
        )
    }
}

from_start! {
    DataRange, r, e,
    {
        Ok(
            match e.local_name().as_ref() {
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
                b"DataUnionOf" => {
                    DataRange::DataUnionOf(
                        till_end(r, b"DataUnionOf")?
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
                                                    e.local_name().as_ref(),r ));
                }
            }
        )
    }
}

from_start! {
    NamedOWLEntity, r, e,
    {
        Ok(
            match e.local_name().as_ref() {
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
                    return Err(error_unknown_entity("NamedOWLEntity",
                                                    e.local_name().as_ref(),r ));
                }
            }
        )
    }
}

trait FromXML<A: ForIRI>: Sized {
    fn from_xml<R: BufRead>(newread: &mut Read<A, R>, end_tag: &[u8]) -> Result<Self, HornedError> {
        Self::from_xml_nc(newread, end_tag)
    }

    fn from_xml_nc<R: BufRead>(
        newread: &mut Read<A, R>,
        end_tag: &[u8],
    ) -> Result<Self, HornedError>;
}

macro_rules! from_xml {
    ($type:ident, $r:ident, $end:ident, $body:tt) => {
        impl<A: ForIRI> FromXML<A> for $type<A> {
            fn from_xml_nc<R: BufRead>(
                $r: &mut Read<A, R>,
                $end: &[u8],
            ) -> Result<$type<A>, HornedError> {
                $body
            }
        }
    };
}

from_xml! {
    Annotation, r, end,
    {

        let mut ap:Option<AnnotationProperty<_>> = None;
        let mut av:Option<AnnotationValue<_>> = None;
        let mut buf = Vec::new();

        loop {
            let e = r.reader.read_resolved_event_into(&mut buf)?;
            match e {
                (ref ns, Event::Start(ref e))
                |
                (ref ns, Event::Empty(ref e))
                    if is_owl(ns) =>
                {
                    match e.local_name().as_ref() {
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

fn from_next<A: ForIRI, R: BufRead, T: FromStart<A>>(r: &mut Read<A, R>) -> Result<T, HornedError> {
    let mut buf = Vec::new();
    loop {
        let e = r.reader.read_resolved_event_into(&mut buf)?;
        match e {
            (ref ns, Event::Empty(ref e)) | (ref ns, Event::Start(ref e)) if is_owl(ns) => {
                return from_start(r, e);
            }
            _ => {}
        }
    }
}

fn discard_till_start<A: ForIRI, R: BufRead>(
    r: &mut Read<A, R>,
    start: &[u8],
) -> Result<(), HornedError> {
    let pos = r.reader.buffer_position();
    let mut buf = Vec::new();
    loop {
        let e = r.reader.read_resolved_event_into(&mut buf)?;

        match e {
            (ref ns, Event::Start(ref e)) if is_owl(ns) && e.local_name().as_ref() == start => {
                return Ok(());
            }
            (_, Event::Eof) => {
                return Err(error_missing_end_tag(start, r, pos));
            }
            _ => {}
        }
    }
}

fn discard_till<A: ForIRI, R: BufRead>(r: &mut Read<A, R>, end: &[u8]) -> Result<(), HornedError> {
    let pos = r.reader.buffer_position();
    let mut buf = Vec::new();
    loop {
        let e = r.reader.read_resolved_event_into(&mut buf)?;
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
    NamedOWLEntity,r, end,
    {
        let ne = from_next(r);
        discard_till(r, end)?;
        ne
    }
}

from_start! {
    Atom, r, e,
    {
        Ok(
            match e.local_name().as_ref() {
                b"BuiltInAtom" => {
                    Atom::BuiltInAtom{
                        pred:named_entity_from_start(r, e, b"BuiltInAtom")?,
                        args:till_end(r, b"BuiltInAtom")?
                    }
                }
                b"ClassAtom" => {
                    let pred = from_next(r)?;
                    let arg = from_next(r)?;
                    Atom::ClassAtom {
                        pred,
                        arg
                    }
                },
                b"DataPropertyAtom" => {
                    let pred = from_next(r)?;
                    let args = (
                        from_next(r)?, from_next(r)?
                    );
                    Atom::DataPropertyAtom {
                        pred, args
                    }
                }
                b"DataRangeAtom" => {
                    Atom::DataRangeAtom{
                        pred: from_next(r)?,
                        arg: from_next(r)?
                    }
                }
                b"DifferentIndividualsAtom" => {
                    Atom::DifferentIndividualsAtom(from_next(r)?, from_next(r)?)
                }
                b"ObjectPropertyAtom" => {
                    let pred = from_next(r)?;
                    let args = (
                        from_next(r)?, from_next(r)?
                    );
                    Atom::ObjectPropertyAtom {
                        pred, args
                    }
                }
                b"SameIndividualAtom" => {
                    Atom::SameIndividualAtom(from_next(r)?, from_next(r)?)
                }
                _=> {
                    return Err(error_unknown_entity("Atom",
                                                    e.local_name().as_ref(),r ));
                }
            }
          )
    }
}

from_start! {
    Variable, r, e,
    {
        named_entity_from_start(r, e, b"Variable")
    }
}

from_start! {
    DArgument, r, e,
    {
        Ok(
            match e.local_name().as_ref() {
                b"Variable" => {
                    DArgument::Variable(Variable::from_start(r, e)?)
                }
                b"Literal" => {
                    DArgument::Literal(Literal::from_start(r, e)?)
                }
                _ => {
                    todo!()
                }
            }
        )
    }
}

from_start! {
    IArgument, r, e,
    {
        Ok(
            match e.local_name().as_ref() {
                b"Variable" => {
                    IArgument::Variable(Variable::from_start(r, e)?)
                }
                b"NamedIndividual" => {
                    IArgument::Individual(NamedIndividual::from_start(r, e)?.into())
                }
                b"AnonymousIndividual" => {
                    IArgument::Individual(AnonymousIndividual::from_start(r, e)?.into())
                }
                a => {
                    eprintln!("{:?}", std::str::from_utf8(a));
                    todo!();
                }
            }
        )
    }
}

from_start! {
    IRI, r, e,
    {
        Self::from_xml(r, e.local_name().as_ref())
    }
}

from_xml! {IRI, r, end,
        {
            let mut iri: Option<IRI<_>> = None;
            let mut buf = Vec::new();
            loop {
                let e = r.reader.read_resolved_event_into(&mut buf)?;
                match e {
                    (ref _ns,Event::Text(ref e)) => {
                        iri = Some(r.build.iri
                                   (decode_expand_curie_maybe(r, e)?));
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
    use crate::ontology::component_mapped::ComponentMappedOntology;
    use std::collections::HashMap;

    pub fn read_ok<R: BufRead>(
        bufread: &mut R,
    ) -> (
        ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
        PrefixMapping,
    ) {
        let b = Build::new();
        let r = read_with_build(bufread, &b);
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        let (o, m) = r.ok().unwrap();
        (o, m)
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
            ont.i()
                .the_ontology_id_or_default()
                .iri
                .as_ref()
                .unwrap()
                .as_ref(),
            "http://www.example.com/iri"
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
            .component_for_kind(ComponentKind::DeclareClass)
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
        let ont_s = include_str!("../../ont/owl-xml/manual/one-class-fully-qualified.owx");
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

        let annotated_component = ont
            .i()
            .component_for_kind(ComponentKind::SubClassOf)
            .next()
            .unwrap();
        assert_eq!(annotated_component.ann.len(), 1);
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
        let sc: &SubClassOf<_> = ont.i().sub_class_of().next().unwrap();
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
        let ont_s = include_str!("../../ont/owl-xml/ontology-annotation.owx");
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

        assert_eq!(ont.i().sub_object_property_of().count(), 1);
    }

    #[test]
    fn test_sub_oproperty_inverse() {
        let ont_s = include_str!("../../ont/owl-xml/suboproperty-inverse.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_object_property_of().count(), 1);
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

        assert_eq!(ont.i().sub_object_property_of().count(), 1);
    }

    #[test]
    fn test_subproperty_chain_with_inverse() {
        let ont_s = include_str!("../../ont/owl-xml/subproperty-chain-with-inverse.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_object_property_of().count(), 1);
    }

    #[test]
    fn test_annotation_on_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-with-annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let mut ann_i = ont
            .i()
            .component_for_kind(ComponentKind::AnnotationAssertion);
        let ann: &AnnotatedComponent<_> = ann_i.next().unwrap();
        assert_eq!(ann.ann.len(), 1);
    }

    #[test]
    fn annotated_transitive() {
        let ont_s = include_str!("../../ont/owl-xml/annotation-on-transitive.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let annotated_component = ont
            .i()
            .component_for_kind(ComponentKind::TransitiveObjectProperty)
            .next()
            .unwrap();
        assert_eq!(annotated_component.ann.len(), 1);
    }

    #[test]
    fn two_annotated_transitive() {
        let ont_s = include_str!("../../ont/owl-xml/two-annotation-on-transitive.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let annotated_component = ont
            .i()
            .component_for_kind(ComponentKind::TransitiveObjectProperty)
            .next()
            .unwrap();

        assert_eq!(annotated_component.ann.len(), 2);
    }

    #[test]
    fn test_sub_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/sub-annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_annotation_property_of().count(), 1);
    }

    #[test]
    fn test_anon_subobjectproperty() {
        let ont_s = include_str!("../../ont/owl-xml/anon-subobjectproperty.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_object_property_of().count(), 1);
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
                assert_eq!(String::from(op), "http://www.example.com/iri#op")
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
            "http://www.example.com/iri#r"
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
                assert_eq!(String::from(kind), "http://www.example.com/iri#D");

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
                DataRange::Datatype(dt) => {
                    dt.is(&OWL2Datatype::Literal)
                }
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
        let ont_s = include_str!("../../ont/owl-xml/ambiguous/annotation-with-anonymous.owx");
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
        assert! {
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

        assert! {
            matches!{
                &ca.ce, ClassExpression::ObjectMinCardinality{n:_, ope:_, bce:_}
            }
        };
    }

    #[test]
    fn gci_and_other_class_relations() {
        let ont_s = include_str!("../../ont/owl-xml/gci_and_other_class_relations.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().sub_class_of().count(), 1);
        let cl = ont.i().sub_class_of().next().unwrap();
        assert! {
            matches! {
                &cl.sup, ClassExpression::ObjectSomeValuesFrom{ope:_, bce:_}
            }
        }

        assert! {
            matches! {
                &cl.sub, ClassExpression::ObjectSomeValuesFrom{ope:_, bce:_}
            }
        }
    }

    #[test]
    fn swrl_basic() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_basic.owx");

        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        dbg!(&ont);
        assert_eq!(ont.i().rule().count(), 1);

        let rule = ont.i().rule().next().unwrap();
        let b = Build::new_rc();
        assert_eq! {
            rule,
            &Rule{
                head:vec![Atom::ClassAtom {
                    pred: ClassExpression::Class(b.class("http://www.example.com/iri#B")),
                    arg: IArgument::Variable(b.iri("http://www.example.com/iri#x").into())
                }],
                body:vec![
                    Atom::ClassAtom {
                        pred: ClassExpression::Class(b.class("http://www.example.com/iri#A")),
                        arg: IArgument::Variable(b.iri("http://www.example.com/iri#x").into())
                    }]
            }
        }
    }

    #[test]
    fn swrl_two_variables() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_two_variables.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let rule = ont.i().rule().next().unwrap();
        assert_eq!(2, rule.head.len());
    }

    #[test]
    fn swrl_class_expression() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_class_expression.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let rule = ont.i().rule().next().unwrap();
        assert! {
            matches!{
                rule.head[0],
                Atom::ClassAtom{pred:ClassExpression::ObjectIntersectionOf(_), arg:_}
            }
        };
    }

    #[test]
    fn swrl_object_property() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_object_property_atom.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let rule = ont.i().rule().next().unwrap();
        assert! {
            matches!{
                rule.head[0],
                Atom::ObjectPropertyAtom{..}
            }
        };
    }

    #[test]
    fn swrl_literal() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_literal.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let rule = ont.i().rule().next().unwrap();
        if let Atom::DataPropertyAtom {
            args: (_, ref darg),
            ..
        } = rule.head[0]
        {
            assert! {
                matches!{
                    darg,
                    DArgument::Literal(Literal::Simple{literal:s}) if s == "Literal String"

                }
            }
        } else {
            assert!(false);
        }
    }

    #[test]
    fn swrl_individual() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_individual.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let rule = ont.i().rule().next().unwrap();
        dbg!(rule);
        if let Atom::ClassAtom { ref arg, .. } = rule.head[0] {
            assert! {
                matches!{
                    arg,
                    IArgument::Individual(Individual::Named(_))

                }
            }
        } else {
            assert!(false);
        }

        if let Atom::ClassAtom { ref arg, .. } = rule.body[0] {
            assert! {
                matches!{
                    arg,
                    IArgument::Individual(Individual::Anonymous(_))

                }
            }
        } else {
            assert!(false);
        }
    }

    #[test]
    fn swrl_different_individual() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_different_individuals.owx");

        let (_ont, _) = read_ok(&mut ont_s.as_bytes());

        assert!(true);
    }

    #[test]
    fn swrl_same_individual() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_same_individual.owx");

        let (_ont, _) = read_ok(&mut ont_s.as_bytes());

        assert!(true);
    }

    #[test]
    fn swrl_built_in() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_built_in.owx");

        let (_ont, _) = read_ok(&mut ont_s.as_bytes());

        assert!(true);
    }

    #[test]
    fn swrl_data_range() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_data_range.owx");

        let (_ont, _) = read_ok(&mut ont_s.as_bytes());

        assert!(true);
    }

    #[test]
    fn family() {
        let ont_s = include_str!("../../ont/owl-xml/manual/family.owx");
        let (_, _) = read_ok(&mut ont_s.as_bytes());
    }
}
