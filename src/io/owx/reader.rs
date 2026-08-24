use curie::PrefixMapping;
use quick_xml::escape::unescape;
use quick_xml::name::ResolveResult;
use quick_xml::name::ResolveResult::Bound;

use crate::error::*;
use crate::io::{ParserConfiguration, StreamComponent};
use crate::model::*;
use crate::vocab::Facet;
use crate::vocab::Namespace::*;
use crate::vocab::OWL;
use crate::vocab::OWL2Datatype;

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::io::BufRead;

use quick_xml::NsReader;
use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::Event;

struct Read<A: ForIRI, R, B: AsRef<Build<A>>>
where
    R: BufRead,
{
    build: B,
    lax: bool,
    mapping: PrefixMapping,
    reader: NsReader<R>,
    base_iri: Option<String>,
    _marker: std::marker::PhantomData<A>,
}

/// Streams an OWX document as `StreamComponent`s, one top-level `<Ontology>`
/// child element at a time -- the streaming counterpart to [`read`]. Built
/// directly on [`Read`]'s existing per-event parsing, just yielding at each
/// point [`read`] used to call `ont.insert(...)`/loop again.
pub struct Reader<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> {
    r: Read<A, R, B>,
    buf: Vec<u8>,
    done: bool,
}

impl<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> Reader<A, R, B> {
    /// A `Reader` over `bufread`. Does no I/O itself -- parsing starts on
    /// the first call to `next()`.
    pub fn new(bufread: R, config: ParserConfiguration<A, B>) -> Self {
        Reader {
            r: Read {
                reader: NsReader::from_reader(bufread),
                build: config.build,
                lax: config.lax,
                mapping: PrefixMapping::default(),
                base_iri: None,
                _marker: std::marker::PhantomData,
            },
            buf: Vec::new(),
            done: false,
        }
    }

    /// One pull's worth of work: read events until there's a `StreamComponent`
    /// to yield, the `</Ontology>` close tag is reached (`Ok(None)`), or an
    /// error occurs.
    fn next_component(
        &mut self,
    ) -> Result<Option<StreamComponent<AnnotatedComponent<A>>>, HornedError> {
        loop {
            match self.r.reader.read_resolved_event_into(&mut self.buf)? {
                (ref ns, Event::Start(ref e)) | (ref ns, Event::Empty(ref e)) if is_owl(ns) => {
                    match e.local_name().as_ref() {
                        b"Ontology" => {
                            let s = get_attr_value_str(&mut self.r.reader, e, b"ontologyIRI")?;
                            if let Some(s) = s {
                                self.r.mapping.set_default(&s);
                                self.r.base_iri = Some(s);
                            }
                            let iri = get_iri_value_for(&mut self.r, e, b"ontologyIRI")?;
                            let viri = get_iri_value_for(&mut self.r, e, b"versionIRI")?;
                            return Ok(Some(StreamComponent::Component(AnnotatedComponent::new(
                                OntologyID { iri, viri },
                                BTreeSet::new(),
                            ))));
                        }
                        b"Prefix" => {
                            let iri = get_attr_value_str(&mut self.r.reader, e, b"IRI")?;
                            let prefix = get_attr_value_str(&mut self.r.reader, e, b"name")?;
                            match (prefix, iri) {
                                (Some(p), Some(i)) => {
                                    let _ = self.r.mapping.add_prefix(&p, &i);
                                    if p.is_empty() {
                                        self.r.mapping.set_default(&i);
                                    }
                                    return Ok(Some(StreamComponent::Prefix(p, i)));
                                }
                                (None, _) => {
                                    return Err(error_missing_attribute("IRI", &mut self.r));
                                }
                                (Some(_), None) => {
                                    return Err(error_missing_attribute("name", &mut self.r));
                                }
                            }
                        }
                        b"Import" => {
                            let iri = IRI::from_xml(&mut self.r, b"Import")?;
                            return Ok(Some(StreamComponent::Component(AnnotatedComponent::new(
                                Import(iri),
                                BTreeSet::new(),
                            ))));
                        }
                        _ => {
                            let aa = AnnotatedComponent::from_start(&mut self.r, e)?;
                            return Ok(Some(StreamComponent::Component(aa)));
                        }
                    }
                }
                (ref ns, Event::End(ref e)) if is_owl_name(ns, e, b"Ontology") => {
                    return Ok(None);
                }
                (_, Event::Eof) => {
                    return Err(error_eof(&self.r));
                }
                (_, Event::Text(ref t)) if !is_blank(t) && !self.r.lax => {
                    return Err(error_unexpected_text(&mut self.r));
                }
                (_, Event::CData(ref t)) if !is_blank(t) && !self.r.lax => {
                    return Err(error_unexpected_text(&mut self.r));
                }
                _ => {}
            }
        }
    }
}

impl<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> Iterator for Reader<A, R, B> {
    type Item = crate::io::Result<StreamComponent<AnnotatedComponent<A>>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.done {
            return None;
        }
        match self.next_component() {
            Ok(Some(item)) => Some(Ok(item)),
            Ok(None) => {
                self.done = true;
                None
            }
            Err(e) => {
                self.done = true;
                Some(Err(e))
            }
        }
    }
}

/// Stream `bufread` as `StreamComponent`s.
pub fn read_to_stream<A: ForIRI, B: AsRef<Build<A>>, R: BufRead>(
    bufread: R,
    config: ParserConfiguration<A, B>,
) -> Reader<A, R, B> {
    Reader::new(bufread, config)
}

/// Read the whole of `bufread` into an `O`. Drains a [`Reader`] internally,
/// so this is `read_to_stream` plus collecting every yielded item -- use
/// `read_to_stream` directly for a large document where materializing the
/// whole thing isn't necessary.
pub fn read<A: ForIRI, B: AsRef<Build<A>>, O: MutableOntology<A> + Default, R: BufRead>(
    bufread: &mut R,
    config: ParserConfiguration<A, B>,
) -> Result<(O, PrefixMapping), HornedError> {
    let mut reader = Reader::new(bufread, config);
    let mut ont: O = Default::default();

    for item in reader.by_ref() {
        match item? {
            StreamComponent::Component(aa) => {
                ont.insert(aa);
            }
            // Already applied to `reader.r.mapping` as it was read.
            StreamComponent::Prefix(..) => {}
        }
    }

    Ok((ont, reader.r.mapping))
}

fn decode_expand_curie_maybe<'a, A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
    val: &'a [u8],
) -> Result<Cow<'a, str>, HornedError> {
    // Okay, so a lot of matching, but without this the borrow checker
    // is gonna complain. This let's us do the following:
    // - if the CURIE can be decoded without replacement, and if it is
    //   already a complete IRI, we don't have to copy it, and we can
    //   return it as as borrowed string.
    // - in any other case, we need to perform a copy, otherwise the decoded
    //   string / unabbreviated IRI is not going to live long enough.
    #[cfg(feature = "encoding")]
    match r.reader.decode(val) {
        Cow::Borrowed(b) => expand_curie_maybe(r, b),
        Cow::Owned(o) => match expand_curie_maybe(r, &o) {
            Cow::Borrowed(b) => Cow::Owned(b.to_string()),
            Cow::Owned(o) => Cow::Owned(o),
        },
    }

    #[cfg(not(feature = "encoding"))]
    match r.reader.decoder().decode(val) {
        Ok(curie) => {
            // As with `get_attr_value_str`, decoding alone doesn't resolve
            // XML entity/character references (e.g. `&#39;`) -- unescape
            // before expanding, same as issue #239's other call site.
            let unescaped = unescape(&curie)
                .map_err(|e| HornedError::ParserError(Box::new(e), Location::Unknown))?;
            let cur = expand_curie_or_base_maybe(r, Cow::Owned(unescaped.into_owned()));
            Ok(cur)
        }
        Err(e) => Err(HornedError::from(e)),
    }
}

/// Expand a curie if there is an appropriate prefix
fn expand_curie_maybe<'a, A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
    val: Cow<'a, str>,
) -> Cow<'a, str> {
    match r.mapping.expand_curie_string(&val) {
        // If we expand use this
        Ok(n) => Cow::Owned(n),
        // Else assume it's a complete URI
        Err(_e) => val,
    }
}

/// Like [`expand_curie_maybe`], except a fragment-only value (starting
/// with `#`) is resolved against the ontology's base IRI rather than the
/// CURIE default prefix. Mirrors the identical guard in `get_iri_value`
/// (issue #212): the default/empty prefix may itself already end in
/// `#`, which would otherwise double up when concatenated with a
/// `#fragment` value (`prefix#` + `#fragment` = `prefix##fragment`).
/// `get_iri_value` special-cases this for the `IRI="..."` *attribute*
/// form; this does the same for the `<IRI>text</IRI>` *element content*
/// form (see issue #226 -- the attribute path was fixed, this sibling
/// path wasn't).
fn expand_curie_or_base_maybe<'a, A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
    val: Cow<'a, str>,
) -> Cow<'a, str> {
    if val.starts_with('#')
        && let Some(base) = r.base_iri.clone()
    {
        Cow::Owned(format!("{base}{val}"))
    } else {
        expand_curie_maybe(r, val)
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
        .map(|val| {
            // Next, decode it to obtain a `str`.
            let decoded = reader
                .decoder()
                .decode(val)
                .map_err(|err| HornedError::ParserError(Box::new(err), Location::Unknown))?;
            // Decoding alone is not sufficient: it only resolves the byte
            // encoding, not XML entity/character references, so e.g.
            // `Alzheimer&#39;s_Disease` would otherwise survive with the
            // literal `&#39;` still in it rather than becoming `Alzheimer's_Disease`
            // (see issue #239). Mirrors the same two-step handling `<Literal>`
            // text already does below.
            unescape(&decoded)
                .map(|s| s.to_string())
                .map_err(|err| HornedError::ParserError(Box::new(err), Location::Unknown))
        })
        .transpose()
}

/// Returns, if present, the IRI for the given opening tag.
fn get_iri_value<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
    event: &BytesStart,
) -> Result<Option<IRI<A>>, HornedError> {
    if let Some(raw) = get_attr_value_str(&mut r.reader, event, b"IRI")? {
        // Fragment-relative IRIs (starting with '#') must be resolved against the
        // ontology base IRI, not the CURIE default: the empty prefix may end with '#',
        // which would produce a doubled '##' when concatenated with a '#local' fragment.
        let base_iri = r.base_iri.clone();
        let resolved: Cow<str> = if raw.starts_with('#') {
            match base_iri {
                Some(base) => Cow::Owned(format!("{base}{raw}")),
                None => expand_curie_maybe(r, Cow::Owned(raw)),
            }
        } else {
            expand_curie_maybe(r, Cow::Owned(raw))
        };
        Ok(Some(r.build.as_ref().iri(resolved)))
    } else {
        get_iri_value_for(r, event, b"abbreviatedIRI")
    }
}

/// Returns, if present, the IRI included in the given attribute for the given opening tag.
fn get_iri_value_for<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
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
                r.build.as_ref().iri(
                    // or a curie
                    x,
                )
            }),
    )
}

fn decode_tag<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    tag: &[u8],
    r: &mut Read<A, R, B>,
) -> Result<String, HornedError> {
    Ok(r.reader.decoder().decode(tag)?.to_string())
}

fn error_missing_end_tag<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    tag: &[u8],
    r: &mut Read<A, R, B>,
    pos: u64,
) -> HornedError {
    match decode_tag(tag, r) {
        Ok(tag) => invalid_at! {pos, "Missing End Tag: expected {tag}"},
        Err(e) => e,
    }
}

fn error_missing_attribute<A: ForIRI, AT: Into<String>, R: BufRead, B: AsRef<Build<A>>>(
    attribute: AT,
    r: &mut Read<A, R, B>,
) -> HornedError {
    let attribute = attribute.into();
    let pos = r.reader.buffer_position();
    invalid_at! {pos, "Missing Attribute: expected {attribute}"}
}

fn error_eof<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(r: &Read<A, R, B>) -> HornedError {
    invalid_at! {r.reader.buffer_position(), "Unexpected EoF"}
}

fn error_unexpected_tag<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    tag: &[u8],
    r: &mut Read<A, R, B>,
) -> HornedError {
    match decode_tag(tag, r) {
        Ok(tag) => invalid_at! {r.reader.buffer_position(), "Unexpected tag: found {tag}"},
        Err(e) => e,
    }
}

fn error_unexpected_end_tag<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    tag: &[u8],
    r: &mut Read<A, R, B>,
) -> HornedError {
    match decode_tag(tag, r) {
        Ok(tag) => invalid_at! {r.reader.buffer_position(), "Unexpected end tag: expected {tag}"},
        Err(e) => e,
    }
}

fn error_unknown_entity<A: ForIRI, AA: Into<String>, R: BufRead, B: AsRef<Build<A>>>(
    kind: AA,
    found: &[u8],
    r: &mut Read<A, R, B>,
) -> HornedError {
    match decode_tag(found, r) {
        Ok(found) => {
            invalid_at! {r.reader.buffer_position(), "Unknown Entity: expected kind of {}, found {found}", kind.into()}
        }
        Err(e) => e,
    }
}

fn error_missing_element<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    tag: &[u8],
    r: &mut Read<A, R, B>,
) -> HornedError {
    match decode_tag(tag, r) {
        Ok(tag) => invalid_at! {r.reader.buffer_position(), "Missing Element: expected {tag}"},
        Err(e) => e,
    }
}

fn error_unexpected_text<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
) -> HornedError {
    invalid_at! {r.reader.buffer_position(), "Unexpected text content"}
}

// Insignificant whitespace between elements is normal, valid XML
// formatting; anything else appearing where only child elements are
// expected is malformed and was previously silently dropped (#72).
fn is_blank(bytes: &[u8]) -> bool {
    bytes.iter().all(u8::is_ascii_whitespace)
}

fn is_owl(res: &ResolveResult) -> bool {
    match res {
        Bound(ns) => ns.as_ref() == OWL.as_bytes(),
        // No `xmlns` was declared anywhere in scope for this unprefixed
        // element -- assume OWL rather than rejecting the document, since
        // that's what every real-world OWL/XML document that omits the
        // (redundant, given `<Ontology>` is unambiguously OWL) default
        // namespace declaration means in practice. `Unknown` (an explicit
        // but undeclared prefix) is left unrecognised, since that's a
        // genuine error rather than an omission.
        ResolveResult::Unbound => true,
        ResolveResult::Unknown(_) => false,
    }
}

fn is_owl_name(res: &ResolveResult, e: &BytesEnd, tag: &[u8]) -> bool {
    is_owl(res) && e.local_name().as_ref() == tag
}

trait FromStart<A: ForIRI>: Sized {
    fn from_start<R: BufRead, B: AsRef<Build<A>>>(
        r: &mut Read<A, R, B>,
        e: &BytesStart,
    ) -> Result<Self, HornedError>;
}

macro_rules! from_start {
    ($type:ident, $r:ident, $e:ident, $body:tt) => {
        impl<A: ForIRI> FromStart<A> for $type<A> {
            fn from_start<R: BufRead, B: AsRef<Build<A>>>(
                $r: &mut Read<A, R, B>,
                $e: &BytesStart,
            ) -> Result<$type<A>, HornedError>
                $body
        }
    };
}

/// Potentially unbalanced
fn named_entity_from_start<A, R, T, B>(
    r: &mut Read<A, R, B>,
    e: &BytesStart,
    tag: &[u8],
) -> Result<T, HornedError>
where
    A: ForIRI,
    R: BufRead,
    T: From<IRI<A>>,
    B: AsRef<Build<A>>,
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

fn from_start<A: ForIRI, R: BufRead, T: FromStart<A>, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
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
            if let Event::End(event) = r.reader.read_event_into(&mut buf)?
                && let b"Literal" = event.local_name().as_ref() { break; }

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
                    => return Err(invalid_at!(r.reader.buffer_position(), "Broken literal")),
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

fn axiom_from_start<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
    e: &BytesStart,
    axiom_kind: &[u8],
) -> Result<Component<A>, HornedError> {
    Ok(match axiom_kind {
        b"Annotation" => OntologyAnnotation(Annotation {
            ap: from_start(r, e)?,
            av: from_next(r)?,
            ann: Default::default(),
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
                ann: Annotation {
                    ap,
                    av,
                    ann: Default::default(),
                },
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

fn from_start_to_end<
    A: ForIRI,
    R: BufRead,
    T: FromStart<A> + std::fmt::Debug,
    B: AsRef<Build<A>>,
>(
    r: &mut Read<A, R, B>,
    e: &BytesStart,
    end_tag: &[u8],
) -> Result<Vec<T>, HornedError> {
    let v = vec![from_start(r, e)?];
    till_end_with(r, end_tag, v)
}

// Keep reading entities, till end_tag is reached
fn till_end<A: ForIRI, R: BufRead, T: FromStart<A> + std::fmt::Debug, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
    end_tag: &[u8],
) -> Result<Vec<T>, HornedError> {
    let operands: Vec<T> = Vec::new();
    till_end_with(r, end_tag, operands)
}

// Keep reading entities, till end_tag is reached
fn till_end_with<A: ForIRI, R: BufRead, T: FromStart<A> + std::fmt::Debug, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
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
            (_, Event::Eof) => {
                return Err(error_eof(r));
            }
            (_, Event::Text(ref t)) if !is_blank(t) && !r.lax => {
                return Err(error_unexpected_text(r));
            }
            (_, Event::CData(ref t)) if !is_blank(t) && !r.lax => {
                return Err(error_unexpected_text(r));
            }
            _ => {}
        }
    }
}

#[allow(clippy::type_complexity)]
fn object_cardinality_restriction<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
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
            0 => r.build.as_ref().class(OWL::Thing.as_ref()).into(),
            1 => vce.remove(0),
            _ => return Err(error_unexpected_tag(end_tag, r)),
        }),
    ))
}

fn data_cardinality_restriction<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
    e: &BytesStart,
    end_tag: &[u8],
) -> Result<(u32, DataProperty<A>, DataRange<A>), HornedError> {
    let n = get_attr_value_str(&mut r.reader, e, b"cardinality")?;
    let n = n.ok_or_else(|| error_missing_attribute("cardinality", r))?;

    let dp = from_next(r)?;
    let mut vdr: Vec<DataRange<_>> = till_end(r, end_tag)?;

    Ok((
        n.parse::<u32>()
            .map_err(|_s| HornedError::invalid("Failed to parse int"))?,
        dp,
        match vdr.len() {
            0 => r
                .build
                .as_ref()
                .datatype(OWL2Datatype::Literal.as_ref())
                .into(),
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
                (_, Event::Eof) => {
                    return Err(error_eof(r));
                }
                _ =>{
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
            _ => Err(error_unknown_entity("Individual", e.local_name().as_ref(), r))
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
            _ => Err(error_unknown_entity("AnnotationSubject", e.local_name().as_ref(), r))
        }

    }
}

impl<A: ForIRI> FromStart<A> for AnonymousIndividual<A> {
    fn from_start<R: BufRead, B: AsRef<Build<A>>>(
        r: &mut Read<A, R, B>,
        e: &BytesStart,
    ) -> Result<AnonymousIndividual<A>, HornedError> {
        let node_id = get_attr_value_str(&mut r.reader, e, b"nodeID")?
            .ok_or_else(|| error_missing_attribute("nodeID Expected", r))?;
        let ai: AnonymousIndividual<_> = r.build.as_ref().anon(node_id);
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
                                ("ObjectPropertyExpression",
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
                    return Err(error_unknown_entity("SubObjectPropertyExpression",
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
                    .map_err(|_| error_unknown_entity("FacetRestriction", &f, r))?,
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
    fn from_xml<R: BufRead, B: AsRef<Build<A>>>(
        newread: &mut Read<A, R, B>,
        end_tag: &[u8],
    ) -> Result<Self, HornedError> {
        Self::from_xml_nc(newread, end_tag)
    }

    fn from_xml_nc<R: BufRead, B: AsRef<Build<A>>>(
        newread: &mut Read<A, R, B>,
        end_tag: &[u8],
    ) -> Result<Self, HornedError>;
}

macro_rules! from_xml {
    ($type:ident, $r:ident, $end:ident, $body:tt) => {
        impl<A: ForIRI> FromXML<A> for $type<A> {
            fn from_xml_nc<R: BufRead, B: AsRef<Build<A>>>(
                $r: &mut Read<A, R, B>,
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
        let mut ann:BTreeSet<Annotation<_>> = BTreeSet::new();
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
                        b"Annotation" => {
                            ann.insert(Annotation::from_xml(r, b"Annotation")?);
                        }
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
                        av:av.unwrap(),
                        ann,
                    });
                },
                (_, Event::Eof) => {
                    return Err(error_eof(r));
                },
                (_, Event::Text(ref t)) if !is_blank(t) && !r.lax => {
                    return Err(error_unexpected_text(r));
                },
                (_, Event::CData(ref t)) if !is_blank(t) && !r.lax => {
                    return Err(error_unexpected_text(r));
                },
                _ =>{}
            }
        }
    }

}

fn from_next<A: ForIRI, R: BufRead, T: FromStart<A>, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
) -> Result<T, HornedError> {
    let mut buf = Vec::new();
    loop {
        let e = r.reader.read_resolved_event_into(&mut buf)?;
        match e {
            (ref ns, Event::Empty(ref e)) | (ref ns, Event::Start(ref e)) if is_owl(ns) => {
                return from_start(r, e);
            }
            (_, Event::Eof) => {
                return Err(error_eof(r));
            }
            (_, Event::Text(ref t)) if !is_blank(t) && !r.lax => {
                return Err(error_unexpected_text(r));
            }
            (_, Event::CData(ref t)) if !is_blank(t) && !r.lax => {
                return Err(error_unexpected_text(r));
            }
            _ => {}
        }
    }
}

fn discard_till_start<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
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

fn discard_till<A: ForIRI, R: BufRead, B: AsRef<Build<A>>>(
    r: &mut Read<A, R, B>,
    end: &[u8],
) -> Result<(), HornedError> {
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
            match e.local_name().as_ref() {
                b"Variable" => {
                    Ok(DArgument::Variable(Variable::from_start(r, e)?))
                }
                b"Literal" => {
                   Ok(DArgument::Literal(Literal::from_start(r, e)?))
                }
                _ => Err(error_unknown_entity("DArgument", e.local_name().as_ref(), r))
            }
    }
}

from_start! {
    IArgument, r, e,
    {

        match e.local_name().as_ref() {
                b"Variable" => {
                    Ok(IArgument::Variable(Variable::from_start(r, e)?))
                }
                b"NamedIndividual" => {
                    Ok(IArgument::Individual(NamedIndividual::from_start(r, e)?.into()))
                }
                b"AnonymousIndividual" => {
                    Ok(IArgument::Individual(AnonymousIndividual::from_start(r, e)?.into()))
                }
                _ => Err(error_unknown_entity("IArgument", e.local_name().as_ref(), r))
            }
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
                        let expanded = decode_expand_curie_maybe(r, e)?;
                        iri = Some(r.build.as_ref().iri(expanded));
                    },
                    (ref ns, Event::End(ref e))
                        if is_owl_name(ns, e, end) =>
                    {
                        return iri.ok_or_else(
                            || error_unexpected_end_tag(end, r)                        );
                    },
                    (_, Event::Eof) => {
                        return Err(error_eof(r));
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
    use crate::ontology::set::SetOntology;
    use std::collections::HashMap;

    pub fn read<R: BufRead>(
        bufread: &mut R,
    ) -> Result<
        (
            ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
            PrefixMapping,
        ),
        HornedError,
    > {
        super::read(bufread, Default::default())
    }

    pub fn read_ok<R: BufRead>(
        bufread: &mut R,
    ) -> (
        ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
        PrefixMapping,
    ) {
        let r = read(bufread);
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        let (o, m) = r.ok().unwrap();
        (o, m)
    }

    #[test]
    fn read_to_stream_yields_ontology_id_then_prefixes() {
        let ont_s = include_str!("../../ont/owl-xml/ont.owx");
        let b = Build::new_rc();
        let items: Vec<_> = super::read_to_stream(ont_s.as_bytes(), ParserConfiguration::new(&b))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        // The Ontology start tag (with its ontologyIRI/versionIRI attributes)
        // comes first in document order, before any of its <Prefix> children.
        assert!(matches!(
            &items[0],
            StreamComponent::Component(ac) if matches!(&ac.component, Component::OntologyID(_))
        ));
        let prefixes: Vec<_> = items[1..]
            .iter()
            .map(|item| match item {
                StreamComponent::Prefix(p, i) => (p.clone(), i.clone()),
                other => panic!("expected a Prefix item, got {other:?}"),
            })
            .collect();
        assert_eq!(prefixes.len(), 6);
        assert!(prefixes.contains(&("o".to_string(), "http://www.example.com/iri#".to_string())));
    }

    #[test]
    fn read_to_stream_agrees_with_read() {
        let ont_s = include_str!("../../ont/owl-xml/ont.owx");
        let b = Build::new_rc();

        let (drained, streamed_mapping): (SetOntology<RcStr>, _) =
            super::read_to_stream(ont_s.as_bytes(), ParserConfiguration::new(&b)).fold(
                (SetOntology::new_rc(), PrefixMapping::default()),
                |(mut ont, mut mapping), item| {
                    match item.unwrap() {
                        StreamComponent::Component(ac) => {
                            ont.insert(ac);
                        }
                        StreamComponent::Prefix(p, i) => {
                            let _ = mapping.add_prefix(&p, &i);
                        }
                    }
                    (ont, mapping)
                },
            );

        let (via_read, read_mapping): (ComponentMappedOntology<RcStr, RcAnnotatedComponent>, _) =
            read_ok(&mut ont_s.as_bytes());
        let via_read: SetOntology<RcStr> = via_read.into();

        assert_eq!(drained, via_read);
        assert_eq!(
            streamed_mapping.mappings().collect::<HashMap<_, _>>(),
            read_mapping.mappings().collect::<HashMap<_, _>>()
        );
    }

    #[test]
    fn test_simple_ontology_prefix() {
        let ont_s = include_str!("../../ont/owl-xml/ont.owx");
        let (_, mapping) = read_ok(&mut ont_s.as_bytes());

        let hash_map: HashMap<&String, &String> = mapping.mappings().collect();
        assert_eq!(6, hash_map.len());
    }

    #[test]
    fn test_ontology_empty_prefix() {
        let ont_s = include_str!("../../ont/owl-xml/manual/empty-prefix.owx");
        let (_, mapping) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(
            mapping.expand_curie_string(""),
            Ok(String::from("http://example.com/"))
        );
        assert_eq!(
            mapping
                .mappings()
                .map(|(k, v)| (k.as_ref(), v.as_ref()))
                .collect::<Vec<_>>(),
            vec![
                ("", "http://example.com/"),
                ("owl", "http://www.w3.org/2002/07/owl#"),
                ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
                ("xml", "http://www.w3.org/XML/1998/namespace"),
                ("xsd", "http://www.w3.org/2001/XMLSchema#"),
                ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
            ]
        )
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

    // https://github.com/phillord/horned-owl/issues/175
    // Annotation lacks an `ann` field for annotationAnnotations (OWL 2 spec).
    // The OWX reader fails entirely on a nested <Annotation> inside <Annotation>
    // ("Unexpected tag: found Annotation"), rather than just silently dropping it.
    #[test]
    fn test_nested_annotation_on_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/nested-annotation-on-annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_class().count(), 1);

        let annotated_component = ont
            .i()
            .component_for_kind(ComponentKind::AnnotationAssertion)
            .next()
            .unwrap();

        // The AnnotationAssertion carries one axiom annotation
        assert_eq!(annotated_component.ann.len(), 1);

        let axiom_ann = annotated_component.ann.iter().next().unwrap();
        assert_eq!(
            axiom_ann.av,
            crate::model::AnnotationValue::Literal(crate::model::Literal::Language {
                literal: "Comment on Comment".to_string(),
                lang: "en".to_string(),
            })
        );

        // The axiom annotation has one nested annotation
        assert_eq!(axiom_ann.ann.len(), 1);
        let nested_ann = axiom_ann.ann.iter().next().unwrap();
        assert_eq!(
            nested_ann.av,
            crate::model::AnnotationValue::Literal(crate::model::Literal::Language {
                literal: "Nested Comment".to_string(),
                lang: "en".to_string(),
            })
        );
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

    // Regression test for https://github.com/phillord/horned-owl/issues/229.
    // The bug itself was in the OFN writer, not the OWX reader, but an
    // ontology with both an Import and an ontology-level Annotation is the
    // trigger shape -- worth an explicit check here too, since (unlike the
    // other three formats' readers) this one has no #[files(...)]-based
    // resource test to pick new fixtures up automatically.
    #[test]
    fn test_import_and_ontology_annotation() {
        let ont_s = include_str!("../../ont/owl-xml/import-and-annotation.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().import().count(), 1);
        assert_eq!(ont.i().ontology_annotation().count(), 1);
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
        unreachable!();
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
        let ont_s = include_str!("../../ont/owl-xml/object-max-cardinality-unqualified.owx");
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

        let DatatypeDefinition { kind, range } = dd;
        {
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

        assert!(matches!(
            cl,
            ClassExpression::DataAllValuesFrom { dp: _, dr: _ }
        ));
    }

    #[test]
    fn data_exact_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/data-exact-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let cl = &ont.i().sub_class_of().next().unwrap().sup;
        assert_eq!(ont.i().sub_class_of().count(), 1);

        assert!(matches!(
            cl,
            ClassExpression::DataExactCardinality {
                n: _n,
                dp: _dp,
                dr: _dr
            }
        ));
    }

    #[test]
    fn data_unqualified_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/data-exact-cardinality-unqualified.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        let cl = &ont.i().sub_class_of().next().unwrap().sup;
        assert_eq!(ont.i().sub_class_of().count(), 1);

        assert!(matches!(
            cl,
            ClassExpression::DataExactCardinality {
                n: _n,
                dp: _dp,
                dr: _dr
            }
        ));
        if let ClassExpression::DataExactCardinality { n: _n, dp: _dp, dr } = cl {
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

        assert!(matches!(
            cl,
            ClassExpression::DataMinCardinality {
                n: _n,
                dp: _dp,
                dr: _dr
            }
        ));
    }

    #[test]
    fn data_max_cardinality() {
        let ont_s = include_str!("../../ont/owl-xml/data-max-cardinality.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let cl = &ont.i().sub_class_of().next().unwrap().sup;
        assert_eq!(ont.i().sub_class_of().count(), 1);

        assert!(matches!(
            cl,
            ClassExpression::DataMaxCardinality {
                n: _n,
                dp: _dp,
                dr: _dr
            }
        ));
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
    fn type_individual_datatype_unqualified() {
        let ont_s = include_str!("../../ont/owl-xml/type-individual-datatype-unqualified.owx");
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
        assert!(matches!(rule.head[0], Atom::ObjectPropertyAtom { .. }));
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
            panic!();
        }
    }

    #[test]
    fn swrl_individual() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_individual.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let rule = ont.i().rule().next().unwrap();
        dbg!(rule);
        assert!(matches!(
            rule.head[0],
            Atom::ClassAtom {
                arg: IArgument::Individual(Individual::Named(_)),
                ..
            }
        ));
        assert!(matches!(
            rule.body[0],
            Atom::ClassAtom {
                arg: IArgument::Individual(Individual::Anonymous(_)),
                ..
            }
        ));
    }

    #[test]
    fn swrl_different_individual() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_different_individuals.owx");

        let (_ont, _) = read_ok(&mut ont_s.as_bytes());
    }

    #[test]
    fn swrl_same_individual() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_same_individual.owx");

        let (_ont, _) = read_ok(&mut ont_s.as_bytes());
    }

    #[test]
    fn swrl_built_in() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_built_in.owx");

        let (_ont, _) = read_ok(&mut ont_s.as_bytes());
    }

    #[test]
    fn swrl_data_range() {
        let ont_s = include_str!("../../ont/owl-xml/swrl_data_range.owx");

        let (_ont, _) = read_ok(&mut ont_s.as_bytes());
    }

    #[test]
    fn family() {
        let ont_s = include_str!("../../ont/owl-xml/manual/family.owx");
        let (_, _) = read_ok(&mut ont_s.as_bytes());
    }

    #[test]
    fn does_read_terminate() {
        let empty = "".to_string();
        let r = read(&mut empty.as_bytes());

        assert!(r.is_err());
    }

    #[test]
    fn does_read_dc_terms_terminate() {
        let ont_s = include_str!("../../ont/owl-xml/manual/terms_short.owx");
        let r = read(&mut ont_s.as_bytes());

        assert!(r.is_err());
    }

    // https://github.com/phillord/horned-owl/issues/49 -- a document with no
    // `xmlns` declared anywhere (so unprefixed elements resolve to
    // `ResolveResult::Unbound`, not `Bound(OWL)`) used to be silently skipped
    // in its entirety, failing with an "Unexpected EoF" error instead of
    // being read as OWL/XML.
    #[test]
    fn missing_default_namespace_assumes_owl() {
        let ont_s = r##"<?xml version="1.0"?>
<Ontology ontologyIRI="http://example.org/tea.owl">
    <Prefix name="owl" IRI="http://www.w3.org/2002/07/owl#"/>
    <Declaration>
        <Class IRI="Tea"/>
    </Declaration>
</Ontology>"##;
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().declare_class().count(), 1);
    }

    // An explicit but undeclared prefix is a genuine error, not an omission
    // -- it should not be assumed to be OWL the way a fully-unbound
    // (no-xmlns-at-all) element is.
    #[test]
    fn unknown_prefix_is_still_an_error() {
        let ont_s = r##"<?xml version="1.0"?>
<foo:Ontology xmlns:foo="http://example.com/not-owl#" ontologyIRI="http://example.org/tea.owl">
    <foo:Declaration>
        <foo:Class IRI="Tea"/>
    </foo:Declaration>
</foo:Ontology>"##;
        let r = read(&mut ont_s.as_bytes());

        assert!(r.is_err(), "Expected a parse error, got {r:?}");
    }

    // https://github.com/phillord/horned-owl/issues/72 -- stray free text
    // between elements was silently dropped instead of being rejected.
    const BROKEN_OWX: &str = r##"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#"
     ontologyIRI="http://www.example.com/iri">
    I am broken
    <Declaration>
        <Class IRI="#C"/>
    </Declaration>
</Ontology>"##;

    #[test]
    fn stray_text_is_rejected_by_default() {
        let r: Result<
            (
                ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
                PrefixMapping,
            ),
            HornedError,
        > = super::read(&mut BROKEN_OWX.as_bytes(), Default::default());

        assert!(r.is_err(), "Expected a parse error, got {r:?}");
    }

    // Regression test for #22: parse errors in the OWX reader should carry a
    // byte position, not Location::Unknown.
    #[test]
    fn parse_error_carries_position() {
        let r: Result<
            (
                ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
                PrefixMapping,
            ),
            HornedError,
        > = super::read(&mut BROKEN_OWX.as_bytes(), Default::default());

        match r {
            Err(HornedError::ValidityError(_, location)) => {
                assert!(
                    !matches!(location, crate::error::Location::Unknown),
                    "expected a byte position in the error location, got Unknown"
                );
            }
            other => panic!("expected a ValidityError, got {other:?}"),
        }
    }

    #[test]
    fn stray_text_is_ignored_in_lax_mode() {
        let config = ParserConfiguration {
            lax: true,
            ..Default::default()
        };
        let r: Result<
            (
                ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
                PrefixMapping,
            ),
            HornedError,
        > = super::read(&mut BROKEN_OWX.as_bytes(), config);

        assert!(r.is_ok(), "Expected ontology, got failure: {:?}", r.err());
    }

    // Regression test: when a <Prefix name="" IRI="...#"/> declaration is present, an
    // IRI="#local" attribute must expand to ontologyIRI + "#local", not
    // prefixIRI + "#local" (which would yield a doubled ##).
    #[test]
    fn relative_iri_with_empty_prefix_no_double_hash() {
        let owx = r##"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#"
     xml:base="http://ontriscal"
     ontologyIRI="http://ontriscal">
    <Prefix name="" IRI="http://ontriscal#"/>
    <Declaration>
        <Class IRI="#MyClass"/>
    </Declaration>
</Ontology>"##;
        let (ont, _): (ComponentMappedOntology<RcStr, RcAnnotatedComponent>, _) =
            super::read(&mut owx.as_bytes(), Default::default()).unwrap();
        let dc = ont.i().declare_class().next().unwrap();
        assert_eq!(dc.0.0.to_string(), "http://ontriscal#MyClass");
    }

    // Regression test for #226: the same doubled-## bug as #212
    // (relative_iri_with_empty_prefix_no_double_hash above), but for a
    // fragment-only IRI given as <IRI>#local</IRI> *element content*
    // (e.g. an AnnotationAssertion subject) rather than an IRI="#local"
    // *attribute*. The #212 fix only covered the attribute form.
    #[test]
    fn relative_iri_element_content_with_empty_prefix_no_double_hash() {
        let owx = r##"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#"
     xml:base="http://ontriscal"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
     ontologyIRI="http://ontriscal">
    <Prefix name="" IRI="http://ontriscal#"/>
    <Prefix name="rdfs" IRI="http://www.w3.org/2000/01/rdf-schema#"/>
    <Declaration>
        <Class IRI="#MyClass"/>
    </Declaration>
    <AnnotationAssertion>
        <AnnotationProperty abbreviatedIRI="rdfs:comment"/>
        <IRI>#MyClass</IRI>
        <Literal>a comment</Literal>
    </AnnotationAssertion>
</Ontology>"##;
        let (ont, _): (ComponentMappedOntology<RcStr, RcAnnotatedComponent>, _) =
            super::read(&mut owx.as_bytes(), Default::default()).unwrap();
        let assertion = ont.i().annotation_assertion().next().unwrap();
        assert_eq!(assertion.subject.to_string(), "http://ontriscal#MyClass");
    }

    // Regression test for #239: an XML numeric character reference (e.g.
    // `&#39;` for an apostrophe) in an `IRI="..."` attribute must be
    // unescaped, not carried through raw. Real corpus ontologies (e.g.
    // APADISORDERS) use this for apostrophes in class-name fragments, like
    // `#Alzheimer&#39;s_Disease`.
    #[test]
    fn iri_attribute_unescapes_xml_entity() {
        let owx = r##"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#" ontologyIRI="http://ex.com/o">
    <Declaration>
        <Class IRI="http://ex.com/o#Alzheimer&#39;s_Disease"/>
    </Declaration>
</Ontology>"##;
        let (ont, _): (ComponentMappedOntology<RcStr, RcAnnotatedComponent>, _) =
            super::read(&mut owx.as_bytes(), Default::default()).unwrap();
        let dc = ont.i().declare_class().next().unwrap();
        assert_eq!(dc.0.0.to_string(), "http://ex.com/o#Alzheimer's_Disease");
    }

    // Same bug, but for the <IRI>text</IRI> *element content* form (e.g. an
    // AnnotationAssertion subject) rather than the IRI="..." attribute.
    #[test]
    fn iri_element_content_unescapes_xml_entity() {
        let owx = r##"<?xml version="1.0"?>
<Ontology xmlns="http://www.w3.org/2002/07/owl#"
     xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
     ontologyIRI="http://ex.com/o">
    <Declaration>
        <Class IRI="http://ex.com/o#Alzheimer's_Disease"/>
    </Declaration>
    <AnnotationAssertion>
        <AnnotationProperty abbreviatedIRI="rdfs:comment"/>
        <IRI>http://ex.com/o#Alzheimer&#39;s_Disease</IRI>
        <Literal>a comment</Literal>
    </AnnotationAssertion>
</Ontology>"##;
        let (ont, _): (ComponentMappedOntology<RcStr, RcAnnotatedComponent>, _) =
            super::read(&mut owx.as_bytes(), Default::default()).unwrap();
        let assertion = ont.i().annotation_assertion().next().unwrap();
        assert_eq!(
            assertion.subject.to_string(),
            "http://ex.com/o#Alzheimer's_Disease"
        );
    }
}
