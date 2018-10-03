use curie::PrefixMapping;

use model::*;
use vocab;


use std::collections::BTreeSet;
use std::io::BufRead;

use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::Event;
use quick_xml::Reader;

use failure::Error;

#[derive(Debug, Fail)]
enum ReadError {
    #[fail(display="End Tag Arrived Unexpectedly: {} at {}", tag, pos)]
    UnexpectedEndTag{tag:String,pos:usize},

    #[fail(display="Missing End Tag: expected {} after {}", tag, pos)]
    MissingEndTag{tag:String,pos:usize},

    #[fail(display="Missing element: Expected {} at {}", tag, pos)]
    MissingElement{tag:String,pos:usize},

    #[fail(display="Missing attribute: Expected {} at {}", attribute, pos)]
    MissingAttribute{attribute:String,pos:usize},

    #[fail(display="Unknown Entity: Expected Kind of {}, found {} at {}", kind, found, pos)]
    UnknownEntity{kind:String, found:String, pos:usize},

    #[fail(display="Unexpected Tag: found {} at {}", tag, pos)]
    UnexpectedTag{tag:String,pos:usize},

    #[fail(display="Unexpected End of File: {}", pos)]
    UnexpectedEof{pos:usize}
}

struct Read<'a, R>
where
    R: BufRead,
{
    build: &'a Build,
    mapping: PrefixMapping,
    reader: Reader<R>,
    buf:Vec<u8>,
    ns_buf:Vec<u8>
}

pub fn read<R: BufRead>(bufread: &mut R)
             -> Result<(Ontology,PrefixMapping),Error>
{
    let b = Build::new();
    read_with_build(bufread, &b)
}

pub fn read_with_build<R: BufRead>(bufread: &mut R, build: &Build) ->
    Result<(Ontology,PrefixMapping),Error>
{
    let reader: Reader<&mut R> = Reader::from_reader(bufread);
    let mut ont = Ontology::new();
    let mapping = PrefixMapping::default();

    let mut r = Read{reader:reader, build:build,
                     mapping: mapping,
                     buf:Vec::new(), ns_buf:Vec::new()};

    loop {

        match read_event(&mut r)? {
            (ref ns, Event::Start(ref e))
                |
            (ref ns, Event::Empty(ref e))
                if *ns == b"http://www.w3.org/2002/07/owl#" =>
            {
                match e.local_name() {
                    b"Ontology" => {
                        let s = attrib_value(&mut r, e, b"ontologyIRI")?;
                        if s.is_some() {
                            r.mapping.set_default(&s.unwrap());
                        }

                        ont.id.iri = read_a_iri_attr
                            (&mut r, e,
                             b"ontologyIRI")?;
                        ont.id.viri = read_a_iri_attr
                            (&mut r, e,
                             b"versionIRI")?;
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
                        ont.insert(
                            Import(IRI::from_xml(&mut r, b"Import")?)
                        );
                    }
                    _ => {
                        let aa = AnnotatedAxiom::from_start(&mut r, e)?;
                        ont.insert(aa);
                    }
                }
            }
            (ref ns,Event::End(ref e))
                if is_owl_name(ns, e, b"Ontology")
                => {
                    break;
                }
            _ =>{}
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
fn read_event<R:BufRead>(read:&mut Read<R>)
                         -> Result<(Vec<u8>, Event<'static>),Error>
{
    let r = read.reader.read_namespaced_event(&mut read.buf,
                                              &mut read.ns_buf);

    match r {
        Ok((_, Event::Eof)) => {
            Err(ReadError::UnexpectedEof{pos:read.reader.buffer_position()}
                .into()
            )
        }
        Ok((option_ns, event)) => {
            Ok((option_ns.unwrap_or(b"").to_owned(),
                event.into_owned()))
        }
        Err(r) => {
            Err(r.into())
        }
    }
}

fn decode_expand_curie_maybe<R:BufRead>(r: &mut Read<R>, val:&[u8]) -> String{
    let s = r.reader.decode(val).into_owned();
    expand_curie_maybe(r, s)
}


/// Expand a curie if there is an appropriate prefix
fn expand_curie_maybe<R:BufRead>(r: &mut Read<R>, val:String) -> String {
    match r.mapping.expand_curie_string(&val) {
        // If we expand use this
        Ok(n) => n,
        // Else assume it's a complete URI
        Err(_e) => val,
    }
}

fn attrib_value<R:BufRead>(r: &mut Read<R>, event: &BytesStart,
                           tag:&[u8]) -> Result<Option<String>,Error> {
    for res in event.attributes() {
        let attrib = res?;
        if attrib.key == tag {
            return Ok(Some(r.reader.decode
                           (&attrib.value).into_owned()));
        }
    }

    Ok(None)
}

fn read_iri_attr<R:BufRead>(r: &mut Read<R>, event: &BytesStart)
                            -> Result<Option<IRI>,Error> {
    let iri = read_a_iri_attr(r, event, b"IRI")?;
    Ok(
        if iri.is_some() {iri}
        else {read_a_iri_attr(r, event, b"abbreviatedIRI")?}
    )
 }

fn read_a_iri_attr<R:BufRead>(r: &mut Read<R>,
                              event: &BytesStart, tag:&[u8])
                              -> Result<Option<IRI>,Error> {
    Ok(
        // check for the attrib, if malformed return
        attrib_value(r, event, tag)?.
        // or transform the some String
            map(|st|
                // Into an iri
                r.build.iri(
                    // or a curie
                    expand_curie_maybe(r, st))))
}

fn error_missing_end_tag<R:BufRead>(tag:&[u8], r:&mut Read<R>, pos: usize)
                                    -> Error
{
    ReadError::MissingEndTag{
        tag: r.reader.decode(tag).into_owned(),
        pos: pos
    }
    .into()
}

fn error_missing_attribute<A:Into<String>,R:BufRead>
    (attribute:A, r:&mut Read<R>)
                                      -> Error
{
    ReadError::MissingAttribute{
        attribute:attribute.into(),
        pos:r.reader.buffer_position()}
    .into()
}

fn error_unexpected_tag<R:BufRead>(tag:&[u8], r: &mut Read<R>)
                                       -> Error
{
    ReadError::UnexpectedTag{tag:r.reader.decode(tag).into_owned(),
                             pos:r.reader.buffer_position()}.into()
}


fn error_unexpected_end_tag<R:BufRead>(tag:&[u8], r: &mut Read<R>)
                                       -> Error
{
    ReadError::UnexpectedEndTag{tag:r.reader.decode(tag).into_owned(),
                                pos:r.reader.buffer_position()}.into()
}

fn error_unknown_entity<A:Into<String>, R:BufRead>(kind:A,
                                                   found: &[u8],
                                                   r: &mut Read<R>)
                                                   -> Error {
    ReadError::UnknownEntity{
        kind: kind.into(),
        found: r.reader.decode(found).into_owned(),
        pos:r.reader.buffer_position()
    }.into()
}

fn error_missing_element<R:BufRead>(tag:&[u8], r: &mut Read<R>)
    -> Error {
    ReadError::MissingElement{
        tag: r.reader.decode(tag).into_owned(),
        pos: r.reader.buffer_position()
    }.into()
}

fn is_owl(ns:&[u8]) -> bool {
    ns == vocab::OWL
}

fn is_owl_name(ns:&[u8], e:&BytesEnd, tag:&[u8]) -> bool {
    is_owl(ns) && e.local_name() == tag
}

trait FromStart: Sized {
    fn from_start<R:BufRead>(r:&mut Read<R>, e:&BytesStart) -> Result<Self,Error>;
}

macro_rules! from_start {
    ($type:ident, $r:ident, $e:ident, $body:tt) => {
        impl FromStart for $type{
            fn from_start<R: BufRead>($r: &mut Read<R>, $e:&BytesStart)
                                      -> Result<$type,Error> {

                $body
            }
        }
    }
}

/// Potentially unbalanced
fn named_entity_from_start<R,T>(r:&mut Read<R>, e:&BytesStart, tag:&[u8])
                                -> Result<T,Error>
    where R:BufRead,
          T:From<IRI>
{
    if let Some(iri) = read_iri_attr(r, e)? {
        if e.local_name() == tag {
            return Ok(T::from(iri));
        }
        else {
            return Err(error_unknown_entity(::std::str::from_utf8(tag).unwrap(),
                                            e.local_name(),r ));
        }
    }
    return Err(error_missing_element(b"IRI",r));
}

fn literal_from_start<R:BufRead>(r:&mut Read<R>, e: &BytesStart)
                      -> Result<AnnotationValue,Error> {

    let datatype_iri = read_a_iri_attr(r, e, b"datatypeIRI")?;
    let lang = attrib_value(r, e, b"xml:lang")?;

    let mut literal:Option<String> = None;

    loop {
        let mut e = read_event(r)?;
        match e {
            (_, Event::Text(ref e)) =>
            {
                literal = Some(r.reader.decode(e).into_owned());
            }
            (ref ns, Event::End(ref mut e))
                if is_owl_name(ns, e, b"Literal") =>
            {
                return Ok(AnnotationValue::PlainLiteral
                          {
                              datatype_iri: datatype_iri,
                              lang: lang,
                              literal: literal
                          });
            }
            _ => {
            }
        }
    }
}

fn from_start<R:BufRead, T:FromStart>(r:&mut Read<R>, e: &BytesStart)
                                      -> Result<T, Error>{
    T::from_start(r, e)
}

from_start! {
    AnnotationValue, r, e, {
        match e.local_name() {
            b"Literal" => {
                literal_from_start(r, e)
            }
            b"AbbreviatedIRI"|b"IRI" => {
                Ok(AnnotationValue::IRI(IRI::from_xml(r, e.local_name())?))
            }
            _ => {
                return Err
                    (error_unexpected_tag(e.local_name(), r));
            }
        }
    }
}

from_start! {
    AnnotationProperty, r, e,
    {
        named_entity_from_start(r, e, b"AnnotationProperty")
    }
}


fn axiom_from_start<R:BufRead>(r:&mut Read<R>, e:&BytesStart, axiom_kind:&[u8])
                               -> Result<Axiom, Error> {
    Ok(
        match axiom_kind
        {
            b"TransitiveObjectProperty" => {
                TransitiveObjectProperty
                    (from_start(r, e)?).into()
            }
            b"DisjointClasses" => {
                DisjointClass(
                    from_start(r, e)?,
                    from_next_tag(r)?
                ).into()
            }
            b"EquivalentClasses" => {
                EquivalentClass(
                    from_start(r, e)?,
                    from_next_tag(r)?
                ).into()
            }
            b"SubAnnotationPropertyOf" => {
                SubAnnotationProperty{
                    super_property:from_start(r, e)?,
                    sub_property:from_next_tag(r)?
                }.into()
            }
            b"InverseObjectProperties" => {
                InverseObjectProperty(
                    from_start(r, e)?,
                    from_next_tag(r)?
                ).into()
            }
            b"SubClassOf" => {
                let super_class = from_start(r, e)?;
                let sub_class = from_next_tag(r)?;
                SubClass{
                    super_class, sub_class
                }.into()
            }
            b"SubObjectPropertyOf" => {
                SubObjectProperty{
                    super_property:from_start(r, e)?,
                    sub_property:from_next_tag(r)?
                }.into()
            }
            b"Declaration" => {
                declaration(
                    from_start(r, e)?
                )
            }
            b"Annotation" => {
                OntologyAnnotation (
                    Annotation {
                        annotation_property:from_start(r, e)?,
                        annotation_value:from_next_tag(r)?
                    }
                ).into()
            }
            b"AnnotationAssertion" => {
                let annotation_property = from_start(r, e)?;
                let annotation_subject = from_next_tag(r)?;
                let annotation_value = from_next_tag(r)?;

                AssertAnnotation {
                    annotation_subject: annotation_subject,
                    annotation: Annotation {
                        annotation_property, annotation_value
                    }
                }.into()
            }
            _ => {
                return Err(error_unexpected_tag(axiom_kind,r));
            }
        }
    )
}

// Keep reading entities, till end_tag is reached
fn till_end<R:BufRead, T:FromStart>(r:&mut Read<R>,
                            end_tag: &[u8])
                            -> Result<Vec<T>,Error> {
    let mut operands:Vec<T> = Vec::new();

    loop {
        let e = read_event(r)?;
        match e {
            (ref ns, Event::Empty(ref e))
                if is_owl(ns) =>
            {
                let op = T::from_start(r, e)?;
                operands.push(op);
            }
            (ref ns, Event::Start(ref e))
                if is_owl(ns) =>
            {
                let op = T::from_start(r, e)?;
                operands.push(op);
                discard_till(r, e.local_name())?;
            }
            (ref ns, Event::End(ref e))
                if is_owl_name(ns, e, end_tag)
                =>
            {
                return Ok(operands);
            }
            _=>{}
        }
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
                    let o = from_next_tag(r)?;
                    let ce = Box::new(from_next_tag(r)?);
                    ClassExpression::Some{o,ce}
                }
                b"ObjectAllValuesFrom" => {
                    let o = from_next_tag(r)?;
                    let ce = Box::new(from_next_tag(r)?);
                    ClassExpression::Only{o,ce}
                }
                b"ObjectIntersectionOf" => {
                    let o = till_end(r, b"ObjectIntersectionOf")?;
                    ClassExpression::And{o}
                }
                b"ObjectUnionOf" => {
                    let o = till_end(r, b"ObjectUnionOf")?;
                    ClassExpression::Or{o}
                }
                b"ObjectComplementOf" => {
                    ClassExpression::Not
                    {ce: Box::new(from_next_tag(r)?)}
                }
                _ => {
                    return Err(error_unexpected_tag(e.local_name(), r))
                }
            }
        )
    }
}

from_start! {
    AnnotatedAxiom, r, e,
    {
        let mut annotation: BTreeSet<Annotation> = BTreeSet::new();
        let mut axiom: Option<Axiom> = None;
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
                            axiom =
                                Some(axiom_from_start(r,e,axiom_kind)?);
                        }
                    }
                }
                (ref ns, Event::End(ref e))
                    if is_owl_name(ns, e, axiom_kind) =>
                {
                    if axiom.is_none() {
                        return Err(error_unexpected_end_tag(axiom_kind, r));
                    }
                    return Ok(AnnotatedAxiom{
                        annotation:annotation, axiom:axiom.unwrap()
                    })
                },
                _=>{
                }
            }
        }
    }
}


from_start!{
    Class, r, e,
    {
        named_entity_from_start(r, e, b"Class")
    }
}

from_start!{
    ObjectProperty, r, e,
    {
        named_entity_from_start(r, e, b"ObjectProperty")
    }
}

from_start!{
    DataProperty, r, e,
    {
        named_entity_from_start(r, e, b"DataProperty")
    }
}

from_start!{
    NamedIndividual, r, e,
    {
        named_entity_from_start(r, e, b"NamedIndividual")
    }
}

from_start!{
    ObjectPropertyExpression, r, e,
    {
        Ok(
            match e.local_name() {
                b"ObjectPropertyChain" => {
                    let o = till_end(r, b"ObjectPropertyChain")?;
                    ObjectPropertyExpression::ObjectPropertyChain(o)

                }
                b"ObjectProperty" => {
                    ObjectPropertyExpression::
                    ObjectProperty(from_start(r, e)?)
                }
                _ => {
                    return Err(error_unknown_entity("Object Property",
                                                    e.local_name(),
                                                    r));
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
                _=> {
                    return Err(error_unknown_entity("NamedEntity",
                                                    e.local_name(),r ));
                }
            }
        )
    }
}




trait FromXML: Sized {
    fn from_xml<R: BufRead>(newread: &mut Read<R>,
                            end_tag: &[u8]) -> Result<Self,Error> {

        let s = Self::from_xml_nc(newread, end_tag);
        newread.buf.clear();
        s
    }

    fn from_xml_nc<R: BufRead>(newread: &mut Read<R>,
                               end_tag: &[u8]) -> Result<Self,Error>;

}

macro_rules! from_xml {
    ($type:ident, $r:ident, $end:ident, $body:tt) => {
        impl FromXML for $type {
            fn from_xml_nc<R: BufRead>($r: &mut Read<R>, $end:&[u8])
                                       -> Result<$type,Error> {

                $body
            }
        }
    }
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
                            ap = Some(AnnotationProperty::from_start(r, e)?),
                        _ =>
                            av = Some(AnnotationValue::from_start(r, e)?),
                    }
                }
                (ref ns, Event::End(ref e))
                    if is_owl_name(ns, e, end) =>
                {
                    if ap.is_none() || av.is_none() {
                        return Err(error_unexpected_end_tag(end, r));
                    }
                    return Ok(Annotation{
                        annotation_property:ap.unwrap(),
                        annotation_value:av.unwrap()
                    });
                },
                _ =>{}
            }
        }
    }

}

fn from_next_tag<R:BufRead, T:FromStart>(r: &mut Read<R>)-> Result<T,Error> {
    loop {
        let e = read_event(r)?;
        match e {
            (ref ns, Event::Empty(ref e))
                |
            (ref ns, Event::Start(ref e))
                if is_owl(ns) =>
            {
                return T::from_start(r, e);
            }
            _ =>{}
        }
    }
}

fn discard_till<R:BufRead>(r:&mut Read<R>, end:&[u8]) -> Result<(),Error> {
    let pos = r.reader.buffer_position();
    loop {
        let e = read_event(r)?;

        match e {
            (ref ns, Event::End(ref e))
                if is_owl_name(ns, e, end) =>
            {
                return Ok(());
            }
            (_, Event::Eof) => {
                return Err(error_missing_end_tag(end,r, pos));
            }
            _=> {},
        }
    }
}

from_xml! {
    NamedEntity,r, end,
    {
        let ne = from_next_tag(r);
        discard_till(r, end)?;
        return ne;
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
                            || error_unexpected_end_tag(end, r)
                        );
                    },
                    _=>{}
                }
            }
        }
}


#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;

    fn read_ok<R:BufRead>(bufread: &mut R) -> (Ontology,PrefixMapping)
    {
        let r = read(bufread);
        assert!(r.is_ok(),
                "Expected ontology, got failure:{:?}",
                r.err());
        r.ok().unwrap()
    }

    #[test]
    fn test_simple_ontology_prefix() {
        let ont_s = include_str!("../ont/owl-xml/one-ont.owl");
        let (_, mapping) = read_ok(&mut ont_s.as_bytes());

        let hash_map: HashMap<&String, &String> = mapping.mappings().collect();
        assert_eq!(6, hash_map.len());
    }

    #[test]
    fn test_simple_ontology() {
        let ont_s = include_str!("../ont/owl-xml/one-ont.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(*ont.id.iri.unwrap(), "http://example.com/iri");
    }

    #[test]
    fn test_simple_ontology_rendered_by_horned() {
        let ont_s = include_str!("../ont/owl-xml/one-ont-from-horned.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(*ont.id.iri.unwrap(), "http://example.com/iri");
    }

    #[test]
    fn test_one_class() {
        let ont_s = include_str!("../ont/owl-xml/one-class.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_class().count(), 1);
        assert_eq!(
            String::from(&ont.declare_class().next().unwrap().0),
            "http://example.com/iri#C"
        );
    }

    #[test]
    fn test_class_with_annotation() {
        let ont_s = include_str!("../ont/owl-xml/declaration-with-annotation.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_class().count(), 1);

        let aa = ont.annotated_axiom
            (AxiomKind::DeclareClass).next().unwrap();

        assert_eq!(aa.annotation.len(), 1);
    }

    #[test]
    fn test_one_class_fqn() {
        let ont_s = include_str!("../ont/owl-xml/one-class-fully-qualified.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_class().count(), 1);
        assert_eq!(
            String::from(&ont.declare_class().next().unwrap().0),
            "http://www.russet.org.uk/#C"
        );
    }

    #[test]
    fn test_ten_class() {
        let ont_s = include_str!("../ont/owl-xml/o10.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_class().count(), 10);
    }

    #[test]
    fn test_one_property() {
        let ont_s = include_str!("../ont/owl-xml/one-oproperty.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_object_property().count(), 1);
    }

    #[test]
    fn test_one_subclass() {
        let ont_s = include_str!("../ont/owl-xml/one-subclass.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
    }

    #[test]
    fn test_one_some() {
        let ont_s = include_str!("../ont/owl-xml/one-some.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
        assert_eq!(ont.declare_object_property().count(), 1);
    }

    #[test]
    fn test_one_only() {
        let ont_s = include_str!("../ont/owl-xml/one-only.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
        assert_eq!(ont.declare_class().count(), 2);
        assert_eq!(ont.declare_object_property().count(), 1);
    }

    #[test]
    fn test_one_and() {
        let ont_s = include_str!("../ont/owl-xml/one-and.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
    }

    #[test]
    fn test_one_or() {
        let ont_s = include_str!("../ont/owl-xml/one-or.owl");
        let (ont,_ ) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
    }

    #[test]
    fn test_one_not() {
        let ont_s = include_str!("../ont/owl-xml/one-not.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
    }

    #[test]
    fn test_one_annotation_property() {
        let ont_s = include_str!("../ont/owl-xml/one-annotation-property.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        assert_eq!(ont.declare_annotation_property().count(), 1);
    }

    #[test]
    fn test_one_annotation() {
        let ont_s = include_str!("../ont/owl-xml/one-annotation.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        assert_eq!(ont.declare_annotation_property().count(), 1);
        assert_eq!(ont.assert_annotation().count(), 1);
    }

    #[test]
    fn test_one_label_non_abbreviated() {
        let ont_s = include_str!("../ont/owl-xml/one-label-non-abbreviated-iri.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.assert_annotation().count(), 1);
    }


    #[test]
    fn test_one_label() {
        let ont_s = include_str!("../ont/owl-xml/one-label.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.assert_annotation().count(), 1);
    }

    #[test]
    fn test_one_ontology_annotation() {
        let ont_s = include_str!("../ont/owl-xml/one-ontology-annotation.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.ontology_annotation().count(), 1);
    }

    #[test]
    fn test_one_equivalent_class() {
        let ont_s = include_str!("../ont/owl-xml/one-equivalent.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.equivalent_class().count(), 1);
    }

    #[test]
    fn test_one_disjoint_class() {
        let ont_s = include_str!("../ont/owl-xml/one-disjoint.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.disjoint_class().count(), 1);
    }

    #[test]
    fn test_one_sub_property() {
        let ont_s = include_str!("../ont/owl-xml/one-suboproperty.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_object_property().count(), 1);
    }

    #[test]
    fn test_one_inverse_property() {
        let ont_s = include_str!("../ont/owl-xml/inverse-properties.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.inverse_object_property().count(), 1);
    }

    #[test]
    fn test_one_transitive_property() {
        let ont_s = include_str!("../ont/owl-xml/transitive-properties.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.transitive_object_property().count(), 1);
    }

    #[test]
    fn test_subproperty_chain() {
        let ont_s = include_str!("../ont/owl-xml/subproperty-chain.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_object_property().count(), 1);
    }

    #[test]
    fn test_annotation_on_annotation() {
        let ont_s = include_str!("../ont/owl-xml/annotation-with-annotation.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());


        let mut ann_i = ont.annotated_axiom(AxiomKind::AssertAnnotation);
        let ann:&AnnotatedAxiom = ann_i.next().unwrap();
        assert_eq!(ann.annotation.len(), 1);
    }

    #[test]
    fn annotated_transitive() {
        let ont_s = include_str!("../ont/owl-xml/annotation-on-transitive.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let annotated_axiom = ont.annotated_axiom
            (AxiomKind::TransitiveObjectProperty).next().unwrap();
        assert_eq!(annotated_axiom.annotation.len(), 1);
    }

    #[test]
    fn two_annotated_transitive() {
        let ont_s = include_str!("../ont/owl-xml/two-annotation-on-transitive.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let annotated_axiom = ont.annotated_axiom
            (AxiomKind::TransitiveObjectProperty).next().unwrap();

        assert_eq!(annotated_axiom.annotation.len(), 2);
    }
    #[test]
    fn test_sub_annotation() {
        let ont_s = include_str!("../ont/owl-xml/sub-annotation.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_annotation_property().count(), 1);
    }


    #[test]
    fn test_data_property() {
        let ont_s = include_str!("../ont/owl-xml/data-property.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_data_property().count(), 1);
    }

    #[test]
    fn test_named_individual() {
        let ont_s = include_str!("../ont/owl-xml/named-individual.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_named_individual().count(), 1);
    }

    #[test]
    fn test_import() {
        let ont_s = include_str!("../ont/owl-xml/import.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.import().count(), 1);
    }
}
