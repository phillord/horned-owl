use curie::PrefixMapping;

use model::*;
use vocab;


use std::collections::BTreeSet;
use std::io::BufRead;
use std::str::from_utf8;

use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::Event;
use quick_xml::Reader;

use failure::Error;

#[derive(Debug, Fail)]
enum ReadError {
    #[fail(display="End Tag Arrived Unexpectedly: {} at {}", tag, pos)]
    UnexpectedEndTag{tag:String,pos:usize},

    #[fail(display="Missing element: Expected {} at {}", tag, pos)]
    MissingElement{tag:String,pos:usize},

    #[fail(display="Missing attribute: Expected {} at {}", attribute, pos)]
    MissingAttribute{attribute:String,pos:usize},

    #[fail(display="Unknown Entity: Expected Kind of {}, found {} at {}",kind, found, pos)]
    UnknownEntity{kind:String, found:String, pos:usize}
}

#[derive(Copy, Clone)]
enum State {
    Top,
    Ontology,
}

struct Read<'a, R>
where
    R: BufRead,
{
    // TODO Ontology needs to be removed from here once we have the
    // trait based system up and running.
    ont: Ontology,
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
    let ont = Ontology::new();
    let mapping = PrefixMapping::default();

    let mut read = Read::new(reader, ont, mapping, build);
    read.parse()?;
    Ok((read.ont, read.mapping))
}

impl<'a, R: BufRead> Read<'a, R> {
    fn new(reader: Reader<R>, ont: Ontology, mapping: PrefixMapping,
           build: &Build
    ) -> Read<R> {
        Read {
            reader: reader,
            ont: ont,
            mapping: mapping,
            build: build,
            buf:Vec::new(),
            ns_buf:Vec::new()
        }
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
    fn read_event(&mut self)
                  -> (Vec<u8>, Event<'static>)
    {
        let r = self.reader.read_namespaced_event(&mut self.buf,
                                                  &mut self.ns_buf);
        match r {
            Ok((option_ns, event)) => {
                (option_ns.unwrap_or(b"").to_owned(),
                 event.into_owned())
            }
            Err(_) => {
                panic!("We panic a lot");
            }
        }
    }

    fn parse(&mut self) -> Result<(),Error>{
        let mut state = State::Top;

        loop {

            let event_tuple;
            {
                event_tuple = self.read_event();
            }

            //println!("r:{:?}", r);
            match event_tuple {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    match (&state, e.local_name()) {
                        (&State::Top, b"Ontology") => {
                            self.ontology(e);
                            state = State::Ontology;
                        }
                        (&State::Ontology, b"Declaration") => {
                            self.declaration()?;
                        }
                        (&State::Ontology, b"SubClassOf") => {
                            self.subclassof();
                        }
                        (&State::Ontology, b"Prefix") => {
                            self.prefix(e)?;
                        }
                        (&State::Ontology, b"AnnotationAssertion") => {
                            self.annotation_assertion();
                        }
                        (&State::Ontology, b"Annotation") => {
                            self.ontology_annotation();
                        }
                        (&State::Ontology, b"EquivalentClasses") => {
                            self.equivalent_class();
                        }
                        (&State::Ontology, b"DisjointClasses") => {
                            self.disjoint_class();
                        }
                        (&State::Ontology, b"SubObjectPropertyOf") => {
                            self.sub_object_property();
                        }
                        (&State::Ontology, b"InverseObjectProperties") => {
                            self.inverse_object_property();
                        }
                        (&State::Ontology, b"TransitiveObjectProperty") => {
                            self.transitive_object_property();
                        }
                        (&State::Ontology, b"SubAnnotationPropertyOf") => {
                            self.sub_annotation_property();
                        }
                        (_, n) => {
                            self.unimplemented_owl(n);
                        }
                    }
                }
                (_, Event::Eof) => {
                    break;
                }
                // Ok((_,Event::End(ref mut e)))=>{
                //     println!("End Event:{:?}", reader.decode(e.local_name()));
                // }
                // Ok((_,Event::Empty(ref mut e)))=>{
                //     println!("Empty Event:{:?}", e.unescape_and_decode(&reader));
                // }
                // Ok((_,Event::Text(ref mut e)))=>{
                //     println!("Empty Event:{:?}", e.unescape_and_decode(&reader));
                // }
                _a => {
                    //println!("Other event:{:?} {:?}", reader.buffer_position(),
                    //a)
                }
            }
        }
        Ok(())
    }

    fn error(&self, message:String) -> ! {
        panic!("Error: {} at {}", message, self.reader.buffer_position());
    }

    fn unimplemented_owl(&self, n: &[u8]) {
        self.error(format!(
            "Ontology: Unknown element in OWL NS:{:?}",
            from_utf8(n).ok().unwrap()
        ));
    }

    fn ontology(&mut self, e: &BytesStart) {
        for res in e.attributes() {
            match res {
                Ok(attrib) => match attrib.key {
                    b"ontologyIRI" => {
                        let s = self.reader.decode(&attrib.value);
                        self.mapping.set_default(&s[..]);
                        self.ont.id.iri = Some(self.build.iri(s.into_owned()));
                    }
                    b"versionIRI" => {
                        self.ont.id.viri =
                            Some(self.build.iri(self.reader.decode(&attrib.value).into_owned()));
                    }
                    _ => (),
                },
                Err(e) => {
                    panic!(
                        "Error at position{}: {:?}",
                        self.reader.buffer_position(),
                        e
                    );
                }
            }
        }
    }

    fn ontology_annotation(&mut self) {
        let ann = self.annotation_r();
        self.ont.insert(
            OntologyAnnotation(ann)
        );
    }

    fn annotation_r(&mut self) -> Annotation {
        let mut annotation_property = None;
        let annotation;

        loop {
            let e = self.read_event();
            match e {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    match annotation_property.clone() {
                        Some(an_p) => {
                            let val =
                                AnnotationValue::from_start(self, e)
                                // TODO Remove
                                .ok().unwrap();

                            annotation = Annotation {
                                annotation_property: an_p,
                                annotation_value: val
                            };
                            return annotation;
                        },
                        None => {
                            annotation_property =
                                AnnotationProperty::from_start(self,e)
                                // TODO: Remove this with ?
                                .ok();
                        },
                    }
                },
                _=>{}
            }
        }
    }

    fn annotation_assertion(&mut self) {
        let mut annotation_property = None;
        let mut annotation_subject = None;
        let mut annotated = BTreeSet::new();

        loop {
            let e = self.read_event();
            match e {
                (ref ns, Event::Start(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" &&
                    e.local_name() == b"Annotation" =>
                {
                    annotated.insert(self.annotation_r());
                }
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    match (annotation_property.clone(),
                           annotation_subject.clone()) {
                        (Some(an_p), Some(an_s)) => {
                            let annotation_value =
                                AnnotationValue::from_start(self, e)
                                // TODO Remove
                                .ok().unwrap();

                            let assertion =
                                AnnotatedAxiom::new(
                                    AssertAnnotation {
                                        annotation_subject: an_s,
                                        annotation: Annotation {
                                            annotation_property: an_p,
                                            annotation_value: annotation_value,
                                        }
                                    },
                                    annotated.clone()
                                );

                            self.ont.insert(assertion);
                        },
                        (Some(_),None)
                            if e.local_name() == b"IRI" ||
                            e.local_name() == b"AbbreviatedIRI" => {
                                annotation_subject = Some(
                                    IRI::from_xml(self,
                                                  e.local_name()))
                                    // TODO Remove
                                    .unwrap().ok();
                        },
                        (None, None) => {
                            annotation_property =
                                AnnotationProperty::from_start(self, e)
                            //TODO Remove
                                .ok();
                        },
                        _ => {
                            self.error(format!("We panic a lot"));
                        }
                    }
                },
                (ref ns, Event::End(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                    && e.local_name() == b"AnnotationAssertion" =>
                {
                    return;
                }
                _=>{}
            }
        }
    }

    fn prefix(&mut self, e: &BytesStart) -> Result<(),Error> {
        let iri = read_iri_attr(self, e)?;
        let prefix = attrib_value(self, e, b"name")?;

        match (iri, prefix) {
            (Some(i), Some(p)) => {
                self.mapping.add_prefix(&p, &i).ok();
                return Ok(());
            }
            (None, _) => {
                return Err(error_missing_attribute("IRI", self));
            }
            (Some(_), None) => {
                return Err(error_missing_attribute("name", self));
            }
        }
    }

    fn declaration(&mut self)
        -> Result<bool,Error>
    {
        let ne = NamedEntity::from_xml(self, b"Declaration")?;
        Ok(self.ont.declare(ne))
    }

    fn transitive_object_property(&mut self) {
        let mut annotated = BTreeSet::new();
        loop {
            let mut e = self.read_event();

            match e {
                (ref ns, Event::Start(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" &&
                    e.local_name() == b"Annotation" =>
                {
                    annotated.insert(self.annotation_r());
                }
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    let op = ObjectProperty::from_start(self, e);
                    self.ont.insert(
                            AnnotatedAxiom::new(
                                TransitiveObjectProperty(
                                    // TODO Remove
                                    op.ok().unwrap()
                                ),
                                annotated.clone()
                            )
                    );
                    return;
                }
                _ => {}
            }
        }

    }


    fn object_property_r(&mut self) -> Vec<ObjectProperty>{
        let mut ops: Vec<ObjectProperty> = Vec::new();

        loop {
            let mut e = self.read_event();

            match e {
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                    && e.local_name() == b"ObjectProperty" =>
                {
                    let op = ObjectProperty::from_start(self, e)
                        // TODO Remove
                        .ok().unwrap();
                    ops.push(op);
                },
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                    && e.local_name() == b"ObjectPropertyChain" =>
                {
                    return ops;
                }
                _ => {}
            }
        }

    }

    fn sub_annotation_property(&mut self) {
        let mut objectproperty_operand: Option<AnnotationProperty> = None;

        loop {
            let mut e = self.read_event();
            match e {
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                    && e.local_name() == b"AnnotationProperty" =>
                {
                    let op = AnnotationProperty::from_start(self, e)
                        // Remove
                        .ok().unwrap();
                    match objectproperty_operand.clone() {
                        Some(superprop) => {
                            self.ont.insert(
                                    SubAnnotationProperty{
                                        super_property:
                                        superprop,
                                        sub_property:
                                        op}
                            );
                        }
                        // Add the new class as an operand
                        None => {
                            objectproperty_operand =
                                Some(op);
                        }
                    }
                }
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                        && e.local_name() == b"SubAnnotationPropertyOf" =>
                {
                    return;
                }
                _ => {}
            }
        }

    }

    fn sub_object_property(&mut self) {
        let mut objectproperty_operand: Option<ObjectPropertyExpression> = None;

        loop {
            let mut e = self.read_event();
            match e {
                (ref ns, Event::Start(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                    && e.local_name() == b"ObjectPropertyChain" => {
                        objectproperty_operand =
                            Some(
                                ObjectPropertyExpression::ObjectPropertyChain(
                                    self.object_property_r()
                                )
                            )
                    },
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                    && e.local_name() == b"ObjectProperty" =>
                {
                    let ne = self.named_entity_r(e);

                    if let NamedEntity::ObjectProperty(op) = ne {
                        match objectproperty_operand.clone() {
                            Some(superprop) => {
                                self.ont.insert(
                                    SubObjectProperty{
                                        super_property:
                                        superprop,
                                        sub_property:
                                        op}
                                );
                            }
                            // Add the new class as an operand
                            None => {
                                objectproperty_operand =
                                    Some(ObjectPropertyExpression::
                                         ObjectProperty(op));
                            }
                        }
                    }
                    else{
                        self.error(format!("{}", "Expecting object property"));
                    }
                }
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                        && e.local_name() == b"SubObjectPropertyOf" =>
                {
                    return;
                }
                _ => {}
            }
        }

    }

    fn inverse_object_property(&mut self) {
        let mut objectproperty_operands: Vec<ObjectProperty> = Vec::new();

        loop {
            let mut e = self.read_event();
            match e {
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    let ne = self.named_entity_r(e);

                    if let NamedEntity::ObjectProperty(op) = ne {
                        match objectproperty_operands.len() {
                            1 => {
                                self.ont.insert(
                                    InverseObjectProperty(
                                        objectproperty_operands.pop().unwrap(),
                                        op)
                                );
                            }
                            // Add the new class as an operand
                            0 => {
                                objectproperty_operands.push(op);
                            }
                            // Shouldn't happen
                            _ => {
                                panic!("We panic a lot!");
                            }
                        }
                    }
                    else{
                        self.error(format!("{}", "Expecting object property"));
                    }
                }
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                        && e.local_name() == b"InverseObjectProperties" =>
                {
                    return;
                }
                _ => {}
            }
        }

    }

    fn subclassof(&mut self) {
        let mut class_operands: Vec<ClassExpression> = Vec::new();

        loop {
            let mut e = self.read_event();
            match e {
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    let ce = self.class_expression_r(e);

                    match class_operands.len() {
                        1 => {
                            self.ont.insert(
                                SubClass{
                                    super_class:class_operands.pop().unwrap(),
                                    sub_class: ce
                                }
                            );
                        }
                        // Add the new class as an operand
                        0 => {
                            class_operands.push(self.class_expression_r(e));
                        }
                        // Shouldn't happen
                        _ => {
                            panic!("We panic a lot!");
                        }
                    }
                }
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                        && e.local_name() == b"SubClassOf" =>
                {
                    return;
                }
                _ => {}
            }
        }

    }

    fn equivalent_class(&mut self) {
        let mut class_operands: Vec<ClassExpression> = Vec::new();

        loop {
            let mut e = self.read_event();
            match e {
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    let ce = self.class_expression_r(e);

                    match class_operands.len() {
                        1 => {
                            self.ont.insert(
                                EquivalentClass(
                                    class_operands.pop().unwrap(),
                                    ce
                                )
                            );
                        }
                        // Add the new class as an operand
                        0 => {
                            class_operands.push(self.class_expression_r(e));
                        }
                        // Shouldn't happen
                        _ => {
                            panic!("We panic a lot!");
                        }
                    }
                }
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                        && e.local_name() == b"EquivalentClasses" =>
                {
                    return;
                }
                _ => {}
            }
        }

    }

    fn disjoint_class(&mut self) {
        let mut class_operands: Vec<ClassExpression> = Vec::new();

        loop {
            let mut e = self.read_event();
            match e {
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    let ce = self.class_expression_r(e);

                    match class_operands.len() {
                        1 => {
                            self.ont.insert(
                                DisjointClass(
                                    class_operands.pop().unwrap(),
                                    ce
                                )
                            );
                        }
                        // Add the new class as an operand
                        0 => {
                            class_operands.push(self.class_expression_r(e));
                        }
                        // Shouldn't happen
                        _ => {
                            panic!("We panic a lot!");
                        }
                    }
                }
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                        && e.local_name() == b"DisjointClasses" =>
                {
                    return;
                }
                _ => {}
            }
        }

    }

    fn class_r(&mut self, e: &BytesStart) -> Class {
        self.build.class(self.iri_attribute_r(e).unwrap())
    }

    fn class_expression_r(&mut self, e: &BytesStart) -> ClassExpression {

        match e.local_name() {
            b"Class" => {
                ClassExpression::Class(self.class_r(e))
            }
            b"ObjectSomeValuesFrom" => {
                self.object_some_values_from_p()
            }
            b"ObjectAllValuesFrom" => {
                self.object_all_values_from_p()
            }
            b"ObjectIntersectionOf" => {
                self.object_intersection_of_p()
            }
            b"ObjectUnionOf" => {
                self.object_union_of_p()
            }
            b"ObjectComplementOf" => {
                self.object_complement_of_p()
            }
            _ => {
                panic!("We panic a lot");
            }
        }
    }

    fn object_complement_of_p(&mut self)
                       -> ClassExpression {
        loop {
            let e = self.read_event();
            match e {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    return ClassExpression::Not{ce:
                                                Box::new
                                                (self.class_expression_r(e))};
                }
                _=>{}
            }
        }
    }

    fn object_union_of_p(&mut self) -> ClassExpression {
        self.object_nary_p(
            b"ObjectUnionOf",
            |o|
            ClassExpression::Or{o})
    }

    fn object_intersection_of_p(&mut self) -> ClassExpression {
        self.object_nary_p(
            b"ObjectIntersectionOf",
            |o|
            ClassExpression::And{o})
    }

    fn object_nary_p(&mut self,
                     tag: &[u8],
                     cons:fn (Vec<ClassExpression>)
                              -> ClassExpression)
                       -> ClassExpression {
        let mut operands:Vec<ClassExpression> = Vec::new();

        loop {
            let e = self.read_event();
            match e {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    let ce = self.class_expression_r(e);
                    operands.push(ce);
                }
                (ref ns, Event::End(ref e))
                    if *ns ==b"http://www.w3.org/2002/07/owl#"
                    && e.local_name() == tag
                    =>
                {
                    return cons(operands);
                }
                _=>{}
            }
        }
    }

    fn object_binary_p(&mut self,
                       cons:fn (ObjectProperty, Box<ClassExpression>)
                                -> ClassExpression)
                       -> ClassExpression {
        let mut o = None;

        loop {
            let e = self.read_event();
            match e {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    match o {
                        Some(o) => {
                            let ce = self.class_expression_r(e);
                            return cons(o, Box::new(ce));
                        }
                        None => {
                            if e.local_name() == b"ObjectProperty" {
                                let iri = self.iri_attribute_r(e).unwrap();
                                o = Some(self.build.object_property(iri))
                            }
                            else {
                                panic!("We panic a lot");
                            }
                        }
                    }
                },
                _=>{}
            }
        }
    }

    fn object_all_values_from_p(&mut self) -> ClassExpression {
        self.object_binary_p(|o, ce|
                             ClassExpression::Only{o, ce})
    }

    fn object_some_values_from_p(&mut self) -> ClassExpression {
        self.object_binary_p(|o, ce|
                             ClassExpression::Some{o, ce})
    }

    fn named_entity_r(&mut self, e: &BytesStart) -> NamedEntity {
        match self.iri_attribute_r(e) {
            Some(iri) => {
                // We already know that this is in the OWL namespace
                match e.local_name() {
                    b"Class" => {
                        NamedEntity::Class(Class(iri))
                    },
                    b"ObjectProperty" => {
                        NamedEntity::ObjectProperty(ObjectProperty(iri))
                    }
                    b"AnnotationProperty" => {
                        NamedEntity::AnnotationProperty(AnnotationProperty(iri))
                    }
                    _=> {
                        self.error(format!("We panic a lot!"));
                    }
                }
            }
            None => {
                self.error(format!("We panic a lot!"));
            }
        }
    }

    fn iri_attribute_r(&mut self, event: &BytesStart) -> Option<IRI> {
        self.iri_from_attribute_r(event, b"IRI").or_else
            (|| self.iri_from_attribute_r(event, b"abbreviatedIRI"))
    }

    fn iri_from_attribute_r(&mut self, event: &BytesStart, tag:&[u8]) -> Option<IRI> {
        for res in event.attributes() {
            match res {
                Ok(attrib) => {
                    if attrib.key == tag {
                        let expanded = self.expand_curie_maybe(&attrib.value);
                        return Some(self.build.iri(expanded));
                    }
                }
                Err(_e) => {
                }
            }
        }
        None
    }

    fn expand_curie_maybe(&self, val:&[u8]) -> String {
        let val = self.reader.decode(val);
        match self.mapping.expand_curie_string(&val) {
            // If we expand use this
            Ok(n) => n,
            // Else assume it's a complete URI
            Err(_e) => val.into_owned(),
        }
    }
}


// Rework

fn read_event<R:BufRead>(read:&mut Read<R>)
              -> (Vec<u8>, Event<'static>)
{
    let r = read.reader.read_namespaced_event(&mut read.buf,
                                              &mut read.ns_buf);
    match r {
        Ok((option_ns, event)) => {
            (option_ns.unwrap_or(b"").to_owned(),
             event.into_owned())
        }
        Err(_) => {
            // TODO Remove this, and return the error
            panic!("We panic a lot");
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

// TODO Temporary to be removed
fn error<R:BufRead>(r:&mut Read<R>, message:String) -> ! {
    panic!("Error: {} at {}", message, r.reader.buffer_position());
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
        let mut e = r.read_event();
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
                let msg = r.reader.decode(e);
                error(r,format!("Parsing of {} not implemented yet:",
                                msg))
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


from_start! {
    NamedEntity, r, e,
    {
        Ok(
            match e.local_name() {
                b"Class" => {
                    NamedEntity::Class
                        (Class::from_start(r, e)?)
                },
                b"ObjectProperty" => {
                    NamedEntity::ObjectProperty
                        (ObjectProperty::from_start(r,e)?)
                }
                b"AnnotationProperty" => {
                    NamedEntity::AnnotationProperty
                        (AnnotationProperty::from_start(r,e)?)
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
    NamedEntity,r, end,
    {
        let mut ne: Option<NamedEntity> = None;
        loop {
            let e = read_event(r);
            match e {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    ne = Some(NamedEntity::from_start(r,e)?);
                }
                (ref ns, Event::End(ref e))
                    if is_owl_name(ns, e, end) =>
                {
                    return ne.ok_or_else(
                        || error_unexpected_end_tag(end, r)
                    );
                },
                _=>{}
            }
        }
    }
}

from_xml! {IRI, r, end,
        {
            let mut iri: Option<IRI> = None;
            loop {
                let e = read_event(r);
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
        let ont_s = include_str!("../ont/one-ont.xml");
        let (_, mapping) = read_ok(&mut ont_s.as_bytes());

        let hash_map: HashMap<&String, &String> = mapping.mappings().collect();
        assert_eq!(6, hash_map.len());
    }

    #[test]
    fn test_simple_ontology() {
        let ont_s = include_str!("../ont/one-ont.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(*ont.id.iri.unwrap(), "http://example.com/iri");
    }

    #[test]
    fn test_simple_ontology_rendered_by_horned() {
        let ont_s = include_str!("../ont/one-ont-from-horned.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(*ont.id.iri.unwrap(), "http://example.com/iri");
    }

    #[test]
    fn test_one_class() {
        let ont_s = include_str!("../ont/one-class.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_class().count(), 1);
        assert_eq!(
            String::from(&ont.declare_class().next().unwrap().0),
            "http://example.com/iri#C"
        );
    }

    #[test]
    fn test_one_class_fqn() {
        let ont_s = include_str!("../ont/one-class-fully-qualified.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_class().count(), 1);
        assert_eq!(
            String::from(&ont.declare_class().next().unwrap().0),
            "http://www.russet.org.uk/#C"
        );
    }

    #[test]
    fn test_ten_class() {
        let ont_s = include_str!("../ont/o10.owl");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_class().count(), 10);
    }

    #[test]
    fn test_one_property() {
        let ont_s = include_str!("../ont/one-oproperty.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_object_property().count(), 1);
    }

    #[test]
    fn test_one_subclass() {
        let ont_s = include_str!("../ont/one-subclass.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
    }

    #[test]
    fn test_one_some() {
        let ont_s = include_str!("../ont/one-some.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
        assert_eq!(ont.declare_object_property().count(), 1);
    }

    #[test]
    fn test_one_only() {
        let ont_s = include_str!("../ont/one-only.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
        assert_eq!(ont.declare_class().count(), 2);
        assert_eq!(ont.declare_object_property().count(), 1);
    }

    #[test]
    fn test_one_and() {
        let ont_s = include_str!("../ont/one-and.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
    }

    #[test]
    fn test_one_or() {
        let ont_s = include_str!("../ont/one-or.xml");
        let (ont,_ ) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
    }

    #[test]
    fn test_one_not() {
        let ont_s = include_str!("../ont/one-not.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_class().count(), 1);
    }

    #[test]
    fn test_one_annotation_property() {
        let ont_s = include_str!("../ont/one-annotation-property.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        assert_eq!(ont.declare_annotation_property().count(), 1);
    }

    #[test]
    fn test_one_annotation() {
        let ont_s = include_str!("../ont/one-annotation.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());
        assert_eq!(ont.declare_annotation_property().count(), 1);
        assert_eq!(ont.assert_annotation().count(), 1);
    }

    #[test]
    fn test_one_label_non_abbreviated() {
        let ont_s = include_str!("../ont/one-label-non-abbreviated-iri.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.assert_annotation().count(), 1);
    }


    #[test]
    fn test_one_label() {
        let ont_s = include_str!("../ont/one-label.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.assert_annotation().count(), 1);
    }

    #[test]
    fn test_one_ontology_annotation() {
        let ont_s = include_str!("../ont/one-ontology-annotation.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.ontology_annotation().count(), 1);
    }

    #[test]
    fn test_one_equivalent_class() {
        let ont_s = include_str!("../ont/one-equivalent.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.equivalent_class().count(), 1);
    }

    #[test]
    fn test_one_disjoint_class() {
        let ont_s = include_str!("../ont/one-disjoint.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.disjoint_class().count(), 1);
    }

    #[test]
    fn test_one_sub_property() {
        let ont_s = include_str!("../ont/one-suboproperty.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_object_property().count(), 1);
    }

    #[test]
    fn test_one_inverse_property() {
        let ont_s = include_str!("../ont/inverse-properties.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.inverse_object_property().count(), 1);
    }

    #[test]
    fn test_one_transitive_property() {
        let ont_s = include_str!("../ont/transitive-properties.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.transitive_object_property().count(), 1);
    }

    #[test]
    fn test_subproperty_chain() {
        let ont_s = include_str!("../ont/subproperty-chain.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_object_property().count(), 1);
    }

    #[test]
    fn test_annotation_on_annotation() {
        let ont_s = include_str!("../ont/annotation-with-annotation.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());


        let mut ann_i = ont.annotated_axiom(AxiomKind::AssertAnnotation);
        let ann:&AnnotatedAxiom = ann_i.next().unwrap();
        assert_eq!(ann.annotation.len(), 1);
    }

    #[test]
    fn annotated_transitive() {
        let ont_s = include_str!("../ont/annotation-on-transitive.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        let annotated_axiom = ont.annotated_axiom
            (AxiomKind::TransitiveObjectProperty).next().unwrap();
        assert_eq!(annotated_axiom.annotation.len(), 1);
    }

    #[test]
    fn test_sub_annotation() {
        let ont_s = include_str!("../ont/sub-annotation.xml");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.sub_annotation_property().count(), 1);
    }
}
