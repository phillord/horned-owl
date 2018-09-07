use curie::PrefixMapping;

use std::io::BufRead;
use std::str::from_utf8;

use quick_xml::events::BytesStart;
use quick_xml::events::Event;
use quick_xml::Reader;

use model::*;

#[derive(Copy, Clone)]
enum State {
    Top,
    Ontology,
}

struct Read<'a, R>
where
    R: BufRead,
{
    ont: Ontology,
    build: &'a Build,
    mapping: PrefixMapping,
    reader: Reader<R>,
    buf:Vec<u8>,
    ns_buf:Vec<u8>
}

pub fn read<R: BufRead>(bufread: &mut R) -> (Ontology, PrefixMapping) {
    let b = Build::new();
    read_with_build(bufread, &b)
}

pub fn read_with_build<R: BufRead>(bufread: &mut R, build: &Build) -> (Ontology, PrefixMapping) {
    let reader: Reader<&mut R> = Reader::from_reader(bufread);
    let ont = Ontology::new();
    let mapping = PrefixMapping::default();

    let mut read = Read::new(reader, ont, mapping, build);
    read.parse();

    (read.ont, read.mapping)
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

    fn parse(&mut self) {
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
                            self.declaration();
                        }
                        (&State::Ontology, b"SubClassOf") => {
                            self.subclassof();
                        }
                        (&State::Ontology, b"Prefix") => {
                            self.prefix(e);
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
        self.ont.annotation.insert(ann);
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
                            let val = self.annotation_value_r(e);
                            annotation = Annotation {
                                annotation_property: an_p,
                                annotation_value: val
                            };
                            return annotation;
                        },
                        None => {
                            match self.named_entity_r(e) {
                                NamedEntity::AnnotationProperty(an_p) => {
                                    annotation_property=Some(an_p);
                                }
                                _=> {
                                    self.error(format!("We panic a lot"));
                                }
                            }
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
        let mut annotated = None;

        loop {
            let e = self.read_event();
            match e {
                (ref ns, Event::Start(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" &&
                    e.local_name() == b"Annotation" =>
                {
                    if let None = annotated {
                        annotated = Some(vec![]);
                    }
                    if let Some(ref mut v) = annotated {
                        let ann = self.annotation_r();
                        v.push(ann);
                    }
                }
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    match (annotation_property.clone(),
                           annotation_subject.clone()) {
                        (Some(an_p), Some(an_s)) => {
                            let annotation_value = self.annotation_value_r(e);
                            let assertion =
                                AnnotationAssertion {
                                    annotation_subject: an_s,
                                    annotation: Annotation {
                                        annotation_property: an_p,
                                        annotation_value: annotation_value,
                                    },
                                    annotated: annotated.clone()
                                };
                            self.ont.annotation_assertion(assertion);
                        },
                        (Some(_),None)
                            if e.local_name() == b"IRI" ||
                            e.local_name() == b"AbbreviatedIRI" => {
                            annotation_subject = Some(self.iri_r());
                        },
                        (None, None) => {
                            match self.named_entity_r(e) {
                                NamedEntity::AnnotationProperty(an_p) => {
                                    annotation_property=Some(an_p);
                                }
                                _=> {
                                    panic!("We panic a lot");
                                }
                            }
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

    fn prefix(&mut self, e: &BytesStart) {
        let mut prefix = None;
        let mut iri = None;

        for res in e.attributes() {
            match res {
                Ok(attrib) => match attrib.key {
                    b"IRI" => {
                        iri = Some(self.reader.decode(&attrib.value).into_owned());
                    }
                    b"name" => {
                        prefix = Some(self.reader.decode(&attrib.value).into_owned());
                    }
                    _ => {}
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

        match (iri, prefix) {
            (Some(i), Some(p)) => {
                self.mapping.add_prefix(&p, &i).ok();
            }
            _ => {
                println!("Incomplete prefix element");
            }
        }
    }

    fn declaration(&mut self) {
        loop {
            let mut e = self.read_event();

            match e {
                (ref ns, Event::Start(ref mut e))
                    |
                (ref ns, Event::Empty(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    let ne = self.named_entity_r(e);
                    self.ont.declare(ne);
                }
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                        && e.local_name() == b"Declaration" =>
                {
                    return;
                }
                _ => {}
            }
        }

    }

    fn transitive_object_property(&mut self) {
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
                        self.ont.transitive_object_property.insert(
                            TransitiveObjectProperty(op)
                        );
                        return;
                    }
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
                    let ne = self.named_entity_r(e);
                    if let NamedEntity::ObjectProperty(op) = ne {
                        ops.push(op);
                    }
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
                    let ne = self.named_entity_r(e);

                    if let NamedEntity::AnnotationProperty(op) = ne {
                        match objectproperty_operand.clone() {
                            Some(superprop) => {
                                self.ont.sub_annotation_property.insert(
                                    SubAnnotationProperty{
                                        superproperty:
                                        superprop,
                                        subproperty:
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
                    else{
                        self.error(format!("{}", "Expecting object property"));
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
                                self.ont.sub_object_property.insert(
                                    SubObjectProperty{
                                        superproperty:
                                        superprop,
                                        subproperty:
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
                                self.ont.inverse_object_property.insert(
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
                            self.ont.subclass_exp(
                                class_operands.pop().unwrap(),
                                ce
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
                            self.ont.equivalent_class.insert(
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
                            self.ont.disjoint_class.insert(
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

    fn annotation_value_r(&mut self, e: &BytesStart) -> AnnotationValue {
        match e.local_name() {
            b"Literal" => {
                self.literal_r(e)
            },
            b"AbbreviatedIRI" | b"IRI" => {
                let iri = self.iri_r();
                AnnotationValue::IRI(iri)
            }
            _ => {
                self.error(
                    format!("Parsing of {} not implemented yet:",
                            self.reader.decode(e)));
            }
        }
    }

    fn literal_r(&mut self, e: &BytesStart) -> AnnotationValue {
        let datatype_iri = self.iri_from_attribute_r(e, b"datatypeIRI");
        let lang = self.attrib_value(e, b"xml:lang");

        let mut literal:Option<String> = None;

        loop {
            let mut e = self.read_event();
            match e {
                (_, Event::Text(ref e)) =>
                {
                    literal = Some(self.reader.decode(e).into_owned());
                }
                (ref ns, Event::End(ref mut e))
                    if *ns == b"http://www.w3.org/2002/07/owl#"
                    && e.local_name() == b"Literal" =>
                {
                    return AnnotationValue::PlainLiteral
                    {datatype_iri: datatype_iri,
                     lang: lang,
                     literal: literal};
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

    fn iri_r(&mut self) -> IRI {
        loop {
            let e = self.read_event();
            match e {
                (ref _ns,Event::Text(ref e)) => {
                    let iri_s =
                        self.expand_curie_maybe(e);
                    return self.build.iri(iri_s);
                },
                (ref ns, Event::End(ref e))
                    if *ns ==b"http://www.w3.org/2002/07/owl#"
                    && (e.local_name() == b"IRI"
                        ||
                        e.local_name() == b"AbbreviatedIRI"
                    ) =>
                {
                    panic!("We panic a lot");
                },
                _=>{}
            }
        }
    }

    fn attrib_value(&mut self, event: &BytesStart, tag:&[u8]) -> Option<String> {
        for res in event.attributes() {
            match res {
                Ok(attrib) => {
                    if attrib.key == tag {
                        return Some(self.reader.decode
                                    (&attrib.value).into_owned());
                    }
                }
                Err(_e) => {
                    panic!("We panic a lot");
                }
            }
        }
        None
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

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_simple_ontology_prefix() {
        let ont_s = include_str!("../ont/one-ont.xml");
        let (_, mapping) = read(&mut ont_s.as_bytes());

        let hash_map: HashMap<&String, &String> = mapping.mappings().collect();
        assert_eq!(6, hash_map.len());
    }
}

#[test]
fn test_simple_ontology() {
    let ont_s = include_str!("../ont/one-ont.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(*ont.id.iri.unwrap(), "http://example.com/iri");
}

#[test]
fn test_simple_ontology_rendered_by_horned() {
    let ont_s = include_str!("../ont/one-ont-from-horned.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(*ont.id.iri.unwrap(), "http://example.com/iri");
}

#[test]
fn test_one_class() {
    let ont_s = include_str!("../ont/one-class.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.declare_class().count(), 1);
    assert_eq!(
        String::from(&ont.declare_class().next().unwrap().0),
        "http://example.com/iri#C"
    );
}

#[test]
fn test_one_class_fqn() {
    let ont_s = include_str!("../ont/one-class-fully-qualified.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.declare_class().count(), 1);
    assert_eq!(
        String::from(&ont.declare_class().next().unwrap().0),
        "http://www.russet.org.uk/#C"
    );
}

#[test]
fn test_ten_class() {
    let ont_s = include_str!("../ont/o10.owl");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.declare_class().count(), 10);
}

#[test]
fn test_one_property() {
    let ont_s = include_str!("../ont/one-oproperty.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.declare_object_property().count(), 1);
}

#[test]
fn test_one_subclass() {
    let ont_s = include_str!("../ont/one-subclass.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.subclass.len(), 1);
}

#[test]
fn test_one_some() {
    let ont_s = include_str!("../ont/one-some.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.subclass.len(), 1);
    assert_eq!(ont.declare_object_property().count(), 1);
}

#[test]
fn test_one_only() {
    let ont_s = include_str!("../ont/one-only.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.subclass.len(), 1);
    assert_eq!(ont.declare_class().count(), 2);
    assert_eq!(ont.declare_object_property().count(), 1);
}

#[test]
fn test_one_and() {
    let ont_s = include_str!("../ont/one-and.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.subclass.len(), 1);
}

#[test]
fn test_one_or() {
    let ont_s = include_str!("../ont/one-or.xml");
    let (ont,_ ) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.subclass.len(), 1);
}

#[test]
fn test_one_not() {
    let ont_s = include_str!("../ont/one-not.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.subclass.len(), 1);
}

#[test]
fn test_one_annotation_property() {
    let ont_s = include_str!("../ont/one-annotation-property.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());
    assert_eq!(ont.declare_annotation_property().count(), 1);
}

#[test]
fn test_one_annotation() {
    let ont_s = include_str!("../ont/one-annotation.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());
    assert_eq!(ont.declare_annotation_property().count(), 1);
    assert_eq!(ont.annotation_assertion.len(), 1);
}

#[test]
fn test_one_label_non_abbreviated() {
    let ont_s = include_str!("../ont/one-label-non-abbreviated-iri.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.annotation_assertion.len(), 1);
}


#[test]
fn test_one_label() {
    let ont_s = include_str!("../ont/one-label.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.annotation_assertion.len(), 1);
}

#[test]
fn test_one_ontology_annotation() {
    let ont_s = include_str!("../ont/one-ontology-annotation.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.annotation.len(), 1);
}

#[test]
fn test_one_equivalent_class() {
    let ont_s = include_str!("../ont/one-equivalent.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.equivalent_class.len(), 1);
}

#[test]
fn test_one_disjoint_class() {
    let ont_s = include_str!("../ont/one-disjoint.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.disjoint_class.len(), 1);
}

#[test]
fn test_one_sub_property() {
    let ont_s = include_str!("../ont/one-suboproperty.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.sub_object_property.len(), 1);
}

#[test]
fn test_one_inverse_property() {
    let ont_s = include_str!("../ont/inverse-properties.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.inverse_object_property.len(), 1);
}

#[test]
fn test_one_transitive_property() {
    let ont_s = include_str!("../ont/transitive-properties.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.transitive_object_property.len(), 1);
}

#[test]
fn test_subproperty_chain() {
    let ont_s = include_str!("../ont/subproperty-chain.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.sub_object_property.len(), 1);
}

#[test]
fn test_annotation_on_annotation() {
    let ont_s = include_str!("../ont/annotation-with-annotation.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());


    let mut ann_i = ont.annotation_assertion.iter();
    let ann:&AnnotationAssertion = ann_i.next().unwrap();
    assert!(ann.annotated.is_some());
}

#[test]
fn test_sub_annotation() {
    let ont_s = include_str!("../ont/sub-annotation.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.sub_annotation_property.len(), 1);
}
