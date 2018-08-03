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

struct Read<R>
where
    R: BufRead,
{
    ont: Ontology,
    mapping: PrefixMapping,
    reader: Reader<R>,
    buf:Vec<u8>,
    ns_buf:Vec<u8>
}

pub fn read<R: BufRead>(bufread: &mut R) -> (Ontology, PrefixMapping) {
    read_with_build(bufread, IRIBuild::new())
}

pub fn read_with_build<R: BufRead>(bufread: &mut R, build: IRIBuild) -> (Ontology, PrefixMapping) {
    let reader: Reader<&mut R> = Reader::from_reader(bufread);
    let ont = Ontology::new_with_build(build);
    let mapping = PrefixMapping::default();

    let mut read = Read::new(reader, ont, mapping);
    read.parse();

    (read.ont, read.mapping)
}

impl<R: BufRead> Read<R> {
    fn new(reader: Reader<R>, ont: Ontology, mapping: PrefixMapping) -> Read<R> {
        Read {
            reader: reader,
            ont: ont,
            mapping: mapping,
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

    fn unimplemented_owl(&self, n: &[u8]) {
        println!(
            "Ontology: Unknown element in OWL NS:{:?}",
            from_utf8(n).ok().unwrap()
        );
    }

    fn ontology(&mut self, e: &BytesStart) {
        for res in e.attributes() {
            match res {
                Ok(attrib) => match attrib.key {
                    b"ontologyIRI" => {
                        let s = self.reader.decode(&attrib.value);
                        self.mapping.set_default(&s[..]);
                        self.ont.id.iri = Some(self.ont.iri(s.into_owned()));
                    }
                    b"versionIRI" => {
                        self.ont.id.viri =
                            Some(self.ont.iri(self.reader.decode(&attrib.value).into_owned()));
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
                    self.ont.named_entity(ne);
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

    fn class_r(&mut self, e: &BytesStart) -> Class {
        Class(self.iri_r(e).unwrap())
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
                     cons:fn (Vec<Box<ClassExpression>>)
                              -> ClassExpression)
                       -> ClassExpression {
        let mut operands:Vec<Box<ClassExpression>> = Vec::new();

        loop {
            let e = self.read_event();
            match e {
                (ref ns, Event::Start(ref e))
                    |
                (ref ns, Event::Empty(ref e))
                    if *ns == b"http://www.w3.org/2002/07/owl#" =>
                {
                    let ce = self.class_expression_r(e);
                    operands.push(Box::new(ce));
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
                                let iri = self.iri_r(e).unwrap();
                                o = Some(ObjectProperty(iri))
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
        let iri = self.iri_r(e).unwrap();
        // We already know that this is in the OWL namespace
        match e.local_name() {
            b"Class" => {
                NamedEntity::Class(Class(iri))
            },
            b"ObjectProperty" => {
                NamedEntity::ObjectProperty(ObjectProperty(iri))
            }
            _ => {
                panic!("We panic a lot");
            }
        }
    }

    fn iri_r(&mut self, event: &BytesStart) -> Option<IRI> {
        for res in event.attributes() {
            match res {
                Ok(attrib) => {
                    match attrib.key {
                        b"IRI" => {
                            let val = self.reader.decode(&attrib.value);
                            let expanded = match self.mapping.expand_curie_string(&val) {
                                // If we expand use this
                                Ok(n) => n,
                                // Else assume it's a complete URI
                                Err(_e) => val.into_owned(),
                            };

                            return Some(self.ont.iri(expanded));
                        }
                        _ => {}
                    }
                }
                Err(_e) => {
                    panic!("We panic a lot");
                }
            }
        }
        None
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

    assert_eq!(ont.class.len(), 1);
    assert_eq!(
        *ont.class.iter().next().unwrap().0,
        "http://example.com/iri#C"
    );
}

#[test]
fn test_one_class_fqn() {
    let ont_s = include_str!("../ont/one-class-fully-qualified.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 1);
    assert_eq!(
        *ont.class.iter().next().unwrap().0,
        "http://www.russet.org.uk/#C"
    );
}

#[test]
fn test_ten_class() {
    let ont_s = include_str!("../ont/o10.owl");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 10);
}

#[test]
fn test_one_property() {
    let ont_s = include_str!("../ont/one-oproperty.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.object_property.len(), 1);
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

}

#[test]
fn test_one_only() {
    let ont_s = include_str!("../ont/one-only.xml");
    let (ont, _) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.subclass.len(), 1);
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
