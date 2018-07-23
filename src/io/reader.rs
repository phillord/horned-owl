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
    Declaration,
    SubClassOf,
}

struct Read<R>
where
    R: BufRead,
{
    ont: Ontology,
    mapping: PrefixMapping,
    reader: Reader<R>,
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
        }
    }

    fn parse(&mut self) {
        let mut state = State::Top;
        let mut buf = Vec::new();
        let mut ns_buf = Vec::new();

        let mut closing_tag: &[u8] = b"";
        let mut closing_state = State::Top;

        let mut class_operands: Vec<ClassExpression> = Vec::new();

        loop {
            let mut e = self.reader.read_namespaced_event(&mut buf, &mut ns_buf);
            //println!("r:{:?}", r);

            match e {
                Ok((ref ns, Event::Start(ref mut e)))
                    if *ns == Some(b"http://www.w3.org/2002/07/owl#") =>
                {
                    match (&state, e.local_name()) {
                        (&State::Top, b"Ontology") => {
                            self.ontology(e);
                            state = State::Ontology;
                        }
                        (&State::Ontology, b"Declaration") => {
                            state = State::Declaration;
                            closing_tag = b"Declaration";
                            closing_state = State::Ontology;
                        }
                        (&State::Ontology, b"SubClassOf") => {
                            state = State::SubClassOf;
                        }
                        (_, n) => {
                            self.unimplemented_owl(n);
                        }
                    }
                }
                Ok((ref ns, Event::Empty(ref mut e)))
                    if *ns == Some(b"http://www.w3.org/2002/07/owl#") =>
                {
                    match (&state, e.local_name()) {
                        (&State::Ontology, b"Prefix") => {
                            self.prefix(e);
                        }
                        (&State::Declaration, b"Class") => {
                            let iri = self.iri(e);
                            self.ont.class_from_iri(iri.unwrap());
                        }
                        (&State::SubClassOf, b"Class") => {
                            match class_operands.len() {
                                // Take off the last class and add it
                                1 => {
                                    let iri = self.iri(e);
                                    let sub = self.ont.class_from_iri(iri.unwrap());

                                    self.ont.subclass_exp(
                                        class_operands.pop().unwrap(),
                                        ClassExpression::Class(sub),
                                    );
                                    class_operands.clear();
                                }
                                // Add the new class as an operand
                                0 => {
                                    let iri = self.iri(e);

                                    class_operands.push(ClassExpression::Class(
                                        self.ont.class_from_iri(iri.unwrap()),
                                    ));
                                }
                                // Shouldn't happen
                                _ => {
                                    panic!("We panic a lot!");
                                }
                            }
                        }
                        (&State::Declaration, b"ObjectProperty") => {
                            let iri = self.iri(e);
                            self.ont.object_property_from_iri(iri.unwrap());
                        }
                        (_, n) => {
                            self.unimplemented_owl(n);
                        }
                    }
                }
                Ok((ref ns, Event::End(ref mut e)))
                    if *ns == Some(b"http://www.w3.org/2002/07/owl#")
                        && e.local_name() == closing_tag =>
                {
                    state = closing_state;
                }

                Err(e) => {
                    println!("Error: {}", e);
                }
                Ok((_, Event::Eof)) => {
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

    fn iri(&mut self, event: &BytesStart) -> Option<IRI> {
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
