use curie::PrefixMapping;

use std::io::BufRead;
use std::str::from_utf8;

use quick_xml::reader::Reader;
use quick_xml::events::Event;
use quick_xml::events::BytesStart;

use model::*;

#[derive(Copy,Clone)]
enum State{
    Top, Ontology, Declaration
}

pub fn read <R: BufRead>(bufread: &mut R) -> (Ontology,PrefixMapping)
{

    let mut reader:Reader<&mut R> = Reader::from_reader(bufread);

    let mut ont = Ontology::new();
    let mut mapping = PrefixMapping::default();

    let mut state = State::Top;
    let mut buf = Vec::new();
    let mut ns_buf = Vec::new();

    let mut closing_tag:&[u8] = b"";
    let mut closing_state = State::Top;

    loop{
        match reader.read_namespaced_event(&mut buf, &mut ns_buf) {
            Ok((ref ns, Event::Start(ref mut e)))
                if *ns == Some(b"http://www.w3.org/2002/07/owl#")
                =>
            {
                match (&state, e.local_name()){
                    (&State::Top, b"Ontology") => {
                        ontology_attributes(&mut ont,&mut mapping,
                                            &mut reader, e);
                        state = State::Ontology;
                    }
                    (&State::Ontology, b"Declaration") => {
                        state = State::Declaration;
                        closing_tag=b"Declaration";
                        closing_state = State::Ontology;
                     }
                    (_,n) => {
                        unimplemented_owl(n);
                    }
                }
            }
            Ok((ref ns,Event::Empty(ref mut e)))
                if *ns == Some(b"http://www.w3.org/2002/07/owl#")
                =>
            {
                match(&state, e.local_name()){
                    (&State::Ontology, b"Prefix") => {
                        prefix_attributes(&mut mapping, &mut reader, e);
                    }
                    (&State::Declaration, b"Class") => {
                        add_class(&mut ont, &mut mapping, &mut reader, e);
                    }
                    (_,n) =>{
                        unimplemented_owl(n);
                    }
                }
            }
            Ok((ref ns,Event::End(ref mut e)))
                if *ns == Some(b"http://www.w3.org/2002/07/owl#") &&
                e.local_name() == closing_tag
                =>
            {
                state=closing_state;
            }

            Err(e) => {
                println!("Error: {}", e);
            },
            Ok((_, Event::Eof)) =>
            {
                break;
            },
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

    (ont,mapping)
}

fn unimplemented_owl(n:&[u8]){
    println!("Ontology: Unknown element in OWL NS:{:?}",
             from_utf8(n).ok().unwrap());
}

fn ontology_attributes<R: BufRead>(ont:&mut Ontology, mapping:&mut PrefixMapping,
                                reader:&mut Reader<R>, e: &BytesStart){
    for res in e.attributes(){
        match res{
            Ok(attrib) => {
                match attrib.key{
                    b"ontologyIRI" => {
                        let s = reader.decode(&attrib.value);
                        mapping.set_default(&s[..]);
                        ont.id.iri = Some(ont.iri(s.into_owned()));
                    },
                    b"versionIRI" => {
                        ont.id.viri = Some(ont.iri
                                           (reader.decode(&attrib.value)
                                            .into_owned()));
                    },
                    _ => ()
                }
            },
            Err(e) => {
                panic!( "Error at position{}: {:?}",
                         reader.buffer_position(),
                         e);
            }
        }
    }
}


fn prefix_attributes<R: BufRead>(mapping:&mut PrefixMapping,
                                 reader:&mut Reader<R>, e: &BytesStart)
{
    let mut prefix=None;
    let mut iri=None;

    for res in e.attributes() {
        match res{
            Ok(attrib) => {
                match attrib.key {
                    b"IRI" => {
                        iri=Some(reader.decode(&attrib.value).into_owned());
                    }
                    b"name" => {
                        prefix=Some(reader.decode(&attrib.value).into_owned());
                    }
                    _=>{}
                }
            },
            Err(e) =>{
                panic!("Error at position{}: {:?}",
                       reader.buffer_position(),
                       e);
            }
        }
    }


    match (iri, prefix) {
        (Some(i), Some(p)) => {
            mapping.add_prefix(&p,&i).ok();
        }
        _=> {
            println!("Incomplete prefix element");
        }
    }
}

#[cfg(test)]
mod test{
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn test_simple_ontology_prefix(){
        let ont_s = include_str!("../ont/one-ont.xml");
        let (_,mapping) = read(&mut ont_s.as_bytes());

        let hash_map: HashMap<&String,&String> = mapping.mappings().collect();
        assert_eq!(6, hash_map.len());
    }
}

#[test]
fn test_simple_ontology(){
    let ont_s = include_str!("../ont/one-ont.xml");
    let (ont,_) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.iri_to_str(ont.id.iri.unwrap()).unwrap(),
               "http://example.com/iri");
}

fn add_class<R: BufRead>(ont:&mut Ontology, mapping: &PrefixMapping,
                         reader:&mut Reader<R>,event:&BytesStart)
             -> Option<Class>
{
    for res in event.attributes(){
        match res {
            Ok(attrib) => {
                match attrib.key{
                    b"IRI" =>{
                        let val = reader.decode(&attrib.value);
                        let expanded =
                            match mapping.expand_curie_string(&val){
                                // If we expand use this
                                Ok(n) => n,
                                // Else assume it's a complete URI
                                Err(_e) => val.into_owned(),
                            };

                        let iri = ont.iri(expanded);
                        return Some(ont.class(iri));
                    },
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

#[test]
fn test_one_class(){
    let ont_s = include_str!("../ont/one-class.xml");
    let (ont,_) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 1);
    assert_eq!(
        ont.iri_to_str(ont.class.iter().next().unwrap().0).unwrap(),
        "http://example.com/iri#C");
}

#[test]
fn test_one_class_fqn(){
    let ont_s = include_str!("../ont/one-class-fully-qualified.xml");
    let (ont,_) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 1);
    assert_eq!(
        ont.iri_to_str(ont.class.iter().next().unwrap().0).unwrap(),
        "http://www.russet.org.uk/#C");
}

#[test]
fn test_ten_class(){
    let ont_s = include_str!("../ont/o10.owl");
    let (ont,_) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 10);
}
