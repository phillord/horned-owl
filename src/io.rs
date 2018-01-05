use curie::PrefixMapping;

use std::io::BufRead;
use std::io::Read;

use xml::EventReader;
use xml::reader::{Events,XmlEvent};
use xml::name::OwnedName;
use xml::attribute::OwnedAttribute;

use model::*;

const OWL_NS:&'static str = "http://www.w3.org/2002/07/owl#";

pub fn read <R: BufRead>(buf: &mut R) -> Ontology {

    let parser = EventReader::new(buf);
    let mut ont = Ontology::new();
    let mut mapping = PrefixMapping::default();

    top(&mut ont, &mut mapping, &mut parser.into_iter());
    ont
}

// Start parser closure needs to take everything as parameters since
// captures breaks borrowing
fn start_parser<F,R>(iterator:&mut Events<R>, level: &str, mut callback: F)
    where F: FnMut(&mut Events<R>, &OwnedName, &Vec<OwnedAttribute>), R: Read
{
    loop{
        match iterator.next(){
            Some(e) => {
                match e {
                    Ok(XmlEvent::StartElement {name, attributes, .. }) =>{
                        match &name.namespace{
                            &Some(ref f) if f == OWL_NS =>
                            {
                                callback(iterator, &name, &attributes);
                            }
                            &Some(_) =>{
                                println!("{}: Unknown Element:{}", level, name)
                            }
                            &None => {
                                println!("{}: Unknown Element without ns:{}", level, name);
                            }
                        }
                    }
                    Err(e) => {
                        println!("Error: {}", e);
                    }
                    _ => {
                        // Ignore non starts
                    }
                }
            },
            None => {
                break;
            }
        }
    }
}


fn top<R: Read>(ont:&mut Ontology, mapping:&mut PrefixMapping,
                iterator:&mut Events<R>){
    start_parser(iterator, "Top Level",
                 |iterator, name, attributes|{
                     match &name.local_name[..] {
                         "Ontology" => {
                             ontology_attributes(ont,mapping,attributes);
                             ontology(ont,mapping,iterator);
                         }
                         _ => {
                             println!("Top Level: Unknown Element in OWL NS:{}",
                                      name);
                         }
                     }
                 });
}

fn ontology_attributes(ont:&mut Ontology, mapping:&mut PrefixMapping,
                       attributes:&Vec<OwnedAttribute>){
    for attrib in attributes{
        match &attrib.name.local_name[..]{
            "ontologyIRI" =>{
                mapping.set_default(&attrib.value[..]);
                ont.id.iri = Some(ont.iri(attrib.value.clone()));
            }
            ,
            "versionIRI" =>
                ont.id.viri = Some(ont.iri(attrib.value.clone())),
            _ => ()
        }
    }
}

fn ontology<R: Read>(ont:&mut Ontology, mapping:&mut PrefixMapping,
            iterator:&mut Events<R>){
    start_parser(iterator, "Ontology",
                 |iterator, name, _attributes|{
                     match &name.local_name[..] {
                         "Declaration" => {
                             declaration(ont,mapping,iterator);
                         }
                         _ => {
                             println!("Ontology: Unknown Element in OWL NS:{}",
                                      name);
                         }
                     }
                 });
}

fn declaration<R: Read>(ont:&mut Ontology, mapping:&mut PrefixMapping,
                  iterator:&mut Events<R>){
    start_parser(iterator, "Declaration",
                 |_iterator, name, attributes|{
                     match &name.local_name[..] {
                         "Class" => {
                             add_class(ont,mapping,attributes);
                         }
                         _ => {
                             println!("Declaration: Unknown Element in OWL NS:{}",
                                      name);
                         }
                     }
                 });
}


#[test]
fn test_simple_ontology(){
    let ont_s = include_str!("ont/one-ont.xml");
    let ont = read(&mut ont_s.as_bytes());

    assert_eq!(ont.iri_to_str(ont.id.iri.unwrap()).unwrap(),
               "http://example.com/iri");
}

fn add_class(ont:&mut Ontology, mapping: &PrefixMapping,
             attributes: &Vec<OwnedAttribute>)
             -> Option<Class>
{
    for attrib in attributes{
        match &attrib.name.local_name[..]{
            "IRI" =>{
                let expanded =
                    match
                    mapping.expand_curie_string(&attrib.value[..]){
                        // If we expand use this
                        Ok(n) => n,
                        // Else assume it's a complete URI
                        Err(_e) => attrib.value.clone()
                    };

                let iri = ont.iri(expanded);
                return Some(ont.class(iri));
            },
            _ => ()
        }
    }
    None
}

#[test]
fn test_one_class(){
    let ont_s = include_str!("ont/one-class.xml");
    let ont = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 1);
    assert_eq!(
        ont.iri_to_str(ont.class.iter().next().unwrap().0).unwrap(),
        "http://example.com/iri#C");
}

#[test]
fn test_one_class_fqn(){
    let ont_s = include_str!("ont/one-class-fully-qualified.xml");
    let ont = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 1);
    assert_eq!(
        ont.iri_to_str(ont.class.iter().next().unwrap().0).unwrap(),
        "http://www.russet.org.uk/#C");
}
