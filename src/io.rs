extern crate xml;

use std::io::BufRead;

use self::xml::EventReader;
use self::xml::reader::XmlEvent;
use self::xml::attribute::OwnedAttribute;

use super::*;

const OWL_NS:&'static str = "http://www.w3.org/2002/07/owl#";

pub fn read <R: BufRead>(buf: &mut R) -> Ontology {

    let parser = EventReader::new(buf);
    let mut ont = Ontology::new();

    for e in parser {
         match e {
             Ok(XmlEvent::StartElement {name, attributes, .. }) =>{
                 match &name.namespace{
                     &Some(ref f) if f == OWL_NS => {
                         match &name.local_name[..] {
                             "Ontology" => handle_ontology(&mut ont, attributes),
                             "Class" => handle_class(&mut ont, attributes),
                             _=> println!("Unknown Element in OWL NS:{}", name),
                         }
                     }
                     &Some(_) =>{
                         println!("Unknown Element:{}", name)
                     }
                     &None => {
                         println!("Unknown Element without ns:{}", name);
                     }
                 }
             }
             Err(e) => {
                 println!("Error: {}", e);
             }
             _ => {

             }
         }
    }

    ont
}

fn handle_ontology(ont:&mut Ontology, attributes:Vec<OwnedAttribute>){
    for attrib in attributes{
        match &attrib.name.local_name[..]{
            "ontologyIRI" =>
                ont.id.iri = Some(ont.iri(attrib.value)),
            "versionIRI" =>
                ont.id.viri = Some(ont.iri(attrib.value)),
            _ => ()
        }
    }
}

#[test]
fn test_simple_ontology(){
    let ont_s = include_str!("ont/simple-ont.xml");
    let ont = read(&mut ont_s.as_bytes());

    println!("IRI is {:?}", &ont.id.iri.unwrap());
}

fn handle_class(ont:&mut Ontology, attributes: Vec<OwnedAttribute>){
    for attrib in attributes{
        match &attrib.name.local_name[..]{
            "IRI" =>{
                let iri = ont.iri(attrib.value);
                ont.class(iri);
            }
            _ => ()
        }
    }
}
