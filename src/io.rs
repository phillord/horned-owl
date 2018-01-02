use curie::PrefixMapping;

use std::io::BufRead;

use xml::EventReader;
use xml::reader::XmlEvent;
use xml::attribute::OwnedAttribute;

use model::*;

const OWL_NS:&'static str = "http://www.w3.org/2002/07/owl#";

pub fn read <R: BufRead>(buf: &mut R) -> Ontology {

    let parser = EventReader::new(buf);
    let mut ont = Ontology::new();
    let mut mapping = PrefixMapping::default();

    for e in parser {
         match e {
             Ok(XmlEvent::StartElement {name, attributes, .. }) =>{
                 match &name.namespace{
                     &Some(ref f) if f == OWL_NS => {
                         match &name.local_name[..] {
                             "Ontology" =>
                                 handle_ontology(&mut ont,
                                                 &mut mapping,
                                                 attributes),
                             "Class" =>
                                 handle_class(&mut ont,
                                              &mapping,
                                              attributes),
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

fn handle_ontology(ont:&mut Ontology, mapper:&mut PrefixMapping,
                   attributes:Vec<OwnedAttribute>){
    for attrib in attributes{
        match &attrib.name.local_name[..]{
            "ontologyIRI" =>{
                mapper.set_default(&attrib.value[..]);
                ont.id.iri = Some(ont.iri(attrib.value));
            }
            ,
            "versionIRI" =>
                ont.id.viri = Some(ont.iri(attrib.value)),
            _ => ()
        }
    }
}

#[test]
fn test_simple_ontology(){
    let ont_s = include_str!("ont/one-ont.xml");
    let ont = read(&mut ont_s.as_bytes());

    assert_eq!(ont.iri_to_str(ont.id.iri.unwrap()).unwrap(),
               "http://example.com/iri");
}

fn handle_class(ont:&mut Ontology, mapper: &PrefixMapping,
                attributes: Vec<OwnedAttribute>){
    for attrib in attributes{
        match &attrib.name.local_name[..]{
            "IRI" =>{
                let expanded =
                    match
                    mapper.expand_curie_string(&attrib.value[..]){
                        // If we expand use this
                        Ok(n) => n,
                        // Else assume it's a complete URI
                        Err(_e) => attrib.value
                    };

                let iri = ont.iri(expanded);
                ont.class(iri);
            },
            _ => ()
        }
    }
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
