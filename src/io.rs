use curie::PrefixMapping;

use std::io::BufRead;

use xml::EventReader;
use xml::attribute::OwnedAttribute;
use xml::reader::XmlEvent;

use model::*;

const OWL_NS:&'static str = "http://www.w3.org/2002/07/owl#";

enum State{
    Top, Ontology, Declaration
}

pub fn read <R: BufRead>(buf: &mut R) -> (Ontology,PrefixMapping) {

    let parser = EventReader::new(buf);
    let mut ont = Ontology::new();
    let mut mapping = PrefixMapping::default();
    let mut state = State::Top;

    let owl_ns = Some(OWL_NS.to_string());

    for e in parser {
        match e {
            Ok(XmlEvent::StartElement {ref name, ref attributes, .. })
                if &name.namespace == &owl_ns =>
            {
                match (&state, &name.local_name[..]) {
                    (&State::Top, "Ontology") => {
                        ontology_attributes(&mut ont,&mut mapping,attributes);
                        state = State::Ontology;
                    }
                    (&State::Ontology, "Declaration") => {
                        state = State::Declaration;
                    }
                    (&State::Ontology,"Prefix") => {
                        prefix_attributes(&mut mapping,attributes);
                    }
                    (&State::Declaration,"Class") => {
                        add_class(&mut ont, &mut mapping,attributes);
                    }

                    _ => {
                        println!("Ontology: Unknown element in OWL NS:{}",
                                 name);
                    }
                }
            },
            Ok(XmlEvent::EndElement {ref name, .. })
                if &name.namespace == &owl_ns =>
            {
                match(&state,&name.local_name[..]){
                    (&State::Declaration,"Declaration") => {
                        state = State::Ontology
                    }
                    _ => {

                    }
                }

            },

            Err(err) => println!("Error:{}", err),
            _ => {
                //Ignore non starts
            }
        }

    }

    (ont, mapping)
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

fn prefix_attributes(mapping:&mut PrefixMapping,
                     attributes:&Vec<OwnedAttribute>){
    let mut prefix=None;
    let mut iri=None;

    for attrib in attributes{
        match &attrib.name.local_name[..]{
            "IRI" => {
                iri=Some(&attrib.value);
            }
            "name" => {
                prefix=Some(&attrib.value);
            }
            _=>{}
        }
    }

    match (iri, prefix) {
        (Some(i), Some(p)) => {
            mapping.add_prefix(&p[..], &i[..]).ok();
        }
        _=> {
            println!("Incomplete prefix element");
        }
    }
}

#[cfg(test)]
mod test{
    use super::*;
    use std::mem::transmute;
    use std::collections::HashMap;

    #[allow(dead_code)]
    #[derive(Debug)]
    struct PrefixMappingHx
    {
        default: Option<String>,
        mapping: HashMap<String,String>
    }

    #[test]
    fn test_simple_ontology_prefix(){
        let ont_s = include_str!("ont/one-ont.xml");
        let (_,mapping) = read(&mut ont_s.as_bytes());

        let open_mapping:PrefixMappingHx = unsafe{transmute(mapping)};

        assert_eq!(6, open_mapping.mapping.len());

    }
}

#[test]
fn test_simple_ontology(){
    let ont_s = include_str!("ont/one-ont.xml");
    let (ont,_) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.iri_to_str(ont.id.iri.unwrap()).unwrap(),
               "http://example.com/iri");
}

fn add_class(ont:&mut Ontology, mapping: &PrefixMapping,
             attributes:&Vec<OwnedAttribute>)
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
    let (ont,_) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 1);
    assert_eq!(
        ont.iri_to_str(ont.class.iter().next().unwrap().0).unwrap(),
        "http://example.com/iri#C");
}

#[test]
fn test_one_class_fqn(){
    let ont_s = include_str!("ont/one-class-fully-qualified.xml");
    let (ont,_) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 1);
    assert_eq!(
        ont.iri_to_str(ont.class.iter().next().unwrap().0).unwrap(),
        "http://www.russet.org.uk/#C");
}

#[test]
fn test_ten_class(){
    let ont_s = include_str!("ont/o10.owl");
    let (ont,_) = read(&mut ont_s.as_bytes());

    assert_eq!(ont.class.len(), 10);
}
