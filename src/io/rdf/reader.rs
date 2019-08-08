use crate::model::*;

use std::io::BufRead;

use curie::PrefixMapping;

use failure::Error;

/// https://www.w3.org/TR/owl2-mapping-to-rdf/

pub fn read<R: BufRead>(bufread: &mut R) -> Result<(Ontology, PrefixMapping), Error> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

pub fn read_with_build<R: BufRead>(
    bufread: &mut R,
    _build: &Build,
) -> Result<(Ontology, PrefixMapping), Error> {
    let parser = sophia::parser::xml::Config::default();
    let triple_iter = parser.parse_bufread(bufread);

    for i in triple_iter {
        dbg!(i);
    }

    bail!("Bleh");
}

#[cfg(test)]
mod test {
    use super::*;
    use std::collections::HashMap;
    macro_rules! inc_rdf {
        ($filename:literal) => {
            include_str!(concat!("../../ont/owl-rdf/", $filename, ".owl"));
        };
    }

    macro_rules! inc_xml {
        ($filename:literal) => {
            include_str!(concat!("../../ont/owl-xml/", $filename, ".owx"));
        };
    }

    fn read_ok<R: BufRead>(bufread: &mut R) -> (Ontology, PrefixMapping) {
        let r = read(bufread);

        assert!(r.is_ok(), "Expected ontology, get failure: {:?}", r.err());
        r.unwrap()
    }

    fn compare(rdfread: &str, xmlread: &str) {
        let (rdfont, rdfmapping) = read_ok(&mut rdfread.as_bytes());
        let (xmlont, xmlmapping) = crate::io::reader::test::read_ok(&mut xmlread.as_bytes());

        assert_eq!(rdfont, xmlont);
        let rdfmapping: &HashMap<&String, &String> = &rdfmapping.mappings().collect();
        let xmlmapping: &HashMap<&String, &String> = &xmlmapping.mappings().collect();

        assert_eq!(rdfmapping, xmlmapping);
    }

    #[test]
    fn test_one_class() {
        let ont_s = inc_rdf!("one-class");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.declare_class().count(), 1);
        assert_eq!(
            String::from(&ont.declare_class().next().unwrap().0),
            "http://example.com/iri#C"
        );

        compare(ont_s, inc_xml!("one-class"))
    }

}
