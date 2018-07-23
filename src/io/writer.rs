use curie::PrefixMapping;
use model::*;


use quick_xml::events::BytesDecl;
use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::Event;
use quick_xml::Writer;

use std::io::Write as StdWrite;

struct Write<'a, W>
    where W: StdWrite
{
    writer: Writer<W>,
    ont: &'a Ontology,
    mapping: &'a PrefixMapping,
}

pub fn write (write: &mut StdWrite,
              ont: &Ontology,
              mapping: Option<&PrefixMapping>) {
    let writer = Writer::new_with_indent(write, ' ' as u8, 4);

    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => {
            &default_mapper
        }
    };

    let mut write = Write::new(writer, ont, mapping);
    write.parse();
}

impl <'a, W> Write<'a,W>
    where W: StdWrite
{
    fn new(writer: Writer<W>, ont:&'a Ontology,
           mapping:&'a PrefixMapping) -> Write<'a, W> {
        Write{writer, ont, mapping}
    }

    fn parse(&mut self) {

        self.writer
            .write_event(Event::Decl(BytesDecl::new(&b"1.0"[..], None, None)))
            .ok();

        let mut elem = BytesStart::owned(b"Ontology".to_vec(), "Ontology".len());
        elem.push_attribute(("xmlns", "http://www.w3.org/2002/07/owl#"));
        self.push_iri_maybe(&mut elem, "ontologyIRI",
                            &self.ont.id.iri);
        self.push_iri_maybe(&mut elem, "versionIRI",
                            &self.ont.id.viri);

        self.writer.write_event(Event::Start(elem)).ok();

        let elem = BytesEnd::owned(b"Ontology".to_vec());

        self.push_prefixes();
        self.push_classes();
        self.push_subclasses();
        self.writer.write_event(Event::End(elem)).ok();
    }

    fn push_iri_maybe(&self, elem: &mut BytesStart,
                      key: &str, iri: &Option<IRI>) {
        match iri {
            Some(iri) => {
                elem.push_attribute((key, &(*iri)[..]));
            }
            None => {}
        }
    }

    fn push_iri_or_curie(
        &self,
        elem: &mut BytesStart,
        iri: &str,
    ) {
        let key = "IRI";
        match self.mapping.shrink_iri(&(*iri)[..]) {
            Ok(curie) => {
                let curie_str = format!("{}", curie);
                elem.push_attribute((key, &curie_str[..]));
                return;
            }
            Err(_) => {
                elem.push_attribute((key, &iri[..]));
            }
        }
    }

    fn push_prefixes(&mut self) {
        for pre in self.mapping.mappings() {
            let mut prefix = BytesStart::owned(b"Prefix".to_vec(),
                                               "Prefix".len());
            prefix.push_attribute(("name", &pre.0[..]));
            prefix.push_attribute(("IRI", &pre.1[..]));
            self.writer.write_event(Event::Empty(prefix)).ok();
        }
    }

    fn push_class(&mut self, iri: &str) {
        let mut class = BytesStart::owned(b"Class".to_vec(), "Class".len());
        self.push_iri_or_curie(&mut class, iri);
        self.writer.write_event(Event::Empty(class)).ok();
    }

    fn push_classes(&mut self) {
        // Make rendering determinisitic in terms of order
        let mut classes: Vec<&String> = self.ont.class
            .iter()
            .map(|c| &(*c.0)).collect::<Vec<&String>>();

        classes.sort();

        for iri in classes {
            let mut declaration = BytesStart::owned(b"Declaration".to_vec(), "Declaration".len());

            self.writer.write_event(Event::Start(declaration)).ok();
            self.push_class(iri);
            self.writer
                .write_event(Event::End(BytesEnd::owned(b"Declaration".to_vec())))
                .ok();
        }
    }

    fn push_class_expression(&mut self, ce: &ClassExpression) {
        match ce {
            &ClassExpression::Class(ref c) => {
                self.push_class(&String::from(c));
            }

            _ => {
                unimplemented!();
            }
        }
    }

    fn push_subclasses(&mut self) {
        for subclass in &self.ont.subclass {
            let mut declaration = BytesStart::owned(b"SubClassOf".to_vec(),
                                                    "SubClassOf".len());
            self.writer.write_event(Event::Start(declaration)).ok();

            self.push_class_expression(&subclass.superclass);
            self.push_class_expression(&subclass.subclass);
            self.writer
                .write_event(Event::End(BytesEnd::owned(b"SubClassOf".to_vec())))
                .ok();
        }
    }
}


#[cfg(test)]
mod test {

    extern crate mktemp;

    use self::mktemp::Temp;
    use super::*;
    use io::reader::*;

    use std::collections::HashMap;

    use std::fs::File;
    use std::io::BufReader;
    use std::io::BufWriter;

    #[test]
    fn test_ont_rt() {
        let mut ont = Ontology::new();
        let iri = ont.iri("http://www.example.com/a".to_string());
        ont.id.iri = Some(iri);
        let temp_file = Temp::new_file().unwrap();
        let file = File::create(&temp_file).ok().unwrap();
        write(&mut BufWriter::new(file), &ont, None);

        let file = File::open(&temp_file).ok().unwrap();
        let (ont2, _) = read(&mut BufReader::new(file));

        assert_eq!(ont.id.iri, ont2.id.iri);
    }

    fn roundtrip(ont: &str) -> (Ontology, PrefixMapping, Ontology, PrefixMapping) {
        let (ont_orig, prefix_orig) = read(&mut ont.as_bytes());
        let mut temp_file = Temp::new_file().unwrap();

        let file = File::create(&temp_file).ok().unwrap();
        let mut buf_writer = BufWriter::new(&file);

        write(&mut buf_writer, &ont_orig, Some(&prefix_orig));
        buf_writer.flush().ok();

        let file = File::open(&temp_file).ok().unwrap();

        let (ont_round, prefix_round) = read(&mut BufReader::new(&file));

        temp_file.release();

        return (ont_orig, prefix_orig, ont_round, prefix_round);
    }

    #[test]
    fn round_one_ont() {
        let (ont_orig, _prefix_orig, ont_round, _prefix_round) =
            roundtrip(include_str!("../ont/one-ont.xml"));

        assert_eq!(ont_orig.id.iri, ont_round.id.iri);
    }

    #[test]
    fn round_one_ont_prefix() {
        let (_ont_orig, prefix_orig, _ont_round, prefix_round) =
            roundtrip(include_str!("../ont/one-ont.xml"));

        let prefix_orig_map: HashMap<&String, &String> = prefix_orig.mappings().collect();

        let prefix_round_map: HashMap<&String, &String> = prefix_round.mappings().collect();

        assert_eq!(prefix_orig_map, prefix_round_map);
    }

    #[test]
    fn round_one_subclass() {
        let (ont_orig, _prefix_orig, ont_round, _prefix_round) =
            roundtrip(include_str!("../ont/one-subclass.xml"));

        assert_eq!(ont_orig, ont_round);
    }
}
