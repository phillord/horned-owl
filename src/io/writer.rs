use curie::PrefixMapping;
use model::*;

use quick_xml::events::BytesDecl;
use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use quick_xml::events::Event;
use quick_xml::Writer;

use std::collections::HashSet;
use std::hash::Hash;
use std::io::Write as StdWrite;

struct Write<'a, W>
where
    W: StdWrite,
{
    writer: Writer<W>,
    ont: &'a Ontology,
    mapping: &'a PrefixMapping,
}

pub fn write(write: &mut StdWrite, ont: &Ontology, mapping: Option<&PrefixMapping>) {
    let writer = Writer::new_with_indent(write, ' ' as u8, 4);

    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => &default_mapper,
    };

    let mut write = Write::new(writer, ont, mapping);
    write.render();
}

impl<'a, W> Write<'a, W>
where
    W: StdWrite,
{
    fn new(writer: Writer<W>, ont: &'a Ontology, mapping: &'a PrefixMapping) -> Write<'a, W> {
        Write {
            writer,
            ont,
            mapping,
        }
    }

    fn render(&mut self) {
        self.writer
            .write_event(Event::Decl(BytesDecl::new(&b"1.0"[..], None, None)))
            .ok();

        let mut elem = BytesStart::owned(b"Ontology".to_vec(), "Ontology".len());
        elem.push_attribute(("xmlns", "http://www.w3.org/2002/07/owl#"));
        self.iri_maybe(&mut elem, "ontologyIRI", &self.ont.id.iri);
        self.iri_maybe(&mut elem, "versionIRI", &self.ont.id.viri);

        self.writer.write_event(Event::Start(elem)).ok();

        let elem = BytesEnd::owned(b"Ontology".to_vec());

        self.prefixes();
        self.ontology_annotation_assertions();
        self.declarations();
        self.subclasses();
        self.equivalent_classes();
        self.disjoint_classes();
        self.annotation_assertions();
        self.suboproperties();
        self.inverse_object_properties();
        self.transitive_object_properties();
        self.writer.write_event(Event::End(elem)).ok();
    }

    fn attribute_maybe(&self, elem: &mut BytesStart, key: &str, val: &Option<String>) {
        match val {
            Some(val) => {
                elem.push_attribute((key, &val[..]));
            }
            None => {}
        }
    }

    fn iri_maybe(&self, elem: &mut BytesStart, key: &str, iri: &Option<IRI>) {
        match iri {
            Some(iri) => {
                elem.push_attribute((key, &(*iri)[..]));
            }
            None => {}
        }
    }

    fn shrink_iri_maybe(&self, iri: &str) -> String {
        match self.mapping.shrink_iri(&(*iri)[..]) {
            Ok(curie) => {
                format!("{}", curie)
            },
            Err(_) => {
                format!("{}", iri)
            },
        }
    }

    fn iri_or_curie(&self, elem: &mut BytesStart, iri: &str) {
        match self.mapping.shrink_iri(&(*iri)[..]) {
            Ok(curie) => {
                let curie = format!("{}", curie);
                elem.push_attribute(("abbreviatedIRI", &curie[..]));
            }
            Err(_) => elem.push_attribute(("IRI", &iri[..])),
        }
    }

    fn prefixes(&mut self) {
        for pre in self.mapping.mappings() {
            let mut prefix = BytesStart::owned(b"Prefix".to_vec(), "Prefix".len());
            prefix.push_attribute(("name", &pre.0[..]));
            prefix.push_attribute(("IRI", &pre.1[..]));
            self.writer.write_event(Event::Empty(prefix)).ok();
        }
    }

    fn object_property(&mut self, o: &ObjectProperty) {
        self.with_iri(b"ObjectProperty", o);
    }

    fn class(&mut self, c: &Class) {
        self.with_iri(b"Class", c);
    }

    fn annotation_property(&mut self, ap: &AnnotationProperty) {
        self.with_iri(b"AnnotationProperty", ap);
    }

    fn with_iri<I>(&mut self, tag: &[u8], into_iri: I)
    where
        I: Into<IRI>,
    {
        let iri: IRI = into_iri.into();
        let mut bytes_start = BytesStart::borrowed(tag, tag.len());

        let iri_string: String = iri.into();
        self.iri_or_curie(&mut bytes_start, &iri_string[..]);
        self.writer.write_event(Event::Empty(bytes_start)).ok();
    }

    fn iri(&mut self, i: &IRI) {
        let iri_st: String = i.into();
        let iri_shrunk = self.shrink_iri_maybe(&iri_st[..]);
        self.write_start_end(b"IRI", |s: &mut Self| {
            s.writer
                .write_event(Event::Text(BytesText::from_escaped_str(&iri_shrunk[..])))
                .ok();
        });
    }

    fn declaration_1<'b, F, E>(&mut self, named_entity: &HashSet<E>, mut ne_writer: F)
    where
        F: FnMut(&mut Self, &E),
        E: 'b + Ord + Hash,
    {
        // Make rendering determinisitic in terms of order
        let mut named_entities: Vec<&E> = named_entity.iter().collect::<Vec<&E>>();

        named_entities.sort();

        for ne in named_entities {
            self.write_start_end(b"Declaration", |s: &mut Self| ne_writer(s, ne));
        }
    }

    fn declarations(&mut self) {
        self.declaration_1(&self.ont.class, |s: &mut Self, c: &Class| s.class(c));
        self.declaration_1(
            &self.ont.object_property,
            |s: &mut Self, o: &ObjectProperty| s.object_property(o),
        );
        self.declaration_1(
            &self.ont.annotation_property,
            |s: &mut Self, a: &AnnotationProperty| s.annotation_property(a),
        );
    }

    fn class_expression(&mut self, ce: &ClassExpression) {
        match ce {
            &ClassExpression::Class(ref c) => {
                self.class(c);
            }
            &ClassExpression::Some { ref o, ref ce } => {
                self.object_some_values_from(o, ce);
            }
            &ClassExpression::Only { ref o, ref ce } => {
                self.object_all_values_from(o, ce);
            }
            &ClassExpression::And { ref o } => {
                self.object_intersection_of(o);
            }
            &ClassExpression::Or { ref o } => {
                self.object_union_of(o);
            }
            &ClassExpression::Not { ref ce } => {
                self.object_complement_of(ce);
            }
        }
    }

    fn write_start_end<F>(&mut self, tag: &[u8], contents: F)
    where
        F: FnMut(&mut Self),
    {
        self.write_start_end_with_tag(tag, |_: &mut Self, _: &mut BytesStart| return, contents);
    }

    fn write_start_end_with_tag<F, G>(&mut self, tag: &[u8], mut start: F, mut contents: G)
    where
        F: FnMut(&mut Self, &mut BytesStart),
        G: FnMut(&mut Self),
    {
        let len = tag.len();
        let mut open = BytesStart::borrowed(tag, len);

        // Pass self like this, because we cannot capture it in the
        // closure, without failing the borrow checker.
        start(self, &mut open);

        self.writer.write_event(Event::Start(open)).ok();

        contents(self);

        self.writer
            .write_event(Event::End(BytesEnd::borrowed(tag)))
            .ok();
    }

    fn object_complement_of(&mut self, ce: &ClassExpression) {
        self.write_start_end(b"ObjectComplementOf", |s: &mut Self| {
            s.class_expression(ce);
        })
    }

    fn object_binary(&mut self, o: &ObjectProperty, ce: &ClassExpression, tag: &[u8]) {
        self.write_start_end(tag, |s: &mut Self| {
            s.object_property(o);
            s.class_expression(ce);
        });
    }

    fn object_nary(&mut self, o: &Vec<ClassExpression>, tag: &[u8]) {
        self.write_start_end(tag, |s: &mut Self| {
            for ce in o.iter() {
                s.class_expression(&(*ce));
            }
        });
    }

    fn object_union_of(&mut self, operands: &Vec<ClassExpression>) {
        self.object_nary(operands, b"ObjectUnionOf")
    }

    fn object_intersection_of(&mut self, operands: &Vec<ClassExpression>) {
        self.object_nary(operands, b"ObjectIntersectionOf")
    }

    fn object_all_values_from(&mut self, o: &ObjectProperty, ce: &ClassExpression) {
        self.object_binary(o, ce, b"ObjectAllValuesFrom")
    }

    fn object_some_values_from(&mut self, o: &ObjectProperty, ce: &ClassExpression) {
        self.object_binary(o, ce, b"ObjectSomeValuesFrom")
    }

    fn subclasses(&mut self) {
        for subclass in &self.ont.subclass {
            self.write_start_end(b"SubClassOf", |s: &mut Write<W>| {
                s.class_expression(&subclass.superclass);
                s.class_expression(&subclass.subclass);
            });
        }
    }

    fn equivalent_classes(&mut self) {
        for equivalent_class in &self.ont.equivalent_class {
            self.write_start_end(b"EquivalentClasses", |s: &mut Write<W>| {
                s.class_expression(&equivalent_class.0);
                s.class_expression(&equivalent_class.1);
            });
        }
    }

    fn disjoint_classes(&mut self) {
        for disjoint_class in &self.ont.disjoint_class {
            self.write_start_end(b"DisjointClasses", |s: &mut Write<W>| {
                s.class_expression(&disjoint_class.0);
                s.class_expression(&disjoint_class.1);
            });
        }
    }

    fn object_property_expression(&mut self, ope: &ObjectPropertyExpression) {
        match ope {
            ObjectPropertyExpression::ObjectPropertyChain(v) => {
                self.write_start_end(b"ObjectPropertyChain", |s: &mut Write<W>| {
                    for op in v {
                        s.object_property(op);
                    }
                })
            }
            ObjectPropertyExpression::ObjectProperty(op) => {
                self.object_property(op);
            }
        }
    }

    fn suboproperties(&mut self) {
        for sub in &self.ont.sub_object_property {
            self.write_start_end(b"SubObjectPropertyOf", |s: &mut Write<W>| {
                s.object_property_expression(&sub.superproperty);
                s.object_property(&sub.subproperty);
            });
        }
    }

    fn transitive_object_properties(&mut self) {
        for trans in &self.ont.transitive_object_property {
            self.write_start_end(b"TransitiveObjectProperty", |s: &mut Write<W>| {
                s.object_property(&trans.0);
            });
        }
    }

    fn inverse_object_properties(&mut self) {
        for sub in &self.ont.inverse_object_property {
            self.write_start_end(b"InverseObjectProperties", |s: &mut Write<W>| {
                s.object_property(&sub.0);
                s.object_property(&sub.1);
            });
        }
    }

    fn annotation_value(&mut self, annotation: &AnnotationValue) {
        match annotation {
            AnnotationValue::IRI(iri) => {
                self.iri(iri);
            },
            AnnotationValue::PlainLiteral {
                datatype_iri,
                lang,
                literal,
            } => {
                self.write_start_end_with_tag(
                    b"Literal",
                    |s: &mut Self, b: &mut BytesStart| {
                        s.attribute_maybe(b, "xml:lang", lang);
                        s.attribute_maybe(
                            b,
                            "datatypeIRI",
                            &datatype_iri.as_ref().map(|s| s.into()),
                        );
                    },
                    |s: &mut Self| {
                        if let Some(l) = literal {
                            s.writer
                                .write_event(Event::Text(BytesText::from_escaped_str(&l[..])))
                                .ok();
                        }
                    },
                );
            }
        }
    }

    fn annotation(&mut self, annotation:&Annotation){
        self.write_start_end(b"Annotation", |s: &mut Write<W>| {
            s.annotation_property(&annotation.annotation_property);
            s.annotation_value(&annotation.annotation_value);
        });
    }

    fn annotations_maybe(&mut self, annotations_maybe: &Option<Vec<Annotation>>){
        if let &Some(ref annotations) = annotations_maybe {
            for annotation in annotations {
                self.annotation(annotation);
            }
        }
    }

    fn annotation_assertions(&mut self) {
        for assertion in &self.ont.annotation_assertion {
            self.write_start_end(b"AnnotationAssertion", |s: &mut Write<W>| {
                s.annotations_maybe(&assertion.annotated);
                s.annotation_property(&assertion.annotation.annotation_property);
                s.iri(&assertion.annotation_subject);
                s.annotation_value(&assertion.annotation.annotation_value);
            })
        }
    }

    fn ontology_annotation_assertions(&mut self) {
        for annotation in &self.ont.annotation {
            self.write_start_end(b"Annotation", |s: &mut Write<W>| {
                s.annotation_property(&annotation.annotation_property);
                s.annotation_value(&annotation.annotation_value);
            })
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

    fn assert_round(ont: &str) -> (Ontology, PrefixMapping, Ontology, PrefixMapping) {
        let (ont_orig, prefix_orig, ont_round, prefix_round) = roundtrip(ont);

        assert_eq!(ont_orig, ont_round);

        {
            let prefix_orig_map: &HashMap<&String, &String> = &prefix_orig.mappings().collect();
            let prefix_round_map: &HashMap<&String, &String> = &prefix_round.mappings().collect();

            assert_eq!(prefix_orig_map, prefix_round_map);
        }
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

    #[test]
    fn round_one_some() {
        assert_round(include_str!("../ont/one-some.xml"));
    }

    #[test]
    fn round_one_only() {
        assert_round(include_str!("../ont/one-only.xml"));
    }

    #[test]
    fn round_one_and() {
        assert_round(include_str!("../ont/one-and.xml"));
    }

    #[test]
    fn round_one_or() {
        assert_round(include_str!("../ont/one-or.xml"));
    }

    #[test]
    fn round_one_not() {
        assert_round(include_str!("../ont/one-not.xml"));
    }

    #[test]
    fn round_one_annotation_property() {
        assert_round(include_str!("../ont/one-annotation-property.xml"));
    }

    #[test]
    fn round_one_annotation() {
        assert_round(include_str!("../ont/one-annotation.xml"));
    }

    #[test]
    fn round_one_label() {
        assert_round(include_str!("../ont/one-label.xml"));
    }

    #[test]
    fn round_one_comment() {
        assert_round(include_str!("../ont/one-comment.xml"));
    }

    #[test]
    fn round_one_ontology_annotation() {
        assert_round(include_str!("../ont/one-ontology-annotation.xml"));
    }

    #[test]
    fn round_one_equivalent_class() {
        assert_round(include_str!("../ont/one-equivalent.xml"));
    }

    #[test]
    fn round_one_disjoint_class() {
        assert_round(include_str!("../ont/one-disjoint.xml"));
    }

    #[test]
    fn round_one_sub_property() {
        assert_round(include_str!("../ont/one-suboproperty.xml"));
    }

    #[test]
    fn round_one_inverse() {
        assert_round(include_str!("../ont/inverse-properties.xml"));
    }

    #[test]
    fn round_one_transitive() {
        assert_round(include_str!("../ont/transitive-properties.xml"));
    }

    #[test]
    fn round_one_subproperty_chain() {
        assert_round(include_str!("../ont/subproperty-chain.xml"));
    }

    #[test]
    fn round_annotation_on_annotation() {
        assert_round(include_str!("../ont/annotation-with-annotation.xml"));
    }
}
