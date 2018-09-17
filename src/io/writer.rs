use curie::PrefixMapping;
use model::*;
use model::Kinded;

use quick_xml::events::BytesDecl;
use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use quick_xml::events::Event;
use quick_xml::Writer;

use std::collections::BTreeSet;
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
        self.subaproperties();
        self.suboproperties();
        self.iter_render(self.ont.annotated_axiom(AxiomKind::InverseObjectProperty));
        self.iter_render(self.ont.annotated_axiom(AxiomKind::TransitiveObjectProperty));
        self.writer.write_event(Event::End(elem)).ok();
    }

    fn iter_render<I, R>(&mut self, i:I)
        where I: Iterator<Item=&'a R>,
              R: Render<'a, W> + 'a
    {
        for item in i {
            item.render(&mut self.writer, self.mapping);
        }
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

    fn declaration_1<'b, F, I>(&mut self,
                                  declare: I,
                                  mut declare_writer: F)
    where
        F: FnMut(&mut Self, &'b AnnotatedAxiom),
        I: Iterator<Item=&'b AnnotatedAxiom>
    {

        let mut declare:Vec<&AnnotatedAxiom> = declare.collect();
        declare.sort_unstable();

        for ne in declare {
            self.write_start_end(b"Declaration",
                                 |s: &mut Self| declare_writer(s, ne));
        }
    }

    fn declarations(&mut self) {
        self.declaration_1(
            self.ont.annotated_axiom(AxiomKind::DeclareClass),
            |s: &mut Self, c: &AnnotatedAxiom|
            if let Axiom::DeclareClass(ref c) = c.axiom {
                s.class(&c.0)
            });

        self.declaration_1(
            self.ont.annotated_axiom(AxiomKind::DeclareObjectProperty),
            |s: &mut Self, o: &AnnotatedAxiom|
            if let Axiom::DeclareObjectProperty(ref o) = o.axiom {
                s.object_property(&o.0)
            });

        self.declaration_1(
            self.ont.annotated_axiom(AxiomKind::DeclareAnnotationProperty),
            |s: &mut Self, a: &AnnotatedAxiom|
            if let Axiom::DeclareAnnotationProperty(ref a) = a.axiom {
                s.annotation_property(&a.0)
            });
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
        for subclass in self.ont.sub_class() {
            self.write_start_end(b"SubClassOf", |s: &mut Write<W>| {
                s.class_expression(&subclass.super_class);
                s.class_expression(&subclass.sub_class);
            });
        }
    }

    fn equivalent_classes(&mut self) {
        for equivalent_class in self.ont.equivalent_class() {
            self.write_start_end(b"EquivalentClasses", |s: &mut Write<W>| {
                s.class_expression(&equivalent_class.0);
                s.class_expression(&equivalent_class.1);
            });
        }
    }

    fn disjoint_classes(&mut self) {
        for disjoint_class in self.ont.disjoint_class() {
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
        for sub in self.ont.sub_object_property() {
            self.write_start_end(b"SubObjectPropertyOf", |s: &mut Write<W>| {
                s.object_property_expression(&sub.super_property);
                s.object_property(&sub.sub_property);
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

    fn annotations_maybe(&mut self, annotations: &BTreeSet<Annotation>){
        for annotation in annotations {
            self.annotation(annotation);
        }
    }

    fn annotation_assertions(&mut self) {
        for annotated_assertion in self.ont.annotated_axiom(AxiomKind::AssertAnnotation) {
            self.write_start_end(b"AnnotationAssertion", |s: &mut Write<W>| {
                s.annotations_maybe(&annotated_assertion.annotation);
                if let Axiom::AssertAnnotation(ref ax) = annotated_assertion.axiom {
                    s.annotation_property(&ax.annotation.annotation_property);
                    s.iri(&ax.annotation_subject);
                    s.annotation_value(&ax.annotation.annotation_value);
                }
            })
        }
    }

    fn subaproperties(&mut self) {
        for sub in self.ont.sub_annotation_property() {
            self.write_start_end(b"SubAnnotationPropertyOf", |s: &mut Write<W>| {
                s.annotation_property(&sub.super_property);
                s.annotation_property(&sub.sub_property);
            });
        }
    }

    fn ontology_annotation_assertions(&mut self) {
        for annotation in self.ont.ontology_annotation() {
            self.write_start_end(b"Annotation", |s: &mut Write<W>| {
                s.annotation_property(&annotation.0.annotation_property);
                s.annotation_value(&annotation.0.annotation_value);
            })
        }
    }
}

fn shrink_iri_maybe<'a>(iri: &str,
                        mapping: &'a PrefixMapping) -> String {
    match mapping.shrink_iri(&(*iri)[..]) {
        Ok(curie) => {
            format!("{}", curie)
        },
        Err(_) => {
            format!("{}", iri)
        },
    }
}

fn attribute_maybe(elem: &mut BytesStart, key: &str, val: &Option<String>) {
    match val {
        Some(val) => {
            elem.push_attribute((key, &val[..]));
        }
        None => {}
    }
}

/// Add an IRI or abbreviatedIRI tag to elem
fn iri_or_curie<'a>(mapping:&'a PrefixMapping, elem: &mut BytesStart, iri: &str) {
    match mapping.shrink_iri(&(*iri)[..]) {
        Ok(curie) => {
            let curie = format!("{}", curie);
            elem.push_attribute(("abbreviatedIRI", &curie[..]));
        }
        Err(_) => elem.push_attribute(("IRI", &iri[..])),
    }
}

/// Write a start and end tag with some contents
fn write_start_end<F,W>(w:&mut Writer<W>, tag: &[u8], contents: F)
    where
    F: FnMut(&mut Writer<W>),
    W: StdWrite
{
    write_start_end_with_start_callback(w, tag,
                                        |_: &mut Writer<W>, _: &mut BytesStart| return,
                                        contents);
}

/// Write a start and end tag with contents but callback on start
fn write_start_end_with_start_callback<F, G, W>(w:&mut Writer<W>, tag: &[u8],
                                                mut start: F, mut contents: G)
    where
    F: FnMut(&mut Writer<W>, &mut BytesStart),
    G: FnMut(&mut Writer<W>),
    W: StdWrite
{
    let len = tag.len();
    let mut open = BytesStart::borrowed(tag, len);

    // Pass self like this, because we cannot capture it in the
    // closure, without failing the borrow checker.
    start(w, &mut open);

    w.write_event(Event::Start(open)).ok();

    contents(w);

    w.write_event(Event::End(BytesEnd::borrowed(tag)))
        .ok();
}

fn tag_with_iri<'a, I,W>(w:&mut Writer<W>, mapping:&'a PrefixMapping,
                         tag: &[u8], into_iri: I)
    where I: Into<IRI>,
          W: StdWrite

{
    let iri: IRI = into_iri.into();
    let mut bytes_start = BytesStart::borrowed(tag, tag.len());

    let iri_string: String = iri.into();
    iri_or_curie(mapping, &mut bytes_start, &iri_string[..]);
    w.write_event(Event::Empty(bytes_start)).ok();
}

fn tag_for_kind (axk:AxiomKind) -> &'static [u8] {
    match axk {
        AxiomKind::InverseObjectProperty =>
            b"InverseObjectProperties",
        AxiomKind::TransitiveObjectProperty =>
            b"TransitiveObjectProperty",
        _ => {
            panic!("Fetching tag for unknown kind");
        }
    }
}

trait Render <'a, W>
{
    /// Render a entity to Write
    ///
    /// The intention here is to write out Write, so eventually Write
    /// should be become a plain old Writer. However, for the moment,
    /// I need access to the methods in Write so I re-write it
    /// incrementally.
    ///
    /// The complexity of this is paragraph is one reason why I need
    /// to redo things!
    fn render(&self, w:&mut Writer<W>, mapping: &'a PrefixMapping)
        where W: StdWrite;
}

macro_rules! render {
    ($type:ident, $self:ident, $write:ident, $map:ident,
     $body:tt) => {

        impl <'a, W> Render<'a, W> for $type {
            fn render(& $self, $write:&mut Writer<W>, $map: &'a PrefixMapping)
                where W: StdWrite
                $body
        }
    }
}

/// TODO -- Throw this away and replace with iter_render, once we move
/// it out
fn annotations_maybe<'a, W>(annotations: &BTreeSet<Annotation>,
                            w:&mut Writer<W>, m: &'a PrefixMapping)
    where W: StdWrite
{
    for annotation in annotations {
        annotation.render(w, m);
    }
}

render!{
    IRI, self, w, m,
    {
        let iri_st: String = self.into();
        let iri_shrunk = shrink_iri_maybe(&iri_st[..], m);
        write_start_end(w, b"IRI", |w| {
            w.write_event(Event::Text(BytesText::from_escaped_str(&iri_shrunk[..])))
                .ok();
        });
    }
}



render!{
    AnnotatedAxiom, self, w, m,
    {
        let tag = tag_for_kind(self.kind());
        write_start_end(w, tag, |w| {
            annotations_maybe(&self.annotation, w, m);
            self.axiom.render(w, m);
        });
    }
}

render! {
    Axiom, self, w, m,
    {
        match self {
            Axiom::TransitiveObjectProperty(ax) =>{
                ax.render(w, m);
            }
            Axiom::InverseObjectProperty(ax) =>{
                ax.render(w, m);
            }
            _ => {
                unimplemented!("Axiom with no render implementation");
            }
        }
    }
}

render!{
    AnnotationValue, self, w, m,
    {
        match self {
            AnnotationValue::IRI(iri) => {
                iri.render(w, m);
            },
            AnnotationValue::PlainLiteral {
                datatype_iri,
                lang,
                literal,
            } => {
                write_start_end_with_start_callback(
                    w,
                    b"Literal",
                    |_s: &mut Writer<W>, b: &mut BytesStart| {
                        attribute_maybe(b, "xml:lang", lang);
                        attribute_maybe(
                            b,
                            "datatypeIRI",
                            &datatype_iri.as_ref().map(|s| s.into()),
                        );
                    },
                    |s: &mut Writer<W>| {
                        if let Some(l) = literal {
                            s.write_event(Event::Text(BytesText::from_escaped_str(&l[..])))
                                .ok();
                        }
                    },
                );
            }
        }
    }
}

render!{
    AnnotationProperty, self, w, m,
    {
        tag_with_iri(w, m, b"AnnotationProperty", self);
    }
}

render!{
    Annotation, self, w, m,
    {
        write_start_end(w, b"Annotation", |w: &mut Writer<W>| {
            self.annotation_property.render(w,m);
            self.annotation_value.render(w,m);
        });
    }
}

render!{
    ObjectProperty, self, w, m,
    {
        tag_with_iri(w, m, b"ObjectProperty", self);
    }
}

render!{
    TransitiveObjectProperty, self, w, m,
    {
        (&self.0).render(w, m);
    }
}

render!{
    InverseObjectProperty, self, w, m,
    {
        (&self.0).render(w, m);
        (&self.1).render(w, m);
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
        let build = Build::new();

        let iri = build.iri("http://www.example.com/a".to_string());
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
    fn round_one_annotated_transitive() {
        assert_round(include_str!("../ont/annotation-on-transitive.xml"));
    }

    #[test]
    fn round_one_subproperty_chain() {
        assert_round(include_str!("../ont/subproperty-chain.xml"));
    }

    #[test]
    fn round_annotation_on_annotation() {
        assert_round(include_str!("../ont/annotation-with-annotation.xml"));
    }

    #[test]
    fn round_sub_annotation() {
        assert_round(include_str!("../ont/sub-annotation.xml"));
    }
}
