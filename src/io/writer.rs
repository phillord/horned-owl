use curie::PrefixMapping;

use model::*;
use model::Kinded;
use vocab;

use quick_xml::events::BytesDecl;
use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use quick_xml::events::Event;
use quick_xml::Writer;

use std::collections::BTreeSet;
use std::io::Write as StdWrite;

/// Write an Ontology to `write`, using the given PrefixMapping
///
/// The ontology is written in OWL
/// [XML](https://www.w3.org/TR/owl2-xml-serialization/) syntax.
pub fn write(write: &mut StdWrite, ont: &Ontology,
             mapping: Option<&PrefixMapping>) {
    let mut writer = Writer::new_with_indent(write, ' ' as u8, 4);

    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => &default_mapper,
    };

    ont.render(&mut writer, &mapping);
}

/// Add an IRI to BytesStart as a element if necessary
///
/// `key` is the attribute name to use.
fn iri_maybe(elem: &mut BytesStart, key: &str, iri: &Option<IRI>) {
    match iri {
        Some(iri) => {
            elem.push_attribute((key, &(*iri)[..]));
        }
        None => {}
    }
}

/// Shrink an IRI to a curie if there is an appropriate prefix
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

/// Maybe An an attribute to `elem` if `val` is not None.
fn attribute_maybe(elem: &mut BytesStart, key: &str, val: &Option<String>) {
    match val {
        Some(val) => {
            elem.push_attribute((key, &val[..]));
        }
        None => {}
    }
}

/// Add an IRI or abbreviatedIRI attribute to elem
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
///
/// `contents` are written with a callback to allow streaming
/// of the output.
fn write_start_end<F,W>(w:&mut Writer<W>, tag: &[u8], contents: F)
    where
    F: FnMut(&mut Writer<W>),
    W: StdWrite
{
    write_start_end_with_start_callback(w, tag,
                                        |_: &mut Writer<W>, _: &mut BytesStart| return,
                                        contents);
}

/// Write a start and end tag with contents and updated start tag
///
/// Like `write_start_end` but the start tag can be modified (by
/// adding attributes for example), with a callback.
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

/// Write a tag with an IRI attribute.
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

/// Fetch the name of the tag that is used to render `AxiomKind`
fn tag_for_kind (axk:AxiomKind) -> &'static [u8] {
    match axk {
        AxiomKind::InverseObjectProperty =>
            b"InverseObjectProperties",
        AxiomKind::TransitiveObjectProperty =>
            b"TransitiveObjectProperty",
        AxiomKind::SubObjectProperty =>
            b"SubObjectPropertyOf",
        AxiomKind::SubAnnotationProperty =>
            b"SubAnnotationPropertyOf",
        AxiomKind::AssertAnnotation =>
            b"AnnotationAssertion",
        AxiomKind::DisjointClass =>
            b"DisjointClasses",
        AxiomKind::EquivalentClass =>
            b"EquivalentClasses",
        AxiomKind::SubClass =>
            b"SubClassOf",
        AxiomKind::DeclareClass =>
            b"Declaration",
        AxiomKind::DeclareObjectProperty =>
            b"Declaration",
        AxiomKind::DeclareAnnotationProperty =>
            b"Declaration",
        AxiomKind::OntologyAnnotation =>
            b"Annotation",
    }
}

/// A trait for rendering an entity
///
/// The implementations of this trait are somewhat inconsistent as to
/// whether they should render their own containing tag. So,
/// `Ontology` renders it's own `Ontology` tag, while `DeclareClass`
/// does not a `Declaration` tag, just the internal `Class` tag.
trait Render <'a, W>
{
    /// Render a entity to Write
    fn render(&self, w:&mut Writer<W>, mapping: &'a PrefixMapping)
        where W: StdWrite;
}

/// The types in `Render` are too long to type.
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

render! {
    Ontology, self, w, m,
    {
        w.write_event(Event::Decl(BytesDecl::new(&b"1.0"[..], None, None)))
            .ok();

        let mut elem = BytesStart::owned_name("Ontology");
        elem.push_attribute((b"xmlns" as &[u8], vocab::OWL));
        iri_maybe(&mut elem, "ontologyIRI", &self.id.iri);
        iri_maybe(&mut elem, "versionIRI", &self.id.viri);

        w.write_event(Event::Start(elem)).ok();

        let elem = BytesEnd::owned(b"Ontology".to_vec());

        m.render(w, m);

        for axk in AxiomKind::all_kinds() {
            for ax in self.annotated_axiom(axk) {
                ax.render(w, m);
            }
        }


        w.write_event(Event::End(elem)).ok();
    }
}

// Didn't seem worth getting the macro working for generic types.
impl <'a, W> Render<'a, W> for &'a BTreeSet<Annotation> {
    fn render(&self, w:&mut Writer<W>, m: &'a PrefixMapping)
        where W: StdWrite {
        for a in self.iter() {
            a.render(w, m);
        }
    }
}

render!{
    PrefixMapping, self, w, _m,
    {
        for pre in self.mappings() {
            let mut prefix = BytesStart::owned_name("Prefix");
            prefix.push_attribute(("name", &pre.0[..]));
            prefix.push_attribute(("IRI", &pre.1[..]));
            w.write_event(Event::Empty(prefix)).ok();
        }
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

render! {
    DeclareClass, self, w, m,
    {
        (&self.0).render(w, m);
    }
}

render! {
    DeclareObjectProperty, self, w, m,
    {
        (&self.0).render(w, m);
    }
}

render! {
    DeclareAnnotationProperty, self, w, m,
    {
        (&self.0).render(w, m);
    }
}

render!{
    AnnotatedAxiom, self, w, m,
    {
        let tag = tag_for_kind(self.kind());
        write_start_end(w, tag, |w| {
            (&self.annotation).render(w, m);
            self.axiom.render(w, m);
        });
    }
}

impl <'a, W> Render<'a, W> for &'a Vec<ClassExpression>{
    fn render(&self, w:&mut Writer<W>, m: &'a PrefixMapping)
        where W: StdWrite {
        for ce in self.iter() {
            ce.render(w, m);
        }
    }
}

impl <'a, W> Render<'a, W> for (&'a ObjectProperty, &'a Box<ClassExpression>){
    fn render(&self, w:&mut Writer<W>, m: &'a PrefixMapping)
        where W: StdWrite {
        (&self.0).render(w, m);
        (&self.1).render(w, m);
    }
}

render!{
    Class, self, w, m,
    {
        tag_with_iri(w, m, b"Class", self);
    }
}

render!{
    ClassExpression, self, w, m,
    {
        match self {
            &ClassExpression::Class(ref c) => {
                c.render(w, m);
            }
            &ClassExpression::Some { ref o, ref ce } => {
                write_start_end(w, b"ObjectSomeValuesFrom",
                                |w| {
                                    (o, ce).render(w, m);
                                });
            }
            &ClassExpression::Only { ref o, ref ce } => {
                write_start_end(w, b"ObjectAllValuesFrom",
                                |w| {
                                    (o, ce).render(w, m);
                                });
            }
            &ClassExpression::And { ref o } => {
                write_start_end(w, b"ObjectIntersectionOf",
                                |w| {
                                    o.render(w, m);
                                }
                );
            }
            &ClassExpression::Or { ref o } => {
                write_start_end(w, b"ObjectUnionOf",
                                |w| {
                                    o.render(w, m);
                                }
                );
            }
            &ClassExpression::Not { ref ce } => {
                write_start_end(w, b"ObjectComplementOf",
                                |w| {
                                    ce.render(w, m);
                                }
                );
            }
        }
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
            Axiom::SubObjectProperty(ax) => {
                ax.render(w, m);
            }
            Axiom::SubAnnotationProperty(ax) => {
                ax.render(w, m);
            }
            Axiom::AssertAnnotation(ax) => {
                ax.render(w, m);
            }
            Axiom::DisjointClass(ax) => {
                ax.render(w, m);
            }
            Axiom::EquivalentClass(ax) => {
                ax.render(w, m);
            }
            Axiom::SubClass(ax) => {
                ax.render(w, m);
            }
            Axiom::DeclareClass(ax) => {
                ax.render(w, m);
            }
            Axiom::DeclareObjectProperty(ax) => {
                ax.render(w, m);
            }
            Axiom::DeclareAnnotationProperty(ax) => {
                ax.render(w, m);
            }
            Axiom::OntologyAnnotation(ax) => {
                ax.render(w, m);
            }
        }
    }
}

render!{
    OntologyAnnotation, self, w, m,
    {
        // There is something slightly wrong with my data model
        // here. An `Annotation` object will normally render itself
        // including the tags. But the OntologyAnnotation is an
        // AnnotatedAxiom which means that we have already got
        // `Annotation` tags, so we just need to render the inner
        // contents.
        self.0.annotation_property.render(w, m);
        self.0.annotation_value.render(w, m);
    }
}

render!{
    AssertAnnotation, self, w, m,
    {
        (&self.annotation.annotation_property).render(w, m);
        (&self.annotation_subject).render(w, m);
        (&self.annotation.annotation_value).render(w, m);
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
        write_start_end(w, b"Annotation", |w| {
            self.annotation_property.render(w,m);
            self.annotation_value.render(w,m);
        });
    }
}

render!{
    SubAnnotationProperty, self, w, m,
    {
        (&self.super_property).render(w, m);
        (&self.sub_property).render(w, m);
    }
}

render!{
    SubClass, self, w, m,
    {
        (&self.super_class).render(w, m);
        (&self.sub_class).render(w, m);
    }
}

render!{
    EquivalentClass, self, w, m,
    {
        (&self.0).render(w,m);
        (&self.1).render(w,m);
    }
}

render!{
    DisjointClass, self, w, m,
    {
        (&self.0).render(w, m);
        (&self.1).render(w, m);
    }
}

render!{
    ObjectProperty, self, w, m,
    {
        tag_with_iri(w, m, b"ObjectProperty", self);
    }
}

render!{
    ObjectPropertyExpression, self, w, m,
    {
        match self {
            ObjectPropertyExpression::ObjectPropertyChain(v) => {
                write_start_end(w, b"ObjectPropertyChain", |w| {
                    for op in v {
                        op.render(w, m);
                    }
                })
            }
            ObjectPropertyExpression::ObjectProperty(op) => {
                op.render(w, m);
            }
        }
    }
}


render!{
    SubObjectProperty, self, w, m,
    {
        (&self.super_property).render(w, m);
        (&self.sub_property).render(w, m)
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
    use std::io::BufRead;
    use std::io::BufWriter;

    fn read_ok<R:BufRead>(bufread: &mut R) -> (Ontology,PrefixMapping)
    {
        read(bufread).ok().unwrap()
    }

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
        let (ont2, _) = read_ok(&mut BufReader::new(file));

        assert_eq!(ont.id.iri, ont2.id.iri);
    }

    fn roundtrip(ont: &str) -> (Ontology, PrefixMapping, Ontology, PrefixMapping) {
        let (ont_orig, prefix_orig) = read_ok(&mut ont.as_bytes());
        let mut temp_file = Temp::new_file().unwrap();

        let file = File::create(&temp_file).ok().unwrap();
        let mut buf_writer = BufWriter::new(&file);

        write(&mut buf_writer, &ont_orig, Some(&prefix_orig));
        buf_writer.flush().ok();

        let file = File::open(&temp_file).ok().unwrap();

        let (ont_round, prefix_round) = read_ok(&mut BufReader::new(&file));

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
