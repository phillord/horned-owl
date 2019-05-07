use curie::PrefixMapping;

use model::Kinded;
use model::*;
use vocab::Namespace::*;
use vocab::WithIRI;

use quick_xml::events::BytesDecl;
use quick_xml::events::BytesEnd;
use quick_xml::events::BytesStart;
use quick_xml::events::BytesText;
use quick_xml::events::Event;
use quick_xml::Writer;

use std::collections::BTreeSet;
use std::io::Write as StdWrite;

use failure::Error;

/// Write an Ontology to `write`, using the given PrefixMapping
///
/// The ontology is written in OWL
/// [XML](https://www.w3.org/TR/owl2-xml-serialization/) syntax.
pub fn write(
    write: &mut StdWrite,
    ont: &Ontology,
    mapping: Option<&PrefixMapping>,
) -> Result<(), Error> {
    let mut writer = Writer::new_with_indent(write, b' ', 4);

    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => &default_mapper,
    };

    ont.render(&mut writer, &mapping)?;

    Ok(())
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
fn shrink_iri_maybe<'a>(iri: &str, mapping: &'a PrefixMapping) -> String {
    match mapping.shrink_iri(&(*iri)[..]) {
        Ok(curie) => format!("{}", curie),
        Err(_) => iri.to_string(),
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
fn iri_or_curie<'a>(mapping: &'a PrefixMapping, elem: &mut BytesStart, iri: &str) {
    match mapping.shrink_iri(&(*iri)[..]) {
        Ok(curie) => {
            let curie = format!("{}", curie);
            elem.push_attribute(("abbreviatedIRI", &curie[..]));
        }
        Err(_) => elem.push_attribute(("IRI", &iri[..])),
    }
}

/// Write a tag with an IRI attribute.
fn with_iri<'a, I, W>(
    w: &mut Writer<W>,
    mapping: &'a PrefixMapping,
    tag: &[u8],
    into_iri: I,
) -> Result<(), Error>
where
    I: Into<IRI>,
    W: StdWrite,
{
    let iri: IRI = into_iri.into();
    let mut bytes_start = BytesStart::borrowed(tag, tag.len());

    let iri_string: String = iri.into();
    iri_or_curie(mapping, &mut bytes_start, &iri_string[..]);
    w.write_event(Event::Empty(bytes_start))?;

    Ok(())
}

/// Fetch the name of the tag that is used to render `AxiomKind`
fn tag_for_kind(axk: AxiomKind) -> &'static [u8] {
    match axk {
        AxiomKind::Import => b"Import",
        AxiomKind::OntologyAnnotation => b"Annotation",
        AxiomKind::DeclareClass => b"Declaration",
        AxiomKind::DeclareObjectProperty => b"Declaration",
        AxiomKind::DeclareAnnotationProperty => b"Declaration",
        AxiomKind::DeclareDataProperty => b"Declaration",
        AxiomKind::DeclareNamedIndividual => b"Declaration",
        AxiomKind::DeclareDatatype => b"Declaration",
        AxiomKind::SubClassOf => b"SubClassOf",
        AxiomKind::EquivalentClasses => b"EquivalentClasses",
        AxiomKind::DisjointClasses => b"DisjointClasses",
        AxiomKind::SubObjectPropertyOf => b"SubObjectPropertyOf",
        AxiomKind::EquivalentObjectProperties => b"EquivalentObjectProperties",
        AxiomKind::DisjointObjectProperties => b"DisjointObjectProperties",
        AxiomKind::InverseObjectProperties => b"InverseObjectProperties",
        AxiomKind::ObjectPropertyDomain => b"ObjectPropertyDomain",
        AxiomKind::ObjectPropertyRange => b"ObjectPropertyRange",
        AxiomKind::FunctionalObjectProperty => b"FunctionalObjectProperty",
        AxiomKind::InverseFunctionalObjectProperty => b"InverseFunctionalObjectProperty",
        AxiomKind::ReflexiveObjectProperty => b"ReflexiveObjectProperty",
        AxiomKind::IrreflexiveObjectProperty => b"IrreflexiveObjectProperty",
        AxiomKind::SymmetricObjectProperty => b"SymmetricObjectProperty",
        AxiomKind::AsymmetricObjectProperty => b"AsymmetricObjectProperty",
        AxiomKind::TransitiveObjectProperty => b"TransitiveObjectProperty",
        AxiomKind::SubDataPropertyOf => b"SubDataPropertyOf",
        AxiomKind::EquivalentDataProperties => b"EquivalentDataProperties",
        AxiomKind::DisjointDataProperties => b"DisjointDataProperties",
        AxiomKind::DataPropertyDomain => b"DataPropertyDomain",
        AxiomKind::DataPropertyRange => b"DataPropertyRange",
        AxiomKind::FunctionalDataProperty => b"FunctionalDataProperty",
        AxiomKind::DatatypeDefinition => b"DatatypeDefinition",
        AxiomKind::HasKey => b"HasKey",
        AxiomKind::SameIndividual => b"SameIndividual",
        AxiomKind::DifferentIndividuals => b"DifferentIndividuals",
        AxiomKind::ClassAssertion => b"ClassAssertion",
        AxiomKind::ObjectPropertyAssertion => b"ObjectPropertyAssertion",
        AxiomKind::NegativeObjectPropertyAssertion => b"NegativeObjectPropertyAssertion",
        AxiomKind::DataPropertyAssertion => b"DataPropertyAssertion",
        AxiomKind::NegativeDataPropertyAssertion => b"NegativeDataPropertyAssertion",
        AxiomKind::AnnotationAssertion => b"AnnotationAssertion",
        AxiomKind::SubAnnotationPropertyOf => b"SubAnnotationPropertyOf",
    }
}

/// A trait for rendering an entity
///
/// The implementations of this trait are somewhat inconsistent as to
/// whether they should render their own containing tag. So,
/// `Ontology` renders it's own `Ontology` tag, while `DeclareClass`
/// does not a `Declaration` tag, just the internal `Class` tag.
trait Render<'a, W: StdWrite> {
    /// Render a entity to Write
    fn render(&self, w: &mut Writer<W>, mapping: &'a PrefixMapping) -> Result<(), Error>;

    fn within(&self, w: &mut Writer<W>, m: &'a PrefixMapping, tag: &[u8]) -> Result<(), Error> {
        let open = BytesStart::borrowed(tag, tag.len());
        w.write_event(Event::Start(open))?;

        self.render(w, m)?;

        w.write_event(Event::End(BytesEnd::borrowed(tag)))?;

        Ok(())
    }

    fn within_tag(
        &self,
        w: &mut Writer<W>,
        m: &'a PrefixMapping,
        open: BytesStart,
    ) -> Result<(), Error> {
        let clone = open.clone();
        w.write_event(Event::Start(clone))?;

        self.render(w, m)?;

        w.write_event(Event::End(BytesEnd::borrowed(open.local_name())))?;

        Ok(())
    }
}

/// The types in `Render` are too long to type.
macro_rules! render {
    ($type:ty, $self:ident, $write:ident, $map:ident,
     $body:tt) => {

        impl <'a, W:StdWrite> Render<'a, W> for $type {
            fn render(& $self, $write:&mut Writer<W>, $map: &'a PrefixMapping)
                      -> Result<(),Error>
                where W: StdWrite
                $body
        }
    }
}

macro_rules! contents {
    ($type:ty, $self:ident, $body:expr) => {
        render! {$type, $self, w, m,{
                $body.render(w, m)?;
                Ok(())
            }
        }
    };
}

macro_rules! content0 {
    ($type:ty) => {
        contents!{$type, self, &self.0}
    }
}

render! {
    Ontology, self, w, m,
    {
        w.write_event(Event::Decl(BytesDecl::new(&b"1.0"[..], None, None)))?;

        let mut elem = BytesStart::owned_name("Ontology");
        elem.push_attribute((b"xmlns" as &[u8], OWL.iri_b()));
        iri_maybe(&mut elem, "ontologyIRI", &self.id.iri);
        iri_maybe(&mut elem, "versionIRI", &self.id.viri);

        w.write_event(Event::Start(elem))?;

        let elem = BytesEnd::owned(b"Ontology".to_vec());

        m.render(w, m)?;

        for axk in AxiomKind::all_kinds() {
            for ax in self.annotated_axiom(axk) {
                ax.render(w, m)?;
            }
        }


        w.write_event(Event::End(elem))?;

        Ok(())
    }
}

// Render Impl for container and collection types
impl<'a, T: Render<'a, W>, W: StdWrite> Render<'a, W> for BTreeSet<T> {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), Error> {
        for item in self.iter() {
            item.render(w, m)?;
        }

        Ok(())
    }
}

impl<'a, O: Render<'a, W>, W: StdWrite> Render<'a, W> for Vec<O> {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), Error>
    where
        W: StdWrite,
    {
        for a in self.iter() {
            a.render(w, m)?;
        }

        Ok(())
    }
}

impl<'a, T: Render<'a, W>, W: StdWrite> Render<'a, W> for Box<T> {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), Error> {
        (**self).render(w, m)?;

        Ok(())
    }
}

impl<'a, A: Render<'a, W>, W: StdWrite> Render<'a, W> for (&'a A,) {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), Error> {
        (&self.0).render(w, m)?;

        Ok(())
    }
}

impl<'a, A: Render<'a, W>, B: Render<'a, W>, W: StdWrite> Render<'a, W> for (&'a A, &'a B) {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), Error> {
        (&self.0).render(w, m)?;
        (&self.1).render(w, m)?;

        Ok(())
    }
}

impl<'a, A: Render<'a, W>, B: Render<'a, W>, C: Render<'a, W>, W: StdWrite> Render<'a, W>
    for (&'a A, &'a B, &'a C)
{
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), Error> {
        (&self.0).render(w, m)?;
        (&self.1).render(w, m)?;
        (&self.2).render(w, m)?;

        Ok(())
    }
}

impl<'a, A: Render<'a, W>, B: Render<'a, W>, C: Render<'a, W>, D: Render<'a, W>, W: StdWrite> Render<'a, W>
    for (&'a A, &'a B, &'a C, &'a D)
{
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), Error> {
        (&self.0).render(w, m)?;
        (&self.1).render(w, m)?;
        (&self.2).render(w, m)?;
        (&self.3).render(w, m)?;

        Ok(())
    }
}

render!{
    PrefixMapping, self, w, _m,
    {
        for pre in self.mappings() {
            let mut prefix = BytesStart::owned_name("Prefix");
            prefix.push_attribute(("name", &pre.0[..]));
            prefix.push_attribute(("IRI", &pre.1[..]));
            w.write_event(Event::Empty(prefix))?;
        }

        Ok(())
    }
}

render!{
    String, self, w, _m,
    {
        w.write_event(Event::Text(BytesText::from_escaped_str(&self[..])))?;
        Ok(())
    }
}

render!{
    IRI, self, w, m,
    {
        let iri_st: String = self.into();
        let iri_shrunk = shrink_iri_maybe(&iri_st[..], m);
        iri_shrunk.within(w, m, b"IRI")?;
        Ok(())
    }
}

content0!{DeclareClass}

content0!{DeclareObjectProperty}

content0!{DeclareAnnotationProperty}

content0!{DeclareDataProperty}

content0!{DeclareNamedIndividual}

content0!{DeclareDatatype}

render!{
    AnnotatedAxiom, self, w, m,
    {
        (
            (&self.annotation),
            (&self.axiom)
        ).within(w, m,
                 tag_for_kind(self.kind()))?;

        Ok(())
    }
}

render!{
    Class, self, w, m,
    {
        with_iri(w, m, b"Class", self)?;

        Ok(())
    }
}

render! {
    &'a ObjectProperty, self, w, m,
    {
        with_iri(w, m, b"ObjectProperty", *self)?;

        Ok(())
    }
}
render! {
    ObjectProperty, self, w, m,
    {
        with_iri(w, m, b"ObjectProperty", self)?;

        Ok(())
    }
}

render! {
    DataProperty, self, w, m,
    {
        with_iri(w, m, b"DataProperty", self)?;

        Ok(())
    }
}

render! {
    Datatype, self, w, m,
    {
        with_iri(w, m, b"Datatype", self)?;

        Ok(())
    }
}
render! {
    NamedIndividual, self, w, m,
    {
        with_iri(w, m, b"NamedIndividual", self)?;

        Ok(())
    }
}

render!{
    ClassExpression, self, w, m,
    {
        match *self {
            ClassExpression::Class(ref c) => {
                c.render(w, m)?;
            }
            ClassExpression::ObjectSomeValuesFrom {ref o, ref ce} => {
                (o, ce).within(w, m, b"ObjectSomeValuesFrom")?;
            }
            ClassExpression::ObjectAllValuesFrom {ref o, ref ce} => {
                (o, ce).within(w, m, b"ObjectAllValuesFrom")?;
            }
            ClassExpression::ObjectIntersectionOf {ref o} => {
                o.within(w, m, b"ObjectIntersectionOf")?;
            }
            ClassExpression::ObjectUnionOf {ref o} => {
                o.within(w, m, b"ObjectUnionOf")?;
            }
            ClassExpression::ObjectComplementOf {ref ce} => {
                ce.within(w, m, b"ObjectComplementOf")?;
            }
            ClassExpression::ObjectHasValue {ref o, ref i} => {
                (o, i).within(w, m, b"ObjectHasValue")?;
            }
            ClassExpression::ObjectOneOf {ref o} => {
                o.within(w, m, b"ObjectOneOf")?;
            }
            ClassExpression::ObjectHasSelf (ref o) => {
                o.within(w, m, b"ObjectHasSelf")?;
            }
            ClassExpression::ObjectMinCardinality{n, ref o, ref ce} => {
                let mut open = BytesStart::owned_name("ObjectMinCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (o, ce).within_tag(w, m, open)?;
            }
            ClassExpression::ObjectMaxCardinality{n, ref o, ref ce} => {
                let mut open = BytesStart::owned_name("ObjectMaxCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (o, ce).within_tag(w, m, open)?;
            }
            ClassExpression::ObjectExactCardinality{n, ref o, ref ce} => {
                let mut open = BytesStart::owned_name("ObjectExactCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (o, ce).within_tag(w, m, open)?;
            }
            ClassExpression::DataSomeValuesFrom{ref dp, ref dr} => {
                (dp, dr).within(w, m, b"DataSomeValuesFrom")?;
            }
            ClassExpression::DataAllValuesFrom{ref dp, ref dr} => {
                (dp, dr).within(w, m, b"DataAllValuesFrom")?;
            }
            ClassExpression::DataHasValue{ref dp, ref l} => {
                (dp, l).within(w, m, b"DataHasValue")?;
            }
            ClassExpression::DataMinCardinality{n, ref dp, ref dr} => {
                let mut open = BytesStart::owned_name("DataMinCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (dp, dr).within_tag(w, m, open)?;
            }
            ClassExpression::DataMaxCardinality{n, ref dp, ref dr} => {
                let mut open = BytesStart::owned_name("DataMaxCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (dp, dr).within_tag(w, m, open)?;
            }
            ClassExpression::DataExactCardinality{n, ref dp, ref dr} => {
                let mut open = BytesStart::owned_name("DataExactCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (dp, dr).within_tag(w, m, open)?;
            }
        }
        Ok(())
    }
}

render! {
    Axiom, self, w, m,
    {
        match self {
            Axiom::Import(ax) => ax.render(w, m)?,
            Axiom::OntologyAnnotation(ax) => ax.render(w, m)?,
            Axiom::DeclareClass(ax) => ax.render(w, m)?,
            Axiom::DeclareObjectProperty(ax) => ax.render(w, m)?,
            Axiom::DeclareAnnotationProperty(ax) => ax.render(w, m)?,
            Axiom::DeclareDataProperty(ax) => ax.render(w, m)?,
            Axiom::DeclareNamedIndividual(ax) => ax.render(w, m)?,
            Axiom::DeclareDatatype(ax) => ax.render(w, m)?,
            Axiom::SubClassOf(ax) => ax.render(w, m)?,
            Axiom::EquivalentClasses(ax) => ax.render(w, m)?,
            Axiom::DisjointClasses(ax) => ax.render(w, m)?,
            Axiom::SubObjectPropertyOf(ax) => ax.render(w, m)?,
            Axiom::EquivalentObjectProperties(ax) => ax.render(w, m)?,
            Axiom::DisjointObjectProperties(ax) => ax.render(w, m)?,
            Axiom::InverseObjectProperties(ax) => ax.render(w, m)?,
            Axiom::ObjectPropertyDomain(ax) => ax.render(w, m)?,
            Axiom::ObjectPropertyRange(ax) => ax.render(w, m)?,
            Axiom::FunctionalObjectProperty(ax) => ax.render(w, m)?,
            Axiom::InverseFunctionalObjectProperty(ax) => ax.render(w, m)?,
            Axiom::ReflexiveObjectProperty(ax) => ax.render(w, m)?,
            Axiom::IrreflexiveObjectProperty(ax) => ax.render(w, m)?,
            Axiom::SymmetricObjectProperty(ax) => ax.render(w, m)?,
            Axiom::AsymmetricObjectProperty(ax) => ax.render(w, m)?,
            Axiom::TransitiveObjectProperty(ax) => ax.render(w, m)?,
            Axiom::SubDataPropertyOf(ax) => ax.render(w, m)?,
            Axiom::EquivalentDataProperties(ax) => ax.render(w, m)?,
            Axiom::DisjointDataProperties(ax) => ax.render(w, m)?,
            Axiom::DataPropertyDomain(ax) => ax.render(w, m)?,
            Axiom::DataPropertyRange(ax) => ax.render(w, m)?,
            Axiom::FunctionalDataProperty(ax) => ax.render(w, m)?,
            Axiom::DatatypeDefinition(ax) => ax.render(w, m)?,
            Axiom::HasKey(ax) => ax.render(w, m)?,
            Axiom::SameIndividual(ax) => ax.render(w, m)?,
            Axiom::DifferentIndividuals(ax) => ax.render(w, m)?,
            Axiom::ClassAssertion(ax) => ax.render(w, m)?,
            Axiom::ObjectPropertyAssertion(ax) => ax.render(w, m)?,
            Axiom::NegativeObjectPropertyAssertion(ax) => ax.render(w, m)?,
            Axiom::DataPropertyAssertion(ax) => ax.render(w, m)?,
            Axiom::NegativeDataPropertyAssertion(ax) => ax.render(w, m)?,
            Axiom::AnnotationAssertion(ax) => ax.render(w, m)?,
            Axiom::SubAnnotationPropertyOf(ax) => ax.render(w, m)?,
        }
        Ok(())
    }
}

contents!{OntologyAnnotation, self,
         (&self.0.annotation,
          &self.0.annotation_property,
          &self.0.annotation_value)
}

contents!{
    Import, self,
    String::from(&self.0)
}

render!{
    PropertyExpression, self, w, m,
    {
        match self {
            PropertyExpression::DataProperty(dp) => dp.render(w, m),
            PropertyExpression::ObjectPropertyExpression(ope) => ope.render(w, m)
        }
    }
}

contents!{HasKey, self, (&self.ce, &self.pe)}

content0!{FunctionalDataProperty}

contents!{DataPropertyRange, self, (&self.dp, &self.dr)}

contents!{DataPropertyDomain, self, (&self.dp, &self.ce)}

content0!{DisjointDataProperties}

content0!{EquivalentObjectProperties}

contents!{SubDataPropertyOf, self, (&self.super_property, &self.sub_property)}

content0!{AsymmetricObjectProperty}

content0!{SymmetricObjectProperty}

content0!{IrreflexiveObjectProperty}

content0!{ReflexiveObjectProperty}

content0!{InverseFunctionalObjectProperty}

content0!{FunctionalObjectProperty}

contents!{ObjectPropertyRange, self, (&self.ope, &self.ce)}

contents!{ObjectPropertyDomain, self, (&self.ope, &self.ce)}

content0!{DisjointObjectProperties}

content0!{EquivalentDataProperties}

content0!{SameIndividual}

content0!{DifferentIndividuals}

contents!{
    ObjectPropertyAssertion, self,
    (
        &self.ope,
        &self.from,
        &self.to
    )
}

contents!{
    NegativeObjectPropertyAssertion, self,
    (&self.ope, &self.from, &self.to)
}

contents!{
    DataPropertyAssertion, self,
    (&self.dp, &self.from, &self.to)
}

contents!{
    NegativeDataPropertyAssertion, self,
    (&self.dp, &self.from, &self.to)
}

contents!{
    ClassAssertion, self, (&self.ce, &self.i)
}

contents!{
    AnnotationAssertion, self,
        (&self.annotation.annotation,
         &self.annotation.annotation_property,
         &self.annotation_subject,
         &self.annotation.annotation_value)
}

render!{
    Literal, self, w, m,
    {
        let mut open = BytesStart::owned_name("Literal");
        attribute_maybe(&mut open, "xml:lang", &self.lang);
        attribute_maybe(
            &mut open,
            "datatypeIRI",
            &self.datatype_iri.as_ref().map(|s| s.into()),
        );
        if let Some(l) = &self.literal {
            l.within_tag(w, m, open)?;
        }

        Ok(())
    }
}

render!{
    AnnotationValue, self, w, m,
    {
        match self {
            AnnotationValue::IRI(iri) => {
                iri.render(w, m)?;
            },
            AnnotationValue::Literal(l) => {
                l.render(w, m)?;
            }
        }

        Ok(())
    }
}

render!{
    AnnotationProperty, self, w, m,
    {
        with_iri(w, m, b"AnnotationProperty", self)?;

        Ok(())
    }
}

render!{
    Annotation, self, w, m,
    {
        (&self.annotation,
         &self.annotation_property,
         &self.annotation_value).
            within(w, m, b"Annotation")?;

        Ok(())
    }
}

contents!{
    SubAnnotationPropertyOf, self,
    (&self.super_property,
     &self.sub_property)
}

contents!{
    SubClassOf, self,
    (&self.super_class,
     &self.sub_class)
}

content0!{EquivalentClasses}

content0!{DisjointClasses}

render! {
    ObjectPropertyExpression, self, w, m,
    {
        match self {
            ObjectPropertyExpression::ObjectProperty(p) => {
                p.render(w, m)?;
            }
            ObjectPropertyExpression::InverseObjectProperty(p) => {
                p.within(w, m, b"ObjectInverseOf")?;
            }
        }

        Ok(())
    }
}

render!{
    SubObjectPropertyExpression, self, w, m,
    {
        match self {
            SubObjectPropertyExpression::ObjectPropertyChain(v) => {
                v.within(w, m, b"ObjectPropertyChain")?;
            }
            SubObjectPropertyExpression::ObjectPropertyExpression(op) => {
                op.render(w, m)?;
            }
        }

        Ok(())
    }
}

contents!{
    SubObjectPropertyOf, self,
    (&self.super_property,
     &self.sub_property)
}

content0!{TransitiveObjectProperty}

contents!{
    InverseObjectProperties, self,
    (&self.0,
     &self.1)
}

contents!{
    DatatypeDefinition, self,
    (&self.kind,
     &self.range)
}

render!{
    FacetRestriction, self, w, m,
    {
        let mut open = BytesStart::owned_name("FacetRestriction");
        // Got the facet IRI from vocab
        open.push_attribute(("facet",
                             &self.f.iri_s()[..]));
        self.l.within_tag(w, m, open)?;

        Ok(())
    }
}

render!{
    DataRange, self, w, m,
    {
        match self {
            DataRange::Datatype(d) => {
                d.render(w, m)?;
            }
            DataRange::DataIntersectionOf(d) => {
                d.within(w, m, b"DataIntersectionOf")?;
            }
            DataRange::DataUnionOf(d) => {
                d.within(w, m, b"DataUnionOf")?;
            }
            DataRange::DataComplementOf(d) => {
                d.within(w, m, b"DataComplementOf")?;
            }
            DataRange::DataOneOf(d) => {
                d.within(w, m, b"DataOneOf")?;
            }
            DataRange::DatatypeRestriction(dt, fr) => {
                (dt, fr).within(w,m, b"DatatypeRestriction")?;
            }
        }

        Ok(())
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
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::BufWriter;

    fn read_ok<R: BufRead>(bufread: &mut R) -> (Ontology, PrefixMapping) {
        let r = read(bufread);
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        r.ok().unwrap()
    }

    #[test]
    fn test_ont_rt() {
        let mut ont = Ontology::new();
        let build = Build::new();

        let iri = build.iri("http://www.example.com/a".to_string());
        ont.id.iri = Some(iri);
        let temp_file = Temp::new_file().unwrap();
        let file = File::create(&temp_file).ok().unwrap();
        write(&mut BufWriter::new(file), &ont, None).ok().unwrap();

        let file = File::open(&temp_file).ok().unwrap();
        let (ont2, _) = read_ok(&mut BufReader::new(file));

        assert_eq!(ont.id.iri, ont2.id.iri);
    }

    fn roundtrip(ont: &str) -> (Ontology, PrefixMapping, Ontology, PrefixMapping) {
        let (ont_orig, prefix_orig) = read_ok(&mut ont.as_bytes());
        let mut temp_file = Temp::new_file().unwrap();

        let file = File::create(&temp_file).ok().unwrap();
        let mut buf_writer = BufWriter::new(&file);

        write(&mut buf_writer, &ont_orig, Some(&prefix_orig))
            .ok()
            .unwrap();
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
            roundtrip(include_str!("../ont/owl-xml/one-ont.owl"));

        assert_eq!(ont_orig.id.iri, ont_round.id.iri);
    }

    #[test]
    fn round_one_ont_prefix() {
        let (_ont_orig, prefix_orig, _ont_round, prefix_round) =
            roundtrip(include_str!("../ont/owl-xml/one-ont.owl"));

        let prefix_orig_map: HashMap<&String, &String> = prefix_orig.mappings().collect();

        let prefix_round_map: HashMap<&String, &String> = prefix_round.mappings().collect();

        assert_eq!(prefix_orig_map, prefix_round_map);
    }

    #[test]
    fn round_one_subclass() {
        let (ont_orig, _prefix_orig, ont_round, _prefix_round) =
            roundtrip(include_str!("../ont/owl-xml/one-subclass.owl"));

        assert_eq!(ont_orig, ont_round);
    }

    #[test]
    fn round_one_some() {
        assert_round(include_str!("../ont/owl-xml/one-some.owl"));
    }

    #[test]
    fn round_one_only() {
        assert_round(include_str!("../ont/owl-xml/one-only.owl"));
    }

    #[test]
    fn round_one_and() {
        assert_round(include_str!("../ont/owl-xml/one-and.owl"));
    }

    #[test]
    fn round_one_or() {
        assert_round(include_str!("../ont/owl-xml/one-or.owl"));
    }

    #[test]
    fn round_one_not() {
        assert_round(include_str!("../ont/owl-xml/one-not.owl"));
    }

    #[test]
    fn round_one_annotation_property() {
        assert_round(include_str!("../ont/owl-xml/one-annotation-property.owl"));
    }

    #[test]
    fn round_one_annotation() {
        assert_round(include_str!("../ont/owl-xml/one-annotation.owl"));
    }

    #[test]
    fn round_one_label() {
        assert_round(include_str!("../ont/owl-xml/one-label.owl"));
    }

    #[test]
    fn round_one_comment() {
        assert_round(include_str!("../ont/owl-xml/one-comment.owl"));
    }

    #[test]
    fn round_one_ontology_annotation() {
        assert_round(include_str!("../ont/owl-xml/one-ontology-annotation.owl"));
    }

    #[test]
    fn round_one_equivalent_class() {
        assert_round(include_str!("../ont/owl-xml/one-equivalent.owl"));
    }

    #[test]
    fn round_one_disjoint_class() {
        assert_round(include_str!("../ont/owl-xml/one-disjoint.owl"));
    }

    #[test]
    fn round_one_sub_property() {
        assert_round(include_str!("../ont/owl-xml/one-suboproperty.owl"));
    }

    #[test]
    fn round_one_inverse() {
        assert_round(include_str!("../ont/owl-xml/inverse-properties.owl"));
    }

    #[test]
    fn round_one_transitive() {
        assert_round(include_str!("../ont/owl-xml/transitive-properties.owl"));
    }

    #[test]
    fn round_one_annotated_transitive() {
        assert_round(include_str!("../ont/owl-xml/annotation-on-transitive.owl"));
    }

    #[test]
    fn round_one_subproperty_chain() {
        assert_round(include_str!("../ont/owl-xml/subproperty-chain.owl"));
    }

    #[test]
    fn round_annotation_on_annotation_assertion() {
        assert_round(include_str!(
            "../ont/owl-xml/annotation-assertion-with-annotation.owl"
        ));
    }

    #[test]
    fn round_annotation_on_annotation() {
        assert_round(include_str!(
            "../ont/owl-xml/annotation-with-annotation.owl"
        ));
    }

    #[test]
    fn round_sub_annotation() {
        assert_round(include_str!("../ont/owl-xml/sub-annotation.owl"));
    }

    #[test]
    fn round_data_property() {
        assert_round(include_str!("../ont/owl-xml/data-property.owl"));
    }

    #[test]
    fn round_named_individual() {
        assert_round(include_str!("../ont/owl-xml/named-individual.owl"));
    }

    #[test]
    fn round_import() {
        assert_round(include_str!("../ont/owl-xml/import.owl"));
    }

    #[test]
    fn datatype() {
        assert_round(include_str!("../ont/owl-xml/datatype.owl"));
    }

    #[test]
    fn object_has_value() {
        assert_round(include_str!("../ont/owl-xml/object-has-value.owl"));
    }

    #[test]
    fn object_one_of() {
        assert_round(include_str!("../ont/owl-xml/object-one-of.owl"));
    }

    #[test]
    fn inverse() {
        assert_round(include_str!("../ont/owl-xml/some-inverse.owl"));
    }

    #[test]
    fn object_unqualified_cardinality() {
        assert_round(include_str!(
            "../ont/owl-xml/object-unqualified-max-cardinality.owl"
        ));
    }

    #[test]
    fn object_min_cardinality() {
        assert_round(include_str!("../ont/owl-xml/object-min-cardinality.owl"));
    }

    #[test]
    fn object_max_cardinality() {
        assert_round(include_str!("../ont/owl-xml/object-max-cardinality.owl"));
    }

    #[test]
    fn object_exact_cardinality() {
        assert_round(include_str!("../ont/owl-xml/object-exact-cardinality.owl"));
    }

    #[test]
    fn datatype_alias() {
        assert_round(include_str!("../ont/owl-xml/datatype-alias.owl"));
    }

    #[test]
    fn datatype_intersection() {
        assert_round(include_str!("../ont/owl-xml/datatype-intersection.owl"));
    }

    #[test]
    fn datatype_union() {
        assert_round(include_str!("../ont/owl-xml/datatype-union.owl"));
    }

    #[test]
    fn datatype_complement() {
        assert_round(include_str!("../ont/owl-xml/datatype-complement.owl"));
    }

    #[test]
    fn datatype_oneof() {
        assert_round(include_str!("../ont/owl-xml/datatype-oneof.owl"));
    }

    #[test]
    fn datatype_some() {
        assert_round(include_str!("../ont/owl-xml/data-some.owl"));
    }

    #[test]
    fn facet_restriction() {
        assert_round(include_str!("../ont/owl-xml/facet-restriction.owl"));
    }

    #[test]
    fn data_only() {
        assert_round(include_str!("../ont/owl-xml/data-only.owl"));
    }
    #[test]
    fn data_exact_cardinality() {
        assert_round(include_str!("../ont/owl-xml/data-exact-cardinality.owl"));
    }

    #[test]
    fn data_has_value() {
        assert_round(include_str!("../ont/owl-xml/data-has-value.owl"));
    }

    #[test]
    fn data_max_cardinality() {
        assert_round(include_str!("../ont/owl-xml/data-max-cardinality.owl"));
    }

    #[test]
    fn data_min_cardinality() {
        assert_round(include_str!("../ont/owl-xml/data-min-cardinality.owl"));
    }

    #[test]
    fn class_assertion() {
        assert_round(include_str!("../ont/owl-xml/class-assertion.owl"));
    }

    #[test]
    fn data_property_assertion() {
        assert_round(include_str!("../ont/owl-xml/data-property-assertion.owl"));
    }

    #[test]
    fn same_individual() {
        assert_round(include_str!("../ont/owl-xml/same-individual.owl"));
    }

    #[test]
    fn different_individuals() {
        assert_round(include_str!("../ont/owl-xml/different-individual.owl"));
    }

    #[test]
    fn negative_data_property_assertion() {
        assert_round(include_str!(
            "../ont/owl-xml/negative-data-property-assertion.owl"
        ));
    }

    #[test]
    fn negative_object_property_assertion() {
        assert_round(include_str!(
            "../ont/owl-xml/negative-object-property-assertion.owl"
        ));
    }

    #[test]
    fn object_property_assertion() {
        assert_round(include_str!("../ont/owl-xml/object-property-assertion.owl"));
    }

    #[test]
    fn data_has_key() {
        assert_round(include_str!("../ont/owl-xml/data-has-key.owl"));
    }

    #[test]
    fn data_property_disjoint() {
        assert_round(include_str!("../ont/owl-xml/data-property-disjoint.owl"));
    }

    #[test]
    fn data_property_domain() {
        assert_round(include_str!("../ont/owl-xml/data-property-domain.owl"));
    }

    #[test]
    fn data_property_equivalent() {
        assert_round(include_str!("../ont/owl-xml/data-property-equivalent.owl"));
    }

    #[test]
    fn data_property_functional() {
        assert_round(include_str!("../ont/owl-xml/data-property-functional.owl"));
    }

    #[test]
    fn data_property_range() {
        assert_round(include_str!("../ont/owl-xml/data-property-range.owl"));
    }

    #[test]
    fn data_property_sub() {
        assert_round(include_str!("../ont/owl-xml/data-property-sub.owl"));
    }

    #[test]
    fn disjoint_object_properties() {
        assert_round(include_str!(
            "../ont/owl-xml/disjoint-object-properties.owl"
        ));
    }

    #[test]
    fn equivalent_object_properties() {
        assert_round(include_str!(
            "../ont/owl-xml/equivalent_object_properties.owl"
        ));
    }

    #[test]
    fn object_has_key() {
        assert_round(include_str!("../ont/owl-xml/object-has-key.owl"));
    }

    #[test]
    fn object_property_asymmetric() {
        assert_round(include_str!(
            "../ont/owl-xml/object-property-asymmetric.owl"
        ));
    }

    #[test]
    fn object_property_domain() {
        assert_round(include_str!("../ont/owl-xml/object-property-domain.owl"));
    }

    #[test]
    fn object_property_functional() {
        assert_round(include_str!(
            "../ont/owl-xml/object-property-functional.owl"
        ));
    }

    #[test]
    fn object_property_inverse_functional() {
        assert_round(include_str!(
            "../ont/owl-xml/object-property-inverse-functional.owl"
        ));
    }

    #[test]
    fn object_property_irreflexive() {
        assert_round(include_str!(
            "../ont/owl-xml/object-property-irreflexive.owl"
        ));
    }

    #[test]
    fn object_property_range() {
        assert_round(include_str!("../ont/owl-xml/object-property-range.owl"));
    }

    #[test]
    fn object_property_reflexive() {
        assert_round(include_str!("../ont/owl-xml/object-property-reflexive.owl"));
    }

    #[test]
    fn object_property_symmetric() {
        assert_round(include_str!("../ont/owl-xml/object-property-symmetric.owl"));
    }

    #[test]
    fn family() {
        assert_round(include_str!("../ont/owl-xml/family.owl"));
    }
}
