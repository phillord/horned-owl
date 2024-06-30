use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::Kinded;
use crate::model::*;
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;
use crate::vocab::Namespace::*;

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
pub fn write<A: ForIRI, AA: ForIndex<A>, W: StdWrite>(
    write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<(), HornedError> {
    let mut writer = Writer::new_with_indent(write, b' ', 4);

    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => &default_mapper,
    };

    render_ont(ont, &mut writer, mapping)?;

    Ok(())
}

/// Add an IRI to BytesStart as a element if necessary
///
/// `key` is the attribute name to use.
fn iri_maybe<A: ForIRI>(elem: &mut BytesStart, key: &str, iri: &Option<IRI<A>>) {
    match iri {
        Some(iri) => {
            elem.push_attribute((key, &(*iri)[..]));
        }
        None => {}
    }
}

fn attribute<I>(elem: &mut BytesStart, key: &str, val: I)
where
    I: Into<String>,
{
    elem.push_attribute((key, &(val.into())[..]));
}

/// Add an IRI or AbbreviatedIRI attribute to elem
fn iri_or_curie(mapping: &PrefixMapping, elem: &mut BytesStart, iri: &str) {
    match mapping.shrink_iri(&(*iri)[..]) {
        Ok(curie) => {
            let curie = format!("{}", curie);
            elem.push_attribute(("abbreviatedIRI", &curie[..]));
        }
        Err(_) => elem.push_attribute(("IRI", iri)),
    }
}

/// Write a tag with an IRI attribute.
fn with_iri<A: ForIRI, I, W>(
    w: &mut Writer<W>,
    mapping: &PrefixMapping,
    // tag: &[u8],
    tag: &str,
    into_iri: I,
) -> Result<(), HornedError>
where
    I: Into<IRI<A>>,
    W: StdWrite,
{
    let iri: IRI<_> = into_iri.into();
    // let mut bytes_start = BytesStart::borrowed(tag, tag.len());
    let mut bytes_start = BytesStart::from_content(tag, tag.len());

    let iri_string: String = iri.into();
    iri_or_curie(mapping, &mut bytes_start, &iri_string[..]);
    w.write_event(Event::Empty(bytes_start))?;

    Ok(())
}

/// Fetch the name of the tag that is used to render `ComponentKind`
fn tag_for_kind(axk: ComponentKind) -> &'static str {
    match axk {
        ComponentKind::OntologyID => {
            panic!("OntologyID found where only axioms were expected.")
        }
        ComponentKind::DocIRI => {
            panic!("DocIRI found where only axioms were expected.")
        }
        ComponentKind::Import => "Import",
        ComponentKind::OntologyAnnotation => "Annotation",
        ComponentKind::DeclareClass => "Declaration",
        ComponentKind::DeclareObjectProperty => "Declaration",
        ComponentKind::DeclareAnnotationProperty => "Declaration",
        ComponentKind::DeclareDataProperty => "Declaration",
        ComponentKind::DeclareNamedIndividual => "Declaration",
        ComponentKind::DeclareDatatype => "Declaration",
        ComponentKind::SubClassOf => "SubClassOf",
        ComponentKind::EquivalentClasses => "EquivalentClasses",
        ComponentKind::DisjointClasses => "DisjointClasses",
        ComponentKind::DisjointUnion => "DisjointUnion",
        ComponentKind::SubObjectPropertyOf => "SubObjectPropertyOf",
        ComponentKind::EquivalentObjectProperties => "EquivalentObjectProperties",
        ComponentKind::DisjointObjectProperties => "DisjointObjectProperties",
        ComponentKind::InverseObjectProperties => "InverseObjectProperties",
        ComponentKind::ObjectPropertyDomain => "ObjectPropertyDomain",
        ComponentKind::ObjectPropertyRange => "ObjectPropertyRange",
        ComponentKind::FunctionalObjectProperty => "FunctionalObjectProperty",
        ComponentKind::InverseFunctionalObjectProperty => "InverseFunctionalObjectProperty",
        ComponentKind::ReflexiveObjectProperty => "ReflexiveObjectProperty",
        ComponentKind::IrreflexiveObjectProperty => "IrreflexiveObjectProperty",
        ComponentKind::SymmetricObjectProperty => "SymmetricObjectProperty",
        ComponentKind::AsymmetricObjectProperty => "AsymmetricObjectProperty",
        ComponentKind::TransitiveObjectProperty => "TransitiveObjectProperty",
        ComponentKind::SubDataPropertyOf => "SubDataPropertyOf",
        ComponentKind::EquivalentDataProperties => "EquivalentDataProperties",
        ComponentKind::DisjointDataProperties => "DisjointDataProperties",
        ComponentKind::DataPropertyDomain => "DataPropertyDomain",
        ComponentKind::DataPropertyRange => "DataPropertyRange",
        ComponentKind::FunctionalDataProperty => "FunctionalDataProperty",
        ComponentKind::DatatypeDefinition => "DatatypeDefinition",
        ComponentKind::HasKey => "HasKey",
        ComponentKind::SameIndividual => "SameIndividual",
        ComponentKind::DifferentIndividuals => "DifferentIndividuals",
        ComponentKind::ClassAssertion => "ClassAssertion",
        ComponentKind::ObjectPropertyAssertion => "ObjectPropertyAssertion",
        ComponentKind::NegativeObjectPropertyAssertion => "NegativeObjectPropertyAssertion",
        ComponentKind::DataPropertyAssertion => "DataPropertyAssertion",
        ComponentKind::NegativeDataPropertyAssertion => "NegativeDataPropertyAssertion",
        ComponentKind::AnnotationAssertion => "AnnotationAssertion",
        ComponentKind::SubAnnotationPropertyOf => "SubAnnotationPropertyOf",
        ComponentKind::AnnotationPropertyDomain => "AnnotationPropertyDomain",
        ComponentKind::AnnotationPropertyRange => "AnnotationPropertyRange",
        ComponentKind::Rule => "DLSafeRule",
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
    fn render(&self, w: &mut Writer<W>, mapping: &'a PrefixMapping) -> Result<(), HornedError>;

    fn within(
        &self,
        w: &mut Writer<W>,
        m: &'a PrefixMapping,
        // tag: &[u8],
        tag: &str,
    ) -> Result<(), HornedError> {
        // let open = BytesStart::borrowed(tag, tag.len());
        let open = BytesStart::from_content(tag, tag.len());
        w.write_event(Event::Start(open))?;

        self.render(w, m)?;

        // w.write_event(Event::End(BytesEnd::borrowed(tag)))?;
        w.write_event(Event::End(BytesEnd::new(tag)))?;

        Ok(())
    }

    fn within_tag(
        &self,
        w: &mut Writer<W>,
        m: &'a PrefixMapping,
        open: BytesStart,
    ) -> Result<(), HornedError> {
        let clone = open.clone();
        w.write_event(Event::Start(clone))?;

        self.render(w, m)?;

        // w.write_event(Event::End(BytesEnd::borrowed(open.local_name())))?;
        w.write_event(Event::End(open.to_end()))?;

        Ok(())
    }
}

/// The types in `Render` are too long to type.
macro_rules! render {
    ($type:ident, $self:ident, $write:ident, $map:ident,
     $body:tt) => {

        impl <'a, A:ForIRI, W:StdWrite> Render<'a, W> for $type<A> {
            fn render(& $self, $write:&mut Writer<W>, $map: &'a PrefixMapping)
                      -> Result<(),HornedError>
                where W: StdWrite
                $body
        }
    }
}

macro_rules! contents {
    ($type:ident, $self:ident, $body:expr) => {
        render! {$type, $self, w, m,{
                $body.render(w, m)?;
                Ok(())
            }
        }
    };
}

macro_rules! content0 {
    ($type:ident) => {
        contents! {$type, self, &self.0}
    };
}

fn render_ont<A: ForIRI, AA: ForIndex<A>, W>(
    o: &ComponentMappedOntology<A, AA>,
    w: &mut Writer<W>,
    m: &PrefixMapping,
) -> Result<(), HornedError>
where
    W: StdWrite,
{
    // w.write_event(Event::Decl(BytesDecl::new(&b"1.0"[..], None, None)))?;
    w.write_event(Event::Decl(BytesDecl::new("1.0", None, None)))?;

    // let mut elem = BytesStart::owned_name("Ontology");
    let mut elem = BytesStart::new("Ontology");
    elem.push_attribute((b"xmlns" as &[u8], OWL.as_bytes()));

    let id = o.i().the_ontology_id_or_default();
    iri_maybe(&mut elem, "xml:base", &id.iri);

    // Render XML Namespaces.
    for pre in m.mappings() {
        elem.push_attribute((format!("xmlns:{}", pre.0).as_bytes(), pre.1.as_bytes()));
    }
    iri_maybe(&mut elem, "ontologyIRI", &id.iri);
    iri_maybe(&mut elem, "versionIRI", &id.viri);

    let elem_end = elem.to_end();
    let ev_end = Event::End(elem_end).into_owned();

    w.write_event(Event::Start(elem))?;

    // let elem = BytesEnd::owned(b"Ontology".to_vec());
    m.render(w, m)?;

    for axk in ComponentKind::all_kinds() {
        for ax in o.i().component_for_kind(axk) {
            ax.render(w, m)?;
        }
    }

    w.write_event(ev_end)?;

    Ok(())
}

// Render Impl for container and collection types
impl<'a, T: Render<'a, W>, W: StdWrite> Render<'a, W> for BTreeSet<T> {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), HornedError> {
        for item in self.iter() {
            item.render(w, m)?;
        }

        Ok(())
    }
}

impl<'a, O: Render<'a, W>, W: StdWrite> Render<'a, W> for Vec<O> {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), HornedError>
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
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), HornedError> {
        (**self).render(w, m)?;

        Ok(())
    }
}

impl<'a, A: Render<'a, W>, W: StdWrite> Render<'a, W> for (&'a A,) {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), HornedError> {
        self.0.render(w, m)?;

        Ok(())
    }
}

impl<'a, A: Render<'a, W>, B: Render<'a, W>, W: StdWrite> Render<'a, W> for (&'a A, &'a B) {
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), HornedError> {
        self.0.render(w, m)?;
        self.1.render(w, m)?;

        Ok(())
    }
}

impl<'a, A: Render<'a, W>, B: Render<'a, W>, C: Render<'a, W>, W: StdWrite> Render<'a, W>
    for (&'a A, &'a B, &'a C)
{
    fn render(&self, w: &mut Writer<W>, m: &'a PrefixMapping) -> Result<(), HornedError> {
        self.0.render(w, m)?;
        self.1.render(w, m)?;
        self.2.render(w, m)?;

        Ok(())
    }
}

impl<'a, W: StdWrite> Render<'a, W> for PrefixMapping {
    fn render(&self, w: &mut Writer<W>, _: &'a PrefixMapping) -> Result<(), HornedError> {
        for pre in self.mappings() {
            // let mut prefix = BytesStart::owned_name("Prefix");
            let mut prefix = BytesStart::new("Prefix");
            prefix.push_attribute(("name", &pre.0[..]));
            prefix.push_attribute(("IRI", &pre.1[..]));
            w.write_event(Event::Empty(prefix))?;
        }

        Ok(())
    }
}

impl<'a, W: StdWrite> Render<'a, W> for String {
    fn render(&self, w: &mut Writer<W>, _: &'a PrefixMapping) -> Result<(), HornedError> {
        // w.write_event(Event::Text(BytesText::from_plain_str(&self[..])))?;
        w.write_event(Event::Text(BytesText::new(&self[..])))?;
        Ok(())
    }
}

render! {
    IRI, self, w, m,
    {
        let iri_st: String = self.into();

        match m.shrink_iri(&iri_st[..]) {
            Ok(curie) => curie.to_string().within(w, m, "AbbreviatedIRI"),
            Err(_) => iri_st.within(w, m, "IRI"),
        }
    }
}

content0! {DeclareClass}

content0! {DeclareObjectProperty}

content0! {DeclareAnnotationProperty}

content0! {DeclareDataProperty}

content0! {DeclareNamedIndividual}

content0! {DeclareDatatype}

render! {
    AnnotatedComponent, self, w, m,
    {
        if !self.is_meta() {
            (
                (&self.ann),
                (&self.component)
            ).within(w, m,
                     tag_for_kind(self.kind()))?;
        }

        Ok(())
    }
}

render! {
    Class, self, w, m,
    {
        with_iri(w, m, "Class", self)?;

        Ok(())
    }
}

// render! {
//     &'a ObjectProperty, self, w, m,
//     {
//         with_iri(w, m, b"ObjectProperty", *self)?;

//         Ok(())
//     }
// }

render! {
    ObjectProperty, self, w, m,
    {
        with_iri(w, m, "ObjectProperty", self)?;

        Ok(())
    }
}

render! {
    DataProperty, self, w, m,
    {
        with_iri(w, m, "DataProperty", self)?;

        Ok(())
    }
}

render! {
    Datatype, self, w, m,
    {
        with_iri(w, m, "Datatype", self)?;

        Ok(())
    }
}

render! {
    Individual, self, w, m,
    {
        match self {
            Self::Anonymous(ai) => ai.render(w, m),
            Self::Named(ni) => ni.render(w, m)
        }
    }
}

render! {
    AnonymousIndividual, self, w, _m,
    {
        // let mut prefix = BytesStart::owned_name("AnonymousIndividual");
        let mut prefix = BytesStart::new("AnonymousIndividual");
        prefix.push_attribute(("nodeID", &self[..]));
        w.write_event(Event::Empty(prefix))?;
        Ok(())
    }
}

render! {
    NamedIndividual, self, w, m,
    {
        with_iri(w, m, "NamedIndividual", self)?;

        Ok(())
    }
}

render! {
    AnnotationSubject, self, w, m,
    {
        match self {
            Self::AnonymousIndividual(ai) => ai.render(w, m),
            Self::IRI(iri) => iri.render(w, m),
        }
    }
}

render! {
    ClassExpression, self, w, m,
    {
        match *self {
            ClassExpression::Class(ref c) => {
                c.render(w, m)?;
            }
            ClassExpression::ObjectSomeValuesFrom {ref ope, ref bce} => {
                (ope, bce).within(w, m, "ObjectSomeValuesFrom")?;
            }
            ClassExpression::ObjectAllValuesFrom {ref ope, ref bce} => {
                (ope, bce).within(w, m, "ObjectAllValuesFrom")?;
            }
            ClassExpression::ObjectIntersectionOf(ref ope) => {
                ope.within(w, m, "ObjectIntersectionOf")?;
            }
            ClassExpression::ObjectUnionOf (ref ope) => {
                ope.within(w, m, "ObjectUnionOf")?;
            }
            ClassExpression::ObjectComplementOf (ref bce) => {
                bce.within(w, m, "ObjectComplementOf")?;
            }
            ClassExpression::ObjectHasValue {ref ope, ref i} => {
                (ope, i).within(w, m, "ObjectHasValue")?;
            }
            ClassExpression::ObjectOneOf (ref ope) => {
                ope.within(w, m, "ObjectOneOf")?;
            }
            ClassExpression::ObjectHasSelf (ref ope) => {
                ope.within(w, m, "ObjectHasSelf")?;
            }
            ClassExpression::ObjectMinCardinality{n, ref ope, ref bce} => {
                // let mut open = BytesStart::owned_name("ObjectMinCardinality");
                let mut open = BytesStart::new("ObjectMinCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (ope, bce).within_tag(w, m, open)?;
            }
            ClassExpression::ObjectMaxCardinality{n, ref ope, ref bce} => {
                // let mut open = BytesStart::owned_name("ObjectMaxCardinality");
                let mut open = BytesStart::new("ObjectMaxCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (ope, bce).within_tag(w, m, open)?;
            }
            ClassExpression::ObjectExactCardinality{n, ref ope, ref bce} => {
                // let mut open = BytesStart::owned_name("ObjectExactCardinality");
                let mut open = BytesStart::new("ObjectExactCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (ope, bce).within_tag(w, m, open)?;
            }
            ClassExpression::DataSomeValuesFrom{ref dp, ref dr} => {
                (dp, dr).within(w, m, "DataSomeValuesFrom")?;
            }
            ClassExpression::DataAllValuesFrom{ref dp, ref dr} => {
                (dp, dr).within(w, m, "DataAllValuesFrom")?;
            }
            ClassExpression::DataHasValue{ref dp, ref l} => {
                (dp, l).within(w, m, "DataHasValue")?;
            }
            ClassExpression::DataMinCardinality{n, ref dp, ref dr} => {
                // let mut open = BytesStart::owned_name("DataMinCardinality");
                let mut open = BytesStart::new("DataMinCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (dp, dr).within_tag(w, m, open)?;
            }
            ClassExpression::DataMaxCardinality{n, ref dp, ref dr} => {
                // let mut open = BytesStart::owned_name("DataMaxCardinality");
                let mut open = BytesStart::new("DataMaxCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (dp, dr).within_tag(w, m, open)?;
            }
            ClassExpression::DataExactCardinality{n, ref dp, ref dr} => {
                // let mut open = BytesStart::owned_name("DataExactCardinality");
                let mut open = BytesStart::new("DataExactCardinality");
                open.push_attribute(("cardinality", &n.to_string()[..]));
                (dp, dr).within_tag(w, m, open)?;
            }
        }
        Ok(())
    }
}

render! {
    Component, self, w, m,
    {
        match self {
            Component::OntologyID(_) => panic!("OntologyID found where only axioms were expected"),
            Component::DocIRI(_) => panic!("DocIRI found where only axioms were expected."),
            Component::Import(ax) => ax.render(w, m)?,
            Component::OntologyAnnotation(ax) => ax.render(w, m)?,
            Component::DeclareClass(ax) => ax.render(w, m)?,
            Component::DeclareObjectProperty(ax) => ax.render(w, m)?,
            Component::DeclareAnnotationProperty(ax) => ax.render(w, m)?,
            Component::DeclareDataProperty(ax) => ax.render(w, m)?,
            Component::DeclareNamedIndividual(ax) => ax.render(w, m)?,
            Component::DeclareDatatype(ax) => ax.render(w, m)?,
            Component::SubClassOf(ax) => ax.render(w, m)?,
            Component::EquivalentClasses(ax) => ax.render(w, m)?,
            Component::DisjointClasses(ax) => ax.render(w, m)?,
            Component::DisjointUnion(ax) => ax.render(w, m)?,
            Component::SubObjectPropertyOf(ax) => ax.render(w, m)?,
            Component::EquivalentObjectProperties(ax) => ax.render(w, m)?,
            Component::DisjointObjectProperties(ax) => ax.render(w, m)?,
            Component::InverseObjectProperties(ax) => ax.render(w, m)?,
            Component::ObjectPropertyDomain(ax) => ax.render(w, m)?,
            Component::ObjectPropertyRange(ax) => ax.render(w, m)?,
            Component::FunctionalObjectProperty(ax) => ax.render(w, m)?,
            Component::InverseFunctionalObjectProperty(ax) => ax.render(w, m)?,
            Component::ReflexiveObjectProperty(ax) => ax.render(w, m)?,
            Component::IrreflexiveObjectProperty(ax) => ax.render(w, m)?,
            Component::SymmetricObjectProperty(ax) => ax.render(w, m)?,
            Component::AsymmetricObjectProperty(ax) => ax.render(w, m)?,
            Component::TransitiveObjectProperty(ax) => ax.render(w, m)?,
            Component::SubDataPropertyOf(ax) => ax.render(w, m)?,
            Component::EquivalentDataProperties(ax) => ax.render(w, m)?,
            Component::DisjointDataProperties(ax) => ax.render(w, m)?,
            Component::DataPropertyDomain(ax) => ax.render(w, m)?,
            Component::DataPropertyRange(ax) => ax.render(w, m)?,
            Component::FunctionalDataProperty(ax) => ax.render(w, m)?,
            Component::DatatypeDefinition(ax) => ax.render(w, m)?,
            Component::HasKey(ax) => ax.render(w, m)?,
            Component::SameIndividual(ax) => ax.render(w, m)?,
            Component::DifferentIndividuals(ax) => ax.render(w, m)?,
            Component::ClassAssertion(ax) => ax.render(w, m)?,
            Component::ObjectPropertyAssertion(ax) => ax.render(w, m)?,
            Component::NegativeObjectPropertyAssertion(ax) => ax.render(w, m)?,
            Component::DataPropertyAssertion(ax) => ax.render(w, m)?,
            Component::NegativeDataPropertyAssertion(ax) => ax.render(w, m)?,
            Component::AnnotationAssertion(ax) => ax.render(w, m)?,
            Component::SubAnnotationPropertyOf(ax) => ax.render(w, m)?,
            Component::AnnotationPropertyDomain(ax) => ax.render(w, m)?,
            Component::AnnotationPropertyRange(ax) => ax.render(w, m)?,
            Component::Rule(ax) => ax.render(w, m)?,
        }
        Ok(())
    }
}

contents! {OntologyAnnotation, self, (&self.0.ap, &self.0.av)}

contents! {Import, self, String::from(&self.0)}

render! {
    PropertyExpression, self, w, m,
    {
        match self {
            PropertyExpression::AnnotationProperty(ap) => ap.render(w, m),
            PropertyExpression::DataProperty(dp) => dp.render(w, m),
            PropertyExpression::ObjectPropertyExpression(ope) => ope.render(w, m)
        }
    }
}

contents! {HasKey, self, (&self.ce, &self.vpe)}

content0! {FunctionalDataProperty}

contents! {DataPropertyRange, self, (&self.dp, &self.dr)}

contents! {DataPropertyDomain, self, (&self.dp, &self.ce)}

content0! {DisjointDataProperties}

content0! {EquivalentObjectProperties}

contents! {SubDataPropertyOf, self, (&self.sub, &self.sup)}

content0! {AsymmetricObjectProperty}

content0! {SymmetricObjectProperty}

content0! {IrreflexiveObjectProperty}

content0! {ReflexiveObjectProperty}

content0! {InverseFunctionalObjectProperty}

content0! {FunctionalObjectProperty}

contents! {ObjectPropertyRange, self, (&self.ope, &self.ce)}

contents! {ObjectPropertyDomain, self, (&self.ope, &self.ce)}

content0! {DisjointObjectProperties}

content0! {EquivalentDataProperties}

content0! {SameIndividual}

content0! {DifferentIndividuals}

contents! {
    ObjectPropertyAssertion, self,
    (
        &self.ope,
        &self.from,
        &self.to
    )
}

contents! {
    NegativeObjectPropertyAssertion, self,
    (&self.ope, &self.from, &self.to)
}

contents! {
    DataPropertyAssertion, self,
    (&self.dp, &self.from, &self.to)
}

contents! {
    NegativeDataPropertyAssertion, self,
    (&self.dp, &self.from, &self.to)
}

contents! {
    ClassAssertion, self, (&self.ce, &self.i)
}

contents! {
    AnnotationAssertion, self,
    (&self.ann.ap,
     &self.subject,
     &self.ann.av)
}

render! {
    Rule, self, w, m,
    {
        let body = BytesStart::new("Body");
        let head = BytesStart::new("Head");

        self.body.within_tag(w, m, body)?;
        self.head.within_tag(w, m, head)
    }
}

render! {
    Atom, self, w, m,
    {
        match self {
            Atom::BuiltInAtom{pred, args} => {
                let mut open = BytesStart::new("BuiltInAtom");
                open.push_attribute(("IRI", pred.0.borrow()));
                args.within_tag(w, m, open)?;
            }
            Atom::ClassAtom{pred, arg} => {
                (pred, arg).within(w, m, "ClassAtom")?;
            }
            Atom::DataPropertyAtom{pred, args} => {
                (pred, &args.0, &args.1).within(w, m, "DataPropertyAtom")?;
            }
            Atom::DataRangeAtom{pred, arg} => {
                (pred, arg).within(w, m, "DataRangeAtom")?;
            }
            Atom::DifferentIndividualsAtom(arg1, arg2) => {
                (arg1, arg2).within(w, m, "DifferentIndividualsAtom")?;
            }
            Atom::ObjectPropertyAtom{pred, args} => {
                (pred, &args.0, &args.1).within(w, m, "ObjectPropertyAtom")?;
            }
            Atom::SameIndividualAtom(arg1, arg2) => {
                (arg1, arg2).within(w, m, "SameIndividualAtom")?;
            }
        }
        Ok(())
    }
}

render! {
    DArgument, self, w, m,
    {
        match self {
            Self::Literal(l) => l.render(w, m)?,
            Self::Variable(v) => v.render(w, m)?
        }
        Ok(())
    }
}

render! {
    IArgument, self, w, m,
    {
        match self {
            Self::Individual(i) => i.render(w, m)?,
            Self::Variable(v) => v.render(w, m)?
        }
        Ok(())
    }
}

render! {
    Variable, self, w, m,
    {
        with_iri(w, m, "Variable", self)
    }
}

render! {
    Literal, self, w, m,
    {
        // let mut open = BytesStart::owned_name("Literal");
        let mut open = BytesStart::new("Literal");

        match self {
            Literal::Datatype{literal, datatype_iri} => {
                attribute(
                    &mut open,
                    "datatypeIRI",
                    datatype_iri,
                );
                literal
            }
            Literal::Language{literal, lang} => {
                attribute(&mut open, "xml:lang", lang);
                literal
            }
            Literal::Simple{literal} => literal

        }.within_tag(w, m, open)?;

        Ok(())
    }
}

render! {
    AnnotationValue, self, w, m,
    {
        match self {
            AnnotationValue::IRI(iri) => {
                iri.render(w, m)?;
            },
            AnnotationValue::Literal(l) => {
                l.render(w, m)?;
            }
            AnnotationValue::AnonymousIndividual(a) => {
                a.render(w, m)?;
            }
        }

        Ok(())
    }
}

render! {
    AnnotationProperty, self, w, m,
    {
        with_iri(w, m, "AnnotationProperty", self)?;

        Ok(())
    }
}

render! {
    Annotation, self, w, m,
    {
        (&self.ap, &self.av).within(w, m, "Annotation")?;

        Ok(())
    }
}

contents! {
    SubAnnotationPropertyOf, self,
    (&self.sub,
     &self.sup)
}

contents! {
    AnnotationPropertyDomain, self, (&self.ap, &self.iri)
}

contents! {
    AnnotationPropertyRange, self, (&self.ap, &self.iri)
}

contents! {
    SubClassOf, self,
    (&self.sub, &self.sup)
}

content0! {EquivalentClasses}

content0! {DisjointClasses}

render! {
    DisjointUnion, self, w, m,
    {
        self.0.render(w, m)?;
        self.1.render(w, m)?;

        Ok(())
    }
}

render! {
    ObjectPropertyExpression, self, w, m,
    {
        match self {
            ObjectPropertyExpression::ObjectProperty(p) => {
                p.render(w, m)?;
            }
            ObjectPropertyExpression::InverseObjectProperty(p) => {
                p.within(w, m, "ObjectInverseOf")?;
            }
        }

        Ok(())
    }
}

render! {
    SubObjectPropertyExpression, self, w, m,
    {
        match self {
            SubObjectPropertyExpression::ObjectPropertyChain(v) => {
                v.within(w, m, "ObjectPropertyChain")?;
            }
            SubObjectPropertyExpression::ObjectPropertyExpression(op) => {
                op.render(w, m)?;
            }
        }

        Ok(())
    }
}

contents! {
    SubObjectPropertyOf, self,
    (&self.sub, &self.sup)
}

content0! {TransitiveObjectProperty}

contents! {
    InverseObjectProperties, self,
    (&self.0,
     &self.1)
}

contents! {
    DatatypeDefinition, self,
    (&self.kind,
     &self.range)
}

render! {
    FacetRestriction, self, w, m,
    {
        // let mut open = BytesStart::owned_name("FacetRestriction");
        let mut open = BytesStart::new("FacetRestriction");
        // Got the facet IRI from vocab
        open.push_attribute(("facet",
                             self.f.as_ref()));
        self.l.within_tag(w, m, open)?;

        Ok(())
    }
}

render! {
    DataRange, self, w, m,
    {
        match self {
            DataRange::Datatype(d) => {
                d.render(w, m)?;
            }
            DataRange::DataIntersectionOf(d) => {
                d.within(w, m, "DataIntersectionOf")?;
            }
            DataRange::DataUnionOf(d) => {
                d.within(w, m, "DataUnionOf")?;
            }
            DataRange::DataComplementOf(d) => {
                d.within(w, m, "DataComplementOf")?;
            }
            DataRange::DataOneOf(d) => {
                d.within(w, m, "DataOneOf")?;
            }
            DataRange::DatatypeRestriction(dt, fr) => {
                (dt, fr).within(w,m, "DatatypeRestriction")?;
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
    use crate::io::owx::reader::*;
    use crate::io::ParserConfiguration;

    use std::collections::HashMap;

    use crate::ontology::component_mapped::RcComponentMappedOntology;
    use std::fs::File;
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::BufWriter;

    use test_generator::test_resources;

    fn read_ok<R: BufRead>(bufread: &mut R) -> (RcComponentMappedOntology, PrefixMapping) {
        let r = read(bufread, ParserConfiguration::default());
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        let (o, m) = r.ok().unwrap();
        (o, m)
    }

    #[test]
    fn test_ont_rt() {
        let mut ont = ComponentMappedOntology::new_rc();
        let build = Build::new();

        let iri = build.iri("http://www.example.com/a".to_string());
        ont.insert(OntologyID {
            iri: Some(iri.clone()),
            viri: None,
        });
        let temp_file = Temp::new_file().unwrap();
        let file = File::create(&temp_file).ok().unwrap();
        write(&mut BufWriter::new(file), &ont, None).ok().unwrap();

        let file = File::open(&temp_file).ok().unwrap();
        let (ont2, _) = read_ok(&mut BufReader::new(file));

        // Check ID is present and not default
        assert!(ont.i().the_ontology_id().is_some());

        // Check IDs are identical
        assert_eq!(
            ont.i().the_ontology_id_or_default().iri,
            ont2.i().the_ontology_id_or_default().iri
        );
    }

    fn roundtrip_1(ont: &str) -> (RcComponentMappedOntology, PrefixMapping, Temp) {
        let (ont_orig, prefix_orig) = read_ok(&mut ont.as_bytes());
        let temp_file = Temp::new_file().unwrap();

        let file = File::create(&temp_file).ok().unwrap();
        let mut buf_writer = BufWriter::new(&file);

        write(&mut buf_writer, &ont_orig, Some(&prefix_orig))
            .ok()
            .unwrap();
        buf_writer.flush().ok();

        (ont_orig, prefix_orig, temp_file)
    }

    fn roundtrip_to_string(ont: &str) -> String {
        let t = roundtrip_1(ont).2;
        let s = std::fs::read_to_string(&t);
        t.release();
        s.ok().unwrap()
    }

    fn roundtrip(
        ont: &str,
    ) -> (
        RcComponentMappedOntology,
        PrefixMapping,
        RcComponentMappedOntology,
        PrefixMapping,
    ) {
        let (ont_orig, prefix_orig, temp_file) = roundtrip_1(ont);

        let file = File::open(&temp_file).ok().unwrap();

        let (ont_round, prefix_round) = read_ok(&mut BufReader::new(&file));
        temp_file.release();

        (ont_orig, prefix_orig, ont_round, prefix_round)
    }

    fn assert_round(
        ont: &str,
    ) -> (
        RcComponentMappedOntology,
        PrefixMapping,
        RcComponentMappedOntology,
        PrefixMapping,
    ) {
        let (ont_orig, prefix_orig, ont_round, prefix_round) = roundtrip(ont);

        assert_eq!(ont_orig, ont_round);

        {
            let prefix_orig_map: &HashMap<&String, &String> = &prefix_orig.mappings().collect();
            let prefix_round_map: &HashMap<&String, &String> = &prefix_round.mappings().collect();

            assert_eq!(prefix_orig_map, prefix_round_map);
        }
        (ont_orig, prefix_orig, ont_round, prefix_round)
    }

    #[test]
    fn test_namespaces() {
        let s = roundtrip_to_string(include_str!("../../ont/owl-xml/ont.owx"));

        assert!(s.contains("xmlns:xsd"));
    }

    #[test]
    fn round_one_ont() {
        let (ont_orig, _prefix_orig, ont_round, _prefix_round) =
            roundtrip(include_str!("../../ont/owl-xml/ont.owx"));

        assert_eq!(
            ont_orig.i().the_ontology_id(),
            ont_round.i().the_ontology_id()
        );
    }

    #[test]
    fn round_one_ont_prefix() {
        let (_ont_orig, prefix_orig, _ont_round, prefix_round) =
            roundtrip(include_str!("../../ont/owl-xml/ont.owx"));

        let prefix_orig_map: HashMap<&String, &String> = prefix_orig.mappings().collect();

        let prefix_round_map: HashMap<&String, &String> = prefix_round.mappings().collect();

        assert_eq!(prefix_orig_map, prefix_round_map);
    }

    #[test_resources("src/ont/owl-xml/*.owx")]
    fn roundtrip_resource(resource: &str) {
        let resource = &slurp::read_all_to_string(resource).unwrap();

        let (ont_orig, _prefix_orig, ont_round, _prefix_round) = roundtrip(resource);

        dbg!(&ont_orig);
        dbg!(&ont_round);
        assert_eq!(ont_orig, ont_round);
    }

    #[test_resources("src/ont/owl-xml/ambiguous/*.owx")]
    fn roundtrip_nonround_resource(resource: &str) {
        let resource = &slurp::read_all_to_string(resource).unwrap();

        assert_round(resource);
    }

    #[test]
    fn annotation_contain_iris() {
        let s = include_str!("../../ont/owl-xml/annotation.owx");

        // https://github.com/phillord/horned-owl/pull/32
        // Check that we are serializing IRIs and not NamedIndividuals
        let s = roundtrip_to_string(s);
        assert!(!s.contains("<NamedIndividual"));
        // I still do not understand IRI vs AbbreviatedIRI
        assert!(s.contains("<AbbreviatedIRI>"));
    }

    #[test]
    fn family() {
        assert_round(include_str!("../../ont/owl-xml/manual/family.owx"));
    }
}
