use std::{collections::{BTreeSet, HashSet}, fmt::Debug, io::Write, rc::Rc};

use failure::Error;
use rio_xml::chunked_formatter::{AsRefBlankNode, AsRefLiteral, AsRefNamedNode, AsRefNamedOrBlankNode, AsRefTerm, AsRefTriple, ChunkedRdfXmlFormatterConfig, PrettyRdfXmlFormatter};
use crate::{model::*, ontology::axiom_mapped::AxiomMappedOntology, vocab::{OWL, RDF, RDFS, WithIRI, Vocab}};

pub fn write<W:Write>(
    write: &mut W,
    ont: &AxiomMappedOntology,
) -> Result<(), Error> {
    // Entirely unsatisfying to set this randomly here, but we can't
    // access ns our parser yet
    let mut p = indexmap::IndexMap::new();
    p.insert("http://www.w3.org/2002/07/owl#".to_string(),"owl".to_string());
    p.insert("http://www.w3.org/XML/1998/namespace".to_string(),"xml".to_string());
    p.insert("http://www.w3.org/2001/XMLSchema#".to_string(),"xsd".to_string());
    p.insert("http://www.w3.org/2000/01/rdf-schema#".to_string(), "rdfs".to_string());

    let mut f:PrettyRdfXmlFormatter<Rc<str>,_> = PrettyRdfXmlFormatter::new(
        write,
        ChunkedRdfXmlFormatterConfig::all().prefix(p)
    )?;
    let mut bng = NodeGenerator::default();
    ont.render(&mut f, &mut bng)?;
    f.finish()?;
    Ok(())
}

#[derive(Default)]
struct NodeGenerator {
    i: u64,
    b: HashSet<Rc<str>>,
    this_bn: Option<AsRefNamedOrBlankNode<Rc<str>>>
}


impl NodeGenerator {
    pub fn nn<V:Into<Vocab>>(&mut self, v:V) -> AsRefNamedNode<Rc<str>> {
        AsRefNamedNode::new(self.cache_rc(v)).into()
    }

    fn cache_rc<V:Into<Vocab>>(&mut self, v:V) -> Rc<str> {
        let voc:&str = v.into().iri_s();
        if let Some(rc) = self.b.get(voc) {
            return rc.clone();
        }

        let rc:Rc<str> = (*voc).into();
        self.b.insert(rc.clone());
        rc
    }

    pub fn bn(&mut self) -> AsRefNamedOrBlankNode<Rc<str>> {
        self.i += 1;
        AsRefNamedOrBlankNode::BlankNode(
            AsRefBlankNode{
                id: format!{"bn{}", self.i}.into()
            }
        )
    }

    pub fn keep_this_bn(&mut self, bn:AsRefNamedOrBlankNode<Rc<str>>) {
        self.this_bn = Some(bn);
    }

    pub fn this_bn(&mut self) -> Option<AsRefNamedOrBlankNode<Rc<str>>> {
        self.this_bn.take()
    }
}

impl From<&IRI> for AsRefTerm<Rc<str>> {
    fn from(iri: &IRI) -> Self {
        AsRefNamedNode::new(iri.into()).into()
    }
}

impl From<&IRI> for AsRefNamedNode<Rc<str>> {
    fn from(iri: &IRI) -> Self {
        AsRefNamedNode::new(iri.into())
    }
}

impl From<&IRI> for AsRefNamedOrBlankNode<Rc<str>> {
    fn from(iri: &IRI) -> Self {
        let nn:AsRefNamedNode<Rc<str>> = iri.into();
        nn.into()
    }
}


trait Render<R> {
    fn render<W:Write>(&self, f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                       ng: &mut NodeGenerator) ->
        Result<R, Error>;
}

enum Annotatable<A:AsRef<str>> {
    Main(AsRefTriple<A>),
    Multiple(Vec<AsRefTriple<A>>),
    Blank(AsRefBlankNode<A>)
}

impl From<AsRefTriple<Rc<str>>> for Annotatable<Rc<str>> {
    fn from(t: AsRefTriple<Rc<str>>) -> Self {
        Self::Main(t)
    }
}

impl From<Vec<AsRefTriple<Rc<str>>>> for Annotatable<Rc<str>> {
    fn from(t: Vec<AsRefTriple<Rc<str>>>) -> Self {
        Self::Multiple(t)
    }
}


/// The types in `Render` are too long to type.
macro_rules! render {
    ($type:ty, $self:ident, $f:ident, $ng:ident, $return:ty,
     $body:tt) => {
        impl Render<$return> for $type {
            fn render<W:Write>(& $self, $f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                               $ng: &mut NodeGenerator)
                               -> Result<$return, Error>
                $body
        }
    }
}

macro_rules! render_to_node {
    ($type:ty, $self:ident, $f:ident, $ng:ident,
     $body:tt) => {
        render!{$type,$self, $f, $ng, AsRefNamedOrBlankNode<Rc<str>>, $body}
    }
}

macro_rules! render_to_vec {
    ($type:ty, $self:ident, $f:ident, $ng:ident,
     $body:tt) => {
        render!{$type, $self, $f, $ng, Vec<AsRefTriple<Rc<str>>>, $body}
    }
}

macro_rules! render_triple {
    ($type:ty, $self:ident, $ng:ident, $sub:expr, $pred:expr, $ob:expr) => {
        render! {
            $type, $self, f, $ng, AsRefTriple<Rc<str>>,
            {
                Ok(triple!(f, $sub, $pred, $ob))
            }
        }
    }
}

macro_rules! triple {
    ($f:ident, $sub:expr, $pred:expr, $ob:expr) => {
        {
            let t = to_triple($sub, $pred, $ob);
            $f.format(t.clone())?;
            t
        }
    }
}

macro_rules! triples {
    ($f:ident) => {};
    ($f:ident, $sub:expr, $pred:expr, $ob:expr) => {
        $f.format(to_triple(
                 $sub, $pred, $ob
                ))?;
     };
    ($f:ident, $sub:expr, $pred:expr, $ob:expr, $($rest:expr),+) => {
        triples!($f, $sub, $pred, $ob);
        triples!($f, $($rest),*);
    }
}

macro_rules! triples_to_node {
    ($f:ident) => {};
    ($f:ident, $sub:expr, $pred:expr, $ob:expr) => {
        {
            let s = $sub;

            $f.format(to_triple(
                s.clone(), $pred, $ob
            ))?;

            s
        }
    };
    ($f:ident, $sub:expr, $pred:expr, $ob:expr, $($rest:expr),+) => {
        {
            let s = triples_to_node!($f, $sub, $pred, $ob);
            triples!($f, $($rest),*);

            s
        }
    }
}

macro_rules! triples_to_vec {
    ($f:ident) => {};
    ($f:ident, $sub:expr, $pred:expr, $ob:expr) => {
        {
            let t = to_triple($sub, $pred, $ob);
            $f.format(t.clone())?;

            vec![t]
        }
     };
    ($f:ident, $sub:expr, $pred:expr, $ob:expr, $($rest:expr),+) => {
        {
            let mut v = triples_to_vec!($f, $sub, $pred, $ob);
            v.extend(triples_to_vec!($f, $($rest),*));
            v
        }
    }
}


fn to_triple<'a, NB, NN, T>(subject:NB, predicate:NN, object:T) -> AsRefTriple<Rc<str>>
where NB: Into<AsRefNamedOrBlankNode<Rc<str>>>,
      NN: Into<AsRefNamedNode<Rc<str>>>,
      T: Into<AsRefTerm<Rc<str>>>
{
    AsRefTriple{
        subject: subject.into(),
        predicate: predicate.into(),
        object: object.into(),
    }
}


impl<T: Debug + Render<AsRefNamedOrBlankNode<Rc<str>>>> Render<AsRefNamedOrBlankNode<Rc<str>>> for &Vec<T> {
    fn render<W:Write>(&self, f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                       ng: &mut NodeGenerator)
                       -> Result<AsRefNamedOrBlankNode<Rc<str>>, Error> {
        let mut rest:Option<AsRefNamedOrBlankNode<Rc<str>>> = None;
        for i in self.iter().rev() {
            let bn = &ng.bn();
            let item = i.render(f, ng)?;

            triples!(
                f, bn.clone(), ng.nn(RDF::First), item
            );

            if let Some(r) = rest.take() {
                triples!(
                    f, bn.clone(), ng.nn(RDF::Rest), r
                );
            } else {
                triples!(
                    f, bn.clone(), ng.nn(RDF::Rest), ng.nn(RDF::Nil)
                );
            }
            rest = Some(bn.clone())
        }
        // Panic if Vec is zero length!
        Ok(rest.unwrap())
    }
}

impl Render<()> for BTreeSet<Annotation> {
    fn render<W:Write>(&self, f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                       ng: &mut NodeGenerator)
                       -> Result<(), Error>
    {
        for r in self.iter() {
            r.render(f, ng)?;
        }
        Ok(())
    }
}


render! {
    &AxiomMappedOntology, self, f, ng, (),
    {
        if let Some(iri) = &self.id().iri {
            triples!(
                f,
                iri,
                ng.nn(RDF::Type),
                ng.nn(OWL::Ontology)
            );

            if let Some(viri) = &self.id().viri {
                triples!(
                    f,
                    iri,
                    ng.nn(OWL::VersionIRI),
                    viri
                );
            }

            let imp = self.i().import();
            for i in imp {
                triples!(
                    f,
                    iri,
                    ng.nn(OWL::Imports),
                    &i.0
                );
            }
            let oa = self.i().ontology_annotation();
            ng.keep_this_bn(iri.into());
            for a in oa {
                a.0.render(f, ng)?;
            }

            for ax in self.i().iter() {
                ax.render(f, ng)?;
            }
        }
        Ok(())
    }
}

impl Render<()> for AnnotatedAxiom {
    fn render<W:Write>(&self, f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                       ng: &mut NodeGenerator)
                           -> Result<(), Error> {
        let ax:Annotatable<Rc<str>> = self.axiom.render(f, ng)?;
        Ok(
            if self.ann.len() != 0 {
                match ax {
                    Annotatable::Main(t) => {
                        let bn = ng.bn();
                        triples! (f,
                                  bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Axiom),
                                  bn.clone(), ng.nn(OWL::AnnotatedSource), t.subject,
                                  bn.clone(), ng.nn(OWL::AnnotatedProperty), t.predicate,
                                  bn.clone(), ng.nn(OWL::AnnotatedTarget), t.object
                                  // And the rest!
                        );

                        ng.keep_this_bn(bn.clone());

                        &self.ann.render(f, ng);
                    }
                    _ => {
                        ()
                        // todo!()
                    }
                }
            }
        )
    }
}


render! {
    Annotation, self, f, ng, AsRefTriple<Rc<str>>,
    {
        let bn = ng.this_bn().ok_or_else(|| format_err!("No bnode available"))?;

        Ok(
            match &self.av {
                AnnotationValue::Literal(l) => {
                    let obj:AsRefTerm<Rc<str>> =
                        match l {
                            Literal::Simple{literal} =>
                                AsRefTerm::Literal(AsRefLiteral::Simple{value:literal.clone().into()}),
                            Literal::Language{literal, lang} =>
                                AsRefTerm::Literal(AsRefLiteral::LanguageTaggedString{value:literal.clone().into(),
                                                                                      language: lang.clone().into()}),
                            Literal::Datatype{literal, datatype_iri} =>
                                AsRefTerm::Literal(AsRefLiteral::Typed{value:literal.clone().into(),
                                                                       datatype:datatype_iri.into()})
                        };

                    triple!(f, bn, &self.ap.0, obj)
                }
                AnnotationValue::IRI(iri) => {
                    triple!(
                        f, bn, &self.ap.0, iri
                    )
                }
            }
        )
    }
}


render!{
    AnnotationAssertion, self, f, ng, AsRefTriple<Rc<str>>,
    {
        let nbn:AsRefNamedOrBlankNode<Rc<str>> = (&self.subject).into();
        ng.keep_this_bn(nbn);

        self.ann.render(f, ng)
    }
}


render! {
    AnnotationPropertyDomain, self, f, ng, AsRefTriple<Rc<str>>,
    {
        Ok(
            triple!(f, &self.ap.0, ng.nn(RDFS::Domain), &self.iri)
        )
    }
}

render! {
    AnnotationPropertyRange, self, f, ng, AsRefTriple<Rc<str>>,
    {
        Ok(
            triple!(f, &self.ap.0, ng.nn(RDFS::Range), &self.iri)
        )
    }
}

impl Render<Annotatable<Rc<str>>> for Axiom {
        fn render<W:Write>(&self, f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                           ng: &mut NodeGenerator)
                           -> Result<Annotatable<Rc<str>>, Error>
    {
        Ok(
            match self {
                // We render imports and ontology annotations earlier
                Axiom::Import(_ax) => vec![].into(),
                Axiom::OntologyAnnotation(ax) => vec![].into(),
                Axiom::DeclareClass(ax) => ax.render(f, ng)?.into(),
                Axiom::DeclareObjectProperty(ax) => ax.render(f, ng)?.into(),
                Axiom::DeclareAnnotationProperty(ax) => ax.render(f, ng)?.into(),
                Axiom::DeclareDataProperty(ax) => ax.render(f, ng)?.into(),
                Axiom::DeclareNamedIndividual(ax) => ax.render(f, ng)?.into(),
                Axiom::DeclareDatatype(ax) => ax.render(f, ng)?.into(),
                Axiom::SubClassOf(ax) => ax.render(f, ng)?.into(),
                // Axiom::EquivalentClasses(ax) => ax.render(f, ng)?.into(),
                // Axiom::DisjointClasses(ax) => ax.render(f, ng)?.into(),
                // Axiom::DisjointUnion(ax) => ax.render(f, ng)?.into(),
                // Axiom::SubObjectPropertyOf(ax) => ax.render(f, ng)?.into(),
                // Axiom::EquivalentObjectProperties(ax) => ax.render(f, ng)?.into(),
                // Axiom::DisjointObjectProperties(ax) => ax.render(f, ng)?.into(),
                // Axiom::InverseObjectProperties(ax) => ax.render(f, ng)?.into(),
                // Axiom::ObjectPropertyDomain(ax) => ax.render(f, ng)?.into(),
                // Axiom::ObjectPropertyRange(ax) => ax.render(f, ng)?.into(),
                // Axiom::FunctionalObjectProperty(ax) => ax.render(f, ng)?.into(),
                // Axiom::InverseFunctionalObjectProperty(ax) => ax.render(f, ng)?.into(),
                // Axiom::ReflexiveObjectProperty(ax) => ax.render(f, ng)?.into(),
                // Axiom::IrreflexiveObjectProperty(ax) => ax.render(f, ng)?.into(),
                // Axiom::SymmetricObjectProperty(ax) => ax.render(f, ng)?.into(),
                // Axiom::AsymmetricObjectProperty(ax) => ax.render(f, ng)?.into(),
                // Axiom::TransitiveObjectProperty(ax) => ax.render(f, ng)?.into(),
                // Axiom::SubDataPropertyOf(ax) => ax.render(f, ng)?.into(),
                // Axiom::EquivalentDataProperties(ax) => ax.render(f, ng)?.into(),
                // Axiom::DisjointDataProperties(ax) => ax.render(f, ng)?.into(),
                // Axiom::DataPropertyDomain(ax) => ax.render(f, ng)?.into(),
                // Axiom::DataPropertyRange(ax) => ax.render(f, ng)?.into(),
                // Axiom::FunctionalDataProperty(ax) => ax.render(f, ng)?.into(),
                // Axiom::DatatypeDefinition(ax) => ax.render(f, ng)?.into(),
                // Axiom::HasKey(ax) => ax.render(f, ng)?.into(),
                // Axiom::SameIndividual(ax) => ax.render(f, ng)?.into(),
                // Axiom::DifferentIndividuals(ax) => ax.render(f, ng)?.into(),
                // Axiom::ClassAssertion(ax) => ax.render(f, ng)?.into(),
                // Axiom::ObjectPropertyAssertion(ax) => ax.render(f, ng)?.into(),
                // Axiom::NegativeObjectPropertyAssertion(ax) => ax.render(f, ng)?.into(),
                // Axiom::DataPropertyAssertion(ax) => ax.render(f, ng)?.into(),
                // Axiom::NegativeDataPropertyAssertion(ax) => ax.render(f, ng)?.into(),
                Axiom::AnnotationAssertion(ax) => ax.render(f, ng)?.into(),
                // Axiom::SubAnnotationPropertyOf(ax) => ax.render(f, ng)?.into(),
                Axiom::AnnotationPropertyDomain(ax) => ax.render(f, ng)?.into(),
                Axiom::AnnotationPropertyRange(ax) => ax.render(f, ng)?.into(),
                _ => todo!("TODO: {:?}", self)
            }
        )
    }
}

render_to_node! {
    ClassExpression, self, f, ng,
    {
        Ok(
            match self {
                ClassExpression::Class(cl) => (&cl.0).into(),
                ClassExpression::ObjectIntersectionOf(v)=>{
                    let bn = ng.bn();
                    let node_seq = v.render(f, ng)?;

                    triples_to_node!(
                         f,
                         bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Class),
                         bn.clone(), ng.nn(OWL::IntersectionOf), node_seq
                    )
                }
                ClassExpression::ObjectUnionOf(v) => {
                    let bn = ng.bn();
                    let node_seq = v.render(f, ng)?;

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Class),
                        bn.clone(), ng.nn(OWL::UnionOf), node_seq
                    )
                }
                ClassExpression::ObjectComplementOf(bce) => {
                    let bn = ng.bn();

                    let node_ce = (*bce).render(f, ng)?;

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Class),
                        bn, ng.nn(OWL::ComplementOf), node_ce
                    )
                },
                ClassExpression::ObjectOneOf(_) => todo!(),
                ClassExpression::ObjectSomeValuesFrom{ref bce, ref ope} => {
                    let bn = ng.bn();
                    let node_ce = bce.render(f, ng)?;
                    let node_ope = ope.render(f, ng)?;

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_ope,
                        bn.clone(), ng.nn(OWL::SomeValuesFrom), node_ce
                    )
                }
                ClassExpression::ObjectAllValuesFrom{ope, bce} => {
                    let bn = ng.bn();
                    let node_ce = bce.render(f, ng)?;
                    let node_ope = ope.render(f, ng)?;

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_ope,
                        bn.clone(), ng.nn(OWL::AllValuesFrom), node_ce
                    )
                }
                ClassExpression::ObjectHasValue{ope:_ope, i:_i} => todo!(),
                ClassExpression::ObjectHasSelf(_ope) =>todo!(),
                ClassExpression::ObjectMinCardinality{n:_n, ope:_ope, bce:_bce} =>todo!(),
                ClassExpression::ObjectMaxCardinality{n:_n, ope:_ope, bce:_bce} =>todo!(),
                ClassExpression::ObjectExactCardinality{n:_n, ope:_ope, bce:_bce} =>todo!(),
                ClassExpression::DataSomeValuesFrom{dp:_dp, dr:_dr} => todo!(),
                ClassExpression::DataAllValuesFrom{dp:_dp, dr:_dr} => todo!(),
                ClassExpression::DataHasValue{dp:_dp, l:_l}=>todo!(),
                ClassExpression::DataMinCardinality{n:_n,dp:_dp,dr:_dr} => todo!(),
                ClassExpression::DataMaxCardinality{n:_n,dp:_dp,dr:_dr} => todo!(),
                ClassExpression::DataExactCardinality{n:_n,dp:_dp,dr:_dr} => todo!(),
            }
        )
    }
}

render_to_node! {
    ObjectPropertyExpression, self, _f, _ng,
    {
        Ok(
            match self {
                ObjectPropertyExpression::ObjectProperty(op)
                    => (&op.0).into(),
                ObjectPropertyExpression::InverseObjectProperty(_op)
                    => todo!()
            }
        )
    }
}


render_triple! {
    DeclareClass, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::Class)
}

render_triple! {
    DeclareDatatype, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(RDFS::Datatype)
}

render_triple!{
    DeclareObjectProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::ObjectProperty)
}

render_triple!{
    DeclareDataProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::DatatypeProperty)
}

render_triple!{
    DeclareAnnotationProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::AnnotationProperty)
}

render_triple!{
    DeclareNamedIndividual, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::NamedIndividual)
}

render! {
    SubClassOf, self, f, ng, AsRefTriple<Rc<str>>,
    {
        let sub = self.sub.render(f, ng)?;
        let obj = self.sup.render(f, ng)?;

        Ok(
            triple!(f,
                    sub,
                    ng.nn(RDFS::SubClassOf),
                    obj
            )
        )
    }
}


#[cfg(test)]
mod test {

    extern crate mktemp;

    use self::mktemp::Temp;
    use super::*;
    use crate::{model::Build, ontology::set::SetOntology};

    // use std::collections::HashMap;

    // use std::fs::File;
    use std::{fs::File, io::{BufRead, BufReader, BufWriter}};
    // use std::io::BufReader;
    // use std::io::BufWriter;

    fn read_ok<R: BufRead>(bufread: &mut R) -> SetOntology {
        let r = crate::io::rdf::reader::read(bufread);
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        let (o, incomplete) = r.ok().unwrap();

        assert!(incomplete.is_complete(), "Read Not Complete: {:#?}", incomplete);
        o.into()
    }

    #[test]
    fn test_ont_rt() {
        let mut ont = AxiomMappedOntology::default();
        let build = Build::new();

        let iri = build.iri("http://www.example.com/a".to_string());
        ont.mut_id().iri = Some(iri);
        let temp_file = Temp::new_file().unwrap();
        let file = File::create(&temp_file).ok().unwrap();
        write(&mut BufWriter::new(file), &ont).ok().unwrap();

        let file = File::open(&temp_file).ok().unwrap();
        let ont2 = read_ok(&mut BufReader::new(file));

        assert_eq!(ont.id().iri, ont2.id().iri);
    }

    fn roundtrip(
        ont: &str,
    ) -> (
        SetOntology,
        SetOntology,
    ) {
        let ont_orig = read_ok(&mut ont.as_bytes());
        let mut temp_file = Temp::new_file().unwrap();

        let file = File::create(&temp_file).ok().unwrap();
        let mut buf_writer = BufWriter::new(&file);

        let amo: AxiomMappedOntology = ont_orig.clone().into();
        write(&mut buf_writer, &amo)
            .ok()
            .unwrap();
        buf_writer.flush().ok();

        let file = File::open(&temp_file).ok().unwrap();
        let ont_round = read_ok(&mut BufReader::new(&file));

        temp_file.release();

        return (ont_orig, ont_round);
    }

    fn assert_round(
        ont: &str,
    ) -> (
        SetOntology,
        SetOntology,
    ) {
        let (ont_orig, ont_round) = roundtrip(ont);

        println!("ont_orig\n{:#?}", ont_orig);
        println!("ont_round\n{:#?}", ont_round);
        assert_eq!(ont_orig, ont_round);

        return (ont_orig, ont_round);
    }

    #[test]
    fn round_ont() {
        assert_round(include_str!("../../ont/owl-rdf/ont.owl"));
    }

    #[test]
    fn round_class() {
        assert_round(include_str!("../../ont/owl-rdf/class.owl"));
    }

    #[test]
    fn round_class_with_annotation() {
        assert_round(include_str!(
            "../../ont/owl-rdf/declaration-with-annotation.owl"
        ));
    }

    #[test]
    fn round_subclass() {
        assert_round(include_str!("../../ont/owl-rdf/subclass.owl"));
    }

    #[test]
    fn round_oproperty() {
        assert_round(include_str!("../../ont/owl-rdf/oproperty.owl"));
    }

    #[test]
    fn round_some() {
        assert_round(include_str!("../../ont/owl-rdf/some.owl"));
    }

    #[test]
    fn round_only() {
        assert_round(include_str!("../../ont/owl-rdf/only.owl"));
    }

    #[test]
    fn round_and() {
        assert_round(include_str!("../../ont/owl-rdf/and.owl"));
    }

    #[test]
    fn round_or() {
        assert_round(include_str!("../../ont/owl-rdf/or.owl"));
    }

    #[test]
    fn round_not() {
        assert_round(include_str!("../../ont/owl-rdf/not.owl"));
    }

    #[test]
    fn round_annotation_property() {
        assert_round(include_str!("../../ont/owl-rdf/annotation-property.owl"));
    }

    #[test]
    fn round_annotation() {
        assert_round(include_str!("../../ont/owl-rdf/annotation.owl"));
    }

    #[test]
    fn round_annotation_domain() {
        assert_round(include_str!("../../ont/owl-rdf/annotation-domain.owl"));
    }

    #[test]
    fn round_annotation_range() {
        assert_round(include_str!("../../ont/owl-rdf/annotation-range.owl"));
    }

    #[test]
    fn round_label() {
        assert_round(include_str!("../../ont/owl-rdf/label.owl"));
    }

    #[test]
    fn round_one_comment() {
         assert_round(include_str!("../../ont/owl-rdf/one-comment.owl"));
    }

    #[test]
    fn round_one_ontology_annotation() {
        assert_round(include_str!(
            "../../ont/owl-rdf/one-ontology-annotation.owl"
        ));
    }

    // #[test]
    // fn round_one_equivalent_class() {
    //     assert_round(include_str!("../../ont/owl-rdf/one-equivalent.owl"));
    // }

    // #[test]
    // fn round_one_disjoint_class() {
    //     assert_round(include_str!("../../ont/owl-rdf/one-disjoint.owl"));
    // }

    // #[test]
    // fn round_disjoint_union() {
    //     assert_round(include_str!("../../ont/owl-rdf/disjoint-union.owl"));
    // }

    // #[test]
    // fn round_one_sub_property() {
    //     assert_round(include_str!("../../ont/owl-rdf/one-suboproperty.owl"));
    // }

    // #[test]
    // fn round_one_inverse() {
    //     assert_round(include_str!("../../ont/owl-rdf/inverse-properties.owl"));
    // }

    // #[test]
    // fn round_one_transitive() {
    //     assert_round(include_str!("../../ont/owl-rdf/transitive-properties.owl"));
    // }

    // #[test]
    // fn round_one_annotated_transitive() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/annotation-on-transitive.owl"
    //     ));
    // }

    // #[test]
    // fn round_one_subproperty_chain() {
    //     assert_round(include_str!("../../ont/owl-rdf/subproperty-chain.owl"));
    // }

    // #[test]
    // fn round_one_subproperty_chain_with_inverse() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/subproperty-chain-with-inverse.owl"
    //     ));
    // }

    // #[test]
    // fn round_annotation_on_annotation() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/annotation-with-annotation.owl"
    //     ));
    // }

    // #[test]
    // fn round_sub_annotation() {
    //     assert_round(include_str!("../../ont/owl-rdf/sub-annotation.owl"));
    // }

    #[test]
    fn round_data_property() {
        assert_round(include_str!("../../ont/owl-rdf/data-property.owl"));
    }

    // #[test]
    // fn round_literal_escaped() {
    //     assert_round(include_str!("../../ont/owl-rdf/literal-escaped.owl"));
    // }

    #[test]
    fn round_named_individual() {
        assert_round(include_str!("../../ont/owl-rdf/named-individual.owl"));
    }

    #[test]
    fn round_import() {
        assert_round(include_str!("../../ont/owl-rdf/import.owl"));
    }

    #[test]
    fn datatype() {
        assert_round(include_str!("../../ont/owl-rdf/datatype.owl"));
    }

    // #[test]
    // fn object_has_value() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-has-value.owl"));
    // }

    // #[test]
    // fn object_one_of() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-one-of.owl"));
    // }

    // #[test]
    // fn inverse() {
    //     assert_round(include_str!("../../ont/owl-rdf/some-inverse.owl"));
    // }

    // #[test]
    // fn object_unqualified_cardinality() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-unqualified-max-cardinality.owl"
    //     ));
    // }

    // #[test]
    // fn object_min_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-min-cardinality.owl"));
    // }

    // #[test]
    // fn object_max_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-max-cardinality.owl"));
    // }

    // #[test]
    // fn object_exact_cardinality() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-exact-cardinality.owl"
    //     ));
    // }

    // #[test]
    // fn datatype_alias() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-alias.owl"));
    // }

    // #[test]
    // fn datatype_intersection() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-intersection.owl"));
    // }

    // #[test]
    // fn datatype_union() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-union.owl"));
    // }

    // #[test]
    // fn datatype_complement() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-complement.owl"));
    // }

    // #[test]
    // fn datatype_oneof() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-oneof.owl"));
    // }

    // #[test]
    // fn datatype_some() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-some.owl"));
    // }

    // #[test]
    // fn facet_restriction() {
    //     assert_round(include_str!("../../ont/owl-rdf/facet-restriction.owl"));
    // }

    // #[test]
    // fn data_only() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-only.owl"));
    // }
    // #[test]
    // fn data_exact_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-exact-cardinality.owl"));
    // }

    // #[test]
    // fn data_has_value() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-has-value.owl"));
    // }

    // #[test]
    // fn data_max_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-max-cardinality.owl"));
    // }

    // #[test]
    // fn data_min_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-min-cardinality.owl"));
    // }

    // #[test]
    // fn class_assertion() {
    //     assert_round(include_str!("../../ont/owl-rdf/class-assertion.owl"));
    // }

    // #[test]
    // fn data_property_assertion() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/data-property-assertion.owl"
    //     ));
    // }

    // #[test]
    // fn same_individual() {
    //     assert_round(include_str!("../../ont/owl-rdf/same-individual.owl"));
    // }

    // #[test]
    // fn different_individuals() {
    //     assert_round(include_str!("../../ont/owl-rdf/different-individual.owl"));
    // }

    // #[test]
    // fn negative_data_property_assertion() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/negative-data-property-assertion.owl"
    //     ));
    // }

    // #[test]
    // fn negative_object_property_assertion() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/negative-object-property-assertion.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_assertion() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-assertion.owl"
    //     ));
    // }

    // #[test]
    // fn data_has_key() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-has-key.owl"));
    // }

    // #[test]
    // fn data_property_disjoint() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-property-disjoint.owl"));
    // }

    // #[test]
    // fn data_property_domain() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-property-domain.owl"));
    // }

    // #[test]
    // fn data_property_equivalent() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/data-property-equivalent.owl"
    //     ));
    // }

    // #[test]
    // fn data_property_functional() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/data-property-functional.owl"
    //     ));
    // }

    // #[test]
    // fn data_property_range() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-property-range.owl"));
    // }

    // #[test]
    // fn data_property_sub() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-property-sub.owl"));
    // }

    // #[test]
    // fn disjoint_object_properties() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/disjoint-object-properties.owl"
    //     ));
    // }

    // #[test]
    // fn equivalent_object_properties() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/equivalent-object-properties.owl"
    //     ));
    // }

    // #[test]
    // fn object_has_key() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-has-key.owl"));
    // }

    // #[test]
    // fn object_property_asymmetric() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-asymmetric.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_domain() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-property-domain.owl"));
    // }

    // #[test]
    // fn object_property_functional() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-functional.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_inverse_functional() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-inverse-functional.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_irreflexive() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-irreflexive.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_range() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-property-range.owl"));
    // }

    // #[test]
    // fn object_property_reflexive() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-reflexive.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_symmetric() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-symmetric.owl"
    //     ));
    // }

    // #[test]
    // fn family() {
    //     assert_round(include_str!("../../ont/owl-rdf/family.owl"));
    // }

}

