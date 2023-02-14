use crate::{
    error::invalid,
    error::HornedError,
    model::*,
    ontology::component_mapped::ComponentMappedOntology,
    vocab::{is_thing, Vocab, WithIRI, OWL, RDF, RDFS, XSD},
};

use crate::ontology::indexed::ForIndex;

use pretty_rdf::{
    ChunkedRdfXmlFormatterConfig, PBlankNode, PLiteral, PNamedNode, PSubject, PTerm, PTriple,
    PrettyRdfXmlFormatter,
};
use std::{
    collections::{BTreeSet, HashSet},
    fmt::Debug,
    io::Write,
};

pub fn write<A: ForIRI, AA: ForIndex<A>, W: Write>(
    write: &mut W,
    ont: &ComponentMappedOntology<A, AA>,
) -> Result<(), HornedError> {
    // Entirely unsatisfying to set this randomly here, but we can't
    // access ns our parser yet
    let mut p = indexmap::IndexMap::new();
    p.insert(
        "http://www.w3.org/2002/07/owl#".to_string(),
        "owl".to_string(),
    );
    p.insert(
        "http://www.w3.org/XML/1998/namespace".to_string(),
        "xml".to_string(),
    );
    let mut f: PrettyRdfXmlFormatter<_, _> =
        PrettyRdfXmlFormatter::new(write, ChunkedRdfXmlFormatterConfig::all().prefix(p))?;
    let mut bng = NodeGenerator::default();
    ont.render(&mut f, &mut bng)?;

    // for i in f.triples() {
    //     println!("{}", i.printable());
    // }

    f.finish()?;
    Ok(())
}

struct NodeGenerator<A: ForIRI> {
    i: u64,
    b: HashSet<A>,
    this_bn: Option<PSubject<A>>,
}

impl<A: ForIRI> Default for NodeGenerator<A> {
    fn default() -> Self {
        NodeGenerator {
            i: 0,
            b: HashSet::new(),
            this_bn: None,
        }
    }
}

impl<A: ForIRI> NodeGenerator<A> {
    /// Generate a NamedNode from a given Vocab element.
    pub fn nn<V: Into<Vocab>>(&mut self, v: V) -> PNamedNode<A> {
        PNamedNode::new(self.cache_rc(v))
    }

    /// Return an cached version of PNamedNode value.
    fn cache_rc<V: Into<Vocab>>(&mut self, v: V) -> A {
        let voc: &str = v.into().as_iri_str();
        if let Some(rc) = self.b.get(voc) {
            return rc.clone();
        }

        let rc: A = (*voc).to_string().into();
        self.b.insert(rc.clone());
        rc
    }

    pub fn bn(&mut self) -> PSubject<A> {
        self.i += 1;
        PSubject::BlankNode(PBlankNode {
            id: format! {"bn{}", self.i}.into(),
        })
    }

    pub fn keep_this_bn(&mut self, bn: PSubject<A>) {
        self.this_bn = Some(bn);
    }

    pub fn this_bn(&mut self) -> Option<PSubject<A>> {
        self.this_bn.take()
    }
}

impl<A: ForIRI> From<&IRI<A>> for PTerm<A> {
    fn from(iri: &IRI<A>) -> Self {
        PNamedNode::new(iri.underlying()).into()
    }
}

impl<A: ForIRI> From<&IRI<A>> for PNamedNode<A> {
    fn from(iri: &IRI<A>) -> Self {
        PNamedNode::new(iri.underlying())
    }
}

impl<A: ForIRI> From<&IRI<A>> for PSubject<A> {
    fn from(iri: &IRI<A>) -> Self {
        let nn = PNamedNode::new(iri.underlying());
        nn.into()
    }
}

impl<A: ForIRI> From<&NamedIndividual<A>> for PTerm<A> {
    fn from(ni: &NamedIndividual<A>) -> Self {
        (&ni.0).into()
    }
}

impl<A: ForIRI> From<&NamedIndividual<A>> for PNamedNode<A> {
    fn from(ni: &NamedIndividual<A>) -> Self {
        (&ni.0).into()
    }
}

impl<A: ForIRI> From<&NamedIndividual<A>> for PSubject<A> {
    fn from(ni: &NamedIndividual<A>) -> Self {
        let nn: PNamedNode<A> = ni.into();
        nn.into()
    }
}

impl<A: ForIRI> From<&AnonymousIndividual<A>> for PTerm<A> {
    fn from(ai: &AnonymousIndividual<A>) -> Self {
        PBlankNode::new(ai.0.clone()).into()
    }
}

impl<A: ForIRI> From<&AnonymousIndividual<A>> for PBlankNode<A> {
    fn from(ai: &AnonymousIndividual<A>) -> Self {
        PBlankNode::new(ai.0.clone())
    }
}

impl<A: ForIRI> From<&AnonymousIndividual<A>> for PSubject<A> {
    fn from(ai: &AnonymousIndividual<A>) -> Self {
        let bn: PBlankNode<A> = ai.into();
        bn.into()
    }
}

impl<A: ForIRI> From<&Individual<A>> for PTerm<A> {
    fn from(ind: &Individual<A>) -> Self {
        match ind {
            Individual::Named(ni) => ni.into(),
            Individual::Anonymous(ai) => ai.into(),
        }
    }
}

impl<A: ForIRI> From<&Individual<A>> for PSubject<A> {
    fn from(ind: &Individual<A>) -> Self {
        match ind {
            Individual::Named(ni) => ni.into(),
            Individual::Anonymous(ai) => ai.into(),
        }
    }
}

trait Render<A: ForIRI, R> {
    fn render<W: Write>(
        &self,
        f: &mut PrettyRdfXmlFormatter<A, W>,
        ng: &mut NodeGenerator<A>,
    ) -> Result<R, HornedError>;
}

enum Annotatable<A: ForIRI> {
    Main(PTriple<A>),
    Multiple(Vec<PTriple<A>>),
    //Blank(PBlankNode<A>)
}

impl<A: ForIRI> From<PTriple<A>> for Annotatable<A> {
    fn from(t: PTriple<A>) -> Self {
        Self::Main(t)
    }
}

impl<A: ForIRI> From<Vec<PTriple<A>>> for Annotatable<A> {
    fn from(t: Vec<PTriple<A>>) -> Self {
        Self::Multiple(t)
    }
}

/// The types in `Render` are too long to type.
macro_rules! render {
    ($type:ident, $self:ident, $f:ident, $ng:ident, $return:ident,
     $body:tt) => {
        impl<A: ForIRI> Render<A, $return<A>> for $type<A> {
            fn render<W:Write>(& $self, $f:&mut PrettyRdfXmlFormatter<A, W>,
                               $ng: &mut NodeGenerator<A>)
                               -> Result<$return<A>, HornedError>
                $body
        }
    }
}

macro_rules! render_to_node {
    ($type:ident, $self:ident, $f:ident, $ng:ident,
     $body:tt) => {
        render! {$type, $self, $f, $ng, PSubject, $body}
    };
}

macro_rules! render_to_vec {
    ($type:ident, $self:ident, $f:ident, $ng:ident,
     $body:tt) => {
        impl<A: ForIRI> Render<A, Vec<PTriple<A>>> for $type<A> {
            fn render<W:Write>(& $self, $f:&mut PrettyRdfXmlFormatter<A, W>,
                               $ng: &mut NodeGenerator<A>)
                               -> Result<Vec<PTriple<A>>, HornedError>
                $body
        }
    };
}

macro_rules! render_triple {
    ($type:ident, $self:ident, $ng:ident, $sub:expr, $pred:expr, $ob:expr) => {
        render! {
            $type, $self, f, $ng, PTriple,
            {
                Ok(triple!(f, $sub, $pred, $ob))
            }
        }
    };
}

macro_rules! triple {
    ($f:ident, $sub:expr, $pred:expr, $ob:expr) => {{
        let t = to_triple($sub, $pred, $ob);
        $f.format(t.clone())?;
        t
    }};
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

fn to_triple<A: ForIRI, NB, NN, T>(subject: NB, predicate: NN, object: T) -> PTriple<A>
where
    NB: Into<PSubject<A>>,
    NN: Into<PNamedNode<A>>,
    T: Into<PTerm<A>>,
{
    PTriple {
        subject: subject.into(),
        predicate: predicate.into(),
        object: object.into(),
    }
}

// impl<A:ForIRI, R> Render<A, PTerm<A>> for R
// where
//     R: Render<A, PSubject<A>>,
// {
//     fn render<W: Write>(
//         &self,
//         f: &mut PrettyRdfXmlFormatter<A, W>,
//         ng: &mut NodeGenerator,
//     ) -> Result<PTerm<A>, HornedError> {
//         Ok(self.render(f, ng)?.into())
//     }
// }

fn render_vec_subject<A: ForIRI, T: Render<A, PSubject<A>>, W: Write>(
    v: &[T],
    f: &mut PrettyRdfXmlFormatter<A, W>,
    ng: &mut NodeGenerator<A>,
) -> Result<PTerm<A>, HornedError> {
    let mut rest: Option<PTerm<A>> = None;
    for i in v.iter().rev() {
        let bn = &ng.bn();
        let item = i.render(f, ng)?;

        triples!(f, bn.clone(), ng.nn(RDF::First), item);

        if let Some(r) = rest.take() {
            triples!(f, bn.clone(), ng.nn(RDF::Rest), r);
        } else {
            triples!(f, bn.clone(), ng.nn(RDF::Rest), ng.nn(RDF::Nil));
        }
        rest = Some(bn.clone().into())
    }
    // Panic if Vec is zero length!
    Ok(rest.unwrap())
}

impl<A: ForIRI, T> Render<A, PTerm<A>> for &Vec<T>
where
    T: Debug + Render<A, PTerm<A>>,
{
    fn render<W: Write>(
        &self,
        f: &mut PrettyRdfXmlFormatter<A, W>,
        ng: &mut NodeGenerator<A>,
    ) -> Result<PTerm<A>, HornedError> {
        let mut rest: Option<PTerm<A>> = None;
        for i in self.iter().rev() {
            let bn = &ng.bn();
            let item = i.render(f, ng)?;

            triples!(f, bn.clone(), ng.nn(RDF::First), item);

            if let Some(r) = rest.take() {
                triples!(f, bn.clone(), ng.nn(RDF::Rest), r);
            } else {
                triples!(f, bn.clone(), ng.nn(RDF::Rest), ng.nn(RDF::Nil));
            }
            rest = Some(bn.clone().into())
        }
        // Panic if Vec is zero length!
        Ok(rest.unwrap())
    }
}

impl<A: ForIRI> Render<A, ()> for BTreeSet<Annotation<A>> {
    fn render<W: Write>(
        &self,
        f: &mut PrettyRdfXmlFormatter<A, W>,
        ng: &mut NodeGenerator<A>,
    ) -> Result<(), HornedError> {
        for r in self.iter() {
            r.render(f, ng)?;
        }
        Ok(())
    }
}

impl<A: ForIRI, AA: ForIndex<A>> Render<A, ()> for &ComponentMappedOntology<A, AA> {
    fn render<W: Write>(
        &self,
        f: &mut PrettyRdfXmlFormatter<A, W>,
        ng: &mut NodeGenerator<A>,
    ) -> Result<(), HornedError> {
        if let Some(iri) = &self.id().iri {
            triples!(f, iri, ng.nn(RDF::Type), ng.nn(OWL::Ontology));

            if let Some(viri) = &self.id().viri {
                triples!(f, iri, ng.nn(OWL::VersionIRI), viri);
            }

            let imp = self.i().import();
            for i in imp {
                triples!(f, iri, ng.nn(OWL::Imports), &i.0);
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

impl<A: ForIRI> Render<A, ()> for AnnotatedComponent<A> {
    fn render<W: Write>(
        &self,
        f: &mut PrettyRdfXmlFormatter<A, W>,
        ng: &mut NodeGenerator<A>,
    ) -> Result<(), HornedError> {
        let ax: Annotatable<A> = self.axiom.render(f, ng)?;
        if !self.ann.is_empty() {
            if let Annotatable::Main(t) = ax {
                let bn = ng.bn();
                    triples!(
                        f,
                        bn.clone(),
                        ng.nn(RDF::Type),
                        ng.nn(OWL::Axiom),
                        bn.clone(),
                        ng.nn(OWL::AnnotatedSource),
                        t.subject,
                        bn.clone(),
                        ng.nn(OWL::AnnotatedProperty),
                        t.predicate,
                        bn.clone(),
                        ng.nn(OWL::AnnotatedTarget),
                        t.object // And the rest!
                    );

                    ng.keep_this_bn(bn);

                    let _ = self.ann.render(f, ng);
            }
        }
        Ok(())
    }
}

render! {
    Literal, self, _f, _ng, PTerm,
    {
        Ok(
            match self {
                Literal::Simple{literal} =>
                    PTerm::Literal(PLiteral::Simple{value:literal.clone().into()}),
                Literal::Language{literal, lang} =>
                    PTerm::Literal(PLiteral::LanguageTaggedString{value:literal.clone().into(),
                                                                          language: lang.clone().into()}),
                Literal::Datatype{literal, datatype_iri} =>
                    PTerm::Literal(PLiteral::Typed{value:literal.clone().into(),
                                                           datatype:datatype_iri.into()})
            }
        )
    }
}

render! {
    Annotation, self, f, ng, PTriple,
    {
        let bn = ng.this_bn().ok_or_else(|| invalid!("{}", "No bnode available"))?;

        Ok(
            match &self.av {
                AnnotationValue::Literal(l) => {
                    let obj = l.render(f, ng)?;

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

render! {
    AnnotationAssertion, self, f, ng, PTriple,
    {
        let nbn:PSubject<A> = (&self.subject).render(f, ng)?;
        ng.keep_this_bn(nbn);

        self.ann.render(f, ng)
    }
}

render! {
    AnnotationPropertyDomain, self, f, ng, PTriple,
    {
        Ok(
            triple!(f, &self.ap.0, ng.nn(RDFS::Domain), &self.iri)
        )
    }
}

render! {
    AnnotationPropertyRange, self, f, ng, PTriple,
    {
        Ok(
            triple!(f, &self.ap.0, ng.nn(RDFS::Range), &self.iri)
        )
    }
}

impl<A: ForIRI> Render<A, Annotatable<A>> for Component<A> {
    fn render<W: Write>(
        &self,
        f: &mut PrettyRdfXmlFormatter<A, W>,
        ng: &mut NodeGenerator<A>,
    ) -> Result<Annotatable<A>, HornedError> {
        Ok(match self {
            // We render imports and ontology annotations earlier
            Component::OntologyIDComponent(_) => todo!(),
            Component::Import(_ax) => vec![].into(),
            Component::OntologyAnnotation(_ax) => vec![].into(),
            Component::DeclareClass(ax) => ax.render(f, ng)?.into(),
            Component::DeclareObjectProperty(ax) => ax.render(f, ng)?.into(),
            Component::DeclareAnnotationProperty(ax) => ax.render(f, ng)?.into(),
            Component::DeclareDataProperty(ax) => ax.render(f, ng)?.into(),
            Component::DeclareNamedIndividual(ax) => ax.render(f, ng)?.into(),
            Component::DeclareDatatype(ax) => ax.render(f, ng)?.into(),
            Component::SubClassOf(ax) => ax.render(f, ng)?.into(),
            Component::EquivalentClasses(ax) => ax.render(f, ng)?.into(),
            Component::DisjointClasses(ax) => ax.render(f, ng)?.into(),
            Component::DisjointUnion(ax) => ax.render(f, ng)?.into(),
            Component::SubObjectPropertyOf(ax) => ax.render(f, ng)?.into(),
            Component::EquivalentObjectProperties(ax) => ax.render(f, ng)?.into(),
            Component::DisjointObjectProperties(ax) => ax.render(f, ng)?.into(),
            Component::InverseObjectProperties(ax) => ax.render(f, ng)?.into(),
            Component::ObjectPropertyDomain(ax) => ax.render(f, ng)?.into(),
            Component::ObjectPropertyRange(ax) => ax.render(f, ng)?.into(),
            Component::FunctionalObjectProperty(ax) => ax.render(f, ng)?.into(),
            Component::InverseFunctionalObjectProperty(ax) => ax.render(f, ng)?.into(),
            Component::ReflexiveObjectProperty(ax) => ax.render(f, ng)?.into(),
            Component::IrreflexiveObjectProperty(ax) => ax.render(f, ng)?.into(),
            Component::SymmetricObjectProperty(ax) => ax.render(f, ng)?.into(),
            Component::AsymmetricObjectProperty(ax) => ax.render(f, ng)?.into(),
            Component::TransitiveObjectProperty(ax) => ax.render(f, ng)?.into(),
            Component::SubDataPropertyOf(ax) => ax.render(f, ng)?.into(),
            Component::EquivalentDataProperties(ax) => ax.render(f, ng)?.into(),
            Component::DisjointDataProperties(ax) => ax.render(f, ng)?.into(),
            Component::DataPropertyDomain(ax) => ax.render(f, ng)?.into(),
            Component::DataPropertyRange(ax) => ax.render(f, ng)?.into(),
            Component::FunctionalDataProperty(ax) => ax.render(f, ng)?.into(),
            Component::DatatypeDefinition(ax) => ax.render(f, ng)?.into(),
            Component::HasKey(ax) => ax.render(f, ng)?.into(),
            Component::SameIndividual(ax) => ax.render(f, ng)?.into(),
            Component::DifferentIndividuals(ax) => ax.render(f, ng)?.into(),
            Component::ObjectPropertyAssertion(ax) => ax.render(f, ng)?.into(),
            Component::NegativeObjectPropertyAssertion(ax) => ax.render(f, ng)?.into(),
            Component::DataPropertyAssertion(ax) => ax.render(f, ng)?.into(),
            Component::NegativeDataPropertyAssertion(ax) => ax.render(f, ng)?.into(),
            Component::AnnotationAssertion(ax) => ax.render(f, ng)?.into(),
            Component::SubAnnotationPropertyOf(ax) => ax.render(f, ng)?.into(),
            Component::AnnotationPropertyDomain(ax) => ax.render(f, ng)?.into(),
            Component::AnnotationPropertyRange(ax) => ax.render(f, ng)?.into(),
            Component::ClassAssertion(ax) => ax.render(f, ng)?.into(),
        })
    }
}

render! {
    ObjectPropertyRange, self, f, ng, PTriple,
    {
        let node_ope:PSubject<_> = self.ope.render(f, ng)?;
        let node_ce:PTerm<_> = self.ce.render(f, ng)?.into();

        Ok(
            triple!(f,
                    node_ope, ng.nn(RDFS::Range), node_ce
            )
        )
    }
}

render! {
    ObjectPropertyDomain, self, f, ng, PTriple,
    {
        let node_ope:PSubject<_> = self.ope.render(f, ng)?;
        let node_ce:PTerm<_> = self.ce.render(f, ng)?.into();

        Ok(
            triple!(f,
                    node_ope, ng.nn(RDFS::Domain), node_ce
            )
        )
    }
}

render! {
    IrreflexiveObjectProperty, self, f, ng, PTriple,
    {
        obj_prop_char(&self.0, f, ng, OWL::IrreflexiveProperty)
    }
}

render! {
    InverseFunctionalObjectProperty, self, f, ng, PTriple,
    {
        obj_prop_char(&self.0, f, ng, OWL::InverseFunctionalProperty)
    }
}

render! {
    FunctionalObjectProperty, self, f, ng, PTriple,
    {
        obj_prop_char(&self.0, f, ng, OWL::FunctionalProperty)
    }
}

render! {
    ReflexiveObjectProperty, self, f, ng, PTriple,
    {
        obj_prop_char(&self.0, f, ng, OWL::ReflexiveProperty)
    }
}

render! {
    SymmetricObjectProperty, self, f, ng, PTriple,
    {
        obj_prop_char(&self.0, f, ng, OWL::SymmetricProperty)
    }
}

render! {
    AsymmetricObjectProperty, self, f, ng, PTriple,
    {
       obj_prop_char(&self.0, f, ng, OWL::AsymmetricProperty)
    }
}

render! {
    SubDataPropertyOf, self, f, ng, PTriple,
    {
        // T(DPE1) rdfs:subPropertyOf T(DPE2) .
        let node_sub:PSubject<_> = self.sub.render(f, ng)?;
        let node_sup:PTerm<_> = self.sup.render(f, ng)?.into();

        Ok(
            triple!(f,
                    node_sub, ng.nn(RDFS::SubPropertyOf), node_sup
            )
        )
    }
}

render! {
    DataPropertyRange, self, f, ng, PTriple,
    {
        let node_dp:PSubject<_> = self.dp.render(f, ng)?;
        let node_dr:PTerm<_> = self.dr.render(f, ng)?.into();

        Ok(
            triple!(f,
                    node_dp, ng.nn(RDFS::Range), node_dr
            )
        )
    }
}

render! {
    FunctionalDataProperty, self, f, ng, PTriple,
    {
        let node_pr:PSubject<_> = self.0.render(f, ng)?;
        Ok(
            triple!(f,
                    node_pr, ng.nn(RDF::Type), ng.nn(OWL::FunctionalProperty)
            )
        )
    }
}

render_to_vec! {
    EquivalentDataProperties, self, f, ng,
    {
        let pred = ng.nn(OWL::EquivalentProperty);
        nary(f, ng, &self.0, pred)
    }
}

render_to_vec! {
    EquivalentObjectProperties, self, f, ng,
    {
        let pred = ng.nn(OWL::EquivalentProperty);
        nary(f, ng, &self.0, pred)
    }
}

render! {
    DataPropertyDomain, self, f, ng, PTriple,
    {
        let node_dp:PSubject<A> = self.dp.render(f, ng)?;
        let node_ce:PTerm<A> = self.ce.render(f, ng)?.into();

        Ok(
            triple!(
                f, node_dp, ng.nn(RDFS::Domain), node_ce
            )
        )
    }
}

render_to_vec! {
    DisjointObjectProperties, self, f, ng,
    {
        //& self.0 Vec<ObjectPropertyExpression>
        members(f, ng,
                OWL::PropertyDisjointWith,
                OWL::AllDisjointProperties,
                &self.0
        )
    }
}

render_to_vec! {
    DisjointDataProperties, self, f, ng,
    {
        //&self.0 Vec<DataProperty>
        members(f, ng,
                OWL::PropertyDisjointWith,
                OWL::AllDisjointProperties,
                &self.0
        )
    }
}

render! {
    ObjectPropertyAssertion, self, f, ng, PTriple,
    {
        match &self.ope {
            ObjectPropertyExpression::ObjectProperty(op) => {
                //ObjectPropertyAssertion( OP a1 a2 ) T(a1) T(OP) T(a2) .
                let node_from:PSubject<_> = self.from.render(f, ng)?;
                let node_op:PNamedNode<_> = (&op.0).into();
                let node_to:PTerm<_> = self.to.render(f,ng)?.into();
                Ok(
                    triple! (
                        f,
                        node_from, node_op, node_to
                    )
                )
            },
            ObjectPropertyExpression::InverseObjectProperty(op) => {
                //ObjectPropertyAssertion( OP a1 a2 ) T(a1) T(OP) T(a2) .
                let node_to:PSubject<_> = self.to.render(f, ng)?;
                let node_op:PNamedNode<_> = (&op.0).into();
                let node_from:PTerm<_> = self.from.render(f, ng)?.into();
                Ok(
                    triple! (
                        f,
                        node_to, node_op, node_from
                    )
                )
            },
        }
    }
}

render_to_vec! {
    NegativeObjectPropertyAssertion, self, f, ng,
    {
        // NegativeObjectPropertyAssertion( OPE a1 a2 )
        //_:x rdf:type owl:NegativePropertyAssertion .
        //_:x owl:sourceIndividual T(a1) .
        //_:x owl:assertionProperty T(OPE) .
        //_:x owl:targetIndividual T(a2) .
        let bn = ng.bn();
        let node_lt:PTerm<_> = self.to.render(f, ng)?.into();
        let node_ope:PTerm<_> = self.ope.render(f, ng)?.into();
        let node_a:PTerm<_> = self.from.render(f, ng)?.into();

        Ok(
            triples_to_vec!(
                f,
                bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::NegativePropertyAssertion),
                bn.clone(), ng.nn(OWL::SourceIndividual), node_a,
                bn.clone(), ng.nn(OWL::AssertionProperty), node_ope,
                bn, ng.nn(OWL::TargetIndividual), node_lt
            )
        )
    }
}
render_to_vec! {
    NegativeDataPropertyAssertion, self, f, ng,
    {
        // NegativeDataPropertyAssertion( DPE a1 a2 )
        //_:x rdf:type owl:NegativePropertyAssertion .
        //_:x owl:sourceIndividual T(a) .
        //_:x owl:assertionProperty T(DPE) .
        //_:x owl:targetValue T(lt) .
        let bn = ng.bn();
        let node_lt:PTerm<_> = self.to.render(f, ng)?;
        let node_dp:PTerm<_> = (&self.dp.0).into();
        let node_a:PTerm<_> = self.from.render(f, ng)?.into();

        Ok(
            triples_to_vec!(
                f,
                bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::NegativePropertyAssertion),
                bn.clone(), ng.nn(OWL::SourceIndividual), node_a,
                bn.clone(), ng.nn(OWL::AssertionProperty), node_dp,
                bn, ng.nn(OWL::TargetValue), node_lt
            )
        )
    }
}

fn members<A: ForIRI, R: Debug + Render<A, PSubject<A>>, W: Write>(
    f: &mut PrettyRdfXmlFormatter<A, W>,
    ng: &mut NodeGenerator<A>,
    ty_two: OWL,
    ty_n: OWL,
    members: &[R],
) -> Result<Vec<PTriple<A>>, HornedError> {
    // DifferentIndividuals( a1 a2 ) T(a1) owl:differentFrom T(a2) .
    // DifferentIndividuals( a1 ... an ), n > 2 _:x rdf:type owl:AllDifferent .
    // _:x owl:members T(SEQ a1 ... an) .
    match members.len() {
        1 => panic!(
            "A members axiom needs at least two members, and I should know how to make errors"
        ),
        2 => {
            let a: PSubject<_> = members[0].render(f, ng)?;
            let b: PTerm<_> = members[1].render(f, ng)?.into();
            Ok(triples_to_vec!(f, a, ng.nn(ty_two), b))
        }
        _ => {
            let bn = ng.bn();
            let node_v: PTerm<_> = render_vec_subject(members, f, ng)?;

            Ok(triples_to_vec!(
                f,
                bn.clone(),
                ng.nn(RDF::Type),
                ng.nn(ty_n),
                bn,
                ng.nn(OWL::Members),
                node_v
            ))
        }
    }
}

render_to_vec! {
    DifferentIndividuals, self, f, ng,
    {
        // Need to support also DisjointData/ObjectProperties which
        // have the same pattern
        //self.0: Vec<Individual>
        members(f, ng,
                OWL::DifferentFrom,
                OWL::AllDifferent,
                &self.0)
    }
}

render_to_vec! {
    SameIndividual, self, f, ng,
    {
        //T(a1) owl:sameAs T(a2) .
        //...
        //T(an-1) owl:sameAs T(an) .
        let pred = ng.nn(OWL::SameAs);
        nary(f, ng, &self.0, pred)
    }
}

render! {
    DataPropertyAssertion, self, f, ng, PTriple,
    {
        //T(a) T(DPE) T(lt) .
        let node_dp:PNamedNode<_> = (&self.dp.0).into();
        let node_i:PSubject<_> = self.from.render(f, ng)?;
        let node_lit:PTerm<_> = self.to.render(f, ng)?;
        Ok(
            triple!(f, node_i, node_dp, node_lit)
        )
    }
}

render! {
    ClassAssertion, self, f, ng, PTriple,
    {   // T(a) rdf:type T(CE) .
        let node_ce:PTerm<_> = self.ce.render(f, ng)?.into();
        let node_i:PSubject<_> = self.i.render(f, ng)?;

        Ok(
            triple!(f, node_i, ng.nn(RDF::Type), node_ce)
        )
    }
}

render_to_node! {
    DataRange, self, f, ng,
    {
        Ok(
            match self {
                Self::Datatype(dt) => (&dt.0).into(),
                Self::DataIntersectionOf(v) => {
                    let vbn = render_vec_subject(v, f, ng)?;
                    let bn = ng.bn();

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(RDFS::Datatype),
                        bn, ng.nn(OWL::IntersectionOf), vbn
                    )
                }
                Self::DataUnionOf(v) => {
                    let vbn = render_vec_subject(v, f, ng)?;
                    let bn = ng.bn();

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(RDFS::Datatype),
                        bn, ng.nn(OWL::UnionOf), vbn
                    )
                }
                Self::DataComplementOf(bdr) => {
                    //_:x rdf:type rdfs:Datatype .
                    //_:x owl:datatypeComplementOf T(DR) .
                    let node_dr:PTerm<A> = bdr.render(f, ng)?.into();
                    let bn = ng.bn();

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(RDFS::Datatype),
                        bn, ng.nn(OWL::DatatypeComplementOf), node_dr
                    )
                }
                Self::DataOneOf(v) => {
                    let vbn = v.render(f, ng)?;
                    let bn = ng.bn();

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(RDFS::Datatype),
                        bn, ng.nn(OWL::OneOf), vbn
                    )
                }
                Self::DatatypeRestriction(dt, vfr) => {
                    // _:x rdf:type rdfs:Datatype .
                    // _:x owl:onDatatype T(DT) .
                    // _:x owl:withRestrictions T(SEQ _:y1 ... _:yn) .
                    // _:y1 F1 lt1 .
                    //  ...
                    // _:yn Fn ltn .
                    let bn = ng.bn();
                    let node_dt:PTerm<_> = (&dt.0).into();
                    let node_vft:PTerm<_> = render_vec_subject(vfr, f, ng)?;

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(RDFS::Datatype),
                        bn.clone(), ng.nn(OWL::OnDatatype), node_dt,
                        bn, ng.nn(OWL::WithRestrictions), node_vft
                    )
                }
            }
        )
    }
}

render_to_node! {
    FacetRestriction, self, f, ng,
    {
        let bn = ng.bn();
        let l:PTerm<_> = self.l.render(f, ng)?;

        Ok(
            triples_to_node!(f, bn, ng.nn(&self.f), l)
        )
    }
}

render! {
    DatatypeDefinition, self, f, ng, PTriple,
    {
        let node_dr:PTerm<_> = self.range.render(f, ng)?.into();
        Ok(
            triple!(f, &self.kind.0, ng.nn(OWL::EquivalentClass), node_dr)
        )
    }
}

render! {
    SubAnnotationPropertyOf, self, f, ng, PTriple,
    {
        Ok(
            triple!(f, &self.sub.0, ng.nn(RDFS::SubPropertyOf), &self.sup.0)
        )
    }
}

render_to_node! {
    Individual, self, f, ng,
    {
        match self {
            Self::Named(ni) => ni.render(f, ng),
            Self::Anonymous(ai) => ai.render(f, ng),
        }
    }
}

render_to_node! {
    AnonymousIndividual, self, _f, _ng,
    {
        Ok(
            PSubject::BlankNode(PBlankNode::new(self.0.clone()))
        )
    }
}

render_to_node! {
    NamedIndividual, self, _f, _ng,
    {
        Ok(
            (&self.0).into()
        )
    }
}

render_to_node! {
    AnnotationSubject, self, f, ng,
    {
        Ok(
            match self {
                Self::AnonymousIndividual(ai) => ai.render(f, ng)?,
                Self::IRI(iri) => iri.into(),
            }
        )
    }
}

render_to_node! {
    DataProperty, self, _f, _ng,
    {
        Ok(
            (&self.0).into()
        )
    }
}

fn obj_cardinality<A: ForIRI, W: Write>(
    n: &u32,
    ope: &ObjectPropertyExpression<A>,
    ce: &ClassExpression<A>,
    unqual: PNamedNode<A>,
    qual: PNamedNode<A>,
    f: &mut PrettyRdfXmlFormatter<A, W>,
    ng: &mut NodeGenerator<A>,
) -> Result<PSubject<A>, HornedError> {
    //_:x rdf:type owl:Restriction .
    //_:x owl:onProperty T(OPE) .
    //_:x owl:maxCardinality "n"^^xsd:nonNegativeInteger ._:x
    let bn = ng.bn();
    let node_ope: PTerm<_> = ope.render(f, ng)?.into();
    let node_n = PTerm::Literal(PLiteral::Typed {
        value: format!("{}", n).into(),
        datatype: ng.nn(XSD::NonNegativeInteger),
    });

    // Unqualified Only
    triples!(
        f,
        bn.clone(),
        ng.nn(RDF::Type),
        ng.nn(OWL::Restriction),
        bn.clone(),
        ng.nn(OWL::OnProperty),
        node_ope
    );

    if let ClassExpression::Class(ref cl) = *ce {
        if is_thing(&cl.0) {
            triples!(f, bn.clone(), unqual, node_n);
            return Ok(bn);
        }
    }
    let node_ce: PTerm<_> = ce.render(f, ng)?.into();

    Ok(triples_to_node!(
        f,
        bn.clone(),
        qual,
        node_n,
        bn,
        ng.nn(OWL::OnClass),
        node_ce
    ))
}

fn data_cardinality<A: ForIRI, W: Write>(
    n: &u32,
    dp: &DataProperty<A>,
    dr: &DataRange<A>,
    qual: PNamedNode<A>,
    f: &mut PrettyRdfXmlFormatter<A, W>,
    ng: &mut NodeGenerator<A>,
) -> Result<PSubject<A>, HornedError> {
    // _:x rdf:type owl:Restriction .
    // _:x owl:onProperty T(DPE) .
    // _:x owl:maxQualifiedCardinality "n"^^xsd:nonNegativeInteger .
    // _:x owl:onDataRange T(DR) .
    let bn = ng.bn();
    let node_dp: PTerm<_> = (&dp.0).into();
    let node_n = PTerm::Literal(PLiteral::Typed {
        value: format!("{}", n).into(),
        datatype: ng.nn(XSD::NonNegativeInteger),
    });
    let node_dr: PTerm<_> = dr.render(f, ng)?.into();

    // Unqualified Only
    Ok(triples_to_node!(
        f,
        bn.clone(),
        ng.nn(RDF::Type),
        ng.nn(OWL::Restriction),
        bn.clone(),
        ng.nn(OWL::OnProperty),
        node_dp,
        bn.clone(),
        qual,
        node_n,
        bn,
        ng.nn(OWL::OnDataRange),
        node_dr
    ))
}

render_to_node! {
    ClassExpression, self, f, ng,
    {
        Ok(
            match self {
                Self::Class(cl) => (&cl.0).into(),
                Self::ObjectIntersectionOf(v)=>{
                    let bn = ng.bn();
                    let node_seq = render_vec_subject(v, f, ng)?;

                    triples_to_node!(
                         f,
                         bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Class),
                         bn, ng.nn(OWL::IntersectionOf), node_seq
                    )
                }
                Self::ObjectUnionOf(v) => {
                    let bn = ng.bn();
                    let node_seq = render_vec_subject(v, f, ng)?;

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Class),
                        bn, ng.nn(OWL::UnionOf), node_seq
                    )
                }
                Self::ObjectComplementOf(bce) => {
                    let bn = ng.bn();

                    let node_ce:PTerm<_> = (*bce).render(f, ng)?.into();

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Class),
                        bn, ng.nn(OWL::ComplementOf), node_ce
                    )
                },
                Self::ObjectOneOf(v) => {
                    let bn = ng.bn();
                    let node_seq = render_vec_subject(v, f, ng)?;

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Class),
                        bn, ng.nn(OWL::OneOf), node_seq
                    )
                }
                Self::ObjectSomeValuesFrom{ref bce, ref ope} => {
                    let bn = ng.bn();
                    let node_ce:PTerm<_> = bce.render(f, ng)?.into();
                    let node_ope:PTerm<_> = ope.render(f, ng)?.into();

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_ope,
                        bn, ng.nn(OWL::SomeValuesFrom), node_ce
                    )
                }
                Self::ObjectAllValuesFrom{ope, bce} => {
                    let bn = ng.bn();
                    let node_ce:PTerm<_> = bce.render(f, ng)?.into();
                    let node_ope:PTerm<_> = ope.render(f, ng)?.into();

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_ope,
                        bn, ng.nn(OWL::AllValuesFrom), node_ce
                    )
                }
                Self::ObjectHasValue{ope, i} => {
                    //_:x rdf:type owl:Restriction .
                    //_:x owl:onProperty T(OPE) .
                    // :x owl:hasValue T(a) .
                    let bn = ng.bn();

                    let node_ope:PTerm<_> = ope.render(f, ng)?.into();
                    let ind:PTerm<_>= i.render(f, ng)?.into();

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_ope,
                        bn, ng.nn(OWL::HasValue), ind
                    )
                }
                Self::ObjectHasSelf(_ope) =>todo!(),
                Self::ObjectMinCardinality{n, ope, bce} => {
                    obj_cardinality(n, ope, bce,
                                    ng.nn(OWL::MinCardinality),
                                    ng.nn(OWL::MinQualifiedCardinality),
                                    f, ng)?
                },
                Self::ObjectMaxCardinality{n, ope, bce} => {
                    obj_cardinality(n, ope, bce,
                                    ng.nn(OWL::MaxCardinality),
                                    ng.nn(OWL::MaxQualifiedCardinality),
                                    f, ng)?
                }
                Self::ObjectExactCardinality{n, ope, bce} => {
                    obj_cardinality(n, ope, bce,
                                    ng.nn(OWL::Cardinality),
                                    ng.nn(OWL::QualifiedCardinality),
                                    f, ng)?
                },
                Self::DataSomeValuesFrom{dp, dr} => {
                    //_:x rdf:type owl:Restriction .
                    //_:x owl:onProperty T(DPE) .
                    //_:x owl:someValuesFrom T(DR) .
                    let bn = ng.bn();
                    let node_dpe:PTerm<_> = (&dp.0).into();
                    let node_dr:PTerm<_> = dr.render(f, ng)?.into();
                    triples_to_node!{
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_dpe,
                        bn, ng.nn(OWL::SomeValuesFrom), node_dr
                    }
                }
                Self::DataAllValuesFrom{dp, dr} => {
                    let bn = ng.bn();
                    let node_dpe:PTerm<_> = (&dp.0).into();
                    let node_dr:PTerm<_> = dr.render(f, ng)?.into();
                    triples_to_node!{
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_dpe,
                        bn, ng.nn(OWL::AllValuesFrom), node_dr
                    }
                }
                Self::DataHasValue{dp, l} => {
                    //  :x rdf:type owl:Restriction .
                    // _:x owl:onProperty T(DPE) .
                    // _:x owl:hasValue T(lt) .
                    let bn = ng.bn();
                    let node_dp:PTerm<_> = (&dp.0).into();
                    let node_l:PTerm<_> = l.render(f, ng)?;

                    triples_to_node!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_dp,
                        bn, ng.nn(OWL::HasValue), node_l
                    )
                }
                Self::DataMinCardinality{n, dp, dr} => {
                    data_cardinality(n, dp, dr, ng.nn(OWL::MinQualifiedCardinality), f, ng)?
                }
                Self::DataMaxCardinality{n, dp, dr} => {
                    data_cardinality(n, dp, dr, ng.nn(OWL::MaxQualifiedCardinality), f, ng)?
                }
                Self::DataExactCardinality{n, dp, dr} => {
                    data_cardinality(n, dp, dr, ng.nn(OWL::QualifiedCardinality), f, ng)?
                },
            }
        )
    }
}

fn nary<A: ForIRI, R: Render<A, PSubject<A>>, W: Write>(
    f: &mut PrettyRdfXmlFormatter<A, W>,
    ng: &mut NodeGenerator<A>,
    entities: &[R],
    pred: PNamedNode<A>,
) -> Result<Vec<PTriple<A>>, HornedError> {
    let mut i = entities.iter();
    let first = i
        .next()
        .ok_or_else(|| invalid!("{}", "N-ary Class axiom with no classes"))?;
    let first: PSubject<_> = first.render(f, ng)?;
    let mut v = vec![];
    for c in i {
        let eq: PSubject<_> = c.render(f, ng)?;
        v.extend(triples_to_vec!(f, first.clone(), pred.clone(), eq));
    }
    Ok(v)
}

render_to_vec! {
    HasKey, self, f, ng,
    {
        //T(CE) owl:hasKey T(SEQ OPE1 ... OPEm DPE1 ... DPEn ) .
        let node_ce:PSubject<_> = self.ce.render(f, ng)?;
        let node_vpe:PTerm<_> = render_vec_subject(&self.vpe, f, ng)?;
        // let g:String = &self.vpe[0];
        // let node_vpe:PTerm<_> = g.render(f, ng)?;

        Ok(
            triples_to_vec!(
                f, node_ce, ng.nn(OWL::HasKey), node_vpe
            )
        )
    }
}

render_to_node! {
    PropertyExpression, self, f, ng,
    {
        Ok(
            match self {
                Self::ObjectPropertyExpression(ope) => ope.render(f, ng)?,
                Self::DataProperty(dp) => (&dp.0).into(),
                Self::AnnotationProperty(ap) => (&ap.0).into()
            }
        )
    }
}

render_to_vec! {
    DisjointClasses, self, f, ng,
    {
        let pred = ng.nn(OWL::DisjointWith);
        nary(f, ng, &self.0, pred)
    }
}

render_to_vec! {
    DisjointUnion, self, f, ng,
    {
        let c:PSubject<A> = (&self.0.0).into();
        let v = render_vec_subject(&self.1, f, ng)?;

        Ok(
            triples_to_vec!(
                f, c, ng.nn(OWL::DisjointUnionOf), v
            )
        )
    }
}

render_to_vec! {
    EquivalentClasses, self, f, ng,
    {
        let pred = ng.nn(OWL::EquivalentClass);
        nary(f, ng, &self.0, pred)
    }
}

render! {
    InverseObjectProperties, self, f, ng, PTriple,
    {
        Ok(
            triple!(
                f, &self.0.0, ng.nn(OWL::InverseOf), &self.1.0
            )
        )
    }
}

render_to_node! {
    ObjectPropertyExpression, self, f, ng,
    {
        Ok(
            match self {
                Self::ObjectProperty(op)
                    => (&op.0).into(),
                Self::InverseObjectProperty(op)
                    => {
                        let o:PTerm<_> = (&op.0).into();

                        triples_to_node!{
                            f, ng.bn(), ng.nn(OWL::InverseOf), o
                        }
                    }
            }
        )
    }
}

render! {
    SubObjectPropertyOf, self, f, ng, PTriple,
    {
        match &self.sub {
            SubObjectPropertyExpression::ObjectPropertyChain(v) => {
                // The subject and object are the other way around for
                // an object property chain from an object property
                // expression.
                //
                // It makes little sense to me.
                let s:PSubject<_> = (&self.sup).render(f, ng)?;
                let o = render_vec_subject(v, f, ng)?;
                Ok(
                    triple!{
                        f, s, ng.nn(OWL::PropertyChainAxiom), o
                    }
                )
            }
            SubObjectPropertyExpression::ObjectPropertyExpression(e) =>{
                let s:PSubject<_> = e.render(f, ng)?;
                let o:PTerm<_> = (&self.sup).render(f, ng)?.into();
                Ok(
                    triple!{
                        f, s, ng.nn(RDFS::SubPropertyOf), o
                    }
                )
            }
        }
    }
}

fn obj_prop_char<A: ForIRI, W: Write>(
    ob: &ObjectPropertyExpression<A>,
    f: &mut PrettyRdfXmlFormatter<A, W>,
    ng: &mut NodeGenerator<A>,
    chr: OWL,
) -> Result<PTriple<A>, HornedError> {
    let s: PSubject<_> = ob.render(f, ng)?;

    Ok(triple! {
        f, s, ng.nn(RDF::Type), ng.nn(chr)
    })
}

render! {
    TransitiveObjectProperty, self, f, ng, PTriple,
    {
       obj_prop_char(&self.0, f, ng, OWL::TransitiveProperty)
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

render_triple! {
    DeclareObjectProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::ObjectProperty)
}

render_triple! {
    DeclareDataProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::DatatypeProperty)
}

render_triple! {
    DeclareAnnotationProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::AnnotationProperty)
}

render_triple! {
    DeclareNamedIndividual, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::NamedIndividual)
}

render! {
    SubClassOf, self, f, ng, PTriple,
    {
        let sub:PSubject<_> = self.sub.render(f, ng)?;
        let obj:PTerm<_> = self.sup.render(f, ng)?.into();

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
    use std::{
        fs::File,
        io::{BufRead, BufReader, BufWriter},
        rc::Rc,
    };
    // use std::io::BufReader;
    // use std::io::BufWriter;

    fn read_ok<R: BufRead>(bufread: &mut R) -> SetOntology<RcStr> {
        let r = crate::io::rdf::reader::read(bufread, Default::default());
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        let (o, incomplete) = r.ok().unwrap();

        assert!(
            incomplete.is_complete(),
            "Read Not Complete: {:#?}",
            incomplete
        );
        o.into()
    }

    #[test]
    fn test_ont_rt() {
        let mut ont = ComponentMappedOntology::new_rc();
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

    fn roundtrip(ont: &str) -> (SetOntology<RcStr>, SetOntology<RcStr>) {
        let ont_orig = read_ok(&mut ont.as_bytes());
        let temp_file = Temp::new_file().unwrap();

        let file = File::create(&temp_file).ok().unwrap();
        let mut buf_writer = BufWriter::new(&file);

        let amo: ComponentMappedOntology<RcStr, Rc<AnnotatedComponent<RcStr>>> =
            ont_orig.clone().into();
        write(&mut buf_writer, &amo).ok().unwrap();
        buf_writer.flush().ok();
        let file = File::open(&temp_file).ok().unwrap();
        let ont_round = read_ok(&mut BufReader::new(&file));

        temp_file.release();

        (ont_orig, ont_round)
    }

    fn assert_round(ont: &str) -> (SetOntology<RcStr>, SetOntology<RcStr>) {
        let (ont_orig, ont_round) = roundtrip(ont);

        //println!("ont_orig\n{:#?}", ont_orig);
        //println!("ont_round\n{:#?}", ont_round);
        assert_eq!(ont_orig, ont_round);

        (ont_orig, ont_round)
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

    #[test]
    fn round_equivalent_class() {
        assert_round(include_str!("../../ont/owl-rdf/equivalent-class.owl"));
    }

    #[test]
    fn round_disjoint_class() {
        assert_round(include_str!("../../ont/owl-rdf/disjoint-class.owl"));
    }

    #[test]
    fn round_disjoint_union() {
        assert_round(include_str!("../../ont/owl-rdf/disjoint-union.owl"));
    }

    #[test]
    fn round_one_sub_property() {
        assert_round(include_str!("../../ont/owl-rdf/suboproperty.owl"));
    }

    #[test]
    fn round_one_inverse() {
        assert_round(include_str!("../../ont/owl-rdf/inverse-properties.owl"));
    }

    #[test]
    fn round_one_transitive() {
        assert_round(include_str!("../../ont/owl-rdf/transitive-properties.owl"));
    }

    #[test]
    fn round_one_annotated_transitive() {
        assert_round(include_str!(
            "../../ont/owl-rdf/annotation-on-transitive.owl"
        ));
    }

    #[test]
    fn round_subproperty_chain() {
        assert_round(include_str!("../../ont/owl-rdf/subproperty-chain.owl"));
    }

    #[test]
    fn round_one_subproperty_chain_with_inverse() {
        assert_round(include_str!(
            "../../ont/owl-rdf/subproperty-chain-with-inverse.owl"
        ));
    }

    #[test]
    fn round_annotation_on_annotation() {
        assert_round(include_str!(
            "../../ont/owl-rdf/annotation-with-annotation.owl"
        ));
    }

    #[test]
    fn round_sub_annotation() {
        assert_round(include_str!("../../ont/owl-rdf/sub-annotation.owl"));
    }

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

    #[test]
    fn object_has_value() {
        assert_round(include_str!("../../ont/owl-rdf/object-has-value.owl"));
    }

    #[test]
    fn object_one_of() {
        assert_round(include_str!("../../ont/owl-rdf/object-one-of.owl"));
    }

    #[test]
    fn inverse() {
        assert_round(include_str!("../../ont/owl-rdf/some-inverse.owl"));
    }

    #[test]
    fn object_unqualified_cardinality() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-unqualified-max-cardinality.owl"
        ));
    }

    #[test]
    fn object_min_cardinality() {
        assert_round(include_str!("../../ont/owl-rdf/object-min-cardinality.owl"));
    }

    #[test]
    fn object_max_cardinality() {
        assert_round(include_str!("../../ont/owl-rdf/object-max-cardinality.owl"));
    }

    #[test]
    fn object_exact_cardinality() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-exact-cardinality.owl"
        ));
    }

    #[test]
    fn datatype_alias() {
        assert_round(include_str!("../../ont/owl-rdf/datatype-alias.owl"));
    }

    #[test]
    fn datatype_intersection() {
        assert_round(include_str!("../../ont/owl-rdf/datatype-intersection.owl"));
    }

    #[test]
    fn datatype_union() {
        assert_round(include_str!("../../ont/owl-rdf/datatype-union.owl"));
    }

    #[test]
    fn datatype_complement() {
        assert_round(include_str!("../../ont/owl-rdf/datatype-complement.owl"));
    }

    #[test]
    fn datatype_oneof() {
        assert_round(include_str!("../../ont/owl-rdf/datatype-oneof.owl"));
    }

    #[test]
    fn datatype_some() {
        assert_round(include_str!("../../ont/owl-rdf/data-some.owl"));
    }

    #[test]
    fn facet_restriction() {
        assert_round(include_str!("../../ont/owl-rdf/facet-restriction.owl"));
    }

    #[test]
    fn data_only() {
        assert_round(include_str!("../../ont/owl-rdf/data-only.owl"));
    }

    #[test]
    fn data_exact_cardinality() {
        assert_round(include_str!("../../ont/owl-rdf/data-exact-cardinality.owl"));
    }

    #[test]
    fn data_has_value() {
        assert_round(include_str!("../../ont/owl-rdf/data-has-value.owl"));
    }

    #[test]
    fn data_max_cardinality() {
        assert_round(include_str!("../../ont/owl-rdf/data-max-cardinality.owl"));
    }

    #[test]
    fn data_min_cardinality() {
        assert_round(include_str!("../../ont/owl-rdf/data-min-cardinality.owl"));
    }

    #[test]
    fn class_assertion() {
        assert_round(include_str!("../../ont/owl-rdf/class-assertion.owl"));
    }

    #[test]
    fn data_property_assertion() {
        assert_round(include_str!(
            "../../ont/owl-rdf/data-property-assertion.owl"
        ));
    }

    #[test]
    fn same_individual() {
        assert_round(include_str!("../../ont/owl-rdf/same-individual.owl"));
    }

    #[test]
    fn different_individuals() {
        assert_round(include_str!("../../ont/owl-rdf/different-individual.owl"));
    }

    #[test]
    fn multi_different_individuals() {
        assert_round(include_str!(
            "../../ont/owl-rdf/multi-different-individual.owl"
        ));
    }

    #[test]
    fn negative_data_property_assertion() {
        assert_round(include_str!(
            "../../ont/owl-rdf/negative-data-property-assertion.owl"
        ));
    }

    #[test]
    fn negative_object_property_assertion() {
        assert_round(include_str!(
            "../../ont/owl-rdf/negative-object-property-assertion.owl"
        ));
    }

    #[test]
    fn object_property_assertion() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-property-assertion.owl"
        ));
    }

    #[test]
    fn data_has_key() {
        assert_round(include_str!("../../ont/owl-rdf/data-has-key.owl"));
    }

    #[test]
    fn data_property_disjoint() {
        assert_round(include_str!("../../ont/owl-rdf/data-property-disjoint.owl"));
    }

    #[test]
    fn data_property_domain() {
        assert_round(include_str!("../../ont/owl-rdf/data-property-domain.owl"));
    }

    #[test]
    fn data_property_equivalent() {
        assert_round(include_str!(
            "../../ont/owl-rdf/data-property-equivalent.owl"
        ));
    }

    #[test]
    fn data_property_functional() {
        assert_round(include_str!(
            "../../ont/owl-rdf/data-property-functional.owl"
        ));
    }

    #[test]
    fn data_property_range() {
        assert_round(include_str!("../../ont/owl-rdf/data-property-range.owl"));
    }

    #[test]
    fn data_property_sub() {
        assert_round(include_str!("../../ont/owl-rdf/data-property-sub.owl"));
    }

    #[test]
    fn disjoint_object_properties() {
        assert_round(include_str!(
            "../../ont/owl-rdf/disjoint-object-properties.owl"
        ));
    }

    #[test]
    fn equivalent_object_properties() {
        assert_round(include_str!(
            "../../ont/owl-rdf/equivalent-object-properties.owl"
        ));
    }

    #[test]
    fn object_has_key() {
        assert_round(include_str!("../../ont/owl-rdf/object-has-key.owl"));
    }

    #[test]
    fn object_has_key_complicated() {
        assert_round(include_str!("../../ont/owl-rdf/multi-has-key.owl"));
    }

    #[test]
    fn object_property_asymmetric() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-property-asymmetric.owl"
        ));
    }

    #[test]
    fn object_property_domain() {
        assert_round(include_str!("../../ont/owl-rdf/object-property-domain.owl"));
    }

    #[test]
    fn object_property_functional() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-property-functional.owl"
        ));
    }

    #[test]
    fn object_property_inverse_functional() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-property-inverse-functional.owl"
        ));
    }

    #[test]
    fn object_property_irreflexive() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-property-irreflexive.owl"
        ));
    }

    #[test]
    fn object_property_range() {
        assert_round(include_str!("../../ont/owl-rdf/object-property-range.owl"));
    }

    #[test]
    fn object_property_reflexive() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-property-reflexive.owl"
        ));
    }

    #[test]
    fn object_property_symmetric() {
        assert_round(include_str!(
            "../../ont/owl-rdf/object-property-symmetric.owl"
        ));
    }

    #[test]
    fn annotation_with_anonymous() {
        assert_round(include_str!(
            "../../ont/owl-rdf/annotation-with-anonymous.owl"
        ));
    }

    #[test]
    fn gci_and_other_class_relations() {
        assert_round(include_str!(
            "../../ont/owl-rdf/gci_and_other_class_relations.owl"
        ));
    }

    // #[test]
    // fn family() {
    //     assert_round(include_str!("../../ont/owl-rdf/family.owl"));
    // }
}
