use rio_api::{
    model::{BlankNode, NamedNode, Subject, Triple},
    parser::TriplesParser,
};
use Term::*;

use crate::{error::HornedError, io::ParserConfiguration, vocab::Facet};
use crate::{model::Literal, ontology::component_mapped::ComponentMappedOntology};
use crate::{model::*, vocab::Vocab};

use crate::ontology::indexed::ForIndex;
use crate::vocab::is_annotation_builtin;
use crate::vocab::OWL2Datatype;
use crate::vocab::OWL as VOWL;
use crate::vocab::RDF as VRDF;
use crate::vocab::SWRL as VSWRL;
use crate::{
    ontology::{
        declaration_mapped::DeclarationMappedIndex,
        indexed::ThreeIndexedOntology,
        logically_equal::{update_or_insert_logically_equal_component, LogicallyEqualIndex},
        set::{SetIndex, SetOntology},
    },
    resolve::strict_resolve_iri,
    vocab::RDFS as VRDFS,
};

use std::io::Cursor;
use std::{collections::BTreeSet, convert::TryFrom};
use std::{collections::HashMap, convert::TryInto};
use std::{io::BufRead, marker::PhantomData};

type RioTerm<'a> = ::rio_api::model::Term<'a>;

macro_rules! ok_some {
    ($body:expr) => {
        (if let Some(retn) = (|| Some($body))() {
            Ok(Some(retn))
        } else {
            Ok(None)
        })
    };
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct BNode<A: ForIRI>(A);

// The order of the variants in the enum is crucial for round-tripping.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Term<A: ForIRI> {
    OWL(VOWL),
    RDF(VRDF),
    RDFS(VRDFS),
    SWRL(VSWRL),
    FacetTerm(Facet),
    Iri(IRI<A>),
    BNode(BNode<A>),
    Literal(Literal<A>),
}

impl<A: ForIRI> From<&VOWL> for Term<A> {
    fn from(value: &VOWL) -> Self {
        Self::OWL(value.clone())
    }
}

impl<A: ForIRI> From<&VRDF> for Term<A> {
    fn from(value: &VRDF) -> Self {
        Self::RDF(value.clone())
    }
}

impl<A: ForIRI> From<&VRDFS> for Term<A> {
    fn from(value: &VRDFS) -> Self {
        Self::RDFS(value.clone())
    }
}

impl<A: ForIRI> From<&VSWRL> for Term<A> {
    fn from(value: &VSWRL) -> Self {
        Self::SWRL(value.clone())
    }
}

impl<A: ForIRI> From<&Facet> for Term<A> {
    fn from(value: &Facet) -> Self {
        Self::FacetTerm(value.clone())
    }
}

impl<A: ForIRI> From<IRI<A>> for Term<A> {
    fn from(value: IRI<A>) -> Self {
        Self::Iri(value)
    }
}

impl<A: ForIRI> From<BNode<A>> for Term<A> {
    fn from(value: BNode<A>) -> Self {
        Self::BNode(value)
    }
}

impl<A: ForIRI> From<Literal<A>> for Term<A> {
    fn from(value: Literal<A>) -> Self {
        Self::Literal(value)
    }
}

impl<A: ForIRI> TryFrom<&crate::vocab::Vocab> for Term<A> {
    type Error = HornedError;

    fn try_from(value: &crate::vocab::Vocab) -> Result<Self, Self::Error> {
        match value {
            crate::vocab::Vocab::Facet(facet) => Ok(facet.into()),
            crate::vocab::Vocab::RDF(rdf) => Ok(rdf.into()),
            crate::vocab::Vocab::RDFS(rdfs) => Ok(rdfs.into()),
            crate::vocab::Vocab::OWL(owl) => Ok(owl.into()),
            crate::vocab::Vocab::SWRL(swrl) => Ok(swrl.into()),
            _ => Err(HornedError::invalid(value.to_string())),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
enum OrTerm<A: ForIRI> {
    Term(Term<A>),
    ClassExpression(ClassExpression<A>),
}

impl<A: ForIRI> From<ClassExpression<A>> for OrTerm<A> {
    fn from(c: ClassExpression<A>) -> OrTerm<A> {
        OrTerm::ClassExpression(c)
    }
}

impl<A: ForIRI> From<Term<A>> for OrTerm<A> {
    fn from(t: Term<A>) -> OrTerm<A> {
        OrTerm::Term(t)
    }
}

impl<A: ForIRI> TryFrom<&NamedNode<'_>> for Term<A> {
    type Error = HornedError;

    fn try_from(value: &NamedNode<'_>) -> Result<Self, Self::Error> {
        if let Some(res) = Vocab::lookup(value.iri) {
            Term::try_from(res)
        } else {
            Err(HornedError::invalid(value.iri))
        }
    }
}

impl TryFrom<&NamedNode<'_>> for crate::vocab::XSD {
    type Error = HornedError;

    fn try_from(value: &NamedNode<'_>) -> Result<Self, Self::Error> {
        value.iri.parse::<Self>()
    }
}

impl<A: ForIRI> Build<A> {
    fn to_term_bn(nn: &BlankNode) -> Term<A> {
        Term::BNode(BNode(nn.id.to_string().into()))
    }

    fn to_pos_triple(&self, rio_triple: Triple, pos: usize) -> PosTriple<A> {
        PosTriple(
            [
                self.to_term_bnn(&rio_triple.subject),
                self.to_term_nn(&rio_triple.predicate),
                self.to_term(&rio_triple.object),
            ],
            pos,
        )
    }

    fn to_term(&self, t: &RioTerm) -> Term<A> {
        match t {
            rio_api::model::Term::NamedNode(iri) => self.to_term_nn(iri),
            rio_api::model::Term::BlankNode(id) => Self::to_term_bn(id),
            rio_api::model::Term::Literal(l) => self.to_term_lt(l),
            rio_api::model::Term::Triple(_) => {
                unimplemented!("Triple subjects are not implemented")
            }
        }
    }

    fn to_term_bnn(&self, subj: &Subject) -> Term<A> {
        match subj {
            Subject::NamedNode(nn) => self.to_term_nn(nn),
            Subject::BlankNode(bn) => Self::to_term_bn(bn),
            Subject::Triple(_) => unimplemented!("Triple subjects are not implemented"),
        }
    }

    fn to_term_nn(&self, nn: &NamedNode) -> Term<A> {
        if let Ok(term) = nn.try_into() {
            term
        } else {
            Term::Iri(self.iri(nn.iri))
        }
    }

    fn to_term_lt(&self, lt: &rio_api::model::Literal) -> Term<A> {
        match lt {
            rio_api::model::Literal::Simple { value } => Term::Literal(Literal::Simple {
                literal: value.to_string(),
            }),
            rio_api::model::Literal::LanguageTaggedString { value, language } => {
                Term::Literal(Literal::Language {
                    literal: value.to_string(),
                    lang: language.to_string(),
                })
            }
            rio_api::model::Literal::Typed { value, datatype } => {
                if let Ok(crate::vocab::XSD::String) = datatype.try_into() {
                    Term::Literal(Literal::Simple {
                        literal: value.to_string(),
                    })
                } else {
                    Term::Literal(Literal::Datatype {
                        literal: value.to_string(),
                        datatype_iri: self.iri(datatype.iri),
                    })
                }
            }
        }
    }
}

macro_rules! d {
    () => {
        Default::default()
    };
}

pub trait RDFOntology<A: ForIRI, AA: ForIndex<A>>:
    AsRef<LogicallyEqualIndex<A, AA>>
    + AsRef<DeclarationMappedIndex<A, AA>>
    + AsRef<SetIndex<A, AA>>
    + Default
    + MutableOntology<A>
{
}

impl<A: ForIRI, AA: ForIndex<A>, T: ?Sized> RDFOntology<A, AA> for T where
    T: AsRef<LogicallyEqualIndex<A, AA>>
        + AsRef<DeclarationMappedIndex<A, AA>>
        + AsRef<SetIndex<A, AA>>
        + Default
        + MutableOntology<A>
{
}

#[derive(Debug)]
#[allow(clippy::type_complexity)]
pub struct ConcreteRDFOntology<A: ForIRI, AA: ForIndex<A>>(
    ThreeIndexedOntology<
        A,
        AA,
        SetIndex<A, AA>,
        DeclarationMappedIndex<A, AA>,
        LogicallyEqualIndex<A, AA>,
    >,
);

impl<A: ForIRI, AA: ForIndex<A>> Default for ConcreteRDFOntology<A, AA> {
    fn default() -> Self {
        Self(Default::default())
    }
}

pub type ConcreteRcRDFOntology = ConcreteRDFOntology<RcStr, RcAnnotatedComponent>;

impl<A: ForIRI, AA: ForIndex<A>> ConcreteRDFOntology<A, AA> {
    pub fn i(&self) -> &SetIndex<A, AA> {
        self.0.i()
    }

    pub fn j(&self) -> &DeclarationMappedIndex<A, AA> {
        self.0.j()
    }

    pub fn k(&self) -> &LogicallyEqualIndex<A, AA> {
        self.0.k()
    }

    pub fn index(
        self,
    ) -> (
        SetIndex<A, AA>,
        DeclarationMappedIndex<A, AA>,
        LogicallyEqualIndex<A, AA>,
    ) {
        self.0.index()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> Ontology<A> for ConcreteRDFOntology<A, AA> {}

impl<A: ForIRI, AA: ForIndex<A>> MutableOntology<A> for ConcreteRDFOntology<A, AA> {
    fn insert<IAA>(&mut self, cmp: IAA) -> bool
    where
        IAA: Into<AnnotatedComponent<A>>,
    {
        self.0.insert(cmp)
    }

    fn take(&mut self, cmp: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>> {
        self.0.take(cmp)
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ConcreteRDFOntology<A, AA>> for SetOntology<A> {
    fn from(rdfo: ConcreteRDFOntology<A, AA>) -> SetOntology<A> {
        rdfo.index().0.into()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ConcreteRDFOntology<A, AA>>
    for ComponentMappedOntology<A, AA>
{
    fn from(rdfo: ConcreteRDFOntology<A, AA>) -> ComponentMappedOntology<A, AA> {
        let so: SetOntology<_> = rdfo.into();
        so.into()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> AsRef<DeclarationMappedIndex<A, AA>>
    for ConcreteRDFOntology<A, AA>
{
    fn as_ref(&self) -> &DeclarationMappedIndex<A, AA> {
        self.j()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> AsRef<LogicallyEqualIndex<A, AA>> for ConcreteRDFOntology<A, AA> {
    fn as_ref(&self) -> &LogicallyEqualIndex<A, AA> {
        self.k()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> AsRef<SetIndex<A, AA>> for ConcreteRDFOntology<A, AA> {
    fn as_ref(&self) -> &SetIndex<A, AA> {
        self.i()
    }
}

#[derive(Debug)]
enum OntologyParserState {
    New,
    Imports,
    Declarations,
    Parse,
}

#[derive(Debug, Default)]
pub struct IncompleteParse<A: ForIRI> {
    pub simple: Vec<PosTriple<A>>,
    pub bnode: Vec<VPosTriple<A>>,
    pub bnode_seq: Vec<Vec<Term<A>>>,

    pub class_expression: Vec<ClassExpression<A>>,
    pub object_property_expression: Vec<ObjectPropertyExpression<A>>,
    pub data_range: Vec<DataRange<A>>,
    pub atom: HashMap<Term<A>, Atom<A>>,

    pub ann_map: HashMap<[Term<A>; 3], BTreeSet<Annotation<A>>>,
}

impl<A: ForIRI> IncompleteParse<A> {
    pub fn is_complete(&self) -> bool {
        self.simple.is_empty()
            && self.bnode.is_empty()
            && self.bnode_seq.is_empty()
            && self.class_expression.is_empty()
            && self.object_property_expression.is_empty()
            && self.data_range.is_empty()
            && self.ann_map.is_empty()
            && self.atom.is_empty()
    }
}

#[derive(Clone, Debug)]
pub struct PosTriple<A: ForIRI>([Term<A>; 3], usize);

impl<A: ForIRI> From<[Term<A>; 3]> for PosTriple<A> {
    fn from(t: [Term<A>; 3]) -> PosTriple<A> {
        PosTriple(t, 0)
    }
}

#[derive(Debug)]
pub struct VPosTriple<A: ForIRI>(Vec<[Term<A>; 3]>, usize);

impl<A: ForIRI> IntoIterator for VPosTriple<A> {
    type Item = [Term<A>; 3];

    type IntoIter = std::vec::IntoIter<[Term<A>; 3]>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<A: ForIRI> std::ops::Deref for VPosTriple<A> {
    type Target = Vec<[Term<A>; 3]>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<A: ForIRI> std::ops::DerefMut for VPosTriple<A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

#[derive(Debug)]
pub struct OntologyParser<'a, A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>> {
    o: O,
    b: &'a Build<A>,
    config: ParserConfiguration,

    triple: Vec<PosTriple<A>>,
    simple: Vec<PosTriple<A>>,
    bnode: HashMap<BNode<A>, VPosTriple<A>>,
    bnode_seq: HashMap<BNode<A>, Vec<Term<A>>>,

    class_expression: HashMap<BNode<A>, ClassExpression<A>>,
    object_property_expression: HashMap<BNode<A>, ObjectPropertyExpression<A>>,
    data_range: HashMap<BNode<A>, DataRange<A>>,
    ann_map: HashMap<[Term<A>; 3], BTreeSet<Annotation<A>>>,
    atom: HashMap<Term<A>, Atom<A>>,
    variable: HashMap<IRI<A>, Variable<A>>,

    state: OntologyParserState,
    error: Result<(), HornedError>,
    p: PhantomData<AA>,
}

impl<'a, A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>> OntologyParser<'a, A, AA, O> {
    pub fn new(
        b: &'a Build<A>,
        triple: Vec<PosTriple<A>>,
        config: ParserConfiguration,
    ) -> OntologyParser<'a, A, AA, O> {
        OntologyParser {
            o: d!(),
            b,
            config,

            triple,
            simple: d!(),
            bnode: d!(),
            bnode_seq: d!(),
            class_expression: d!(),
            object_property_expression: d!(),
            data_range: d!(),
            ann_map: d!(),
            atom: d!(),
            variable: d!(),
            state: OntologyParserState::New,
            error: Ok(()),
            p: d!(),
        }
    }

    pub fn from_bufread<'b, R: BufRead>(
        b: &'a Build<A>,
        bufread: &'b mut R,
        config: ParserConfiguration,
    ) -> OntologyParser<'a, A, AA, O> {
        let mut parser = rio_xml::RdfXmlParser::new(bufread, None);
        let mut triples = vec![];
        let last_pos = std::cell::Cell::new(0);
        let mut on_triple = |rio_triple: rio_api::model::Triple| -> Result<_, HornedError> {
            triples.push(b.to_pos_triple(rio_triple, last_pos.get()));
            Ok(())
        };

        while !parser.is_end() {
            parser.parse_step(&mut on_triple).unwrap();
            last_pos.set(parser.buffer_position());
        }

        OntologyParser::new(b, triples, config)
    }

    pub fn from_doc_iri(
        b: &'a Build<A>,
        iri: &IRI<A>,
        config: ParserConfiguration,
    ) -> OntologyParser<'a, A, AA, O> {
        OntologyParser::from_bufread(
            b,
            &mut Cursor::new(strict_resolve_iri(iri).expect("the IRI should resolve successfully")),
            config,
        )
    }

    fn group_triples(
        triple: Vec<PosTriple<A>>,
        simple: &mut Vec<PosTriple<A>>,
        bnode: &mut HashMap<BNode<A>, VPosTriple<A>>,
    ) {
        // Next group together triples on a BNode, so we have
        // HashMap<BNodeID, Vec<[SpTerm; 3]> All of which should be
        // triples should begin with the BNodeId. We should be able to
        // gather these in a single pass.
        for t in triple {
            match t.0 {
                [_, Term::OWL(VOWL::DisjointWith), _]
                | [_, Term::OWL(VOWL::EquivalentClass), _]
                | [_, Term::OWL(VOWL::InverseOf), _]
                | [_, Term::RDFS(VRDFS::SubClassOf), _] => {
                    simple.push(t);
                }
                [Term::BNode(ref id), _, _] => {
                    let v = bnode
                        .entry(id.clone())
                        .or_insert_with(|| VPosTriple(vec![], t.1));
                    v.push(t.0)
                }
                _ => {
                    simple.push(t);
                }
            }
        }
    }

    fn stitch_seqs_1(&mut self) {
        let mut extended = false;

        for (k, v) in std::mem::take(&mut self.bnode) {
            match v.as_slice() {
                [[_, Term::RDF(VRDF::First), val],
                 [_, Term::RDF(VRDF::Rest), Term::BNode(bnode_id)],
                 // Some sequences have a Type List, some do not
                 ..
                ] => {
                    let some_seq = self.bnode_seq.remove(bnode_id);
                    if let Some(mut seq) = some_seq {
                        seq.push(val.clone());
                        self.bnode_seq.insert(k.clone(), seq);
                        extended = true;
                    } else {
                        self.bnode.insert(k, v);
                    }
                }
                _ => {
                    self.bnode.insert(k, v);
                }
            };
        }

        if extended && !self.bnode.is_empty() {
            self.stitch_seqs_1()
        }
    }

    fn stitch_seqs(&mut self) {
        for (k, v) in std::mem::take(&mut self.bnode) {
            match v.as_slice() {
                [[_, Term::RDF(VRDF::First), val],
                 [_, Term::RDF(VRDF::Rest), Term::RDF(VRDF::Nil)],
                 // Lists may or may not have a "list" RDF type
                 ..
                ] =>
                {
                    self.bnode_seq.insert(k.clone(), vec![val.clone()]);
                }
                _ => {
                    self.bnode.insert(k, v);
                }
            };
        }

        self.stitch_seqs_1();

        for (_, v) in self.bnode_seq.iter_mut() {
            v.reverse();
        }
    }

    fn resolve_imports(&mut self) -> Vec<IRI<A>> {
        let mut v = vec![];
        for t in std::mem::take(&mut self.simple) {
            match t.0 {
                [Term::Iri(_), Term::OWL(VOWL::Imports), Term::Iri(imp)] => {
                    v.push(imp.clone());
                    self.merge(AnnotatedComponent {
                        component: Import(imp).into(),
                        ann: BTreeSet::new(),
                    });
                }
                _ => self.simple.push(t),
            }
        }

        v
        // Section 3.1.2/table 4 of RDF Graphs
    }

    fn headers(&mut self) {
        //Section 3.1.2/table 4
        //   *:x rdf:type owl:Ontology .
        //[ *:x owl:versionIRI *:y .]
        let mut iri: Option<IRI<_>> = None;
        let mut viri: Option<IRI<_>> = None;

        for t in std::mem::take(&mut self.simple) {
            match t.0 {
                [Term::Iri(s), Term::RDF(VRDF::Type), Term::OWL(VOWL::Ontology)] => {
                    iri = Some(s.clone());
                }
                [Term::Iri(s), Term::OWL(VOWL::VersionIRI), Term::Iri(ob)]
                    if iri.as_ref() == Some(&s) =>
                {
                    viri = Some(ob.clone());
                }
                _ => self.simple.push(t),
            }
        }

        self.o.insert(OntologyID { iri, viri });
    }

    fn backward_compat(&mut self) {
        // Table 5, Table 6
    }

    fn parse_annotations(&self, triples: &[[Term<A>; 3]]) -> BTreeSet<Annotation<A>> {
        let mut ann = BTreeSet::default();
        for a in triples {
            ann.insert(self.annotation(a));
        }
        ann
    }

    fn annotation(&self, t: &[Term<A>; 3]) -> Annotation<A> {
        match t {
            // We assume that anything passed to here is an
            // annotation built in type
            [s, RDFS(rdfs), b] => {
                let iri = self.b.iri(rdfs.as_ref());
                self.annotation(&[s.clone(), Term::Iri(iri), b.clone()])
            }
            [s, OWL(owl), b] => {
                let iri = self.b.iri(owl.as_ref());
                self.annotation(&[s.clone(), Term::Iri(iri), b.clone()])
            }
            [_, Iri(p), ob @ Term::Literal(_)] => Annotation {
                ap: AnnotationProperty(p.clone()),
                av: self.fetch_literal(ob).unwrap().into(),
            },
            [_, Iri(p), Iri(ob)] => {
                // IRI annotation value
                Annotation {
                    ap: AnnotationProperty(p.clone()),
                    av: ob.clone().into(),
                }
            }
            [_, Iri(p), Term::BNode(bnodeid)] => Annotation {
                ap: AnnotationProperty(p.clone()),
                av: AnonymousIndividual(bnodeid.0.clone()).into(),
            },
            _ => {
                todo!()
            }
        }
    }

    fn merge<IAA: Into<AnnotatedComponent<A>>>(&mut self, cmp: IAA) {
        let cmp = cmp.into();
        update_or_insert_logically_equal_component(&mut self.o, cmp);
    }

    fn axiom_annotations(&mut self) {
        for (k, v) in std::mem::take(&mut self.bnode) {
            match v.as_slice() {
                [[_, Term::OWL(VOWL::AnnotatedProperty), p],//:
                 [_, Term::OWL(VOWL::AnnotatedSource), sb],//:
                 [_, Term::OWL(VOWL::AnnotatedTarget), ob],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Axiom)], ann @ ..] =>
                {
                    self.ann_map.insert(
                        [sb.clone(), p.clone(), ob.clone()],
                        self.parse_annotations(ann),
                    );
                }

                _ => {
                    self.bnode.insert(k, v);
                }
            }
        }
    }

    fn declarations(&mut self) {
        // Table 7
        for triple in std::mem::take(&mut self.simple) {
            let entity = match triple.0 {
                // TODO Change this into a single outer match
                [Term::Iri(ref s), Term::RDF(VRDF::Type), ref entity] => {
                    // TODO Move match into function
                    match entity {
                        Term::OWL(VOWL::Class) => Some(Class(s.clone()).into()),
                        Term::OWL(VOWL::ObjectProperty) => Some(ObjectProperty(s.clone()).into()),
                        Term::OWL(VOWL::AnnotationProperty) => {
                            Some(AnnotationProperty(s.clone()).into())
                        }
                        Term::OWL(VOWL::DatatypeProperty) => Some(DataProperty(s.clone()).into()),
                        Term::OWL(VOWL::NamedIndividual) => Some(NamedIndividual(s.clone()).into()),
                        Term::RDFS(VRDFS::Datatype) => Some(Datatype(s.clone()).into()),
                        _ => None,
                    }
                }
                _ => None,
            };

            if let Some(entity) = entity {
                let ann = self.ann_map.remove(&triple.0).unwrap_or_default();
                let ne: NamedOWLEntity<_> = entity;
                self.merge(AnnotatedComponent {
                    component: ne.into(),
                    ann,
                });
            } else {
                self.simple.push(triple);
            }
        }
    }

    fn data_ranges(&mut self) -> Result<(), HornedError> {
        let data_range_len = self.data_range.len();
        let mut facet_map: HashMap<Term<A>, PosTriple<A>> = HashMap::new();

        for (k, v) in std::mem::take(&mut self.bnode) {
            match v.as_slice() {
                [triple @ [_, Term::FacetTerm(_), _]] => {
                    facet_map.insert(Term::BNode(k), PosTriple(triple.clone(), v.1));
                }
                _ => {
                    self.bnode.insert(k, v);
                }
            }
        }

        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            let dr: Result<_, HornedError> = match v.as_slice() {
                [[_, Term::OWL(VOWL::IntersectionOf), Term::BNode(bnodeid)],//: rustfmt hard line!
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    ok_some! {
                        DataRange::DataIntersectionOf(
                            self.fetch_dr_seq(bnodeid)?
                        )
                    }
                }
                [[_, Term::OWL(VOWL::UnionOf), Term::BNode(bnodeid)],//: rustfmt hard line!
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    ok_some! {
                        DataRange::DataUnionOf(
                            self.fetch_dr_seq(bnodeid)?
                        )
                    }
                }
                [[_, Term::OWL(VOWL::DatatypeComplementOf), term],//:
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    ok_some! {
                      DataRange::DataComplementOf(
                            Box::new(self.fetch_dr(term)?)
                        )
                    }
                }
                [[_, Term::OWL(VOWL::OneOf), Term::BNode(bnode)],//:
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    ok_some! {
                        DataRange::DataOneOf(
                            self.fetch_literal_seq(bnode)?
                        )
                    }
                }
                [[_, Term::OWL(VOWL::OnDatatype), Term::Iri(iri)],//:
                 [_, Term::OWL(VOWL::WithRestrictions), Term::BNode(id)],//:
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    ok_some! {
                        {
                            let facet_seq = self.bnode_seq
                                .remove(id)?;
                            let some_facets =
                                facet_seq.into_iter().map(|id|
                                                          match facet_map.remove(&id)?.0 {
                                                              [_, Term::FacetTerm(facet), literal] => Some(
                                                                  FacetRestriction {
                                                                      f: facet,
                                                                      l: self.fetch_literal(&literal)?,
                                                                  }
                                                              ),
                                                              _ => None
                                                          }
                                );

                            let facets:Option<Vec<FacetRestriction<_>>> = some_facets.collect();
                            DataRange::DatatypeRestriction(
                                iri.into(),
                                facets?
                            )
                        }
                    }
                }
                _ => Ok(None),
            };

            if let Some(dr) = dr? {
                self.data_range.insert(this_bnode, dr);
            } else {
                self.bnode.insert(this_bnode, v);
            }
        }

        if self.data_range.len() > data_range_len {
            self.data_ranges()?;
        }

        // Shove any remaining facets back onto bnode so that they get
        // reported at the end
        self.bnode
            .extend(facet_map.into_iter().filter_map(|(k, v)| match k {
                Term::BNode(id) => Some((id, VPosTriple(vec![v.0], v.1))),
                _ => None,
            }));

        Ok(())
    }

    fn object_property_expressions(&mut self) {
        for t in std::mem::take(&mut self.simple) {
            match t.0 {
                [Term::BNode(bn), Term::OWL(VOWL::InverseOf), Term::Iri(iri)] => {
                    self.object_property_expression.insert(
                        bn.clone(),
                        ObjectPropertyExpression::InverseObjectProperty(iri.into()),
                    );
                }
                _ => {
                    self.simple.push(t);
                }
            }
        }
    }

    fn fetch_iri(&self, t: &Term<A>) -> Option<IRI<A>> {
        match t {
            Term::Iri(iri) => Some(iri.clone()),
            _ => None,
        }
    }

    fn fetch_sope(&mut self, t: &Term<A>, ic: &[&O]) -> Option<SubObjectPropertyExpression<A>> {
        Some(self.fetch_ope(t, ic)?.into())
    }

    fn fetch_ope(&mut self, t: &Term<A>, ic: &[&O]) -> Option<ObjectPropertyExpression<A>> {
        match self.find_property_kind(t, ic)? {
            PropertyExpression::ObjectPropertyExpression(ope) => Some(ope),
            _ => None,
        }
    }

    fn fetch_ap(&mut self, t: &Term<A>, ic: &[&O]) -> Option<AnnotationProperty<A>> {
        match self.find_property_kind(t, ic)? {
            PropertyExpression::AnnotationProperty(ap) => Some(ap),
            _ => None,
        }
    }

    fn fetch_dp(&mut self, t: &Term<A>, ic: &[&O]) -> Option<DataProperty<A>> {
        match self.find_property_kind(t, ic)? {
            PropertyExpression::DataProperty(dp) => Some(dp),
            _ => None,
        }
    }

    fn fetch_ce(&mut self, tce: &Term<A>) -> Option<ClassExpression<A>> {
        match tce {
            Term::Iri(cl) => Some(Class(cl.clone()).into()),
            Term::BNode(id) => self.class_expression.remove(id),
            _ => None,
        }
    }

    fn fetch_ce_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<ClassExpression<A>>> {
        if !self.bnode_seq.get(bnodeid)?.iter().all(|tce| match tce {
            Term::BNode(id) => self.class_expression.contains_key(id),
            _ => true,
        }) {
            return None;
        }

        self.bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .iter()
            .map(|tce| self.fetch_ce(tce))
            .collect()
    }

    fn fetch_ni_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<Individual<A>>> {
        self.bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .iter()
            .map(|t| self.fetch_iri(t).map(|iri| NamedIndividual(iri).into()))
            .collect()
    }

    fn fetch_dr_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<DataRange<A>>> {
        self.bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .iter()
            .map(|t| self.fetch_dr(t))
            .collect()
    }

    // TODO Fix code duplication
    fn fetch_literal_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<Literal<A>>> {
        self.bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .iter()
            .map(|t| self.fetch_literal(t))
            .collect()
    }

    // TODO Really, really fix code duplication
    fn fetch_atom_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<Atom<A>>> {
        self.bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .iter()
            .map(|t| self.atom.remove(t))
            .collect()
    }

    // TODO Really, really fix code duplication
    fn fetch_dargument_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<DArgument<A>>> {
        self.bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .iter()
            .map(|t| self.to_dargument(t))
            .collect()
    }

    fn fetch_dr(&mut self, t: &Term<A>) -> Option<DataRange<A>> {
        match t {
            Term::Iri(iri) => {
                let dt: Datatype<_> = iri.into();
                Some(dt.into())
            }
            Term::BNode(id) => self.data_range.remove(id),
            _ => todo!(),
        }
    }

    fn fetch_u32(&self, t: &Term<A>) -> Option<u32> {
        match t {
            Term::Literal(val) => val.literal().parse::<u32>().ok(),
            _ => None,
        }
    }

    fn fetch_literal(&self, t: &Term<A>) -> Option<Literal<A>> {
        match t {
            Term::Literal(ob) => Some(ob.clone()),
            _ => None,
        }
    }

    #[allow(clippy::wrong_self_convention)]
    fn to_iargument(&mut self, t: &Term<A>, ic: &[&O]) -> Option<IArgument<A>> {
        match t {
            Term::BNode(bn) => Some(IArgument::Individual(
                AnonymousIndividual(bn.0.clone()).into(),
            )),
            Term::Iri(iri) => self
                .variable
                .get(iri)
                .map(|var| var.clone().into())
                .or_else(|| {
                    if self.find_declaration_kind(iri, ic)
                        == Some(NamedOWLEntityKind::NamedIndividual)
                    {
                        Some(NamedIndividual(iri.clone()).into())
                    } else {
                        None
                    }
                }),
            _ => None,
        }
    }

    fn to_dargument(&self, t: &Term<A>) -> Option<DArgument<A>> {
        match t {
            Term::Literal(l) => Some(DArgument::Literal(l.clone())),
            Term::Iri(i) => self.variable.get(i).map(|v| DArgument::Variable(v.clone())),
            _ => None,
        }
    }

    fn find_term_kind(&mut self, term: &Term<A>, ic: &[&O]) -> Option<NamedOWLEntityKind> {
        match term {
            Term::Iri(iri) if crate::vocab::is_xsd_datatype(iri) => {
                Some(NamedOWLEntityKind::Datatype)
            }
            Term::Iri(iri) => self.find_declaration_kind(iri, ic),
            // TODO: this might be too general. At the moment, I am
            // only using this function to distinguish between a
            // datatype and an class
            _ => Some(NamedOWLEntityKind::Class),
        }
    }

    fn find_declaration_kind(&mut self, iri: &IRI<A>, ic: &[&O]) -> Option<NamedOWLEntityKind> {
        [&self.o]
            .iter()
            .chain(ic.iter())
            .map(|o| <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(o).declaration_kind(iri))
            .find(|d| d.is_some())
            .flatten()
    }

    fn find_property_kind(&mut self, term: &Term<A>, ic: &[&O]) -> Option<PropertyExpression<A>> {
        match term {
            Term::OWL(vowl) => {
                let iri = self.b.iri(vowl.as_ref());
                self.find_property_kind(&Term::Iri(iri), ic)
            }
            Term::Iri(iri) => match self.find_declaration_kind(iri, ic) {
                Some(NamedOWLEntityKind::AnnotationProperty) => {
                    Some(PropertyExpression::AnnotationProperty(iri.into()))
                }
                Some(NamedOWLEntityKind::DataProperty) => {
                    Some(PropertyExpression::DataProperty(iri.into()))
                }
                Some(NamedOWLEntityKind::ObjectProperty) => {
                    Some(PropertyExpression::ObjectPropertyExpression(iri.into()))
                }
                _ => None,
            },
            Term::BNode(id) => Some(self.object_property_expression.remove(id)?.into()),
            _ => None,
        }
    }

    fn class_expressions(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        let class_expression_len = self.class_expression.len();
        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            // rustfmt breaks this (putting the triples all on one
            // line) so skip
            let ce: Result<_, HornedError> = match v.as_slice() {
                [[_, Term::OWL(VOWL::OnProperty), pr],//:
                 [_, Term::OWL(VOWL::SomeValuesFrom), ce_or_dr],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ClassExpression::ObjectSomeValuesFrom {
                                    ope,
                                    bce: self.fetch_ce(ce_or_dr)?.into()
                                }
                            },
                            PropertyExpression::DataProperty(dp) => {
                                ClassExpression::DataSomeValuesFrom {
                                    dp,
                                    dr: self.fetch_dr(ce_or_dr)?
                                }
                            },
                            _ => panic!("Unexpected Property Kind")
                        }
                    }
                },
                [[_, Term::OWL(VOWL::HasValue), val],//:
                 [_, Term::OWL(VOWL::OnProperty), pr],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ClassExpression::ObjectHasValue {
                                    ope,
                                    i: NamedIndividual(self.fetch_iri(val)?).into()
                                }
                            },
                            PropertyExpression::DataProperty(dp) => {
                                ClassExpression::DataHasValue {
                                    dp,
                                    l: self.fetch_literal(val)?
                                }
                            }
                            _ => panic!("Unexpected Property kind"),
                        }
                    }
                },
                [[_, Term::OWL(VOWL::AllValuesFrom), ce_or_dr],//:
                 [_, Term::OWL(VOWL::OnProperty), pr],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ClassExpression::ObjectAllValuesFrom {
                                    ope,
                                    bce: self.fetch_ce(ce_or_dr)?.into()
                                }
                            },
                            PropertyExpression::DataProperty(dp) => {
                                ClassExpression::DataAllValuesFrom {
                                    dp,
                                    dr: self.fetch_dr(ce_or_dr)?
                                }
                            },
                            _ => panic!("Unexpected Property Kind")
                        }
                    }
                },
                [[_, Term::OWL(VOWL::OneOf), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    ok_some!{
                        ClassExpression::ObjectOneOf(
                            self.fetch_ni_seq(bnodeid)?
                        )
                    }
                 },
                 [[_, Term::OWL(VOWL::HasSelf), _],//:
                  [_, Term::OWL(VOWL::OnProperty), pr],
                  [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    ok_some!{
                        ClassExpression::ObjectHasSelf(
                            self.fetch_ope(pr, ic)?
                        )
                    }
                }
                [[_, Term::OWL(VOWL::IntersectionOf), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    ok_some!{
                        ClassExpression::ObjectIntersectionOf(
                            self.fetch_ce_seq(bnodeid)?
                        )
                    }
                },
                [[_, Term::OWL(VOWL::UnionOf), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    ok_some!{
                        ClassExpression::ObjectUnionOf(
                            self.fetch_ce_seq(
                                bnodeid,
                            )?
                        )
                    }
                },
                [[_, Term::OWL(VOWL::ComplementOf), tce],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    ok_some!{
                        ClassExpression::ObjectComplementOf(
                            self.fetch_ce(tce)?.into()
                        )
                    }
                },
                [[_, Term::OWL(VOWL::OnDataRange), dr],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::OWL(VOWL::QualifiedCardinality), literal],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        ClassExpression::DataExactCardinality
                        {
                            n:self.fetch_u32(literal)?,
                            dp: pr.into(),
                            dr: self.fetch_dr(dr)?
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MaxQualifiedCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnDataRange), dr],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        ClassExpression::DataMaxCardinality
                        {
                            n:self.fetch_u32(literal)?,
                            dp: pr.into(),
                            dr: self.fetch_dr(dr)?
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MinQualifiedCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnDataRange), dr],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        ClassExpression::DataMinCardinality
                        {
                            n:self.fetch_u32(literal)?,
                            dp: pr.into(),
                            dr: self.fetch_dr(dr)?
                        }
                    }
                }
                //_:x rdf:type owl:Restriction .
                //_:x owl:cardinality NN_INT(n) .
                //_:x owl:onProperty y .
                //{ OPE(y) ≠ ε }
                [[_, Term::OWL(VOWL::Cardinality), literal],//:
                 [_, Term::OWL(VOWL::OnProperty), pr],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ClassExpression::ObjectExactCardinality
                                {
                                    n:self.fetch_u32(literal)?,
                                    ope,
                                    bce: self.b.class(VOWL::Thing).into()
                                }
                            },
                            PropertyExpression::DataProperty(dp) => {
                                ClassExpression::DataExactCardinality
                                {
                                    n:self.fetch_u32(literal)?,
                                    dp,
                                    dr: self.b.datatype(OWL2Datatype::Literal).into(),
                                }
                            }
                            _ => {
                                todo!("Unexpected property kind")
                            }
                        }
                    }
                }
                [[_, Term::OWL(VOWL::OnClass), tce],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::OWL(VOWL::QualifiedCardinality), literal],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        ClassExpression::ObjectExactCardinality
                        {
                            n:self.fetch_u32(literal)?,
                            ope: pr.into(),
                            bce: self.fetch_ce(tce)?.into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MinCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        ClassExpression::ObjectMinCardinality
                        {
                            n:self.fetch_u32(literal)?,
                            ope: pr.into(),
                            bce: self.b.class(VOWL::Thing).into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MinQualifiedCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnClass), tce],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        ClassExpression::ObjectMinCardinality
                        {
                            n:self.fetch_u32(literal)?,
                            ope: pr.into(),
                            bce: self.fetch_ce(tce)?.into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MaxCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        ClassExpression::ObjectMaxCardinality
                        {
                            n:self.fetch_u32(literal)?,
                            ope: pr.into(),
                            bce: self.b.class(VOWL::Thing).into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MaxQualifiedCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnClass), tce],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    ok_some!{
                        ClassExpression::ObjectMaxCardinality
                        {
                            n:self.fetch_u32(literal)?,
                            ope: pr.into(),
                            bce: self.fetch_ce(tce)?.into()
                        }
                    }
                }
                _a => Ok(None),
            };

            if let Some(ce) = ce? {
                self.class_expression.insert(this_bnode, ce);
            } else {
                self.bnode.insert(this_bnode, v);
            }
        }

        if self.class_expression.len() > class_expression_len {
            self.class_expressions(ic)?
        }

        Ok(())
    }

    fn axioms(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        let mut single_bnodes = vec![];

        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            let axiom: Result<_, HornedError> = match v.as_slice() {
                [[_, Term::OWL(VOWL::AssertionProperty), pr],//:
                 [_, Term::OWL(VOWL::SourceIndividual), Term::Iri(i)],//:
                 [_, target_type, target],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::NegativePropertyAssertion)]] =>
                {
                    ok_some! {
                        match target_type {
                            Term::OWL(VOWL::TargetIndividual) =>
                                NegativeObjectPropertyAssertion {
                                    ope: self.fetch_ope(pr, ic)?,
                                    from: i.into(),
                                    to: self.fetch_iri(target)?.into(),
                                }.into(),
                            Term::OWL(VOWL::TargetValue) =>
                                NegativeDataPropertyAssertion {
                                    dp: self.fetch_dp(pr, ic)?,
                                    from: i.into(),
                                    to: self.fetch_literal(target)?,
                                }.into(),
                            _ => todo!()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::Members), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDifferent)]] =>
                {
                    ok_some! {
                        DifferentIndividuals (
                            self.fetch_ni_seq(bnodeid)?
                        ).into()
                    }
                }
                [[_, Term::OWL(VOWL::DistinctMembers), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDifferent)]] =>
                {
                    ok_some! {
                        DifferentIndividuals (
                            self.fetch_ni_seq(bnodeid)?
                        ).into()
                    }
                }
                _ => Ok(None),
            };

            if let Some(axiom) = axiom? {
                self.merge(AnnotatedComponent {
                    component: axiom,
                    ann: BTreeSet::new(),
                })
            } else if v.len() == 1 {
                single_bnodes.push(v[0].clone());
            } else {
                self.bnode.insert(this_bnode, v);
            }
        }

        for triple in std::mem::take(&mut self.simple)
            .into_iter()
            .chain(single_bnodes.into_iter().map(|t| t.into()))
        {
            let axiom: Result<_, HornedError> = match &triple.0 {
                [sub_tce, Term::RDFS(VRDFS::SubClassOf), sup_tce] => ok_some! {
                    SubClassOf {
                        sub: self.fetch_ce(sub_tce)?,
                        sup: self.fetch_ce(sup_tce)?,
                    }
                    .into()
                },
                // TODO: We need to check whether these
                // EquivalentClasses have any other EquivalentClasses
                // and add to that axiom
                [a, Term::OWL(VOWL::EquivalentClass), b] => match self.find_term_kind(a, ic) {
                    Some(NamedOWLEntityKind::Class) => ok_some! {
                        EquivalentClasses(
                            vec![
                                self.fetch_ce(a)?,
                                self.fetch_ce(b)?,
                            ]).into()
                    },
                    Some(NamedOWLEntityKind::Datatype) => {
                        if let Term::Iri(iri) = a {
                            ok_some! {
                                DatatypeDefinition{
                                    kind: iri.clone().into(),
                                    range: self.fetch_dr(b)?,
                                }.into()
                            }
                        } else {
                            Err(HornedError::invalid_at(
                                "Unexpected entity in equivalent datatype",
                                triple.1,
                            ))
                        }
                    }
                    _ => Err(HornedError::invalid_at(
                        format!(
                            "Unknown entity in equivalent class statement: {:?}",
                            triple.0
                        ),
                        triple.1,
                    )),
                },
                [class, Term::OWL(VOWL::HasKey), Term::BNode(bnodeid)] => {
                    ok_some! {
                        {
                            let vpe: Option<Vec<PropertyExpression<_>>> = self.bnode_seq
                                .remove(bnodeid)?
                                .into_iter()
                                .map(|pr| self.find_property_kind(&pr, ic))
                                .collect();

                            HasKey{
                                ce:self.fetch_ce(class)?,
                                vpe: vpe?
                            }.into()
                        }
                    }
                }
                [Term::Iri(iri), Term::OWL(VOWL::DisjointUnionOf), Term::BNode(bnodeid)] => {
                    ok_some! {
                        DisjointUnion(
                            Class(iri.clone()),
                            self.fetch_ce_seq(bnodeid)?
                        ).into()
                    }
                }
                [Term::Iri(p), Term::OWL(VOWL::InverseOf), Term::Iri(r)] => Ok(Some(
                    InverseObjectProperties(ObjectProperty(p.clone()), ObjectProperty(r.clone()))
                        .into(),
                )),
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::TransitiveProperty)] => {
                    ok_some! {
                        TransitiveObjectProperty(self.fetch_ope(pr, ic)?).into()
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::FunctionalProperty)] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                FunctionalObjectProperty(ope).into()
                            },
                            PropertyExpression::DataProperty(dp) => {
                                FunctionalDataProperty(dp).into()
                            },
                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::AsymmetricProperty)] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                AsymmetricObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::SymmetricProperty)] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                SymmetricObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::ReflexiveProperty)] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ReflexiveObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::IrreflexiveProperty)] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                IrreflexiveObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::InverseFunctionalProperty)] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                InverseFunctionalObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [Term::Iri(sub), Term::RDF(VRDF::Type), cls] => ok_some! {
                    {
                        ClassAssertion {
                            ce: self.fetch_ce(cls)?,
                            i: NamedIndividual(sub.clone()).into()
                        }.into()
                    }
                },
                [a, Term::OWL(VOWL::DisjointWith), b] => ok_some! {
                        DisjointClasses(vec![
                            self.fetch_ce(a)?,
                            self.fetch_ce(b)?
                        ]).into()
                },
                [pr, Term::RDFS(VRDFS::SubPropertyOf), t] => {
                    ok_some! {
                        match self.find_property_kind(t, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) =>
                                SubObjectPropertyOf {
                                    sup: ope,
                                    sub: self.fetch_sope(pr, ic)?,
                                }.into(),
                            PropertyExpression::DataProperty(dp) =>
                                SubDataPropertyOf {
                                    sup: dp,
                                    sub: self.fetch_dp(pr, ic)?
                                }.into(),
                            PropertyExpression::AnnotationProperty(ap) =>
                                SubAnnotationPropertyOf {
                                    sup: ap,
                                    sub: self.fetch_ap(pr, ic)?
                                }.into(),
                        }
                    }
                }
                [Term::Iri(pr), Term::OWL(VOWL::PropertyChainAxiom), Term::BNode(id)] => {
                    ok_some! {
                        SubObjectPropertyOf {
                            sub: SubObjectPropertyExpression::ObjectPropertyChain(
                                self.bnode_seq
                                    .remove(id)?
                                    .iter()
                                    .map(|t| self.fetch_ope(t, ic).unwrap())
                                    .collect()
                            ),
                            sup: ObjectProperty(pr.clone()).into(),
                        }.into()
                    }
                }
                [pr, Term::RDFS(VRDFS::Domain), t] => {
                    ok_some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => ObjectPropertyDomain {
                                ope,
                                ce: self.fetch_ce(t)?,
                            }
                            .into(),
                            PropertyExpression::DataProperty(dp) => DataPropertyDomain {
                                dp,
                                ce: self.fetch_ce(t)?,
                            }
                            .into(),
                            PropertyExpression::AnnotationProperty(ap) => AnnotationPropertyDomain {
                                ap,
                                iri: self.fetch_iri(t)?,
                            }
                            .into(),
                        }
                    }
                }
                [pr, Term::RDFS(VRDFS::Range), t] => ok_some! {
                    match self.find_property_kind(pr, ic)? {
                        PropertyExpression::ObjectPropertyExpression(ope) => ObjectPropertyRange {
                            ope,
                            ce: self.fetch_ce(t)?,
                        }
                        .into(),
                        PropertyExpression::DataProperty(dp) => DataPropertyRange {
                            dp,
                            dr: self.fetch_dr(t)?,
                        }
                        .into(),
                        PropertyExpression::AnnotationProperty(ap) => AnnotationPropertyRange {
                            ap,
                            iri: self.fetch_iri(t)?,
                        }
                        .into(),
                    }
                },
                [r, Term::OWL(VOWL::PropertyDisjointWith), s] => ok_some! {
                    match self.find_property_kind(r, ic)? {
                        PropertyExpression::ObjectPropertyExpression(ope) => DisjointObjectProperties (
                            vec![ope, self.fetch_ope(s, ic)?]
                        )
                        .into(),
                        PropertyExpression::DataProperty(dp) => DisjointDataProperties (
                            vec![dp, self.fetch_dp(s, ic)?]
                        )
                            .into(),
                        _ => todo!()
                    }
                },
                [r, Term::OWL(VOWL::EquivalentProperty), s] => ok_some! {
                    match self.find_property_kind(r, ic)? {
                        PropertyExpression::ObjectPropertyExpression(ope) => EquivalentObjectProperties (
                            vec![ope, self.fetch_ope(s, ic)?]
                        )
                        .into(),
                        PropertyExpression::DataProperty(dp) => EquivalentDataProperties (
                            vec![dp, self.fetch_dp(s, ic)?]
                        )
                        .into(),
                        _ => todo!()
                    }
                },
                [Term::Iri(sub), Term::OWL(VOWL::SameAs), Term::Iri(obj)] => {
                    Ok(Some(SameIndividual(vec![sub.into(), obj.into()]).into()))
                }
                [Term::Iri(i), Term::OWL(VOWL::DifferentFrom), Term::Iri(j)] => {
                    Ok(Some(DifferentIndividuals(vec![i.into(), j.into()]).into()))
                }
                [Term::Iri(sub), Term::Iri(pred), t @ Term::Literal(_)] => ok_some! {
                    match (self.find_declaration_kind(sub, ic)?,
                           self.find_declaration_kind(pred, ic)?) {
                        (NamedOWLEntityKind::NamedIndividual,
                         NamedOWLEntityKind::DataProperty) => {
                            DataPropertyAssertion {
                                dp: pred.clone().into(),
                                from: sub.into(),
                                to: self.fetch_literal(t)?
                            }.into()
                        }
                        _ => {
                            return None;
                            //todo!()
                        }
                    }
                },
                [Term::Iri(sub), Term::Iri(pred), Term::Iri(obj)] => ok_some! {
                    match (self.find_declaration_kind(sub, ic)?,
                           self.find_declaration_kind(pred, ic)?,
                           self.find_declaration_kind(obj, ic)?) {
                        (NamedOWLEntityKind::NamedIndividual,
                         NamedOWLEntityKind::ObjectProperty,
                         NamedOWLEntityKind::NamedIndividual) => {
                            ObjectPropertyAssertion {
                                ope: ObjectProperty(pred.clone()).into(),
                                from: sub.into(),
                                to: obj.into()
                            }.into()
                        }
                        _ => todo!()
                    }
                },
                _ => Ok(None),
            };

            if let Some(axiom) = axiom? {
                let ann = self.ann_map.remove(&triple.0).unwrap_or_default();
                self.merge(AnnotatedComponent {
                    component: axiom,
                    ann,
                })
            } else {
                self.simple.push(triple)
            }
        }

        Ok(())
    }

    fn swrl(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        // identify variables first
        for triple in std::mem::take(&mut self.simple) {
            match &triple.0 {
                [Term::Iri(s), Term::RDF(VRDF::Type), Term::SWRL(VSWRL::Variable)] => {
                    self.variable.insert(s.clone(), Variable(s.clone()));
                }
                _ => {
                    self.simple.push(triple);
                }
            }
        }

        // Next identify the atoms with a big pattern matcher over bnodes
        for (bnode, triple) in std::mem::take(&mut self.bnode) {
            let atom: Result<_, HornedError> = match triple.as_slice() {
                [[_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::ClassAtom)], [_, Term::SWRL(VSWRL::Argument1), arg], [_, Term::SWRL(VSWRL::ClassPredicate), pred]] =>
                {
                    ok_some! {
                        {
                            Atom::ClassAtom{
                                pred: self.fetch_ce(pred)?,
                                arg: self.to_iargument(arg, ic)?
                            }
                        }
                    }
                }
                [[_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::DataRangeAtom)], [_, Term::SWRL(VSWRL::Argument1), arg], [_, Term::SWRL(VSWRL::DataRange), pred]] =>
                {
                    ok_some! {
                        {
                            Atom::DataRangeAtom{
                                pred: self.fetch_dr(pred)?,
                                arg: self.to_dargument(arg)?
                            }
                        }
                    }
                }
                [[_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::IndividualPropertyAtom)], [_, Term::SWRL(VSWRL::Argument1), arg1], [_, Term::SWRL(VSWRL::Argument2), arg2], [_, Term::SWRL(VSWRL::PropertyPredicate), pred]] =>
                {
                    ok_some! {
                        match self.find_property_kind(pred, ic)? {
                            PropertyExpression::ObjectPropertyExpression(pred) => {
                                Atom::ObjectPropertyAtom{
                                    pred,
                                    args: (
                                        self.to_iargument(arg1, ic)?,
                                        self.to_iargument(arg2, ic)?,
                                    )
                                }
                            }
                            _=> todo!()
                        }
                    }
                }
                [[_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::DatavaluedPropertyAtom)], [_, Term::SWRL(VSWRL::Argument1), arg1], [_, Term::SWRL(VSWRL::Argument2), arg2], [_, Term::SWRL(VSWRL::PropertyPredicate), pred]] =>
                {
                    ok_some! {
                        Atom::DataPropertyAtom {
                            pred: self.fetch_dp(pred, ic)?,
                            args: (
                                self.to_dargument(arg1)?,
                                self.to_dargument(arg2)?,
                            )
                        }
                    }
                }
                [[_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::DifferentIndividualsAtom)], [_, Term::SWRL(VSWRL::Argument1), arg1], [_, Term::SWRL(VSWRL::Argument2), arg2]] =>
                {
                    ok_some! {
                        Atom::DifferentIndividualsAtom(
                            self.to_iargument(arg1, ic)?,
                            self.to_iargument(arg2, ic)?,
                        )
                    }
                }
                [[_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::SameIndividualAtom)], [_, Term::SWRL(VSWRL::Argument1), arg1], [_, Term::SWRL(VSWRL::Argument2), arg2]] =>
                {
                    ok_some! {
                        Atom::SameIndividualAtom(
                            self.to_iargument(arg1, ic)?,
                            self.to_iargument(arg2, ic)?,
                        )
                    }
                }
                [[_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::BuiltinAtom)], [_, Term::SWRL(VSWRL::Arguments), Term::BNode(args)], [_, Term::SWRL(VSWRL::Builtin), Term::Iri(iri)]] =>
                {
                    ok_some! {
                        Atom::BuiltInAtom{
                            pred: iri.clone(),
                            args: self.fetch_dargument_seq(args)?
                        }
                    }
                }
                _ => Ok(None),
            };

            if let Some(atom) = atom? {
                self.atom.insert(Term::BNode(bnode), atom);
            } else {
                self.bnode.insert(bnode, triple);
            }
        }

        // now identfy the rules using "imp" over the bnodes, we
        // should have everything else in place by then to build the
        // entire rule
        for (bnode, triple) in std::mem::take(&mut self.bnode) {
            let rule: Result<_, HornedError> = match triple.as_slice() {
                [[_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::Imp)], [_, Term::SWRL(VSWRL::Body), Term::BNode(body_bn)], [_, Term::SWRL(VSWRL::Head), Term::BNode(head_bn)]] =>
                {
                    ok_some! {
                        Rule {
                            head: self.fetch_atom_seq(head_bn)?,
                            body: self.fetch_atom_seq(body_bn)?,
                        }
                    }
                }
                _ => Ok(None),
            };

            if let Some(rule) = rule? {
                self.merge(rule);
            } else {
                self.bnode.insert(bnode, triple);
            }
        }

        Ok(())
    }

    fn simple_annotations(&mut self, parse_all: bool) {
        let ont_id = <O as AsRef<SetIndex<A, AA>>>::as_ref(&self.o).the_ontology_id_or_default();
        for triple in std::mem::take(&mut self.simple) {
            let firi = |s: &mut OntologyParser<_, _, _>, t, iri: &IRI<_>| {
                let ann = s.ann_map.remove(t).unwrap_or_default();
                s.merge(AnnotatedComponent {
                    component: AnnotationAssertion {
                        subject: iri.into(),
                        ann: s.annotation(t),
                    }
                    .into(),
                    ann,
                })
            };

            match &triple.0 {
                // Catch anything about the ontology and assume it is
                // an annotation. Some versions of the OWL API do not
                // declare annotation properties for ontology annotations
                [Term::Iri(iri), _, _] if ont_id.iri.as_ref() == Some(iri) => {
                    self.o
                        .insert(OntologyAnnotation(self.annotation(&triple.0)));
                }
                [Term::Iri(iri), Term::RDFS(rdfs), _] if rdfs.is_builtin() => {
                    firi(self, &triple.0, iri)
                }
                [Term::Iri(iri), Term::Iri(ap), _]
                    if parse_all
                        || <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&self.o)
                            .is_annotation_property(ap)
                        || is_annotation_builtin(ap.as_ref()) =>
                {
                    firi(self, &triple.0, iri)
                }
                _ => {
                    self.simple.push(triple);
                }
            }
        }
        for (k, v) in std::mem::take(&mut self.bnode) {
            let fbnode = |s: &mut OntologyParser<_, _, _>, t, ind: &BNode<A>| {
                let ann = s.ann_map.remove(t).unwrap_or_default();
                let ind: AnonymousIndividual<A> = s.b.anon(ind.0.clone());
                s.merge(AnnotatedComponent {
                    component: AnnotationAssertion {
                        subject: ind.into(),
                        ann: s.annotation(t),
                    }
                    .into(),
                    ann,
                })
            };

            match v.as_slice() {
                [triple @ [Term::BNode(ind), Term::RDFS(rdfs), _]] if rdfs.is_builtin() => {
                    fbnode(self, triple, ind)
                }
                [triple @ [Term::BNode(ind), Term::Iri(ap), _]]
                    if parse_all
                        || <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&self.o)
                            .is_annotation_property(ap)
                        || is_annotation_builtin(ap) =>
                {
                    fbnode(self, triple, ind)
                }
                _ => {
                    self.bnode.insert(k, v);
                }
            }
        }
    }

    /// Parse all imports and add to the Ontology.
    /// Return an error is we are in the wrong state
    pub fn parse_imports(&mut self) -> Result<Vec<IRI<A>>, HornedError> {
        match self.state {
            OntologyParserState::New => {
                let triple = std::mem::take(&mut self.triple);
                Self::group_triples(triple, &mut self.simple, &mut self.bnode);

                // sort the triples, so that I can get a dependable order
                for (_, vec) in self.bnode.iter_mut() {
                    vec.sort();
                }

                self.stitch_seqs();

                // Table 10
                self.axiom_annotations();
                let v = self.resolve_imports();
                self.state = OntologyParserState::Imports;

                Ok(v)
            }
            _ => todo!(),
        }
    }

    /// Parse all declarations and add to the ontology.
    /// HornedError if we are not in the right state
    pub fn parse_declarations(&mut self) -> Result<(), HornedError> {
        match self.state {
            OntologyParserState::New => {
                self.parse_imports().and_then(|_| self.parse_declarations())
            }
            OntologyParserState::Imports => {
                self.backward_compat();

                // for t in bnode.values() {
                //     match t.as_slice()[0] {
                //         [BNode(s), RDF(VRDF::First), ob] => {
                //             //let v = vec![];
                //             // So, we have captured first (value of which is ob)
                //             // Rest of the sequence could be either in
                //             // bnode_seq or in bnode -- confusing
                //             //bnode_seq.insert(s.clone(), self.seq())
                //         }
                //     }
                // }

                // Then handle SEQ this should give HashMap<BNodeID,
                // Vec<[SpTerm]> where the BNodeID is the first node of the
                // seq, and the SpTerms are the next in order. This will
                // require multiple passes through the triples (This is Table
                // 3 in the structural Specification)

                // At this point we should have everything we need to be able
                // to make all the entities that we need, already grouped into
                // a place we can access it.

                // Now we work through the tables in the RDF serialization

                // Table 4: headers. To do this fully requires imports also,
                // but we need to fudge this a little. We need to be to able
                // to read an ontology just for declarations. At the moment, I
                // don't know how to get to another set of triples for these
                // -- we will need some kind of factory.
                self.headers();

                // Can we pull out annotations at this point and handle them
                // as we do in reader2? Transform them into a triple which we
                // handle normally, then bung the annotation on later?

                // Table 5: Backward compatibility -- skip this for now (maybe
                // for ever)

                // Table 6: Don't understand this

                // Table 7: Declarations (this should be simple, if we have a
                // generic solution for handling annotations, there is no
                // handling of bnodes).
                self.declarations();
                self.state = OntologyParserState::Declarations;
                Ok(())
            }
            _ => {
                todo!();
            }
        }
    }

    /// Complete the parse of the ontology.
    ///
    /// ic is a Vec of references to the import closure. These RDF
    /// ontologies do not need to be completely parsed, but will be
    /// relied on to resolve declarations.
    pub fn finish_parse(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        // Table 10
        self.simple_annotations(false);

        self.data_ranges()?;

        // Table 8:
        self.object_property_expressions();

        // Table 13: Parsing of Class Expressions
        self.class_expressions(ic)?;

        // Table 16: Axioms without annotations
        self.axioms(ic)?;

        // SWRL rules
        self.swrl(ic)?;

        if self.config.rdf.lax {
            self.simple_annotations(true);
        }
        self.state = OntologyParserState::Parse;
        Ok(())
    }

    pub fn parse(mut self) -> Result<(O, IncompleteParse<A>), HornedError> {
        if self.error.is_err() {
            return Err(self.error.unwrap_err());
        }

        match self.state {
            OntologyParserState::New => {
                // Ditch the vec that this might return as we don't
                // need it!
                self.error = self.parse_imports().and(Ok(()));
                self.parse()
            }
            OntologyParserState::Imports => {
                self.error = self.parse_declarations();
                self.parse()
            }
            OntologyParserState::Declarations => {
                self.error = self.finish_parse(vec![].as_slice());
                self.parse()
            }
            OntologyParserState::Parse => self.as_ontology_and_incomplete(),
        }
    }

    pub fn ontology_ref(&self) -> &O {
        &self.o
    }

    pub fn mut_ontology_ref(&mut self) -> &mut O {
        &mut self.o
    }

    /// Consume the parser and return an Ontology.
    pub fn as_ontology(self) -> Result<O, HornedError> {
        self.error.and(Ok(self.o))
    }

    /// Consume the parser and return an Ontology and any data
    /// structures that have not been fully parsed
    pub fn as_ontology_and_incomplete(mut self) -> Result<(O, IncompleteParse<A>), HornedError> {
        if self.error.is_err() {
            return Err(self.error.unwrap_err());
        }

        // Regroup so that they print out nicer
        let mut simple = vec![];

        Self::group_triples(
            std::mem::take(&mut self.simple),
            &mut simple,
            &mut self.bnode,
        );

        let bnode: Vec<_> = self.bnode.into_values().collect();
        let bnode_seq: Vec<_> = self.bnode_seq.into_values().collect();
        let class_expression: Vec<_> = self.class_expression.into_values().collect();
        let object_property_expression: Vec<_> =
            self.object_property_expression.into_values().collect();
        let data_range = self.data_range.into_values().collect();

        Ok((
            self.o,
            IncompleteParse {
                simple,
                bnode,
                bnode_seq,
                class_expression,
                object_property_expression,
                data_range,
                ann_map: self.ann_map,
                atom: self.atom,
            },
        ))
    }
}

pub fn parser_with_build<'b, A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>, R: BufRead>(
    bufread: &mut R,
    build: &'b Build<A>,
    config: ParserConfiguration,
) -> OntologyParser<'b, A, AA, O> {
    OntologyParser::from_bufread(build, bufread, config)
}

pub fn read_with_build<A: ForIRI, AA: ForIndex<A>, R: BufRead>(
    bufread: &mut R,
    build: &Build<A>,
    config: ParserConfiguration,
) -> Result<(ConcreteRDFOntology<A, AA>, IncompleteParse<A>), HornedError> {
    parser_with_build(bufread, build, config).parse()
}

pub fn read<R: BufRead>(
    bufread: &mut R,
    config: ParserConfiguration,
) -> Result<
    (
        ConcreteRDFOntology<RcStr, RcAnnotatedComponent>,
        IncompleteParse<RcStr>,
    ),
    HornedError,
> {
    let b = Build::new_rc();
    read_with_build(bufread, &b, config)
}

#[cfg(test)]
mod test {
    use super::*;

    use std::path::PathBuf;
    use std::rc::Rc;

    use crate::io::RDFParserConfiguration;
    use crate::normalize::normalize;
    use crate::ontology::component_mapped::RcComponentMappedOntology;
    use pretty_assertions::assert_eq;
    use test_generator::test_resources;

    fn read_ok<R: BufRead>(
        bufread: &mut R,
    ) -> ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>> {
        let r = read(bufread, Default::default());

        if let Err(e) = r {
            panic!("Expected ontology, get failure: {:?}", e,);
        }

        let (ont, incomp) = r.unwrap();
        dbg!(&ont, &incomp);
        assert!(incomp.is_complete());
        ont
    }

    fn compare(test: &str) {
        let dot = test.rfind('.').unwrap();
        let slash = test.rfind('/').unwrap();
        let stem = &test[(slash + 1)..dot];

        compare_two(stem, stem);
    }

    fn compare_two(testrdf: &str, testowl: &str) {
        let dir_path_buf = PathBuf::from(file!());
        let dir = dir_path_buf.parent().unwrap().to_string_lossy();

        compare_str(
            &slurp::read_all_to_string(format!("{}/../../ont/owl-rdf/{}.owl", dir, testrdf))
                .unwrap(),
            &slurp::read_all_to_string(format!("{}/../../ont/owl-xml/{}.owx", dir, testowl))
                .unwrap(),
        );
    }

    fn slurp_rdfont(testrdf: &str) -> std::string::String {
        let dir_path_buf = PathBuf::from(file!());
        let dir = dir_path_buf.parent().unwrap().to_string_lossy();

        slurp::read_all_to_string(format!("{}/../../ont/owl-rdf/{}.owl", dir, testrdf)).unwrap()
    }

    fn compare_str(rdfread: &str, xmlread: &str) {
        let rdfont: SetOntology<_> = read_ok(&mut rdfread.as_bytes()).into();
        let xmlont: SetOntology<_> = crate::io::owx::reader::test::read_ok(&mut xmlread.as_bytes())
            .0
            .into();

        let rdfont = normalize(rdfont.into_iter().collect());
        let xmlont = normalize(xmlont.into_iter().collect());
        dbg!(&xmlont);
        dbg!(&rdfont);
        assert_eq!(rdfont, xmlont);
    }

    // #[test]
    // fn read_iri() {
    //     let dir_path_buf = PathBuf::from(file!());
    //     let dir = dir_path_buf.parent().unwrap()
    //         .parent().unwrap()
    //         .parent().unwrap();
    //     let cdir = dir.canonicalize().unwrap();
    //     let b = Build::new();
    //     let i:IRI = b.iri(
    //         format!("file://{}/ont/owl-rdf/and.owl", cdir.to_string_lossy())
    //     );

    //     let op = OntologyParser::from_doc_iri(&b, &i);
    //     let _o = op.parse().unwrap();
    //     assert!(true);
    // }

    #[test_resources("src/ont/owl-rdf/*.owl")]
    fn compare_to_xml(resource: &str) {
        compare(resource)
    }

    #[test_resources("src/ont/owl-rdf/ambiguous/*.owl")]
    fn test_read_ok(resource: &str) {
        let resource = &slurp::read_all_to_string(resource).unwrap();

        read_ok(&mut resource.as_bytes());
    }

    #[test]
    fn one_some_reversed() {
        compare_two("manual/one-some-reversed-triples", "some");
    }

    #[test]
    fn one_some_property_filler_reversed() {
        compare_two("manual/one-some-property-filler-reversed", "some");
    }

    #[test]
    fn annotation_missing_declaration() {
        let ont_s = slurp_rdfont("manual/annotation_no_declaration");

        let ont_strict = read(&mut ont_s.as_bytes(), Default::default());
        let ont_lax = read(
            &mut ont_s.as_bytes(),
            ParserConfiguration {
                rdf: RDFParserConfiguration { lax: true },
                ..Default::default()
            },
        );

        // Both should parse without error
        let (ont_strict, incomp_strict) = ont_strict.unwrap();
        let (ont_lax, incomp_lax) = ont_lax.unwrap();

        // Strict parsing should be incomplete
        assert!(!incomp_strict.is_complete());
        assert!(incomp_lax.is_complete());

        let ont_strict: ComponentMappedOntology<_, _> = ont_strict.into();
        let ont_lax: ComponentMappedOntology<_, _> = ont_lax.into();

        // strict mode should have no annotation assertions or
        // annotation properties
        assert_eq!(ont_strict.i().annotation_assertion().count(), 0);
        assert_eq!(ont_strict.i().declare_annotation_property().count(), 0);

        // lax mode should have one annotation but this annotation
        // should cause the declaration of the annotation property
        assert_eq!(ont_lax.i().annotation_assertion().count(), 1);
        assert_eq!(ont_lax.i().declare_annotation_property().count(), 0);
    }

    #[test]
    fn broken_ontology_annotation() {
        // Some verisons of the OWL API do not include an
        // AnnotationProperty declaration. We should make this work.
        let ont: SetOntology<_> =
            read_ok(&mut slurp_rdfont("manual/broken-ontology-annotation").as_bytes()).into();
        let ont: ComponentMappedOntology<_, RcAnnotatedComponent> = ont.into();
        assert_eq!(ont.i().ontology_annotation().count(), 1);
        assert_eq!(ont.i().declare_annotation_property().count(), 0);
    }

    #[test]
    fn import_with_partial_parse() {
        let b = Build::new_rc();
        let mut p: OntologyParser<_, Rc<AnnotatedComponent<RcStr>>, ConcreteRDFOntology<_, _>> =
            parser_with_build(
                &mut slurp_rdfont("import").as_bytes(),
                &b,
                Default::default(),
            );
        let _ = p.parse_imports();

        let rdfont = p.as_ontology().unwrap();
        let so: SetOntology<_> = rdfont.into();
        let amont: RcComponentMappedOntology = so.into();
        assert_eq!(amont.i().import().count(), 1);
    }

    #[test]
    fn declaration_with_partial_parse() {
        let b = Build::new_rc();

        let mut p: OntologyParser<_, Rc<AnnotatedComponent<RcStr>>, ConcreteRDFOntology<_, _>> =
            parser_with_build(
                &mut slurp_rdfont("class").as_bytes(),
                &b,
                Default::default(),
            );
        let _ = p.parse_declarations();

        let rdfont = p.as_ontology().unwrap();
        let so: SetOntology<_> = rdfont.into();
        let amont: RcComponentMappedOntology = so.into();
        assert_eq!(amont.i().declare_class().count(), 1);
    }

    #[test]
    fn import_property_in_bits() -> Result<(), HornedError> {
        let b = Build::new_rc();
        let p: OntologyParser<_, Rc<AnnotatedComponent<RcStr>>, ConcreteRDFOntology<_, _>> =
            parser_with_build(
                &mut slurp_rdfont("withimport/other-property").as_bytes(),
                &b,
                Default::default(),
            );
        let (family_other, incomplete) = p.parse()?;
        assert!(incomplete.is_complete());

        let mut p = parser_with_build(
            &mut slurp_rdfont("withimport/import-property").as_bytes(),
            &b,
            Default::default(),
        );
        p.parse_imports()?;
        p.parse_declarations()?;
        p.finish_parse(vec![&family_other].as_slice())?;

        let (_rdfont, incomplete) = p.as_ontology_and_incomplete()?;
        assert!(incomplete.is_complete());
        Ok(())
    }

    #[test]
    fn annotation_with_anonymous() {
        let s = slurp_rdfont("ambiguous/annotation-with-anonymous");
        let ont: ComponentMappedOntology<_, _> = read_ok(&mut s.as_bytes()).into();

        // We cannot do the usual "compare" because the anonymous
        // individuals break a direct comparision
        assert_eq!(ont.i().annotation_assertion().count(), 1);

        let _aa = ont.i().annotation_assertion().next();
    }

    // #[test]
    // fn import_property() {
    //     compare("import-property")
    // }

    // #[test]
    // fn family_import() -> Result<(),HornedError>{
    //     let b = Build::new();
    //     let p = parser_with_build(&mut slurp_rdfont("family-other").as_bytes(), &b);
    //     let (family_other, incomplete) = p.parse()?;
    //     assert!(incomplete.is_complete());

    //     let mut p = parser_with_build(&mut slurp_rdfont("family").as_bytes(), &b);
    //     p.parse_imports()?;
    //     p.parse_declarations()?;
    //     p.finish_parse(vec![&family_other])?;

    //     let (_rdfont, incomplete) = p.as_ontology_and_incomplete()?;

    //     assert!(incomplete.is_complete());
    //     Ok(())
    // }

    // #[test]
    // fn family() {
    //     compare("family");
    // }
}
