use Term::*;
use rio_api::{model::{BlankNode, NamedNode, Subject}, parser::TriplesParser};

use crate::{model::Literal, ontology::axiom_mapped::AxiomMappedOntology};
use crate::model::*;

use crate::vocab::WithIRI;
use crate::vocab::OWL as VOWL;
use crate::vocab::RDF as VRDF;
use crate::{ontology::
            {
                set::{SetIndex,SetOntology},
                declaration_mapped::DeclarationMappedIndex,
                indexed::ThreeIndexedOntology,
                logically_equal::
                {
                    LogicallyEqualIndex,
                    update_or_insert_logically_equal_axiom,
                },
            },
            vocab::RDFS as VRDFS, resolve::strict_resolve_iri
};

use enum_meta::Meta;
use failure::Error;


use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::BufRead;
use std::io::Cursor;
use std::rc::Rc;

type RioTerm<'a> = ::rio_api::model::Term<'a>;

macro_rules! some {
    ($body:expr) => {
        (|| Some($body))()
    };
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Ord, PartialOrd)]
pub struct BNode(Rc<str>);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Term {
    Iri(IRI),
    BNode(BNode),
    Literal(Literal),
    OWL(VOWL),
    RDF(VRDF),
    RDFS(VRDFS),
    FacetTerm(Facet),
}

impl Term {
    fn ord(&self) -> isize {
        match self {
            OWL(_) => 1,
            RDF(_) => 2,
            RDFS(_) => 3,
            FacetTerm(_) => 4,
            Iri(_) => 5,
            Term::BNode(_) => 6,
            Literal(_) => 7,
        }
    }
}

// impl PartialEq for Term {
//     fn eq(&self, other: &Self) -> bool {
//         match
//     }
// }

impl Ord for Term {
    fn cmp(&self, other: &Term) -> Ordering {
        match (self, other) {
            (OWL(s), OWL(o)) => s.cmp(o),
            (RDF(s), RDF(o)) => s.cmp(o),
            (RDFS(s), RDFS(o)) => s.cmp(o),
            (FacetTerm(s), FacetTerm(o)) => s.cmp(o),
            (Iri(s), Iri(o)) => s.to_string().cmp(&o.to_string()),
            (Term::BNode(s), Term::BNode(o)) => (*s).cmp(&(*o)),
            (Literal(s), Literal(o)) => (s.literal()).cmp(o.literal()),
            _ => self.ord().cmp(&other.ord()),
        }
   }
}

impl PartialOrd for Term {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

// impl Term {
//     pub fn n3_maybe(&self) -> String {
//         match self {
//             Iri(_) | BNode(_) | Literal(_, _) | Variable(_) => self.n3(),
//             OWL(v) => format!("{:?}", v),
//             RDFS(v) => format!("{:?}", v),
//             RDF(v) => format!("{:?}", v),
//         }
//     }

//     pub fn n3(&self) -> String {
//         match self {
//             Iri(i) => sophia::term::Term::Iri(i.clone()).n3(),
//             BNode(id) => sophia::term::Term::BNode(id.clone()).n3(),
//             Literal(l, k) => sophia::term::Term::Literal(l.clone(), k.clone()).n3(),
//             Variable(v) => sophia::term::Term::Variable(v.clone()).n3(),
//             OWL(v) => vocab_to_term(v).n3(),
//             RDFS(v) => vocab_to_term(v).n3(),
//             RDF(v) => vocab_to_term(v).n3(),
//         }
//     }

//     pub fn value(&self) -> String {
//         match self {
//             Iri(i) => sophia::term::Term::Iri(i.clone()).value(),
//             BNode(id) => sophia::term::Term::BNode(id.clone()).value(),
//             Literal(l, k) => sophia::term::Term::Literal(l.clone(), k.clone()).value(),
//             Variable(v) => sophia::term::Term::Variable(v.clone()).value(),
//             OWL(v) => vocab_to_term(v).value(),
//             RDFS(v) => vocab_to_term(v).value(),
//             RDF(v) => vocab_to_term(v).value(),
//         }
//     }
// }

trait Convert {
    fn to_iri(&self, b: &Build) -> IRI;
}

// TODO
// impl Convert for SpIri {
//     fn to_iri(&self, b: &Build) -> IRI {
//         b.iri(self.chars().collect::<String>())
//     }
// }

impl Convert for rio_api::model::NamedNode<'_> {
    fn to_iri(&self, b: &Build) -> IRI {
        b.iri(&self.to_string())
    }
}

trait TryBuild<N: From<IRI>> {
    fn to_some_iri(&self, b: &Build) -> Option<IRI>;

    fn to_iri_maybe(&self, b: &Build) -> Result<IRI, Error> {
        match self.to_some_iri(b) {
            Some(iri) => Ok(iri),
            None => todo!("Fix this"),
        }
    }

    fn try_build(&self, b: &Build) -> Result<N, Error> {
        Ok(self.to_iri_maybe(b)?.into())
    }
}

// impl<N: From<IRI>> TryBuild<N> for Option<SpIri> {
//     fn to_some_iri(&self, b: &Build) -> Option<IRI> {
//         self.as_ref().map(|i| i.to_iri(b))
//     }
// }

#[derive(Clone, Debug, Eq, PartialEq)]
enum OrTerm {
    Term(Term),
    ClassExpression(ClassExpression),
}

impl From<ClassExpression> for OrTerm {
    fn from(c: ClassExpression) -> OrTerm {
        OrTerm::ClassExpression(c)
    }
}

impl From<Term> for OrTerm {
    fn from(t: Term) -> OrTerm {
        OrTerm::Term(t)
    }
}

fn vocab_lookup() -> HashMap<&'static String, Term> {
    let mut m = HashMap::default();

    for v in VOWL::all() {
        match v {
            // Skip the builtin properties or we have to treat them separately
            VOWL::TopDataProperty => None,
            _ => m.insert(v.iri_s(), Term::OWL(v)),
        };
    }

    for v in VRDFS::all() {
        m.insert(v.iri_s(), Term::RDFS(v));
    }

    for v in VRDF::all() {
        m.insert(v.iri_s(), Term::RDF(v));
    }

    for v in Facet::all() {
        m.insert(v.iri_s(), Term::FacetTerm(v));
    }
    m
}


fn to_term_nn<'a>(nn: &'a NamedNode,
                  m: &HashMap<&String, Term>,
                  b: &Build) -> Term {
    let s = nn.iri.to_string();
    if let Some(term) = m.get(&s) {
        return term.clone();
    }
    Term::Iri(b.iri(s))
}

fn to_term_bn<'a>(nn: &'a BlankNode) -> Term {
    Term::BNode(BNode(nn.id.to_string().into()))
}

fn to_term_lt<'a>(lt: &'a rio_api::model::Literal, b: &Build)-> Term {
    match lt {
        rio_api::model::Literal::Simple{value} =>
            Term::Literal(Literal::Simple{literal:value.to_string()}),
        rio_api::model::Literal::LanguageTaggedString { value, language } =>
            Term::Literal(Literal::Language{literal:value.to_string(), lang: language.to_string()}),
        rio_api::model::Literal::Typed { value, datatype }
        if datatype.iri == "http://www.w3.org/2001/XMLSchema#string" =>
            Term::Literal(Literal::Simple{literal: value.to_string()}),
        rio_api::model::Literal::Typed { value, datatype } => {
            Term::Literal(Literal::Datatype{literal: value.to_string(),
                                            datatype_iri: b.iri(datatype.iri.to_string())})
        }
    }
}

fn to_term_nnb<'a>(nnb: &'a Subject,
                   m: &HashMap<&String, Term>,
                   b: &Build) -> Term {
    match nnb {
        Subject::NamedNode(nn) => {
            to_term_nn(nn, m, b)
        }
        Subject::BlankNode(bn) => {
            to_term_bn(bn)
        }
        Subject::Triple(_) =>
            unimplemented!("Triple subjects are not implemented")
    }
}

fn to_term<'a>(t: &'a RioTerm, m: &HashMap<&String, Term>, b: &Build) -> Term {
    match t {
        rio_api::model::Term::NamedNode(iri) => to_term_nn(iri, m, b),
        rio_api::model::Term::BlankNode(id) => to_term_bn(id),
        rio_api::model::Term::Literal(l) => to_term_lt(l, b),
        rio_api::model::Term::Triple(_) => unimplemented!("Triple subjects are not implemented")
    }
}

macro_rules! d {
    () => {
        Default::default()
    };
}

pub type RDFOntology = ThreeIndexedOntology<SetIndex,DeclarationMappedIndex,LogicallyEqualIndex>;

impl From<RDFOntology> for SetOntology {
    fn from(so: RDFOntology) -> SetOntology {
        let id: OntologyID = so.id().clone();
        let (si, i1, i2) = so.index();
        // Drop the rest of these so that we can consume the Rc
        drop(i1);
        drop(i2);
        (id, si.into_iter()).into()
    }
}

impl From<RDFOntology> for AxiomMappedOntology {
    fn from(rdfo: RDFOntology) -> AxiomMappedOntology {
        let so: SetOntology = rdfo.into();
        so.into()
    }
}

#[derive(Debug)]
enum OntologyParserState {
    New, Imports, Declarations, Parse
}

#[derive(Debug, Default)]
pub struct IncompleteParse {
    pub simple: Vec<[Term; 3]>,
    pub bnode: Vec<Vec<[Term; 3]>>,
    pub bnode_seq: Vec<Vec<Term>>,

    pub class_expression: Vec<ClassExpression>,
    pub object_property_expression: Vec<ObjectPropertyExpression>,
    pub data_range: Vec<DataRange>,
    pub ann_map: HashMap<[Term;3], BTreeSet<Annotation>>,
}

impl IncompleteParse {
    pub fn is_complete(&self) -> bool {
        self.simple.is_empty() &&
            self.bnode.is_empty() &&
            self.bnode_seq.is_empty() &&
            self.class_expression.is_empty() &&
            self.object_property_expression.is_empty() &&
            self.data_range.is_empty() &&
            self.ann_map.is_empty()
    }
}

#[derive(Debug)]
pub struct OntologyParser<'a> {
    o: RDFOntology,
    b: &'a Build,

    triple: Vec<[Term; 3]>,
    simple: Vec<[Term; 3]>,
    bnode: HashMap<BNode, Vec<[Term; 3]>>,
    bnode_seq: HashMap<BNode, Vec<Term>>,

    class_expression: HashMap<BNode, ClassExpression>,
    object_property_expression: HashMap<BNode, ObjectPropertyExpression>,
    data_range: HashMap<BNode, DataRange>,
    ann_map: HashMap<[Term; 3], BTreeSet<Annotation>>,
    state: OntologyParserState,
    error: Result<(),Error>,
}

impl<'a> OntologyParser<'a> {
    pub fn new(b: &'a Build, triple: Vec<[Term; 3]>) -> OntologyParser {
        OntologyParser {
            o: d!(),
            b,

            triple,
            simple: d!(),
            bnode: d!(),
            bnode_seq: d!(),
            class_expression: d!(),
            object_property_expression: d!(),
            data_range: d!(),
            ann_map: d!(),
            state: OntologyParserState::New,
            error: Ok(()),
        }
    }

    pub fn from_bufread<'b, R: BufRead>(b: &'a Build, bufread: &'b mut R)
                                        -> OntologyParser<'a> {
        let m = vocab_lookup();
        let triple_iter = rio_xml::RdfXmlParser::new(bufread, None).into_iter(
            |rio_triple|
            Ok(
                [
                    to_term_nnb(&rio_triple.subject, &m, b),
                    to_term_nn(&rio_triple.predicate, &m, b),
                    to_term(&rio_triple.object, &m, b)
                ]
            )
        );
        let results: Vec<Result<[Term; 3], Error>> = triple_iter.collect();
        let triples: Result<Vec<_>, _> = results.into_iter().collect();
        let triple_v: Vec<[Term; 3]> = triples.unwrap();
        //dbg!(&triple_v);
        OntologyParser::new(b, triple_v)
    }

    pub fn from_doc_iri(b: &'a Build, iri: &IRI) -> OntologyParser<'a> {
        OntologyParser::from_bufread(
            b, &mut Cursor::new(strict_resolve_iri(iri))
        )
    }

    fn group_triples(
        triple: Vec<[Term; 3]>,
        simple: &mut Vec<[Term; 3]>,
        bnode: &mut HashMap<BNode, Vec<[Term; 3]>>,
    ) {
        // Next group together triples on a BNode, so we have
        // HashMap<BNodeID, Vec<[SpTerm; 3]> All of which should be
        // triples should begin with the BNodeId. We should be able to
        // gather these in a single pass.
        for t in &triple {
            match t {
                [Term::BNode(id), _, _] => {
                    let v = bnode.entry(id.clone()).or_insert_with(Vec::new);
                    v.push(t.clone())
                }
                _ => {
                    simple.push(t.clone());
                }
            }
        }
    }

    fn stitch_seqs_1(&mut self) {
        let mut extended = false;

        for (k, v) in std::mem::take(&mut self.bnode) {
            let _ = match v.as_slice() {
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

        if extended && self.bnode.len() > 0 {
            self.stitch_seqs_1()
        }
    }

    fn stitch_seqs(&mut self) {
        for (k, v) in std::mem::take(&mut self.bnode) {
            let _ = match v.as_slice() {
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

    fn resolve_imports(&mut self) {
        for t in std::mem::take(&mut self.simple) {
            match t {
                [Term::Iri(_), Term::OWL(VOWL::Imports), Term::Iri(imp)] => {
                    self.merge(AnnotatedAxiom {
                        axiom: Import(imp).into(),
                        ann: BTreeSet::new(),
                    });
                }
                _ => self.simple.push(t.clone()),
            }
        }

        // Section 3.1.2/table 4 of RDF Graphs
    }

    fn headers(&mut self) {
        //Section 3.1.2/table 4
        //   *:x rdf:type owl:Ontology .
        //[ *:x owl:versionIRI *:y .]
        let mut iri: Option<IRI> = None;
        let mut viri: Option<IRI> = None;

        for t in std::mem::take(&mut self.simple) {
            match t {
                [Term::Iri(s), Term::RDF(VRDF::Type), Term::OWL(VOWL::Ontology)] => {
                    iri = Some(s.clone());
                }
                [Term::Iri(s), Term::OWL(VOWL::VersionIRI), Term::Iri(ob)]
                    if iri.as_ref() == Some(&s) =>
                {
                    viri = Some(ob.clone());
                }
                _ => self.simple.push(t.clone()),
            }
        }

        self.o.mut_id().iri = iri;
        self.o.mut_id().viri = viri;
    }

    fn backward_compat(&mut self) {
        // Table 5, Table 6
    }

    fn parse_annotations(&self, triples: &[[Term; 3]]) -> BTreeSet<Annotation> {
        let mut ann = BTreeSet::default();
        for a in triples {
            ann.insert(self.annotation(a));
        }
        ann
    }

    fn annotation(&self, t: &[Term; 3]) -> Annotation {
        match t {
            // We assume that anything passed to here is an
            // annotation built in type
            [s, RDFS(rdfs), b] => {
                let iri = self.b.iri(rdfs.iri_s());
                self.annotation(&[s.clone(), Term::Iri(iri), b.clone()])
            }
            [s, OWL(owl), b] => {
                let iri = self.b.iri(owl.iri_s());
                self.annotation(&[s.clone(), Term::Iri(iri), b.clone()])
            }
            [_, Iri(p), ob @ Term::Literal(_)] => Annotation {
                ap: AnnotationProperty(p.clone()),
                av: self.to_literal(ob).unwrap().into(),
            },
            [_, Iri(p), Iri(ob)] => {
                // IRI annotation value
                Annotation {
                    ap: AnnotationProperty(p.clone()),
                    av: ob.clone().into(),
                }
            }
            _ => {
                dbg!(t);
                todo!()
            }
        }
    }

    fn merge<A: Into<AnnotatedAxiom>>(&mut self, ax: A) {
        let ax = ax.into();
        update_or_insert_logically_equal_axiom(&mut self.o, ax);
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
                    self.simple.push([sb.clone(), p.clone(), ob.clone()])
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
            let entity = match &triple {
                // TODO Change this into a single outer match
                [Term::Iri(s), Term::RDF(VRDF::Type), entity] => {
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
                        _ => {
                            None
                        },
                    }
                }
                _ => None,
            };

            if let Some(entity) = entity {
                let ann = self
                    .ann_map
                    .remove(&triple)
                    .unwrap_or_else(|| BTreeSet::new());
                let ne: NamedEntity = entity;
                self.merge(AnnotatedAxiom {
                    axiom: ne.into(),
                    ann,
                });
            } else {
                self.simple.push(triple);
            }
        }
    }

    fn data_ranges(&mut self) {
        let data_range_len = self.data_range.len();
        let mut facet_map: HashMap<Term, [Term; 3]> = HashMap::new();

        for (k, v) in std::mem::take(&mut self.bnode) {
            match v.as_slice() {
                [triple @ [_, Term::FacetTerm(_), _]] => {
                    facet_map.insert(Term::BNode(k), triple.clone());
                }
                _ => {
                    self.bnode.insert(k, v);
                }
            }
        }

        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            let dr = match v.as_slice() {
                [[_, Term::OWL(VOWL::IntersectionOf), Term::BNode(bnodeid)],//: rustfmt hard line!
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    some! {
                        DataRange::DataIntersectionOf(
                            self.to_dr_seq(bnodeid)?
                        )
                    }
                }
                [[_, Term::OWL(VOWL::DatatypeComplementOf), term],//:
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    some! {
                      DataRange::DataComplementOf(
                            Box::new(self.to_dr(term)?)
                        )
                    }
                }
                [[_, Term::OWL(VOWL::OneOf), Term::BNode(bnode)],//:
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    some! {
                        DataRange::DataOneOf(
                            self.to_literal_seq(bnode)?
                        )
                    }
                }
                [[_, Term::OWL(VOWL::OnDatatype), Term::Iri(iri)],//:
                 [_, Term::OWL(VOWL::WithRestrictions), Term::BNode(id)],//:
                 [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)]] =>
                {
                    some! {
                        {
                            let facet_seq = self.bnode_seq
                                .remove(id)?;
                            let some_facets:Vec<Option<FacetRestriction>> =
                                facet_seq.into_iter().map(|id|
                                                          match facet_map.remove(&id)? {
                                                              [_, Term::FacetTerm(facet), literal] => Some(
                                                                  FacetRestriction {
                                                                      f: facet,
                                                                      l: self.to_literal(&literal)?,
                                                                  }
                                                              ),
                                                              _ => None
                                                          }
                                )
                                .collect();

                            let facets:Option<Vec<FacetRestriction>> = some_facets.into_iter().collect();
                            DataRange::DatatypeRestriction(
                                iri.into(),
                                facets?
                            )
                        }
                    }
                }
                _ => None,
            };

            if let Some(dr) = dr {
                self.data_range.insert(this_bnode, dr);
            } else {
                self.bnode.insert(this_bnode, v);
            }
        }

        if self.data_range.len() > data_range_len {
            self.data_ranges();
        }

        // Shove any remaining facets back onto bnode so that they get
        // reported at the end
        self.bnode
            .extend(facet_map.into_iter().filter_map(|(k, v)| match k {
                Term::BNode(id) => Some((id, vec![v])),
                _ => None,
            }));
    }

    fn object_property_expressions(&mut self) {
        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            let mut ope = None;
            let mut new_triple = vec![];
            for t in v {
                match t {
                    [Term::BNode(_), Term::OWL(VOWL::InverseOf), Term::Iri(iri)] => {
                        ope = Some(ObjectPropertyExpression::InverseObjectProperty(iri.into()))
                    }
                    _ => {
                        new_triple.push(t);
                    }
                };
            }

            if let Some(ope) = ope {
                self.object_property_expression
                    .insert(this_bnode.clone(), ope);
            }

            if new_triple.len() > 0 {
                self.bnode.insert(this_bnode, new_triple);
            }
        }
    }

    fn to_iri(&self, t: &Term) -> Option<IRI> {
        match t {
            Term::Iri(iri) => Some(iri.clone()),
            _ => None,
        }
    }

    fn to_sope(&mut self, t: &Term, ic:&[&RDFOntology]) -> Option<SubObjectPropertyExpression> {
        Some(self.to_ope(t, ic)?.into())
    }

    fn to_ope(&mut self, t: &Term, ic:&[&RDFOntology]) -> Option<ObjectPropertyExpression> {
        match self.find_property_kind(t, ic)? {
            PropertyExpression::ObjectPropertyExpression(ope) => Some(ope),
            _ => None,
        }
    }

    fn to_ap(&mut self, t: &Term, ic:&[&RDFOntology]) -> Option<AnnotationProperty> {
        match self.find_property_kind(t, ic)? {
            PropertyExpression::AnnotationProperty(ap) => Some(ap),
            _ => None,
        }
    }

    fn to_dp(&mut self, t: &Term, ic:&[&RDFOntology]) -> Option<DataProperty> {
        match self.find_property_kind(t, ic)? {
            PropertyExpression::DataProperty(dp) => Some(dp),
            _ => None,
        }
    }

    fn to_ce(&mut self, tce: &Term) -> Option<ClassExpression> {
        match tce {
            Term::Iri(cl) => Some(Class(cl.clone()).into()),
            Term::BNode(id) => self.class_expression.remove(id),
            _ => None,
        }
    }

    fn to_ce_seq(&mut self, bnodeid: &BNode) -> Option<Vec<ClassExpression>> {
        if !self.bnode_seq
            .get(bnodeid)?
            .iter()
            .all(|tce|
                 match tce {
                     Term::BNode(id) => self.class_expression.contains_key(id),
                     _=> true,
                 }
            ) {
                return None;
        }


        let v: Vec<Option<ClassExpression>> = self
            .bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .into_iter()
            .map(|tce| {
                self.to_ce(tce)
            })
            .collect();

        // Check all not None. If any are, return all the
        // class_expressions are are Some!
        // All or nothing
        v.into_iter().collect()
    }

    fn to_ni_seq(&mut self, bnodeid: &BNode) -> Option<Vec<Individual>> {
        let v: Vec<Option<Individual>> = self
            .bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .into_iter()
            .map(|t| self.to_iri(t)
                 .map(|iri|
                      NamedIndividual(iri.clone())
                      .into()))
            .collect();

        v.into_iter().collect()
    }

    fn to_dr_seq(&mut self, bnodeid: &BNode) -> Option<Vec<DataRange>> {
        let v: Vec<Option<DataRange>> = self
            .bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .into_iter()
            .map(|t| self.to_dr(t))
            .collect();

        v.into_iter().collect()
    }

    // TODO Fix code duplication
    fn to_literal_seq(&mut self, bnodeid: &BNode) -> Option<Vec<Literal>> {
        let v: Vec<Option<Literal>> = self
            .bnode_seq
            .remove(bnodeid)
            .as_ref()?
            .into_iter()
            .map(|t| self.to_literal(t))
            .collect();

        v.into_iter().collect()
    }

    fn to_dr(&mut self, t: &Term) -> Option<DataRange> {
        match t {
            Term::Iri(iri) => {
                let dt: Datatype = iri.into();
                Some(dt.into())
            }
            Term::BNode(id) => self.data_range.remove(id),
            _ => todo!(),
        }
    }

    fn to_u32(&self, t: &Term) -> Option<u32> {
        match t {
            Term::Literal(val) => val.literal().parse::<u32>().ok(),
            _ => None,
        }
    }

    fn to_literal(&self, t: &Term) -> Option<Literal> {
        match t {
            Term::Literal(ob) => Some(ob.clone()),
            _ => return None,
        }
    }

    fn find_declaration_kind(&mut self, iri: &IRI, ic:&[&RDFOntology]) -> Option<NamedEntityKind> {
        [&self.o].iter().chain(
            ic.iter()
        ).map(
            |o| o.j().declaration_kind(iri)
        ).find(
            |d| d.is_some()
        ).flatten()
    }

    fn find_property_kind(&mut self, term: &Term, ic:&[&RDFOntology]) -> Option<PropertyExpression> {
        match term {
            Term::Iri(iri) => match self.find_declaration_kind(iri, ic) {
                Some(NamedEntityKind::AnnotationProperty) => {
                    Some(PropertyExpression::AnnotationProperty(iri.into()))
                }
                Some(NamedEntityKind::DataProperty) => {
                    Some(PropertyExpression::DataProperty(iri.into()))
                }
                Some(NamedEntityKind::ObjectProperty) => {
                    Some(PropertyExpression::ObjectPropertyExpression(iri.into()))
                }
                _ => None,
            },
            Term::BNode(id) => Some(self.object_property_expression.remove(id)?.into()),
            _ => None,
        }
    }

    fn class_expressions(&mut self, ic: &[&RDFOntology]) {
        let class_expression_len = self.class_expression.len();
        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            // rustfmt breaks this (putting the triples all on one
            // line) so skip
            let ce = match v.as_slice() {
                [[_, Term::OWL(VOWL::OnProperty), pr],//:
                 [_, Term::OWL(VOWL::SomeValuesFrom), ce_or_dr],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ClassExpression::ObjectSomeValuesFrom {
                                    ope,
                                    bce: self.to_ce(ce_or_dr)?.into()
                                }
                            },
                            PropertyExpression::DataProperty(dp) => {
                                ClassExpression::DataSomeValuesFrom {
                                    dp,
                                    dr: self.to_dr(ce_or_dr)?
                                }
                            },
                            _ => panic!("Unexpected Property Kind")
                        }
                    }
                },
                [[_, Term::OWL(VOWL::HasValue), val],//:
                 [_, Term::OWL(VOWL::OnProperty), pr],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ClassExpression::ObjectHasValue {
                                    ope,
                                    i: NamedIndividual(self.to_iri(val)?).into()
                                }
                            },
                            PropertyExpression::DataProperty(dp) => {
                                ClassExpression::DataHasValue {
                                    dp,
                                    l: self.to_literal(val)?
                                }
                            }
                            _ => panic!("Unexpected Property kind"),
                        }
                    }
                },
                [[_, Term::OWL(VOWL::AllValuesFrom), ce_or_dr],//:
                 [_, Term::OWL(VOWL::OnProperty), pr],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ClassExpression::ObjectAllValuesFrom {
                                    ope,
                                    bce: self.to_ce(ce_or_dr)?.into()
                                }
                            },
                            PropertyExpression::DataProperty(dp) => {
                                ClassExpression::DataAllValuesFrom {
                                    dp,
                                    dr: self.to_dr(ce_or_dr)?
                                }
                            },
                            _ => panic!("Unexpected Property Kind")
                        }
                    }
                },
                [[_, Term::OWL(VOWL::OneOf), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    some!{
                        ClassExpression::ObjectOneOf(
                            self.to_ni_seq(bnodeid)?
                        )
                    }
                },
                [[_, Term::OWL(VOWL::IntersectionOf), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    some!{
                        ClassExpression::ObjectIntersectionOf(
                            self.to_ce_seq(bnodeid)?
                        )
                    }
                },
                [[_, Term::OWL(VOWL::UnionOf), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    some!{
                        ClassExpression::ObjectUnionOf(
                            self.to_ce_seq(
                                bnodeid,
                            )?
                        )
                    }
                },
                [[_, Term::OWL(VOWL::ComplementOf), tce],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    some!{
                        ClassExpression::ObjectComplementOf(
                            self.to_ce(&tce)?.into()
                        )
                    }
                },
                [[_, Term::OWL(VOWL::OnDataRange), dr],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::OWL(VOWL::QualifiedCardinality), literal],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::DataExactCardinality
                        {
                            n:self.to_u32(literal)?,
                            dp: pr.into(),
                            dr: self.to_dr(dr)?
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MaxQualifiedCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnDataRange), dr],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::DataMaxCardinality
                        {
                            n:self.to_u32(literal)?,
                            dp: pr.into(),
                            dr: self.to_dr(dr)?
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MinQualifiedCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnDataRange), dr],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::DataMinCardinality
                        {
                            n:self.to_u32(literal)?,
                            dp: pr.into(),
                            dr: self.to_dr(dr)?
                        }
                    }
                }
                //_:x rdf:type owl:Restriction .
                //_:x owl:cardinality NN_INT(n) .
                //_:x owl:onProperty y .
                //{ OPE(y) ≠ ε }
                [[_, Term::OWL(VOWL::Cardinality), literal],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectExactCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.b.class(VOWL::Thing.iri_s()).into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::OnClass), tce],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::OWL(VOWL::QualifiedCardinality), literal],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectExactCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.to_ce(tce)?.into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MinCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectMinCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.b.class(VOWL::Thing.iri_s()).into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MinQualifiedCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnClass), tce],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectMinCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.to_ce(tce)?.into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MaxCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectMaxCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.b.class(VOWL::Thing.iri_s()).into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MaxQualifiedCardinality), literal],//:
                 [_, Term::OWL(VOWL::OnClass), tce],//:
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectMaxCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.to_ce(tce)?.into()
                        }
                    }
                }
                _a => None,
            };

            if let Some(ce) = ce {
                self.class_expression.insert(this_bnode, ce);
            } else {
                self.bnode.insert(this_bnode, v);
            }
        }

        if self.class_expression.len() > class_expression_len {
            self.class_expressions(ic)
        }
    }

    fn axioms(&mut self, ic:&[&RDFOntology]) {
        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            let axiom: Option<Axiom> = match v.as_slice() {
                [[_, Term::OWL(VOWL::AssertionProperty), pr],//:
                 [_, Term::OWL(VOWL::SourceIndividual), Term::Iri(i)],//:
                 [_, target_type, target],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::NegativePropertyAssertion)]] =>
                {
                    some! {
                        match target_type {
                            Term::OWL(VOWL::TargetIndividual) =>
                                NegativeObjectPropertyAssertion {
                                    ope: self.to_ope(pr, ic)?,
                                    from: i.into(),
                                    to: self.to_iri(target)?.into(),
                                }.into(),
                            Term::OWL(VOWL::TargetValue) =>
                                NegativeDataPropertyAssertion {
                                    dp: self.to_dp(pr, ic)?,
                                    from: i.into(),
                                    to: self.to_literal(target)?,
                                }.into(),
                            _ => todo!()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::Members), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDifferent)]] =>
                {
                    some! {
                        DifferentIndividuals (
                            self.to_ni_seq(bnodeid)?
                        ).into()
                    }
                }
                [[_, Term::OWL(VOWL::DistinctMembers), Term::BNode(bnodeid)],//:
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDifferent)]] =>
                {
                    some! {
                        DifferentIndividuals (
                            self.to_ni_seq(bnodeid)?
                        ).into()
                    }
                }
                _ => None,
            };

            if let Some(axiom) = axiom {
                self.merge(AnnotatedAxiom {
                    axiom,
                    ann: BTreeSet::new(),
                })
            } else {
                self.bnode.insert(this_bnode, v);
            }
        }

        for triple in std::mem::take(&mut self.simple).into_iter().chain(
            std::mem::take(&mut self.bnode)
                .into_iter()
                .map(|(_k, v)| v)
                .flatten(),
        ) {
            let axiom: Option<Axiom> = match &triple {
                [Term::Iri(sub), Term::RDFS(VRDFS::SubClassOf), tce] => some! {
                    SubClassOf {
                        sub: Class(sub.clone()).into(),
                        sup: self.to_ce(tce)?,
                    }
                    .into()
                },
                // TODO: We need to check whether these
                // EquivalentClasses have any other EquivalentClasses
                // and add to that axiom
                [Term::Iri(a), Term::OWL(VOWL::EquivalentClass), b] => {
                    some! {
                        {
                            match self.find_declaration_kind(a, ic)? {
                                NamedEntityKind::Class => {
                                    EquivalentClasses(
                                        vec![
                                            Class(a.clone()).into(),
                                            self.to_ce(b)?,
                                        ]).into()
                                }
                                NamedEntityKind::Datatype => {
                                    DatatypeDefinition{
                                        kind: Datatype(a.clone()).into(),
                                        range: self.to_dr(b)?,
                                    }.into()
                                }
                                _=> todo!()
                            }
                        }
                    }
                }
                [class, Term::OWL(VOWL::HasKey), Term::BNode(bnodeid)] => {
                    some! {
                        {
                            let v:Vec<Option<PropertyExpression>> = self.bnode_seq
                                .remove(&bnodeid)?
                                .into_iter()
                                .map(|pr| self.find_property_kind(&pr, ic))
                                .collect();
                            let vpe: Option<Vec<PropertyExpression>> = v.into_iter().collect();

                            HasKey{
                                ce:self.to_ce(class)?,
                                vpe: vpe?
                            }.into()
                        }
                    }
                }
                [Term::Iri(iri), Term::OWL(VOWL::DisjointUnionOf), Term::BNode(bnodeid)] => {
                    some! {
                        DisjointUnion(
                            Class(iri.clone()),
                            self.to_ce_seq(bnodeid)?
                        ).into()
                    }
                }
                [Term::Iri(p), Term::OWL(VOWL::InverseOf), Term::Iri(r)] => {
                    some! {
                        InverseObjectProperties (ObjectProperty(p.clone()),
                                                 ObjectProperty(r.clone())).into()
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::TransitiveProperty)] => {
                    some! {
                        TransitiveObjectProperty(self.to_ope(pr, ic)?).into()
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::FunctionalProperty)] => {
                    some! {
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
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                AsymmetricObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::SymmetricProperty)] => {
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                SymmetricObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::ReflexiveProperty)] => {
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                ReflexiveObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::IrreflexiveProperty)] => {
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                IrreflexiveObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::InverseFunctionalProperty)] => {
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => {
                                InverseFunctionalObjectProperty(ope).into()
                            },

                            _ => todo!()
                        }
                    }
                }
                [Term::Iri(sub), Term::RDF(VRDF::Type), cls] => some! {
                    {
                        ClassAssertion {
                            ce: self.to_ce(cls)?,
                            i: NamedIndividual(sub.clone()).into()
                        }.into()
                    }
                },

                [Term::Iri(a), Term::OWL(VOWL::DisjointWith), Term::Iri(b)] => {
                    Some(
                        DisjointClasses(vec![
                            Class(a.clone()).into(),
                            Class(b.clone()).into(),
                        ])
                        .into(),
                    )
                }
                [pr, Term::RDFS(VRDFS::SubPropertyOf), t] => {
                    some! {
                        match self.find_property_kind(t, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) =>
                                SubObjectPropertyOf {
                                    sup: ope,
                                    sub: self.to_sope(pr, ic)?,
                                }.into(),
                            PropertyExpression::DataProperty(dp) =>
                                SubDataPropertyOf {
                                    sup: dp,
                                    sub: self.to_dp(pr, ic)?
                                }.into(),
                            PropertyExpression::AnnotationProperty(ap) =>
                                SubAnnotationPropertyOf {
                                    sup: ap,
                                    sub: self.to_ap(pr, ic)?
                                }.into(),
                        }
                    }
                }
                [Term::Iri(pr), Term::OWL(VOWL::PropertyChainAxiom), Term::BNode(id)] => {
                    some! {
                        SubObjectPropertyOf {
                            sub: SubObjectPropertyExpression::ObjectPropertyChain(
                                self.bnode_seq
                                    .remove(id)?
                                    .iter()
                                    .map(|t| self.to_ope(t, ic).unwrap())
                                    .collect()
                            ),
                            sup: ObjectProperty(pr.clone()).into(),
                        }.into()
                    }
                }
                [pr, Term::RDFS(VRDFS::Domain), t] => {
                    some! {
                        match self.find_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => ObjectPropertyDomain {
                                ope,
                                ce: self.to_ce(t)?,
                            }
                            .into(),
                            PropertyExpression::DataProperty(dp) => DataPropertyDomain {
                                dp,
                                ce: self.to_ce(t)?,
                            }
                            .into(),
                            PropertyExpression::AnnotationProperty(ap) => AnnotationPropertyDomain {
                                ap: ap,
                                iri: self.to_iri(t)?,
                            }
                            .into(),
                        }
                    }
                }
                [pr, Term::RDFS(VRDFS::Range), t] => some! {
                    match self.find_property_kind(pr, ic)? {
                        PropertyExpression::ObjectPropertyExpression(ope) => ObjectPropertyRange {
                            ope,
                            ce: self.to_ce(t)?,
                        }
                        .into(),
                        PropertyExpression::DataProperty(dp) => DataPropertyRange {
                            dp,
                            dr: self.to_dr(t)?,
                        }
                        .into(),
                        PropertyExpression::AnnotationProperty(ap) => AnnotationPropertyRange {
                            ap: ap,
                            iri: self.to_iri(t)?,
                        }
                        .into(),
                    }
                },
                [r, Term::OWL(VOWL::PropertyDisjointWith), s] => some! {
                    match self.find_property_kind(r, ic)? {
                        PropertyExpression::ObjectPropertyExpression(ope) => DisjointObjectProperties (
                            vec![ope, self.to_ope(s, ic)?]
                        )
                        .into(),
                        PropertyExpression::DataProperty(dp) => DisjointDataProperties (
                            vec![dp, self.to_dp(s, ic)?]
                        )
                            .into(),
                        _ => todo!()
                    }
                },
                [r, Term::OWL(VOWL::EquivalentProperty), s] => some! {
                    match self.find_property_kind(r, ic)? {
                        PropertyExpression::ObjectPropertyExpression(ope) => EquivalentObjectProperties (
                            vec![ope, self.to_ope(s, ic)?]
                        )
                        .into(),
                        PropertyExpression::DataProperty(dp) => EquivalentDataProperties (
                            vec![dp, self.to_dp(s, ic)?]
                        )
                        .into(),
                        _ => todo!()
                    }
                },
                [Term::Iri(sub), Term::OWL(VOWL::SameAs), Term::Iri(obj)] => some! {
                    SameIndividual(vec![sub.into(),
                                        obj.into()]).into()
                },
                [Term::Iri(i), Term::OWL(VOWL::DifferentFrom), Term::Iri(j)] => {
                    some! {
                        DifferentIndividuals (
                            vec![i.into(), j.into()]
                        ).into()
                    }
                }
                [Term::Iri(s), Term::Iri(_), _] if self.o.id().iri.as_ref() == Some(&s) => some! {
                    OntologyAnnotation(
                        self.annotation(&triple)
                    ).into()
                },

                [Term::Iri(sub), Term::Iri(pred), t @ Term::Literal(_)] => some! {
                    match (self.find_declaration_kind(sub, ic)?,
                           self.find_declaration_kind(pred, ic)?) {
                        (NamedEntityKind::NamedIndividual,
                         NamedEntityKind::DataProperty) => {
                            DataPropertyAssertion {
                                dp: DataProperty(pred.clone()).into(),
                                from: sub.into(),
                                to: self.to_literal(t)?
                            }.into()
                        }
                        _ => todo!()
                    }
                },
                [Term::Iri(sub), Term::Iri(pred), Term::Iri(obj)] => some! {
                    match (self.find_declaration_kind(sub, ic)?,
                           self.find_declaration_kind(pred, ic)?,
                           self.find_declaration_kind(obj, ic)?) {
                        (NamedEntityKind::NamedIndividual,
                         NamedEntityKind::ObjectProperty,
                         NamedEntityKind::NamedIndividual) => {
                            ObjectPropertyAssertion {
                                ope: ObjectProperty(pred.clone()).into(),
                                from: sub.into(),
                                to: obj.into()
                            }.into()
                        }
                        _ => todo!()
                    }
                },
                _ => None,
            };

            if let Some(axiom) = axiom {
                let ann = self
                    .ann_map
                    .remove(&triple)
                    .unwrap_or_else(|| BTreeSet::new());
                self.merge(AnnotatedAxiom { axiom, ann })
            } else {
                self.simple.push(triple)
            }
        }
    }

    fn simple_annotations(&mut self) {
        for triple in std::mem::take(&mut self.simple) {
            let firi = |s:&mut OntologyParser, t, iri:&IRI| {
                let ann = s
                    .ann_map
                    .remove(t)
                    .unwrap_or_else(|| BTreeSet::new());
                s.merge(AnnotatedAxiom {
                    axiom: AnnotationAssertion {
                        subject: iri.into(),
                        ann: s.annotation(t),
                    }
                    .into(),
                ann,
                })
            };

            match &triple {
                [Term::Iri(iri), Term::RDFS(rdfs), _] if rdfs.is_builtin() => {
                    firi(self, &triple, iri)
                }
                [Term::Iri(iri), Term::OWL(VOWL::VersionInfo), _] => {
                    firi(self, &triple, iri)
                }
                [Term::Iri(iri), Term::Iri(ap), _] if (&self.o).j().is_annotation_property(&ap) => {
                    firi(self, &triple, iri)
                }
                _ => {
                    self.simple.push(triple);
                }
            }
        }
        for (k, v) in std::mem::take(&mut self.bnode){
            let fbnode = |s:&mut OntologyParser, t, ind:&BNode| {
                let ann = s
                    .ann_map
                    .remove(t)
                    .unwrap_or_else(|| BTreeSet::new());
                let ind:AnonymousIndividual = ind.0.clone().into();
                s.merge(AnnotatedAxiom {
                    axiom: AnnotationAssertion {
                        subject: ind.into(),
                        ann: s.annotation(t),
                    }
                    .into(),
                ann,
                })
            };

            match v.as_slice() {
                [triple@[Term::BNode(ind), Term::RDFS(rdfs), _]] if rdfs.is_builtin() => {
                    fbnode(self, &triple, ind)
                }
                [triple@[Term::BNode(ind), Term::OWL(VOWL::VersionInfo), _]] => {
                    fbnode(self, &triple, ind)
                }
                [triple@[Term::BNode(ind), Term::Iri(ap), _]] if (&self.o).j().is_annotation_property(&ap) => {
                    fbnode(self, &triple, ind)
                }
                _=>{
                    self.bnode.insert(k, v);
                }

            }
        }


/*


*/

    }

    /// Parse all imports and add to the Ontology.
    /// Return an error is we are in the wrong state
    pub fn parse_imports(&mut self) -> Result<(), Error> {
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
                self.resolve_imports();
                self.state = OntologyParserState::Imports;
                Ok(())
            }
            _ => todo!()
        }
    }

    /// Parse all declarations and add to the ontology.
    /// Error if we are not in the right state
    pub fn parse_declarations(&mut self) -> Result<(), Error> {
        match self.state {
            OntologyParserState::New => {
                self.parse_imports().and_then(
                    |_| self.parse_declarations()
                )
            }
            OntologyParserState::Imports => {
                dbg!(self.backward_compat());

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
                // as we do in reader2? Tranform them into a triple which we
                // handle normally, then bung the annotation on later?

                // Table 5: Backward compability -- skip this for now (maybe
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
    pub fn finish_parse(&mut self, ic: &[&RDFOntology]) -> Result<(), Error> {
        // Table 10
        self.simple_annotations();

        self.data_ranges();

        // Table 8:
        self.object_property_expressions();

        // Table 13: Parsing of Class Expressions
        self.class_expressions(ic);

        // Table 16: Axioms without annotations
        self.axioms(ic);

        self.state = OntologyParserState::Parse;
        Ok(())
    }

    pub fn parse(mut self) -> Result<(RDFOntology, IncompleteParse),Error> {
        if self.error.is_err() {
            return Err(self.error.unwrap_err());
        }

        match self.state {
            OntologyParserState::New => {
                self.error = self.parse_imports();
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

    pub fn ontology_ref(&self) -> &RDFOntology {
        &self.o
    }

    /// Consume the parser and return an Ontology.
    pub fn as_ontology(self) -> Result<RDFOntology, Error> {
        self.error.and(Ok(self.o))
    }

    /// Consume the parser and return an Ontology and any data
    /// structures that have not been fully parsed
    pub fn as_ontology_and_incomplete(mut self) -> Result<(RDFOntology, IncompleteParse), Error> {
        if self.error.is_err() {
            return Err(self.error.unwrap_err());
        }

        // Regroup so that they print out nicer
        let mut simple = vec![];
        let mut bnode = HashMap::default();

        Self::group_triples(std::mem::take(&mut self.simple),
                            &mut simple, &mut bnode);

        let bnode:Vec<_> = bnode.into_iter().map(|kv| kv.1).collect();
        let bnode_seq:Vec<_> = self.bnode_seq.into_iter().map(|kv| kv.1).collect();
        let class_expression:Vec<_> = self.class_expression.into_iter().map(|kv| kv.1).collect();
        let object_property_expression:Vec<_> = self.object_property_expression
            .into_iter().map(|kv| kv.1).collect();
        let data_range = self.data_range.into_iter().map(|kv| kv.1).collect();

        Ok((self.o, IncompleteParse {
            simple, bnode, bnode_seq, class_expression,
            object_property_expression, data_range,
            ann_map: self.ann_map
        }
        ))
    }
}

pub fn parser_with_build<'a, 'b, R: BufRead>(
    bufread: &'a mut R,
    build: &'b Build,
) -> OntologyParser<'b> {
    OntologyParser::from_bufread(build, bufread)
}

pub fn read_with_build<R: BufRead>(
    bufread: &mut R,
    build: &Build,
) -> Result<(RDFOntology, IncompleteParse), Error> {
    parser_with_build(bufread, build).parse()
}

pub fn read<R: BufRead>(bufread: &mut R) -> Result<(RDFOntology, IncompleteParse), Error> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

#[cfg(test)]
mod test {
    use super::*;

    use std::io::Write;
    use std::path::PathBuf;

    use pretty_assertions::assert_eq;
    use crate::ontology::axiom_mapped::AxiomMappedOntology;

    fn init_log() {
        let _ = env_logger::builder()
            .format(|buf, record| writeln!(buf, "{}", record.args()))
            .is_test(true)
            .try_init();
    }

    fn read_ok<R: BufRead>(bufread: &mut R) -> RDFOntology {
        init_log();

        let r = read(bufread);

        if let Err(e) = r {
            panic!(
                "Expected ontology, get failure: {:?} {:?}",
                e,
                e.backtrace()
            );
        }

        let (ont, incomp) = r.unwrap();
        assert!(incomp.is_complete());
        ont
    }

    fn compare(test: &str) {
        compare_two(test, test);
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

    fn slurp_rdfont(testrdf:&str) -> std::string::String {
        let dir_path_buf = PathBuf::from(file!());
        let dir = dir_path_buf.parent().unwrap().to_string_lossy();

        slurp::read_all_to_string(dbg!(format!("{}/../../ont/owl-rdf/{}.owl",
                                               dir, testrdf)))
            .unwrap()
    }

    fn compare_str(rdfread: &str, xmlread: &str) {
        let rdfont: SetOntology = read_ok(&mut rdfread.as_bytes()).into();
        let xmlont: SetOntology = crate::io::owx::reader::test::read_ok(&mut xmlread.as_bytes())
            .0.into();

        //dbg!(&rdfont); if true {panic!()};

        assert_eq!(rdfont, xmlont);

        //let rdfmapping: &HashMap<&String, &String> = &rdfmapping.mappings().collect();
        //let xmlmapping: &HashMap<&String, &String> = &xmlmapping.mappings().collect();

        //assert_eq!(rdfmapping, xmlmapping);
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

    #[test]
    fn class() {
        compare("class");
    }

    #[test]
    fn declaration_with_annotation() {
        compare("declaration-with-annotation");
    }

    #[test]
    fn declaration_with_two_annotation() {
        compare("declaration-with-two-annotation");
    }

    #[test]
    fn class_with_two_annotations() {
        compare("class_with_two_annotations");
    }

    #[test]
    fn ont() {
        compare("ont");
    }

    #[test]
    fn one_subclass() {
        compare("one-subclass");
    }

    #[test]
    fn subclass_with_annotation() {
        compare("annotation-on-subclass");
    }

    #[test]
    fn oproperty() {
        compare("oproperty");
    }

    #[test]
    fn some() {
        compare("some");
    }

    #[test]
    fn some_not() {
        compare("some-not");
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
    fn only() {
        compare("only");
    }

    #[test]
    fn and() {
        compare("and");
    }

    #[test]
    fn or() {
        compare("or");
    }

    #[test]
    fn not() {
        compare("not");
    }

    #[test]
    fn annotation_property() {
        compare("annotation-property");
    }

    #[test]
    fn annotation() {
        compare("annotation");
    }

    #[test]
    fn annotation_domain() {
        compare("annotation-domain");
    }

    #[test]
    fn annotation_range() {
        compare("annotation-range");
    }

    #[test]
    fn label() {
        compare("label");
    }

    #[test]
    fn one_comment() {
        // This is currently failing because the XML parser gives the
        // comment a language and a datatype ("PlainLiteral") while
        // the RDF one gives it just the language, as literals can't
        // be both. Which is correct?
        compare("one-comment");
    }

    #[test]
    fn one_ontology_annotation() {
        compare("one-ontology-annotation");
    }

    #[test]
    fn equivalent_class() {
        compare("equivalent-class");
    }

    #[test]
    fn equivalent_classes() {
        compare("equivalent_classes");
    }

    #[test]
    fn disjoint_class() {
        compare("disjoint-class");
    }

    #[test]
    fn disjoint_union() {
        compare("disjoint-union");
    }

    #[test]
    fn sub_oproperty() {
        compare("suboproperty");
    }

    #[test]
    fn sub_oproperty_inverse() {
        compare("suboproperty-inverse");
    }

    #[test]
    fn one_inverse() {
        compare("inverse-properties");
    }

    #[test]
    fn one_transitive() {
        compare("transitive-properties");
    }

    #[test]
    fn inverse_transitive() {
        compare("inverse-transitive")
    }

    #[test]
    fn one_annotated_transitive() {
        compare("annotation-on-transitive");
    }

    #[test]
    fn subproperty_chain() {
        compare("subproperty-chain");
    }

    #[test]
    fn one_subproperty_chain_with_inverse() {
        compare("subproperty-chain-with-inverse");
    }

    #[test]
    fn annotation_on_annotation() {
        compare("annotation-with-annotation");
    }

    #[test]
    fn non_built_in_annotation_on_annotation() {
        compare("annotation-with-non-builtin-annotation");
    }

    #[test]
    fn sub_annotation() {
        compare("sub-annotation");
    }

    #[test]
    fn data_property() {
        compare("data-property");
    }

    // #[test]
    // fn literal_escaped() {
    //     compare("literal-escaped");
    // }

    #[test]
    fn named_individual() {
        compare("named-individual");
    }

    #[test]
    fn import() {
        compare("import");
    }

    #[test]
    fn import_with_partial_parse(){
        let b = Build::new();
        let mut p = parser_with_build(&mut slurp_rdfont("import").as_bytes(), &b);
        let _ = p.parse_imports();

        let rdfont = p.as_ontology().unwrap();
        let so:SetOntology = rdfont.into();
        let amont:AxiomMappedOntology = so.into();
        assert_eq!(amont.i().import().count(), 1);
    }

    #[test]
    fn declaration_with_partial_parse(){
        let b = Build::new();
        let mut p = parser_with_build(&mut slurp_rdfont("class").as_bytes(), &b);
        let _ = p.parse_declarations();

        let rdfont = p.as_ontology().unwrap();
        let so:SetOntology = rdfont.into();
        let amont:AxiomMappedOntology = so.into();
        assert_eq!(amont.i().declare_class().count(), 1);
    }

    #[test]
    fn datatype() {
        compare("datatype");
    }

    #[test]
    fn object_has_value() {
        compare("object-has-value");
    }

    #[test]
    fn object_one_of() {
        compare("object-one-of");
    }

    #[test]
    fn inverse() {
        compare("some-inverse");
    }

    #[test]
    fn object_unqualified_cardinality() {
        compare("object-unqualified-max-cardinality");
    }

    #[test]
    fn object_min_cardinality() {
        compare("object-min-cardinality");
    }

    #[test]
    fn object_max_cardinality() {
        compare("object-max-cardinality");
    }

    #[test]
    fn object_exact_cardinality() {
        compare("object-exact-cardinality");
    }

    #[test]
    fn datatype_alias() {
        compare("datatype-alias");
    }

    #[test]
    fn datatype_intersection() {
        compare("datatype-intersection");
    }

    #[test]
    fn datatype_union() {
        compare("datatype-union");
    }

    #[test]
    fn datatype_complement() {
        compare("datatype-complement");
    }

    #[test]
    fn datatype_oneof() {
        compare("datatype-oneof");
    }

    #[test]
    fn datatype_some() {
        compare("data-some");
    }

    #[test]
    fn facet_restriction() {
        compare("facet-restriction");
    }

    #[test]
    fn facet_restriction_complex() {
        compare("facet-restriction-complex");
    }

    #[test]
    fn data_only() {
        compare("data-only");
    }

    #[test]
    fn data_exact_cardinality() {
        compare("data-exact-cardinality");
    }

    #[test]
    fn data_has_value() {
        compare("data-has-value");
    }

    #[test]
    fn data_max_cardinality() {
        compare("data-max-cardinality");
    }

    #[test]
    fn data_min_cardinality() {
        compare("data-min-cardinality");
    }

    #[test]
    fn class_assertion() {
        compare("class-assertion");
    }

    #[test]
    fn data_property_assertion() {
        compare("data-property-assertion");
    }

    #[test]
    fn same_individual() {
        compare("same-individual");
    }

    #[test]
    fn different_individuals() {
        compare("different-individual");
    }

    #[test]
    fn negative_data_property_assertion() {
        compare("negative-data-property-assertion");
    }

    #[test]
    fn negative_object_property_assertion() {
        compare("negative-object-property-assertion");
    }

    #[test]
    fn object_property_assertion() {
        compare("object-property-assertion");
    }

    #[test]
    fn data_has_key() {
        compare("data-has-key");
    }

    #[test]
    fn data_property_disjoint() {
        compare("data-property-disjoint");
    }

    #[test]
    fn data_property_domain() {
        compare("data-property-domain");
    }

    #[test]
    fn data_property_equivalent() {
        compare("data-property-equivalent");
    }

    #[test]
    fn data_property_functional() {
        compare("data-property-functional");
    }

    #[test]
    fn data_property_range() {
        compare("data-property-range");
    }

    #[test]
    fn data_property_sub() {
        compare("data-property-sub");
    }

    #[test]
    fn disjoint_object_properties() {
        compare("disjoint-object-properties");
    }

    #[test]
    fn equivalent_object_properties() {
        compare("equivalent-object-properties");
    }

    #[test]
    fn object_has_key() {
        compare("object-has-key");
    }

    #[test]
    fn object_property_asymmetric() {
        compare("object-property-asymmetric");
    }

    #[test]
    fn object_property_domain() {
        compare("object-property-domain");
    }

    #[test]
    fn object_property_functional() {
        compare("object-property-functional");
    }

    #[test]
    fn object_property_inverse_functional() {
        compare("object-property-inverse-functional");
    }

    #[test]
    fn object_property_irreflexive() {
        compare("object-property-irreflexive");
    }

    #[test]
    fn object_property_range() {
        compare("object-property-range");
    }

    #[test]
    fn object_property_reflexive() {
        compare("object-property-reflexive");
    }

    #[test]
    fn object_property_symmetric() {
        compare("object-property-symmetric");
    }

    #[test]
    fn family_other() {
        compare("family-other")
    }

    #[test]
    fn type_complex() {
        compare("type-complex")
    }

    #[test]
    fn type_individual_datatype() {
        compare("type-individual-datatype")
    }

    #[test]
    fn type_individual_datatype_unqualified() {
        compare("type-individual-datatype-unqualified")
    }

    #[test]
    fn intersection() {
        compare("intersection")
    }

    #[test]
    fn happy_person() {
        compare("happy_person")
    }

    #[test]
    fn import_property_in_bits() -> Result<(),Error>{
        let b = Build::new();
        let p = parser_with_build(&mut slurp_rdfont("other-property").as_bytes(), &b);
        let (family_other, incomplete) = p.parse()?;
        assert!(incomplete.is_complete());

        let mut p = parser_with_build(&mut slurp_rdfont("import-property").as_bytes(), &b);
        p.parse_imports()?;
        p.parse_declarations()?;
        p.finish_parse(vec![&family_other].as_slice())?;

        let (_rdfont, incomplete) = p.as_ontology_and_incomplete()?;
        assert!(incomplete.is_complete());
        Ok(())
     }


    #[test]
    fn annotation_with_anonymous() {
        let s = slurp_rdfont("annotation-with-anonymous");
        let ont:AxiomMappedOntology = read_ok(
            &mut s.as_bytes()
        ).into();

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
    // fn family_import() -> Result<(),Error>{
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
