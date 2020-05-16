#![allow(unused_imports)]
use Term::*;

use curie::PrefixMapping;

use crate::index::find_declaration_kind;
use crate::index::is_annotation_property;
use crate::index::update_logically_equal_axiom;
use crate::model::Literal;
use crate::model::*;
use crate::vocab::WithIRI;
use crate::vocab::OWL as VOWL;
use crate::vocab::RDF as VRDF;
use crate::vocab::RDFS as VRDFS;

use enum_meta::Meta;
use failure::Error;
use failure::SyncFailure;

use log::{debug, trace};

use sophia::term::BNodeId;
use sophia::term::IriData;
use sophia::term::LiteralKind;

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::BufRead;
use std::rc::Rc;

// Two type aliases for "SoPhia" entities.
type SpTerm = sophia::term::Term<Rc<str>>;
type SpIri = IriData<Rc<str>>;
type SpBNode = BNodeId<Rc<str>>;

macro_rules! some {
    ($body:expr) => {
        (|| Some($body))()
    };
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Term {
    Iri(IRI),
    BNode(SpBNode),
    Literal(Rc<str>, LiteralKind<Rc<str>>),
    Variable(Rc<str>),
    OWL(VOWL),
    RDF(VRDF),
    RDFS(VRDFS),
}

impl Term {
    fn ord(&self) -> isize {
        match self {
            OWL(_) => 1,
            RDF(_) => 2,
            RDFS(_) => 3,
            Iri(_) => 4,
            BNode(_) => 5,
            Literal(_, _) => 6,
            Variable(_) => 7,
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
            (Iri(s), Iri(o)) => s.to_string().cmp(&o.to_string()),
            (BNode(s), BNode(o)) => (*s).cmp(&(*o)),
            (Literal(s, _), Literal(o, _)) => s.cmp(o),
            (Variable(s), Variable(o)) => s.cmp(o),
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

impl Convert for SpIri {
    fn to_iri(&self, b: &Build) -> IRI {
        b.iri(self.to_string())
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

impl<N: From<IRI>> TryBuild<N> for Option<SpIri> {
    fn to_some_iri(&self, b: &Build) -> Option<IRI> {
        self.as_ref().map(|i| i.to_iri(b))
    }
}

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

fn vocab_to_term<'a, V: WithIRI<'a>>(v: &V) -> SpTerm {
    // unwrap should be safe for all known WithIRIs
    sophia::term::Term::new_iri(Rc::from(v.iri_str())).unwrap()
}

fn vocab_lookup() -> HashMap<SpTerm, Term> {
    let mut m = HashMap::default();

    for v in VOWL::all() {
        m.insert(vocab_to_term(&v), Term::OWL(v));
    }

    for v in VRDFS::all() {
        m.insert(vocab_to_term(&v), Term::RDFS(v));
    }

    for v in VRDF::all() {
        m.insert(vocab_to_term(&v), Term::RDF(v));
    }

    m
}

fn to_term(t: &SpTerm, m: &HashMap<SpTerm, Term>, b: &Build) -> Term {
    if let Some(t) = m.get(t) {
        t.clone()
    } else {
        match t {
            sophia::term::Term::Iri(i) => Iri(i.to_iri(b)),
            sophia::term::Term::BNode(id) => BNode(id.clone()),
            sophia::term::Term::Literal(l, k) => Literal(l.clone(), k.clone()),
            sophia::term::Term::Variable(v) => Variable(v.clone()),
        }
    }
}

struct OntologyParser<'a> {
    o: Ontology,
    b: &'a Build,
}

impl<'a> OntologyParser<'a> {
    fn new(b: &'a Build) -> OntologyParser {
        OntologyParser {
            o: Ontology::default(),
            b,
        }
    }

    fn group_triples(
        &mut self,
        triple: Vec<[Term; 3]>,
        simple: &mut Vec<[Term; 3]>,
        bnode: &mut HashMap<SpBNode, Vec<[Term; 3]>>,
    ) {
        // Next group together triples on a BNode, so we have
        // HashMap<BNodeID, Vec<[SpTerm; 3]> All of which should be
        // triples should begin with the BNodeId. We should be able to
        // gather these in a single pass.
        for t in &triple {
            match t {
                [BNode(id), _, _] => {
                    let v = bnode.entry(id.clone()).or_insert_with(Vec::new);
                    v.push(t.clone())
                }
                _ => {
                    simple.push(t.clone());
                }
            }
        }
    }

    fn stitch_seqs_1(
        &self,
        bnode: &mut HashMap<SpBNode, Vec<[Term; 3]>>,
        bnode_seq: &mut HashMap<SpBNode, Vec<Term>>,
    ) {
        let mut temp_bnode: HashMap<SpBNode, Vec<[Term; 3]>> = Default::default();
        let mut extended = false;
        std::mem::swap(bnode, &mut temp_bnode);

        for (k, v) in temp_bnode {
            #[rustfmt::skip]
            let _ = match v.as_slice() {
                [[_, Term::RDF(VRDF::First), val],
                 [_, Term::RDF(VRDF::Rest), Term::BNode(bnode_id)]] => {
                    let some_seq = bnode_seq.remove(bnode_id);
                    if let Some(mut seq) = some_seq {
                        seq.push(val.clone());
                        bnode_seq.insert(k.clone(), seq);
                        extended = true;
                    } else {
                        bnode.insert(k, v);
                    }
                }
                _ => {
                    bnode.insert(k, v);
                }
            };
        }

        if extended && bnode.len() > 0 {
            self.stitch_seqs_1(bnode, bnode_seq)
        }
    }

    fn stitch_seqs(
        &self,
        bnode: &mut HashMap<SpBNode, Vec<[Term; 3]>>,
    ) -> HashMap<SpBNode, Vec<Term>> {
        let mut bnode_seq_reversed: HashMap<SpBNode, Vec<Term>> = Default::default();
        let mut temp_bnode: HashMap<SpBNode, Vec<[Term; 3]>> = Default::default();
        std::mem::swap(bnode, &mut temp_bnode);

        for (k, v) in temp_bnode {
            #[rustfmt::skip]
            let _ = match v.as_slice() {
                [[_, Term::RDF(VRDF::First), val],
                 [_, Term::RDF(VRDF::Rest), Term::RDF(VRDF::Nil)]] =>
                {
                    bnode_seq_reversed.insert(k.clone(), vec![val.clone()]);
                }
                _ => {
                    bnode.insert(k, v);
                }
            };
        }

        self.stitch_seqs_1(bnode, &mut bnode_seq_reversed);

        for (_, v) in bnode_seq_reversed.iter_mut() {
            v.reverse();
        }

        bnode_seq_reversed
    }

    fn resolve_imports(&mut self) {
        // Section 3.1.2/table 4 of RDF Graphs
    }

    fn headers(&mut self, simple: &mut Vec<[Term; 3]>) {
        //Section 3.1.2/table 4
        //   *:x rdf:type owl:Ontology .
        //[ *:x owl:versionIRI *:y .]
        let mut iri: Option<IRI> = None;
        let mut viri: Option<IRI> = None;
        let mut temp_simple = vec![];
        std::mem::swap(&mut temp_simple, simple);

        for t in temp_simple {
            match t {
                [Term::Iri(s), Term::RDF(VRDF::Type), Term::OWL(VOWL::Ontology)] => {
                    iri = Some(s.clone());
                }
                [Term::Iri(s), Term::OWL(VOWL::VersionIRI), Term::Iri(ob)]
                    if iri.as_ref() == Some(&s) =>
                {
                    viri = Some(ob.clone());
                }
                _ => simple.push(t.clone()),
            }
        }

        self.o.id.iri = iri;
        self.o.id.viri = viri;
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
            [_, Iri(p), Term::Literal(ob, kind)] => Annotation {
                ap: AnnotationProperty(p.clone()),
                av: match kind {
                    LiteralKind::Lang(lang) => Literal::Language {
                        lang: lang.clone().to_string(),
                        literal: ob.clone().to_string(),
                    }
                    .into(),
                    LiteralKind::Datatype(iri)
                        if iri.to_string() == "http://www.w3.org/2001/XMLSchema#string" =>
                    {
                        Literal::Simple {
                            literal: ob.clone().to_string(),
                        }
                        .into()
                    }
                    LiteralKind::Datatype(iri) => Literal::Datatype {
                        datatype_iri: iri.to_iri(self.b),
                        literal: ob.clone().to_string(),
                    }
                    .into(),
                },
            },
            [_, Iri(p), Iri(ob)] => {
                // IRI annotation value
                Annotation {
                    ap: AnnotationProperty(p.clone()),
                    av: ob.clone().into(),
                }
            }
            _ => todo!(),
        }
    }

    fn merge<A: Into<AnnotatedAxiom>>(&mut self, ax: A) {
        let ax = ax.into();
        update_logically_equal_axiom(&mut self.o, ax);
    }

    fn in_or_merge<A: Into<AnnotatedAxiom>>(&mut self, ax: A) {
        let ax = ax.into();
        let is_empty = ax.ann.is_empty();

        if is_empty {
            self.o.insert(ax);
        } else {
            update_logically_equal_axiom(&mut self.o, ax);
        }
    }

    fn axiom_annotations(
        &mut self,
        simple: &mut Vec<[Term; 3]>,
        bnode: &mut HashMap<SpBNode, Vec<[Term; 3]>>,
    ) -> HashMap<[Term; 3], BTreeSet<Annotation>> {
        let mut annotated_triples = HashMap::new();
        let mut temp_bnode = HashMap::new();
        std::mem::swap(&mut temp_bnode, bnode);

        for (k, v) in temp_bnode {
            match v.as_slice() {
                #[rustfmt::skip]
                [[_, Term::OWL(VOWL::AnnotatedProperty), p],
                 [_, Term::OWL(VOWL::AnnotatedSource), sb],
                 [_, Term::OWL(VOWL::AnnotatedTarget), ob],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Axiom)],
                 ann @ ..] =>
                {
                    annotated_triples.insert(
                        [sb.clone(), p.clone(), ob.clone()],
                        self.parse_annotations(ann),
                    );
                    simple.push([sb.clone(), p.clone(), ob.clone()])
                }

                _ => {
                    bnode.insert(k, v);
                }
            }
        }
        annotated_triples
    }

    fn declarations(
        &mut self,
        simple: &mut Vec<[Term; 3]>,
        ann_map: &mut HashMap<[Term; 3], BTreeSet<Annotation>>,
    ) {
        // Table 7
        for triple in std::mem::take(simple) {
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
                        Term::RDFS(VRDFS::Datatype) => Some(Datatype(s.clone()).into()),
                        _ => None,
                    }
                }
                _ => None,
            };

            if let Some(entity) = entity {
                self.merge(AnnotatedAxiom {
                    axiom: declaration(entity),
                    ann: ann_map.remove(&triple).unwrap_or_else(|| BTreeSet::new()),
                })
            } else {
                simple.push(triple);
            }
        }
    }

    fn object_property_expressions(
        &mut self,
        bnode: &mut HashMap<SpBNode, Vec<[Term; 3]>>,
    ) -> HashMap<SpBNode, ObjectPropertyExpression> {
        let mut object_property_expression = HashMap::new();

        for (this_bnode, v) in std::mem::take(bnode) {
            let ope = match v.as_slice() {
                [[Term::BNode(_), Term::OWL(VOWL::InverseOf), Term::Iri(iri)]] => {
                    Some(ObjectPropertyExpression::InverseObjectProperty(iri.into()))
                }
                _ => None,
            };

            if let Some(ope) = ope {
                object_property_expression.insert(this_bnode, ope);
            } else {
                bnode.insert(this_bnode, v);
            }
        }

        object_property_expression
    }

    fn class_expressions(
        &mut self,
        bnode: &mut HashMap<SpBNode, Vec<[Term; 3]>>,
        bnode_seq: &mut HashMap<SpBNode, Vec<Term>>,
    ) -> HashMap<SpBNode, ClassExpression> {
        let mut hm = HashMap::new();
        self.class_expressions_1(bnode, bnode_seq, &mut hm);
        hm
    }

    fn to_iri(&self, t: &Term) -> Option<IRI> {
        match t {
            Term::Iri(iri) => Some(iri.clone()),
            _ => None,
        }
    }

    fn to_sope(
        &mut self,
        t: &Term,
        object_property_expression: &mut HashMap<SpBNode, ObjectPropertyExpression>,
    ) -> Option<SubObjectPropertyExpression> {
        Some(self.to_ope(t, object_property_expression)?.into())
    }

    fn to_ope(
        &mut self,
        t: &Term,
        object_property_expression: &mut HashMap<SpBNode, ObjectPropertyExpression>,
    ) -> Option<ObjectPropertyExpression> {
        match t {
            Term::Iri(iri) => Some(ObjectProperty(iri.clone()).into()),
            Term::BNode(id) => object_property_expression.remove(id),
            _ => None,
        }
    }

    fn to_ce(
        &mut self,
        tce: &Term,
        class_expression: &mut HashMap<SpBNode, ClassExpression>,
    ) -> Option<ClassExpression> {
        match tce {
            Term::Iri(cl) => Some(Class(cl.clone()).into()),
            Term::BNode(id) => class_expression.remove(id),
            _ => None,
        }
    }

    fn to_ce_seq(
        &mut self,
        term_seq: &Option<Vec<Term>>,
        class_expression: &mut HashMap<SpBNode, ClassExpression>,
    ) -> Option<Vec<ClassExpression>> {
        let v: Vec<Option<ClassExpression>> = term_seq
            .as_ref()?
            .into_iter()
            .map(|tce| self.to_ce(&tce, class_expression))
            .collect();

        // All or nothing
        v.into_iter().collect()
    }

    fn to_dr(&self, t: &Term) -> Option<DataRange> {
        match t {
            Term::Iri(iri) => {
                let dt: Datatype = iri.into();
                Some(dt.into())
            }
            _ => todo!(),
        }
    }

    fn to_u32(&self, t: &Term) -> Option<u32> {
        match t {
            Term::Literal(val, LiteralKind::Datatype(_)) => val.parse::<u32>().ok(),
            _ => None,
        }
    }

    fn find_property_kind(&self, iri: &IRI) -> PropertyExpression {
        match find_declaration_kind(&self.o, iri) {
            Some(NamedEntityKind::AnnotationProperty) => {
                PropertyExpression::AnnotationProperty(iri.into())
            }
            Some(NamedEntityKind::DataProperty) => PropertyExpression::DataProperty(iri.into()),
            Some(NamedEntityKind::ObjectProperty) => {
                PropertyExpression::ObjectPropertyExpression(iri.into())
            }
            _ => todo!(),
        }
    }

    fn class_expressions_1(
        &mut self,
        bnode: &mut HashMap<SpBNode, Vec<[Term; 3]>>,
        bnode_seq: &mut HashMap<SpBNode, Vec<Term>>,
        class_expression: &mut HashMap<SpBNode, ClassExpression>,
    ) {
        let class_expression_len = class_expression.len();
        for (this_bnode, v) in std::mem::take(bnode) {
            // rustfmt breaks this (putting the triples all on one
            // line) so skip
            #[rustfmt::skip]
            let ce = match v.as_slice() {
                [[_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],
                 [_, Term::OWL(VOWL::SomeValuesFrom), ce_or_dr],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    match self.find_property_kind(pr) {
                         PropertyExpression::ObjectPropertyExpression(ope) =>
                            some!{
                                ClassExpression::ObjectSomeValuesFrom {
                                    ope,
                                    bce: self.to_ce(ce_or_dr, class_expression)?.into()
                                }
                            },
                         PropertyExpression::DataProperty(dp) => {
                             some!{
                                 ClassExpression::DataSomeValuesFrom {
                                     dp,
                                     dr: self.to_dr(ce_or_dr)?
                                 }

                             }
                         },
                         _ => panic!("Unexpected Property Kind")
                    }
                }
                [[_, Term::OWL(VOWL::AllValuesFrom), ce_or_dr],
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]] => {
                    match self.find_property_kind(pr) {
                        PropertyExpression::ObjectPropertyExpression(ope) => {
                            some!{
                                ClassExpression::ObjectAllValuesFrom {
                                    ope,
                                    bce: self.to_ce(ce_or_dr, class_expression)?.into()
                                }
                            }
                        },
                        PropertyExpression::DataProperty(dp) => {
                            some! {
                                ClassExpression::DataAllValuesFrom {
                                    dp,
                                    dr: self.to_dr(ce_or_dr)?
                                }
                            }
                        },
                        _ => panic!("Unexpected Property Kind")
                    }
                },
                [[_, Term::OWL(VOWL::IntersectionOf), Term::BNode(bnodeid)],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    some!{
                        ClassExpression::ObjectIntersectionOf(
                            self.to_ce_seq(&bnode_seq.remove(bnodeid), class_expression)?
                        )
                    }
                },
                [[_, Term::OWL(VOWL::UnionOf), Term::BNode(bnodeid)],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    some!{
                        ClassExpression::ObjectUnionOf(
                            self.to_ce_seq(
                                &bnode_seq.remove(bnodeid),
                                class_expression
                            )?
                        )
                    }
                },
                [[_, Term::OWL(VOWL::ComplementOf), tce],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)]] => {
                    some!{
                        ClassExpression::ObjectComplementOf(
                            self.to_ce(&tce, class_expression)?.into()
                        )
                    }
                },
                [[_, Term::OWL(VOWL::OnClass), tce],
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],
                 [_, Term::OWL(VOWL::QualifiedCardinality), literal],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectExactCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.to_ce(tce, class_expression)?.into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MinQualifiedCardinality), literal],
                 [_, Term::OWL(VOWL::OnClass), tce],
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectMinCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.to_ce(tce, class_expression)?.into()
                        }
                    }
                }
                [[_, Term::OWL(VOWL::MaxQualifiedCardinality), literal],
                 [_, Term::OWL(VOWL::OnClass), tce],
                 [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],
                 [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)]
                ] => {
                    some!{
                        ClassExpression::ObjectMaxCardinality
                        {
                            n:self.to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.to_ce(tce, class_expression)?.into()
                        }
                    }
                }

                _a => None,
            };

            if let Some(ce) = ce {
                class_expression.insert(this_bnode, ce);
            } else {
                bnode.insert(this_bnode, v);
            }
        }

        if class_expression.len() > class_expression_len {
            dbg!("Recursing");
            self.class_expressions_1(bnode, bnode_seq, class_expression)
        }
    }

    fn axioms(
        &mut self,
        simple: &mut Vec<[Term; 3]>,
        ann_map: &mut HashMap<[Term; 3], BTreeSet<Annotation>>,
        class_expression: &mut HashMap<SpBNode, ClassExpression>,
        bnode_seq: &mut HashMap<SpBNode, Vec<Term>>,
        object_property_expression: &mut HashMap<SpBNode, ObjectPropertyExpression>,
    ) {
        for triple in std::mem::take(simple) {
            let axiom: Option<Axiom> = match &triple {
                [Term::Iri(sub), Term::RDFS(VRDFS::SubClassOf), tce] => some! {
                    SubClassOf {
                        sub: Class(sub.clone()).into(),
                        sup: self.to_ce(tce, class_expression)?,
                    }
                    .into()
                },
                // TODO: We need to check whether these
                // EquivalentClasses have any other EquivalentClasses
                // and add to that axiom
                [Term::Iri(a), Term::OWL(VOWL::EquivalentClass), Term::Iri(b)] => {
                    Some(
                        EquivalentClasses(vec![
                            // The order is not important here, but
                            // this way around matches with the XML reader
                            Class(b.clone()).into(),
                            Class(a.clone()).into(),
                        ])
                        .into(),
                    )
                }
                [Term::Iri(a), Term::OWL(VOWL::DisjointWith), Term::Iri(b)] => {
                    Some(
                        DisjointClasses(vec![
                            // The order is not important here, but
                            // this way around matches with the XML reader
                            Class(b.clone()).into(),
                            Class(a.clone()).into(),
                        ])
                        .into(),
                    )
                }
                [pr, Term::RDFS(VRDFS::SubPropertyOf), t] => some! {
                    SubObjectPropertyOf {
                        sub: self.to_sope(pr, object_property_expression)?,
                        sup: self.to_ope(t, object_property_expression)?,
                    }.into()
                },
                [Term::Iri(pr), Term::OWL(VOWL::PropertyChainAxiom), Term::BNode(id)] => {
                    some! {
                        SubObjectPropertyOf {
                            sub: SubObjectPropertyExpression::ObjectPropertyChain(
                                bnode_seq
                                    .remove(id)?
                                    .iter()
                                    .map(|t| self.to_ope(t, object_property_expression).unwrap())
                                    .collect()
                            ),
                            sup: ObjectProperty(pr.clone()).into(),
                        }.into()
                    }
                }
                [Term::Iri(pr), Term::RDFS(VRDFS::Domain), t] => {
                    some! {
                        match self.find_property_kind(pr) {
                            PropertyExpression::ObjectPropertyExpression(ope) => ObjectPropertyDomain {
                                ope,
                                ce: self.to_ce(t, class_expression)?,
                            }
                            .into(),
                            PropertyExpression::DataProperty(dp) => DataPropertyDomain {
                                dp,
                                ce: self.to_ce(t, class_expression)?,
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
                [Term::Iri(pr), Term::RDFS(VRDFS::Range), t] => some! {
                    match self.find_property_kind(pr) {
                        PropertyExpression::ObjectPropertyExpression(ope) => ObjectPropertyRange {
                            ope,
                            ce: self.to_ce(t, class_expression)?,
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
                _ => None,
            };

            if let Some(axiom) = axiom {
                self.merge(AnnotatedAxiom {
                    axiom,
                    ann: ann_map.remove(&triple).unwrap_or_else(|| BTreeSet::new()),
                })
            } else {
                simple.push(triple)
            }
        }
    }

    fn simple_annotations(&mut self, simple: &mut Vec<[Term; 3]>) {
        for triple in std::mem::take(simple) {
            if let Some(iri) = match &triple {
                [Term::Iri(iri), Term::RDFS(rdfs), _] if rdfs.is_builtin() => Some(iri),
                [Term::Iri(iri), Term::OWL(VOWL::VersionInfo), _] => Some(iri),
                [Term::Iri(iri), Term::Iri(ap), _] if is_annotation_property(&self.o, &ap) => {
                    eprintln!("is annotation");
                    Some(iri)
                }
                _ => None,
            } {
                self.o.insert(AnnotationAssertion {
                    subject: iri.clone(),
                    ann: self.annotation(&triple),
                });
            } else {
                simple.push(triple);
            }
        }
    }

    fn read(mut self, triple: Vec<[SpTerm; 3]>) -> Result<Ontology, Error> {
        // move to our own Terms, with IRIs swapped

        let m = vocab_lookup();
        let triple: Vec<[Term; 3]> = triple
            .into_iter()
            .map(|t| {
                [
                    to_term(&t[0], &m, self.b),
                    to_term(&t[1], &m, self.b),
                    to_term(&t[2], &m, self.b),
                ]
            })
            .collect();

        let mut simple: Vec<[Term; 3]> = vec![];
        let mut bnode: HashMap<SpBNode, Vec<[Term; 3]>> = Default::default();

        self.group_triples(triple, &mut simple, &mut bnode);

        // sort the triples, so that I can get a dependable order
        for (_, vec) in bnode.iter_mut() {
            vec.sort();
        }

        let mut bnode_seq = self.stitch_seqs(&mut bnode);

        // Table 10
        let mut ann_map = self.axiom_annotations(&mut simple, &mut bnode);

        self.resolve_imports();
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
        self.headers(&mut simple);

        // Can we pull out annotations at this point and handle them
        // as we do in reader2? Tranform them into a triple which we
        // handle normally, then bung the annotation on later?

        // Table 5: Backward compability -- skip this for now (maybe
        // for ever)

        // Table 6: Don't understand this

        // Table 7: Declarations (this should be simple, if we have a
        // generic solution for handling annotations, there is no
        // handling of bnodes).
        self.declarations(&mut simple, &mut ann_map);

        // Table 10
        self.simple_annotations(&mut simple);

        // Table 8:
        let mut object_property_expression = self.object_property_expressions(&mut bnode);
        // Table 13: Parsing of Class Expressions
        let mut class_expression = self.class_expressions(&mut bnode, &mut bnode_seq);
        // Table 16: Axioms without annotations
        self.axioms(
            &mut simple,
            &mut ann_map,
            &mut class_expression,
            &mut bnode_seq,
            &mut object_property_expression,
        );

        if simple.len() > 0 {
            dbg!("simple remaining", simple);
        }

        if bnode.len() > 0 {
            dbg!("bnodes remaining", bnode);
        }

        if bnode_seq.len() > 0 {
            dbg!("sequences remaining", bnode_seq);
        }

        if ann_map.len() > 0 {
            dbg!("annotations remaining", ann_map);
        }

        if class_expression.len() > 0 {
            dbg!("class_expression remaining", class_expression);
        }

        if object_property_expression.len() > 0 {
            dbg!(
                "object property expression remaining",
                object_property_expression
            );
        }
        Ok(self.o)
    }
}

pub fn read_with_build<R: BufRead>(
    bufread: &mut R,
    build: &Build,
) -> Result<(Ontology, PrefixMapping), Error> {
    let parser = sophia::parser::xml::Config::default();
    let triple_iter = parser.parse_bufread(bufread);

    let triple_result: Result<Vec<_>, _> = triple_iter.collect();
    let triple_v: Vec<[SpTerm; 3]> = triple_result.map_err(SyncFailure::new)?;

    return OntologyParser::new(build)
        .read(triple_v)
        .map(|o| return (o, PrefixMapping::default()));
}

pub fn read<R: BufRead>(bufread: &mut R) -> Result<(Ontology, PrefixMapping), Error> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

#[cfg(test)]
mod test {
    use super::*;

    use std::io::Write;
    use std::path::PathBuf;

    use pretty_assertions::assert_eq;

    fn init_log() {
        let _ = env_logger::builder()
            .format(|buf, record| writeln!(buf, "{}", record.args()))
            .is_test(true)
            .try_init();
    }

    fn read_ok<R: BufRead>(bufread: &mut R) -> (Ontology, PrefixMapping) {
        init_log();

        let r = read(bufread);

        assert!(r.is_ok(), "Expected ontology, get failure: {:?}", r.err());
        r.unwrap()
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

    fn compare_str(rdfread: &str, xmlread: &str) {
        let (rdfont, _rdfmapping) = read_ok(&mut rdfread.as_bytes());
        let (xmlont, _xmlmapping) = crate::io::reader::test::read_ok(&mut xmlread.as_bytes());

        //dbg!(&rdfont); if true {panic!()};

        assert_eq!(rdfont, xmlont);

        //let rdfmapping: &HashMap<&String, &String> = &rdfmapping.mappings().collect();
        //let xmlmapping: &HashMap<&String, &String> = &xmlmapping.mappings().collect();

        //assert_eq!(rdfmapping, xmlmapping);
    }

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

    // #[test]
    // fn one_ontology_annotation() {
    //     compare("one-ontology-annotation");
    // }

    #[test]
    fn one_equivalent_class() {
        compare("one-equivalent");
    }

    #[test]
    fn one_disjoint_class() {
        compare("one-disjoint");
    }

    // #[test]
    // fn disjoint_union() {
    //    compare("disjoint-union");
    // }

    #[test]
    fn sub_oproperty() {
        compare("suboproperty");
    }

    #[test]
    fn sub_oproperty_inverse() {
        compare("suboproperty-inverse");
    }

    // #[test]
    // fn one_inverse() {
    //     compare("inverse-properties");
    // }

    // #[test]
    // fn one_transitive() {
    //     compare("transitive-properties");
    // }

    // #[test]
    // fn inverse_transitive() {
    //     compare("inverse-transitive")
    // }

    // #[test]
    // fn one_annotated_transitive() {
    //     compare("annotation-on-transitive");
    // }

    #[test]
    fn subproperty_chain() {
        compare("subproperty-chain");
    }

    // #[test]
    // fn one_subproperty_chain_with_inverse() {
    //     compare("subproperty-chain-with-inverse");
    // }

    //#[test]
    //fn annotation_on_annotation() {
    //    compare("annotation-with-annotation");
    //}

    // #[test]
    // fn sub_annotation() {
    //     compare("sub-annotation");
    // }

    #[test]
    fn data_property() {
        compare("data-property");
    }

    // #[test]
    // fn literal_escaped() {
    //     compare("literal-escaped");
    // }

    // #[test]
    // fn named_individual() {
    //     compare("named-individual");
    // }

    // #[test]
    // fn import() {
    //     compare("import");
    // }

    #[test]
    fn datatype() {
        compare("datatype");
    }

    // #[test]
    // fn object_has_value() {
    //     compare("object-has-value");
    // }

    // #[test]
    // fn object_one_of() {
    //     compare("object-one-of");
    // }

    // #[test]
    // fn inverse() {
    //     compare("some-inverse");
    // }

    // #[test]
    // fn object_unqualified_cardinality() {
    //     compare("object-unqualified-max-cardinality");
    // }

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

    // #[test]
    // fn datatype_alias() {
    //     compare("datatype-alias");
    // }

    // #[test]
    // fn datatype_intersection() {
    //     compare("datatype-intersection");
    // }

    // #[test]
    // fn datatype_union() {
    //     compare("datatype-union");
    // }

    // #[test]
    // fn datatype_complement() {
    //     compare("datatype-complement");
    // }

    // #[test]
    // fn datatype_oneof() {
    //     compare("datatype-oneof");
    // }

    #[test]
    fn datatype_some() {
        compare("data-some");
    }

    // #[test]
    // fn facet_restriction() {
    //     compare("facet-restriction");
    // }

    #[test]
    fn data_only() {
        compare("data-only");
    }

    // #[test]
    // fn data_exact_cardinality() {
    //     compare("data-exact-cardinality");
    // }

    // #[test]
    // fn data_has_value() {
    //     compare("data-has-value");
    // }

    // #[test]
    // fn data_max_cardinality() {
    //     compare("data-max-cardinality");
    // }

    // #[test]
    // fn data_min_cardinality() {
    //     compare("data-min-cardinality");
    // }

    // #[test]
    // fn class_assertion() {
    //     compare("class-assertion");
    // }

    // #[test]
    // fn data_property_assertion() {
    //     compare("data-property-assertion");
    // }

    // #[test]
    // fn same_individual() {
    //     compare("same-individual");
    // }

    // #[test]
    // fn different_individuals() {
    //     compare("different-individual");
    // }

    // #[test]
    // fn negative_data_property_assertion() {
    //     compare("negative-data-property-assertion");
    // }

    // #[test]
    // fn negative_object_property_assertion() {
    //     compare("negative-object-property-assertion");
    // }

    // #[test]
    // fn object_property_assertion() {
    //     compare("object-property-assertion");
    // }

    // #[test]
    // fn data_has_key() {
    //     compare("data-has-key");
    // }

    // #[test]
    // fn data_property_disjoint() {
    //     compare("data-property-disjoint");
    // }

    #[test]
    fn data_property_domain() {
        compare("data-property-domain");
    }

    // #[test]
    // fn data_property_equivalent() {
    //     compare("data-property-equivalent");
    // }

    // #[test]
    // fn data_property_functional() {
    //     compare("data-property-functional");
    // }

    #[test]
    fn data_property_range() {
        compare("data-property-range");
    }

    // #[test]
    // fn data_property_sub() {
    //     compare("data-property-sub");
    // }

    // #[test]
    // fn disjoint_object_properties() {
    //     compare("disjoint-object-properties");
    // }

    // #[test]
    // fn equivalent_object_properties() {
    //     compare("equivalent_object_properties");
    // }

    // #[test]
    // fn object_has_key() {
    //     compare("object-has-key");
    // }

    // #[test]
    // fn object_property_asymmetric() {
    //    compare("object-property-asymmetric");
    // }

    #[test]
    fn object_property_domain() {
        compare("object-property-domain");
    }

    // #[test]
    // fn object_property_functional() {
    //     compare("object-property-functional");
    // }

    // #[test]
    // fn object_property_inverse_functional() {
    //     compare("object-property-inverse-functional");
    // }

    // #[test]
    // fn object_property_irreflexive() {
    //     compare("object-property-irreflexive");
    // }

    #[test]
    fn object_property_range() {
        compare("object-property-range");
    }

    // #[test]
    // fn object_property_reflexive() {
    //     compare("object-property-reflexive");
    // }

    // #[test]
    // fn object_property_symmetric() {
    //     compare("object-property-symmetric");
    // }

    // #[test]
    // fn family() {
    //     compare("family");
    // }
}
