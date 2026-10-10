use Term::*;
use oxrdf::{BlankNode, NamedNode, NamedOrBlankNode, Triple};

use crate::{
    error::HornedError,
    io::{ParserConfiguration, RDFParserConfiguration},
    vocab::Facet,
};
use crate::{model::Literal, ontology::component_mapped::ComponentMappedOntology};
use crate::{model::*, vocab::Vocab};

use crate::ontology::indexed::ForIndex;
use crate::vocab::OWL as VOWL;
use crate::vocab::OWL2Datatype;
use crate::vocab::RDF as VRDF;
use crate::vocab::SWRL as VSWRL;
use crate::vocab::is_annotation_builtin;
use crate::{
    ontology::{
        declaration_mapped::DeclarationMappedIndex,
        indexed::ThreeIndexedOntology,
        logically_equal::{LogicallyEqualIndex, update_or_insert_logically_equal_component},
        set::{SetIndex, SetIndexIter, SetOntology},
    },
    vocab::RDFS as VRDFS,
};

use std::collections::BTreeSet;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::fmt::Debug;
use std::{io::BufRead, marker::PhantomData};

type OxTerm<'a> = ::oxrdf::Term;

/// Evaluate $body which should return a value while allowing the use
/// of the ? operator within body.
///
/// This is useful for unpacking multiple Option return values. The
/// first that unpacks to return makes the whole body return None.
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
#[allow(dead_code)]
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

impl<A: ForIRI> Term<A> {
    fn substitute(self) -> Term<A> {
        if let Term::Iri(ref iri) = self
            && let Some(vocab) = Vocab::lookup(iri)
        {
            return match vocab {
                crate::vocab::Vocab::Facet(facet) => facet.into(),
                crate::vocab::Vocab::RDF(rdf) => rdf.into(),
                crate::vocab::Vocab::RDFS(rdfs) => rdfs.into(),
                crate::vocab::Vocab::OWL(owl) => owl.into(),
                crate::vocab::Vocab::SWRL(swrl) => swrl.into(),
                _ => self,
            };
        }
        self
    }
}

impl<A: ForIRI> TryFrom<&NamedNode> for Term<A> {
    type Error = HornedError;

    fn try_from(value: &NamedNode) -> Result<Self, Self::Error> {
        if let Some(res) = Vocab::lookup(value.as_str()) {
            Term::try_from(res)
        } else {
            Err(HornedError::invalid(value.as_str()))
        }
    }
}

impl TryFrom<&NamedNode> for crate::vocab::XSD {
    type Error = HornedError;

    fn try_from(value: &NamedNode) -> Result<Self, Self::Error> {
        value.as_str().parse::<Self>()
    }
}

impl<A: ForIRI> Build<A> {
    fn to_term_bn(nn: &BlankNode) -> Term<A> {
        Term::BNode(BNode(nn.clone().into_string().into()))
    }

    fn convert_to_pos_triple(&self, rio_triple: Triple, pos: u64) -> PosTriple<A> {
        PosTriple(
            [
                self.to_term_bnn(&rio_triple.subject),
                self.to_term_nn(&rio_triple.predicate),
                self.to_term(&rio_triple.object),
            ],
            pos,
        )
    }

    fn substitute_term(&self, term: [Term<A>; 3]) -> [Term<A>; 3] {
        let [subject, predicate, object] = term;
        let predicate = predicate.substitute();
        let object = if matches!(predicate, Term::RDF(VRDF::Type)) {
            object.substitute()
        } else {
            object
        };
        [subject, predicate, object]
    }

    fn substitute_triple(&self, triple: PosTriple<A>) -> PosTriple<A> {
        let PosTriple(term, pos) = triple;

        let term = self.substitute_term(term);

        PosTriple(term, pos)
    }

    fn convert_substitute_triple(&self, rio_triple: Triple, pos: u64) -> PosTriple<A> {
        self.substitute_triple(self.convert_to_pos_triple(rio_triple, pos))
    }

    fn to_term(&self, t: &OxTerm) -> Term<A> {
        match t {
            oxrdf::Term::NamedNode(iri) => self.to_term_nn(iri),
            oxrdf::Term::BlankNode(id) => Self::to_term_bn(id),
            oxrdf::Term::Literal(l) => self.to_term_lt(l),
        }
    }

    fn to_term_bnn(&self, subj: &NamedOrBlankNode) -> Term<A> {
        match subj {
            NamedOrBlankNode::NamedNode(nn) => self.to_term_nn(nn),
            NamedOrBlankNode::BlankNode(bn) => Self::to_term_bn(bn),
        }
    }

    fn to_term_nn(&self, nn: &NamedNode) -> Term<A> {
        Term::Iri(self.iri(nn.as_str()))
    }

    fn to_term_lt(&self, lt: &oxrdf::Literal) -> Term<A> {
        if let Some(lang) = lt.language() {
            return Term::Literal(Literal::Language {
                literal: lt.value().to_string(),
                lang: lang.to_string(),
            });
        }

        if lt.datatype().as_str() == "http://www.w3.org/2001/XMLSchema#string" {
            return Term::Literal(Literal::Simple {
                literal: lt.value().to_string(),
            });
        }

        Term::Literal(Literal::Datatype {
            literal: lt.value().to_string(),
            datatype_iri: self.iri(lt.datatype().as_str()),
        })
    }
}

macro_rules! d {
    () => {
        Default::default()
    };
}

/// The RDFOntology supports logical equality and IRI->type mapping
/// which are the two speeds ups that we need for RDF parsing.
pub trait RDFOntology<A: ForIRI, AA: ForIndex<A>>:
    AsRef<LogicallyEqualIndex<A, AA>>
    + AsRef<DeclarationMappedIndex<A, AA>>
    + AsRef<SetIndex<A, AA>>
    + Default
    + Debug
    + MutableOntology<A>
{
}

impl<A: ForIRI, AA: ForIndex<A>, T> RDFOntology<A, AA> for T where
    T: AsRef<LogicallyEqualIndex<A, AA>>
        + AsRef<DeclarationMappedIndex<A, AA>>
        + AsRef<SetIndex<A, AA>>
        + Default
        + Debug
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

impl<A: ForIRI, AA: ForIndex<A>> Ontology<A> for ConcreteRDFOntology<A, AA> {
    type ComponentIter<'c>
        = SetIndexIter<'c, A, AA>
    where
        Self: 'c,
        A: 'c;

    fn iter(&self) -> Self::ComponentIter<'_> {
        self.i().into_iter()
    }
}

impl<A: ForIRI, AA: ForIndex<A>> IntoIterator for ConcreteRDFOntology<A, AA> {
    type Item = AnnotatedComponent<A>;
    type IntoIter = <SetIndex<A, AA> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        let (i, _, _) = self.index();
        i.into_iter()
    }
}

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

impl ConcreteRDFOntology<RcStr, crate::model::RcAnnotatedComponent> {
    /// Fast conversion into a `SetOntology`: drop the declaration/equality
    /// indexes first (releasing their shared `Rc` references) so the remaining
    /// component `Rc`s are uniquely held, then MOVE the components out instead of
    /// deep-cloning them. The naive `From` clones all ~5.5M components on a large
    /// ontology; this avoids that half of the conversion cost.
    pub fn into_set_ontology_fast(self) -> SetOntology<RcStr> {
        let (set_index, decl, equal) = self.index();
        drop(decl);
        drop(equal);
        set_index.into_set_ontology_moving()
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

/// Represents all the parts of a set of RDF triples that were not
/// able to be completed parsed to OWL2 structures.
#[derive(Debug, Default)]
pub struct IncompleteParse<A: ForIRI> {
    /// Simple Triples are those were subject, object and predicate
    /// are all IRIs
    pub simple: Vec<PosTriple<A>>,
    /// BNode triples are those that start with a BNode, except where
    /// they are part of an RDF sequence.
    pub bnode: Vec<VPosTriple<A>>,
    /// BNode seq are those triples that are part of a sequence.
    pub bnode_seq: Vec<Vec<Term<A>>>,

    /// ClassExpression's that are otherwise unconnected to
    /// other parts of the Ontology.
    pub class_expression: Vec<ClassExpression<A>>,

    /// ObjectPropertyExpression' that are otherwise unconnected to
    /// other parts of the Ontology.
    pub object_property_expression: Vec<ObjectPropertyExpression<A>>,
    /// DataRange's that are otherwise unconnected to other parts of the
    /// Ontology.
    pub data_range: Vec<DataRange<A>>,
    /// Atom's that are otherwise unconnected to other parts of the
    /// Ontology.
    pub atom: HashMap<Term<A>, Atom<A>>,

    /// Annotations that are otherwise unconnected to other parts of
    /// the Ontology
    // A base triple may be reified by several `owl:Axiom` blocks, each
    // carrying a different annotation set (e.g. one synonym with two separate
    // xref provenances). Keep them all so every annotated axiom is recovered;
    // a single `BTreeSet` here silently dropped all but one (nondeterministic).
    pub ann_map: HashMap<[Term<A>; 3], Vec<BTreeSet<Annotation<A>>>>,
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

/// A triple of terms with a position from the file from which the
/// triple was read.
#[derive(Clone, Debug)]
pub struct PosTriple<A: ForIRI>([Term<A>; 3], u64);

impl<A: ForIRI> From<[Term<A>; 3]> for PosTriple<A> {
    fn from(t: [Term<A>; 3]) -> PosTriple<A> {
        PosTriple(t, 0)
    }
}

impl<A: ForIRI> PosTriple<A> {
    pub fn triple(&self) -> &[Term<A>; 3] {
        &self.0
    }

    pub fn as_triple(self) -> [Term<A>; 3] {
        self.0
    }

    pub fn triple_mut(&mut self) -> &mut [Term<A>; 3] {
        &mut self.0
    }

    pub fn position(&self) -> u64 {
        self.1
    }
}

/// A set of triples with a position in the file from which the
/// triples were loaded.
#[derive(Debug)]
pub struct VPosTriple<A: ForIRI>(Vec<[Term<A>; 3]>, u64);

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

impl<A: ForIRI> VPosTriple<A> {
    pub fn vec_triple(&self) -> &Vec<[Term<A>; 3]> {
        &self.0
    }

    pub fn as_triple(self) -> Vec<[Term<A>; 3]> {
        self.0
    }

    pub fn triple_mut(&mut self) -> &mut Vec<[Term<A>; 3]> {
        &mut self.0
    }

    pub fn position(&self) -> u64 {
        self.1
    }
}

/// `owl:Annotation` nodes, each with its triples, under the source each names.
type AnnotationNodes<A> = HashMap<Term<A>, Vec<(BNode<A>, VPosTriple<A>)>>;

/// An ontology parser which takes a set of RDF triples and turns them
/// into an RDFOntology.
#[derive(Debug)]
pub struct OntologyParser<
    A: ForIRI,
    AA: ForIndex<A>,
    O: RDFOntology<A, AA>,
    B: AsRef<Build<A>> = Build<A>,
> {
    /// The ontology being populated
    o: O,
    config: ParserConfiguration<A, B>,

    // A vector of the triples from which we are parsing
    triple: Vec<PosTriple<A>>,
    // Triples with an IRI for subject, predicate and object
    simple: Vec<PosTriple<A>>,
    // Triples that start with a BNode
    bnode: HashMap<BNode<A>, VPosTriple<A>>,
    // The object of triples that are part of a sequence, keyed on the
    // bnode subject of the first known triple that is part of that sequence
    bnode_seq: HashMap<BNode<A>, Vec<Term<A>>>,

    // Parsed OWL Objects keyed on their bnode
    class_expression: HashMap<BNode<A>, ClassExpression<A>>,
    object_property_expression: HashMap<BNode<A>, ObjectPropertyExpression<A>>,
    data_range: HashMap<BNode<A>, DataRange<A>>,
    // Bnodes from the three maps above that have been retrieved at least
    // once. Entries there are looked up, not removed, because the same
    // bnode can legitimately be referenced from more than one place (e.g.
    // a restriction shared by two rdf:List members) -- see #254.
    used_bnode: HashSet<BNode<A>>,
    // Blank nodes in object position of some triple: what another construct
    // refers to. A class expression nothing refers to stands alone; see
    // `as_ontology_and_incomplete`.
    referenced_bnodes: HashSet<BNode<A>>,
    // Annotations mapped to Triples (one entry per reifying owl:Axiom block).
    ann_map: HashMap<[Term<A>; 3], Vec<BTreeSet<Annotation<A>>>>,
    // The owl:Annotation nodes not yet read, under the source each names: the
    // node, or the ontology's IRI, one of whose annotations each annotates.
    annotation_nodes: AnnotationNodes<A>,
    // Blank nodes stated `owl:inverseOf` a named property: what an inverse in
    // a list says, which two lists naming one inverse by different nodes share.
    inverse_nodes: HashMap<BNode<A>, IRI<A>>,
    // The lists the reifications of axioms stated with a list name as their
    // `owl:annotatedTarget`: copies of the axiom's own list, read once the
    // axiom takes the annotations keyed by their content.
    list_copies: Vec<BNode<A>>,
    // The annotations a stated list took by its content, under that content's
    // key: a list stated again with the same members states the same axiom.
    claimed_lists: HashMap<[Term<A>; 3], Vec<BTreeSet<Annotation<A>>>>,
    // The base triples restored from reifications that name, as their
    // `owl:annotatedTarget`, a node of their own describing the class
    // expression the axiom states, and the unannotated axioms such a triple's
    // annotated axioms stand for, which go once the parse is done.
    copied_targets: HashSet<[Term<A>; 3]>,
    bare_twins: Vec<Component<A>>,
    // The ontology's header nodes, named or blank: every subject typed
    // `owl:Ontology` and every subject and object of `owl:imports`. What any
    // of them states is the ontology's, and one of them names it.
    ontology_nodes: HashSet<Term<A>>,
    ontology_node: Option<Term<A>>,
    atom: HashMap<Term<A>, Atom<A>>,
    variable: HashMap<IRI<A>, Variable<A>>,

    // Blank nodes in the order this document first mentions them, and the
    // anonymous individual each one turns out to name.
    bnode_order: HashMap<A, usize>,
    bnode_names: std::cell::RefCell<HashMap<A, AnonymousIndividual<A>>>,
    // Whether an anonymous individual is named by its blank node's own label
    // (see `read_statements`), and the order the axiom nodes' annotations are
    // read in.
    keep_labels: bool,
    axiom_rank: HashMap<A, usize>,
    // The owl:Annotation nodes, by label, ranked in the order their annotations
    // are read: of several naming the same annotation of one source, the first
    // annotates it.
    annotation_rank: HashMap<A, usize>,

    // How far through the parse have we got?
    state: OntologyParserState,
    // AA is otherwise unreferenced
    p: PhantomData<AA>,
}

impl<A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>, B: AsRef<Build<A>>>
    OntologyParser<A, AA, O, B>
{
    /// Return a new empty OntologyParser.
    pub fn new(triple: Vec<PosTriple<A>>, config: ParserConfiguration<A, B>) -> Self {
        // A document's blank nodes are numbered as it is parsed: each node it
        // declares takes one id, in the order the document first mentions it,
        // and the individuals among them take the ids that follow. Both counts
        // come off the `Build`, so documents parsed one after another for a
        // merge keep their nodes apart.
        let mut bnode_order: HashMap<A, usize> = HashMap::default();
        for PosTriple(terms, _) in triple.iter() {
            for t in terms {
                if let Term::BNode(BNode(label)) = t {
                    let next = bnode_order.len();
                    bnode_order.entry(label.clone()).or_insert(next);
                }
            }
        }
        config.build.as_ref().skip_bnode_labels(bnode_order.len());

        OntologyParser {
            o: d!(),
            config,
            bnode_order,
            bnode_names: d!(),
            keep_labels: false,
            axiom_rank: d!(),
            annotation_rank: d!(),

            triple,
            simple: d!(),
            bnode: d!(),
            bnode_seq: d!(),
            class_expression: d!(),
            object_property_expression: d!(),
            data_range: d!(),
            used_bnode: d!(),
            referenced_bnodes: d!(),
            ann_map: d!(),
            annotation_nodes: d!(),
            inverse_nodes: d!(),
            list_copies: d!(),
            claimed_lists: d!(),
            copied_targets: d!(),
            ontology_nodes: d!(),
            ontology_node: d!(),
            bare_twins: d!(),
            atom: d!(),
            variable: d!(),
            state: OntologyParserState::New,
            p: d!(),
        }
    }

    /// The anonymous individual a blank node names.
    ///
    /// A document's blank nodes are numbered as it is parsed: each node it
    /// declares takes one id, and the individuals among them take the ids that
    /// follow, in the order the document mentions them. So every triple about
    /// one node meets one individual, and two documents parsed for one merge
    /// keep their nodes apart. Where the document is not being numbered, a
    /// fresh predictable name. Either way a label names one individual for
    /// the whole parse, so every triple about a node meets the same one.
    fn anon_for_bnode(&self, bn: &BNode<A>) -> AnonymousIndividual<A> {
        let known = { self.bnode_names.borrow().get(&bn.0).cloned() };
        if let Some(i) = known {
            return i;
        }
        let b = self.config.build.as_ref();
        let i = if self.keep_labels {
            b.anon(bn.0.as_ref())
        } else {
            match b.next_bnode_label() {
                Some(label) => b.anon(label),
                None => b.anon_renumbered(),
            }
        };
        self.bnode_names.borrow_mut().insert(bn.0.clone(), i.clone());
        i
    }

    /// Return a new OntologyParser taking all triples from an BufRead
    /// in RDF-XML.
    pub fn from_bufread<R: BufRead>(
        bufread: &mut R,
        config: RDFParserConfiguration<A, B>,
    ) -> Result<Self, HornedError> {
        let format = config.format.unwrap_or(oxrdfio::RdfFormat::RdfXml);
        Self::from_bufread_with_format(bufread, config.common, format)
    }

    pub fn from_bufread_with_format<R: BufRead>(
        bufread: &mut R,
        config: ParserConfiguration<A, B>,
        format: oxrdfio::RdfFormat,
    ) -> Result<Self, HornedError> {
        // In lax mode (OWLAPI/ROBOT's default), parse leniently: oxrdf otherwise
        // hard-errors on inputs OWLAPI accepts — e.g. an invalid BCP47 language
        // tag such as `xml:lang="e"` (a real typo in GSSO) — and the parse
        // would then fail on the whole document. Lenient mode keeps the
        // raw language tag / IRI instead of validating it, matching how OWLAPI
        // preserves such literals verbatim.
        let parser = oxrdfio::RdfParser::from_format(format);
        let parser = if config.lax { parser.lenient() } else { parser };
        let mut triples = vec![];
        let last_pos = std::cell::Cell::new(0);

        for ox_quad in parser.for_reader(bufread) {
            let ox_triple = ox_quad
                .map_err(|e| {
                    HornedError::ParserError(Box::new(e), crate::error::Location::Unknown)
                })?
                .into();
            triples.push(
                config
                    .build
                    .as_ref()
                    .convert_substitute_triple(ox_triple, last_pos.get()),
            );
            //last_pos.set(parser.buffer_position().try_into().unwrap());
        }

        Ok(OntologyParser::new(triples, config))
    }

    /// Return an new OntologyParser taking all triples in RDF-XML from the given IRI.
    pub fn from_doc_iri(
        iri: &IRI<A>,
        config: RDFParserConfiguration<A, B>,
    ) -> Result<Self, HornedError> {
        let mut cursor = crate::io::resolve_doc_iri(iri, &config.common)?;
        OntologyParser::from_bufread(&mut cursor, config)
    }

    /// Groups `triples` into `simple` (those which do not start with a BNode) and those that do.
    fn group_triples(
        triples: Vec<PosTriple<A>>,
        simple: &mut Vec<PosTriple<A>>,
        bnode: &mut HashMap<BNode<A>, VPosTriple<A>>,
    ) {
        // Next group together triples on a BNode, so we have
        // HashMap<BNodeID, Vec<[SpTerm; 3]> All of which should be
        // triples should begin with the BNodeId. We should be able to
        // gather these in a single pass.
        for t in triples {
            match t.triple() {
                // These triples define axioms and are pattern matched
                // along with the simple triples. This makes much of
                // my documentation slightly wrong.
                // A chain is stated of an inverse's node as of a named property.
                [_, Term::OWL(VOWL::DisjointWith), _]
                | [_, Term::OWL(VOWL::EquivalentClass), _]
                | [_, Term::OWL(VOWL::InverseOf), _]
                | [_, Term::OWL(VOWL::PropertyChainAxiom), _]
                | [_, Term::RDFS(VRDFS::SubClassOf), _] => {
                    simple.push(t);
                }
                [Term::BNode(id), _, _] => {
                    // Are there any triples on this bnode already
                    let v = bnode
                        .entry(id.clone())
                        // if there are not store the location of this as it is the first
                        .or_insert_with(|| VPosTriple(vec![], t.1));
                    v.push(t.as_triple())
                }
                _ => {
                    simple.push(t);
                }
            }
        }
    }

    /// Find and group all triples on a sequence.
    /// Find and group all triples on a sequence (RDF list).
    ///
    /// Each list cell is a bnode `c` with `c rdf:first val; c rdf:rest next`
    /// (`next` another bnode or `rdf:nil`). The previous implementation grew
    /// each list one element per full re-scan of *every* bnode and recursed
    /// until no list grew — O(list-length × bnode-count), ~97s on phenio. This
    /// instead indexes the cells once and walks each list head-to-tail following
    /// the `rest` pointers directly: O(total list cells). Output is identical:
    /// `bnode_seq[head]` is the list's values in order, and incomplete (non
    /// nil-terminated) or non-list bnodes are left untouched in `self.bnode`.
    fn stitch_seqs(&mut self) {
        use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
        // Pull out the list cells, keeping non-list bnodes in place. For each
        // cell record (first value, rest target) and remember which bnodes are
        // pointed at by some `rest` (so the remainder are list heads).
        let mut cells: HashMap<BNode<A>, (Term<A>, Option<BNode<A>>, VPosTriple<A>)> = HashMap::default();
        let mut pointed: HashSet<BNode<A>> = HashSet::default();
        for (k, v) in std::mem::take(&mut self.bnode) {
            let parsed: Option<(Term<A>, Option<BNode<A>>)> = match v.as_slice() {
                [
                    [_, Term::RDF(VRDF::First), val],
                    [_, Term::RDF(VRDF::Rest), Term::Iri(iri)],
                    ..,
                ] if **iri == **VRDF::Nil => Some((val.clone(), None)),
                [
                    [_, Term::RDF(VRDF::First), val],
                    [_, Term::RDF(VRDF::Rest), Term::BNode(id)],
                    ..,
                ] => Some((val.clone(), Some(id.clone()))),
                _ => None,
            };
            match parsed {
                Some((val, rest)) => {
                    if let Some(ref id) = rest {
                        pointed.insert(id.clone());
                    }
                    cells.insert(k, (val, rest, v));
                }
                None => {
                    self.bnode.insert(k, v);
                }
            }
        }

        // Walk each head (a cell not pointed at by any `rest`) to its tail,
        // collecting values in order. Only nil-terminated chains become seqs.
        let heads: Vec<BNode<A>> = cells.keys().filter(|k| !pointed.contains(*k)).cloned().collect();
        let mut consumed: HashSet<BNode<A>> = HashSet::default();
        for head in heads {
            let mut chain: Vec<BNode<A>> = Vec::new();
            let mut vals: Vec<Term<A>> = Vec::new();
            let mut cur = head.clone();
            let mut terminated = false;
            loop {
                match cells.get(&cur) {
                    Some((val, rest, _)) if !consumed.contains(&cur) && !chain.contains(&cur) => {
                        chain.push(cur.clone());
                        vals.push(val.clone());
                        match rest {
                            None => {
                                terminated = true;
                                break;
                            }
                            Some(next) => cur = next.clone(),
                        }
                    }
                    _ => break, // dangling / cyclic / already consumed
                }
            }
            if terminated {
                for b in &chain {
                    consumed.insert(b.clone());
                }
                self.bnode_seq.insert(head, vals);
            }
        }

        // Incomplete-list cells (never part of a nil-terminated chain) go back.
        for (k, (_, _, v)) in cells {
            if !consumed.contains(&k) {
                self.bnode.insert(k, v);
            }
        }
    }

    /// Find the ontology's header nodes among the document's statements,
    /// `triple`, in order, and the one the ontology is named by: the only one;
    /// else the first the document states, unless a header names it as the
    /// value of an annotation and it imports nothing; else the first that no
    /// header names so.
    fn find_ontology_nodes(&mut self, triple: &[PosTriple<A>]) {
        let mut stated: Vec<&Term<A>> = vec![];
        let mut importers: HashSet<&Term<A>> = HashSet::default();
        for t in triple {
            match t.triple() {
                [s, Term::RDF(VRDF::Type), Term::OWL(VOWL::Ontology)] => stated.push(s),
                [s, Term::OWL(VOWL::Imports), o] => {
                    importers.insert(s);
                    stated.push(s);
                    stated.push(o);
                }
                _ => {}
            }
        }
        self.ontology_nodes = stated.iter().map(|t| (*t).clone()).collect();
        let named: HashSet<&Term<A>> = triple
            .iter()
            .filter_map(|t| match t.triple() {
                [s, p, o @ Term::Iri(_)]
                    if self.ontology_nodes.contains(s)
                        && self.ontology_nodes.contains(o)
                        && !importers.contains(o)
                        && !matches!(p, Term::RDF(_) | Term::OWL(VOWL::Imports) | Term::OWL(VOWL::VersionIRI)) =>
                {
                    Some(o)
                }
                _ => None,
            })
            .collect();
        self.ontology_node = match stated.first() {
            Some(first) if self.ontology_nodes.len() == 1 || !named.contains(first) => Some((*first).clone()),
            _ => stated.into_iter().find(|t| !named.contains(t)).cloned(),
        };
    }

    /// Process all import statements: every `owl:imports`, whatever node it
    /// is stated of.
    fn resolve_imports(&mut self) -> Vec<IRI<A>> {
        let mut v = vec![];
        for t in std::mem::take(&mut self.simple) {
            match t.0 {
                [Term::Iri(_), Term::OWL(VOWL::Imports), Term::Iri(imp)] => v.push(imp.clone()),
                _ => self.simple.push(t),
            }
        }
        let mut groups: Vec<&BNode<A>> = self.bnode.keys().collect();
        groups.sort_by_key(|k| self.bnode_order.get(&k.0).copied().unwrap_or(usize::MAX));
        let groups: Vec<BNode<A>> = groups.into_iter().cloned().collect();
        for k in groups {
            if let Some(group) = self.bnode.get_mut(&k) {
                group.retain(|t| match t {
                    [_, Term::OWL(VOWL::Imports), Term::Iri(imp)] => {
                        v.push(imp.clone());
                        false
                    }
                    _ => true,
                });
            }
        }
        for imp in v.clone() {
            self.merge(AnnotatedComponent { component: Import(imp).into(), ann: BTreeSet::new() });
        }

        v
        // Section 3.1.2/table 4 of RDF Graphs
    }

    /// Process the header statements. Every `owl:versionIRI` is consumed, and
    /// the one stated of the node the ontology is named by gives its version.
    /// A blank node names no ontology: the ontology is then anonymous.
    fn headers(&mut self) {
        //Section 3.1.2/table 4
        //   *:x rdf:type owl:Ontology .
        //[ *:x owl:versionIRI *:y .]
        let mut versions: HashMap<Term<A>, IRI<A>> = HashMap::default();
        for t in std::mem::take(&mut self.simple) {
            match t.triple() {
                [Term::Iri(_), Term::RDF(VRDF::Type), Term::OWL(VOWL::Ontology)] => {}
                [s, Term::OWL(VOWL::VersionIRI), Term::Iri(ob)] => {
                    versions.insert(s.clone(), ob.clone());
                }
                _ => self.simple.push(t),
            }
        }
        for group in self.bnode.values_mut() {
            group.retain(|t| match t {
                [s, Term::OWL(VOWL::VersionIRI), Term::Iri(ob)] => {
                    versions.insert(s.clone(), ob.clone());
                    false
                }
                _ => true,
            });
        }
        let iri = match &self.ontology_node {
            Some(Term::Iri(iri)) => Some(iri.clone()),
            _ => None,
        };
        let viri = iri.as_ref().and_then(|i| versions.get(&Term::Iri(i.clone())).cloned());
        self.o.insert(OntologyID { iri, viri });
    }

    /// Table 5 and Table 6 (OWL 2 Mapping to RDF Graphs S3.1.2),
    /// backward compatibility with OWL 1 DL, applied in the order the
    /// spec prescribes -- Table 5 first, then Table 6.
    fn backward_compat(&mut self) {
        const OWL_DATA_RANGE: &str = "http://www.w3.org/2002/07/owl#DataRange";
        let is_data_range = |t: &Term<A>| matches!(t, Term::Iri(i) if i.as_ref() == OWL_DATA_RANGE);

        // The types each subject carries, named and blank-node subjects alike,
        // and the blank nodes that are cells of an RDF list.
        let mut types: HashMap<Term<A>, Vec<Term<A>>> = HashMap::default();
        let mut list_cells: HashSet<Term<A>> = HashSet::default();
        let triples = self
            .simple
            .iter()
            .map(|t| t.triple())
            .chain(self.bnode.values().flat_map(|v| v.iter()));
        for t in triples {
            match t {
                [s, Term::RDF(VRDF::Type), o] => types.entry(s.clone()).or_default().push(o.clone()),
                [s, Term::RDF(VRDF::First), _] => {
                    list_cells.insert(s.clone());
                }
                _ => {}
            }
        }

        // Table 5: the type triples a more specific type makes redundant.
        // Otherwise they survive into declaration processing and produce a
        // spurious ClassAssertion, or break the pattern of the expression
        // they type.
        let mut remove: HashSet<[Term<A>; 3]> = HashSet::default();
        // Table 6: the declarations OWL 1 left implicit.
        let mut add: Vec<[Term<A>; 3]> = vec![];
        for (s, ts) in &types {
            let has = |f: &dyn Fn(&Term<A>) -> bool| ts.iter().any(f);
            let typed = |t: Term<A>| [s.clone(), Term::RDF(VRDF::Type), t];
            if has(&|t| {
                matches!(
                    t,
                    Term::OWL(VOWL::Class | VOWL::Restriction) | Term::RDFS(VRDFS::Datatype)
                ) || is_data_range(t)
            }) {
                remove.insert(typed(Term::RDFS(VRDFS::Class)));
            }
            if has(&|t| matches!(t, Term::OWL(VOWL::Restriction))) {
                remove.insert(typed(Term::OWL(VOWL::Class)));
            }
            if has(&|t| {
                matches!(
                    t,
                    Term::OWL(
                        VOWL::ObjectProperty
                            | VOWL::DatatypeProperty
                            | VOWL::AnnotationProperty
                            | VOWL::OntologyProperty
                            | VOWL::FunctionalProperty
                            | VOWL::InverseFunctionalProperty
                            | VOWL::TransitiveProperty
                    )
                )
            }) {
                remove.insert(typed(Term::RDF(VRDF::Property)));
            }
            if list_cells.contains(s) {
                remove.insert(typed(Term::RDF(VRDF::List)));
            }
            // Table 6 declares named properties: a blank node carrying one of
            // these types is a property expression, not an entity.
            let named = matches!(s, Term::Iri(_));
            // owl:InverseFunctionalProperty, owl:TransitiveProperty and
            // owl:SymmetricProperty each imply owl:ObjectProperty.
            if named
                && has(&|t| {
                    matches!(
                        t,
                        Term::OWL(
                            VOWL::InverseFunctionalProperty
                                | VOWL::TransitiveProperty
                                | VOWL::SymmetricProperty
                        )
                    )
                })
                && !has(&|t| matches!(t, Term::OWL(VOWL::ObjectProperty)))
            {
                add.push(typed(Term::OWL(VOWL::ObjectProperty)));
            }
            // owl:OntologyProperty is read as owl:AnnotationProperty.
            if named && has(&|t| matches!(t, Term::OWL(VOWL::OntologyProperty))) {
                remove.insert(typed(Term::OWL(VOWL::OntologyProperty)));
                if !has(&|t| matches!(t, Term::OWL(VOWL::AnnotationProperty))) {
                    add.push(typed(Term::OWL(VOWL::AnnotationProperty)));
                }
            }
        }
        if !remove.is_empty() {
            self.simple.retain(|t| !remove.contains(t.triple()));
            for v in self.bnode.values_mut() {
                v.retain(|t| !remove.contains(t));
            }
            self.bnode.retain(|_, v| !v.is_empty());
        }
        self.simple.extend(add.into_iter().map(PosTriple::from));

        // In lax mode, as OWLAPI reads it, a blank node that describes an
        // expression without saying what it is takes the type its predicates
        // imply: one with `owl:onProperty` is a restriction; one with
        // `owl:onDatatype`, `owl:withRestrictions` or `owl:datatypeComplementOf`
        // a data range; and one with `owl:unionOf`, `owl:intersectionOf`,
        // `owl:complementOf` or `owl:oneOf` a class. A node typed in the OWL
        // vocabulary or as a data range says what it is already.
        if self.config.lax {
            for v in self.bnode.values_mut() {
                let typed = v.iter().any(|t| match t {
                    [_, Term::RDF(VRDF::Type), Term::OWL(_) | Term::RDFS(VRDFS::Datatype)] => true,
                    [_, Term::RDF(VRDF::Type), Term::Iri(i)] => i.as_ref().starts_with(crate::vocab::Namespace::OWL.as_ref()),
                    _ => false,
                });
                if typed {
                    continue;
                }
                let has = |ps: &[VOWL]| v.iter().any(|t| matches!(&t[1], Term::OWL(p) if ps.contains(p)));
                let implied = if has(&[VOWL::OnProperty]) {
                    Term::OWL(VOWL::Restriction)
                } else if has(&[VOWL::OnDatatype, VOWL::WithRestrictions, VOWL::DatatypeComplementOf]) {
                    Term::RDFS(VRDFS::Datatype)
                } else if has(&[VOWL::UnionOf, VOWL::IntersectionOf, VOWL::ComplementOf, VOWL::OneOf]) {
                    Term::OWL(VOWL::Class)
                } else {
                    continue;
                };
                let subject = v[0][0].clone();
                v.push([subject, Term::RDF(VRDF::Type), implied]);
                v.sort();
            }
        }
    }

    fn parse_annotations(
        &self,
        triples: &[[Term<A>; 3]],
    ) -> Result<BTreeSet<Annotation<A>>, HornedError> {
        let mut ann = BTreeSet::default();
        for a in triples {
            ann.insert(self.annotation(a)?);
        }
        Ok(ann)
    }

    // Process annotations
    fn annotation(&self, t: &[Term<A>; 3]) -> Result<Annotation<A>, HornedError> {
        match t {
            // We assume that anything passed to here is an
            // annotation built in type
            [s, RDFS(rdfs), b] => {
                let iri = self.config.build.as_ref().iri(rdfs.as_ref());
                self.annotation(&[s.clone(), Term::Iri(iri), b.clone()])
            }
            [s, OWL(owl), b] => {
                let iri = self.config.build.as_ref().iri(owl.as_ref());
                self.annotation(&[s.clone(), Term::Iri(iri), b.clone()])
            }
            [_, Iri(p), ob @ Term::Literal(_)] => Ok(Annotation {
                ap: AnnotationProperty(p.clone()),
                av: self.convert_to_literal(ob).unwrap().into(),
                ann: Default::default(),
            }),
            [_, Iri(p), Iri(ob)] => {
                // IRI annotation value
                Ok(Annotation {
                    ap: AnnotationProperty(p.clone()),
                    av: ob.clone().into(),
                    ann: Default::default(),
                })
            }
            [_, Iri(p), Term::BNode(bn)] => Ok(Annotation {
                ap: AnnotationProperty(p.clone()),
                av: self.anon_for_bnode(bn).into(),
                ann: Default::default(),
            }),
            all => Err(HornedError::invalid(format!(
                "Invalid annotation found {:?}",
                all
            ))),
        }
    }

    fn merge<IAA: Into<AnnotatedComponent<A>>>(&mut self, cmp: IAA) {
        let cmp = cmp.into();
        update_or_insert_logically_equal_component(&mut self.o, cmp);
    }

    /// Insert an annotated component directly, WITHOUT merging it onto a
    /// logically-equal axiom. Used where each component is a distinct intended
    /// axiom — e.g. several `owl:Axiom` blocks reify the same base triple with
    /// different annotation sets (NCIT-style multi-source synonyms). Merging
    /// would union those annotation sets and collapse them into one axiom.
    fn insert_distinct<IAA: Into<AnnotatedComponent<A>>>(&mut self, cmp: IAA) {
        self.o.insert(cmp.into());
    }

    /// Process axiom annotations.
    fn axiom_annotations(&mut self) -> Result<(), HornedError> {
        // The owl:Annotation nodes first. Each annotates one annotation of the
        // source it names, and reading that source's annotations looks them up.
        for (k, v) in self.take_bnode_groups() {
            match self.annotation_node(v) {
                Ok((source, v)) => self.annotation_nodes.entry(source).or_default().push((k, v)),
                Err(v) => {
                    self.bnode.insert(k, v);
                }
            }
        }

        // Every base triple a reification names, with the position of the block
        // that named it, so one the document leaves unstated can be restored.
        let mut reified: Vec<([Term<A>; 3], u64)> = Vec::new();

        for (k, v) in self.take_bnode_groups() {
            let pos = v.1;
            match v.as_slice() {
                [
                    [_, Term::OWL(VOWL::AnnotatedProperty), p], //:
                    [_, Term::OWL(VOWL::AnnotatedSource), sb],  //:
                    [_, Term::OWL(VOWL::AnnotatedTarget), ob],  //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Axiom)],
                    ann @ ..,
                ] => {
                    // The original axiom that this annotation sits on will
                    // have its IRIs converted to OWL/RDF vocab, so we must do
                    // this here or they will not match the key of the
                    // annotation. Push (don't overwrite): several owl:Axiom
                    // blocks may reify the same base triple with distinct
                    // annotation sets, each a separate annotated axiom.
                    let mut key = self.config.build.as_ref().substitute_term([sb.clone(), p.clone(), ob.clone()]);
                    // The reification of an axiom stated with a list — a
                    // property chain, a disjoint union, a key — names as its
                    // `annotatedTarget` a list of its own, equal in content to
                    // the axiom's (both written as `parseType="Collection"`).
                    // Its annotations are keyed by the list's content, so they
                    // match the axiom whichever node carries the list.
                    if matches!(
                        key[1],
                        Term::OWL(VOWL::PropertyChainAxiom | VOWL::DisjointUnionOf | VOWL::HasKey)
                    ) && let Term::BNode(ref b) = key[2]
                        && let Some(members) = self.bnode_seq.get(b)
                    {
                        self.list_copies.push(b.clone());
                        key[2] = self.canon_list_term(members);
                    }
                    reified.push((key.clone(), pos));
                    let anns = self.annotations_of(&Term::BNode(k.clone()), ann)?;
                    self.ann_map.entry(key).or_default().push(anns);
                }

                _ => {
                    self.bnode.insert(k, v);
                }
            }
        }

        self.restore_reified_triples(reified);
        Ok(())
    }

    /// An `owl:Annotation` node's source, with the node's statements naming
    /// that source alone; or the statements back, when the node is not one.
    ///
    /// A node can name several sources: an annotation on an axiom written as
    /// pairwise statements is reified once per statement, and the annotation's
    /// own annotations are written once, naming every reification. Such a node
    /// annotates the reification whose annotations are read first, in the
    /// order `read_statements` was given, and no other.
    fn annotation_node(&self, mut v: VPosTriple<A>) -> Result<(Term<A>, VPosTriple<A>), VPosTriple<A>> {
        let sources: Vec<Term<A>> = v
            .iter()
            .filter_map(|t| match t {
                [_, Term::OWL(VOWL::AnnotatedSource), s @ (Term::BNode(_) | Term::Iri(_))] => Some(s.clone()),
                _ => None,
            })
            .collect();
        if sources.len() > 1 {
            let rank = |s: &Term<A>| match s {
                Term::BNode(b) => self.axiom_rank.get(&b.0).copied(),
                _ => None,
            };
            let Some(first) = sources.iter().filter(|s| rank(s).is_some()).min_by_key(|s| rank(s)).cloned() else {
                return Err(v);
            };
            v.retain(|t| !matches!(t, [_, Term::OWL(VOWL::AnnotatedSource), s] if *s != first));
        }
        match v.as_slice() {
            [
                [_, Term::OWL(VOWL::AnnotatedProperty), _],
                [_, Term::OWL(VOWL::AnnotatedSource), source @ (Term::BNode(_) | Term::Iri(_))],
                [_, Term::OWL(VOWL::AnnotatedTarget), _],
                [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Annotation)],
                ..,
            ] => {
                let source = source.clone();
                Ok((source, v))
            }
            _ => Err(v),
        }
    }

    /// The annotations `triples` state of `source`, each carrying the
    /// annotations stated of it in turn, to any depth.
    ///
    /// The annotations of an annotation are an `owl:Annotation` node naming
    /// the annotation's subject as its `owl:annotatedSource` — an `owl:Axiom`
    /// block, a node an axiom is stated on, another `owl:Annotation` node, or
    /// the ontology — and the annotation's property and value as its
    /// `owl:annotatedProperty` and `owl:annotatedTarget`. A node that matches
    /// none of the source's annotations is left unread.
    fn annotations_of(
        &mut self,
        source: &Term<A>,
        triples: &[[Term<A>; 3]],
    ) -> Result<BTreeSet<Annotation<A>>, HornedError> {
        let anns = self.parse_annotations(triples)?;
        self.annotate_annotations(source, anns)
    }

    /// `anns`, the annotations `source` states, each carrying the annotations
    /// the `owl:Annotation` nodes naming `source` state of it.
    fn annotate_annotations(
        &mut self,
        source: &Term<A>,
        anns: BTreeSet<Annotation<A>>,
    ) -> Result<BTreeSet<Annotation<A>>, HornedError> {
        let Some(mut nodes) = self.annotation_nodes.remove(source) else {
            return Ok(anns);
        };
        nodes.sort_by_key(|(node, _)| self.annotation_rank.get(&node.0).copied().unwrap_or(usize::MAX));
        let mut anns: Vec<Annotation<A>> = anns.into_iter().collect();
        let mut claimed = vec![false; anns.len()];
        let mut unread = vec![];
        for (node, v) in nodes {
            let [
                [_, Term::OWL(VOWL::AnnotatedProperty), p],
                [_, Term::OWL(VOWL::AnnotatedSource), _],
                [_, Term::OWL(VOWL::AnnotatedTarget), ob],
                [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Annotation)],
                nested @ ..,
            ] = v.as_slice()
            else {
                unread.push((node, v));
                continue;
            };
            let annotated = self.annotation(&[Term::BNode(node.clone()), p.clone(), ob.clone()])?;
            match anns.iter().position(|a| a.ap == annotated.ap && a.av == annotated.av) {
                Some(i) if !claimed[i] => {
                    claimed[i] = true;
                    let nested = self.annotations_of(&Term::BNode(node), nested)?;
                    anns[i].ann.extend(nested);
                }
                _ => unread.push((node, v)),
            }
        }
        if !unread.is_empty() {
            self.annotation_nodes.insert(source.clone(), unread);
        }
        Ok(anns.into_iter().collect())
    }

    /// Put back the base triple of a reification the document does not state.
    ///
    /// An `owl:Axiom` block names the axiom it annotates by subject, predicate
    /// and object. A document that carries the block without stating that triple
    /// still means the axiom, so it is restored here and the ordinary translation
    /// builds it, carrying the block's annotations.
    ///
    /// An SSSOM mapping set in RDF is written exactly this way — every mapping is
    /// an `owl:Axiom` block and no base triple is stated — and uPheno's
    /// `components/upheno-mappings.owl` is a SPARQL update over those base
    /// triples. Without this its 51,582 mappings reach the update as nothing but
    /// anonymous individuals and the component comes out empty.
    ///
    /// A triple about an anonymous individual is restored too: one whose
    /// subject is a blank node describing nothing but an individual joins that
    /// node's statements, and one whose object is a blank node that states
    /// nothing of its own names the individual that node is. Any other
    /// blank-node subject or object belongs to a construct held elsewhere — a
    /// class expression, an RDF list — which the translation reaches by its own
    /// route.
    ///
    /// The exception is a reification whose `owl:annotatedTarget` is a node of
    /// its own describing a class expression, where the axiom's triple names
    /// another node describing the same one: an `owl:Axiom` block that spells
    /// the axiom's class expression out again in place of naming its node. Its
    /// triple is restored with that node as the object, so the axiom is read
    /// from it with the block's annotations; and the unannotated axiom the
    /// axiom's own triple states goes once the parse is done, so the axiom is
    /// left annotated.
    ///
    /// A literal typed `xsd:string` and the untyped literal of its text are
    /// one literal, so a block naming a stated triple with the literal typed
    /// the other way names that triple, and the axiom takes the block's
    /// literal.
    fn restore_reified_triples(&mut self, reified: Vec<([Term<A>; 3], u64)>) {
        if reified.is_empty() {
            return;
        }
        let mut stated: rustc_hash::FxHashSet<[Term<A>; 3]> =
            self.simple.iter().map(|t| t.triple().clone()).collect();
        let mut add: Vec<PosTriple<A>> = Vec::new();
        let mut retyped: HashMap<[Term<A>; 3], Term<A>> = HashMap::default();
        for (key, pos) in reified {
            // A blank-node subject that describes an individual takes the
            // statement among its own; one that describes a construct, a class
            // expression, is the axiom's own node, and what it names is taken
            // as for a named subject.
            if let Term::BNode(subject) = &key[0] {
                let individual =
                    self.bnode.get(subject).is_none_or(|group| group.0.iter().all(Self::individual_statement));
                if individual || !matches!(&key[2], Term::BNode(_)) {
                    self.restore_individual_statement(subject.clone(), key, pos);
                    continue;
                }
            }
            if let Term::BNode(target) = &key[2]
                && !self.bnode.contains_key(target)
                && !self.bnode_seq.contains_key(target)
                && Self::individual_object(&key)
            {
                if stated.insert(key.clone()) {
                    add.push(PosTriple(key, pos));
                }
                continue;
            }
            if let Term::BNode(target) = &key[2] {
                let construct = self.bnode.contains_key(target) && !self.bnode_seq.contains_key(target);
                if construct && stated.insert(key.clone()) {
                    self.copied_targets.insert(key.clone());
                    add.push(PosTriple(key, pos));
                }
                continue;
            }
            if !stated.contains(&key)
                && let Some(other) = Self::other_string_typing(self.config.build.as_ref(), &key)
                && stated.remove(&other)
            {
                retyped.insert(other, key[2].clone());
                stated.insert(key);
                continue;
            }
            if stated.insert(key.clone()) {
                add.push(PosTriple(key, pos));
            }
        }
        if !retyped.is_empty() {
            for t in &mut self.simple {
                if let Some(o) = retyped.get(&t.0) {
                    t.0[2] = o.clone();
                }
            }
        }
        if add.is_empty() {
            return;
        }
        // A restored triple takes the position of the block that named it, and is
        // merged in at that point. The existing entries keep the order they are
        // in — the rest of the parse reads them in document order — so this is a
        // merge into that sequence, not a sort of it.
        add.sort_by_key(|t| t.position());
        let old = std::mem::take(&mut self.simple);
        let mut it = add.into_iter().peekable();
        for t in old {
            while it.peek().is_some_and(|n| n.position() <= t.position()) {
                self.simple.push(it.next().expect("peeked"));
            }
            self.simple.push(t);
        }
        self.simple.extend(it);
    }

    /// Put back `triple`, a reified statement about the blank node `subject`
    /// that the document does not state, among that node's statements, when
    /// it and they all describe an anonymous individual. A group that is
    /// started here takes the position of the block that named the triple.
    fn restore_individual_statement(&mut self, subject: BNode<A>, triple: [Term<A>; 3], pos: u64) {
        if self.bnode_seq.contains_key(&subject) || !Self::individual_statement(&triple) {
            return;
        }
        let other = Self::other_string_typing(self.config.build.as_ref(), &triple);
        let group = self.bnode.entry(subject).or_insert_with(|| VPosTriple(vec![], pos));
        if group.0.contains(&triple) || !group.0.iter().all(Self::individual_statement) {
            return;
        }
        // The statement with the literal typed the other way is this one, and
        // takes the block's literal.
        if let Some(i) = other.and_then(|other| group.0.iter().position(|t| *t == other)) {
            group.0[i] = triple;
        } else {
            group.0.push(triple);
        }
        group.0.sort();
    }

    /// `t` with its literal object typed `xsd:string` when it is untyped, and
    /// untyped when it is typed so: the same literal, as a statement typing it
    /// the other way states it.
    fn other_string_typing(b: &Build<A>, t: &[Term<A>; 3]) -> Option<[Term<A>; 3]> {
        const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";
        let o = match &t[2] {
            Term::Literal(Literal::Simple { literal }) => {
                Literal::Datatype { literal: literal.clone(), datatype_iri: b.iri(XSD_STRING) }
            }
            Term::Literal(Literal::Datatype { literal, datatype_iri }) if datatype_iri.as_ref() == XSD_STRING => {
                Literal::Simple { literal: literal.clone() }
            }
            _ => return None,
        };
        Some([t[0].clone(), t[1].clone(), Term::Literal(o)])
    }

    /// Whether the object of `t` is an individual: what `owl:sameAs` and
    /// `owl:differentFrom` relate, or the value of a property or an annotation.
    fn individual_object(t: &[Term<A>; 3]) -> bool {
        match t {
            [_, Term::OWL(VOWL::SameAs | VOWL::DifferentFrom), _] => true,
            [_, Term::RDFS(rdfs), _] => rdfs.is_builtin(),
            [_, Term::Iri(pred), _] => !is_reserved_iri(pred),
            _ => false,
        }
    }

    /// Whether `t` says something of an anonymous individual: types it, makes
    /// it the same as or different from an individual, or relates it by a
    /// property or an annotation.
    fn individual_statement(t: &[Term<A>; 3]) -> bool {
        match t {
            [_, Term::RDF(VRDF::Type), Term::Iri(cls)] => !is_reserved_iri(cls),
            [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::NamedIndividual | VOWL::Thing | VOWL::Nothing)] => true,
            [_, Term::RDF(VRDF::Type), Term::BNode(_)] => true,
            [_, Term::OWL(VOWL::SameAs | VOWL::DifferentFrom), _] => true,
            [_, Term::RDFS(rdfs), _] => rdfs.is_builtin(),
            [_, Term::Iri(pred), _] => !is_reserved_iri(pred),
            _ => false,
        }
    }

    /// The annotations recorded under the content of the list `id`, the
    /// object of `[subject, pred, id]`, moved onto that triple's key, where the
    /// axiom it states takes them; the reifications' copies of the list are
    /// read with them. A list the subject states again with the same members
    /// states the same axiom, and takes the same annotations.
    fn claim_list_annotations(&mut self, subject: &Term<A>, pred: VOWL, id: &BNode<A>) {
        let Some(members) = self.bnode_seq.get(id) else { return };
        let canon = self.canon_list_term(members);
        let key = [subject.clone(), Term::OWL(pred.clone()), canon.clone()];
        let anns = match self.ann_map.remove(&key) {
            Some(anns) => {
                self.claimed_lists.insert(key, anns.clone());
                anns
            }
            None => match self.claimed_lists.get(&key) {
                Some(anns) => anns.clone(),
                None => return,
            },
        };
        self.ann_map.entry([subject.clone(), Term::OWL(pred), Term::BNode(id.clone())]).or_default().extend(anns);
        let copies = std::mem::take(&mut self.list_copies);
        for copy in copies {
            let same = &copy != id && self.bnode_seq.get(&copy).is_some_and(|m| self.canon_list_term(m) == canon);
            if same {
                self.bnode_seq.remove(&copy);
            } else {
                self.list_copies.push(copy);
            }
        }
    }

    /// A content-based key term for an RDF list: the members of a property
    /// chain, a disjoint union or a key. Used so a reification whose
    /// `annotatedTarget` is a distinct but structurally-equal Collection bnode
    /// still matches the axiom built from a different list bnode of the same
    /// content. An inverse member is keyed by the property it names, since each
    /// list states it with a node of its own.
    fn canon_list_term(&self, members: &[Term<A>]) -> Term<A> {
        let s: String = members
            .iter()
            .map(|t| match t {
                Term::BNode(b) if self.inverse_nodes.contains_key(b) => {
                    format!("inverse {:?}", self.inverse_nodes[b])
                }
                _ => format!("{t:?}"),
            })
            .collect::<Vec<_>>()
            .join("\u{1}");
        Term::BNode(BNode(format!("__list__\u{1}{s}").into()))
    }

    /// Take the reified annotation sets recorded for a base triple. Returns one
    /// empty set when there were none (so the bare, unannotated axiom is still
    /// emitted); otherwise one set per reifying `owl:Axiom` block, so each
    /// distinct annotated axiom is recovered rather than silently dropped.
    fn take_anns(&mut self, t: &[Term<A>; 3]) -> Vec<BTreeSet<Annotation<A>>> {
        match self.ann_map.remove(t) {
            Some(v) if !v.is_empty() => v,
            _ => vec![BTreeSet::new()],
        }
    }

    /// Process named entity declaration axioms
    fn declarations(&mut self) {
        // Table 7
        for t in std::mem::take(&mut self.simple) {
            let entity = match t.triple() {
                [Term::Iri(s), Term::RDF(VRDF::Type), entity] => match entity {
                    Term::OWL(VOWL::Class) => Some(Class(s.clone()).into()),
                    Term::OWL(VOWL::ObjectProperty) => Some(ObjectProperty(s.clone()).into()),
                    Term::OWL(VOWL::AnnotationProperty) => {
                        Some(AnnotationProperty(s.clone()).into())
                    }
                    Term::OWL(VOWL::DatatypeProperty) => Some(DataProperty(s.clone()).into()),
                    Term::OWL(VOWL::NamedIndividual) => Some(NamedIndividual(s.clone()).into()),
                    Term::RDFS(VRDFS::Datatype) => Some(Datatype(s.clone()).into()),
                    _ => None,
                },
                _ => None,
            };

            if let Some(entity) = entity {
                let ne: NamedOWLEntity<_> = entity;
                // Each reifying owl:Axiom block over this base triple is a
                // distinct annotated axiom; insert each directly rather than
                // merging (which would union their annotation sets).
                for ann in self.take_anns(t.triple()) {
                    self.insert_distinct(AnnotatedComponent {
                        component: ne.clone().into(),
                        ann,
                    });
                }
            } else {
                self.simple.push(t);
            }
        }
    }

    /// Process data ranges
    fn data_ranges(&mut self) -> Result<(), HornedError> {
        let data_range_len = self.data_range.len();
        let mut facet_map: HashMap<Term<A>, PosTriple<A>> = HashMap::default();

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
                // A connective's list may be empty, `rdf:nil`.
                [
                    [_, Term::OWL(VOWL::IntersectionOf), list @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_))], //: rustfmt hard line!
                    [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)],
                ] => {
                    ok_some! {
                        DataRange::DataIntersectionOf(
                            self.retrieve_to_list(list, Self::retrieve_to_dr_seq)?
                        )
                    }
                }
                [
                    [_, Term::OWL(VOWL::UnionOf), list @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_))], //: rustfmt hard line!
                    [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)],
                ] => {
                    ok_some! {
                        DataRange::DataUnionOf(
                            self.retrieve_to_list(list, Self::retrieve_to_dr_seq)?
                        )
                    }
                }
                [
                    [_, Term::OWL(VOWL::DatatypeComplementOf), term], //:
                    [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)],
                ] => {
                    ok_some! {
                      DataRange::DataComplementOf(
                            Box::new(self.retrieve_to_dr(term)?)
                        )
                    }
                }
                [
                    [_, Term::OWL(VOWL::OneOf), list @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_))], //:
                    [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)],
                ] => {
                    ok_some! {
                        DataRange::DataOneOf(
                            self.retrieve_to_list(list, Self::retrieve_to_literal_seq)?
                        )
                    }
                }
                // Table 14: an OWL 1 DL enumerated data range. An empty one
                // is the complement of rdfs:Literal.
                [
                    [_, Term::OWL(VOWL::OneOf), list @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_))], //:
                    [_, Term::RDF(VRDF::Type), Term::Iri(data_range)],
                ] if data_range.as_ref() == "http://www.w3.org/2002/07/owl#DataRange" => {
                    ok_some! {{
                        let literals = self.retrieve_to_list(list, Self::retrieve_to_literal_seq)?;
                        if literals.is_empty() {
                            DataRange::DataComplementOf(Box::new(
                                self.config.build.as_ref().datatype(OWL2Datatype::Literal).into(),
                            ))
                        } else {
                            DataRange::DataOneOf(literals)
                        }
                    }}
                }
                [
                    [_, Term::OWL(VOWL::OnDatatype), Term::Iri(iri)], //:
                    [_, Term::OWL(VOWL::WithRestrictions), Term::BNode(id)], //:
                    [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)],
                ] => {
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
                                                                      l: self.convert_to_literal(&literal)?,
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

            match dr? {
                Some(dr) => {
                    self.data_range.insert(this_bnode, dr);
                }
                _ => {
                    self.bnode.insert(this_bnode, v);
                }
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

    /// Process ObjectPropertyExpression
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

    /// Look up a bnode in one of the resolved-object maps
    /// (`class_expression`, `object_property_expression`, `data_range`)
    /// without removing it, recording it in `used_bnode` on success so
    /// that a genuinely unreferenced entry can still be told apart from
    /// one that has been (possibly repeatedly) retrieved.
    fn get_used<V: Clone>(
        map: &HashMap<BNode<A>, V>,
        used_bnode: &mut HashSet<BNode<A>>,
        id: &BNode<A>,
    ) -> Option<V> {
        let v = map.get(id).cloned();
        if v.is_some() {
            used_bnode.insert(id.clone());
        }
        v
    }

    /// Take out the blank-node groups still to read, in the order the
    /// document first mentions their nodes.
    ///
    /// A pass that can name an anonymous individual visits the groups this
    /// way: the individuals take their ids in the order they are first named,
    /// and a blank node's label is whatever the RDF parser made up for it, so
    /// the order the groups are stored in says nothing about the document.
    fn take_bnode_groups(&mut self) -> Vec<(BNode<A>, VPosTriple<A>)> {
        let order = &self.bnode_order;
        let mut groups: Vec<(usize, BNode<A>, VPosTriple<A>)> = std::mem::take(&mut self.bnode)
            .into_iter()
            .map(|(k, v)| (order.get(&k.0).copied().unwrap_or(usize::MAX), k, v))
            .collect();
        groups.sort_unstable_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        groups.into_iter().map(|(_, k, v)| (k, v)).collect()
    }

    // The following are a set of methods which move between RDF types
    // and OWL types. We use a standard naming scheme, with "convert"
    // where the change is stateless (except for `Build` caching),
    // "retrieve" where it involves changing other data structures by
    // side effect.

    /// Given a Term return an IRI if it can be converted to it
    fn convert_to_iri(&self, t: &Term<A>) -> Option<IRI<A>> {
        match t {
            Term::OWL(vowl) => Some(self.config.build.as_ref().iri(vowl.as_ref())),
            Term::Iri(iri) => Some(iri.clone()),
            _ => None,
        }
    }

    /// Retrieve or convert to an SubObjectPropertyExpression or None.
    fn retrieve_to_sope(&mut self, t: &Term<A>) -> Option<SubObjectPropertyExpression<A>> {
        self.retrieve_to_ope(t).map(Into::into)
    }

    /// Retrieve or convert to an ObjectPropertyExpression or None.
    ///
    /// If we have a BNode, then need to retrieve this from an early
    /// part of the parse. If we have a term that can be converted
    /// into an IRI, then form an ObjectProperty from that.
    fn retrieve_to_ope(&mut self, t: &Term<A>) -> Option<ObjectPropertyExpression<A>> {
        if let Term::BNode(id) = t {
            // If it is a BNode then extract
            Self::get_used(&self.object_property_expression, &mut self.used_bnode, id)
        } else {
            // Else convert it to an ObjectProperty
            self.convert_to_iri(t).map(Into::into)
        }
    }

    /// Convert Term to AnnotationProperty or None
    fn convert_to_ap(&mut self, t: &Term<A>) -> Option<AnnotationProperty<A>> {
        self.convert_to_iri(t).map(Into::into)
    }

    /// Convert Term to DataProperty or None
    fn convert_to_dp(&mut self, t: &Term<A>) -> Option<DataProperty<A>> {
        self.convert_to_iri(t).map(Into::into)
    }

    /// Convert a Term to a ClassExpression or retrieve it if it is a BNode
    fn retrieve_to_ce(&mut self, tce: &Term<A>) -> Option<ClassExpression<A>> {
        match tce {
            Term::BNode(id) => Self::get_used(&self.class_expression, &mut self.used_bnode, id),
            _ => self.convert_to_iri(tce).map(Into::into),
        }
    }

    /// Retrieve an RDF sequence to OWL entities or None. See also
    /// [`retrieve_to_ce_seq`] and related methods.
    fn retrieve_to_seq<E>(
        &mut self,
        bnodeid: &BNode<A>,
        f: fn(s: &mut Self, &Term<A>) -> Option<E>,
    ) -> Option<Vec<E>> {
        self.bnode_seq
            // Returns Option<Vec<Term<A>>>
            .remove(bnodeid)
            // Returns Option<&Vec<Term<A>>>
            .as_ref()?
            .iter()
            // Returns iter Option<E>.
            // We pass in self here rather than the rather clearer use
            // of self in closures in calling functions because of the
            // capture semantics of the closure.
            .map(|e| f(self, e))
            // Collections to Option<Vec<E>>
            .collect()
    }

    /// Retrieve a `Vec` of `ClassExpression`, or None.
    fn retrieve_to_ce_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<ClassExpression<A>>> {
        // For retrieve_to_ce_seq we need to check first that the all
        // the elements of the seq are going to return class
        // expression.
        //
        // The reason for this is the `class_expressions` method
        // builds up the `class_expression` hash by a recursive
        // call. The first time we need a CE seq, all of the elements
        // might not resolve. So we need not to remove the seq from
        // the bnode_seq Vec, or it will be gone the next time around
        // when we might need it
        if !self.bnode_seq.get(bnodeid)?.iter().all(|tce| match tce {
            Term::BNode(id) => self.class_expression.contains_key(id),
            _ => true,
        }) {
            return None;
        }

        self.retrieve_to_seq(bnodeid, |slf, t| slf.retrieve_to_ce(t))
    }

    /// The members of the list `t` names: `rdf:nil` is the empty list, and a
    /// blank node is the head of one stitched into `bnode_seq`.
    fn retrieve_to_list<E>(
        &mut self,
        t: &Term<A>,
        f: fn(&mut Self, &BNode<A>) -> Option<Vec<E>>,
    ) -> Option<Vec<E>> {
        match t {
            Term::BNode(id) => f(self, id),
            Term::RDF(VRDF::Nil) => Some(vec![]),
            Term::Iri(iri) if iri.as_ref() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil" => {
                Some(vec![])
            }
            _ => None,
        }
    }

    /// Retrieve a Vec of Individual or None: a member is the named
    /// individual an IRI is or the anonymous one a blank node names.
    fn retrieve_to_ni_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<Individual<A>>> {
        self.retrieve_to_seq(bnodeid, |slf, t| slf.term_to_individual(t))
    }

    /// Retrieve the members of an `owl:oneOf` class, as
    /// [`Self::retrieve_to_ni_seq`] does, except that in lax mode a literal
    /// member, which names no individual, is left out, as OWLAPI leaves it.
    fn retrieve_to_enumeration(&mut self, bnodeid: &BNode<A>) -> Option<Vec<Individual<A>>> {
        if !self.config.lax {
            return self.retrieve_to_ni_seq(bnodeid);
        }
        self.bnode_seq
            .remove(bnodeid)?
            .iter()
            .filter(|t| !matches!(t, Term::Literal(_)))
            .map(|t| self.term_to_individual(t))
            .collect()
    }

    /// Retrieve a Vec of DataRange or None.
    fn retrieve_to_dr_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<DataRange<A>>> {
        // As with `retrieve_to_ce_seq`: `data_ranges` fills `data_range` over
        // repeated passes, so an anonymous member of this seq may not be a
        // data range yet. Retrieving now would take the seq out of
        // `bnode_seq` for good, and the pass that could complete it would
        // find nothing left to read.
        if !self.bnode_seq.get(bnodeid)?.iter().all(|tdr| match tdr {
            Term::BNode(id) => self.data_range.contains_key(id),
            _ => true,
        }) {
            return None;
        }

        self.retrieve_to_seq(bnodeid, |slf, t| slf.retrieve_to_dr(t))
    }

    /// Retrieve a Vec of Literal or None.
    fn retrieve_to_literal_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<Literal<A>>> {
        self.retrieve_to_seq(bnodeid, |slf, t| slf.convert_to_literal(t))
    }

    /// Retrieve a Vec of Atom or None.
    fn retrieve_to_atom_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<Atom<A>>> {
        self.retrieve_to_seq(bnodeid, |slf, t| slf.atom.remove(t))
    }

    /// Retrieve a Vec of Dargument or None.
    fn retrieve_to_dargument_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<DArgument<A>>> {
        self.retrieve_to_seq(bnodeid, |slf, t| slf.retrieve_to_dargument(t))
    }

    /// Retrieve a DataRange or None.
    fn retrieve_to_dr(&mut self, t: &Term<A>) -> Option<DataRange<A>> {
        match t {
            Term::Iri(iri) => {
                let dt: Datatype<_> = iri.into();
                Some(dt.into())
            }
            Term::BNode(id) => Self::get_used(&self.data_range, &mut self.used_bnode, id),
            _ => None,
        }
    }

    /// Convert to u32 or None.
    fn convert_to_u32(&self, t: &Term<A>) -> Option<u32> {
        match t {
            Term::Literal(val) => val.literal().parse::<u32>().ok(),
            _ => None,
        }
    }

    /// Convert to Literal or None.
    fn convert_to_literal(&self, t: &Term<A>) -> Option<Literal<A>> {
        match t {
            Term::Literal(ob) => Some(ob.clone()),
            _ => None,
        }
    }

    /// Convert to an IArgument or None
    fn retrieve_to_iargument(&mut self, t: &Term<A>) -> Option<IArgument<A>> {
        match t {
            Term::BNode(bn) => {
                Some(IArgument::Individual(self.anon_for_bnode(bn).into()))
            }
            Term::Iri(iri) => self
                // if it is a variable return it
                .variable
                .get(iri)
                .map(|var| var.clone().into())
                // or else it's an individual
                .or_else(|| Some(NamedIndividual(iri.clone()).into())),
            _ => None,
        }
    }

    /// Retrieve or Convert to a DArgument or None.
    fn retrieve_to_dargument(&self, t: &Term<A>) -> Option<DArgument<A>> {
        match t {
            Term::Literal(l) => Some(DArgument::Literal(l.clone())),
            Term::Iri(i) => self.variable.get(i).map(|v| DArgument::Variable(v.clone())),
            _ => None,
        }
    }

    /// Give a Term, return the NamedOWLEntityKind that it represents,
    /// or a Class if we do not know.
    fn distinguish_term_kind(&mut self, term: &Term<A>, ic: &[&O]) -> Option<NamedOWLEntityKind> {
        match term {
            Term::Iri(iri) if crate::vocab::is_xsd_datatype(iri) => {
                Some(NamedOWLEntityKind::Datatype)
            }
            Term::Iri(iri) => self.distinguish_declaration_kind(iri, ic),
            // TODO: this might be too general. At the moment, I am
            // only using this function to distinguish between a
            // datatype and an class
            _ => Some(NamedOWLEntityKind::Class),
        }
    }

    /// As [`Self::distinguish_term_kind`], but for the subject of an
    /// `owl:equivalentClass` triple, which is either a class (giving an
    /// `EquivalentClasses` axiom) or a datatype (giving a `DatatypeDefinition`).
    ///
    /// A `Declaration` is the only evidence `distinguish_term_kind` can consult,
    /// and OWL does not require one: OWLAPI/ROBOT type an entity from the axioms
    /// it occurs in, so
    /// `EquivalentClasses(obo:GO_0051932 ObjectIntersectionOf(…))` with no
    /// `Declaration(Class(obo:GO_0051932))` — exactly what CL's `cl-edit.owl`
    /// contains — is legal, yet we rejected any serialization of it with
    /// "Unknown entity in equivalent class statement". Fall back to the kind the
    /// axiom position implies, using the object as the tie-breaker: an object
    /// that parsed as a data range means a datatype definition, anything else (a
    /// named class, a class-expression bnode) means a class.
    fn distinguish_equivalence_kind(
        &mut self,
        sub: &Term<A>,
        obj: &Term<A>,
        ic: &[&O],
    ) -> Option<NamedOWLEntityKind> {
        if let Some(kind) = self.distinguish_term_kind(sub, ic) {
            return Some(kind);
        }

        match obj {
            Term::BNode(id) if self.data_range.contains_key(id) => {
                Some(NamedOWLEntityKind::Datatype)
            }
            Term::Iri(iri) if crate::vocab::is_xsd_datatype(iri) => {
                Some(NamedOWLEntityKind::Datatype)
            }
            _ => Some(NamedOWLEntityKind::Class),
        }
    }

    /// Given an IRI work out its declaration kind, as defined in
    /// either this Ontology or any Ontology in the import closure.
    ///
    /// If there are multiple contradictory declarations, declarations
    /// in this Ontology will considered first, but otherwise the
    /// result is not defined.
    fn distinguish_declaration_kind(
        &mut self,
        iri: &IRI<A>,
        ic: &[&O],
    ) -> Option<NamedOWLEntityKind> {
        // For this ontology
        [&self.o]
            .iter()
            // and the import closure
            .chain(ic.iter())
            // find the first declaration
            .find_map(|o| {
                <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(o).declaration_kind(iri)
            })
    }

    /// Distinguish or retrieve the property kind using either a
    /// declaration or, for BNode the presence of an
    /// ObjectPropertyExpression.
    ///
    /// In lax mode, return an ObjectProperty if no declaration is
    /// known.
    fn distinguish_retrieve_property_kind(
        &mut self,
        term: &Term<A>,
        ic: &[&O],
    ) -> Option<PropertyExpression<A>> {
        match term {
            Term::OWL(vowl) => {
                let iri = self.config.build.as_ref().iri(vowl.as_ref());
                self.distinguish_retrieve_property_kind(&Term::Iri(iri), ic)
            }
            Term::Iri(iri) => match self.distinguish_declaration_kind(iri, ic) {
                Some(NamedOWLEntityKind::AnnotationProperty) => {
                    Some(PropertyExpression::AnnotationProperty(iri.into()))
                }
                Some(NamedOWLEntityKind::DataProperty) => {
                    Some(PropertyExpression::DataProperty(iri.into()))
                }
                Some(NamedOWLEntityKind::ObjectProperty) => {
                    Some(PropertyExpression::ObjectPropertyExpression(iri.into()))
                }
                _ if self.config.lax => {
                    Some(PropertyExpression::ObjectPropertyExpression(iri.into()))
                }
                _ => None,
            },
            Term::BNode(id) => Some(
                Self::get_used(&self.object_property_expression, &mut self.used_bnode, id)?.into(),
            ),
            _ => None,
        }
    }

    /// Return the property kind of the pair of terms.
    ///
    /// If either or both of the pair is a known type return that.  If
    /// the pair are two different types, return an Error if parsing
    /// is strict or favour the first, if possible. If neither is
    /// known return None.
    #[allow(clippy::type_complexity)]
    fn distinguish_retrieve_property_term_pair_kind(
        &mut self,
        a: &Term<A>,
        b: &Term<A>,
        ic: &[&O],
    ) -> Result<Option<(PropertyExpression<A>, PropertyExpression<A>)>, HornedError> {
        let mut mix_match = |a, b| match (
            Self::get_used(&self.object_property_expression, &mut self.used_bnode, a),
            self.distinguish_declaration_kind(b, ic),
        ) {
            (Some(ope), Some(NamedOWLEntityKind::ObjectProperty)) | (Some(ope), None) => {
                Ok(Some((ope.into(), ObjectProperty(b.clone()).into())))
            }
            (Some(ope), _any) if self.config.lax => {
                Ok(Some((ope.into(), ObjectProperty(b.clone()).into())))
            }
            _ => Err(HornedError::invalid(format!(
                "Types of two properties do not match: {:?} and {:?}",
                a, b
            ))),
        };

        match (a, b) {
            (Term::BNode(a), Term::BNode(b)) => {
                Ok(
                    Self::get_used(&self.object_property_expression, &mut self.used_bnode, a)
                        .zip(Self::get_used(
                            &self.object_property_expression,
                            &mut self.used_bnode,
                            b,
                        ))
                        .map(|(a, b)| {
                            (
                                PropertyExpression::ObjectPropertyExpression(a),
                                PropertyExpression::ObjectPropertyExpression(b),
                            )
                        }),
                )
            }
            (Term::BNode(a), Term::Iri(b)) => mix_match(a, b),
            (Term::Iri(a), Term::BNode(b)) => {
                let t = mix_match(b, a);
                t.map(|o| o.map(|t| (t.1, t.0)))
            }
            (Term::Iri(a), Term::Iri(b)) => self.distinguish_property_iri_pair_kind(a, b, ic),
            _ => Ok(None),
        }
    }

    /// Distinguish the type of a property pair. If one property has a
    /// known type, both are assumed to be the same; if there are
    /// contradictory types return an error or, in lax mode, favour
    /// the first.
    #[allow(clippy::type_complexity)]
    fn distinguish_property_iri_pair_kind(
        &mut self,
        a: &IRI<A>,
        b: &IRI<A>,
        ic: &[&O],
    ) -> Result<Option<(PropertyExpression<A>, PropertyExpression<A>)>, HornedError> {
        use crate::model::NamedOWLEntityKind as NEK;
        match (
            self.distinguish_declaration_kind(a, ic),
            self.distinguish_declaration_kind(b, ic),
        ) {
            (Some(NEK::ObjectProperty), Some(NEK::ObjectProperty))
            | (Some(NEK::ObjectProperty), None)
            | (None, Some(NEK::ObjectProperty)) => Ok(Some((
                ObjectProperty(a.clone()).into(),
                ObjectProperty(b.clone()).into(),
            ))),
            (Some(NEK::DataProperty), Some(NEK::DataProperty))
            | (Some(NEK::DataProperty), None)
            | (None, Some(NEK::DataProperty)) => Ok(Some((
                DataProperty(a.clone()).into(),
                DataProperty(b.clone()).into(),
            ))),
            (Some(NEK::AnnotationProperty), Some(NEK::AnnotationProperty))
            | (Some(NEK::AnnotationProperty), None)
            | (None, Some(NEK::AnnotationProperty)) => Ok(Some((
                AnnotationProperty(a.clone()).into(),
                AnnotationProperty(b.clone()).into(),
            ))),
            (Some(NEK::ObjectProperty), _) if self.config.lax => Ok(Some((
                ObjectProperty(a.clone()).into(),
                ObjectProperty(b.clone()).into(),
            ))),
            (Some(NEK::DataProperty), _) if self.config.lax => Ok(Some((
                DataProperty(a.clone()).into(),
                DataProperty(b.clone()).into(),
            ))),
            (Some(NEK::AnnotationProperty), _) if self.config.lax => Ok(Some((
                AnnotationProperty(a.clone()).into(),
                AnnotationProperty(b.clone()).into(),
            ))),
            (None, None) => Ok(None),
            _ => Err(HornedError::invalid(format!(
                "Types of two properties do not match: {:?} and {:?}",
                a, b
            ))),
        }
    }

    /// Returns an errorif spe is an AnnotationProperty or None if it
    /// is none.
    ///
    /// panics if spe is any other kind of property
    fn error_or_none_on_annotation<X>(
        spe: Option<PropertyExpression<A>>,
        pos: u64,
    ) -> Result<Option<X>, HornedError> {
        match spe {
            Some(PropertyExpression::AnnotationProperty(_)) => Err(HornedError::invalid_at(
                "Unexpected property kind in restriction",
                pos,
            )),
            None => Ok(None),
            // We should already have checked to see whether spe is an
            // Object or Data property meaning that the whole match is
            // exhaustive but we cannot express that in the type system.
            _ => panic!("error_or_none_on_annotation called with wrong type"),
        }
    }

    /// Whether a term OWLAPI's lax reading meets in a class-or-data-range
    /// position reads as a class expression: anything it does not know as a
    /// data range does. A declared class is one whatever else it is declared;
    /// a built-in or declared datatype, or a blank node read as a data range,
    /// is a data range.
    fn is_class_expression_lax(&mut self, t: &Term<A>, ic: &[&O]) -> bool {
        match t {
            Term::BNode(id) => !self.data_range.contains_key(id),
            Term::Literal(_) => false,
            _ => match self.convert_to_iri(t) {
                Some(iri) => match self.distinguish_declaration_kind(&iri, ic) {
                    Some(NamedOWLEntityKind::Class) => true,
                    Some(NamedOWLEntityKind::Datatype) => false,
                    _ => !crate::vocab::is_lax_builtin_datatype(&iri),
                },
                None => true,
            },
        }
    }

    /// The kind of property an `owl:someValuesFrom` or `owl:allValuesFrom`
    /// restriction over `filler` is read with. In lax mode the filler decides,
    /// as OWLAPI reads it: over a class expression the restriction is an object
    /// restriction, and over a data range a data restriction, whatever `pr` is
    /// declared. A property declared an annotation property, or an inverse,
    /// keeps its kind, and strict mode goes by the property alone.
    fn restriction_property_kind(
        &mut self,
        pr: &Term<A>,
        filler: &Term<A>,
        ic: &[&O],
    ) -> Option<PropertyExpression<A>> {
        let declared = self.distinguish_retrieve_property_kind(pr, ic);
        if !self.config.lax || matches!(pr, Term::BNode(_)) {
            return declared;
        }
        // A property declared for annotations is read as the filler makes
        // it, as one declared otherwise is.
        match declared {
            None => declared,
            Some(_) => {
                let iri = self.convert_to_iri(pr)?;
                Some(if self.is_class_expression_lax(filler, ic) {
                    PropertyExpression::ObjectPropertyExpression(iri.into())
                } else {
                    PropertyExpression::DataProperty(iri.into())
                })
            }
        }
    }

    /// The kind of property an `owl:hasValue` restriction is read with. In lax
    /// mode the value decides, as OWLAPI reads it: a literal makes it a data
    /// restriction and anything else an object restriction. Otherwise as
    /// [`Self::restriction_property_kind`].
    fn has_value_property_kind(
        &mut self,
        pr: &Term<A>,
        value: &Term<A>,
        ic: &[&O],
    ) -> Option<PropertyExpression<A>> {
        let declared = self.distinguish_retrieve_property_kind(pr, ic);
        if !self.config.lax || matches!(pr, Term::BNode(_)) {
            return declared;
        }
        match declared {
            None => declared,
            Some(_) => {
                let iri = self.convert_to_iri(pr)?;
                Some(if matches!(value, Term::Literal(_)) {
                    PropertyExpression::DataProperty(iri.into())
                } else {
                    PropertyExpression::ObjectPropertyExpression(iri.into())
                })
            }
        }
    }

    /// The kind of property an `rdfs:range` statement is read with. In lax
    /// mode, as OWLAPI reads it: a declared object property over a class
    /// expression, a declared data property over a data range and a declared
    /// annotation property over a named range keep their kind; any other
    /// statement is an object property range over a class expression and a
    /// data property range over a data range. Strict mode goes by the property
    /// alone.
    fn range_property_kind(
        &mut self,
        pr: &Term<A>,
        range: &Term<A>,
        ic: &[&O],
    ) -> Option<PropertyExpression<A>> {
        let declared = self.distinguish_retrieve_property_kind(pr, ic);
        if !self.config.lax || matches!(pr, Term::BNode(_)) {
            return declared;
        }
        let class = self.is_class_expression_lax(range, ic);
        match declared {
            Some(PropertyExpression::ObjectPropertyExpression(_)) if class => declared,
            Some(PropertyExpression::DataProperty(_)) if !class => declared,
            Some(PropertyExpression::AnnotationProperty(_)) if !matches!(range, Term::BNode(_)) => {
                declared
            }
            None => None,
            Some(_) => {
                let iri = self.convert_to_iri(pr)?;
                Some(if class {
                    PropertyExpression::ObjectPropertyExpression(iri.into())
                } else {
                    PropertyExpression::DataProperty(iri.into())
                })
            }
        }
    }

    /// Process class expressions.
    fn class_expressions(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        let mut parsed_new_ce = false;

        for (this_bnode, v) in self.take_bnode_groups() {
            let ce: Result<_, HornedError> = match v.as_slice() {
                [
                    [_, Term::OWL(VOWL::OnProperty), pr],           //:
                    [_, Term::OWL(VOWL::SomeValuesFrom), ce_or_dr], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => match self.restriction_property_kind(pr, ce_or_dr, ic) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectSomeValuesFrom {
                            ope,
                            bce: self.retrieve_to_ce(ce_or_dr)?.into()
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataSomeValuesFrom {
                            dp,
                            dr: self.retrieve_to_dr(ce_or_dr)?
                        })
                    }
                    any => Self::error_or_none_on_annotation(any, v.position()),
                },
                [
                    [_, Term::OWL(VOWL::HasValue), val],  //:
                    [_, Term::OWL(VOWL::OnProperty), pr], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => match self.has_value_property_kind(pr, val, ic) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectHasValue {
                            ope,
                            i: self.term_to_individual(val)?
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataHasValue {
                            dp,
                            l: self.convert_to_literal(val)?
                        })
                    }
                    any => Self::error_or_none_on_annotation(any, v.position()),
                },
                [
                    [_, Term::OWL(VOWL::AllValuesFrom), ce_or_dr], //:
                    [_, Term::OWL(VOWL::OnProperty), pr],          //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => match self.restriction_property_kind(pr, ce_or_dr, ic) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectAllValuesFrom {
                            ope,
                            bce: self.retrieve_to_ce(ce_or_dr)?.into()
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataAllValuesFrom {
                            dp,
                            dr: self.retrieve_to_dr(ce_or_dr)?
                        })
                    }
                    any => Self::error_or_none_on_annotation(any, v.position()),
                },
                [
                    [_, Term::OWL(VOWL::OneOf), list @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_))], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)],
                ] => Ok(self
                    .retrieve_to_list(list, Self::retrieve_to_enumeration)
                    .map(ClassExpression::ObjectOneOf)),
                // Table 13 types an enumeration owl:Class. One written without
                // the type is still an ObjectOneOf when every member is an
                // individual, named or anonymous: a DataOneOf's members are
                // literals (Table 12).
                [[_, Term::OWL(VOWL::OneOf), Term::BNode(bnodeid)]]
                    if self.bnode_seq.get(bnodeid).is_some_and(|members| {
                        !members.is_empty()
                            && members.iter().all(|m| matches!(m, Term::Iri(_) | Term::BNode(_)))
                    }) =>
                {
                    Ok(self
                        .retrieve_to_ni_seq(bnodeid)
                        .map(ClassExpression::ObjectOneOf))
                }
                [
                    [_, Term::OWL(VOWL::HasSelf), _], //:
                    [_, Term::OWL(VOWL::OnProperty), pr],
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => Ok(self.retrieve_to_ope(pr).map(ClassExpression::ObjectHasSelf)),
                // A connective's list may be empty, `rdf:nil`.
                [
                    [_, Term::OWL(VOWL::IntersectionOf), list @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_))], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)],
                ] => Ok(self
                    .retrieve_to_list(list, Self::retrieve_to_ce_seq)
                    .map(ClassExpression::ObjectIntersectionOf)),
                [
                    [_, Term::OWL(VOWL::UnionOf), list @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_))], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)],
                ] => Ok(self
                    .retrieve_to_list(list, Self::retrieve_to_ce_seq)
                    .map(ClassExpression::ObjectUnionOf)),
                [
                    [_, Term::OWL(VOWL::ComplementOf), tce], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)],
                ] => Ok(self
                    .retrieve_to_ce(tce)
                    .map(|ce| ClassExpression::ObjectComplementOf(ce.into()))),
                [
                    [_, Term::OWL(VOWL::OnDataRange), dr],               //:
                    [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],     //:
                    [_, Term::OWL(VOWL::QualifiedCardinality), literal], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => {
                    ok_some! {
                        ClassExpression::DataExactCardinality
                        {
                            n:self.convert_to_u32(literal)?,
                            dp: pr.into(),
                            dr: self.retrieve_to_dr(dr)?
                        }
                    }
                }
                [
                    [_, Term::OWL(VOWL::MaxQualifiedCardinality), literal], //:
                    [_, Term::OWL(VOWL::OnDataRange), dr],                  //:
                    [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],        //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => {
                    ok_some! {
                        ClassExpression::DataMaxCardinality
                        {
                            n:self.convert_to_u32(literal)?,
                            dp: pr.into(),
                            dr: self.retrieve_to_dr(dr)?
                        }
                    }
                }
                [
                    [_, Term::OWL(VOWL::MinQualifiedCardinality), literal], //:
                    [_, Term::OWL(VOWL::OnDataRange), dr],                  //:
                    [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],        //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => {
                    ok_some! {
                        ClassExpression::DataMinCardinality
                        {
                            n:self.convert_to_u32(literal)?,
                            dp: pr.into(),
                            dr: self.retrieve_to_dr(dr)?
                        }
                    }
                }
                //_:x rdf:type owl:Restriction .
                //_:x owl:cardinality NN_INT(n) .
                //_:x owl:onProperty y .
                //{ OPE(y) ≠ ε }
                [
                    [_, Term::OWL(VOWL::Cardinality), literal], //:
                    [_, Term::OWL(VOWL::OnProperty), pr],       //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => match self.distinguish_retrieve_property_kind(pr, ic) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectExactCardinality {
                            n: self.convert_to_u32(literal)?,
                            ope,
                            bce: self.config.build.as_ref().class(VOWL::Thing).into()
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataExactCardinality {
                            n: self.convert_to_u32(literal)?,
                            dp,
                            dr: self
                                .config
                                .build
                                .as_ref()
                                .datatype(OWL2Datatype::Literal)
                                .into(),
                        })
                    }
                    any => Self::error_or_none_on_annotation(any, v.position()),
                },
                [
                    [_, Term::OWL(VOWL::OnClass), tce],                  //:
                    [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],     //:
                    [_, Term::OWL(VOWL::QualifiedCardinality), literal], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => {
                    ok_some! {
                        ClassExpression::ObjectExactCardinality
                        {
                            n:self.convert_to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.retrieve_to_ce(tce)?.into()
                        }
                    }
                }
                [
                    [_, Term::OWL(VOWL::MinCardinality), literal], //:
                    [_, Term::OWL(VOWL::OnProperty), pr],          //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => match self.distinguish_retrieve_property_kind(pr, ic).or_else(|| {
                    // An undeclared property keeps the object reading an
                    // unqualified cardinality always had.
                    self.convert_to_iri(pr)
                        .map(|iri| PropertyExpression::ObjectPropertyExpression(iri.into()))
                }) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectMinCardinality {
                            n: self.convert_to_u32(literal)?,
                            ope,
                            bce: self.config.build.as_ref().class(VOWL::Thing).into()
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataMinCardinality {
                            n: self.convert_to_u32(literal)?,
                            dp,
                            dr: self
                                .config
                                .build
                                .as_ref()
                                .datatype(OWL2Datatype::Literal)
                                .into(),
                        })
                    }
                    any => Self::error_or_none_on_annotation(any, v.position()),
                },
                [
                    [_, Term::OWL(VOWL::MinQualifiedCardinality), literal], //:
                    [_, Term::OWL(VOWL::OnClass), tce],                     //:
                    [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],        //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => {
                    ok_some! {
                        ClassExpression::ObjectMinCardinality
                        {
                            n:self.convert_to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.retrieve_to_ce(tce)?.into()
                        }
                    }
                }
                [
                    [_, Term::OWL(VOWL::MaxCardinality), literal], //:
                    [_, Term::OWL(VOWL::OnProperty), pr],          //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => match self.distinguish_retrieve_property_kind(pr, ic).or_else(|| {
                    // An undeclared property keeps the object reading an
                    // unqualified cardinality always had.
                    self.convert_to_iri(pr)
                        .map(|iri| PropertyExpression::ObjectPropertyExpression(iri.into()))
                }) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectMaxCardinality {
                            n: self.convert_to_u32(literal)?,
                            ope,
                            bce: self.config.build.as_ref().class(VOWL::Thing).into()
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataMaxCardinality {
                            n: self.convert_to_u32(literal)?,
                            dp,
                            dr: self
                                .config
                                .build
                                .as_ref()
                                .datatype(OWL2Datatype::Literal)
                                .into(),
                        })
                    }
                    any => Self::error_or_none_on_annotation(any, v.position()),
                },
                [
                    [_, Term::OWL(VOWL::MaxQualifiedCardinality), literal], //:
                    [_, Term::OWL(VOWL::OnClass), tce],                     //:
                    [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)],        //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => {
                    ok_some! {
                        ClassExpression::ObjectMaxCardinality
                        {
                            n:self.convert_to_u32(literal)?,
                            ope: pr.into(),
                            bce: self.retrieve_to_ce(tce)?.into()
                        }
                    }
                }
                // An unqualified cardinality with a filler all the same, the
                // shape OWL 1.1 documents wrote, is the qualified one.
                [
                    [_, Term::OWL(card @ (VOWL::Cardinality | VOWL::MaxCardinality | VOWL::MinCardinality)), literal], //:
                    [_, Term::OWL(VOWL::OnClass), tce],              //:
                    [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => ok_some! {{
                    let n = self.convert_to_u32(literal)?;
                    let ope: ObjectPropertyExpression<A> = pr.into();
                    let bce: Box<ClassExpression<A>> = self.retrieve_to_ce(tce)?.into();
                    match card {
                        VOWL::Cardinality => ClassExpression::ObjectExactCardinality { n, ope, bce },
                        VOWL::MaxCardinality => ClassExpression::ObjectMaxCardinality { n, ope, bce },
                        _ => ClassExpression::ObjectMinCardinality { n, ope, bce },
                    }
                }},
                [
                    [_, Term::OWL(card @ (VOWL::Cardinality | VOWL::MaxCardinality | VOWL::MinCardinality)), literal], //:
                    [_, Term::OWL(VOWL::OnDataRange), dr],           //:
                    [_, Term::OWL(VOWL::OnProperty), Term::Iri(pr)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => ok_some! {{
                    let n = self.convert_to_u32(literal)?;
                    let dp: DataProperty<A> = pr.into();
                    let dr = self.retrieve_to_dr(dr)?;
                    match card {
                        VOWL::Cardinality => ClassExpression::DataExactCardinality { n, dp, dr },
                        VOWL::MaxCardinality => ClassExpression::DataMaxCardinality { n, dp, dr },
                        _ => ClassExpression::DataMinCardinality { n, dp, dr },
                    }
                }},
                _a => Ok(None),
            };

            match ce? {
                Some(ce) => {
                    self.class_expression.insert(this_bnode, ce);
                    parsed_new_ce = true;
                }
                _ => {
                    self.bnode.insert(this_bnode, v);
                }
            }
        }

        if parsed_new_ce {
            self.class_expressions(ic)?
        }

        Ok(())
    }

    /// The disjointness an `owl:AllDisjointProperties` node states of the
    /// properties its list names: of data properties when a member is
    /// declared one, otherwise of object properties.
    fn disjoint_properties(&mut self, bnodeid: &BNode<A>, ic: &[&O]) -> Option<Component<A>> {
        let members = self.bnode_seq.get(bnodeid)?.clone();
        let data = members.iter().any(|m| {
            matches!(m, Term::Iri(iri)
                if self.distinguish_declaration_kind(iri, ic) == Some(NamedOWLEntityKind::DataProperty))
        });
        if data {
            let dps = self.retrieve_to_seq(bnodeid, |slf, t| slf.convert_to_dp(t))?;
            Some(DisjointDataProperties(dps).into())
        } else {
            let opes = self.retrieve_to_seq(bnodeid, |slf, t| slf.retrieve_to_ope(t))?;
            Some(DisjointObjectProperties(opes).into())
        }
    }

    #[allow(clippy::type_complexity)]
    fn axioms(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        let mut single_bnodes = vec![];
        let mut leftover = vec![];

        // An axiom stated on a node of its own carries its annotations on that
        // node, after the triples that state it.
        let annotations = |ann: &[[Term<A>; 3]]| {
            ann.iter().all(|t| matches!(t[1], Term::RDFS(_) | Term::Iri(_)))
        };
        for (this_bnode, v) in self.take_bnode_groups() {
            let (axiom, ann): (Result<Option<Component<A>>, HornedError>, &[[Term<A>; 3]]) = match v
                .as_slice()
            {
                [
                    [_, Term::OWL(VOWL::AssertionProperty), pr],     //:
                    [_, Term::OWL(VOWL::SourceIndividual), source], //:
                    [_, target_type, target],                        //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::NegativePropertyAssertion)], //:
                    ann @ ..,
                ] if annotations(ann) => (
                    match target_type {
                        Term::OWL(VOWL::TargetIndividual) => ok_some!(
                            NegativeObjectPropertyAssertion {
                                ope: self.retrieve_to_ope(pr)?,
                                from: self.term_to_individual(source)?,
                                to: self.term_to_individual(target)?,
                            }
                            .into()
                        ),
                        Term::OWL(VOWL::TargetValue) => ok_some!(
                            NegativeDataPropertyAssertion {
                                dp: self.convert_to_dp(pr)?,
                                from: self.term_to_individual(source)?,
                                to: self.convert_to_literal(target)?,
                            }
                            .into()
                        ),
                        _ => Err(HornedError::invalid_at(
                            "Unable to interpret negative property assertion",
                            v.position(),
                        )),
                    },
                    ann,
                ),
                [
                    [_, Term::OWL(VOWL::Members), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDisjointClasses)],
                    ann @ ..,
                ] if annotations(ann) => (
                    ok_some! {
                        DisjointClasses (
                            self.retrieve_to_ce_seq(bnodeid)?
                        ).into()
                    },
                    ann,
                ),
                [
                    [_, Term::OWL(VOWL::Members), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDisjointProperties)],
                    ann @ ..,
                ] if annotations(ann) => (Ok(self.disjoint_properties(bnodeid, ic)), ann),
                [
                    [_, Term::OWL(VOWL::Members | VOWL::DistinctMembers), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDifferent)],
                    ann @ ..,
                ] if annotations(ann) => (
                    ok_some! {
                        DifferentIndividuals (
                            self.retrieve_to_ni_seq(bnodeid)?
                        ).into()
                    },
                    ann,
                ),
                _ => (Ok(None), &[]),
            };

            match axiom? {
                // Two nodes stating one axiom with different annotations are
                // two axioms.
                Some(axiom) => {
                    let ann = self.annotations_of(&Term::BNode(this_bnode), ann)?;
                    let cmp = AnnotatedComponent { component: axiom, ann };
                    if cmp.ann.is_empty() {
                        self.merge(cmp)
                    } else {
                        self.insert_distinct(cmp)
                    }
                }
                _ => leftover.push((this_bnode, v)),
            }
        }

        // What no axiom pattern matched may describe an anonymous individual
        // (Table 16). The groups were taken out in the order the document
        // mentions their nodes, and the individuals take their ids that way.
        for (this_bnode, v) in leftover {
            match self.anonymous_individual_assertions(&v)? {
                Some(assertions) => {
                    for (triple, axiom) in assertions {
                        // Distinct reifications of one base triple stay distinct
                        // annotated axioms.
                        for ann in self.take_anns(&triple) {
                            self.insert_distinct(AnnotatedComponent {
                                component: axiom.clone(),
                                ann,
                            });
                        }
                    }
                }
                None if v.len() == 1 => single_bnodes.push(v[0].clone()),
                None => {
                    self.bnode.insert(this_bnode, v);
                }
            }
        }

        for t in std::mem::take(&mut self.simple)
            .into_iter()
            .chain(single_bnodes.into_iter().map(|t| t.into()))
        {
            let axiom: Result<_, HornedError> = match t.triple() {
                [sub_tce, Term::RDFS(VRDFS::SubClassOf), sup_tce] => ok_some! {
                    SubClassOf {
                        sub: self.retrieve_to_ce(sub_tce)?,
                        sup: self.retrieve_to_ce(sup_tce)?,
                    }
                    .into()
                },
                // TODO: We need to check whether these
                // EquivalentClasses have any other EquivalentClasses
                // and add to that axiom
                [a, Term::OWL(VOWL::EquivalentClass), b] => match self
                    .distinguish_equivalence_kind(a, b, ic)
                {
                    Some(NamedOWLEntityKind::Class) => ok_some! {
                        EquivalentClasses(
                            vec![
                                self.retrieve_to_ce(a)?,
                                self.retrieve_to_ce(b)?,
                            ]).into()
                    },
                    Some(NamedOWLEntityKind::Datatype) => {
                        if let Term::Iri(iri) = a {
                            ok_some! {
                                DatatypeDefinition{
                                    kind: iri.clone().into(),
                                    range: self.retrieve_to_dr(b)?,
                                }.into()
                            }
                        } else {
                            Err(HornedError::invalid_at(
                                "Unexpected entity in equivalent datatype",
                                t.position(),
                            ))
                        }
                    }
                    _ => Err(HornedError::invalid_at(
                        format!(
                            "Unknown entity in equivalent class statement: {:?}",
                            t.triple()
                        ),
                        t.position(),
                    )),
                },
                [class, Term::OWL(VOWL::HasKey), Term::BNode(bnodeid)] => {
                    self.claim_list_annotations(class, VOWL::HasKey, bnodeid);
                    ok_some! {
                        {
                            let vpe: Option<Vec<PropertyExpression<_>>> = self.bnode_seq
                                .remove(bnodeid)?
                                .into_iter()
                                .map(|pr| self.distinguish_retrieve_property_kind(&pr, ic))
                                .collect();

                            HasKey{
                                ce:self.retrieve_to_ce(class)?,
                                vpe: vpe?
                            }.into()
                        }
                    }
                }
                // The list may be empty (`rdf:nil`): a disjoint union of no
                // classes.
                [
                    subject @ Term::Iri(iri),
                    Term::OWL(VOWL::DisjointUnionOf),
                    seq @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_)),
                ] => {
                    if let Term::BNode(bnodeid) = seq {
                        self.claim_list_annotations(subject, VOWL::DisjointUnionOf, bnodeid);
                    }
                    ok_some! {
                        DisjointUnion(
                            Class(iri.clone()),
                            self.retrieve_to_list(seq, Self::retrieve_to_ce_seq)?
                        ).into()
                    }
                }
                // `P owl:inverseOf Q` is an InverseObjectProperties axiom. The
                // bnode-subject form (`_:x owl:inverseOf R`, defining the inverse
                // expression ObjectInverseOf(R)) is consumed earlier in
                // `object_property_expressions`, so a triple reaching here with a
                // named subject is a genuine axiom; either side may itself be an
                // inverse expression (a bnode), so resolve both via retrieve_to_ope.
                [p @ Term::Iri(_), Term::OWL(VOWL::InverseOf), r] => ok_some! {
                    InverseObjectProperties(
                        self.retrieve_to_ope(p)?,
                        self.retrieve_to_ope(r)?
                    ).into()
                },
                [
                    pr,
                    Term::RDF(VRDF::Type),
                    Term::OWL(VOWL::TransitiveProperty),
                ] => {
                    ok_some! {
                        TransitiveObjectProperty(self.retrieve_to_ope(pr)?).into()
                    }
                }
                [pr, Term::RDF(VRDF::Type), Term::OWL(VOWL::FunctionalProperty)] //:
                   =>
                       match self.distinguish_retrieve_property_kind(pr, ic) {
                           Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                               Ok(Some(FunctionalObjectProperty(ope).into()))
                           },
                           Some(PropertyExpression::DataProperty(dp)) => {
                               Ok(Some(FunctionalDataProperty(dp).into()))
                           },
                           any => Self::error_or_none_on_annotation(any, t.position())
                       }
                [
                    pr,
                    Term::RDF(VRDF::Type),
                    Term::OWL(VOWL::AsymmetricProperty | VOWL::AntisymmetricProperty),
                ] => Ok(self
                    .retrieve_to_ope(pr)
                    .map(AsymmetricObjectProperty)
                    .map(Into::into)),
                [
                    pr,
                    Term::RDF(VRDF::Type),
                    Term::OWL(VOWL::SymmetricProperty),
                ] => Ok(self
                    .retrieve_to_ope(pr)
                    .map(SymmetricObjectProperty)
                    .map(Into::into)),
                [
                    pr,
                    Term::RDF(VRDF::Type),
                    Term::OWL(VOWL::ReflexiveProperty),
                ] => Ok(self
                    .retrieve_to_ope(pr)
                    .map(ReflexiveObjectProperty)
                    .map(Into::into)),
                [
                    pr,
                    Term::RDF(VRDF::Type),
                    Term::OWL(VOWL::IrreflexiveProperty),
                ] => Ok(self
                    .retrieve_to_ope(pr)
                    .map(IrreflexiveObjectProperty)
                    .map(Into::into)),
                [
                    pr,
                    Term::RDF(VRDF::Type),
                    Term::OWL(VOWL::InverseFunctionalProperty),
                ] => Ok(self
                    .retrieve_to_ope(pr)
                    .map(InverseFunctionalObjectProperty)
                    .map(Into::into)),
                [Term::Iri(sub), Term::RDF(VRDF::Type), cls] => ok_some! {
                    {
                        ClassAssertion {
                            ce: self.retrieve_to_ce(cls)?,
                            i: NamedIndividual(sub.clone()).into()
                        }.into()
                    }
                },
                [a, Term::OWL(VOWL::DisjointWith), b] => ok_some! {
                        DisjointClasses(vec![
                            self.retrieve_to_ce(a)?,
                            self.retrieve_to_ce(b)?,
                        ]).into()
                },
                [pr, Term::RDFS(VRDFS::SubPropertyOf), spr] => {
                    // If spr's kind can't be determined on its own (e.g. an
                    // undeclared external property), fall back to pr's --
                    // rdfs:subPropertyOf necessarily relates two properties
                    // of the same kind, so a known sub-property is evidence
                    // for its otherwise-unknown super-property's kind, not
                    // just an unfounded guess.
                    ok_some! {
                        match self
                            .distinguish_retrieve_property_kind(spr, ic)
                            .or_else(|| self.distinguish_retrieve_property_kind(pr, ic))?
                        {
                            PropertyExpression::ObjectPropertyExpression(_) =>
                                SubObjectPropertyOf {
                                    sup: self.retrieve_to_ope(spr)?,
                                    sub: self.retrieve_to_sope(pr)?,
                                }.into(),
                            PropertyExpression::DataProperty(_) =>
                                SubDataPropertyOf {
                                    sup: self.convert_to_dp(spr)?,
                                    sub: self.convert_to_dp(pr)?
                                }.into(),
                            PropertyExpression::AnnotationProperty(_) =>
                                SubAnnotationPropertyOf {
                                    sup: self.convert_to_ap(spr)?,
                                    sub: self.convert_to_ap(pr)?
                                }.into(),
                        }
                    }
                }
                // The super-property is a named property, or an inverse stated
                // as the node the chain is stated of.
                [
                    pr @ (Term::Iri(_) | Term::BNode(_)),
                    Term::OWL(VOWL::PropertyChainAxiom),
                    Term::BNode(id),
                ] => {
                    self.claim_list_annotations(pr, VOWL::PropertyChainAxiom, id);
                    ok_some! {{
                        let sup = self.retrieve_to_ope(pr)?;
                        let members = self.bnode_seq.get(id)?.clone();
                        let chain: Option<Vec<_>> =
                            members.iter().map(|t| self.retrieve_to_ope(t)).collect();
                        let chain = chain?;
                        self.bnode_seq.remove(id);
                        SubObjectPropertyOf {
                            sub: SubObjectPropertyExpression::ObjectPropertyChain(chain),
                            sup,
                        }.into()
                    }}
                }
                [pr, Term::RDFS(VRDFS::Domain), t] => {
                    ok_some! {
                        match self.distinguish_retrieve_property_kind(pr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) => ObjectPropertyDomain {
                                ope,
                                ce: self.retrieve_to_ce(t)?,
                            }
                            .into(),
                            PropertyExpression::DataProperty(dp) => DataPropertyDomain {
                                dp,
                                ce: self.retrieve_to_ce(t)?,
                            }
                            .into(),
                            PropertyExpression::AnnotationProperty(ap) => AnnotationPropertyDomain {
                                ap,
                                iri: self.convert_to_iri(t)?,
                            }
                            .into(),
                        }
                    }
                }
                [pr, Term::RDFS(VRDFS::Range), t] => ok_some! {
                    match self.range_property_kind(pr, t, ic)? {
                        PropertyExpression::ObjectPropertyExpression(ope) => ObjectPropertyRange {
                            ope,
                            ce: self.retrieve_to_ce(t)?,
                        }
                        .into(),
                        PropertyExpression::DataProperty(dp) => DataPropertyRange {
                            dp,
                            dr: self.retrieve_to_dr(t)?,
                        }
                        .into(),
                        PropertyExpression::AnnotationProperty(ap) => AnnotationPropertyRange {
                            ap,
                            iri: self.convert_to_iri(t)?,
                        }
                        .into(),
                    }
                },
                [r, Term::OWL(VOWL::PropertyDisjointWith), s] => {
                    match self.distinguish_retrieve_property_term_pair_kind(r, s, ic) {
                        Ok(Some((
                            PropertyExpression::ObjectPropertyExpression(r),
                            PropertyExpression::ObjectPropertyExpression(s),
                        ))) => Ok(Some(DisjointObjectProperties(vec![r, s]).into())),
                        Ok(Some((
                            PropertyExpression::DataProperty(r),
                            PropertyExpression::DataProperty(s),
                        ))) => Ok(Some(DisjointDataProperties(vec![r, s]).into())),
                        Ok(Some((
                            PropertyExpression::AnnotationProperty(r),
                            PropertyExpression::AnnotationProperty(s),
                        ))) => Err(HornedError::invalid(format!(
                            "Annotation properties cannot be disjoint: {:?}, {:?}",
                            r, s
                        ))),
                        // owlready2 emits `owl:equivalentProperty` /
                        // `owl:propertyDisjointWith` with a literal object (and may
                        // even declare the predicate an annotation property, as GSSO
                        // does). Such a triple cannot be a property relation, so
                        // OWLAPI reads it as an annotation assertion. In lax mode do
                        // the same rather than failing the whole document.
                        Ok(None) => match (r, s) {
                            (Term::Iri(sub), Term::Literal(_)) if self.config.lax => self
                                .annotation(t.triple())
                                .map(|ann| {
                                    Some(AnnotationAssertion { subject: sub.into(), ann }.into())
                                }),
                            // Two properties of no known kind relate nothing
                            // the reader can state; the statement stays unread.
                            _ if self.config.lax => Ok(None),
                            _ => Err(HornedError::invalid(
                                "Cannot distinguish the types of {r} and {s}",
                            )),
                        },
                        Err(err) => Err(err),
                        _ => unreachable!("Unexpected error in disjoint property matching"),
                    }
                }
                [r, Term::OWL(VOWL::EquivalentProperty), s] => {
                    match self.distinguish_retrieve_property_term_pair_kind(r, s, ic) {
                        Ok(Some((
                            PropertyExpression::ObjectPropertyExpression(r),
                            PropertyExpression::ObjectPropertyExpression(s),
                        ))) => Ok(Some(EquivalentObjectProperties(vec![r, s]).into())),
                        Ok(Some((
                            PropertyExpression::DataProperty(r),
                            PropertyExpression::DataProperty(s),
                        ))) => Ok(Some(EquivalentDataProperties(vec![r, s]).into())),
                        Ok(Some((
                            PropertyExpression::AnnotationProperty(r),
                            PropertyExpression::AnnotationProperty(s),
                        ))) => Err(HornedError::invalid(format!(
                            "Annotation properties cannot be equivalent: {:?}, {:?}",
                            r, s
                        ))),
                        // owlready2 emits `owl:equivalentProperty` /
                        // `owl:propertyDisjointWith` with a literal object (and may
                        // even declare the predicate an annotation property, as GSSO
                        // does). Such a triple cannot be a property relation, so
                        // OWLAPI reads it as an annotation assertion. In lax mode do
                        // the same rather than failing the whole document.
                        Ok(None) => match (r, s) {
                            (Term::Iri(sub), Term::Literal(_)) if self.config.lax => self
                                .annotation(t.triple())
                                .map(|ann| {
                                    Some(AnnotationAssertion { subject: sub.into(), ann }.into())
                                }),
                            // Two properties of no known kind relate nothing
                            // the reader can state; the statement stays unread.
                            _ if self.config.lax => Ok(None),
                            _ => Err(HornedError::invalid(
                                "Cannot distinguish the types of {r} and {s}",
                            )),
                        },
                        Err(err) => Err(err),
                        _ => unreachable!("Unexpected error in equivalent property matching"),
                    }
                }
                // The other individual may be anonymous; a blank-node subject
                // is read with the rest of what its node states.
                [Term::Iri(sub), Term::OWL(VOWL::SameAs), obj @ (Term::Iri(_) | Term::BNode(_))] => {
                    ok_some!(SameIndividual(vec![sub.into(), self.term_to_individual(obj)?]).into())
                }
                [Term::Iri(i), Term::OWL(VOWL::DifferentFrom), j @ (Term::Iri(_) | Term::BNode(_))] => {
                    ok_some!(DifferentIndividuals(vec![i.into(), self.term_to_individual(j)?]).into())
                }
                [Term::Iri(sub), Term::Iri(pred), lit @ Term::Literal(_)] => {
                    // A `subject predicate "literal"` triple is a DataPropertyAssertion
                    // only when `predicate` is a *declared* data property; otherwise it
                    // is an AnnotationAssertion — matching OWLAPI/ROBOT, which default an
                    // undeclared property used with a literal to an annotation property.
                    if <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&self.o)
                        .is_declaration_kind(pred, NamedOWLEntityKind::DataProperty)
                    {
                        ok_some! {
                            DataPropertyAssertion {
                                dp: pred.clone().into(),
                                from: sub.into(),
                                to: self.convert_to_literal(lit)?
                            }.into()
                        }
                    } else {
                        self.annotation(t.triple())
                            .map(|ann| Some(AnnotationAssertion { subject: sub.into(), ann }.into()))
                    }
                }
                // Table 18: OWL 1 DL states `EquivalentClasses(C, CE)` for a named
                // class `C` by attaching the connective or enumeration to `C`
                // itself. The connective holds the members its list names, however
                // many: none, one or more.
                [
                    x @ Term::Iri(_),
                    Term::OWL(op @ (VOWL::IntersectionOf | VOWL::UnionOf | VOWL::OneOf)),
                    seq @ (Term::BNode(_) | Term::RDF(VRDF::Nil) | Term::Iri(_)),
                ] if self.distinguish_term_kind(x, ic) == Some(NamedOWLEntityKind::Class) => {
                    ok_some! {{
                        let ce = match op {
                            VOWL::OneOf => ClassExpression::ObjectOneOf(self.retrieve_to_list(seq, Self::retrieve_to_ni_seq)?),
                            VOWL::IntersectionOf => {
                                ClassExpression::ObjectIntersectionOf(self.retrieve_to_list(seq, Self::retrieve_to_ce_seq)?)
                            }
                            _ => ClassExpression::ObjectUnionOf(self.retrieve_to_list(seq, Self::retrieve_to_ce_seq)?),
                        };
                        EquivalentClasses(vec![self.retrieve_to_ce(x)?, ce]).into()
                    }}
                }
                [x @ Term::Iri(_), Term::OWL(VOWL::ComplementOf), y]
                    if self.distinguish_term_kind(x, ic) == Some(NamedOWLEntityKind::Class) =>
                {
                    ok_some! {{
                        EquivalentClasses(vec![
                            self.retrieve_to_ce(x)?,
                            ClassExpression::ObjectComplementOf(self.retrieve_to_ce(y)?.into()),
                        ])
                        .into()
                    }}
                }
                // Table 16: `a p _:x` asserts a declared object property `p` between
                // `a` and the anonymous individual `_:x`. Any other predicate
                // annotates `a` with that individual, as the IRI-object rule below
                // reads an undeclared predicate.
                [Term::Iri(sub), Term::Iri(pred), Term::BNode(obj)] if !is_reserved_iri(pred) => {
                    if <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&self.o)
                        .is_declaration_kind(pred, NamedOWLEntityKind::ObjectProperty)
                    {
                        Ok(Some(
                            ObjectPropertyAssertion {
                                ope: ObjectProperty(pred.clone()).into(),
                                from: sub.into(),
                                to: self.anon_for_bnode(obj).into(),
                            }
                            .into(),
                        ))
                    } else {
                        self.annotation(t.triple())
                            .map(|ann| Some(AnnotationAssertion { subject: sub.into(), ann }.into()))
                    }
                }
                [Term::Iri(sub), Term::Iri(pred), Term::Iri(obj)] => {
                    // A `subject predicate object` triple (all IRIs) is an
                    // ObjectPropertyAssertion only when `predicate` is a *declared object
                    // property*; otherwise it is an IRI-valued AnnotationAssertion —
                    // matching OWLAPI/ROBOT, which default an *undeclared* IRI-predicate
                    // (e.g. bare `MONDO_x skos:exactMatch mesh:y` mapping triples, or a
                    // declared annotation property like `obo:IAO_0000231`) to an
                    // annotation property rather than an object-property edge. This is the
                    // IRI-object twin of the literal-object rule above (declared data
                    // property → DataPropertyAssertion, else AnnotationAssertion), and
                    // stops undeclared mapping properties polluting the ABox handed to the
                    // reasoner (MONDO's ~111k skos mappings).
                    if <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&self.o)
                        .is_declaration_kind(pred, NamedOWLEntityKind::ObjectProperty)
                    {
                        Ok(Some(
                            ObjectPropertyAssertion {
                                ope: ObjectProperty(pred.clone()).into(),
                                from: sub.into(),
                                to: obj.into(),
                            }
                            .into(),
                        ))
                    } else {
                        self.annotation(t.triple())
                            .map(|ann| Some(AnnotationAssertion { subject: sub.into(), ann }.into()))
                    }
                }
                _ => Ok(None),
            };

            match axiom? {
                Some(axiom) => {
                    let axiom: Component<A> = axiom;
                    let copied = self.copied_targets.contains(t.triple());
                    // Distinct reifications of the same base triple are distinct
                    // annotated axioms; insert each rather than merging.
                    for ann in self.take_anns(t.triple()) {
                        if copied && !ann.is_empty() {
                            self.bare_twins.push(axiom.clone());
                        }
                        self.insert_distinct(AnnotatedComponent {
                            component: axiom.clone(),
                            ann,
                        });
                    }
                }
                _ => self.simple.push(t),
            }
        }

        Ok(())
    }

    fn swrl(&mut self) -> Result<(), HornedError> {
        // identify variables first
        for t in std::mem::take(&mut self.simple) {
            match t.triple() {
                [Term::Iri(s), Term::RDF(VRDF::Type), Term::SWRL(VSWRL::Variable)] => {
                    self.variable.insert(s.clone(), Variable(s.clone()));
                }
                _ => {
                    self.simple.push(t);
                }
            }
        }

        // Next identify the atoms with a big pattern matcher over bnodes. An
        // atom's argument can name an anonymous individual, so the atoms are
        // read in the order the document mentions them.
        for (bnode, triple) in self.take_bnode_groups() {
            let atom: Result<_, HornedError> = match triple.as_slice() {
                [
                    [_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::ClassAtom)],
                    [_, Term::SWRL(VSWRL::Argument1), arg],
                    [_, Term::SWRL(VSWRL::ClassPredicate), pred],
                ] => {
                    ok_some! {
                        {
                            Atom::ClassAtom{
                                pred: self.retrieve_to_ce(pred)?,
                                arg: self.retrieve_to_iargument(arg)?
                            }
                        }
                    }
                }
                [
                    [_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::DataRangeAtom)],
                    [_, Term::SWRL(VSWRL::Argument1), arg],
                    [_, Term::SWRL(VSWRL::DataRange), pred],
                ] => {
                    ok_some! {
                        {
                            Atom::DataRangeAtom{
                                pred: self.retrieve_to_dr(pred)?,
                                arg: self.retrieve_to_dargument(arg)?
                            }
                        }
                    }
                }
                [
                    [_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::IndividualPropertyAtom)],
                    [_, Term::SWRL(VSWRL::Argument1), arg1],
                    [_, Term::SWRL(VSWRL::Argument2), arg2],
                    [_, Term::SWRL(VSWRL::PropertyPredicate), pred],
                ] => {
                    ok_some! {
                        Atom::ObjectPropertyAtom{
                            pred: self.retrieve_to_ope(pred)?,
                            args: (
                                self.retrieve_to_iargument(arg1)?,
                                self.retrieve_to_iargument(arg2)?,
                            )
                        }
                    }
                }
                [
                    [_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::DatavaluedPropertyAtom)],
                    [_, Term::SWRL(VSWRL::Argument1), arg1],
                    [_, Term::SWRL(VSWRL::Argument2), arg2],
                    [_, Term::SWRL(VSWRL::PropertyPredicate), pred],
                ] => {
                    ok_some! {
                        Atom::DataPropertyAtom {
                            pred: self.convert_to_dp(pred)?,
                            args: (
                                self.retrieve_to_dargument(arg1)?,
                                self.retrieve_to_dargument(arg2)?,
                            )
                        }
                    }
                }
                [
                    [_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::DifferentIndividualsAtom)],
                    [_, Term::SWRL(VSWRL::Argument1), arg1],
                    [_, Term::SWRL(VSWRL::Argument2), arg2],
                ] => {
                    ok_some! {
                        Atom::DifferentIndividualsAtom(
                            self.retrieve_to_iargument(arg1)?,
                            self.retrieve_to_iargument(arg2)?,
                        )
                    }
                }
                [
                    [_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::SameIndividualAtom)],
                    [_, Term::SWRL(VSWRL::Argument1), arg1],
                    [_, Term::SWRL(VSWRL::Argument2), arg2],
                ] => {
                    ok_some! {
                        Atom::SameIndividualAtom(
                            self.retrieve_to_iargument(arg1)?,
                            self.retrieve_to_iargument(arg2)?,
                        )
                    }
                }
                [
                    [_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::BuiltinAtom)],
                    [_, Term::SWRL(VSWRL::Arguments), Term::BNode(args)],
                    [_, Term::SWRL(VSWRL::Builtin), Term::Iri(iri)],
                ] => {
                    ok_some! {
                        Atom::BuiltInAtom{
                            pred: iri.clone(),
                            args: self.retrieve_to_dargument_seq(args)?
                        }
                    }
                }
                _ => Ok(None),
            };

            match atom? {
                Some(atom) => {
                    self.atom.insert(Term::BNode(bnode), atom);
                }
                _ => {
                    self.bnode.insert(bnode, triple);
                }
            }
        }

        // now identfy the rules using "imp" over the bnodes, we
        // should have everything else in place by then to build the
        // entire rule
        for (bnode, triples) in std::mem::take(&mut self.bnode) {
            // Identify a SWRL rule bnode: `rdf:type swrl:Imp` plus `swrl:body`
            // and `swrl:head`. An annotated rule (rdfs:comment/label on the Imp
            // bnode) carries extra triples, so scan for the required parts
            // position-independently and treat every other triple as an axiom
            // annotation rather than requiring an exact 3-triple match (which
            // silently dropped all annotated rules).
            let mut is_imp = false;
            let mut body_bn = None;
            let mut head_bn = None;
            let mut ann_triples: Vec<[Term<A>; 3]> = Vec::new();
            for t in triples.as_slice() {
                match t {
                    [_, Term::RDF(VRDF::Type), Term::SWRL(VSWRL::Imp)] => is_imp = true,
                    [_, Term::SWRL(VSWRL::Body), Term::BNode(b)] => body_bn = Some(b.clone()),
                    [_, Term::SWRL(VSWRL::Head), Term::BNode(h)] => head_bn = Some(h.clone()),
                    other => ann_triples.push(other.clone()),
                }
            }

            let built = match (is_imp, &body_bn, &head_bn) {
                (true, Some(body_bn), Some(head_bn)) => (|| {
                    Some(Rule {
                        head: self.retrieve_to_atom_seq(head_bn)?,
                        body: self.retrieve_to_atom_seq(body_bn)?,
                    })
                })(),
                _ => None,
            };

            match built {
                Some(rule) => {
                    // The annotations stated on the Imp node whose values are
                    // literals annotate the rule, each bare. One whose value is
                    // an IRI or an anonymous individual is an assertion about
                    // the node, an anonymous individual, also bare. Reified
                    // (owl:Axiom) annotations of the rule's type triple are the
                    // rule's too. `ann_map` is Vec-valued, so drain every set.
                    let (literals, resources): (Vec<_>, Vec<_>) =
                        ann_triples.into_iter().partition(|t| matches!(t[2], Term::Literal(_)));
                    let mut ann = self.parse_annotations(&literals)?;
                    for t in &resources {
                        let ann = self.annotation(t)?;
                        let subject: AnonymousIndividual<A> = self.anon_for_bnode(&bnode);
                        self.insert_distinct(AnnotatedComponent {
                            component: AnnotationAssertion { subject: subject.into(), ann }.into(),
                            ann: BTreeSet::new(),
                        });
                    }
                    let key = self.config.build.as_ref().substitute_term([
                        Term::BNode(bnode.clone()),
                        Term::RDF(VRDF::Type),
                        Term::SWRL(VSWRL::Imp),
                    ]);
                    for set in self.ann_map.remove(&key).unwrap_or_default() {
                        ann.extend(set);
                    }
                    let cmp: Component<A> = rule.into();
                    self.insert_distinct(AnnotatedComponent { component: cmp, ann });
                }
                None => {
                    self.bnode.insert(bnode, triples);
                }
            }
        }

        Ok(())
    }

    /// Every triple about the blank node `_:x` read as an assertion about the
    /// anonymous individual `_:x` (Table 16): a class assertion, a property
    /// assertion, `owl:sameAs`, `owl:differentFrom` or an annotation. All or
    /// nothing: a triple of any other shape means the node is not an
    /// individual, or its description is malformed, so nothing is consumed and
    /// the triples stay reported as unparsed.
    ///
    /// A blank node is an individual because a class types it or a property
    /// relates it to something. Every other thing a blank node can be names a
    /// vocabulary term in its triples (`owl:Restriction`, `rdf:List`,
    /// `owl:Axiom`), which is the shape refused here. A predicate is read as
    /// the rules for a named subject read it: a declared data or object
    /// property asserts, a declared or built-in annotation property annotates,
    /// and in lax mode so does any other.
    #[allow(clippy::type_complexity)]
    fn anonymous_individual_assertions(
        &mut self,
        v: &[[Term<A>; 3]],
    ) -> Result<Option<Vec<([Term<A>; 3], Component<A>)>>, HornedError> {
        let Some([Term::BNode(sub), ..]) = v.first() else {
            return Ok(None);
        };
        let anon = self.anon_for_bnode(sub);
        let mut used_ces = vec![];
        let mut out = vec![];
        for t in v {
            let cmp: Component<A> = match t {
                // Table 7 declares only IRIs; typing a blank node
                // owl:NamedIndividual says no more than that it is an individual.
                [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::NamedIndividual)] => continue,
                [_, Term::RDF(VRDF::Type), Term::Iri(cls)] if !is_reserved_iri(cls) => {
                    ClassAssertion {
                        ce: Class(cls.clone()).into(),
                        i: anon.clone().into(),
                    }
                    .into()
                }
                [_, Term::RDF(VRDF::Type), cls @ Term::OWL(VOWL::Thing | VOWL::Nothing)] => {
                    let Some(ce) = self.retrieve_to_ce(cls) else {
                        return Ok(None);
                    };
                    ClassAssertion { ce, i: anon.clone().into() }.into()
                }
                [_, Term::RDF(VRDF::Type), Term::BNode(id)] => {
                    let Some(ce) = self.class_expression.get(id).cloned() else {
                        return Ok(None);
                    };
                    used_ces.push(id.clone());
                    ClassAssertion { ce, i: anon.clone().into() }.into()
                }
                [_, Term::OWL(VOWL::SameAs), o] => {
                    let Some(other) = self.term_to_individual(o) else {
                        return Ok(None);
                    };
                    SameIndividual(vec![anon.clone().into(), other]).into()
                }
                [_, Term::OWL(VOWL::DifferentFrom), o] => {
                    let Some(other) = self.term_to_individual(o) else {
                        return Ok(None);
                    };
                    DifferentIndividuals(vec![anon.clone().into(), other]).into()
                }
                [_, Term::RDFS(rdfs), _] if rdfs.is_builtin() => AnnotationAssertion {
                    subject: anon.clone().into(),
                    ann: self.annotation(t)?,
                }
                .into(),
                [_, Term::Iri(pred), o] if !is_reserved_iri(pred) => {
                    let index = <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&self.o);
                    let declared = |kind| index.is_declaration_kind(pred, kind);
                    let annotates = self.config.lax
                        || declared(NamedOWLEntityKind::AnnotationProperty)
                        || is_annotation_builtin(pred);
                    match o {
                        Term::Literal(_) if declared(NamedOWLEntityKind::DataProperty) => {
                            let Some(to) = self.convert_to_literal(o) else {
                                return Ok(None);
                            };
                            DataPropertyAssertion {
                                dp: pred.clone().into(),
                                from: anon.clone().into(),
                                to,
                            }
                            .into()
                        }
                        Term::Iri(_) | Term::BNode(_)
                            if declared(NamedOWLEntityKind::ObjectProperty) =>
                        {
                            let Some(to) = self.term_to_individual(o) else {
                                return Ok(None);
                            };
                            ObjectPropertyAssertion {
                                ope: ObjectProperty(pred.clone()).into(),
                                from: anon.clone().into(),
                                to,
                            }
                            .into()
                        }
                        Term::Literal(_) | Term::Iri(_) | Term::BNode(_) if annotates => {
                            AnnotationAssertion {
                                subject: anon.clone().into(),
                                ann: self.annotation(t)?,
                            }
                            .into()
                        }
                        _ => return Ok(None),
                    }
                }
                _ => return Ok(None),
            };
            out.push((t.clone(), cmp));
        }
        self.used_bnode.extend(used_ces);
        Ok(Some(out))
    }

    /// The individual a term denotes: the named one an IRI is, or the
    /// anonymous one a blank node names.
    fn term_to_individual(&self, t: &Term<A>) -> Option<Individual<A>> {
        match t {
            Term::Iri(iri) => Some(iri.into()),
            Term::BNode(id) => Some(self.anon_for_bnode(id).into()),
            _ => None,
        }
    }

    fn simple_annotations(&mut self, parse_all: bool) -> Result<(), HornedError> {
        for t in std::mem::take(&mut self.simple) {
            let firi =
                |s: &mut OntologyParser<A, AA, O, B>, t, iri: &IRI<_>| -> Result<(), HornedError> {
                    let base = s.annotation(t)?;
                    // Several owl:Axiom blocks may reify the same base triple
                    // with distinct annotation sets (e.g. NCIT synonyms with
                    // different source annotations) — each is a separate
                    // annotated axiom. Insert each directly; merging would union
                    // their annotation sets and collapse them into one.
                    for ann in s.take_anns(t) {
                        s.insert_distinct(AnnotatedComponent {
                            component: AnnotationAssertion {
                                subject: iri.into(),
                                ann: base.clone(),
                            }
                            .into(),
                            ann,
                        });
                    }
                    Ok(())
                };

            match t.triple() {
                // Catch anything about the ontology and assume it is
                // an annotation. Some versions of the OWL API do not
                // declare annotation properties for ontology annotations
                [s @ Term::Iri(iri), _, o] if self.ontology_nodes.contains(s) => {
                    let ann = BTreeSet::from([self.annotation(t.triple())?]);
                    // Only an annotation whose value is a literal carries the
                    // annotations stated of it; one whose value is an IRI or an
                    // anonymous individual is read bare.
                    let ann = match o {
                        Term::Literal(_) => self.annotate_annotations(&Term::Iri(iri.clone()), ann)?,
                        _ => ann,
                    };
                    for ann in ann {
                        self.o.insert(OntologyAnnotation(ann));
                    }
                }
                [Term::Iri(iri), Term::RDFS(rdfs), _] if rdfs.is_builtin() => {
                    firi(self, t.triple(), iri)?
                }
                [Term::Iri(iri), Term::Iri(ap), _]
                    if parse_all
                        || <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&self.o)
                            .is_annotation_property(ap)
                        || is_annotation_builtin(ap.as_ref()) =>
                {
                    firi(self, t.triple(), iri)?
                }
                _ => {
                    self.simple.push(t);
                }
            }
        }
        // The individuals among a document's blank nodes take their ids in the
        // order the document mentions them, so the groups are visited that way
        // rather than in whatever order they were collected.
        for (k, v) in self.take_bnode_groups() {
            // A blank header node: what it states besides its type annotates
            // the ontology, as a named header's statements do.
            if self.ontology_nodes.contains(&Term::BNode(k.clone())) {
                for t in v.iter() {
                    if matches!(t, [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Ontology)]) {
                        continue;
                    }
                    let ann = BTreeSet::from([self.annotation(t)?]);
                    let ann = match &t[2] {
                        Term::Literal(_) => self.annotate_annotations(&Term::BNode(k.clone()), ann)?,
                        _ => ann,
                    };
                    for ann in ann {
                        self.o.insert(OntologyAnnotation(ann));
                    }
                }
                continue;
            }
            let fbnode =
                |s: &mut OntologyParser<A, AA, O, B>, t, bn: &BNode<A>| -> Result<_, HornedError> {
                    let ind: AnonymousIndividual<A> = s.anon_for_bnode(bn);
                    let base = s.annotation(t)?;
                    // As above: distinct reifications stay distinct axioms.
                    for ann in s.take_anns(t) {
                        s.insert_distinct(AnnotatedComponent {
                            component: AnnotationAssertion {
                                subject: ind.clone().into(),
                                ann: base.clone(),
                            }
                            .into(),
                            ann,
                        });
                    }
                    Ok(())
                };

            // A blank node is an anonymous INDIVIDUAL because something types it by
            // an ordinary class — `_:x rdf:type sssom:MappingSet`. Every other thing
            // a blank node can be names a vocabulary term there (`owl:Restriction`,
            // `rdf:List`, `owl:Axiom`), which is a different `Term` variant, so the
            // type triple alone tells the two apart. Without one, a leftover group is
            // structure this parse did not understand and is left where it is.
            let typed_by_a_class = v.iter().any(|t| {
                matches!(t, [Term::BNode(_), Term::RDF(VRDF::Type), Term::Iri(_)])
            });
            // …and the individual's other triples are its annotations.
            let states_an_individual = |s: &OntologyParser<A, AA, O, B>, t: &[Term<A>; 3]| match t {
                [Term::BNode(_), Term::RDF(VRDF::Type), Term::Iri(_)] => true,
                [Term::BNode(_), Term::RDFS(rdfs), _] => rdfs.is_builtin(),
                [Term::BNode(_), Term::Iri(ap), _] => {
                    parse_all
                        || <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&s.o)
                            .is_annotation_property(ap)
                        || is_annotation_builtin(ap)
                }
                _ => false,
            };

            match v.as_slice() {
                [triple @ [Term::BNode(ind), Term::RDFS(rdfs), _]] if rdfs.is_builtin() => {
                    fbnode(self, triple, ind)?
                }
                [triple @ [Term::BNode(ind), Term::Iri(ap), _]]
                    if parse_all
                        || <O as AsRef<DeclarationMappedIndex<A, AA>>>::as_ref(&self.o)
                            .is_annotation_property(ap)
                        || is_annotation_builtin(ap) =>
                {
                    fbnode(self, triple, ind)?
                }
                // An anonymous individual that says more than one thing about
                // itself — a type and its annotations, as an SSSOM mapping set
                // does. The triples are ONE individual's, so they take one node id
                // between them rather than one each.
                _ if v.len() > 1
                    && typed_by_a_class
                    && v.iter().all(|t| states_an_individual(self, t)) =>
                {
                    let ind: AnonymousIndividual<A> = match v.first() {
                        Some([Term::BNode(bn), ..]) => self.anon_for_bnode(bn),
                        _ => self.config.build.as_ref().anon_renumbered(),
                    };
                    for triple in v.iter() {
                        if let [_, Term::RDF(VRDF::Type), Term::Iri(cls)] = triple {
                            let component: Component<A> =
                                ClassAssertion { ce: Class(cls.clone()).into(), i: ind.clone().into() }.into();
                            // Each reification of the type is an annotated axiom
                            // of its own.
                            for ann in self.take_anns(triple) {
                                let ac = AnnotatedComponent { component: component.clone(), ann };
                                if ac.ann.is_empty() {
                                    self.merge(ac);
                                } else {
                                    self.insert_distinct(ac);
                                }
                            }
                            continue;
                        }
                        let base = self.annotation(triple)?;
                        for ann in self.take_anns(triple) {
                            self.insert_distinct(AnnotatedComponent {
                                component: AnnotationAssertion {
                                    subject: ind.clone().into(),
                                    ann: base.clone(),
                                }
                                .into(),
                                ann,
                            });
                        }
                    }
                }
                _ => {
                    self.bnode.insert(k, v);
                }
            }
        }

        Ok(())
    }

    /// Parse all imports and add to the Ontology.
    /// Return an error is we are in the wrong state
    pub fn parse_imports(&mut self) -> Result<Vec<IRI<A>>, HornedError> {
        match self.state {
            OntologyParserState::New => {
                let timing = std::env::var("OWLMAKE_TIMING").is_ok();
                macro_rules! step {
                    ($name:expr, $body:expr) => {{
                        let t = crate::time::Instant::now();
                        let r = $body;
                        if timing {
                            eprintln!("    imports/{} {:.1}s", $name, t.elapsed().as_secs_f64());
                        }
                        r
                    }};
                }
                let triple = std::mem::take(&mut self.triple);
                self.referenced_bnodes = triple
                    .iter()
                    .filter_map(|t| match &t.triple()[2] {
                        Term::BNode(id) => Some(id.clone()),
                        _ => None,
                    })
                    .collect();
                self.find_ontology_nodes(&triple);
                step!("group_triples", Self::group_triples(triple, &mut self.simple, &mut self.bnode));

                // Identical RDF triples denote the same triple (RDF is a set). A
                // writer that reifies N annotated axioms sharing one base triple
                // may serialise that base N times (owlmake's does, one per
                // annotated synonym/xref); without dedup the first occurrence
                // consumes the reifications from `ann_map` and each duplicate then
                // re-emits as a spurious *unannotated* axiom. Drop exact duplicate
                // simple triples, preserving first-seen order.
                step!("dedup_simple", {
                    let mut seen: rustc_hash::FxHashSet<[Term<A>; 3]> =
                        rustc_hash::FxHashSet::default();
                    self.simple.retain(|t| seen.insert(t.triple().clone()));
                });

                // sort the triples, so that I can get a dependable order
                step!("bnode_sort", for (_, vec) in self.bnode.iter_mut() {
                    vec.sort();
                });

                step!("stitch_seqs", self.stitch_seqs());

                self.inverse_nodes = self
                    .simple
                    .iter()
                    .filter_map(|t| match t.triple() {
                        [Term::BNode(b), Term::OWL(VOWL::InverseOf), Term::Iri(r)] => {
                            Some((b.clone(), r.clone()))
                        }
                        _ => None,
                    })
                    .collect();

                // Table 10
                step!("axiom_annotations", self.axiom_annotations()?);
                let v = step!("resolve_imports", self.resolve_imports());
                self.state = OntologyParserState::Imports;

                Ok(v)
            }
            _ => panic!(
                "parse_imports called out of order: expected OntologyParserState::New, got {:?}",
                self.state
            ),
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

                // Table 7: Declarations (this should be simple, if we have a
                // generic solution for handling annotations, there is no
                // handling of bnodes).
                self.declarations();
                self.state = OntologyParserState::Declarations;
                Ok(())
            }
            _ => panic!(
                "parse_declarations called out of order: expected OntologyParserState::Imports, got {:?}",
                self.state
            ),
        }
    }

    /// Complete the parse of the ontology.
    ///
    /// ic is a Vec of references to the import closure. These RDF
    /// ontologies do not need to be completely parsed, but will be
    /// relied on to resolve declarations.
    pub fn finish_parse(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        let timing = std::env::var("OWLMAKE_TIMING").is_ok();
        macro_rules! phase {
            ($name:expr, $body:expr) => {{
                let t = crate::time::Instant::now();
                let r = $body;
                if timing {
                    eprintln!(
                        "  rdf-map: {} {:.1}s (simple={}, bnode={})",
                        $name,
                        t.elapsed().as_secs_f64(),
                        self.simple.len(),
                        self.bnode.len(),
                    );
                }
                r
            }};
        }
        // Table 10
        phase!("simple_annotations", self.simple_annotations(false)?);
        phase!("data_ranges", self.data_ranges()?);
        // Table 8:
        phase!("object_property_expressions", self.object_property_expressions());
        // Table 13: Parsing of Class Expressions
        phase!("class_expressions", self.class_expressions(ic)?);
        // Table 16: Axioms without annotations
        phase!("axioms", self.axioms(ic)?);
        // SWRL rules
        phase!("swrl", self.swrl()?);

        if self.config.lax {
            phase!("simple_annotations(lax)", self.simple_annotations(true)?);
        }
        for component in std::mem::take(&mut self.bare_twins) {
            self.o.remove(&AnnotatedComponent { component, ann: BTreeSet::new() });
        }
        self.state = OntologyParserState::Parse;
        Ok(())
    }

    /// Parse an Ontology or return an Error if this fails.
    pub fn parse(mut self) -> Result<(O, IncompleteParse<A>), HornedError> {
        let timing = std::env::var("OWLMAKE_TIMING").is_ok();
        match self.state {
            OntologyParserState::New => {
                // Ditch the vec that this might return as we don't
                // need it!
                let t = crate::time::Instant::now();
                self.parse_imports().and(Ok(()))?;
                if timing {
                    eprintln!("  rdf-read: parse_imports {:.1}s", t.elapsed().as_secs_f64());
                }
                self.parse()
            }
            OntologyParserState::Imports => {
                let t = crate::time::Instant::now();
                self.parse_declarations()?;
                if timing {
                    eprintln!(
                        "  rdf-read: parse_declarations {:.1}s (simple={}, bnode={})",
                        t.elapsed().as_secs_f64(),
                        self.simple.len(),
                        self.bnode.len()
                    );
                }
                self.parse()
            }
            OntologyParserState::Declarations => {
                self.finish_parse(vec![].as_slice())?;
                self.parse()
            }
            OntologyParserState::Parse => Ok(self.as_ontology_and_incomplete()),
        }
    }

    /// Return a reference to the Ontology
    ///
    /// The ontology will be incomplete or even empty if the parse has not been completed.
    /// See `parse` to ensure that this has happened.
    pub fn ontology_ref(&self) -> &O {
        &self.o
    }

    /// Return a mutable reference to the Ontology
    ///
    /// The ontology will be incomplete or even empty if the parse has not been completed.
    /// See `parse` to ensure that this has happened.
    pub fn mut_ontology_ref(&mut self) -> &mut O {
        &mut self.o
    }

    /// Consume the parser and return an Ontology.
    ///
    /// The ontology will be incomplete or even empty if the parse has not been completed.
    /// See `parse` to ensure that this has happened.
    pub fn as_ontology(self) -> O {
        self.o
    }

    /// Consume the parser and return an Ontology and any data
    /// structures that have not been fully parsed
    ///
    /// The ontology will be incomplete or even empty if the parse has not been completed.
    /// See `parse` to ensure that this has happened.
    pub fn as_ontology_and_incomplete(mut self) -> (O, IncompleteParse<A>) {
        // Regroup so that they print out nicer
        let mut simple = vec![];

        Self::group_triples(
            std::mem::take(&mut self.simple),
            &mut simple,
            &mut self.bnode,
        );

        // An owl:Annotation node nothing read is reported with the rest.
        let bnode: Vec<_> = self
            .bnode
            .into_values()
            .chain(self.annotation_nodes.into_values().flatten().map(|(_, v)| v))
            .collect();
        let bnode_seq: Vec<_> = self.bnode_seq.into_values().collect();

        // Entries in `used_bnode` were retrieved (possibly more than
        // once, e.g. a restriction shared by two rdf:List members -- see
        // #254) rather than removed, so they must be filtered out here
        // to still be excluded from the incomplete-parse report.
        // A class expression is consumed when its pattern matches, whether
        // or not an axiom then uses it (S3.2.4: each time a pattern is
        // matched, the matched triples are removed from the graph). So one
        // that no triple refers to stands alone, parsed completely and
        // contributing no axiom, as an OWL 1 document states a class
        // description exists. One referred to but never used is still
        // reported: the reference was lost.
        let class_expression: Vec<_> = self
            .class_expression
            .into_iter()
            .filter(|(k, _)| !self.used_bnode.contains(k) && self.referenced_bnodes.contains(k))
            .map(|(_, v)| v)
            .collect();
        let object_property_expression: Vec<_> = self
            .object_property_expression
            .into_iter()
            .filter(|(k, _)| !self.used_bnode.contains(k))
            .map(|(_, v)| v)
            .collect();
        let data_range: Vec<_> = self
            .data_range
            .into_iter()
            .filter(|(k, _)| !self.used_bnode.contains(k))
            .map(|(_, v)| v)
            .collect();

        (
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
        )
    }
}

/// True for an IRI in the RDF, RDFS, OWL or XSD namespace: reserved
/// vocabulary, never a user's class or an individual's property.
fn is_reserved_iri<A: ForIRI>(iri: &IRI<A>) -> bool {
    [
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
        "http://www.w3.org/2000/01/rdf-schema#",
        "http://www.w3.org/2002/07/owl#",
        "http://www.w3.org/2001/XMLSchema#",
    ]
    .iter()
    .any(|ns| iri.as_ref().starts_with(ns))
}

pub fn parser_with_build<
    A: ForIRI,
    AA: ForIndex<A>,
    O: RDFOntology<A, AA>,
    B: AsRef<Build<A>>,
    R: BufRead,
>(
    bufread: &mut R,
    config: RDFParserConfiguration<A, B>,
) -> Result<OntologyParser<A, AA, O, B>, HornedError> {
    OntologyParser::from_bufread(bufread, config)
}

/// What the reader that parsed a document knows of the order its statements
/// are read in, beyond the order it made them.
#[derive(Clone, Debug, Default)]
pub struct StatementOrder {
    /// The document's axiom nodes, by label, in the order their annotations
    /// are read: an `owl:Annotation` node naming several of them as its source
    /// annotates the first.
    pub axiom_nodes: Vec<String>,
    /// The document's `owl:Annotation` nodes, by label, in the order their
    /// annotations are read: of several naming the same annotation of one
    /// source, the first annotates it.
    pub annotation_nodes: Vec<String>,
    /// The statements, by position, whose literal the document types
    /// `xsd:string`: a `Triple` holds that literal and the untyped one alike,
    /// and an ontology sorts them apart.
    pub typed_strings: std::collections::HashSet<usize>,
}

/// Read the statements of a document another reader has already parsed, in
/// the order it made them, as `order` says they are read.
///
/// Every blank node that is an anonymous individual is named by its own
/// label.
pub fn read_statements<A: ForIRI, AA: ForIndex<A>, B: AsRef<Build<A>>>(
    statements: impl IntoIterator<Item = Triple>,
    config: ParserConfiguration<A, B>,
    order: &StatementOrder,
) -> Result<(ConcreteRDFOntology<A, AA>, IncompleteParse<A>), HornedError> {
    let typed_strings = &order.typed_strings;
    let build = config.build.as_ref();
    let triples = statements
        .into_iter()
        .enumerate()
        .map(|(i, t)| {
            let PosTriple([s, p, o], pos) = build.convert_substitute_triple(t, 0);
            let o = match o {
                Term::Literal(Literal::Simple { literal }) if typed_strings.contains(&i) => {
                    Term::Literal(Literal::Datatype {
                        literal,
                        datatype_iri: build.iri("http://www.w3.org/2001/XMLSchema#string"),
                    })
                }
                o => o,
            };
            PosTriple([s, p, o], pos)
        })
        .collect();
    let mut parser: OntologyParser<A, AA, ConcreteRDFOntology<A, AA>, B> = OntologyParser::new(triples, config);
    parser.keep_labels = true;
    parser.axiom_rank = order.axiom_nodes.iter().enumerate().map(|(i, n)| (A::from(n.clone()), i)).collect();
    parser.annotation_rank =
        order.annotation_nodes.iter().enumerate().map(|(i, n)| (A::from(n.clone()), i)).collect();
    parser.parse()
}

/// Read the whole of `bufread` into a `ConcreteRDFOntology`, along with
/// whatever the parse couldn't map to OWL2 structures. A caller wanting a
/// different `Ontology` implementation converts from the result --
/// `ConcreteRDFOntology` has direct `Into` impls for `SetOntology` and
/// `ComponentMappedOntology`, and `.into_iter().collect()` reaches any
/// other `MutableOntology` implementor.
pub fn read<A: ForIRI, AA: ForIndex<A>, B: AsRef<Build<A>>, R: BufRead>(
    bufread: &mut R,
    config: RDFParserConfiguration<A, B>,
) -> Result<(ConcreteRDFOntology<A, AA>, IncompleteParse<A>), HornedError> {
    parser_with_build(bufread, config)?.parse()
}

#[cfg(test)]
mod test {
    use super::*;

    use std::path::PathBuf;
    use std::rc::Rc;

    use crate::normalize::normalize;
    use crate::ontology::component_mapped::RcComponentMappedOntology;
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    fn read_ok<R: BufRead>(
        bufread: &mut R,
    ) -> ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>> {
        let r = read(bufread, Default::default());

        if let Err(e) = r {
            panic!("Expected ontology, get failure: {e:?}",);
        }

        let (ont, incomp) = r.unwrap();
        dbg!(&ont, &incomp);
        assert!(incomp.is_complete());
        ont
    }

    fn compare_two(testrdf: &str, testowl: &str) {
        let dir_path_buf = PathBuf::from(file!());
        let dir = dir_path_buf.parent().unwrap().to_string_lossy();

        compare_str(
            &slurp::read_all_to_string(format!("{dir}/../../ont/owl-rdf/{testrdf}.owl")).unwrap(),
            &slurp::read_all_to_string(format!("{dir}/../../ont/owl-xml/{testowl}.owx")).unwrap(),
        );
    }

    fn slurp_rdfont(testrdf: &str) -> std::string::String {
        let dir_path_buf = PathBuf::from(file!());
        let dir = dir_path_buf.parent().unwrap().to_string_lossy();

        slurp::read_all_to_string(format!("{dir}/../../ont/owl-rdf/{testrdf}.owl")).unwrap()
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

    #[test]
    fn test_iterable_ontology_iter() {
        let mut o: ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>> = Default::default();
        let build = Build::new_rc();
        o.insert(DeclareClass(build.class("http://www.example.com#a")));
        o.insert(DeclareClass(build.class("http://www.example.com#b")));

        assert_eq!(Ontology::iter(&o).count(), 2);
    }

    #[test]
    fn test_iterable_ontology_into_iter() {
        let mut o: ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>> = Default::default();
        let build = Build::new_rc();
        o.insert(DeclareClass(build.class("http://www.example.com#a")));
        o.insert(DeclareClass(build.class("http://www.example.com#b")));

        assert_eq!(o.into_iter().count(), 2);
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

    #[rstest]
    fn compare_to_xml(#[files("src/ont/owl-rdf/*.owl")] resource: PathBuf) {
        let stem = resource.file_stem().unwrap().to_str().unwrap();
        compare_two(stem, stem);
    }

    #[rstest]
    fn test_read_ok(#[files("src/ont/owl-rdf/ambiguous/*.owl")] resource: PathBuf) {
        let resource = &slurp::read_all_to_string(&resource).unwrap();
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
    fn non_deterministic_rdf_parse() {
        //    https://github.com/phillord/horned-owl/issues/123
        let mut vont: Vec<SetOntology<_>> = vec![];
        for _ in 0..10 {
            vont.push(read_ok(&mut slurp_rdfont("manual/oeo-snippet").as_bytes()).into());
        }

        let first = &vont[0];
        assert!(vont.iter().all(|ont| ont == first));
    }

    #[test]
    fn punning_in_ec() {
        //    https://github.com/phillord/horned-owl/issues/124
        //    https://github.com/phillord/horned-owl/issues/129

        let _ont: SetOntology<_> =
            read_ok(&mut slurp_rdfont("manual/ec_short_124_129").as_bytes()).into();
    }

    #[test]
    fn import_with_partial_parse() {
        let mut p: OntologyParser<_, Rc<AnnotatedComponent<RcStr>>, ConcreteRDFOntology<_, _>, _> =
            parser_with_build(&mut slurp_rdfont("import").as_bytes(), Default::default()).unwrap();
        p.parse_imports().unwrap();

        let rdfont = p.as_ontology();
        let so: SetOntology<_> = rdfont.into();
        let amont: RcComponentMappedOntology = so.into();
        assert_eq!(amont.i().import().count(), 1);
    }

    #[test]
    fn declaration_with_partial_parse() {
        let mut p: OntologyParser<_, Rc<AnnotatedComponent<RcStr>>, ConcreteRDFOntology<_, _>, _> =
            parser_with_build(&mut slurp_rdfont("class").as_bytes(), Default::default()).unwrap();
        let _ = p.parse_declarations();

        let rdfont = p.as_ontology();
        let so: SetOntology<_> = rdfont.into();
        let amont: RcComponentMappedOntology = so.into();
        assert_eq!(amont.i().declare_class().count(), 1);
    }

    #[test]
    fn import_property_in_bits() -> Result<(), HornedError> {
        let b = Build::new_rc();
        let p: OntologyParser<_, Rc<AnnotatedComponent<RcStr>>, ConcreteRDFOntology<_, _>, _> =
            parser_with_build(
                &mut slurp_rdfont("withimport/other-property").as_bytes(),
                ParserConfiguration::new(&b).into(),
            )?;
        let (family_other, incomplete) = p.parse()?;
        assert!(incomplete.is_complete());

        let mut p = parser_with_build(
            &mut slurp_rdfont("withimport/import-property").as_bytes(),
            ParserConfiguration::new(&b).into(),
        )?;
        p.parse_imports()?;
        p.parse_declarations()?;
        p.finish_parse(vec![&family_other].as_slice())?;

        let (_rdfont, incomplete) = p.as_ontology_and_incomplete();
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

    #[test]
    fn shared_restriction_bnode() {
        // https://github.com/phillord/horned-owl/issues/254
        // A class-expression blank node referenced from two separate
        // rdf:List members must be resolved for both references, not just
        // the first one to consume it. See galen_snippet.owl for
        // provenance -- extracted from the real GALEN corpus file.
        let (ont, incomplete) = read::<RcStr, RcAnnotatedComponent, _, _>(
            &mut slurp_rdfont("manual/galen_snippet").as_bytes(),
            Default::default(),
        )
        .unwrap();
        assert!(incomplete.is_complete());

        let ont: ComponentMappedOntology<_, RcAnnotatedComponent> = ont.into();
        assert_eq!(ont.i().sub_class_of().count(), 1);
        assert_eq!(ont.i().equivalent_class().count(), 1);
    }

    #[test]
    fn legacy_rdf_property_declaration() {
        // https://github.com/phillord/horned-owl/issues/255
        // Table 5 (OWL 2 Mapping to RDF Graphs S3.1.2): an OWL 1-style
        // `x rdf:type rdf:Property` triple, paired with an explicit
        // `x rdf:type owl:AnnotationProperty`, must not survive to be
        // read back as a spurious ClassAssertion(Class(rdf:Property), x).
        // See ontokbcf_snippet.owl for provenance -- extracted from the
        // real ONTOKBCF corpus file.
        let (ont, incomplete) = read::<RcStr, RcAnnotatedComponent, _, _>(
            &mut slurp_rdfont("manual/ontokbcf_snippet").as_bytes(),
            Default::default(),
        )
        .unwrap();
        assert!(incomplete.is_complete());

        let ont: ComponentMappedOntology<_, RcAnnotatedComponent> = ont.into();
        assert_eq!(ont.i().declare_annotation_property().count(), 2);
        assert_eq!(ont.i().class_assertion().count(), 0);
    }

    #[test]
    fn table_6_rewrites() {
        // https://github.com/phillord/horned-owl/issues/255
        // Table 6 (OWL 2 Mapping to RDF Graphs S3.1.2): owl:OntologyProperty
        // is rewritten to owl:AnnotationProperty; owl:SymmetricProperty
        // (like owl:TransitiveProperty/owl:InverseFunctionalProperty)
        // additionally implies owl:ObjectProperty.
        let xml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:owl="http://www.w3.org/2002/07/owl#"
         xmlns="http://example.org/mini#"
         xml:base="http://example.org/mini">
  <owl:Ontology rdf:about="http://example.org/mini"/>

  <rdf:Description rdf:about="http://example.org/mini#p1">
    <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#OntologyProperty"/>
  </rdf:Description>

  <!-- SymmetricProperty is not itself a Table 5 trigger, and Table 5 runs
       before Table 6 per spec, so this bare rdf:Property triple is *not*
       cleaned up even though Table 6 goes on to add owl:ObjectProperty for
       the same subject -- deliberately asserted below, not a bug. -->
  <rdf:Property rdf:ID="p2">
    <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#SymmetricProperty"/>
  </rdf:Property>
</rdf:RDF>"#;

        let (ont, incomplete): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
            read(&mut xml.as_bytes(), Default::default()).unwrap();
        assert!(!incomplete.is_complete());
        assert_eq!(incomplete.simple.len(), 1);

        let ont: ComponentMappedOntology<_, RcAnnotatedComponent> = ont.into();
        assert_eq!(ont.i().declare_annotation_property().count(), 1);
        assert_eq!(ont.i().declare_object_property().count(), 1);
        assert_eq!(ont.i().symmetric_object_property().count(), 1);
        assert_eq!(ont.i().class_assertion().count(), 0);
    }

    #[test]
    fn legacy_antisymmetric_property_declaration() {
        // https://github.com/phillord/horned-owl/issues/256
        // owl:AntisymmetricProperty is the OWL 1.1 working-draft name for
        // what became owl:AsymmetricProperty in the OWL 2 REC. It must be
        // read as AsymmetricObjectProperty, not fall through to a spurious
        // ClassAssertion. See spo_snippet.owl for provenance -- extracted
        // from the real SPO (Multiscale Skin Physiology Ontology) corpus
        // file, where ro:has_grain is typed both AntisymmetricProperty and
        // IrreflexiveProperty.
        let (ont, incomplete) = read::<RcStr, RcAnnotatedComponent, _, _>(
            &mut slurp_rdfont("manual/spo_snippet").as_bytes(),
            Default::default(),
        )
        .unwrap();
        assert!(incomplete.is_complete());

        let ont: ComponentMappedOntology<_, RcAnnotatedComponent> = ont.into();
        assert_eq!(ont.i().asymmetric_object_property().count(), 1);
        assert_eq!(ont.i().irreflexive_object_property().count(), 1);
        assert_eq!(ont.i().class_assertion().count(), 0);
    }

    #[test]
    fn sub_property_of_infers_kind_from_known_sibling() {
        // https://github.com/phillord/horned-owl/issues/257
        // rdfs:subPropertyOf necessarily relates two properties of the same
        // kind, so if the super-property's kind can't be determined on its
        // own (here, dc:terms:alternative is external and never declared),
        // fall back to the sub-property's known kind instead of dropping
        // the triple. See edam_bioimaging_snippet.owl for provenance --
        // extracted from the real EDAM-BIOIMAGING corpus file.
        let (ont, incomplete) = read::<RcStr, RcAnnotatedComponent, _, _>(
            &mut slurp_rdfont("manual/edam_bioimaging_snippet").as_bytes(),
            Default::default(),
        )
        .unwrap();
        assert!(incomplete.is_complete());

        let ont: ComponentMappedOntology<_, RcAnnotatedComponent> = ont.into();
        assert_eq!(ont.i().sub_annotation_property_of().count(), 1);
    }

    #[test]
    fn orphan_entries_reported_as_incomplete() {
        // #254's fix changed class_expression/object_property_expression/
        // data_range lookups from consuming (.remove()) to non-destructive,
        // tracked via `used_bnode` -- an entry in any of the three maps
        // that is fully parseable but never referenced by anything else
        // must still be reported as incomplete, not silently treated as
        // "used" just because it was resolved. One orphan per map, so a
        // regression in any single map's filtering shows up on its own.
        let xml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
         xmlns:owl="http://www.w3.org/2002/07/owl#">
    <owl:Ontology rdf:about="http://www.example.com/iri"/>
    <owl:ObjectProperty rdf:about="http://www.example.com/iri#p"/>
    <owl:ObjectProperty rdf:about="http://www.example.com/iri#q"/>
    <owl:Class rdf:about="http://www.example.com/iri#B"/>

    <!-- orphan ClassExpression -->
    <rdf:Description rdf:nodeID="OrphanCE">
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Restriction"/>
        <owl:onProperty rdf:resource="http://www.example.com/iri#p"/>
        <owl:someValuesFrom rdf:resource="http://www.example.com/iri#B"/>
    </rdf:Description>

    <!-- orphan ObjectPropertyExpression -->
    <rdf:Description rdf:nodeID="OrphanOPE">
        <owl:inverseOf rdf:resource="http://www.example.com/iri#q"/>
    </rdf:Description>

    <!-- orphan DataRange -->
    <rdf:Description rdf:nodeID="OrphanDRList">
        <rdf:first rdf:resource="http://www.w3.org/2001/XMLSchema#integer"/>
        <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
    </rdf:Description>
    <rdf:Description rdf:nodeID="OrphanDR">
        <rdf:type rdf:resource="http://www.w3.org/2000/01/rdf-schema#Datatype"/>
        <owl:unionOf rdf:nodeID="OrphanDRList"/>
    </rdf:Description>
</rdf:RDF>"#;

        let (_ont, incomplete): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
            read(&mut xml.as_bytes(), Default::default()).unwrap();

        assert!(!incomplete.is_complete());
        // A class expression nothing refers to is consumed by its own
        // pattern (see `as_ontology_and_incomplete`), so only the property
        // expression and the data range are reported. A class expression
        // that is referred to but never used is covered by
        // `referenced_but_unused_class_expression_is_reported`.
        assert_eq!(incomplete.class_expression.len(), 0);
        assert_eq!(incomplete.object_property_expression.len(), 1);
        assert_eq!(incomplete.data_range.len(), 1);
    }

    /// RDF/XML with the ontology header and the `ex:` namespace; `body` is
    /// what the document says.
    fn owl1_doc(body: &str) -> String {
        format!(
            r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
         xmlns:owl="http://www.w3.org/2002/07/owl#"
         xmlns:ex="http://example.com/x#">
    <owl:Ontology rdf:about="http://example.com/x"/>
{body}
</rdf:RDF>"#
        )
    }

    fn read_owl1(
        body: &str,
    ) -> (
        ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
        IncompleteParse<RcStr>,
    ) {
        let (ont, incomplete) = read::<RcStr, RcAnnotatedComponent, _, _>(
            &mut owl1_doc(body).as_bytes(),
            Default::default(),
        )
        .unwrap();
        (ont.into(), incomplete)
    }

    #[test]
    fn an_ontology_annotation_with_a_resource_value_is_read_bare() {
        // The annotations stated of an ontology annotation are its own when its
        // value is a literal, and not read when its value is an IRI or an
        // anonymous individual.
        let annotation = |target: &str| {
            format!(
                r#"<owl:Annotation>
        <owl:annotatedSource rdf:resource="http://example.com/x"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#seeAlso"/>
        {target}
        <rdfs:comment>nested</rdfs:comment>
    </owl:Annotation>"#
            )
        };
        let (ont, _) = read_owl1(&format!(
            r#"<rdf:Description rdf:about="http://example.com/x">
        <rdfs:seeAlso rdf:resource="http://example.com/x#elsewhere"/>
        <rdfs:seeAlso rdf:nodeID="v"/>
        <rdfs:seeAlso>text</rdfs:seeAlso>
    </rdf:Description>
    {}
    {}
    {}"#,
            annotation(r#"<owl:annotatedTarget rdf:resource="http://example.com/x#elsewhere"/>"#),
            annotation(r#"<owl:annotatedTarget rdf:nodeID="v"/>"#),
            annotation("<owl:annotatedTarget>text</owl:annotatedTarget>"),
        ));
        let mut nested: Vec<(bool, usize)> = ont
            .i()
            .component_for_kind(ComponentKind::OntologyAnnotation)
            .map(|ac| match &ac.component {
                Component::OntologyAnnotation(OntologyAnnotation(a)) => {
                    (matches!(a.av, AnnotationValue::Literal(_)), a.ann.len())
                }
                c => panic!("{c:?}"),
            })
            .collect();
        nested.sort();
        assert_eq!(nested, vec![(false, 0), (false, 0), (true, 1)]);
    }

    #[test]
    fn a_rule_takes_its_literal_annotations_bare() {
        // A literal annotation of the rule's node annotates the rule, without
        // the annotations stated of it; one whose value is an anonymous
        // individual is an assertion about the node.
        let (ont, _) = read_owl1(
            r#"<swrl:Variable rdf:about="urn:swrl:var#x" xmlns:swrl="http://www.w3.org/2003/11/swrl#"/>
    <owl:Class rdf:about="http://example.com/x#A"/>
    <owl:Class rdf:about="http://example.com/x#B"/>
    <swrl:Imp rdf:nodeID="rule" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
        <swrl:body>
            <swrl:AtomList>
                <rdf:first>
                    <swrl:ClassAtom>
                        <swrl:classPredicate rdf:resource="http://example.com/x#A"/>
                        <swrl:argument1 rdf:resource="urn:swrl:var#x"/>
                    </swrl:ClassAtom>
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:body>
        <swrl:head>
            <swrl:AtomList>
                <rdf:first>
                    <swrl:ClassAtom>
                        <swrl:classPredicate rdf:resource="http://example.com/x#B"/>
                        <swrl:argument1 rdf:resource="urn:swrl:var#x"/>
                    </swrl:ClassAtom>
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:head>
        <rdfs:label>rule</rdfs:label>
        <rdfs:seeAlso rdf:nodeID="v"/>
    </swrl:Imp>
    <owl:Annotation>
        <owl:annotatedSource rdf:nodeID="rule"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#label"/>
        <owl:annotatedTarget>rule</owl:annotatedTarget>
        <rdfs:comment>nested</rdfs:comment>
    </owl:Annotation>"#,
        );
        let rules: Vec<_> = ont.i().component_for_kind(ComponentKind::Rule).collect();
        assert_eq!(rules.len(), 1, "{rules:#?}");
        let anns: Vec<_> = rules[0].ann.iter().collect();
        assert_eq!(anns.len(), 1, "{anns:#?}");
        assert!(matches!(anns[0].av, AnnotationValue::Literal(_)) && anns[0].ann.is_empty(), "{anns:#?}");
        let assertions: Vec<_> = ont.i().component_for_kind(ComponentKind::AnnotationAssertion).collect();
        assert!(
            matches!(
                &assertions[..],
                [ac] if matches!(&ac.component, Component::AnnotationAssertion(AnnotationAssertion {
                    subject: AnnotationSubject::AnonymousIndividual(_),
                    ann: Annotation { av: AnnotationValue::AnonymousIndividual(_), .. },
                }))
            ),
            "{assertions:#?}"
        );
    }

    #[test]
    fn of_two_annotation_nodes_naming_one_annotation_the_first_read_annotates_it() {
        // Two `owl:Annotation` nodes name the same annotation of an axiom; the
        // one read first gives it its annotations, and the other's are not
        // read. Which is first is the order the parse says.
        use oxrdf::Literal as OxLiteral;
        let nn = |s: &str| NamedNode::new_unchecked(s);
        let bn = |s: &str| BlankNode::new_unchecked(s);
        let owl = |l: &str| nn(&format!("http://www.w3.org/2002/07/owl#{l}"));
        let rdf_type = nn("http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
        let (comment, label) =
            (nn("http://www.w3.org/2000/01/rdf-schema#comment"), nn("http://www.w3.org/2000/01/rdf-schema#label"));
        let (a, c) = (nn("http://example.com/A"), nn("http://example.com/C"));
        let sub = nn("http://www.w3.org/2000/01/rdf-schema#subClassOf");
        let mut statements = vec![
            Triple::new(a.clone(), sub.clone(), c.clone()),
            Triple::new(bn("ax"), rdf_type.clone(), owl("Axiom")),
            Triple::new(bn("ax"), owl("annotatedSource"), a.clone()),
            Triple::new(bn("ax"), owl("annotatedProperty"), sub.clone()),
            Triple::new(bn("ax"), owl("annotatedTarget"), c.clone()),
            Triple::new(bn("ax"), comment.clone(), OxLiteral::new_simple_literal("branch")),
        ];
        for (node, nested) in [("one", "first"), ("two", "second")] {
            statements.extend([
                Triple::new(bn(node), rdf_type.clone(), owl("Annotation")),
                Triple::new(bn(node), owl("annotatedSource"), bn("ax")),
                Triple::new(bn(node), owl("annotatedProperty"), comment.clone()),
                Triple::new(bn(node), owl("annotatedTarget"), OxLiteral::new_simple_literal("branch")),
                Triple::new(bn(node), label.clone(), OxLiteral::new_simple_literal(nested)),
            ]);
        }
        for (first, expect) in [("one", "first"), ("two", "second")] {
            let b = Build::new_rc();
            let rest = if first == "one" { "two" } else { "one" };
            let order = StatementOrder {
                axiom_nodes: vec!["ax".into()],
                annotation_nodes: vec![first.into(), rest.into()],
                ..Default::default()
            };
            let (ont, _) = read_statements::<RcStr, RcAnnotatedComponent, _>(
                statements.clone(),
                ParserConfiguration::new(&b),
                &order,
            )
            .unwrap();
            let ont: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = ont.into();
            let subs: Vec<_> = ont.i().component_for_kind(ComponentKind::SubClassOf).collect();
            let anns: Vec<_> = subs[0].ann.iter().collect();
            assert_eq!(anns.len(), 1, "{anns:#?}");
            let nested: Vec<_> = anns[0].ann.iter().map(|n| n.av.clone()).collect();
            assert_eq!(nested, vec![AnnotationValue::Literal(Literal::Simple { literal: expect.into() })]);
        }
    }

    #[test]
    fn a_chain_stated_twice_and_reified_once_is_one_annotated_axiom() {
        // Each statement of the chain is a list of its own, and the block
        // reifying it a third: one axiom, annotated.
        let chain = r#"<owl:propertyChainAxiom rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#p"/>
            <rdf:Description rdf:about="http://example.com/x#q"/>
        </owl:propertyChainAxiom>"#;
        let (ont, incomplete) = read_owl1(&format!(
            r#"<owl:ObjectProperty rdf:about="http://example.com/x#p"/>
    <owl:ObjectProperty rdf:about="http://example.com/x#q"/>
    <owl:ObjectProperty rdf:about="http://example.com/x#r">
        {chain}
        {chain}
    </owl:ObjectProperty>
    <owl:Axiom>
        <owl:annotatedSource rdf:resource="http://example.com/x#r"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2002/07/owl#propertyChainAxiom"/>
        <owl:annotatedTarget rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#p"/>
            <rdf:Description rdf:about="http://example.com/x#q"/>
        </owl:annotatedTarget>
        <rdfs:comment>c</rdfs:comment>
    </owl:Axiom>"#
        ));
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let chains: Vec<_> = ont.i().component_for_kind(ComponentKind::SubObjectPropertyOf).collect();
        assert_eq!(chains.len(), 1, "{chains:#?}");
        assert_eq!(chains[0].ann.len(), 1, "{chains:#?}");
    }

    #[test]
    fn owl1_named_class_connectives_are_equivalences() {
        // Table 18: a connective or enumeration attached to a named class
        // states an equivalence.
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#B"/>
    <owl:Class rdf:about="http://example.com/x#C"/>
    <owl:Class rdf:about="http://example.com/x#A">
        <owl:unionOf rdf:parseType="Collection">
            <owl:Class rdf:about="http://example.com/x#B"/>
            <owl:Class rdf:about="http://example.com/x#C"/>
        </owl:unionOf>
    </owl:Class>
    <owl:Class rdf:about="http://example.com/x#D">
        <owl:complementOf rdf:resource="http://example.com/x#B"/>
    </owl:Class>
    <owl:Thing rdf:about="http://example.com/x#a"/>
    <owl:Thing rdf:about="http://example.com/x#b"/>
    <owl:Class rdf:about="http://example.com/x#E">
        <owl:oneOf rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#a"/>
            <rdf:Description rdf:about="http://example.com/x#b"/>
        </owl:oneOf>
    </owl:Class>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        assert_eq!(ont.i().equivalent_class().count(), 3);
        assert!(ont.i().equivalent_class().any(|e| {
            matches!(e.0.as_slice(), [_, ClassExpression::ObjectUnionOf(v)] if v.len() == 2)
        }));
        assert!(ont.i().equivalent_class().any(|e| {
            matches!(e.0.as_slice(), [_, ClassExpression::ObjectComplementOf(_)])
        }));
        assert!(ont.i().equivalent_class().any(|e| {
            matches!(e.0.as_slice(), [_, ClassExpression::ObjectOneOf(v)] if v.len() == 2)
        }));
    }

    #[test]
    fn connectives_hold_the_members_their_lists_name() {
        // An empty or one-member connective is that connective, a named
        // class's (Table 18) or an anonymous one alike; an empty list is
        // `rdf:nil`.
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#B"/>
    <owl:Class rdf:about="http://example.com/x#EmptyI"><owl:intersectionOf rdf:parseType="Collection"/></owl:Class>
    <owl:Class rdf:about="http://example.com/x#EmptyO"><owl:oneOf rdf:parseType="Collection"/></owl:Class>
    <owl:Class rdf:about="http://example.com/x#OneU">
        <owl:unionOf rdf:parseType="Collection"><owl:Class rdf:about="http://example.com/x#B"/></owl:unionOf>
    </owl:Class>
    <owl:Class rdf:about="http://example.com/x#Sub">
        <rdfs:subClassOf><owl:Class><owl:unionOf rdf:parseType="Collection"/></owl:Class></rdfs:subClassOf>
    </owl:Class>
    <owl:DatatypeProperty rdf:about="http://example.com/x#d">
        <rdfs:range><rdfs:Datatype><owl:oneOf rdf:parseType="Collection"/></rdfs:Datatype></rdfs:range>
    </owl:DatatypeProperty>
    <owl:DatatypeProperty rdf:about="http://example.com/x#e">
        <rdfs:range><rdfs:Datatype><owl:intersectionOf rdf:parseType="Collection"/></rdfs:Datatype></rdfs:range>
    </owl:DatatypeProperty>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let defines = |ce: &dyn Fn(&ClassExpression<RcStr>) -> bool| ont.i().equivalent_class().any(|e| e.0.iter().any(ce));
        assert!(defines(&|ce| *ce == ClassExpression::ObjectIntersectionOf(vec![])));
        assert!(defines(&|ce| *ce == ClassExpression::ObjectOneOf(vec![])));
        assert!(defines(&|ce| matches!(ce, ClassExpression::ObjectUnionOf(v) if v.len() == 1)));
        assert!(ont.i().sub_class_of().any(|sc| sc.sup == ClassExpression::ObjectUnionOf(vec![])));
        let ranges: Vec<&DataRange<RcStr>> = ont.i().data_property_range().map(|r| &r.dr).collect();
        assert!(ranges.contains(&&DataRange::DataOneOf(vec![])), "{ranges:?}");
        assert!(ranges.contains(&&DataRange::DataIntersectionOf(vec![])), "{ranges:?}");
    }

    #[test]
    fn every_ontology_header_annotates_and_imports() {
        // Every node typed `owl:Ontology`, named or blank, states the
        // ontology's annotations (with their own annotations) and imports.
        // The first header the document states names the ontology, and when
        // that one is a blank node the ontology is anonymous.
        let doc = |first: &str, second: &str, reified: &str| {
            format!(
                r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
         xmlns:owl="http://www.w3.org/2002/07/owl#">
    <owl:Ontology {first}>
        <rdfs:comment>first</rdfs:comment>
        <owl:imports rdf:resource="http://example.com/one"/>
        <owl:versionIRI rdf:resource="http://example.com/v1"/>
    </owl:Ontology>
    <owl:Ontology {second}>
        <rdfs:comment>second</rdfs:comment>
        <owl:imports rdf:resource="http://example.com/two"/>
        <owl:versionIRI rdf:resource="http://example.com/v2"/>
    </owl:Ontology>
    <owl:Annotation>
        <owl:annotatedSource {reified}/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#comment"/>
        <owl:annotatedTarget>first</owl:annotatedTarget>
        <rdfs:label>nested</rdfs:label>
    </owl:Annotation>
    <owl:Class rdf:about="http://example.com/x#A"/>
</rdf:RDF>"#
            )
        };
        for (first, second, iri, viri) in [
            ("rdf:nodeID=\"o\"", "", None, None),
            ("rdf:nodeID=\"o\"", "rdf:about=\"http://example.com/n2\"", None, None),
            ("rdf:about=\"http://example.com/n1\"", "rdf:nodeID=\"o\"", Some("http://example.com/n1"), Some("http://example.com/v1")),
            (
                "rdf:about=\"http://example.com/n1\"",
                "rdf:about=\"http://example.com/n2\"",
                Some("http://example.com/n1"),
                Some("http://example.com/v1"),
            ),
        ] {
            let doc = doc(first, second, &first.replace("about", "resource"));
            let (ont, incomplete) =
                read::<RcStr, RcAnnotatedComponent, _, _>(&mut doc.as_bytes(), Default::default()).unwrap();
            let ont: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = ont.into();
            let id = ont.i().ontology_id().next().cloned().unwrap_or_default();
            assert_eq!(id.iri.as_ref().map(|i| i.as_ref()), iri, "{first} {second}");
            assert_eq!(id.viri.as_ref().map(|i| i.as_ref()), viri, "{first} {second}");
            let anns: Vec<_> = ont.i().ontology_annotation().collect();
            assert_eq!(anns.len(), 2, "{first} {second}: {anns:?}");
            // The annotation the reification names is itself annotated,
            // whichever header it was stated of.
            assert!(anns.iter().any(|a| a.0.ann.len() == 1), "{first} {second}: {anns:?}");
            let imports: BTreeSet<_> = ont.i().import().map(|i| i.0.as_ref().to_string()).collect();
            assert_eq!(imports, BTreeSet::from(["http://example.com/one".into(), "http://example.com/two".into()]));
            // Nothing else: no header node read as an individual, nothing left over.
            assert_eq!(ont.i().iter().count(), 6, "{first} {second}: {:#?}", ont.i().iter().collect::<Vec<_>>());
            assert!(incomplete.is_complete(), "{first} {second}: {incomplete:?}");
        }
    }

    #[test]
    fn anonymous_individual_from_a_single_type_triple() {
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#C"/>
    <rdf:Description>
        <rdf:type rdf:resource="http://example.com/x#C"/>
    </rdf:Description>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let assertions: Vec<_> = ont.i().class_assertion().collect();
        assert_eq!(assertions.len(), 1);
        assert!(matches!(assertions[0].i, Individual::Anonymous(_)));
    }

    #[test]
    fn property_assertion_with_an_anonymous_object() {
        // Table 16: a declared object property asserts; any other predicate
        // annotates, as it does with an IRI object.
        let (ont, incomplete) = read_owl1(
            r#"<owl:ObjectProperty rdf:about="http://example.com/x#p"/>
    <owl:AnnotationProperty rdf:about="http://example.com/x#note"/>
    <owl:Thing rdf:about="http://example.com/x#a">
        <ex:p><rdf:Description rdf:nodeID="x"/></ex:p>
        <ex:note><rdf:Description rdf:nodeID="y"/></ex:note>
    </owl:Thing>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let opa: Vec<_> = ont.i().object_property_assertion().collect();
        assert_eq!(opa.len(), 1);
        assert!(matches!(opa[0].to, Individual::Anonymous(_)));
        assert!(ont.i().annotation_assertion().any(|a| {
            matches!(a.ann.av, AnnotationValue::AnonymousIndividual(_))
        }));
    }

    #[test]
    fn anonymous_individual_assertions_share_one_individual() {
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#C"/>
    <owl:ObjectProperty rdf:about="http://example.com/x#p"/>
    <owl:DatatypeProperty rdf:about="http://example.com/x#d"/>
    <owl:NamedIndividual rdf:about="http://example.com/x#a"/>
    <owl:NamedIndividual rdf:about="http://example.com/x#b"/>
    <rdf:Description rdf:nodeID="x">
        <rdf:type rdf:resource="http://example.com/x#C"/>
        <ex:p rdf:resource="http://example.com/x#b"/>
        <ex:d>1</ex:d>
        <owl:sameAs rdf:resource="http://example.com/x#a"/>
    </rdf:Description>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let typed = ont.i().class_assertion().next().unwrap().i.clone();
        assert!(matches!(typed, Individual::Anonymous(_)));
        assert_eq!(ont.i().object_property_assertion().next().unwrap().from, typed);
        assert_eq!(ont.i().data_property_assertion().next().unwrap().from, typed);
        assert!(ont.i().same_individual().next().unwrap().0.contains(&typed));
    }

    #[test]
    fn a_statement_typing_a_string_keeps_its_type_and_its_reification_compares_untyped() {
        // The statement leaves the label untyped and the block naming it types
        // it `xsd:string`: one literal, so one axiom, annotated, with the
        // block's literal. A typed statement no block names stays typed.
        use oxrdf::Literal as OxLiteral;
        let nn = |s: &str| NamedNode::new_unchecked(s);
        let ax = || BlankNode::new_unchecked("ax");
        let (a, c) = (nn("http://example.com/A"), nn("http://example.com/C"));
        let label = nn("http://www.w3.org/2000/01/rdf-schema#label");
        let owl = |l: &str| nn(&format!("http://www.w3.org/2002/07/owl#{l}"));
        let statements = vec![
            Triple::new(a.clone(), label.clone(), OxLiteral::new_simple_literal("x")),
            Triple::new(ax(), nn("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"), owl("Axiom")),
            Triple::new(ax(), owl("annotatedSource"), a.clone()),
            Triple::new(ax(), owl("annotatedProperty"), label.clone()),
            Triple::new(ax(), owl("annotatedTarget"), OxLiteral::new_simple_literal("x")),
            Triple::new(ax(), nn("http://www.w3.org/2000/01/rdf-schema#comment"), OxLiteral::new_simple_literal("c")),
            Triple::new(c.clone(), label.clone(), OxLiteral::new_simple_literal("y")),
        ];
        let typed: std::collections::HashSet<usize> = [4, 6].into();
        let b = Build::new_rc();
        let order = StatementOrder { typed_strings: typed, ..Default::default() };
        let (ont, incomplete) =
            read_statements::<RcStr, RcAnnotatedComponent, _>(statements, ParserConfiguration::new(&b), &order)
                .unwrap();
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let ont: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = ont.into();
        let mut labels: Vec<_> = ont.i().component_for_kind(ComponentKind::AnnotationAssertion).collect();
        labels.sort();
        let xsd_string = b.iri("http://www.w3.org/2001/XMLSchema#string");
        let expect = |s: &str, l: &str| -> (IRI<RcStr>, Literal<RcStr>) {
            (b.iri(s), Literal::Datatype { literal: l.into(), datatype_iri: xsd_string.clone() })
        };
        let got: Vec<_> = labels
            .iter()
            .map(|ac| match &ac.component {
                Component::AnnotationAssertion(AnnotationAssertion {
                    subject: AnnotationSubject::IRI(s),
                    ann: Annotation { av: AnnotationValue::Literal(l), .. },
                }) => ((s.clone(), l.clone()), ac.ann.len()),
                c => panic!("{c:?}"),
            })
            .collect();
        assert_eq!(got, vec![(expect("http://example.com/A", "x"), 1), (expect("http://example.com/C", "y"), 0)]);
    }

    #[test]
    fn an_anonymous_equivalence_reified_with_a_copied_target_keeps_its_annotation() {
        // The axiom's own node states the equivalence, inside the reification
        // that names it, and the reification's target is a copy of the
        // expression the node is equivalent to: one annotated axiom.
        let doc = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
         xmlns:owl="http://www.w3.org/2002/07/owl#">
    <owl:Ontology rdf:about="http://example.com/e"/>
    <owl:ObjectProperty rdf:about="http://example.com/e#p"/>
    <owl:ObjectProperty rdf:about="http://example.com/e#q"/>
    <owl:Class rdf:about="http://example.com/e#A"/>
    <owl:Axiom>
        <owl:annotatedSource>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://example.com/e#p"/>
                <owl:someValuesFrom rdf:resource="http://example.com/e#A"/>
                <owl:equivalentClass>
                    <owl:Restriction>
                        <owl:onProperty rdf:resource="http://example.com/e#q"/>
                        <owl:someValuesFrom rdf:resource="http://example.com/e#A"/>
                    </owl:Restriction>
                </owl:equivalentClass>
            </owl:Restriction>
        </owl:annotatedSource>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2002/07/owl#equivalentClass"/>
        <owl:annotatedTarget>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://example.com/e#q"/>
                <owl:someValuesFrom rdf:resource="http://example.com/e#A"/>
            </owl:Restriction>
        </owl:annotatedTarget>
        <rdfs:comment>anonymous pair</rdfs:comment>
    </owl:Axiom>
</rdf:RDF>"#;
        let (ont, incomplete) =
            read::<RcStr, RcAnnotatedComponent, _, _>(&mut doc.as_bytes(), Default::default()).unwrap();
        let ont: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = ont.into();
        let equivalences: Vec<_> = ont.i().component_for_kind(ComponentKind::EquivalentClasses).collect();
        assert_eq!(equivalences.len(), 1, "{equivalences:#?}");
        assert_eq!(equivalences[0].ann.len(), 1, "{equivalences:#?}");
        assert!(incomplete.is_complete(), "{incomplete:?}");
    }

    #[test]
    fn a_reified_statement_of_an_anonymous_individual_needs_no_stated_triple() {
        // An `owl:Axiom` block means its axiom whether or not the document
        // states the triple it names, an anonymous individual's included: a
        // difference whose object is a node stating nothing, and a type of a
        // node whose other statements describe the same individual.
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#C"/>
    <owl:NamedIndividual rdf:about="http://example.com/x#i"/>
    <rdf:Description rdf:nodeID="t"/>
    <rdf:Description rdf:nodeID="s">
        <rdfs:label>s</rdfs:label>
    </rdf:Description>
    <owl:Axiom>
        <owl:annotatedSource rdf:resource="http://example.com/x#i"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2002/07/owl#differentFrom"/>
        <owl:annotatedTarget rdf:nodeID="t"/>
        <rdfs:comment>different</rdfs:comment>
    </owl:Axiom>
    <owl:Axiom>
        <owl:annotatedSource rdf:nodeID="s"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#type"/>
        <owl:annotatedTarget rdf:resource="http://example.com/x#C"/>
        <rdfs:comment>typed</rdfs:comment>
    </owl:Axiom>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let anonymous = |i: &Individual<RcStr>| matches!(i, Individual::Anonymous(_));
        let annotated: Vec<_> = ont.i().iter().filter(|ac| !ac.ann.is_empty()).collect();
        assert_eq!(annotated.len(), 2, "{annotated:?}");
        assert!(annotated.iter().any(|ac| matches!(&ac.component,
            Component::DifferentIndividuals(d) if d.0.len() == 2 && d.0.iter().any(anonymous))));
        let typed = annotated
            .iter()
            .find_map(|ac| match &ac.component {
                Component::ClassAssertion(ca) => Some(ca.i.clone()),
                _ => None,
            })
            .expect("the class assertion");
        let Individual::Anonymous(typed) = typed else { panic!("{typed:?}") };
        // The node's own statement stays its individual's.
        let subject: AnnotationSubject<RcStr> = typed.into();
        assert!(ont.i().iter().any(|ac| matches!(&ac.component,
            Component::AnnotationAssertion(a) if a.subject == subject)));
    }

    #[test]
    fn anonymous_individuals_wherever_an_axiom_names_one() {
        // A has-value's value, an enumeration's member, the other member of a
        // sameness and of a difference, a negative assertion's source and
        // target: each a blank node, each an anonymous individual.
        let (ont, incomplete) = read_owl1(
            r#"<owl:ObjectProperty rdf:about="http://example.com/x#p"/>
    <owl:Class rdf:about="http://example.com/x#C"/>
    <owl:Class rdf:about="http://example.com/x#A">
        <rdfs:subClassOf>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://example.com/x#p"/>
                <owl:hasValue><rdf:Description rdf:nodeID="x"/></owl:hasValue>
            </owl:Restriction>
        </rdfs:subClassOf>
    </owl:Class>
    <owl:Class rdf:about="http://example.com/x#B">
        <rdfs:subClassOf>
            <owl:Class>
                <owl:oneOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://example.com/x#i"/>
                    <rdf:Description/>
                </owl:oneOf>
            </owl:Class>
        </rdfs:subClassOf>
    </owl:Class>
    <owl:NamedIndividual rdf:about="http://example.com/x#i">
        <owl:sameAs><rdf:Description/></owl:sameAs>
        <owl:differentFrom><rdf:Description/></owl:differentFrom>
    </owl:NamedIndividual>
    <owl:NegativePropertyAssertion>
        <owl:sourceIndividual><rdf:Description/></owl:sourceIndividual>
        <owl:assertionProperty rdf:resource="http://example.com/x#p"/>
        <owl:targetIndividual><rdf:Description/></owl:targetIndividual>
    </owl:NegativePropertyAssertion>
    <owl:AllDifferent>
        <owl:distinctMembers rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#i"/>
            <rdf:Description/>
        </owl:distinctMembers>
    </owl:AllDifferent>
    <rdf:Description rdf:nodeID="x">
        <rdf:type rdf:resource="http://example.com/x#C"/>
    </rdf:Description>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let anonymous = |i: &Individual<RcStr>| matches!(i, Individual::Anonymous(_));
        let value = ont
            .i()
            .sub_class_of()
            .find_map(|sc| match &sc.sup {
                ClassExpression::ObjectHasValue { i, .. } => Some(i.clone()),
                _ => None,
            })
            .expect("the has-value");
        assert!(anonymous(&value));
        assert!(ont.i().class_assertion().any(|ca| ca.i == value));
        assert!(ont.i().sub_class_of().any(|sc| {
            matches!(&sc.sup, ClassExpression::ObjectOneOf(v) if v.len() == 2 && v.iter().any(anonymous))
        }));
        assert!(ont.i().same_individual().any(|s| s.0.iter().any(anonymous)));
        assert_eq!(ont.i().different_individuals().filter(|d| d.0.iter().any(anonymous)).count(), 2);
        let npa: Vec<_> = ont.i().negative_object_property_assertion().collect();
        assert_eq!(npa.len(), 1);
        assert!(anonymous(&npa[0].from) && anonymous(&npa[0].to));
    }

    #[test]
    fn anonymous_individuals_are_numbered_in_document_order() {
        // Annotation values named by blank nodes take their ids in the order
        // the document states them, whatever labels the parser gives the nodes.
        let mut body = String::new();
        for (sub, sup) in [("A", "B"), ("B", "C"), ("C", "D"), ("D", "E")] {
            body.push_str(&format!(
                r#"    <owl:Class rdf:about="http://example.com/x#{sub}">
        <rdfs:subClassOf rdf:resource="http://example.com/x#{sup}"/>
    </owl:Class>
    <owl:Axiom>
        <owl:annotatedSource rdf:resource="http://example.com/x#{sub}"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#subClassOf"/>
        <owl:annotatedTarget rdf:resource="http://example.com/x#{sup}"/>
        <rdfs:seeAlso><rdf:Description><rdf:type rdf:resource="http://example.com/x#{sub}"/></rdf:Description></rdfs:seeAlso>
    </owl:Axiom>
"#
            ));
        }
        let b = Build::new_rc();
        b.set_bnode_base(100);
        let (ont, incomplete) = read::<RcStr, RcAnnotatedComponent, _, _>(
            &mut owl1_doc(&body).as_bytes(),
            ParserConfiguration::new(&b).into(),
        )
        .unwrap();
        let ont: ComponentMappedOntology<RcStr, RcAnnotatedComponent> = ont.into();
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let mut ids: Vec<(String, u64)> = ont
            .i()
            .sub_class_of()
            .map(|sc| {
                let ClassExpression::Class(sub) = &sc.sub else { panic!("{sc:?}") };
                let ann = ont
                    .i()
                    .iter()
                    .find(|ac| matches!(&ac.component, Component::SubClassOf(x) if x == sc))
                    .unwrap()
                    .ann
                    .iter()
                    .next()
                    .unwrap()
                    .clone();
                let AnnotationValue::AnonymousIndividual(a) = ann.av else { panic!("{ann:?}") };
                let n = a.0.strip_prefix("genid").unwrap().parse().unwrap();
                (sub.0.to_string(), n)
            })
            .collect();
        ids.sort();
        let numbers: Vec<u64> = ids.iter().map(|(_, n)| *n).collect();
        let mut sorted = numbers.clone();
        sorted.sort();
        assert_eq!(numbers, sorted, "{ids:?}");
    }

    #[test]
    fn annotations_of_annotations_to_any_depth() {
        // Three levels on an assertion, two on the ontology's annotation, and
        // two on axioms stated on a node of their own.
        let (ont, incomplete) = read_owl1(
            r#"<rdf:Description rdf:about="http://example.com/x">
        <rdfs:comment>one</rdfs:comment>
    </rdf:Description>
    <owl:Annotation>
        <owl:annotatedSource rdf:resource="http://example.com/x"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#comment"/>
        <owl:annotatedTarget>one</owl:annotatedTarget>
        <rdfs:comment>two</rdfs:comment>
    </owl:Annotation>
    <owl:ObjectProperty rdf:about="http://example.com/x#q"/>
    <owl:NamedIndividual rdf:about="http://example.com/x#i">
        <ex:q rdf:resource="http://example.com/x#j"/>
    </owl:NamedIndividual>
    <owl:NamedIndividual rdf:about="http://example.com/x#j"/>
    <owl:Annotation>
        <owl:annotatedSource>
            <owl:Annotation>
                <owl:annotatedSource rdf:nodeID="a"/>
                <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#comment"/>
                <owl:annotatedTarget>one</owl:annotatedTarget>
                <rdfs:comment>two</rdfs:comment>
            </owl:Annotation>
        </owl:annotatedSource>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#comment"/>
        <owl:annotatedTarget>two</owl:annotatedTarget>
        <rdfs:comment>three</rdfs:comment>
    </owl:Annotation>
    <owl:Axiom rdf:nodeID="a">
        <owl:annotatedSource rdf:resource="http://example.com/x#i"/>
        <owl:annotatedProperty rdf:resource="http://example.com/x#q"/>
        <owl:annotatedTarget rdf:resource="http://example.com/x#j"/>
        <rdfs:comment>one</rdfs:comment>
    </owl:Axiom>
    <rdf:Description rdf:nodeID="n">
        <rdfs:comment>negative one</rdfs:comment>
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#NegativePropertyAssertion"/>
        <owl:sourceIndividual rdf:resource="http://example.com/x#i"/>
        <owl:assertionProperty rdf:resource="http://example.com/x#q"/>
        <owl:targetIndividual rdf:resource="http://example.com/x#j"/>
    </rdf:Description>
    <owl:Annotation>
        <owl:annotatedSource rdf:nodeID="n"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#comment"/>
        <owl:annotatedTarget>negative one</owl:annotatedTarget>
        <rdfs:comment>negative two</rdfs:comment>
    </owl:Annotation>
    <owl:ObjectProperty rdf:about="http://example.com/x#p"/>
    <owl:ObjectProperty rdf:about="http://example.com/x#r"/>
    <rdf:Description>
        <rdfs:comment>disjoint</rdfs:comment>
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#AllDisjointProperties"/>
        <owl:members rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#p"/>
            <rdf:Description rdf:about="http://example.com/x#q"/>
            <rdf:Description>
                <owl:inverseOf rdf:resource="http://example.com/x#r"/>
            </rdf:Description>
        </owl:members>
    </rdf:Description>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let comment = |a: &Annotation<RcStr>| match &a.av {
            AnnotationValue::Literal(Literal::Simple { literal }) => literal.clone(),
            other => panic!("{other:?}"),
        };
        // Each level of `ann` holds one annotation; the comments down the chain.
        let chain = |a: &Annotation<RcStr>| {
            let mut out = vec![comment(a)];
            let mut at = a.clone();
            while let Some(next) = at.ann.iter().next().cloned() {
                out.push(comment(&next));
                at = next;
            }
            out
        };
        let opa = ont
            .i()
            .iter()
            .find(|ac| matches!(ac.component, Component::ObjectPropertyAssertion(_)))
            .unwrap();
        assert_eq!(chain(opa.ann.iter().next().unwrap()), ["one", "two", "three"]);
        let ont_ann = ont
            .i()
            .iter()
            .find_map(|ac| match &ac.component {
                Component::OntologyAnnotation(OntologyAnnotation(a)) => Some(a.clone()),
                _ => None,
            })
            .unwrap();
        assert_eq!(chain(&ont_ann), ["one", "two"]);
        let npa = ont
            .i()
            .iter()
            .find(|ac| matches!(ac.component, Component::NegativeObjectPropertyAssertion(_)))
            .unwrap();
        assert_eq!(chain(npa.ann.iter().next().unwrap()), ["negative one", "negative two"]);
        let disjoint = ont
            .i()
            .iter()
            .find(|ac| matches!(&ac.component, Component::DisjointObjectProperties(d) if d.0.len() == 3))
            .unwrap();
        assert_eq!(chain(disjoint.ann.iter().next().unwrap()), ["disjoint"]);
    }

    #[test]
    fn property_chains_under_and_over_an_inverse() {
        // A chain stated of an inverse's node; and an annotated chain with an
        // inverse member whose reification names a list of its own.
        let (ont, incomplete) = read_owl1(
            r#"<owl:ObjectProperty rdf:about="http://example.com/x#p"/>
    <owl:ObjectProperty rdf:about="http://example.com/x#q"/>
    <owl:ObjectProperty rdf:about="http://example.com/x#r"/>
    <rdf:Description>
        <owl:inverseOf rdf:resource="http://example.com/x#r"/>
        <owl:propertyChainAxiom rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#p"/>
            <rdf:Description rdf:about="http://example.com/x#q"/>
        </owl:propertyChainAxiom>
    </rdf:Description>
    <owl:ObjectProperty rdf:about="http://example.com/x#s">
        <owl:propertyChainAxiom rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#p"/>
            <rdf:Description>
                <owl:inverseOf rdf:resource="http://example.com/x#q"/>
            </rdf:Description>
        </owl:propertyChainAxiom>
    </owl:ObjectProperty>
    <owl:Axiom>
        <owl:annotatedSource rdf:resource="http://example.com/x#s"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2002/07/owl#propertyChainAxiom"/>
        <owl:annotatedTarget rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#p"/>
            <rdf:Description>
                <owl:inverseOf rdf:resource="http://example.com/x#q"/>
            </rdf:Description>
        </owl:annotatedTarget>
        <rdfs:comment>chain</rdfs:comment>
    </owl:Axiom>"#,
        );
        // The reification's own copy of the list only names the chain; it is
        // left over, as any copy is.
        assert!(incomplete.simple.is_empty() && incomplete.bnode.is_empty(), "{incomplete:?}");
        let chains: Vec<_> = ont
            .i()
            .iter()
            .filter(|ac| {
                matches!(&ac.component, Component::SubObjectPropertyOf(s)
                    if matches!(s.sub, SubObjectPropertyExpression::ObjectPropertyChain(_)))
            })
            .collect();
        assert_eq!(chains.len(), 2, "{chains:?}");
        assert!(chains.iter().any(|ac| matches!(&ac.component,
            Component::SubObjectPropertyOf(s) if matches!(s.sup, ObjectPropertyExpression::InverseObjectProperty(_)))));
        assert!(chains.iter().any(|ac| !ac.ann.is_empty()), "{chains:?}");
    }

    #[test]
    fn owl1_enumerated_data_range() {
        // Table 14, with the list cells' redundant rdf:List types dropped by
        // Table 5.
        let (ont, incomplete) = read_owl1(
            r#"<owl:DatatypeProperty rdf:about="http://example.com/x#d">
        <rdfs:range>
            <owl:DataRange>
                <owl:oneOf>
                    <rdf:List>
                        <rdf:first rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">1</rdf:first>
                        <rdf:rest>
                            <rdf:List>
                                <rdf:first rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">2</rdf:first>
                                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
                            </rdf:List>
                        </rdf:rest>
                    </rdf:List>
                </owl:oneOf>
            </owl:DataRange>
        </rdfs:range>
    </owl:DatatypeProperty>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let ranges: Vec<_> = ont.i().data_property_range().collect();
        assert_eq!(ranges.len(), 1);
        assert!(matches!(&ranges[0].dr, DataRange::DataOneOf(v) if v.len() == 2));
    }

    #[test]
    fn standalone_class_expression_is_complete() {
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#B"/>
    <owl:Class rdf:about="http://example.com/x#C"/>
    <owl:Class>
        <owl:unionOf rdf:parseType="Collection">
            <owl:Class rdf:about="http://example.com/x#B"/>
            <owl:Class rdf:about="http://example.com/x#C"/>
        </owl:unionOf>
    </owl:Class>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        assert_eq!(ont.i().equivalent_class().count(), 0);
    }

    #[test]
    fn referenced_but_unused_class_expression_is_reported() {
        let (_ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#B"/>
    <owl:Class rdf:about="http://example.com/x#C"/>
    <owl:Class rdf:about="http://example.com/x#A">
        <ex:related>
            <owl:Class>
                <owl:unionOf rdf:parseType="Collection">
                    <owl:Class rdf:about="http://example.com/x#B"/>
                    <owl:Class rdf:about="http://example.com/x#C"/>
                </owl:unionOf>
            </owl:Class>
        </ex:related>
    </owl:Class>"#,
        );
        assert!(!incomplete.is_complete());
        assert_eq!(incomplete.class_expression.len(), 1);
    }

    #[test]
    fn enumeration_without_its_class_type() {
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#A">
        <rdfs:subClassOf>
            <rdf:Description>
                <owl:oneOf rdf:parseType="Collection">
                    <rdf:Description rdf:about="http://example.com/x#a"/>
                    <rdf:Description rdf:about="http://example.com/x#b"/>
                </owl:oneOf>
            </rdf:Description>
        </rdfs:subClassOf>
    </owl:Class>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let sub: Vec<_> = ont.i().sub_class_of().collect();
        assert_eq!(sub.len(), 1);
        assert!(matches!(&sub[0].sup, ClassExpression::ObjectOneOf(v) if v.len() == 2));
    }

    #[test]
    fn a_disjoint_union_of_an_empty_list_is_read() {
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#U">
        <owl:disjointUnionOf rdf:parseType="Collection"/>
    </owl:Class>
    <owl:Class rdf:about="http://example.com/x#V">
        <owl:disjointUnionOf rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.com/x#B"/>
        </owl:disjointUnionOf>
    </owl:Class>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let mut unions: Vec<(String, usize)> = ont
            .iter()
            .filter_map(|ac| match &ac.component {
                Component::DisjointUnion(DisjointUnion(c, members)) => Some((c.0.to_string(), members.len())),
                _ => None,
            })
            .collect();
        unions.sort();
        assert_eq!(
            unions,
            [("http://example.com/x#U".to_string(), 0), ("http://example.com/x#V".to_string(), 1)]
        );
    }

    #[test]
    fn owl1_redundant_types_are_dropped() {
        // Table 5: rdfs:Class beside owl:Class, owl:Class beside
        // owl:Restriction and rdf:Property beside owl:ObjectProperty are
        // dropped, so nothing is read as a class assertion and the restriction
        // keeps its pattern. Table 6: a transitive property is an object
        // property.
        let (ont, incomplete) = read_owl1(
            r#"<rdfs:Class rdf:about="http://example.com/x#A">
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Class"/>
    </rdfs:Class>
    <rdf:Property rdf:about="http://example.com/x#p">
        <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#ObjectProperty"/>
    </rdf:Property>
    <owl:Class rdf:about="http://example.com/x#B">
        <rdfs:subClassOf>
            <owl:Restriction>
                <rdf:type rdf:resource="http://www.w3.org/2002/07/owl#Class"/>
                <owl:onProperty rdf:resource="http://example.com/x#p"/>
                <owl:someValuesFrom rdf:resource="http://example.com/x#A"/>
            </owl:Restriction>
        </rdfs:subClassOf>
    </owl:Class>
    <owl:TransitiveProperty rdf:about="http://example.com/x#t"/>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        assert_eq!(ont.i().class_assertion().count(), 0);
        assert_eq!(ont.i().sub_class_of().count(), 1);
        assert_eq!(ont.i().declare_object_property().count(), 2);
        assert_eq!(ont.i().transitive_object_property().count(), 1);
    }

    #[test]
    fn undeclared_property_in_unqualified_cardinality_reads_as_object() {
        let (ont, incomplete) = read_owl1(
            r#"<owl:Class rdf:about="http://example.com/x#B">
        <rdfs:subClassOf>
            <owl:Restriction>
                <owl:onProperty rdf:resource="http://example.com/x#q"/>
                <owl:minCardinality rdf:datatype="http://www.w3.org/2001/XMLSchema#nonNegativeInteger">1</owl:minCardinality>
            </owl:Restriction>
        </rdfs:subClassOf>
    </owl:Class>"#,
        );
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let sub: Vec<_> = ont.i().sub_class_of().collect();
        assert_eq!(sub.len(), 1);
        assert!(matches!(sub[0].sup, ClassExpression::ObjectMinCardinality { n: 1, .. }));
    }

    #[test]
    fn error_on_some_broken() {
        // Check error handling on (some a c) where a is an annotation property
        let err = read::<RcStr, RcAnnotatedComponent, _, _>(
            &mut slurp_rdfont("manual/some-broken").as_bytes(),
            Default::default(),
        )
        .unwrap_err();

        assert!(matches! {err, HornedError::ValidityError(_,_)})
    }

    #[test]
    fn error_not_panic_on_malformed_rdf_xml() {
        // Issue #205: malformed RDF/XML (here, an invalid duplicate XML
        // attribute -- oxrdfio's underlying `quick-xml` parser rejects
        // this) used to panic via an unchecked `unwrap()` on the
        // underlying oxrdfio parser's error. It should be a recoverable
        // `HornedError` instead, regardless of what produced the
        // malformed input.
        let xml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
     xmlns:owl="http://www.w3.org/2002/07/owl#">
    <owl:Ontology rdf:about="http://www.example.com/iri"
                  owl:versionInfo="first" owl:versionInfo="second"/>
</rdf:RDF>"#;

        let err =
            read::<RcStr, RcAnnotatedComponent, _, _>(&mut xml.as_bytes(), Default::default())
                .unwrap_err();

        assert!(matches! {err, HornedError::ParserError(_,_)})
    }

    fn read_from_format<R: BufRead>(bufread: &mut R, format: oxrdfio::RdfFormat) {
        let (ont, incomp): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
            OntologyParser::from_bufread_with_format(bufread, Default::default(), format)
                .unwrap()
                .parse()
                .unwrap();

        dbg!(ont, incomp);
    }

    /// In lax mode, as OWLAPI reads them: a blank node that does not say what
    /// it is is the expression its predicates describe; an
    /// `owl:someValuesFrom`, `owl:allValuesFrom` or `owl:hasValue` restriction,
    /// and an `rdfs:range`, take their kind from what fills them rather than
    /// from the property's declaration; and an `owl:oneOf` class leaves out
    /// its literal members.
    #[test]
    fn lax_reading_types_expressions_by_what_fills_them() {
        use crate::io::ofn::writer::AsFunctional;
        let ttl = r#"@prefix : <http://example.org/> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
<http://example.org/o> a owl:Ontology .
:C a owl:Class .
:DT a rdfs:Datatype .
:d a owl:DatatypeProperty .
:p a owl:ObjectProperty .
:q a owl:ObjectProperty .
:i a owl:NamedIndividual .
:A1 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:someValuesFrom [ owl:oneOf ( "x" "y" ) ] ] .
:A2 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:allValuesFrom [ owl:unionOf ( xsd:string xsd:integer ) ] ] .
:A3 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:someValuesFrom xsd:string ] .
:A4 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :p ; owl:someValuesFrom xsd:string ] .
:A5 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:hasValue :i ] .
:A6 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :p ; owl:hasValue "lit" ] .
:A7 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:someValuesFrom :C ] .
:A8 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:someValuesFrom :U ] .
:A9 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:someValuesFrom :DT ] .
:A10 rdfs:subClassOf [ owl:onProperty :p ; owl:someValuesFrom :C ] .
:A11 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:someValuesFrom [ a rdfs:Datatype ; owl:oneOf ( "x" ) ] ] .
:A12 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :p ; owl:allValuesFrom [ a owl:Class ; owl:oneOf ( :i "x" ) ] ] .
:A13 rdfs:subClassOf [ a owl:Restriction ; owl:onProperty :d ; owl:someValuesFrom [ owl:datatypeComplementOf xsd:string ] ] .
:A14 rdfs:subClassOf [ owl:onProperty :p ; owl:someValuesFrom [ a owl:Class ; owl:oneOf rdf:nil ] ] .
:d rdfs:range [ owl:oneOf ( "a" "b" ) ] .
:q rdfs:range xsd:string .
"#;
        let common: ParserConfiguration<RcStr> = ParserConfiguration { lax: true, ..Default::default() };
        let config = RDFParserConfiguration { common, format: Some(oxrdfio::RdfFormat::Turtle) };
        let (ont, incomplete): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
            read(&mut ttl.as_bytes(), config).unwrap();
        assert!(incomplete.is_complete(), "{incomplete:?}");
        let mut got: Vec<String> = ont
            .iter()
            .filter(|ac| {
                matches!(
                    ac.component,
                    Component::SubClassOf(_)
                        | Component::ObjectPropertyRange(_)
                        | Component::DataPropertyRange(_)
                )
            })
            .map(|ac| ac.component.as_functional().to_string().replace("http://example.org/", ""))
            .collect();
        got.sort();
        let xsd = |n: &str| format!("<http://www.w3.org/2001/XMLSchema#{n}>");
        let mut want = vec![
            "SubClassOf(<A1> ObjectSomeValuesFrom(<d> ObjectOneOf()))".to_string(),
            format!("SubClassOf(<A2> ObjectAllValuesFrom(<d> ObjectUnionOf({} {})))", xsd("string"), xsd("integer")),
            format!("SubClassOf(<A3> DataSomeValuesFrom(<d> {}))", xsd("string")),
            format!("SubClassOf(<A4> DataSomeValuesFrom(<p> {}))", xsd("string")),
            "SubClassOf(<A5> ObjectHasValue(<d> <i>))".to_string(),
            "SubClassOf(<A6> DataHasValue(<p> \"lit\"))".to_string(),
            "SubClassOf(<A7> ObjectSomeValuesFrom(<d> <C>))".to_string(),
            "SubClassOf(<A8> ObjectSomeValuesFrom(<d> <U>))".to_string(),
            "SubClassOf(<A9> DataSomeValuesFrom(<d> <DT>))".to_string(),
            "SubClassOf(<A10> ObjectSomeValuesFrom(<p> <C>))".to_string(),
            "SubClassOf(<A11> DataSomeValuesFrom(<d> DataOneOf(\"x\")))".to_string(),
            "SubClassOf(<A12> ObjectAllValuesFrom(<p> ObjectOneOf(<i>)))".to_string(),
            format!("SubClassOf(<A13> DataSomeValuesFrom(<d> DataComplementOf({})))", xsd("string")),
            "SubClassOf(<A14> ObjectSomeValuesFrom(<p> ObjectOneOf()))".to_string(),
            "ObjectPropertyRange(<d> ObjectOneOf())".to_string(),
            format!("DataPropertyRange(<q> {})", xsd("string")),
        ];
        want.sort();
        assert_eq!(got, want);
    }

    #[test]
    fn test_ttl() {
        let ont = r#"@prefix : <http://www.example.com/iri#> .
@prefix o: <http://www.example.com/iri#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix xml: <http://www.w3.org/XML/1998/namespace> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@base <http://www.example.com/iri> .

<http://www.example.com/iri> rdf:type owl:Ontology ;
                              owl:versionIRI <http://www.example.com/viri> .

#################################################################
#    Classes
#################################################################

###  http://www.example.com/iri#C
o:C rdf:type owl:Class .
"#;

        read_from_format(&mut ont.as_bytes(), oxrdfio::RdfFormat::Turtle);
    }

    #[test]
    fn test_jsonld() {
        let ont = r#"[
  {
    "@id": "http://www.example.com/iri",
    "@type": [
      "http://www.w3.org/2002/07/owl#Ontology"
    ],
    "http://www.w3.org/2002/07/owl#versionIRI": [
      {
        "@id": "http://www.example.com/viri"
      }
    ]
  },
  {
    "@id": "http://www.example.com/iri#C",
    "@type": [
      "http://www.w3.org/2002/07/owl#Class"
    ]
  },
  {
    "@id": "http://www.example.com/viri"
  },
  {
    "@id": "http://www.w3.org/2002/07/owl#Class"
  },
  {
    "@id": "http://www.w3.org/2002/07/owl#Ontology"
  }
]"#;
        read_from_format(
            &mut ont.as_bytes(),
            oxrdfio::RdfFormat::JsonLd {
                profile: oxrdfio::JsonLdProfileSet::empty(),
            },
        );
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

    #[test]
    fn rdfs_class_does_not_produce_class_assertion() {
        // rdfs:Class is the RDFS metaclass, not a valid OWL class expression.
        // A triple <X> rdf:type rdfs:Class should NOT become a ClassAssertion
        // (ClassAssertion(Class(rdfs:Class), X) is meaningless in OWL DL).
        // The triple should be left in the incomplete parse instead.
        let xml = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
         xmlns:owl="http://www.w3.org/2002/07/owl#">
    <owl:Ontology rdf:about="http://www.example.com/iri"/>
    <rdfs:Class rdf:about="http://www.example.com/iri#C"/>
</rdf:RDF>"#;

        let (ont, incomplete): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
            read(&mut xml.as_bytes(), Default::default()).unwrap();

        let ont: SetOntology<_> = ont.into();
        let class_assertions: Vec<_> = ont
            .iter()
            .filter(|ac| matches!(ac.component, Component::ClassAssertion(_)))
            .collect();
        assert!(
            class_assertions.is_empty(),
            "rdfs:Class should not produce ClassAssertion axioms, got: {class_assertions:?}"
        );
        assert!(
            !incomplete.is_complete(),
            "rdfs:Class triple should remain in the incomplete parse"
        );
    }
}
