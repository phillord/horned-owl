use Term::*;
use oxrdf::{BlankNode, NamedNode, NamedOrBlankNode, Triple};

use crate::{error::HornedError, io::ParserConfiguration, vocab::Facet};
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
    resolve::strict_resolve_iri,
    vocab::RDFS as VRDFS,
};

use std::collections::BTreeSet;
use rustc_hash::{FxHashMap as HashMap, FxHashSet as HashSet};
use std::fmt::Debug;
use std::io::Cursor;
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

/// An ontology parser which takes a set of RDF triples and turns them
/// into an RDFOntology.
#[derive(Debug)]
pub struct OntologyParser<'a, A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>> {
    /// The ontology being populated
    o: O,
    b: &'a Build<A>,
    config: ParserConfiguration,

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
    // Which of those a `retrieve_to_ce` actually handed to an axiom. The map is
    // read non-destructively (see `retrieve_to_ce`), so it cannot itself say what
    // is left over; without this every blank-node class expression in the document
    // was reported as unparsed, and `horned-validate` failed on files it had read
    // perfectly — `src/ont/owl-rdf/and.owl` among them.
    class_expression_used: HashSet<BNode<A>>,
    object_property_expression: HashMap<BNode<A>, ObjectPropertyExpression<A>>,
    data_range: HashMap<BNode<A>, DataRange<A>>,
    // Annotations mapped to Triples (one entry per reifying owl:Axiom block).
    ann_map: HashMap<[Term<A>; 3], Vec<BTreeSet<Annotation<A>>>>,
    atom: HashMap<Term<A>, Atom<A>>,
    variable: HashMap<IRI<A>, Variable<A>>,

    // How far through the parse have we got?
    state: OntologyParserState,
    // AA is otherwise unreferenced
    p: PhantomData<AA>,
}

impl<'a, A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>> OntologyParser<'a, A, AA, O> {
    /// Return a new empty OntologyParser.
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
            class_expression_used: d!(),
            object_property_expression: d!(),
            data_range: d!(),
            ann_map: d!(),
            atom: d!(),
            variable: d!(),
            state: OntologyParserState::New,
            p: d!(),
        }
    }

    /// Return a new OntologyParser taking all triples from an BufRead
    /// in RDF-XML.
    pub fn from_bufread<'b, R: BufRead>(
        b: &'a Build<A>,
        bufread: &'b mut R,
        config: ParserConfiguration,
    ) -> Result<OntologyParser<'a, A, AA, O>, HornedError> {
        let format = config.rdf.format.unwrap_or(oxrdfio::RdfFormat::RdfXml);
        Self::from_bufread_with_format(b, bufread, config, format)
    }

    pub fn from_bufread_with_format<'b, R: BufRead>(
        b: &'a Build<A>,
        bufread: &'b mut R,
        config: ParserConfiguration,
        format: oxrdfio::RdfFormat,
    ) -> Result<OntologyParser<'a, A, AA, O>, HornedError> {
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
            triples.push(b.convert_substitute_triple(ox_triple, last_pos.get()));
            //last_pos.set(parser.buffer_position().try_into().unwrap());
        }

        Ok(OntologyParser::new(b, triples, config))
    }

    /// Return an new OntologyParser taking all triples in RDF-XML from the given IRI.
    pub fn from_doc_iri(
        b: &'a Build<A>,
        iri: &IRI<A>,
        config: ParserConfiguration,
    ) -> Result<OntologyParser<'a, A, AA, O>, HornedError> {
        OntologyParser::from_bufread(
            b,
            &mut Cursor::new(strict_resolve_iri(
                iri,
                config.remote_body_limit,
                config.local_only,
            )?),
            config,
        )
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
                [_, Term::OWL(VOWL::DisjointWith), _]
                | [_, Term::OWL(VOWL::EquivalentClass), _]
                | [_, Term::OWL(VOWL::InverseOf), _]
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

    /// Process all import statements
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

    /// Process the header statement
    fn headers(&mut self) {
        //Section 3.1.2/table 4
        //   *:x rdf:type owl:Ontology .
        //[ *:x owl:versionIRI *:y .]
        let mut iri: Option<IRI<_>> = None;
        let mut viri: Option<IRI<_>> = None;

        for t in std::mem::take(&mut self.simple) {
            match t.triple() {
                [Term::Iri(s), Term::RDF(VRDF::Type), Term::OWL(VOWL::Ontology)] => {
                    iri = Some(s.clone());
                }
                [Term::Iri(s), Term::OWL(VOWL::VersionIRI), Term::Iri(ob)]
                    if iri.as_ref() == Some(s) =>
                {
                    viri = Some(ob.clone());
                }
                _ => self.simple.push(t),
            }
        }

        self.o.insert(OntologyID { iri, viri });
    }

    /// We should process the backward compatability rules, but
    /// currently do nothing here at all. I expect that there are not
    /// many OWL1 ontologies that need processing in existence.
    fn backward_compat(&mut self) {
        // Table 5, Table 6
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
                let iri = self.b.iri(rdfs.as_ref());
                self.annotation(&[s.clone(), Term::Iri(iri), b.clone()])
            }
            [s, OWL(owl), b] => {
                let iri = self.b.iri(owl.as_ref());
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
            [_, Iri(p), Term::BNode(_)] => Ok(Annotation {
                ap: AnnotationProperty(p.clone()),
                av: self.b.anon_renumbered().into(),
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
        let mut bnode_to_key: HashMap<BNode<A>, [Term<A>; 3]> = HashMap::default();
        // Every base triple a reification names, with the position of the block
        // that named it, so one the document leaves unstated can be restored.
        let mut reified: Vec<([Term<A>; 3], u64)> = Vec::new();

        for (k, v) in std::mem::take(&mut self.bnode) {
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
                    let mut key = self.b.substitute_term([sb.clone(), p.clone(), ob.clone()]);
                    // A property-chain reification often points `annotatedTarget`
                    // at a SEPARATE Collection bnode that is structurally equal
                    // to — but a distinct node from — the chain's own list (e.g.
                    // ENVO serializes both as `parseType="Collection"`). Key such
                    // annotations by the list's content so they match the axiom
                    // regardless of which bnode carries the list.
                    if matches!(key[1], Term::OWL(VOWL::PropertyChainAxiom)) {
                        if let Term::BNode(ref b) = key[2] {
                            if let Some(members) = self.bnode_seq.get(b) {
                                key[2] = Self::canon_list_term(members);
                            }
                        }
                    }
                    // Record the bnode → axiom-key mapping so a nested
                    // annotation (owl:Annotation whose annotatedSource is THIS
                    // reification bnode) can find the axiom it refines.
                    bnode_to_key.insert(k, key.clone());
                    reified.push((key.clone(), pos));
                    let anns = self.parse_annotations(ann)?;
                    self.ann_map.entry(key).or_default().push(anns);
                }

                _ => {
                    self.bnode.insert(k, v);
                }
            }
        }

        // Second pass: owl:Annotation bnodes attach nested annotations
        // to the annotation identified by (annotatedSource bnode,
        // annotatedProperty, annotatedTarget).
        for (k, v) in std::mem::take(&mut self.bnode) {
            match v.as_slice() {
                [
                    [_, Term::OWL(VOWL::AnnotatedProperty), p],
                    [_, Term::OWL(VOWL::AnnotatedSource), Term::BNode(sb_bnode)],
                    [_, Term::OWL(VOWL::AnnotatedTarget), ob],
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Annotation)],
                    nested_ann @ ..,
                ] => {
                    if let Some(ann_key) = bnode_to_key.get(sb_bnode).cloned() {
                        let ref_ann =
                            self.annotation(&[Term::BNode(k.clone()), p.clone(), ob.clone()])?;
                        let nested = self.parse_annotations(nested_ann)?;
                        // `ann_map` is keyed to a Vec of annotation sets (several
                        // owl:Axiom blocks may reify the same base triple); refine
                        // the reified annotation wherever it appears.
                        if let Some(ann_sets) = self.ann_map.get_mut(&ann_key) {
                            for ann_set in ann_sets.iter_mut() {
                                if let Some(mut target) = ann_set.take(&ref_ann) {
                                    target.ann = nested.clone();
                                    ann_set.insert(target);
                                    break;
                                }
                            }
                        }
                    } else {
                        self.bnode.insert(k, v);
                    }
                }
                _ => {
                    self.bnode.insert(k, v);
                }
            }
        }

        self.restore_reified_triples(reified);
        Ok(())
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
    /// Only a triple between named things is restored: a blank-node subject or
    /// object belongs to a construct held elsewhere — a class expression, an RDF
    /// list — which the translation reaches by its own route.
    fn restore_reified_triples(&mut self, reified: Vec<([Term<A>; 3], u64)>) {
        if reified.is_empty() {
            return;
        }
        let mut stated: rustc_hash::FxHashSet<[Term<A>; 3]> =
            self.simple.iter().map(|t| t.triple().clone()).collect();
        let mut add: Vec<PosTriple<A>> = Vec::new();
        for (key, pos) in reified {
            if matches!(key[0], Term::BNode(_)) || matches!(key[2], Term::BNode(_)) {
                continue;
            }
            if stated.insert(key.clone()) {
                add.push(PosTriple(key, pos));
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

    /// A content-based key term for an RDF list (the members of a property
    /// chain). Used so a reification whose `annotatedTarget` is a distinct but
    /// structurally-equal Collection bnode still matches the axiom built from a
    /// different list bnode of the same content.
    fn canon_list_term(members: &[Term<A>]) -> Term<A> {
        let s: String = members
            .iter()
            .map(|t| format!("{t:?}"))
            .collect::<Vec<_>>()
            .join("\u{1}");
        Term::BNode(BNode(format!("__chain__\u{1}{s}").into()))
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
                [
                    [_, Term::OWL(VOWL::IntersectionOf), Term::BNode(bnodeid)], //: rustfmt hard line!
                    [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)],
                ] => {
                    ok_some! {
                        DataRange::DataIntersectionOf(
                            self.retrieve_to_dr_seq(bnodeid)?
                        )
                    }
                }
                [
                    [_, Term::OWL(VOWL::UnionOf), Term::BNode(bnodeid)], //: rustfmt hard line!
                    [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)],
                ] => {
                    ok_some! {
                        DataRange::DataUnionOf(
                            self.retrieve_to_dr_seq(bnodeid)?
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
                    [_, Term::OWL(VOWL::OneOf), Term::BNode(bnode)], //:
                    [_, Term::RDF(VRDF::Type), Term::RDFS(VRDFS::Datatype)],
                ] => {
                    ok_some! {
                        DataRange::DataOneOf(
                            self.retrieve_to_literal_seq(bnode)?
                        )
                    }
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

    // The following are a set of methods which move between RDF types
    // and OWL types. We use a standard naming scheme, with "convert"
    // where the change is stateless (except for `Build` caching),
    // "retrieve" where it involves changing other data structures by
    // side effect.

    /// Given a Term return an IRI if it can be converted to it
    fn convert_to_iri(&self, t: &Term<A>) -> Option<IRI<A>> {
        match t {
            Term::OWL(vowl) => Some(self.b.iri(vowl.as_ref())),
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
            self.object_property_expression.remove(id)
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
            // Non-destructive: a blank-node class expression may be referenced by
            // more than one axiom. ROBOT's RDF/XML writer shares one restriction
            // bnode between, e.g., an `equivalentClass` intersection and the
            // `subClassOf` axioms `relax` derives from it; removing the CE on first
            // use silently dropped every later reference (and its axiom). Cloning
            // leaves it available; any genuinely unconsumed CE is still reported via
            // IncompleteParse and never enters the ontology.
            Term::BNode(id) => {
                let ce = self.class_expression.get(id).cloned();
                if ce.is_some() {
                    self.class_expression_used.insert(id.clone());
                }
                ce
            }
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

    /// Retrieve a Vec of Individual or None.
    fn retrieve_to_ni_seq(&mut self, bnodeid: &BNode<A>) -> Option<Vec<Individual<A>>> {
        self.retrieve_to_seq(bnodeid, |slf, t| slf.convert_to_iri(t).map(Into::into))
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
            Term::BNode(id) => self.data_range.remove(id),
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
            Term::BNode(_) => Some(IArgument::Individual(self.b.anon_renumbered().into())),
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
                let iri = self.b.iri(vowl.as_ref());
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
            Term::BNode(id) => Some(self.object_property_expression.remove(id)?.into()),
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
            self.object_property_expression.remove(a),
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
            (Term::BNode(a), Term::BNode(b)) => Ok(self
                .object_property_expression
                .remove(a)
                .zip(self.object_property_expression.remove(b))
                .map(|(a, b)| {
                    (
                        PropertyExpression::ObjectPropertyExpression(a),
                        PropertyExpression::ObjectPropertyExpression(b),
                    )
                })),
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

    /// Process class expressions.
    fn class_expressions(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        let mut parsed_new_ce = false;

        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            let ce: Result<_, HornedError> = match v.as_slice() {
                [
                    [_, Term::OWL(VOWL::OnProperty), pr],           //:
                    [_, Term::OWL(VOWL::SomeValuesFrom), ce_or_dr], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => match self.distinguish_retrieve_property_kind(pr, ic) {
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
                ] => match self.distinguish_retrieve_property_kind(pr, ic) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectHasValue {
                            ope,
                            i: NamedIndividual(self.convert_to_iri(val)?).into()
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
                ] => match self.distinguish_retrieve_property_kind(pr, ic) {
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
                    [_, Term::OWL(VOWL::OneOf), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)],
                ] => Ok(self
                    .retrieve_to_ni_seq(bnodeid)
                    .map(ClassExpression::ObjectOneOf)),
                [
                    [_, Term::OWL(VOWL::HasSelf), _], //:
                    [_, Term::OWL(VOWL::OnProperty), pr],
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Restriction)],
                ] => Ok(self.retrieve_to_ope(pr).map(ClassExpression::ObjectHasSelf)),
                [
                    [_, Term::OWL(VOWL::IntersectionOf), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)],
                ] => Ok(self
                    .retrieve_to_ce_seq(bnodeid)
                    .map(ClassExpression::ObjectIntersectionOf)),
                [
                    [_, Term::OWL(VOWL::UnionOf), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Class)],
                ] => Ok(self
                    .retrieve_to_ce_seq(bnodeid)
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
                            bce: self.b.class(VOWL::Thing).into()
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataExactCardinality {
                            n: self.convert_to_u32(literal)?,
                            dp,
                            dr: self.b.datatype(OWL2Datatype::Literal).into(),
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
                ] => match self.distinguish_retrieve_property_kind(pr, ic) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectMinCardinality {
                            n: self.convert_to_u32(literal)?,
                            ope,
                            bce: self.b.class(VOWL::Thing).into()
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataMinCardinality {
                            n: self.convert_to_u32(literal)?,
                            dp,
                            dr: self.b.datatype(OWL2Datatype::Literal).into(),
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
                ] => match self.distinguish_retrieve_property_kind(pr, ic) {
                    Some(PropertyExpression::ObjectPropertyExpression(ope)) => {
                        ok_some!(ClassExpression::ObjectMaxCardinality {
                            n: self.convert_to_u32(literal)?,
                            ope,
                            bce: self.b.class(VOWL::Thing).into()
                        })
                    }
                    Some(PropertyExpression::DataProperty(dp)) => {
                        ok_some!(ClassExpression::DataMaxCardinality {
                            n: self.convert_to_u32(literal)?,
                            dp,
                            dr: self.b.datatype(OWL2Datatype::Literal).into(),
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

    fn axioms(&mut self, ic: &[&O]) -> Result<(), HornedError> {
        let mut single_bnodes = vec![];

        for (this_bnode, v) in std::mem::take(&mut self.bnode) {
            let axiom: Result<_, HornedError> = match v.as_slice() {
                [
                    [_, Term::OWL(VOWL::AssertionProperty), pr],          //:
                    [_, Term::OWL(VOWL::SourceIndividual), Term::Iri(i)], //:
                    [_, target_type, target],                             //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::NegativePropertyAssertion)], //:
                ] => match target_type {
                    Term::OWL(VOWL::TargetIndividual) => ok_some!(
                        NegativeObjectPropertyAssertion {
                            ope: self.retrieve_to_ope(pr)?,
                            from: i.into(),
                            to: self.convert_to_iri(target)?.into(),
                        }
                        .into()
                    ),
                    Term::OWL(VOWL::TargetValue) => ok_some!(
                        NegativeDataPropertyAssertion {
                            dp: self.convert_to_dp(pr)?,
                            from: i.into(),
                            to: self.convert_to_literal(target)?,
                        }
                        .into()
                    ),
                    _ => Err(HornedError::invalid_at(
                        "Unable to interpret negative property assertion",
                        v.position(),
                    )),
                },
                [
                    [_, Term::OWL(VOWL::Members), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDisjointClasses)],
                ] => {
                    ok_some! {
                        DisjointClasses (
                            self.retrieve_to_ce_seq(bnodeid)?
                        ).into()
                    }
                }
                [
                    [_, Term::OWL(VOWL::Members), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDifferent)],
                ] => {
                    ok_some! {
                        DifferentIndividuals (
                            self.retrieve_to_ni_seq(bnodeid)?
                        ).into()
                    }
                }
                [
                    [_, Term::OWL(VOWL::DistinctMembers), Term::BNode(bnodeid)], //:
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::AllDifferent)],
                ] => {
                    ok_some! {
                        DifferentIndividuals (
                            self.retrieve_to_ni_seq(bnodeid)?
                        ).into()
                    }
                }
                _ => Ok(None),
            };

            match axiom? {
                Some(axiom) => self.merge(AnnotatedComponent {
                    component: axiom,
                    ann: BTreeSet::new(),
                }),
                _ => {
                    if v.len() == 1 {
                        single_bnodes.push(v[0].clone());
                    } else {
                        self.bnode.insert(this_bnode, v);
                    }
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
                [
                    Term::Iri(iri),
                    Term::OWL(VOWL::DisjointUnionOf),
                    Term::BNode(bnodeid),
                ] => {
                    ok_some! {
                        DisjointUnion(
                            Class(iri.clone()),
                            self.retrieve_to_ce_seq(bnodeid)?
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
                    Term::OWL(VOWL::AsymmetricProperty),
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
                    ok_some! {
                        match self.distinguish_retrieve_property_kind(spr, ic)? {
                            PropertyExpression::ObjectPropertyExpression(ope) =>
                                SubObjectPropertyOf {
                                    sup: ope,
                                    sub: self.retrieve_to_sope(pr)?,
                                }.into(),
                            PropertyExpression::DataProperty(dp) =>
                                SubDataPropertyOf {
                                    sup: dp,
                                    sub: self.convert_to_dp(pr)?
                                }.into(),
                            PropertyExpression::AnnotationProperty(ap) =>
                                SubAnnotationPropertyOf {
                                    sup: ap,
                                    sub: self.convert_to_ap(pr)?
                                }.into(),
                        }
                    }
                }
                [
                    Term::Iri(pr),
                    Term::OWL(VOWL::PropertyChainAxiom),
                    Term::BNode(id),
                ] => {
                    // If a property-chain reification stored its annotations under
                    // the list's content key (because its annotatedTarget was a
                    // distinct Collection bnode), relocate them onto this base
                    // triple's key so the generic take_anns below attaches them.
                    if let Some(members) = self.bnode_seq.get(id) {
                        let canon = Self::canon_list_term(members);
                        let canon_key = [
                            Term::Iri(pr.clone()),
                            Term::OWL(VOWL::PropertyChainAxiom),
                            canon,
                        ];
                        if let Some(anns) = self.ann_map.remove(&canon_key) {
                            let base_key = [
                                Term::Iri(pr.clone()),
                                Term::OWL(VOWL::PropertyChainAxiom),
                                Term::BNode(id.clone()),
                            ];
                            self.ann_map.entry(base_key).or_default().extend(anns);
                        }
                    }
                    ok_some! {
                        SubObjectPropertyOf {
                            sub: SubObjectPropertyExpression::ObjectPropertyChain(
                                self.bnode_seq
                                    .remove(id)?
                                    .iter()
                                    .map(|t| self.retrieve_to_ope(t).unwrap())
                                    .collect()
                            ),
                            sup: ObjectProperty(pr.clone()).into(),
                        }.into()
                    }
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
                    match self.distinguish_retrieve_property_kind(pr, ic)? {
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
                        Ok(None) => match r {
                            Term::Iri(sub) if self.config.lax => self
                                .annotation(t.triple())
                                .map(|ann| {
                                    Some(AnnotationAssertion { subject: sub.into(), ann }.into())
                                }),
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
                        Ok(None) => match r {
                            Term::Iri(sub) if self.config.lax => self
                                .annotation(t.triple())
                                .map(|ann| {
                                    Some(AnnotationAssertion { subject: sub.into(), ann }.into())
                                }),
                            _ => Err(HornedError::invalid(
                                "Cannot distinguish the types of {r} and {s}",
                            )),
                        },
                        Err(err) => Err(err),
                        _ => unreachable!("Unexpected error in equivalent property matching"),
                    }
                }
                [Term::Iri(sub), Term::OWL(VOWL::SameAs), Term::Iri(obj)] => {
                    Ok(Some(SameIndividual(vec![sub.into(), obj.into()]).into()))
                }
                [Term::Iri(i), Term::OWL(VOWL::DifferentFrom), Term::Iri(j)] => {
                    Ok(Some(DifferentIndividuals(vec![i.into(), j.into()]).into()))
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
                    // Distinct reifications of the same base triple are distinct
                    // annotated axioms; insert each rather than merging.
                    for ann in self.take_anns(t.triple()) {
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

        // Next identify the atoms with a big pattern matcher over bnodes
        for (bnode, triple) in std::mem::take(&mut self.bnode) {
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
                    // Annotations attached directly to the Imp node (OWLAPI /
                    // Protégé style), plus any reified (owl:Axiom) annotations
                    // collected earlier — the form Horned-OWL's own RDF writer
                    // produces. `ann_map` is Vec-valued, so drain every set.
                    let mut ann = self.parse_annotations(&ann_triples)?;
                    let key = self.b.substitute_term([
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

    fn simple_annotations(&mut self, parse_all: bool) -> Result<(), HornedError> {
        let ont_id = <O as AsRef<SetIndex<A, AA>>>::as_ref(&self.o).the_ontology_id_or_default();
        for t in std::mem::take(&mut self.simple) {
            let firi =
                |s: &mut OntologyParser<_, _, _>, t, iri: &IRI<_>| -> Result<(), HornedError> {
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
                [Term::Iri(iri), _, _] if ont_id.iri.as_ref() == Some(iri) => {
                    self.o
                        .insert(OntologyAnnotation(self.annotation(t.triple())?));
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
        for (k, v) in std::mem::take(&mut self.bnode) {
            let fbnode =
                |s: &mut OntologyParser<_, _, _>, t, _: &BNode<A>| -> Result<_, HornedError> {
                    let ind: AnonymousIndividual<A> = s.b.anon_renumbered();
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
            let states_an_individual = |s: &OntologyParser<_, _, _>, t: &[Term<A>; 3]| match t {
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
                    let ind: AnonymousIndividual<A> = self.b.anon_renumbered();
                    for triple in v.iter() {
                        if let [_, Term::RDF(VRDF::Type), Term::Iri(cls)] = triple {
                            self.merge(AnnotatedComponent {
                                component: ClassAssertion {
                                    ce: Class(cls.clone()).into(),
                                    i: ind.clone().into(),
                                }
                                .into(),
                                ann: BTreeSet::new(),
                            });
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

        let bnode: Vec<_> = self.bnode.into_values().collect();
        let bnode_seq: Vec<_> = self.bnode_seq.into_values().collect();
        let used = std::mem::take(&mut self.class_expression_used);
        let class_expression: Vec<_> = self
            .class_expression
            .into_iter()
            .filter(|(id, _)| !used.contains(id))
            .map(|(_, ce)| ce)
            .collect();
        let object_property_expression: Vec<_> =
            self.object_property_expression.into_values().collect();
        let data_range = self.data_range.into_values().collect();

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

pub fn parser_with_build<'b, A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>, R: BufRead>(
    bufread: &mut R,
    build: &'b Build<A>,
    config: ParserConfiguration,
) -> Result<OntologyParser<'b, A, AA, O>, HornedError> {
    OntologyParser::from_bufread(build, bufread, config)
}

pub fn read_with_build<A: ForIRI, AA: ForIndex<A>, R: BufRead>(
    bufread: &mut R,
    build: &Build<A>,
    config: ParserConfiguration,
) -> Result<(ConcreteRDFOntology<A, AA>, IncompleteParse<A>), HornedError> {
    parser_with_build(bufread, build, config)?.parse()
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
        let b = Build::new_rc();
        let mut p: OntologyParser<_, Rc<AnnotatedComponent<RcStr>>, ConcreteRDFOntology<_, _>> =
            parser_with_build(
                &mut slurp_rdfont("import").as_bytes(),
                &b,
                Default::default(),
            )
            .unwrap();
        p.parse_imports().unwrap();

        let rdfont = p.as_ontology();
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
            )
            .unwrap();
        let _ = p.parse_declarations();

        let rdfont = p.as_ontology();
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
            )?;
        let (family_other, incomplete) = p.parse()?;
        assert!(incomplete.is_complete());

        let mut p = parser_with_build(
            &mut slurp_rdfont("withimport/import-property").as_bytes(),
            &b,
            Default::default(),
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
    fn error_on_some_broken() {
        // Check error handling on (some a c) where a is an annotation property
        let err = read(
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

        let err = read(&mut xml.as_bytes(), Default::default()).unwrap_err();

        assert!(matches! {err, HornedError::ParserError(_,_)})
    }

    fn read_from_format<R: BufRead>(
        bufread: &mut R,
        config: ParserConfiguration,
        format: oxrdfio::RdfFormat,
    ) {
        let (ont, incomp): (ConcreteRDFOntology<RcStr, Rc<AnnotatedComponent<RcStr>>>, _) =
            OntologyParser::from_bufread_with_format(&Build::new_rc(), bufread, config, format)
                .unwrap()
                .parse()
                .unwrap();

        dbg!(ont, incomp);
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

        read_from_format(
            &mut ont.as_bytes(),
            Default::default(),
            oxrdfio::RdfFormat::Turtle,
        );
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
            Default::default(),
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
