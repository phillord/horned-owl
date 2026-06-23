pub mod ox;

use indexmap::IndexMap;
use quick_xml::{
    events::{BytesDecl, BytesEnd, BytesStart, BytesText, Event},
    Writer,
};

use oxrdf::{LiteralRef, NamedOrBlankNodeRef, TermRef, TripleRef};

use std::{
    self,
    cell::RefCell,
    fmt,
    hash::{Hash, Hasher},
    io::{self, Write},
};
use std::{
    cmp::Ordering,
    collections::{HashMap, VecDeque},
    fmt::{Debug, Formatter},
};

// Utilities
pub fn is_name_start_char(c: char) -> bool {
    // ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6] | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]
    matches!(c,
        ':'
        | 'A'..='Z'
        | '_'
        | 'a'..='z'
        | '\u{C0}'..='\u{D6}'
        | '\u{D8}'..='\u{F6}'
        | '\u{F8}'..='\u{2FF}'
        | '\u{370}'..='\u{37D}'
        | '\u{37F}'..='\u{1FFF}'
        | '\u{200C}'..='\u{200D}'
        | '\u{2070}'..='\u{218F}'
        | '\u{2C00}'..='\u{2FEF}'
        | '\u{3001}'..='\u{D7FF}'
        | '\u{F900}'..='\u{FDCF}'
        | '\u{FDF0}'..='\u{FFFD}'
        | '\u{10000}'..='\u{EFFFF}')
}

pub fn is_name_char(c: char) -> bool {
    // NameStartChar | "-" | "." | [0-9] | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
    is_name_start_char(c)
        || matches!(c,  '-' | '.' | '0'..='9' | '\u{B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}')
}

fn map_err(error: quick_xml::Error) -> io::Error {
    io::Error::new(io::ErrorKind::Other, error)
}

// Begin RDF data model

// The RDF data model here is very similar to that in oxrdf and
// originally rio. Re-implementing it rather than just reusing it adds
// considerable complexity, so requires some explanation.

// With Rio, all the entities were hard-coded to the type 'str. This
// brings with it the cost of life time management which was likely to
// create difficulties for both this library and horned-owl for which
// I wrote this library.

// I am guessing it is these difficulties that resulted in rio being
// re-written to oxrdf, as this now contains duplicate data models,
// one hardcoded to 'str and one owned String.

// The second of these would fulfil my needs, however, requires a full
// clone of all String instances, while Horned-OWL uses generics which
// allow the use of smart pointers. My initial testing suggests moving
// to String from AsRef<str> adds 20-30% overhead for large
// serialisations.

// So we are stuck with two nearly identical implementations.


/// An RDF IRI
#[derive(Ord, PartialOrd, Clone)]
pub struct PNamedNode<A: AsRef<str>> {
    pub iri: A,
    // true if we have previously split iri
    position_cache: RefCell<bool>,
    // position at which the fragment occurs
    position_split: RefCell<Option<usize>>,
}

impl<A: AsRef<str>> PNamedNode<A> {
    pub fn new(iri: A) -> Self {
        PNamedNode {
            iri,
            position_cache: RefCell::new(false),
            position_split: RefCell::new(None),
        }
    }
}

impl<A: Debug + AsRef<str>> Debug for PNamedNode<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::core::fmt::Result {
        match *self {
            PNamedNode {
                ref iri,
                position_cache: _,
                position_split: _,
            } => {
                let mut debug_trait_builder = f.debug_struct("PNamedNode");
                let _ = debug_trait_builder.field("iri", &&(*iri));
                debug_trait_builder.finish()
            }
        }
    }
}

impl<A: AsRef<str>> Hash for PNamedNode<A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.iri.as_ref().hash(state);
    }
}

impl<A: AsRef<str>> PartialEq for PNamedNode<A> {
    fn eq(&self, other: &Self) -> bool {
        self.iri.as_ref() == other.iri.as_ref()
    }
}

impl<A: AsRef<str>> Eq for PNamedNode<A> {}

impl<A: AsRef<str>> PNamedNode<A> {
    fn split_iri(&self) -> (&str, &str) {
        let iri = self.iri.as_ref();

        let mut position_cache = self.position_cache.borrow_mut();
        let mut position_split = self.position_split.borrow_mut();
        let position_base;
        let position_add;

        if !*position_cache {
            *position_cache = true;
            position_base = iri.rfind(|c| !is_name_char(c) || c == ':');
            if let Some(position_base) = position_base {
                position_add = iri[position_base..].find(|c| is_name_start_char(c) && c != ':');
                if let Some(position_add) = position_add {
                    *position_split = Some(position_base + position_add);
                }
            }
        }

        if let Some(position_split) = *position_split {
            (&iri[..position_split], &iri[position_split..])
        } else {
            (iri, "")
        }
    }
}

impl<A: AsRef<str>> fmt::Display for PNamedNode<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<{}>", self.as_ref())
    }
}

impl<A: AsRef<str>> AsRef<str> for PNamedNode<A> {
    fn as_ref(&self) -> &str {
        self.iri.as_ref()
    }
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub struct PBlankNode<A: AsRef<str>> {
    pub id: A,
}

impl<A: AsRef<str>> PBlankNode<A> {
    pub fn new(id: A) -> Self {
        PBlankNode { id }
    }
}

impl<A: AsRef<str>> fmt::Display for PBlankNode<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

impl<A: AsRef<str>> AsRef<str> for PBlankNode<A> {
    fn as_ref(&self) -> &str {
        self.id.as_ref()
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PLiteral<A: AsRef<str>> {
    Simple { value: A },
    LanguageTaggedString { value: A, language: A },
    Typed { value: A, datatype: PNamedNode<A> },
}

impl<A: AsRef<str>> fmt::Display for PLiteral<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let nn: LiteralRef<'_> = self.into();
        write!(f, "{}", nn)
    }
}


#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Clone, Hash)]
pub enum PNamedOrBlankNode<A: AsRef<str>> {
    NamedNode(PNamedNode<A>),
    BlankNode(PBlankNode<A>),
}

impl<A: AsRef<str>> fmt::Display for PNamedOrBlankNode<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let nn: NamedOrBlankNodeRef<'_> = self.into();
        write!(f, "{}", nn)
    }
}

impl<A: AsRef<str>> From<PNamedNode<A>> for PNamedOrBlankNode<A> {
    fn from(nn: PNamedNode<A>) -> Self {
        PNamedOrBlankNode::NamedNode(nn)
    }
}

impl<A: AsRef<str>> From<PBlankNode<A>> for PNamedOrBlankNode<A> {
    fn from(nn: PBlankNode<A>) -> Self {
        PNamedOrBlankNode::BlankNode(nn)
    }
}

impl<A: AsRef<str>> AsRef<str> for PNamedOrBlankNode<A> {
    fn as_ref(&self) -> &str {
        match self {
            PNamedOrBlankNode::NamedNode(nn) => nn.as_ref(),
            PNamedOrBlankNode::BlankNode(bn) => bn.as_ref(),
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PTerm<A: AsRef<str>> {
    NamedNode(PNamedNode<A>),
    BlankNode(PBlankNode<A>),
    Literal(PLiteral<A>),
}

impl<A: AsRef<str>> PartialEq<PNamedOrBlankNode<A>> for PTerm<A> {
    fn eq(&self, other: &PNamedOrBlankNode<A>) -> bool {
        match (self, other) {
            (Self::NamedNode(nn), PNamedOrBlankNode::NamedNode(onn)) => {
                nn.iri.as_ref() == onn.iri.as_ref()
            }
            (Self::BlankNode(bn), PNamedOrBlankNode::BlankNode(obn)) => {
                bn.id.as_ref() == obn.id.as_ref()
            }
            _ => false,
        }
    }
}

impl<A: AsRef<str>> From<PNamedOrBlankNode<A>> for PTerm<A> {
    fn from(nbn: PNamedOrBlankNode<A>) -> Self {
        match nbn {
            PNamedOrBlankNode::NamedNode(nn) => nn.into(),
            PNamedOrBlankNode::BlankNode(bn) => bn.into(),
        }
    }
}

impl<A: AsRef<str>> fmt::Display for PTerm<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let t:TermRef<'_> = self.into();
        write!(f, "{}", t)
    }
}

impl<A: AsRef<str>> From<PBlankNode<A>> for PTerm<A> {
    fn from(nn: PBlankNode<A>) -> Self {
        PTerm::BlankNode(nn)
    }
}

impl<A: AsRef<str>> From<PNamedNode<A>> for PTerm<A> {
    fn from(nn: PNamedNode<A>) -> Self {
        PTerm::NamedNode(nn)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PTriple<A: AsRef<str>> {
    pub subject: PNamedOrBlankNode<A>,
    pub predicate: PNamedNode<A>,
    pub object: PTerm<A>,
}

impl<A: AsRef<str>> PTriple<A> {
    pub fn new(
        subject: PNamedOrBlankNode<A>,
        predicate: PNamedNode<A>,
        object: PTerm<A>,
    ) -> PTriple<A> {
        PTriple {
            subject,
            predicate,
            object,
        }
    }

    pub fn is_type(&self) -> bool {
        self.predicate.iri.as_ref() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
    }

    pub fn is_collection(&self) -> bool {
        self.is_collection_first() || self.is_collection_rest()
    }

    pub fn is_collection_first(&self) -> bool {
        &self.predicate.iri.as_ref() == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
    }

    pub fn is_collection_rest(&self) -> bool {
        &self.predicate.iri.as_ref() == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
    }

    pub fn is_collection_end(&self) -> bool {
        if let PTerm::NamedNode(nn) = &self.object {
            nn.iri.as_ref() == "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"
        } else {
            false
        }
    }

    pub fn printable(&self) -> String {
        format!("{}\n\t{}\n\t{}", self.subject, self.predicate, self.object)
    }
}

impl<A: AsRef<str>> fmt::Display for PTriple<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let t: TripleRef<'_> = self.into();
        write!(f, "{}", t)
    }
}

// End basic RDF data model

/// Triple like objects contain a single subject but potentially multiple normal triples.
trait TripleLike<A>
where
    A: AsRef<str> + Clone,
{
    /// Can a new Triple be accepted onto this TripleLike.
    fn accept(&mut self, t: PTriple<A>) -> Option<PTriple<A>>;

    /// What is the subject of the triple like object
    fn subject(&self) -> &PNamedOrBlankNode<A>;

    /// Return all triples that have a literal as object
    fn literal_objects(&self) -> Vec<&PTriple<A>>;

    /// Return all types
    fn find_typed(&self) -> Option<&PTriple<A>>;

    /// Return all triples
    fn triples(&self) -> Vec<&PTriple<A>>;
}

/// A multi-triple contains multiple triples with the same shared subject
/// These will be rendered as a shared node
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PMultiTriple<A: AsRef<str>> {
    vec: Vec<PTriple<A>>,
}

impl<A> PMultiTriple<A>
where
    A: AsRef<str> + PartialEq,
{
    #[allow(dead_code)]
    pub(crate) fn empty() -> PMultiTriple<A> {
        PMultiTriple { vec: vec![] }
    }

    pub fn new(vec: Vec<PTriple<A>>) -> PMultiTriple<A> {
        PMultiTriple { vec }
    }
}

impl<A> TripleLike<A> for PMultiTriple<A>
where
    A: AsRef<str> + Clone + PartialEq,
{
    fn accept(&mut self, t: PTriple<A>) -> Option<PTriple<A>> {
        if self.subject().as_ref() == t.subject.as_ref() {
            self.vec.push(t);
            None
        } else {
            Some(t)
        }
    }

    fn subject(&self) -> &PNamedOrBlankNode<A> {
        // There should be no empty instances, so this should be safe
        &self.vec[0].subject
    }

    fn literal_objects(&self) -> Vec<&PTriple<A>> {
        self.vec
            .iter()
            .filter(|t| matches!(t.object, PTerm::Literal(_)))
            .collect()
    }

    fn find_typed(&self) -> Option<&PTriple<A>> {
        self.vec.iter().find(|et| et.is_type())
    }

    fn triples(&self) -> Vec<&PTriple<A>> {
        self.vec.iter().collect()
    }
}

/// Contains a set of triples in a collection
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct PTripleSeq<A: AsRef<str>> {
    list_seq: VecDeque<(
        // the bnode of this section of the seq
        PNamedOrBlankNode<A>,
        // the first triple -- as we build from the rest triples this must be option
        Option<PTriple<A>>,
        // the rest triple
        PTriple<A>,
    )>,
}

impl<A: AsRef<str> + Eq> From<PTripleSeq<A>> for Vec<PMultiTriple<A>> {
    fn from(seq: PTripleSeq<A>) -> Self {
        let mut v = vec![];
        for tup in seq.list_seq {
            let mut items = vec![];
            if let Some(t) = tup.1 {
                items.push(t);
            }
            items.push(tup.2);
            v.push(PMultiTriple::new(items));
        }
        v
    }
}

impl<A: AsRef<str> + Clone> PTripleSeq<A> {
    #[allow(dead_code)]
    pub(crate) fn empty() -> PTripleSeq<A> {
        PTripleSeq {
            list_seq: VecDeque::new(),
        }
    }

    pub fn from_end(t: PTriple<A>) -> PTripleSeq<A> {
        let mut seq = PTripleSeq {
            list_seq: vec![].into(),
        };
        if let PNamedOrBlankNode::BlankNode(_) = &t.subject {
            seq.list_seq.push_front((t.subject.clone(), None, t));
        } else {
            todo!("This shouldn't happen")
        }
        seq
    }

    pub fn has_literal(&self) -> bool {
        self.list_seq.iter().any(|(_, t, _)| {
            matches!(
                t,
                Some(PTriple {
                    subject: _,
                    predicate: _,
                    object: PTerm::Literal(_)
                })
            )
        })
    }
}

impl<A> TripleLike<A> for PTripleSeq<A>
where
    A: AsRef<str> + Clone + Debug + Eq + PartialEq,
{
    fn accept(&mut self, t: PTriple<A>) -> Option<PTriple<A>> {
        if t.is_collection_first() {
            if let Some(pos) = self.list_seq.iter().position(|tup| &tup.0 == &t.subject) {
                if let Some(tuple) = self.list_seq.get_mut(pos) {
                    (*tuple).1 = Some(t)
                }

                return None;
            }
        }

        if let PTerm::BlankNode(bn) = &t.object {
            if let &PNamedOrBlankNode::BlankNode(ref snn) = self.subject() {
                if t.is_collection_rest() && snn == bn {
                    self.list_seq.push_front((t.subject.clone(), None, t));
                    return None;
                }
            }
        }

        Some(t)
    }

    fn subject(&self) -> &PNamedOrBlankNode<A> {
        &self.list_seq[0].0
    }

    fn literal_objects(&self) -> Vec<&PTriple<A>> {
        vec![]
    }

    fn find_typed(&self) -> Option<&PTriple<A>> {
        None
    }

    fn triples(&self) -> Vec<&PTriple<A>> {
        self.list_seq
            .iter()
            .flat_map(|(_, ot, t)| ot.iter().chain(std::iter::once(t)))
            .collect()
    }
}

/// Any form of triple container
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum PExpandedTriple<A: AsRef<str>> {
    PMultiTriple(PMultiTriple<A>),
    PTripleSeq(PTripleSeq<A>),
}

impl<A> From<PTriple<A>> for PMultiTriple<A>
where
    A: AsRef<str> + Clone + Debug + Eq + PartialEq,
{
    fn from(t: PTriple<A>) -> Self {
        PMultiTriple { vec: vec![t] }
    }
}

impl<A> From<PTriple<A>> for PExpandedTriple<A>
where
    A: AsRef<str> + Clone + Debug + Eq + PartialEq,
{
    fn from(t: PTriple<A>) -> Self {
        let t: PMultiTriple<A> = t.into();
        t.into()
    }
}

impl<A> From<PMultiTriple<A>> for PExpandedTriple<A>
where
    A: AsRef<str> + Clone + Debug + Eq + PartialEq,
{
    fn from(t: PMultiTriple<A>) -> Self {
        PExpandedTriple::PMultiTriple(t)
    }
}

impl<A> From<PTripleSeq<A>> for PExpandedTriple<A>
where
    A: AsRef<str> + Clone + Debug + Eq + PartialEq,
{
    fn from(t: PTripleSeq<A>) -> Self {
        PExpandedTriple::PTripleSeq(t)
    }
}

impl<A> TripleLike<A> for PExpandedTriple<A>
where
    A: AsRef<str> + Clone + Debug + Eq + PartialEq,
{
    fn accept(&mut self, triple: PTriple<A>) -> Option<PTriple<A>> {
        match self {
            Self::PMultiTriple(mt) => mt.accept(triple),
            Self::PTripleSeq(seq) => seq.accept(triple),
        }
    }

    fn subject(&self) -> &PNamedOrBlankNode<A> {
        match self {
            Self::PMultiTriple(mt) => mt.subject(),
            Self::PTripleSeq(seq) => seq.subject(),
        }
    }

    fn literal_objects(&self) -> Vec<&PTriple<A>> {
        match self {
            Self::PMultiTriple(mt) => mt.literal_objects(),
            Self::PTripleSeq(seq) => seq.literal_objects(),
        }
    }

    fn find_typed(&self) -> Option<&PTriple<A>> {
        match self {
            Self::PMultiTriple(mt) => mt.find_typed(),
            Self::PTripleSeq(seq) => seq.find_typed(),
        }
    }

    fn triples(&self) -> Vec<&PTriple<A>> {
        match self {
            Self::PMultiTriple(mt) => mt.triples(),
            Self::PTripleSeq(seq) => seq.triples(),
        }
    }
}

#[derive(Clone, Debug)]
enum PExpandedTripleKind {
    Multi,
    Seq,
}

/// A set of triple like objects that represents a coherent chunk
#[derive(Debug)]
pub struct PChunk<A: AsRef<str>> {
    queue: VecDeque<(PNamedOrBlankNode<A>, PExpandedTripleKind)>,
    store: HashMap<PNamedOrBlankNode<A>, (Option<PMultiTriple<A>>, Option<PTripleSeq<A>>)>,
    bnode_object_count: HashMap<PBlankNode<A>, usize>,
}

impl<A> PChunk<A>
where
    A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq,
{
    /// Given a set of triples normalize these to a chunk wth appropriate prettification applied
    pub fn normalize(v: Vec<PTriple<A>>) -> Self {
        let mut etv: IndexMap<PNamedOrBlankNode<A>, PMultiTriple<A>> = Default::default();
        let mut seq: Vec<PTripleSeq<A>> = vec![];
        let mut seq_rest: HashMap<PNamedOrBlankNode<A>, PTriple<A>> = Default::default();
        let mut seq_first: HashMap<PNamedOrBlankNode<A>, PTriple<A>> = Default::default();
        let mut bnode_object_count: HashMap<PBlankNode<A>, usize> = Default::default();

        'top: for t in v {
            if let PTerm::BlankNode(ref bn) = &t.object {
                bnode_object_count
                    .entry(bn.clone())
                    .and_modify(|e| *e += 1)
                    .or_insert(1);
            }

            // We have a collection end. Create a new seq and store it
            if t.is_collection_end() {
                seq.push(PTripleSeq::from_end(t));
                continue 'top;
            }

            // We have a collection part. Remember for later
            if t.is_collection_rest() {
                if let PTerm::BlankNode(ref bn) = &t.object {
                    seq_rest.insert(PNamedOrBlankNode::BlankNode(bn.clone()), t);
                }
                continue 'top;
            }
            if t.is_collection_first() {
                seq_first.insert(t.subject.clone(), t);
                continue 'top;
            }

            // We have something else. Combine it with existing multi
            // triples.
            if let Some(multi) = etv.get_mut(&t.subject) {
                multi.accept(t);
            } else {
                // We have an orphan triple, store it a new multi
                etv.insert(t.subject.clone(), t.into());
            }
        }

        // We grow the sequence form the beginning
        for s in seq.iter_mut() {
            loop {
                if let Some(t) = seq_first.remove(s.subject()) {
                    s.accept(t);
                }

                if let Some(t) = seq_rest.remove(s.subject()) {
                    s.accept(t);
                } else {
                    break;
                }
            }
        }

        let mut queue = VecDeque::with_capacity(etv.len() + seq.len());
        let mut store = HashMap::with_capacity(etv.len() + seq.len());

        for (subj, mt) in etv {
            queue.push_back((subj.clone(), PExpandedTripleKind::Multi));
            store.insert(subj, (Some(mt), None));
        }
        for s in seq {
            let subj = s.subject().clone();
            store.entry(subj.clone()).or_insert((None, None)).1 = Some(s);
            queue.push_back((subj, PExpandedTripleKind::Seq));
        }

        PChunk { queue, store, bnode_object_count }
    }

    pub fn empty() -> Self {
        PChunk {
            queue: VecDeque::new(),
            store: HashMap::new(),
            bnode_object_count: HashMap::new(),
        }
    }

    // I don't think we ever need this function
    pub fn sort(&mut self) {
        self.queue
            .make_contiguous()
            .sort_by(|(a_subj, a_kind), (b_subj, b_kind)| {
                match (a_kind, b_kind) {
                    (PExpandedTripleKind::Multi, PExpandedTripleKind::Seq) => {
                        return Ordering::Less;
                    }
                    (PExpandedTripleKind::Seq, PExpandedTripleKind::Multi) => {
                        return Ordering::Greater;
                    }
                    _ => {}
                }
                match (a_subj, b_subj) {
                    (PNamedOrBlankNode::NamedNode(_), PNamedOrBlankNode::BlankNode(_)) => {
                        Ordering::Less
                    }
                    (PNamedOrBlankNode::BlankNode(_), PNamedOrBlankNode::NamedNode(_)) => {
                        Ordering::Greater
                    }
                    _ => Ordering::Equal,
                }
            });
    }

    pub fn accept_or_push_back(&mut self, t: PTriple<A>) {
        if let Some(entry) = self.store.get_mut(&t.subject) {
            if let Some(mt) = &mut entry.0 {
                mt.accept(t);
                return;
            }
        }
        self.push_back(PExpandedTriple::PMultiTriple(t.into()));
    }

    pub fn push_back(&mut self, et: PExpandedTriple<A>) {
        let (subj, kind) = match et {
            PExpandedTriple::PMultiTriple(mt) => {
                let subj = mt.subject().clone();
                self.store.entry(subj.clone()).or_insert((None, None)).0 = Some(mt);
                (subj, PExpandedTripleKind::Multi)
            }
            PExpandedTriple::PTripleSeq(seq) => {
                let subj = seq.subject().clone();
                self.store.entry(subj.clone()).or_insert((None, None)).1 = Some(seq);
                (subj, PExpandedTripleKind::Seq)
            }
        };
        self.queue.push_back((subj, kind));
    }

    pub fn pop_front(&mut self) -> Option<PExpandedTriple<A>> {
        loop {
            let (subj, kind) = self.queue.pop_front()?;
            let (result, now_empty) = match self.store.get_mut(&subj) {
                None => (None, false),
                Some(entry) => {
                    let result = match kind {
                        PExpandedTripleKind::Multi => {
                            entry.0.take().map(PExpandedTriple::PMultiTriple)
                        }
                        PExpandedTripleKind::Seq => {
                            entry.1.take().map(PExpandedTriple::PTripleSeq)
                        }
                    };
                    let now_empty = entry.0.is_none() && entry.1.is_none();
                    (result, now_empty)
                }
            };
            if now_empty {
                self.store.remove(&subj);
            }
            if result.is_some() {
                return result;
            }
            // None means tombstone; continue to next queue entry
        }
    }

    fn take_subject(
        &mut self,
        bn: &PBlankNode<A>,
    ) -> (Option<PMultiTriple<A>>, Option<PTripleSeq<A>>) {
        let key = PNamedOrBlankNode::BlankNode(bn.clone());
        // Queue entries for this subject become tombstones, cleaned up lazily by pop_front
        self.store.remove(&key).unwrap_or((None, None))
    }

    fn object_count(&self, bn: &PBlankNode<A>) -> usize {
        self.bnode_object_count.get(bn).copied().unwrap_or(0)
    }
}

#[derive(Clone, Debug, Default)]
pub struct ChunkedRdfXmlFormatterConfig {
    indent: usize,
    base: Option<String>,
    prefix: IndexMap<String, String>,
}

impl ChunkedRdfXmlFormatterConfig {
    pub fn none() -> Self {
        ChunkedRdfXmlFormatterConfig {
            indent: 0,
            base: None,
            prefix: IndexMap::new(),
        }
    }
    pub fn all() -> Self {
        ChunkedRdfXmlFormatterConfig {
            indent: 4,
            base: None,
            prefix: IndexMap::new(),
        }
    }

    pub fn base(mut self, base: Option<String>) -> Self {
        self.base = base;
        self
    }

    pub fn prefix(mut self, indexmap: IndexMap<String, String>) -> Self {
        self.prefix = indexmap;
        self
    }

    pub fn indent(mut self, indent: usize) -> Self {
        self.indent = indent;
        self
    }
}

pub struct ChunkedRdfXmlFormatter<A: AsRef<str>, W: Write> {
    writer: Writer<W>,
    config: ChunkedRdfXmlFormatterConfig,
    pub(crate) open_tag_stack: Vec<Vec<u8>>,
    last_open_tag: Option<BytesStart<'static>>,
    chunk: PChunk<A>,
}

impl<A, W> ChunkedRdfXmlFormatter<A, W>
where
    A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq,
    W: Write,
{
    pub fn new(write: W, mut config: ChunkedRdfXmlFormatterConfig) -> Result<Self, io::Error> {
        config.prefix.insert(
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#".to_string(),
            "rdf".to_string(),
        );

        Self {
            writer: Writer::new_with_indent(write, b' ', config.indent),
            config,
            open_tag_stack: Default::default(),
            last_open_tag: None,
            chunk: PChunk::empty(),
        }
        .write_declaration()
    }

    fn write_declaration(mut self) -> Result<Self, io::Error> {
        self.write_event(Event::Decl(BytesDecl::new("1.0", Some("UTF-8"), None)))
            .map_err(map_err)?;
        let mut rdf_open = BytesStart::new("rdf:RDF");
        self.write_prefix(&mut rdf_open)?;
        self.write_event(Event::Start(rdf_open)).map_err(map_err)?;
        Ok(self)
    }

    fn write_prefix(&mut self, rdf_open: &mut BytesStart<'_>) -> Result<(), io::Error> {
        if let Some(ref base) = self.config.base {
            rdf_open.push_attribute(("xmlns", &base[..]));
        }
        for i in &self.config.prefix {
            let ns = format!("xmlns:{}", &i.1);
            rdf_open.push_attribute((&ns[..], &i.0[..]));
        }

        Ok(())
    }

    fn write_complete_open(&mut self) -> Result<(), quick_xml::Error> {
        if let Some(bs) = self.last_open_tag.take() {
            self.writer.write_event(Event::Start(bs))?;
        }
        self.last_open_tag = None;
        Ok(())
    }

    // Write a single event here.
    fn write_event(&mut self, event: Event<'_>) -> Result<(), quick_xml::Error> {
        self.write_complete_open()?;

        // If this is a start event, capture it, and hold it till the
        // next event. If the next event is a cognate close, send a Empty.
        self.writer.write_event(event)
    }

    fn write_start(&mut self, event: Event<'_>) -> Result<(), quick_xml::Error> {
        self.write_complete_open()?;
        match event {
            Event::Start(bs) => {
                self.open_tag_stack.push(bs.name().into_inner().to_vec());
                self.last_open_tag = Some(bs.to_owned());
            }
            _ => panic!("Only pass a start event to write start"),
        }
        Ok(())
    }

    fn write_close(&mut self) -> Result<(), io::Error> {
        let close = self.open_tag_stack.pop().ok_or(io::Error::new(
            io::ErrorKind::Other,
            "close when no close is available",
        ))?;

        //  println!("\nwrite_close:");
        if let Some(empty) = self.last_open_tag.take() {
            self.write_event(Event::Empty(empty)).map_err(map_err)
        } else {
            self.write_event(Event::End(BytesEnd::new(String::from_utf8_lossy(&close))))
                .map_err(map_err)
        }
    }

    fn bytes_start_iri<'a>(&mut self, nn: &'a PNamedNode<A>) -> BytesStart<'a> {
        let (iri_protocol_and_host, iri_qname) = nn.split_iri();
        if let Some(iri_ns_prefix) = &self.config.prefix.get(iri_protocol_and_host) {
            BytesStart::new(format!("{}:{}", &iri_ns_prefix, &iri_qname))
        } else {
            let mut bs = BytesStart::new(iri_qname);
            bs.push_attribute(("xmlns", iri_protocol_and_host));
            bs
        }
    }

    fn format_head<'a>(
        &mut self,
        mt: &'a PMultiTriple<A>,
        chunk: &PChunk<A>,
    ) -> Result<Vec<&'a PTriple<A>>, io::Error> {
        let mut triples_rendered = vec![];
        // oh dearie, dearie me! This is hideous
        let description_open = if let Some(typ) = mt.find_typed() {
            if let PTerm::NamedNode(ref nn) = &typ.object {
                triples_rendered.push(typ);
                let mut bs = self.bytes_start_iri(nn);
                if let PNamedOrBlankNode::BlankNode(ref bn) = &typ.subject {
                    if chunk.object_count(bn) > 1 {
                        bs.push_attribute(("rdf:nodeID", bn.as_ref()));
                    }
                }
                Some(bs)
            } else {
                None
            }
        } else {
            None
        };

        let mut description_open =
            description_open.unwrap_or_else(|| BytesStart::new("rdf:Description"));

        match mt.subject() {
            PNamedOrBlankNode::NamedNode(ref n) => {
                description_open.push_attribute(("rdf:about", n.iri.as_ref()))
            }
            PNamedOrBlankNode::BlankNode(_) => {
                // Empty
            }
        }

        // TODO: Shares lots of code with format_property
        // TODO: check all properties unique!!
        for literal_t in mt.literal_objects() {
            if let PTerm::Literal(l) = &literal_t.object {
                match l {
                    PLiteral::Simple { value } => {
                        let (iri_protocol_and_host, iri_qname) = literal_t.predicate.split_iri();

                        if let Some(iri_ns_prefix) = &self.config.prefix.get(iri_protocol_and_host)
                        {
                            description_open.push_attribute((
                                &format!("{}:{}", &iri_ns_prefix, &iri_qname)[..],
                                value.as_ref(),
                            ));
                            triples_rendered.push(literal_t);
                        }
                    }
                    PLiteral::LanguageTaggedString {
                        value: _,
                        language: _,
                    } => {
                        // Don't do anything here, because the
                        // language environment is wrong. Render later.
                    }
                    PLiteral::Typed {
                        value: _,
                        datatype: _,
                    } => {
                        // Don't do anything here because we need to
                        // render later.
                    }
                }
            } else {
                debug_assert!(
                    false,
                    "Non literal object returned from literal object method"
                );
            }
        }
        self.write_start(Event::Start(description_open))
            .map_err(map_err)?;

        Ok(triples_rendered)
    }

    fn format_object(
        &mut self,
        mut property_open: BytesStart<'_>,
        object: &PTerm<A>,
        chunk: &mut PChunk<A>,
        collection: bool,
    ) -> Result<(), io::Error> {
        match object {
            PTerm::NamedNode(n) => {
                // Rewrite: 2.4 Empty Property Elements
                if collection {
                    property_open.push_attribute(("rdf:about", n.iri.as_ref()));
                } else {
                    property_open.push_attribute(("rdf:resource", n.iri.as_ref()));
                }

                self.write_start(Event::Start(property_open))
                    .map_err(map_err)?;
            }
            PTerm::BlankNode(bn) => {
                if chunk.object_count(bn) == 1 {
                    match chunk.take_subject(bn) {
                        (None, Some(seq)) => {
                            if !seq.has_literal() {
                                property_open.push_attribute(("rdf:parseType", "Collection"));
                            }
                            self.write_start(Event::Start(property_open))
                                .map_err(map_err)?;
                            if seq.has_literal() {
                                self.format_seq_longhand(&seq, chunk)?;
                            } else {
                                self.format_seq_shorthand(&seq, chunk)?;
                            }
                            return Ok(());
                        }
                        (Some(mt), None) => {
                            self.write_start(Event::Start(property_open))
                                .map_err(map_err)?;
                            self.format_multi(&mt, chunk)?;
                            return Ok(());
                        }
                        (Some(mt), Some(seq)) => {
                            self.write_start(Event::Start(property_open))
                                .map_err(map_err)?;
                            // Put MT back so format_seq_longhand can merge the seq triples
                            // into it (preserving the rdf:type shorthand element name).
                            chunk.push_back(PExpandedTriple::PMultiTriple(mt));
                            self.format_seq_longhand(&seq, chunk)?;
                            return Ok(());
                        }
                        (None, None) => {}
                    }
                }
                property_open.push_attribute(("rdf:nodeID", bn.as_ref()));
                self.write_start(Event::Start(property_open))
                    .map_err(map_err)?;
            }
            PTerm::Literal(l) => {
                let content = match l {
                    PLiteral::Simple { value } => {
                        property_open.push_attribute((
                            "rdf:datatype",
                            "http://www.w3.org/2001/XMLSchema#string",
                        ));
                        value
                    }
                    PLiteral::LanguageTaggedString { value, language } => {
                        property_open.push_attribute(("xml:lang", language.as_ref()));
                        value
                    }
                    PLiteral::Typed { value, datatype } => {
                        property_open.push_attribute(("rdf:datatype", datatype.iri.as_ref()));
                        value
                    }
                };
                self.write_start(Event::Start(property_open))
                    .map_err(map_err)?;
                self.write_event(Event::Text(BytesText::new(&content.as_ref())))
                    .map_err(map_err)?;
            }
        };

        Ok(())
    }

    fn format_property_arc(
        &mut self,
        triple: &PTriple<A>,
        rendered_in_head: &Vec<&PTriple<A>>,
        chunk: &mut PChunk<A>,
    ) -> Result<(), io::Error> {
        if rendered_in_head.contains(&triple) {
            return Ok(());
        }

        let property_open = self.bytes_start_iri(&triple.predicate);
        self.format_object(property_open, &triple.object, chunk, false)?;

        self.write_close()?;
        Ok(())
    }

    fn format_seq_longhand(
        &mut self,
        seq: &PTripleSeq<A>,
        chunk: &mut PChunk<A>,
    ) -> Result<(), io::Error> {
        // We can't format seqs with literals in like this -- we need
        // to do long hand
        //if seq.has_literal() {
        let subj = seq.subject().clone();
        // Turn it into a set of triples
        let v: Vec<&PTriple<A>> = seq.triples();
        for i in v {
            chunk.accept_or_push_back(i.clone())
        }

        if let PNamedOrBlankNode::BlankNode(n) = subj {
            return match chunk.take_subject(&n) {
                (Some(mt), None) => {
                    self.format_removed_expanded(&PExpandedTriple::PMultiTriple(mt), chunk)
                }
                (None, Some(_seq)) => {
                    todo!("We shouldn't get here");
                }
                (Some(_mt), Some(_seq)) => {
                    todo!("We shouldn't get here");
                }
                _ => {
                    todo!("We shouldn't get here");
                }
            };
        } else {
            todo!("We shouldn't get here")
        }
    }

    fn format_seq_shorthand(
        &mut self,
        seq: &PTripleSeq<A>,
        chunk: &mut PChunk<A>,
    ) -> Result<(), io::Error> {
        for tup in seq.list_seq.iter() {
            if let Some(ref triple) = tup.1 {
                match &triple.object {
                    // Just render in place
                    PTerm::BlankNode(bn) => {
                        let (mt_opt, seq_opt) = chunk.take_subject(bn);
                        if let Some(mt) = mt_opt {
                            self.format_removed_expanded(
                                &PExpandedTriple::PMultiTriple(mt),
                                chunk,
                            )?;
                        }
                        if let Some(seq) = seq_opt {
                            self.format_removed_expanded(
                                &PExpandedTriple::PTripleSeq(seq),
                                chunk,
                            )?;
                        }
                    }
                    // render the object, but not the property which
                    // is the collection joiner
                    PTerm::NamedNode(_) => {
                        let property_open = BytesStart::new("rdf:Description");
                        self.format_object(property_open, &triple.object, chunk, true)?;
                        self.write_close()?;
                    }
                    any => {
                        dbg!(any);
                        todo!()
                    }
                }
            }
        }

        Ok(())
    }

    fn format_multi(
        &mut self,
        multi_triple: &PMultiTriple<A>,
        chunk: &mut PChunk<A>,
    ) -> Result<(), io::Error> {
        let rendered_in_head = self.format_head(multi_triple, chunk)?;

        // Rewrite: 2.3 Multiple Property Elements
        for triple in multi_triple.vec.iter() {
            self.format_property_arc(triple, &rendered_in_head, chunk)?;
        }

        self.write_close()?;
        Ok(())
    }

    fn format_removed_expanded(
        &mut self,
        expanded: &PExpandedTriple<A>,
        chunk: &mut PChunk<A>,
    ) -> Result<(), io::Error> {
        match expanded {
            PExpandedTriple::PMultiTriple(ref mt) => {
                self.format_multi(mt, chunk)?;
            }
            PExpandedTriple::PTripleSeq(ref seq) => {
                self.format_seq_longhand(seq, chunk)?;
            }
        }

        Ok(())
    }

    pub fn chunk_seq(&mut self, seq: PTripleSeq<A>) {
        self.chunk.push_back(seq.into())
    }

    pub fn chunk_triple(&mut self, triple: PTriple<A>) {
        self.chunk.push_back(triple.into());
    }

    pub fn chunk_multi(&mut self, multi: PMultiTriple<A>) {
        self.chunk.push_back(multi.into())
    }

    pub fn sort_chunk(&mut self) {
        self.chunk.sort()
    }

    pub fn finish_chunk(&mut self) -> Result<(), io::Error> {
        let mut chk = PChunk::empty();
        std::mem::swap(&mut self.chunk, &mut chk);
        self.format_chunk(chk)
    }

    pub fn format_chunk(&mut self, mut chunk: PChunk<A>) -> Result<(), io::Error> {
        loop {
            let optet = chunk.pop_front();
            if let Some(et) = optet {
                // If this is a blank node
                if let PNamedOrBlankNode::BlankNode(bn) = et.subject() {
                    // And there is later triple which will reference this as an object
                    if chunk.object_count(bn) == 1 {
                        // Don't render it here, but later
                        chunk.push_back(et);
                        continue;
                    }
                }

                self.format_removed_expanded(&et, &mut chunk)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// Finishes writing and returns the underlying `Write`
    pub fn finish(mut self) -> Result<W, io::Error> {
        while !self.open_tag_stack.is_empty() {
            self.write_close()?;
        }

        self.finish_chunk()?;

        self.write_event(Event::End(BytesEnd::new("rdf:RDF")))
            .map_err(map_err)?;

        Ok(self.writer.into_inner())
    }
}

pub trait RdfFormatter<A: AsRef<str>, W> {
    fn format(&mut self, triple: PTriple<A>) -> Result<(), io::Error>;

    fn finish(self) -> Result<W, io::Error>;
}

pub struct PrettyRdfXmlFormatter<A: AsRef<str> + Debug, W: Write>(
    ChunkedRdfXmlFormatter<A, W>,
    pub Vec<PTriple<A>>,
);

impl<A, W> PrettyRdfXmlFormatter<A, W>
where
    A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq,
    W: Write,
{
    pub fn new(write: W, config: ChunkedRdfXmlFormatterConfig) -> Result<Self, io::Error> {
        Ok(PrettyRdfXmlFormatter(
            ChunkedRdfXmlFormatter::new(write, config)?,
            vec![],
        ))
    }

    pub fn triples(&self) -> Vec<PTriple<A>> {
        self.1.clone()
    }
}

impl<A: AsRef<str> + Clone + Debug + Eq + Hash, W: Write> RdfFormatter<A, W>
    for PrettyRdfXmlFormatter<A, W>
{
    fn format(&mut self, triple: PTriple<A>) -> Result<(), io::Error> {
        let _ = &self.1.push(triple);
        Ok(())
    }

    fn finish(mut self) -> Result<W, io::Error> {
        let chk = PChunk::normalize(self.1);
        self.0.format_chunk(chk)?;
        self.0.finish()
    }
}

pub struct NonPrettyRdfXmlFormatter<A: AsRef<str> + Debug, W: Write>(ChunkedRdfXmlFormatter<A, W>);

impl<A, W> NonPrettyRdfXmlFormatter<A, W>
where
    A: AsRef<str> + Clone + Debug + Eq + Hash + PartialEq,
    W: Write,
{
    pub fn new(write: W, config: ChunkedRdfXmlFormatterConfig) -> Result<Self, io::Error> {
        Ok(NonPrettyRdfXmlFormatter(ChunkedRdfXmlFormatter::new(
            write, config,
        )?))
    }
}

impl<A, W> RdfFormatter<A, W> for NonPrettyRdfXmlFormatter<A, W>
where
    A: AsRef<str> + Clone + Debug + Eq + Hash,
    W: Write,
{
    fn format(&mut self, triple: PTriple<A>) -> Result<(), io::Error> {
        self.0.chunk_triple(triple);
        self.0.finish_chunk()?;

        Ok(())
    }

    fn finish(self) -> Result<W, io::Error> {
        self.0.finish()
    }
}

#[cfg(test)]
mod test {
    use indexmap::{indexmap, IndexMap};

    use oxrdf::{NamedNodeRef, TripleRef};
    use oxrdfio::RdfParser;
    use pretty_assertions::assert_eq;

    use super::{
        ChunkedRdfXmlFormatter, ChunkedRdfXmlFormatterConfig, PBlankNode, PChunk, PExpandedTriple,
        PNamedNode, PTriple,
    };

    fn tnn() -> PTriple<String> {
        PTriple {
            subject: PNamedNode::new("http://example.com/s".to_string()).into(),
            predicate: PNamedNode::new("http://example.com/p".to_string()).into(),
            object: PNamedNode::new("http://example.com/o".to_string()).into(),
        }
    }

    fn tnn1() -> PTriple<String> {
        PTriple {
            subject: PNamedNode::new("http://example.com/s1".to_string()).into(),
            predicate: PNamedNode::new("http://example.com/p1".to_string()).into(),
            object: PNamedNode::new("http://example.com/o1".to_string()).into(),
        }
    }

    fn bnn() -> PTriple<String> {
        PTriple {
            subject: PBlankNode::new("hello_id".to_string()).into(),
            predicate: PNamedNode::new("http://example.com/p".to_string()).into(),
            object: PNamedNode::new("http://example.com/o".to_string()).into(),
        }
    }

    fn some_seq() -> PChunk<String> {
        PChunk::normalize(vec![
            PTriple {
                subject: PBlankNode::new("seq0".to_string()).into(),
                predicate: PNamedNode::new("http://example.com/p".to_string()).into(),
                object: PNamedNode::new("http://example.com/o".to_string()).into(),
            },
            PTriple {
                subject: PBlankNode::new("seq0".to_string()).into(),
                predicate: PNamedNode::new(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
                        .to_string()
                        .into(),
                ),
                object: PBlankNode::new("seq1".to_string()).into(),
            },
            PTriple {
                subject: PBlankNode::new("seq0".to_string()).into(),
                predicate: PNamedNode::new(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
                        .to_string()
                        .into(),
                ),
                object: PNamedNode::new(
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil".to_string(),
                )
                .into(),
            },
        ])
    }

    #[test]
    pub fn rio_conversion() {
        // Test addded because of failure to compile horned-triples
        // which seemed to argue that this .into conversion was not
        // possible.
        let _: PTriple<String> = TripleRef {
            subject: NamedNodeRef::new_unchecked("http://example.com/foo").into(),
            predicate: NamedNodeRef::new_unchecked("http://schema.org/sameAs").into(),
            object: NamedNodeRef::new_unchecked("http://example.com/foo").into(),
        }
        .into();

        assert!(true);
    }

    #[test]
    pub fn chunk_hello_world() {
        assert!(true)
    }

    #[test]
    pub fn simple_chunk() {
        let chk = PChunk::normalize(vec![tnn()]);

        assert_eq!(chk.queue.len(), 1);
    }

    #[test]
    pub fn multi_chunk() {
        let chk = PChunk::normalize(vec![tnn(), tnn(), tnn()]);

        assert_eq!(chk.queue.len(), 1);
    }

    #[test]
    pub fn multi_chunk_sort_stable() {
        let mut chk: PChunk<String> = PChunk::empty();
        chk.push_back(tnn().into());
        chk.push_back(tnn1().into());
        chk.sort();

        assert_eq!(chk.pop_front(), Some(tnn().into()));
        assert_eq!(chk.pop_front(), Some(tnn1().into()));

        let mut chk: PChunk<String> = PChunk::empty();
        chk.push_back(tnn1().into());
        chk.push_back(tnn().into());
        chk.sort();

        assert_eq!(chk.pop_front(), Some(tnn1().into()));
        assert_eq!(chk.pop_front(), Some(tnn().into()));
    }

    #[test]
    pub fn multi_chunk_sort() {
        // Get an seq that we made earlier
        let mut s = some_seq();
        s.pop_front();
        let s = s.pop_front().unwrap();

        let mut chk: PChunk<String> = PChunk::empty();

        chk.push_back(s);
        chk.push_back(bnn().into());
        chk.push_back(tnn().into());

        chk.sort();

        assert_eq!(chk.pop_front(), Some(tnn().into()));
        assert_eq!(chk.pop_front(), Some(bnn().into()));
        assert!(matches! {
            chk.pop_front(), Some(PExpandedTriple::PTripleSeq(_))
        });
    }

    #[test]
    pub fn multi_chunk_find_subject_with_seq() {
        let mut chk = some_seq();

        let sub = chk.take_subject(&PBlankNode::new("seq0".to_string()));

        assert!(matches! {
            sub,
            (Some(_), Some(_))
        })
    }

    fn spec_prefix() -> IndexMap<&'static str, &'static str> {
        indexmap![
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
            "http://purl.org/dc/elements/1.1/" => "dc",
            "http://example.org/stuff/1.0/" => "ex"
        ]
    }

    #[allow(dead_code)]
    fn from_nt(nt: &str) -> Result<String, Box<dyn std::error::Error>> {
        from_nt_prefix(
            nt,
            indexmap!("http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf"),
        )
    }

    fn from_nt_prefix(
        nt: &str,
        prefix: IndexMap<&str, &str>,
    ) -> Result<String, Box<dyn std::error::Error>> {
        let source: Vec<PTriple<String>> = RdfParser::from_format(oxrdfio::RdfFormat::NTriples)
            .for_reader(nt.as_bytes())
            .map(Result::unwrap)
            .map(Into::into)
            .collect();

        let sink = vec![];

        let mut config = ChunkedRdfXmlFormatterConfig::all();
        config.prefix = prefix
            .into_iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        let mut f = ChunkedRdfXmlFormatter::new(sink, config)?;
        let chk = PChunk::normalize(source);
        f.format_chunk(chk)?;

        let w = f.finish()?;
        let s = String::from_utf8(w)?;
        println!("XML Out {}", s);
        Ok(s)
    }

    fn nt_xml_roundtrip_prefix(nt: &str, xml: &str, prefix: IndexMap<&str, &str>) {
        assert_eq!(from_nt_prefix(nt, prefix).unwrap(), xml);
    }

    #[allow(dead_code)]
    fn nt_xml_roundtrip(nt: &str, xml: &str) {
        assert_eq!(from_nt(nt).unwrap(), xml);
    }

    fn xml_roundtrip(
        xml: &str,
        prefix: Option<IndexMap<&str, &str>>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        xml_from_to(xml, xml, prefix)
    }

    fn xml_from_to(
        xml_from: &str,
        xml_to: &str,
        prefix: Option<IndexMap<&str, &str>>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let source: Vec<PTriple<String>> =
            RdfParser::from_format(oxrdfio::RdfFormat::RdfXml)
                .for_reader(xml_from.as_bytes())
                .map(Result::unwrap)
                .map(Into::into)
                .collect();

        let sink = vec![];

        let prefix = prefix.unwrap_or_else(|| {
            indexmap![
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf"
            ]
        });
        let prefix = prefix
            .into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect();

        let config = ChunkedRdfXmlFormatterConfig::all()
            .base(Some("http://www.example.com/iri#".into()))
            .prefix(prefix);

        let mut f = ChunkedRdfXmlFormatter::new(sink, config)?;
        let mut chk = PChunk::normalize(source);
        chk.sort();
        f.format_chunk(chk)?;

        let w = f.finish()?;
        let roundxml = String::from_utf8(w)?;
        println!("XML_from:\n{}\n", xml_from);
        println!("XML_to:\n{}\n", xml_to);
        println!("Round:\n{}", roundxml);

        assert_eq!(xml_to, roundxml);

        Ok(())
    }

    #[test]
    fn example4_single_triple() {
        nt_xml_roundtrip_prefix(
            r###"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF1.1 XML Syntax" .
"###,
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar" dc:title="RDF1.1 XML Syntax"/>
</rdf:RDF>"###,
            spec_prefix(),
        )
    }

    #[test]
    fn example4_multiple_property_elements() {
        nt_xml_roundtrip_prefix(
            r###"<http://www.w3.org/TR/rdf-syntax-grammar> <http://purl.org/dc/elements/1.1/title> "RDF1.1 XML Syntax" .
<http://www.w3.org/TR/rdf-syntax-grammar> <http://example.org/stuff/1.0/editor> _:genid1 .
_:genid1 <http://example.org/stuff/1.0/fullName> "Dave Beckett" .
_:genid1 <http://example.org/stuff/1.0/homePage> <http://purl.org/net/dajobe/> ."###,
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar" dc:title="RDF1.1 XML Syntax">
        <ex:editor>
            <rdf:Description ex:fullName="Dave Beckett">
                <ex:homePage rdf:resource="http://purl.org/net/dajobe/"/>
            </rdf:Description>
        </ex:editor>
    </rdf:Description>
</rdf:RDF>"###,
            spec_prefix(),
        );
    }

    #[test]
    fn example14_typed_nodes() {
        nt_xml_roundtrip_prefix(
            r###"<http://example.org/thing> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/stuff/1.0/Document> .
<http://example.org/thing> <http://purl.org/dc/elements/1.1/title> "A marvelous thing" ."###,
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <ex:Document rdf:about="http://example.org/thing" dc:title="A marvelous thing"/>
</rdf:RDF>"###,
            spec_prefix(),
        )
    }

    #[test]
    fn example19_collections() {
        nt_xml_roundtrip_prefix(
            r###"_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/banana> .
_:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/apple> .
_:genid1 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid2 .
_:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#first> <http://example.org/pear> .
_:genid2 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> _:genid3 .
_:genid3 <http://www.w3.org/1999/02/22-rdf-syntax-ns#rest> <http://www.w3.org/1999/02/22-rdf-syntax-ns#nil> .
<http://example.org/basket> <http://example.org/stuff/1.0/hasFruit> _:genid1 ."###,
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://example.org/basket">
        <ex:hasFruit rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.org/banana"/>
            <rdf:Description rdf:about="http://example.org/apple"/>
            <rdf:Description rdf:about="http://example.org/pear"/>
        </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"###,
            spec_prefix(),
        )
    }

    #[test]
    fn example4_xml_roundtrip() {
        // Test the XML roundtrip machinary
        xml_roundtrip(
r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
    <rdf:Description rdf:about="http://www.w3.org/TR/rdf-syntax-grammar">
        <title xmlns="http://purl.org/dc/elements/1.1/" rdf:datatype="http://www.w3.org/2001/XMLSchema#string">RDF1.1 XML Syntax</title>
    </rdf:Description>
</rdf:RDF>"###,
            None
        ).unwrap();
    }

    // Seq Handling
    #[test]
    fn seq_simple() {
        xml_roundtrip(
        r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://example.org/basket">
        <ex:hasFruit rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.org/banana"/>
            <rdf:Description rdf:about="http://example.org/apple"/>
            <rdf:Description rdf:about="http://example.org/pear"/>
        </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://purl.org/dc/elements/1.1/" => "dc",
                    "http://example.org/stuff/1.0/" => "ex"
                ]
            )
        ).unwrap();
    }

    #[test]
    fn seq_longhand() {
        xml_from_to(
                r###"<?xml version="1.0"?>
<rdf:RDF  xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
   <rdf:Description rdf:about="http://example.org/basket">
       <ex:hasFruit>
           <rdf:Description>
              <rdf:first rdf:resource="http://example.org/banana"/>
              <rdf:rest>
                 <rdf:Description>
                    <rdf:first rdf:resource="http://example.org/apple"/>
                    <rdf:rest>
                        <rdf:Description>
                            <rdf:first rdf:resource="http://example.org/pear"/>
                            <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
                        </rdf:Description>
                    </rdf:rest>
                 </rdf:Description>
              </rdf:rest>
           </rdf:Description>
       </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"###,
        r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://example.org/basket">
        <ex:hasFruit rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.org/banana"/>
            <rdf:Description rdf:about="http://example.org/apple"/>
            <rdf:Description rdf:about="http://example.org/pear"/>
        </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://purl.org/dc/elements/1.1/" => "dc",
                    "http://example.org/stuff/1.0/" => "ex"
                ]
            )
        ).unwrap();
    }

    #[test]
    fn seq_longhand_with_type_declaration() {
        xml_from_to(
                r###"<?xml version="1.0"?>
<rdf:RDF  xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
   <rdf:Description rdf:about="http://example.org/basket">
       <ex:hasFruit>
           <rdf:Description>
              <rdf:type rdf:resource="http://example.org/fruitList"/>
              <rdf:first rdf:resource="http://example.org/banana"/>
              <rdf:rest>
                 <rdf:Description>
                    <rdf:type rdf:resource="http://example.org/fruitList"/>
                    <rdf:first rdf:resource="http://example.org/apple"/>
                    <rdf:rest>
                        <rdf:Description>
                            <rdf:type rdf:resource="http://example.org/fruitList"/>
                            <rdf:first rdf:resource="http://example.org/pear"/>
                            <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
                        </rdf:Description>
                    </rdf:rest>
                 </rdf:Description>
              </rdf:rest>
           </rdf:Description>
       </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"###,
                r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://example.org/basket">
        <ex:hasFruit>
            <fruitList xmlns="http://example.org/">
                <rdf:first rdf:resource="http://example.org/banana"/>
                <rdf:rest>
                    <fruitList xmlns="http://example.org/">
                        <rdf:first rdf:resource="http://example.org/apple"/>
                        <rdf:rest>
                            <fruitList xmlns="http://example.org/">
                                <rdf:first rdf:resource="http://example.org/pear"/>
                                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
                            </fruitList>
                        </rdf:rest>
                    </fruitList>
                </rdf:rest>
            </fruitList>
        </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://purl.org/dc/elements/1.1/" => "dc",
                    "http://example.org/stuff/1.0/" => "ex"
                ]
            )
            ).unwrap();
    }

    /// I don't know if this is valid at all at the moment
    /// nor what it should serialize as
    #[test]
    #[ignore]
    fn seq_longhand_with_literal() {
        xml_from_to(
                r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://example.org/basket">
        <ex:hasFruit rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.org/banana">
                 <rdf:value rdf:datatype="string">Yellow</rdf:value>
            </rdf:Description>
            <rdf:Description rdf:about="http://example.org/apple">
                 <rdf:value>Red</rdf:value>
            </rdf:Description>
            <rdf:Description rdf:about="http://example.org/pear">
                 <rdf:value>Green</rdf:value>
            </rdf:Description>
        </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"###,
        r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:ex="http://example.org/stuff/1.0/">
    <rdf:Description rdf:about="http://example.org/basket">
        <ex:hasFruit rdf:parseType="Collection">
            <rdf:Description rdf:about="http://example.org/banana"/>
            <rdf:Description rdf:about="http://example.org/apple"/>
            <rdf:Description rdf:about="http://example.org/pear"/>
        </ex:hasFruit>
    </rdf:Description>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://purl.org/dc/elements/1.1/" => "dc",
                    "http://example.org/stuff/1.0/" => "ex"
                ]
            )
        ).unwrap();
    }

    // Following Tests are all from specific bugs mostly found from developing horned-owl
    #[test]
    fn double_rdf_tag() {
        // Cut down from swrl_rule_basic test
        // This was producing a tag inside a tag
        xml_roundtrip(
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
</rdf:RDF>"###,
            None
        ).unwrap()
    }

    #[test]
    fn swrl_rule_basic() {
        // Test from Horned-OWL that I am struggling to roundtrip, so test the RDF serialization
        xml_roundtrip(r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
    <owl:Ontology rdf:about="http://www.example.com/iri">
        <owl:versionIRI rdf:resource="http://www.example.com/viri"/>
    </owl:Ontology>
    <owl:Class rdf:about="http://www.example.com/iri#A"/>
    <owl:Class rdf:about="http://www.example.com/iri#B"/>
    <swrl:Variable rdf:about="http://www.example.com/iri#x"/>
    <swrl:Imp>
        <swrl:body>
            <swrl:AtomList>
                <rdf:first>
                    <swrl:ClassAtom>
                        <swrl:classPredicate rdf:resource="http://www.example.com/iri#A"/>
                        <swrl:argument1 rdf:resource="http://www.example.com/iri#x"/>
                    </swrl:ClassAtom>
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:body>
        <swrl:head>
            <swrl:AtomList>
                <rdf:first>
                    <swrl:ClassAtom>
                        <swrl:classPredicate rdf:resource="http://www.example.com/iri#B"/>
                        <swrl:argument1 rdf:resource="http://www.example.com/iri#x"/>
                    </swrl:ClassAtom>
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:head>
    </swrl:Imp>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://www.w3.org/2002/07/owl#" => "owl",
                    "http://www.w3.org/2003/11/swrl#" => "swrl"
                ]
            )
        ).unwrap()
    }

    #[test]
    fn swrl_rule_basic_minimal() {
        // Cut down test from swrl_rule_basic test to isolate the problem
        xml_roundtrip(r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
    <swrl:Imp>
        <swrl:body>
            <swrl:AtomList>
                <rdf:first>
                    <swrl:ClassAtom/>
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:body>
    </swrl:Imp>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://www.w3.org/2003/11/swrl#" => "swrl"
                ]
            )
        ).unwrap()
    }

    /// This test checks whether bnodes which can elided actually
    /// are. In this case, the complex ClassAtom bnode should be
    /// pulled into the AtomList
    #[test]
    fn list_with_bnode_pull_in_backwards() {
        xml_from_to(
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
    <swrl:Imp>
        <swrl:head>
            <swrl:AtomList>
                <rdf:first rdf:nodeID="bn1">
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:head>
    </swrl:Imp>
    <swrl:ClassAtom rdf:nodeID="bn1">
         <swrl:classPredicate rdf:resource="http://www.example.com/iri#B"/>
          <swrl:argument1 rdf:resource="http://www.example.com/iri#x"/>
    </swrl:ClassAtom>
</rdf:RDF>"###,
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
    <swrl:Imp>
        <swrl:head>
            <swrl:AtomList>
                <rdf:first>
                    <swrl:ClassAtom>
                        <swrl:classPredicate rdf:resource="http://www.example.com/iri#B"/>
                        <swrl:argument1 rdf:resource="http://www.example.com/iri#x"/>
                    </swrl:ClassAtom>
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:head>
    </swrl:Imp>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://www.w3.org/2002/07/owl#" => "owl",
                    "http://www.w3.org/2003/11/swrl#" => "swrl"
                ]
            )
        ).unwrap()
    }

    /// Similar to the last test, we check to see whether the bnode is
    /// elided. However, in this case, we change the order around so
    /// that the bnode triples appear before the list.
    #[test]
    fn list_with_bnode_pull_in_forward() {
        xml_from_to(
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
    <swrl:ClassAtom rdf:nodeID="bn1">
         <swrl:classPredicate rdf:resource="http://www.example.com/iri#B"/>
          <swrl:argument1 rdf:resource="http://www.example.com/iri#x"/>
    </swrl:ClassAtom>
    <swrl:Imp>
        <swrl:head>
            <swrl:AtomList>
                <rdf:first rdf:nodeID="bn1">
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:head>
    </swrl:Imp>
</rdf:RDF>"###,
            r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
    <swrl:Imp>
        <swrl:head>
            <swrl:AtomList>
                <rdf:first>
                    <swrl:ClassAtom>
                        <swrl:classPredicate rdf:resource="http://www.example.com/iri#B"/>
                        <swrl:argument1 rdf:resource="http://www.example.com/iri#x"/>
                    </swrl:ClassAtom>
                </rdf:first>
                <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
            </swrl:AtomList>
        </swrl:head>
    </swrl:Imp>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://www.w3.org/2002/07/owl#" => "owl",
                    "http://www.w3.org/2003/11/swrl#" => "swrl"
                ]
            )
        ).unwrap()
    }

    /// The bnode genid1 cannot be elided here when we render the
    /// restriction even though it normally would be because of the
    /// reference of it from annotatedTarget.
    #[test]
    fn non_elidable_bnode() {
        xml_roundtrip(
    r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
    <owl:Class rdf:about="http://www.example.com/iri#B">
        <rdfs:subClassOf rdf:nodeID="genid1"/>
    </owl:Class>
    <owl:Restriction rdf:nodeID="genid1">
        <owl:onProperty rdf:resource="http://www.example.com/iri#r"/>
        <owl:someValuesFrom rdf:resource="http://www.example.com/iri#A"/>
    </owl:Restriction>
    <owl:Axiom>
        <owl:annotatedSource rdf:resource="http://www.example.com/iri#B"/>
        <owl:annotatedProperty rdf:resource="http://www.w3.org/2000/01/rdf-schema#subClassOf"/>
        <owl:annotatedTarget rdf:nodeID="genid1"/>
        <rdfs:comment xml:lang="en">Annotation on subclass axiom</rdfs:comment>
    </owl:Axiom>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://www.w3.org/2000/01/rdf-schema#" => "rdfs",
                    "http://www.w3.org/2002/07/owl#" => "owl",
                    "http://www.w3.org/2003/11/swrl#" => "swrl"
                ]
            )
        ).unwrap()
    }

    /// I think the problem here is that the type AtomList triple is being rendered as a short cut
    /// and when this happens the object pull in is not happening
    #[test]
    #[ignore]
    fn seq_with_pull_in_bnode() {
        xml_roundtrip(
    r###"<?xml version="1.0" encoding="UTF-8"?>
<rdf:RDF xmlns="http://www.example.com/iri#" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#" xmlns:owl="http://www.w3.org/2002/07/owl#" xmlns:swrl="http://www.w3.org/2003/11/swrl#">
    <rdf:Description>
        <rdf:type rdf:resource="http://www.w3.org/2003/11/swrl#Imp"/>
        <swrl:body>
            <rdf:Description>
                <rdf:type rdf:resource="http://www.w3.org/2003/11/swrl#AtomList"/>
                <rdf:first>
                    <rdf:Description>
                        <rdf:type rdf:resource="http://www.w3.org/2003/11/swrl#ClassAtom"/>
                        <swrl:classPredicate rdf:resource="http://www.example.com/iri#A1"/>
                        <swrl:argument1 rdf:resource="http://www.example.com/iri#x"/>
                    </rdf:Description>
                </rdf:first>
                <rdf:rest>
                    <rdf:Description>
                        <rdf:type rdf:resource="http://www.w3.org/2003/11/swrl#AtomList"/>
                        <rdf:first>
                            <rdf:Description>
                                <rdf:type rdf:resource="http://www.w3.org/2003/11/swrl#ClassAtom"/>
                                <swrl:classPredicate rdf:resource="http://www.example.com/iri#A"/>
                                <swrl:argument1 rdf:resource="http://www.example.com/iri#x"/>
                            </rdf:Description>
                        </rdf:first>
                        <rdf:rest rdf:resource="http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"/>
                    </rdf:Description>
                </rdf:rest>
            </rdf:Description>
        </swrl:body>
     </rdf:Description>
</rdf:RDF>"###,
            Some(
                indexmap![
                    "http://www.w3.org/1999/02/22-rdf-syntax-ns#" => "rdf",
                    "http://www.w3.org/2000/01/rdf-schema#" => "rdfs",
                    "http://www.w3.org/2002/07/owl#" => "owl",
                    "http://www.w3.org/2003/11/swrl#" => "swrl"
                ]
            )
        ).unwrap()
    }
}
