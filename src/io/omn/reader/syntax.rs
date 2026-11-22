//! The Manchester Syntax grammar as a hand-written parser, a step for step
//! port of `horned-pest/src/grammars/omn.pest`.
//!
//! The pest grammar is a PEG, so ordered choice commits and repetition does
//! not backtrack into itself. Each function here follows one rule and tries
//! the same alternatives in the same order, so it accepts exactly the same
//! text. The result is a tree that borrows from the input and holds no
//! resolved IRIs; [`super::convert`] turns it into model values, as the
//! `from_pair` functions do for the pest reader.
//!
//! Whitespace and comments are skipped before every token, which is what
//! pest does implicitly between the elements of a non-atomic rule.
use std::cell::RefCell;

use crate::io::ofn::reader::bcp47::language_tag_len;
use crate::io::ofn::reader::lex::{pn_chars, pn_local, pname, pname_ns};
use crate::io::ofn::reader::rfc3987::is_iri;

/// The text left after a successful parse, and what was parsed.
pub type Res<'a, T> = Option<(&'a str, T)>;

/// The marker that starts the trailing functional syntax block.
pub const GENERAL_AXIOMS: &str = "# General axioms";

/// Skip whitespace and comments, but not the `# General axioms` marker.
pub fn sp(mut i: &str) -> &str {
    loop {
        i = i.trim_start_matches([' ', '\t', '\n', '\r']);
        if i.starts_with('#') && !i.starts_with(GENERAL_AXIOMS) {
            i = i.find(['\n', '\r']).map_or(&i[i.len()..], |p| &i[p..]);
        } else {
            return i;
        }
    }
}

// ---------------------------------------------------------------------------
// The tree

/// An IRI as written, not yet resolved against the prefixes.
#[derive(Debug, Clone)]
pub struct Iri<'a> {
    pub kind: IriKind<'a>,
    /// The text of the IRI, which locates errors.
    pub text: &'a str,
}

#[derive(Debug, Clone)]
pub enum IriKind<'a> {
    /// `<...>`, the text between the brackets.
    Full(&'a str),
    /// `prefix:local`.
    Abbrev(&'a str, &'a str),
    /// A bare local name, in the default namespace.
    Simple(&'a str),
}

#[derive(Debug, Clone)]
pub enum Ind<'a> {
    /// `_:label`, as written.
    Anon(&'a str),
    Named(Iri<'a>),
}

#[derive(Debug, Clone)]
pub enum Lit<'a> {
    Typed(String, Iri<'a>),
    Lang(String, &'a str),
    Simple(String),
    Integer(&'a str),
    Decimal(&'a str),
    Float(&'a str),
    Boolean(&'a str),
}

#[derive(Debug, Clone)]
pub struct Ope<'a> {
    pub inverse: bool,
    pub iri: Iri<'a>,
}

#[derive(Debug, Clone)]
pub struct Desc<'a>(pub Vec<Conj<'a>>);

#[derive(Debug, Clone)]
pub enum Conj<'a> {
    /// `ClassIRI that [not] Restriction (and [not] Restriction)*`
    That(Iri<'a>, Vec<(bool, Restr<'a>)>),
    And(Vec<Prim<'a>>),
}

#[derive(Debug, Clone)]
pub struct Prim<'a> {
    pub not: bool,
    pub body: PrimBody<'a>,
}

#[derive(Debug, Clone)]
pub enum PrimBody<'a> {
    Restr(Box<Restr<'a>>),
    OneOf(Vec<Ind<'a>>),
    Paren(Box<Desc<'a>>),
    Class(Iri<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RKw {
    Some,
    Only,
    Value,
    SelfKw,
    Min,
    Max,
    Exactly,
}

#[derive(Debug, Clone)]
pub enum Prop<'a> {
    /// One of the arms that name a data property.
    Data(Iri<'a>),
    Obj(Ope<'a>),
}

#[derive(Debug, Clone)]
pub enum Filler<'a> {
    None,
    Prim(Box<Prim<'a>>),
    Dr(SynDataRange<'a>),
    Lit(Lit<'a>),
    Ind(Ind<'a>),
}

#[derive(Debug, Clone)]
pub struct Restr<'a> {
    /// The text of the whole restriction.
    pub text: &'a str,
    pub prop: Prop<'a>,
    pub kw: RKw,
    pub card: Option<&'a str>,
    pub filler: Filler<'a>,
}

#[derive(Debug, Clone)]
pub struct SynDataRange<'a>(pub Vec<DataConj<'a>>);

#[derive(Debug, Clone)]
pub struct DataConj<'a>(pub Vec<DataPrim<'a>>);

#[derive(Debug, Clone)]
pub struct DataPrim<'a> {
    pub not: bool,
    pub atom: DataAtom<'a>,
}

#[derive(Debug, Clone)]
pub enum DataAtom<'a> {
    OneOf(Vec<Lit<'a>>),
    Restriction(Iri<'a>, Vec<Facet<'a>>),
    Paren(Box<SynDataRange<'a>>),
    Datatype(Iri<'a>),
}

#[derive(Debug, Clone)]
pub struct Facet<'a> {
    pub symbol: &'a str,
    pub lit: Lit<'a>,
}

#[derive(Debug, Clone)]
pub struct Annotations<'a>(pub Vec<AnnEntry<'a>>);

#[derive(Debug, Clone)]
pub struct AnnEntry<'a> {
    pub nested: Option<Annotations<'a>>,
    pub property: Iri<'a>,
    pub target: Target<'a>,
}

#[derive(Debug, Clone)]
pub enum Target<'a> {
    Lit(Lit<'a>),
    Anon(&'a str),
    Iri(Iri<'a>),
}

/// `[Annotations:] item { , [Annotations:] item }`
#[derive(Debug, Clone)]
pub struct AnnList<'a, T>(pub Vec<(Option<Annotations<'a>>, T)>);

#[derive(Debug, Clone)]
pub struct Fact<'a> {
    pub not: bool,
    pub ope: Ope<'a>,
    pub target: FactTarget<'a>,
}

#[derive(Debug, Clone)]
pub enum FactTarget<'a> {
    Lit(Lit<'a>),
    Ind(Ind<'a>),
}

/// The clause keywords of every frame.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ck {
    Annotations,
    SubClassOf,
    EquivalentTo,
    DisjointWith,
    DisjointUnionOf,
    HasKey,
    SubPropertyChain,
    SubPropertyOf,
    InverseOf,
    Domain,
    Range,
    Characteristics,
    Types,
    Facts,
    SameAs,
    DifferentFrom,
}

#[derive(Debug, Clone)]
pub enum Body<'a> {
    Annotations(Annotations<'a>),
    Descs(AnnList<'a, Desc<'a>>),
    Opes(AnnList<'a, Ope<'a>>),
    Iris(AnnList<'a, Iri<'a>>),
    Inds(AnnList<'a, Ind<'a>>),
    DataRanges(Vec<SynDataRange<'a>>),
    DataRange(SynDataRange<'a>),
    Chain(Vec<Ope<'a>>),
    Keys(Vec<Ope<'a>>),
    Chars(Vec<&'a str>),
    Facts(Vec<Fact<'a>>),
}

#[derive(Debug, Clone)]
pub struct Clause<'a> {
    pub kw: Ck,
    /// The `Annotations:` between the keyword and the list.
    pub ann: Option<Annotations<'a>>,
    pub body: Body<'a>,
}

#[derive(Debug, Clone)]
pub enum Subject<'a> {
    Desc(Desc<'a>),
    Ope(Ope<'a>),
    Iri(Iri<'a>),
    Ind(Ind<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FrameKind {
    Class,
    ObjectProperty,
    DataProperty,
    AnnotationProperty,
    Individual,
    Datatype,
}

#[derive(Debug, Clone)]
pub struct Frame<'a> {
    pub kind: FrameKind,
    /// `Annotations:` before a class subject.
    pub subject_ann: Option<Annotations<'a>>,
    pub subject: Subject<'a>,
    pub clauses: Vec<Clause<'a>>,
}

#[derive(Debug, Clone)]
pub enum SynAtom<'a> {
    SameAs(Arg<'a>, Arg<'a>),
    DifferentFrom(Arg<'a>, Arg<'a>),
    /// A predicate and one or more arguments.
    Pred(Desc<'a>, Vec<Arg<'a>>),
}

#[derive(Debug, Clone)]
pub enum Arg<'a> {
    Var(Iri<'a>),
    Lit(Lit<'a>),
    Ind(Ind<'a>),
}

#[derive(Debug, Clone)]
pub struct RuleFrame<'a> {
    pub ann: Option<Annotations<'a>>,
    pub body: Vec<SynAtom<'a>>,
    pub head: Vec<SynAtom<'a>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MiscKind {
    EquivalentClasses,
    DisjointClasses,
    EquivalentProperties,
    DisjointProperties,
    SameIndividual,
    DifferentIndividuals,
}

#[derive(Debug, Clone)]
pub struct Misc<'a> {
    pub kind: MiscKind,
    pub ann: Option<Annotations<'a>>,
    pub body: Body<'a>,
}

#[derive(Debug, Clone)]
pub struct Header<'a> {
    pub iri: Option<Iri<'a>>,
    pub version: Option<Iri<'a>>,
    pub imports: Vec<Iri<'a>>,
    pub annotations: Vec<Annotations<'a>>,
}

#[derive(Debug, Clone)]
pub enum Unit<'a> {
    Prefix(&'a str, &'a str),
    Header(Header<'a>),
    Frame(Box<Frame<'a>>),
    Rule(Box<RuleFrame<'a>>),
    Misc(Box<Misc<'a>>),
}

// ---------------------------------------------------------------------------
// The parser

/// The furthest point reached, and what was wanted there.
#[derive(Default)]
struct Furthest<'a> {
    at: Option<&'a str>,
    expected: Vec<&'static str>,
}

pub struct Syntax<'a> {
    furthest: RefCell<Furthest<'a>>,
}

impl<'a> Default for Syntax<'a> {
    fn default() -> Self {
        Syntax {
            furthest: RefCell::new(Furthest {
                at: None,
                expected: Vec::new(),
            }),
        }
    }
}

fn starts_ci(i: &str, lit: &str) -> bool {
    i.len() >= lit.len()
        && i.is_char_boundary(lit.len())
        && i[..lit.len()].eq_ignore_ascii_case(lit)
}

fn alpha_run(i: &str) -> usize {
    i.bytes().take_while(u8::is_ascii_alphabetic).count()
}

/// The unit keywords, which can only start a document element.
pub const UNIT_KEYWORDS: &[&str] = &[
    "Prefix:",
    "Ontology:",
    "Class:",
    "ObjectProperty:",
    "DataProperty:",
    "AnnotationProperty:",
    "Individual:",
    "Datatype:",
    "Rule:",
    "EquivalentClasses:",
    "DisjointClasses:",
    "EquivalentProperties:",
    "DisjointProperties:",
    "SameIndividual:",
    "DifferentIndividuals:",
];

impl<'a> Syntax<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    /// Where parsing got furthest and what it wanted there.
    pub fn furthest(&self) -> Option<(&'a str, Vec<&'static str>)> {
        let f = self.furthest.borrow();
        f.at.map(|a| (a, f.expected.clone()))
    }

    fn no<T>(&self, at: &'a str, what: &'static str) -> Option<T> {
        let mut f = self.furthest.borrow_mut();
        match f.at {
            Some(cur) if cur.len() < at.len() => {}
            Some(cur) if cur.len() == at.len() => {
                if !f.expected.contains(&what) {
                    f.expected.push(what);
                }
            }
            _ => {
                f.at = Some(at);
                f.expected = vec![what];
            }
        }
        None
    }

    // -- tokens ------------------------------------------------------------

    /// A literal punctuation string.
    fn punct(&self, i: &'a str, p: &'static str) -> Res<'a, ()> {
        let i = sp(i);
        match i.strip_prefix(p) {
            Some(r) => Some((r, ())),
            None => self.no(i, p),
        }
    }

    /// A case-insensitive literal such as `Class:`.
    fn ci(&self, i: &'a str, lit: &'static str) -> Res<'a, ()> {
        let i = sp(i);
        if starts_ci(i, lit) {
            Some((&i[lit.len()..], ()))
        } else {
            self.no(i, lit)
        }
    }

    /// A keyword followed by the name boundary: `^"or" ~ NameBoundary`.
    fn kw(&self, i: &'a str, word: &'static str) -> Res<'a, &'a str> {
        let i = sp(i);
        if starts_ci(i, word) {
            let r = &i[word.len()..];
            let boundary = match r.chars().next() {
                Some(c) => !(pn_chars(c) || c == ':'),
                None => true,
            };
            if boundary {
                return Some((r, &i[..word.len()]));
            }
        }
        self.no(i, word)
    }

    // -- terminals ---------------------------------------------------------

    /// `FullIRI = "<" ~ RFC3987_Iri ~ ">"`
    fn full_iri(&self, i: &'a str) -> Res<'a, &'a str> {
        let i = sp(i);
        let Some(r) = i.strip_prefix('<') else {
            return self.no(i, "IRI");
        };
        match r.find('>') {
            Some(n) if is_iri(&r[..n]) => Some((&r[n + 1..], &r[..n])),
            _ => self.no(i, "IRI"),
        }
    }

    /// `IRI = FullIRI | AbbreviatedIRI | SimpleIRI`
    pub fn iri(&self, i: &'a str) -> Res<'a, Iri<'a>> {
        let i = sp(i);
        if i.starts_with('#') {
            // The marker for the trailing block, which `pname` would skip.
            return self.no(i, "IRI");
        }
        let text = |rest: &str| &i[..i.len() - rest.len()];
        if let Some((r, s)) = self.full_iri(i) {
            return Some((
                r,
                Iri {
                    kind: IriKind::Full(s),
                    text: text(r),
                },
            ));
        }
        if let Ok((r, (prefix, local, _))) = pname(i) {
            return Some((
                r,
                Iri {
                    kind: IriKind::Abbrev(prefix, local),
                    text: text(r),
                },
            ));
        }
        if let Some(n) = pn_local(i)
            && !i[n..].starts_with(':')
        {
            return Some((
                &i[n..],
                Iri {
                    kind: IriKind::Simple(&i[..n]),
                    text: &i[..n],
                },
            ));
        }
        self.no(i, "IRI")
    }

    /// `SPARQL_BlankNodeLabel`, returning `_:label`.
    fn blank(&self, i: &'a str) -> Res<'a, &'a str> {
        let i = sp(i);
        if let Some(r) = i.strip_prefix("_:")
            && let Some(n) = pn_local(r)
        {
            return Some((&r[n..], &i[..2 + n]));
        }
        self.no(i, "blank node")
    }

    /// `Individual = AnonymousIndividual | IRI`
    pub fn individual(&self, i: &'a str) -> Res<'a, Ind<'a>> {
        if let Some((r, b)) = self.blank(i) {
            return Some((r, Ind::Anon(b)));
        }
        let (r, iri) = self.iri(i)?;
        Some((r, Ind::Named(iri)))
    }

    /// `QuotedString`, unescaped as the pest reader does.
    fn quoted(&self, i: &'a str) -> Res<'a, String> {
        let i = sp(i);
        let Some(r) = i.strip_prefix('"') else {
            return self.no(i, "quoted string");
        };
        let b = r.as_bytes();
        let mut k = 0;
        loop {
            match b.get(k) {
                None => return self.no(i, "closing `\"`"),
                Some(b'"') => break,
                Some(b'\\') if matches!(b.get(k + 1), Some(b'\\') | Some(b'"')) => k += 2,
                Some(_) => {
                    // Step over one whole character.
                    k += r[k..].chars().next().map_or(1, char::len_utf8);
                }
            }
        }
        let body = &r[..k];
        let s = if body.contains(r"\\") || body.contains("\\\"") {
            body.replace(r"\\", r"\").replace("\\\"", "\"")
        } else {
            body.to_string()
        };
        Some((&r[k + 1..], s))
    }

    /// `LanguageTag = "@" ~ BCP47_LanguageTag`
    fn language_tag(&self, i: &'a str) -> Res<'a, &'a str> {
        let i = sp(i);
        let Some(r) = i.strip_prefix('@') else {
            return self.no(i, "`@`");
        };
        match language_tag_len(r) {
            Some(n) => Some((&r[n..], &r[..n])),
            None => self.no(r, "language tag"),
        }
    }

    fn digits(s: &str, from: usize) -> usize {
        from + s[from..].bytes().take_while(u8::is_ascii_digit).count()
    }

    /// The length of a numeric literal of the given kind at the start of `s`.
    fn number(s: &str, kind: char) -> Option<usize> {
        let mut e = 0;
        if s.starts_with(['+', '-']) {
            e = 1;
        }
        match kind {
            'i' => {
                let d = Self::digits(s, e);
                (d > e).then_some(d)
            }
            'd' => {
                let d = Self::digits(s, e);
                if d == e || !s[d..].starts_with('.') {
                    return None;
                }
                let f = Self::digits(s, d + 1);
                (f > d + 1).then_some(f)
            }
            _ => {
                let d = Self::digits(s, e);
                let mut end = if d > e {
                    let mut end = d;
                    if s[d..].starts_with('.') {
                        let f = Self::digits(s, d + 1);
                        if f > d + 1 {
                            end = f;
                        }
                    }
                    end
                } else if s[e..].starts_with('.') {
                    let f = Self::digits(s, e + 1);
                    if f == e + 1 {
                        return None;
                    }
                    f
                } else {
                    return None;
                };
                if s[end..].starts_with(['e', 'E']) {
                    let mut x = end + 1;
                    if s[x..].starts_with(['+', '-']) {
                        x += 1;
                    }
                    let d = Self::digits(s, x);
                    if d > x {
                        end = d;
                    }
                }
                s[end..].starts_with(['f', 'F']).then_some(end + 1)
            }
        }
    }

    /// `Literal`
    pub fn literal(&self, i: &'a str) -> Res<'a, Lit<'a>> {
        let i = sp(i);
        if let Some((r, s)) = self.quoted(i) {
            if let Some((r2, _)) = self.punct(r, "^^")
                && let Some((r3, dt)) = self.iri(r2)
            {
                return Some((r3, Lit::Typed(s, dt)));
            }
            if let Some((r2, tag)) = self.language_tag(r) {
                return Some((r2, Lit::Lang(s, tag)));
            }
            return Some((r, Lit::Simple(s)));
        }
        for word in ["true", "false"] {
            if let Some(r) = i.strip_prefix(word) {
                let boundary = match r.chars().next() {
                    Some(c) => !(pn_chars(c) || c == ':'),
                    None => true,
                };
                if boundary {
                    return Some((r, Lit::Boolean(&i[..word.len()])));
                }
            }
        }
        if let Some(n) = Self::number(i, 'f') {
            return Some((&i[n..], Lit::Float(&i[..n])));
        }
        if let Some(n) = Self::number(i, 'd') {
            return Some((&i[n..], Lit::Decimal(&i[..n])));
        }
        if let Some(n) = Self::number(i, 'i') {
            return Some((&i[n..], Lit::Integer(&i[..n])));
        }
        self.no(i, "literal")
    }

    /// `Cardinality = ASCII_DIGIT+`
    fn cardinality(&self, i: &'a str) -> Res<'a, &'a str> {
        let i = sp(i);
        let n = Self::digits(i, 0);
        if n == 0 {
            return self.no(i, "cardinality");
        }
        Some((&i[n..], &i[..n]))
    }

    /// `ope`
    pub fn ope(&self, i: &'a str) -> Res<'a, Ope<'a>> {
        if let Some((r, _)) = self.kw(i, "inverse") {
            if let Some((r, _)) = self.punct(r, "(")
                && let Some((r, iri)) = self.iri(r)
                && let Some((r, _)) = self.punct(r, ")")
            {
                return Some((r, Ope { inverse: true, iri }));
            }
            if let Some((r, iri)) = self.iri(r) {
                return Some((r, Ope { inverse: true, iri }));
            }
        }
        let (r, iri) = self.iri(i)?;
        Some((
            r,
            Ope {
                inverse: false,
                iri,
            },
        ))
    }

    // -- class expressions -------------------------------------------------

    /// `Description = Conjunction (OrKw Conjunction)*`
    pub fn description(&self, i: &'a str) -> Res<'a, Desc<'a>> {
        let (mut r, c) = self.conjunction(i)?;
        let mut v = vec![c];
        while let Some((r2, _)) = self.kw(r, "or")
            && let Some((r3, c)) = self.conjunction(r2)
        {
            v.push(c);
            r = r3;
        }
        Some((r, Desc(v)))
    }

    /// `Conjunction = ThatConjunction | Primary (AndKw Primary)*`
    fn conjunction(&self, i: &'a str) -> Res<'a, Conj<'a>> {
        if let Some(x) = self.that_conjunction(i) {
            return Some(x);
        }
        let (mut r, p) = self.primary(i)?;
        let mut v = vec![p];
        while let Some((r2, _)) = self.kw(r, "and")
            && let Some((r3, p)) = self.primary(r2)
        {
            v.push(p);
            r = r3;
        }
        Some((r, Conj::And(v)))
    }

    fn that_conjunction(&self, i: &'a str) -> Res<'a, Conj<'a>> {
        let (r, cls) = self.iri(i)?;
        let (r, _) = self.kw(r, "that")?;
        let (mut r, op) = self.that_operand(r)?;
        let mut v = vec![op];
        while let Some((r2, _)) = self.kw(r, "and")
            && let Some((r3, op)) = self.that_operand(r2)
        {
            v.push(op);
            r = r3;
        }
        Some((r, Conj::That(cls, v)))
    }

    fn that_operand(&self, i: &'a str) -> Res<'a, (bool, Restr<'a>)> {
        let (r, not) = match self.kw(i, "not") {
            Some((r, _)) => (r, true),
            None => (i, false),
        };
        let (r, restr) = self.restriction(r)?;
        Some((r, (not, restr)))
    }

    /// `Primary = NotKw? ~ (Restriction | Atomic)`
    fn primary(&self, i: &'a str) -> Res<'a, Prim<'a>> {
        let (r, not) = match self.kw(i, "not") {
            Some((r, _)) => (r, true),
            None => (i, false),
        };
        if let Some((r, restr)) = self.restriction(r) {
            return Some((
                r,
                Prim {
                    not,
                    body: PrimBody::Restr(Box::new(restr)),
                },
            ));
        }
        let (r, body) = self.atomic(r)?;
        Some((r, Prim { not, body }))
    }

    /// `Atomic = ObjectOneOf | "(" ~ Description ~ ")" | ClassIRI`
    fn atomic(&self, i: &'a str) -> Res<'a, PrimBody<'a>> {
        if let Some((mut r, _)) = self.punct(i, "{")
            && let Some((r1, first)) = self.individual(r)
        {
            let mut v = vec![first];
            r = r1;
            while let Some((r2, _)) = self.punct(r, ",")
                && let Some((r3, x)) = self.individual(r2)
            {
                v.push(x);
                r = r3;
            }
            if let Some((r, _)) = self.punct(r, "}") {
                return Some((r, PrimBody::OneOf(v)));
            }
        }
        if let Some((r, _)) = self.punct(i, "(")
            && let Some((r, d)) = self.description(r)
            && let Some((r, _)) = self.punct(r, ")")
        {
            return Some((r, PrimBody::Paren(Box::new(d))));
        }
        let (r, iri) = self.iri(i)?;
        Some((r, PrimBody::Class(iri)))
    }

    // -- restrictions ------------------------------------------------------

    /// `KnownDatatypeIRI`, matched as a lookahead only.
    fn known_datatype(&self, i: &'a str) -> bool {
        let i = sp(i);
        for p in ["xsd:", "rdf:", "rdfs:"] {
            if let Some(r) = i.strip_prefix(p)
                && pn_local(sp(r)).is_some()
            {
                return true;
            }
        }
        if let Some(r) = i.strip_prefix('<') {
            for ns in [
                "http://www.w3.org/2001/XMLSchema#",
                "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
                "http://www.w3.org/2000/01/rdf-schema#",
            ] {
                if let Some(mut r) = sp(r).strip_prefix(ns) {
                    // (!(">" | " ") ~ ANY)*, each step preceded by a skip.
                    loop {
                        let s = sp(r);
                        match s.chars().next() {
                            Some(c) if c != '>' && c != ' ' => r = &s[c.len_utf8()..],
                            _ => break,
                        }
                    }
                    if sp(r).starts_with('>') {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// `DataShaped = NotKw? ~ (DatatypeRestriction | KnownDatatypeIRI |
    /// DataShapedParenthesized | DataOneOf)`
    fn data_shaped(&self, i: &'a str) -> bool {
        let r = match self.kw(i, "not") {
            Some((r, _)) => r,
            None => i,
        };
        if self.datatype_restriction(r).is_some() || self.known_datatype(r) {
            return true;
        }
        if let Some((r2, _)) = self.punct(r, "(")
            && (self.known_datatype(r2) || self.data_one_of(r2).is_some())
        {
            return true;
        }
        self.data_one_of(r).is_some()
    }

    /// One of the five data arms: `dp kw [Cardinality] &DataShaped DataRange`.
    fn data_arm(&self, i: &'a str, word: &'static str, kw: RKw, card: bool) -> Res<'a, Restr<'a>> {
        let (r, dp) = self.iri(i)?;
        let (r, _) = self.kw(r, word)?;
        let (r, card) = if card {
            let (r, c) = self.cardinality(r)?;
            (r, Some(c))
        } else {
            (r, None)
        };
        if !self.data_shaped(r) {
            return self.no(sp(r), "data range");
        }
        let (r, dr) = self.data_range(r)?;
        Some((
            r,
            Restr {
                text: "",
                prop: Prop::Data(dp),
                kw,
                card,
                filler: Filler::Dr(dr),
            },
        ))
    }

    /// An object arm: `ope kw ...`.
    fn object_arm(&self, i: &'a str, word: &'static str, kw: RKw) -> Res<'a, Restr<'a>> {
        let (r, ope) = self.ope(i)?;
        let (r, _) = self.kw(r, word)?;
        let mut card = None;
        let mut r = r;
        let filler = match kw {
            RKw::Some | RKw::Only => {
                let (r2, p) = self.primary(r)?;
                r = r2;
                Filler::Prim(Box::new(p))
            }
            RKw::Value => {
                let (r2, ind) = self.individual(r)?;
                r = r2;
                Filler::Ind(ind)
            }
            RKw::SelfKw => Filler::None,
            RKw::Min | RKw::Max | RKw::Exactly => {
                let (r2, c) = self.cardinality(r)?;
                card = Some(c);
                r = r2;
                match self.primary(r) {
                    Some((r3, p)) => {
                        r = r3;
                        Filler::Prim(Box::new(p))
                    }
                    None => Filler::None,
                }
            }
        };
        Some((
            r,
            Restr {
                text: "",
                prop: Prop::Obj(ope),
                kw,
                card,
                filler,
            },
        ))
    }

    /// `Restriction`, trying the arms in grammar order.
    pub fn restriction(&self, i: &'a str) -> Res<'a, Restr<'a>> {
        let start = sp(i);
        let done = |(r, mut x): (&'a str, Restr<'a>)| {
            x.text = &start[..start.len() - r.len()];
            Some((r, x))
        };
        if let Some(x) = self.data_arm(i, "some", RKw::Some, false) {
            return done(x);
        }
        if let Some(x) = self.data_arm(i, "only", RKw::Only, false) {
            return done(x);
        }
        if let Some(x) = self.data_arm(i, "min", RKw::Min, true) {
            return done(x);
        }
        if let Some(x) = self.data_arm(i, "max", RKw::Max, true) {
            return done(x);
        }
        if let Some(x) = self.data_arm(i, "exactly", RKw::Exactly, true) {
            return done(x);
        }
        if let Some(x) = self.object_arm(i, "some", RKw::Some) {
            return done(x);
        }
        if let Some(x) = self.object_arm(i, "only", RKw::Only) {
            return done(x);
        }
        // DataPropertyIRI ~ ValueKw ~ Literal
        if let Some((r, dp)) = self.iri(i)
            && let Some((r, _)) = self.kw(r, "value")
            && let Some((r, l)) = self.literal(r)
        {
            return done((
                r,
                Restr {
                    text: "",
                    prop: Prop::Data(dp),
                    kw: RKw::Value,
                    card: None,
                    filler: Filler::Lit(l),
                },
            ));
        }
        for (word, kw) in [
            ("value", RKw::Value),
            ("Self", RKw::SelfKw),
            ("min", RKw::Min),
            ("max", RKw::Max),
            ("exactly", RKw::Exactly),
        ] {
            if let Some(x) = self.object_arm(i, word, kw) {
                return done(x);
            }
        }
        self.no(start, "restriction")
    }

    // -- data ranges -------------------------------------------------------

    /// `DataRange = DataConjunction (OrKw DataConjunction)*`
    pub fn data_range(&self, i: &'a str) -> Res<'a, SynDataRange<'a>> {
        let (mut r, c) = self.data_conjunction(i)?;
        let mut v = vec![c];
        while let Some((r2, _)) = self.kw(r, "or")
            && let Some((r3, c)) = self.data_conjunction(r2)
        {
            v.push(c);
            r = r3;
        }
        Some((r, SynDataRange(v)))
    }

    fn data_conjunction(&self, i: &'a str) -> Res<'a, DataConj<'a>> {
        let (mut r, p) = self.data_primary(i)?;
        let mut v = vec![p];
        while let Some((r2, _)) = self.kw(r, "and")
            && let Some((r3, p)) = self.data_primary(r2)
        {
            v.push(p);
            r = r3;
        }
        Some((r, DataConj(v)))
    }

    fn data_primary(&self, i: &'a str) -> Res<'a, DataPrim<'a>> {
        let (r, not) = match self.kw(i, "not") {
            Some((r, _)) => (r, true),
            None => (i, false),
        };
        let (r, atom) = self.data_atomic(r)?;
        Some((r, DataPrim { not, atom }))
    }

    fn data_atomic(&self, i: &'a str) -> Res<'a, DataAtom<'a>> {
        if let Some((r, v)) = self.data_one_of(i) {
            return Some((r, DataAtom::OneOf(v)));
        }
        if let Some((r, (dt, f))) = self.datatype_restriction(i) {
            return Some((r, DataAtom::Restriction(dt, f)));
        }
        if let Some((r, _)) = self.punct(i, "(")
            && let Some((r, d)) = self.data_range(r)
            && let Some((r, _)) = self.punct(r, ")")
        {
            return Some((r, DataAtom::Paren(Box::new(d))));
        }
        let (r, iri) = self.iri(i)?;
        Some((r, DataAtom::Datatype(iri)))
    }

    /// `DataOneOf = "{" ~ Literal ~ ("," ~ Literal)* ~ "}"`
    fn data_one_of(&self, i: &'a str) -> Res<'a, Vec<Lit<'a>>> {
        let (r, _) = self.punct(i, "{")?;
        let (mut r, first) = self.literal(r)?;
        let mut v = vec![first];
        while let Some((r2, _)) = self.punct(r, ",")
            && let Some((r3, l)) = self.literal(r2)
        {
            v.push(l);
            r = r3;
        }
        let (r, _) = self.punct(r, "}")?;
        Some((r, v))
    }

    /// `DatatypeRestriction = DatatypeIRI ~ "[" ~ Facet ~ ("," ~ Facet)* ~ "]"`
    fn datatype_restriction(&self, i: &'a str) -> Res<'a, (Iri<'a>, Vec<Facet<'a>>)> {
        let (r, dt) = self.iri(i)?;
        let (r, _) = self.punct(r, "[")?;
        let (mut r, first) = self.facet(r)?;
        let mut v = vec![first];
        while let Some((r2, _)) = self.punct(r, ",")
            && let Some((r3, f)) = self.facet(r2)
        {
            v.push(f);
            r = r3;
        }
        let (r, _) = self.punct(r, "]")?;
        Some((r, (dt, v)))
    }

    fn facet(&self, i: &'a str) -> Res<'a, Facet<'a>> {
        let s = sp(i);
        let symbol = [">=", "<=", ">", "<"]
            .iter()
            .find(|p| s.starts_with(**p))
            .map(|p| (&s[p.len()..], &s[..p.len()]))
            .or_else(|| {
                [
                    "length",
                    "minLength",
                    "maxLength",
                    "pattern",
                    "langRange",
                    "totalDigits",
                    "fractionDigits",
                ]
                .iter()
                .find_map(|w| self.kw(s, w))
            });
        let Some((r, symbol)) = symbol else {
            return self.no(s, "facet");
        };
        let (r, lit) = self.literal(r)?;
        Some((r, Facet { symbol, lit }))
    }

    // -- annotations -------------------------------------------------------

    /// `Annotations = ^"Annotations:" ~ AnnotationEntry ~ ("," ~ AnnotationEntry)*`
    pub fn annotations(&self, i: &'a str) -> Res<'a, Annotations<'a>> {
        let (r, _) = self.ci(i, "Annotations:")?;
        let (mut r, first) = self.ann_entry(r)?;
        let mut v = vec![first];
        while let Some((r2, _)) = self.punct(r, ",")
            && let Some((r3, e)) = self.ann_entry(r2)
        {
            v.push(e);
            r = r3;
        }
        Some((r, Annotations(v)))
    }

    /// `AnnotationEntry = Annotations? ~ IRI ~ AnnotationTarget`
    fn ann_entry(&self, i: &'a str) -> Res<'a, AnnEntry<'a>> {
        let (r, nested) = match self.annotations(i) {
            Some((r, a)) => (r, Some(a)),
            None => (i, None),
        };
        let (r, property) = self.iri(r)?;
        let (r, target) = if let Some((r, l)) = self.literal(r) {
            (r, Target::Lit(l))
        } else if let Some((r, b)) = self.blank(r) {
            (r, Target::Anon(b))
        } else {
            let (r, iri) = self.iri(r)?;
            (r, Target::Iri(iri))
        };
        Some((
            r,
            AnnEntry {
                nested,
                property,
                target,
            },
        ))
    }

    fn opt_annotations(&self, i: &'a str) -> (&'a str, Option<Annotations<'a>>) {
        match self.annotations(i) {
            Some((r, a)) => (r, Some(a)),
            None => (i, None),
        }
    }

    /// `Annotations? ~ item ~ ("," ~ Annotations? ~ item)*`
    fn ann_list<T>(
        &self,
        i: &'a str,
        item: impl Fn(&Self, &'a str) -> Res<'a, T>,
    ) -> Res<'a, AnnList<'a, T>> {
        let (r, a) = self.opt_annotations(i);
        let (mut r, first) = item(self, r)?;
        let mut v = vec![(a, first)];
        while let Some((r2, _)) = self.punct(r, ",") {
            let (r3, a) = self.opt_annotations(r2);
            let Some((r4, x)) = item(self, r3) else {
                break;
            };
            v.push((a, x));
            r = r4;
        }
        Some((r, AnnList(v)))
    }

    /// `item ~ ("," ~ item)*`
    fn plain_list<T>(
        &self,
        i: &'a str,
        item: impl Fn(&Self, &'a str) -> Res<'a, T>,
    ) -> Res<'a, Vec<T>> {
        let (mut r, first) = item(self, i)?;
        let mut v = vec![first];
        while let Some((r2, _)) = self.punct(r, ",")
            && let Some((r3, x)) = item(self, r2)
        {
            v.push(x);
            r = r3;
        }
        Some((r, v))
    }

    // -- frames ------------------------------------------------------------

    fn characteristic(&self, i: &'a str) -> Res<'a, &'a str> {
        [
            "Functional",
            "InverseFunctional",
            "Reflexive",
            "Irreflexive",
            "Symmetric",
            "Asymmetric",
            "Transitive",
        ]
        .iter()
        .find_map(|w| self.kw(i, w))
    }

    /// `Fact = NotKw? ~ ope ~ (Literal | Individual)`
    fn fact(&self, i: &'a str) -> Res<'a, Fact<'a>> {
        let (r, not) = match self.kw(i, "not") {
            Some((r, _)) => (r, true),
            None => (i, false),
        };
        let (r, ope) = self.ope(r)?;
        if let Some((r, l)) = self.literal(r) {
            return Some((
                r,
                Fact {
                    not,
                    ope,
                    target: FactTarget::Lit(l),
                },
            ));
        }
        let (r, ind) = self.individual(r)?;
        Some((
            r,
            Fact {
                not,
                ope,
                target: FactTarget::Ind(ind),
            },
        ))
    }

    /// `PropertyChain = ope ~ (OKw ~ ope)+`
    fn property_chain(&self, i: &'a str) -> Res<'a, Vec<Ope<'a>>> {
        let (mut r, first) = self.ope(i)?;
        let mut v = vec![first];
        while let Some((r2, _)) = self.kw(r, "o")
            && let Some((r3, x)) = self.ope(r2)
        {
            v.push(x);
            r = r3;
        }
        if v.len() < 2 {
            return self.no(sp(r), "`o`");
        }
        Some((r, v))
    }

    fn body(&self, i: &'a str, kind: Bk) -> Res<'a, Body<'a>> {
        Some(match kind {
            Bk::Descs => {
                let (r, l) = self.ann_list(i, Self::description)?;
                (r, Body::Descs(l))
            }
            Bk::Opes => {
                let (r, l) = self.ann_list(i, Self::ope)?;
                (r, Body::Opes(l))
            }
            Bk::Iris => {
                let (r, l) = self.ann_list(i, Self::iri)?;
                (r, Body::Iris(l))
            }
            Bk::Inds => {
                let (r, l) = self.ann_list(i, Self::individual)?;
                (r, Body::Inds(l))
            }
            Bk::DataRanges => {
                let (r, l) = self.plain_list(i, Self::data_range)?;
                (r, Body::DataRanges(l))
            }
            Bk::DataRange => {
                let (r, d) = self.data_range(i)?;
                (r, Body::DataRange(d))
            }
            Bk::Chain => {
                let (r, l) = self.property_chain(i)?;
                (r, Body::Chain(l))
            }
            Bk::Keys => {
                let (r, l) = self.plain_list(i, Self::ope)?;
                (r, Body::Keys(l))
            }
            Bk::Chars => {
                let (r, l) = self.plain_list(i, Self::characteristic)?;
                (r, Body::Chars(l))
            }
            Bk::Facts => {
                let (r, l) = self.plain_list(i, Self::fact)?;
                (r, Body::Facts(l))
            }
        })
    }

    /// One clause of a frame, from the clause table.
    fn clause(&self, i: &'a str, table: &[(&'static str, Ck, Bk)]) -> Res<'a, Clause<'a>> {
        if let Some((r, a)) = self.annotations(i) {
            return Some((
                r,
                Clause {
                    kw: Ck::Annotations,
                    ann: None,
                    body: Body::Annotations(a),
                },
            ));
        }
        for (word, kw, bk) in table {
            if let Some((r, _)) = self.ci(i, word) {
                let (r, ann) = self.opt_annotations(r);
                let (r, body) = self.body(r, *bk)?;
                return Some((r, Clause { kw: *kw, ann, body }));
            }
        }
        self.no(sp(i), "clause")
    }

    fn clauses(
        &self,
        mut r: &'a str,
        table: &[(&'static str, Ck, Bk)],
    ) -> (&'a str, Vec<Clause<'a>>) {
        let mut v = Vec::new();
        while let Some((r2, c)) = self.clause(r, table) {
            v.push(c);
            r = r2;
        }
        (r, v)
    }

    /// Just the keyword and subject of a `DataProperty:` or `Datatype:` frame.
    pub fn frame_head(&self, i: &'a str) -> Option<(FrameKind, Iri<'a>)> {
        if let Some((r, _)) = self.ci(i, "DataProperty:") {
            let (_, iri) = self.iri(r)?;
            return Some((FrameKind::DataProperty, iri));
        }
        let (r, _) = self.ci(i, "Datatype:")?;
        let (_, iri) = self.iri(r)?;
        Some((FrameKind::Datatype, iri))
    }

    /// Any frame or `Misc`, or the header, a rule or a prefix declaration.
    pub fn unit(&self, i: &'a str) -> Res<'a, Unit<'a>> {
        let s = sp(i);
        let word = &s[..alpha_run(s)];
        let tail = &s[word.len()..];
        // Dispatch on the keyword, as the alternatives in the grammar are
        // distinguished by it alone.
        if !tail.starts_with(':') {
            return self.no(s, "frame");
        }
        let lower = word.to_ascii_lowercase();
        let r = &tail[1..];
        match lower.as_str() {
            "prefix" => {
                let (r, prefix) = self.prefix_name(r)?;
                let (r, iri) = self.full_iri(r)?;
                Some((r, Unit::Prefix(prefix, iri)))
            }
            "ontology" => self.header(r),
            "class" => {
                let (r, subject_ann) = self.opt_annotations(r);
                let (r, d) = self.description(r)?;
                let (r, clauses) = self.clauses(r, CLASS_CLAUSES);
                Some((
                    r,
                    Unit::Frame(Box::new(Frame {
                        kind: FrameKind::Class,
                        subject_ann,
                        subject: Subject::Desc(d),
                        clauses,
                    })),
                ))
            }
            "objectproperty" => {
                let (r, o) = self.ope(r)?;
                let (r, clauses) = self.clauses(r, OBJECT_PROPERTY_CLAUSES);
                Some((
                    r,
                    frame(FrameKind::ObjectProperty, Subject::Ope(o), clauses),
                ))
            }
            "dataproperty" => {
                let (r, s) = self.iri(r)?;
                let (r, clauses) = self.clauses(r, DATA_PROPERTY_CLAUSES);
                Some((r, frame(FrameKind::DataProperty, Subject::Iri(s), clauses)))
            }
            "annotationproperty" => {
                let (r, s) = self.iri(r)?;
                let (r, clauses) = self.clauses(r, ANNOTATION_PROPERTY_CLAUSES);
                Some((
                    r,
                    frame(FrameKind::AnnotationProperty, Subject::Iri(s), clauses),
                ))
            }
            "individual" => {
                let (r, s) = self.individual(r)?;
                let (r, clauses) = self.clauses(r, INDIVIDUAL_CLAUSES);
                Some((r, frame(FrameKind::Individual, Subject::Ind(s), clauses)))
            }
            "datatype" => {
                let (r, s) = self.iri(r)?;
                let (r, clauses) = self.clauses(r, DATATYPE_CLAUSES);
                Some((r, frame(FrameKind::Datatype, Subject::Iri(s), clauses)))
            }
            "rule" => {
                let (r, ann) = self.opt_annotations(r);
                let (r, body) = self.atom_list(r)?;
                let (r, _) = self.punct(r, "->")?;
                let (r, head) = self.atom_list(r)?;
                Some((r, Unit::Rule(Box::new(RuleFrame { ann, body, head }))))
            }
            "equivalentclasses" => self.misc(r, MiscKind::EquivalentClasses, Bk::Descs),
            "disjointclasses" => self.misc(r, MiscKind::DisjointClasses, Bk::Descs),
            "equivalentproperties" => self.misc(r, MiscKind::EquivalentProperties, Bk::Opes),
            "disjointproperties" => self.misc(r, MiscKind::DisjointProperties, Bk::Opes),
            "sameindividual" => self.misc(r, MiscKind::SameIndividual, Bk::Inds),
            "differentindividuals" => self.misc(r, MiscKind::DifferentIndividuals, Bk::Inds),
            _ => self.no(s, "frame"),
        }
    }

    fn misc(&self, r: &'a str, kind: MiscKind, bk: Bk) -> Res<'a, Unit<'a>> {
        let (r, ann) = self.opt_annotations(r);
        let (r, body) = self.body(r, bk)?;
        Some((r, Unit::Misc(Box::new(Misc { kind, ann, body }))))
    }

    /// `PrefixName = SPARQL_PnameNs`
    fn prefix_name(&self, i: &'a str) -> Res<'a, &'a str> {
        match pname_ns(i) {
            Ok((r, p)) => Some((r, p)),
            Err(_) => self.no(sp(i), "prefix name"),
        }
    }

    /// `OntologyHeader`, after the `Ontology:` keyword.
    fn header(&self, r: &'a str) -> Res<'a, Unit<'a>> {
        let mut h = Header {
            iri: None,
            version: None,
            imports: Vec::new(),
            annotations: Vec::new(),
        };
        let mut r = r;
        if let Some((r2, iri)) = self.iri(r) {
            h.iri = Some(iri);
            r = r2;
            if let Some((r3, v)) = self.iri(r) {
                h.version = Some(v);
                r = r3;
            }
        }
        while let Some((r2, _)) = self.ci(r, "Import:")
            && let Some((r3, iri)) = self.iri(r2)
        {
            h.imports.push(iri);
            r = r3;
        }
        while let Some((r2, a)) = self.annotations(r) {
            h.annotations.push(a);
            r = r2;
        }
        Some((r, Unit::Header(h)))
    }

    // -- SWRL --------------------------------------------------------------

    fn atom_list(&self, i: &'a str) -> Res<'a, Vec<SynAtom<'a>>> {
        self.plain_list(i, Self::atom)
    }

    fn arg(&self, i: &'a str) -> Res<'a, Arg<'a>> {
        if let Some((r, _)) = self.punct(i, "?")
            && let Some((r, v)) = self.iri(r)
        {
            return Some((r, Arg::Var(v)));
        }
        if let Some((r, l)) = self.literal(i) {
            return Some((r, Arg::Lit(l)));
        }
        let (r, ind) = self.individual(i)?;
        Some((r, Arg::Ind(ind)))
    }

    /// `SwrlIObj = Variable | Individual`
    fn iobj(&self, i: &'a str) -> Res<'a, Arg<'a>> {
        if let Some((r, _)) = self.punct(i, "?")
            && let Some((r, v)) = self.iri(r)
        {
            return Some((r, Arg::Var(v)));
        }
        let (r, ind) = self.individual(i)?;
        Some((r, Arg::Ind(ind)))
    }

    fn atom(&self, i: &'a str) -> Res<'a, SynAtom<'a>> {
        for (word, same) in [("SameAs", true), ("DifferentFrom", false)] {
            if let Some((r, _)) = self.kw(i, word)
                && let Some((r, _)) = self.punct(r, "(")
                && let Some((r, a)) = self.iobj(r)
                && let Some((r, _)) = self.punct(r, ",")
                && let Some((r, b)) = self.iobj(r)
                && let Some((r, _)) = self.punct(r, ")")
            {
                return Some((
                    r,
                    if same {
                        SynAtom::SameAs(a, b)
                    } else {
                        SynAtom::DifferentFrom(a, b)
                    },
                ));
            }
        }
        // SwrlNary needs two or more arguments, SwrlUnary exactly one.
        let (r, pred) = self.description(i)?;
        let (r, _) = self.punct(r, "(")?;
        let (r, args) = self.plain_list(r, Self::arg)?;
        let (r, _) = self.punct(r, ")")?;
        Some((r, SynAtom::Pred(pred, args)))
    }
}

fn frame<'a>(kind: FrameKind, subject: Subject<'a>, clauses: Vec<Clause<'a>>) -> Unit<'a> {
    Unit::Frame(Box::new(Frame {
        kind,
        subject_ann: None,
        subject,
        clauses,
    }))
}

/// How the body of a clause is read.
#[derive(Clone, Copy)]
pub enum Bk {
    Descs,
    Opes,
    Iris,
    Inds,
    DataRanges,
    DataRange,
    Chain,
    Keys,
    Chars,
    Facts,
}

type Table = &'static [(&'static str, Ck, Bk)];

const CLASS_CLAUSES: Table = &[
    ("SubClassOf:", Ck::SubClassOf, Bk::Descs),
    ("EquivalentTo:", Ck::EquivalentTo, Bk::Descs),
    ("DisjointWith:", Ck::DisjointWith, Bk::Descs),
    ("DisjointUnionOf:", Ck::DisjointUnionOf, Bk::Descs),
    ("HasKey:", Ck::HasKey, Bk::Keys),
];

const OBJECT_PROPERTY_CLAUSES: Table = &[
    ("SubPropertyChain:", Ck::SubPropertyChain, Bk::Chain),
    ("SubPropertyOf:", Ck::SubPropertyOf, Bk::Opes),
    ("EquivalentTo:", Ck::EquivalentTo, Bk::Opes),
    ("DisjointWith:", Ck::DisjointWith, Bk::Opes),
    ("InverseOf:", Ck::InverseOf, Bk::Opes),
    ("Domain:", Ck::Domain, Bk::Descs),
    ("Range:", Ck::Range, Bk::Descs),
    ("Characteristics:", Ck::Characteristics, Bk::Chars),
];

const DATA_PROPERTY_CLAUSES: Table = &[
    ("SubPropertyOf:", Ck::SubPropertyOf, Bk::Iris),
    ("EquivalentTo:", Ck::EquivalentTo, Bk::Iris),
    ("DisjointWith:", Ck::DisjointWith, Bk::Iris),
    ("Domain:", Ck::Domain, Bk::Descs),
    ("Range:", Ck::Range, Bk::DataRanges),
    ("Characteristics:", Ck::Characteristics, Bk::Chars),
];

const ANNOTATION_PROPERTY_CLAUSES: Table = &[
    ("SubPropertyOf:", Ck::SubPropertyOf, Bk::Iris),
    ("Domain:", Ck::Domain, Bk::Iris),
    ("Range:", Ck::Range, Bk::Iris),
];

const INDIVIDUAL_CLAUSES: Table = &[
    ("Types:", Ck::Types, Bk::Descs),
    ("Facts:", Ck::Facts, Bk::Facts),
    ("SameAs:", Ck::SameAs, Bk::Inds),
    ("DifferentFrom:", Ck::DifferentFrom, Bk::Inds),
];

const DATATYPE_CLAUSES: Table = &[("EquivalentTo:", Ck::EquivalentTo, Bk::DataRange)];
