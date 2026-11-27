//! A streaming Manchester Syntax reader.
//!
//! A document is a sequence of units: prefix declarations, the ontology
//! header, frames, rules and `Misc` axioms, then optionally a block of
//! functional syntax. [`Reader`] finds where the next unit ends, parses just
//! that text and yields its components, so memory is bounded by the largest
//! single unit.
//!
//! A unit ends where the next unit keyword begins. Such a keyword can also
//! appear as the start of a prefixed name (`Class:foo`), so each candidate
//! is checked by parsing the text before it: it is the end of the unit only
//! if that text is a complete unit.
use std::cell::RefCell;
use std::collections::VecDeque;
use std::io::{BufRead, Read};
use std::marker::PhantomData;

use curie::PrefixMapping;
use nom::error::ParseError;

use crate::error::HornedError;
use crate::io::StreamComponent;
use crate::io::ofn::reader::error::{Origin, PErr, too_deep_message};
use crate::model::*;

use super::convert::{Conv, Declarations, Out};
use super::syntax::*;

const MIN_CHUNK: usize = 64 * 1024;
const LONGEST_KEYWORD: usize = 21;

pub(super) type Item<A> = StreamComponent<AnnotatedComponent<A>>;

// ---------------------------------------------------------------------------
// Finding units

enum Scan {
    /// A candidate unit start at this offset.
    At(usize),
    /// The text ends inside something that must be read to its end.
    More,
    /// No further candidate.
    End,
}

fn is_name_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'_' | b'-' | b'.' | b':' | b'%') || b >= 0x80
}

/// Does a unit keyword or the general axioms marker start at `i`?
fn candidate_at(s: &str, i: usize) -> bool {
    let b = s.as_bytes();
    if b[i] == b'#' {
        return s[i..].starts_with(GENERAL_AXIOMS);
    }
    if !b[i].is_ascii_alphabetic() || (i > 0 && is_name_byte(b[i - 1])) {
        return false;
    }
    let tail = &s[i..];
    UNIT_KEYWORDS.iter().any(|k| {
        tail.len() >= k.len() && tail.as_bytes()[..k.len()].eq_ignore_ascii_case(k.as_bytes())
    })
}

/// The next candidate unit start at or after `from`. IRIs, strings and
/// comments are skipped, so keywords inside them are not seen.
fn scan_candidate(s: &str, from: usize, eof: bool) -> Scan {
    let b = s.as_bytes();
    let mut i = from;
    let more = |eof: bool| if eof { Scan::End } else { Scan::More };
    while i < b.len() {
        match b[i] {
            b'"' => {
                i += 1;
                loop {
                    match b.get(i) {
                        None => return more(eof),
                        Some(b'"') => {
                            i += 1;
                            break;
                        }
                        Some(b'\\') if matches!(b.get(i + 1), Some(b'\\') | Some(b'"')) => i += 2,
                        Some(_) => i += 1,
                    }
                }
            }
            b'<' => {
                // An IRI has no whitespace; `<` as a facet is followed by one.
                match b[i + 1..]
                    .iter()
                    .position(|c| *c == b'>' || c.is_ascii_whitespace())
                {
                    Some(p) if b[i + 1 + p] == b'>' => i += p + 2,
                    Some(_) => i += 1,
                    None => return more(eof),
                }
            }
            b'#' => {
                if s.len() - i < GENERAL_AXIOMS.len() && !eof {
                    return Scan::More;
                }
                if candidate_at(s, i) {
                    return Scan::At(i);
                }
                match b[i..].iter().position(|c| *c == b'\n' || *c == b'\r') {
                    Some(p) => i += p,
                    None => return more(eof),
                }
            }
            c if c.is_ascii_alphabetic() => {
                if !eof && s.len() - i < LONGEST_KEYWORD {
                    // The keyword may be cut by the end of the text.
                    let tail = &b[i..];
                    if UNIT_KEYWORDS.iter().any(|k| {
                        let n = tail.len().min(k.len());
                        tail[..n].eq_ignore_ascii_case(&k.as_bytes()[..n])
                    }) {
                        return Scan::More;
                    }
                }
                if candidate_at(s, i) {
                    return Scan::At(i);
                }
                // Skip the rest of the word so its letters are not tested.
                i += 1;
                while i < b.len() && is_name_byte(b[i]) {
                    i += 1;
                }
            }
            _ => i += 1,
        }
    }
    more(eof)
}

// ---------------------------------------------------------------------------
// The reader

#[derive(Clone, Copy, PartialEq, Eq)]
enum Phase {
    /// Prefix declarations, then the header or the first frame.
    Top,
    /// Frames, rules and `Misc` axioms.
    Body,
    Done,
}

/// What `parse_next` found.
enum Step<A: ForIRI> {
    NeedMore,
    /// Nothing but space remains.
    End,
    Unit {
        /// Bytes of `buf` the unit used, from `pos`.
        used: usize,
        items: Vec<Item<A>>,
        prefix: Option<(String, String)>,
        phase: Phase,
    },
    Error(HornedError),
}

/// Streams a Manchester Syntax document as [`StreamComponent`]s, one unit at
/// a time. See the [module documentation](self).
///
/// Which restrictions are data restrictions depends on which properties and
/// datatypes the document declares with `DataProperty:` and `Datatype:`
/// frames. A streaming reader knows only the frames it has read, so a use
/// that comes before the declaration is read as an object restriction.
/// [`read`](super::read) reads the declarations first, so it does not have
/// this limit.
pub struct Reader<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> {
    input: R,
    build: B,
    mapping: PrefixMapping,
    decls: RefCell<Declarations<A>>,
    buf: String,
    pos: usize,
    origin: Origin,
    tail: Vec<u8>,
    eof: bool,
    started: bool,
    phase: Phase,
    pending: VecDeque<Item<A>>,
    min_chunk: usize,
    _marker: PhantomData<A>,
}

impl<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> Reader<A, R, B> {
    pub fn new(bufread: R, config: crate::io::ParserConfiguration<A, B>) -> Self {
        Reader {
            input: bufread,
            build: config.build,
            mapping: PrefixMapping::default(),
            decls: RefCell::new(Declarations::default()),
            buf: String::new(),
            pos: 0,
            origin: Origin::default(),
            tail: Vec::new(),
            eof: false,
            started: false,
            phase: Phase::Top,
            pending: VecDeque::new(),
            min_chunk: MIN_CHUNK,
            _marker: PhantomData,
        }
    }

    pub fn from_bufread(bufread: R, config: crate::io::ParserConfiguration<A, B>) -> Self {
        Self::new(bufread, config)
    }

    /// Read at least this many bytes at a time. For testing refills.
    #[cfg(test)]
    pub(super) fn with_chunk_size(mut self, n: usize) -> Self {
        self.min_chunk = n;
        self
    }

    pub fn prefixes(&self) -> &PrefixMapping {
        &self.mapping
    }

    pub fn into_prefixes(self) -> PrefixMapping {
        self.mapping
    }

    /// Use these declarations, as if every frame had been read already.
    pub(super) fn with_declarations(mut self, decls: Declarations<A>) -> Self {
        self.decls = RefCell::new(decls);
        self
    }

    /// The text the reader will parse, when it is all in memory already.
    pub(super) fn with_text(mut self, text: String) -> Self {
        self.buf = text;
        self.eof = true;
        self.started = true;
        self
    }

    fn fill(&mut self) -> Result<(), HornedError> {
        if !self.started {
            self.started = true;
            crate::io::skip_bom(&mut self.input)?;
        }
        let consumed = &self.buf[..self.pos];
        self.origin = self.origin.advance(consumed);
        self.buf.drain(..self.pos);
        self.pos = 0;

        let want = self.min_chunk.max(self.buf.len());
        let mut chunk = std::mem::take(&mut self.tail);
        let have = chunk.len();
        (&mut self.input)
            .take(want as u64)
            .read_to_end(&mut chunk)?;
        self.eof = chunk.len() - have < want;

        match std::str::from_utf8(&chunk) {
            Ok(s) => self.buf.push_str(s),
            Err(e) if e.error_len().is_none() && !self.eof => {
                let valid = e.valid_up_to();
                self.buf
                    .push_str(std::str::from_utf8(&chunk[..valid]).unwrap());
                self.tail = chunk[valid..].to_vec();
            }
            Err(_) => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidData,
                    "stream did not contain valid UTF-8",
                )
                .into());
            }
        }
        Ok(())
    }

    fn syntax_error(&self, at: &str, expected: &[&'static str], window: &str) -> HornedError {
        let mut it = expected.iter();
        let first = it.next().copied().unwrap_or("unit");
        let mut e = PErr::expected(at, first);
        for w in it {
            e = e.or(PErr::expected(at, w));
        }
        e.into_horned(window, self.origin)
    }

    /// Parse and convert the unit that starts the unparsed text.
    fn parse_next(&self) -> Step<A> {
        let text = &self.buf[self.pos..];
        let eof = self.eof;
        let start = text.len() - sp(text).len();
        let body = &text[start..];

        // Nothing left, or the end of a comment that runs off the text.
        if body.is_empty() {
            return if eof { Step::End } else { Step::NeedMore };
        }

        if body.starts_with(GENERAL_AXIOMS) {
            if !eof {
                return Step::NeedMore;
            }
            return self.general_axioms(start, body);
        }

        let build = self.build.as_ref();
        let mut from = start + 1;
        loop {
            let (end, last) = match scan_candidate(text, from, eof) {
                Scan::At(c) => (c, false),
                Scan::More => return Step::NeedMore,
                Scan::End => (text.len(), true),
            };
            let slice = &text[start..end];
            let syn = Syntax::new();
            let unit = syn.unit(slice);
            if let Some(at) = syn.too_deep() {
                return Step::Error(
                    PErr::semantic(at, 0, too_deep_message()).into_horned(&self.buf, self.origin),
                );
            }
            match unit {
                Some((rest, unit)) if sp(rest).is_empty() => {
                    return self.convert_unit(start, end, slice, unit, build);
                }
                result => {
                    if last {
                        // Report the furthest failure of the whole unit.
                        let (at, expected) = match (&result, syn.furthest()) {
                            (Some((rest, _)), Some((at, exp))) if at.len() <= sp(rest).len() => {
                                (at, exp)
                            }
                            (Some((rest, _)), _) => (sp(rest), vec!["end of unit"]),
                            (None, Some(f)) => f,
                            (None, None) => (slice, vec!["unit"]),
                        };
                        return Step::Error(self.syntax_error(at, &expected, &self.buf));
                    }
                    from = end + 1;
                }
            }
        }
    }

    fn convert_unit(
        &self,
        start: usize,
        end: usize,
        slice: &str,
        unit: Unit<'_>,
        build: &Build<A>,
    ) -> Step<A> {
        let used = end;
        let mut items = Vec::new();
        let mut prefix = None;
        let mut phase = self.phase;
        let bad = |this: &Self, what: &'static str| {
            Step::Error(this.syntax_error(slice, &[what], &this.buf))
        };
        match unit {
            Unit::Prefix(name, iri) => {
                if self.phase != Phase::Top {
                    return bad(self, "frame");
                }
                if PrefixMapping::default().add_prefix(name, iri).is_err() {
                    return Step::Error(HornedError::invalid_at(
                        "invalid prefix declaration",
                        crate::error::Location::BytePosition(
                            (self.origin.offset + self.pos + start) as u64,
                        ),
                    ));
                }
                prefix = Some((name.to_string(), iri.to_string()));
                items.push(StreamComponent::Prefix(name.to_string(), iri.to_string()));
            }
            Unit::Header(h) => {
                if self.phase != Phase::Top {
                    return bad(self, "frame");
                }
                phase = Phase::Body;
                let decls = self.decls.borrow();
                let conv = Conv {
                    build,
                    prefixes: &self.mapping,
                    decls: &decls,
                    unit: slice,
                };
                let mut out = Out(Vec::new());
                if let Err(e) = header(&conv, &h, &mut out) {
                    return Step::Error(e.into_horned(&self.buf, self.origin));
                }
                items.extend(out.0.into_iter().map(StreamComponent::Component));
            }
            Unit::Frame(_) | Unit::Rule(_) | Unit::Misc(_) => {
                phase = Phase::Body;
                // A frame declares its own subject before its clauses.
                if let Unit::Frame(f) = &unit
                    && matches!(f.kind, FrameKind::DataProperty | FrameKind::Datatype)
                    && let Subject::Iri(s) = &f.subject
                {
                    let none = Declarations::default();
                    let conv = Conv {
                        build,
                        prefixes: &self.mapping,
                        decls: &none,
                        unit: slice,
                    };
                    if let Ok(iri) = conv.iri(s) {
                        let mut d = self.decls.borrow_mut();
                        match f.kind {
                            FrameKind::DataProperty => d.data_props.insert(iri),
                            _ => d.datatypes.insert(iri),
                        };
                    }
                }
                let decls = self.decls.borrow();
                let conv = Conv {
                    build,
                    prefixes: &self.mapping,
                    decls: &decls,
                    unit: slice,
                };
                let mut out = Out(Vec::new());
                let r = match &unit {
                    Unit::Frame(f) => conv.frame(f, &mut out),
                    Unit::Rule(r) => conv.rule(r, &mut out),
                    Unit::Misc(m) => conv.misc(m, &mut out),
                    _ => unreachable!(),
                };
                if let Err(e) = r {
                    return Step::Error(e.into_horned(&self.buf, self.origin));
                }
                items.extend(out.0.into_iter().map(StreamComponent::Component));
            }
        }
        Step::Unit {
            used,
            items,
            prefix,
            phase,
        }
    }

    /// The trailing block of functional syntax axioms.
    fn general_axioms(&self, start: usize, body: &str) -> Step<A> {
        let _ = start;
        let axioms = body.strip_prefix(GENERAL_AXIOMS).unwrap_or(body).trim();
        let mut items = Vec::new();
        if !axioms.is_empty() {
            let wrapped = format!("Ontology(\n{axioms}\n)");
            let parsed: Result<(crate::ontology::set::SetOntology<A>, _), _> =
                crate::io::ofn::reader::read(
                    &mut std::io::Cursor::new(wrapped.into_bytes()),
                    crate::io::ParserConfiguration::new(self.build.as_ref()),
                );
            match parsed {
                Ok((block, _)) => {
                    for ac in block {
                        if matches!(
                            ac.component,
                            Component::OntologyID(_) | Component::DocIRI(_)
                        ) {
                            continue;
                        }
                        items.push(StreamComponent::Component(ac));
                    }
                }
                Err(e) => {
                    let n = axioms.lines().filter(|l| !l.trim().is_empty()).count();
                    eprintln!(
                        "warning: omn reader could not parse the {n}-line \
                         `# General axioms` block ({e}); skipping it"
                    );
                }
            }
        }
        Step::Unit {
            used: self.buf.len() - self.pos,
            items,
            prefix: None,
            phase: Phase::Done,
        }
    }

    fn next_item(&mut self) -> Result<Option<Item<A>>, HornedError> {
        loop {
            if let Some(it) = self.pending.pop_front() {
                return Ok(Some(it));
            }
            if self.phase == Phase::Done {
                return Ok(None);
            }
            match self.parse_next() {
                Step::NeedMore => self.fill()?,
                Step::End => self.phase = Phase::Done,
                Step::Error(e) => return Err(e),
                Step::Unit {
                    used,
                    items,
                    prefix,
                    phase,
                } => {
                    self.pos += used;
                    self.phase = phase;
                    if let Some((name, iri)) = prefix {
                        // Checked in `convert_unit`.
                        let _ = self.mapping.add_prefix(&name, &iri);
                    }
                    self.pending.extend(items);
                }
            }
        }
    }
}

/// The components of the ontology header, with the id last as the pest
/// reader inserts it.
fn header<'a, A: ForIRI>(
    conv: &Conv<'_, 'a, A>,
    h: &Header<'a>,
    out: &mut Out<A>,
) -> Result<(), PErr<'a>> {
    let mut id = OntologyID::default();
    let mut has_id = false;
    if let Some(iri) = &h.iri {
        id.iri = Some(conv.iri(iri)?);
        has_id = true;
    }
    if let Some(iri) = &h.version {
        id.viri = Some(conv.iri(iri)?);
        has_id = true;
    }
    for iri in &h.imports {
        out.insert(Import(conv.iri(iri)?));
    }
    for a in &h.annotations {
        for ann in conv.annotations(a)? {
            out.insert(OntologyAnnotation(ann));
        }
    }
    if has_id {
        out.insert(id);
    }
    Ok(())
}

impl<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> Iterator for Reader<A, R, B> {
    type Item = crate::io::Result<Item<A>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_item() {
            Ok(Some(item)) => Some(Ok(item)),
            Ok(None) => None,
            Err(e) => {
                self.phase = Phase::Done;
                self.pending.clear();
                Some(Err(e))
            }
        }
    }
}

/// Stream `bufread` as `StreamComponent`s.
pub fn read_to_stream<A: ForIRI, B: AsRef<Build<A>>, R: BufRead>(
    bufread: R,
    config: crate::io::ParserConfiguration<A, B>,
) -> Reader<A, R, B> {
    Reader::new(bufread, config)
}

// ---------------------------------------------------------------------------
// Declarations

/// The `DataProperty:` and `Datatype:` subjects anywhere in `text`.
pub(super) fn prescan<A: ForIRI>(text: &str, build: &Build<A>) -> Declarations<A> {
    let mut prefixes = PrefixMapping::default();
    let mut decls = Declarations::default();
    let mut from = 0;
    let first = sp(text);
    let mut pending_first = Some(text.len() - first.len());
    loop {
        let at = match pending_first.take() {
            Some(p) if p < text.len() => p,
            _ => match scan_candidate(text, from, true) {
                Scan::At(c) => c,
                _ => break,
            },
        };
        from = at + 1;
        let syn = Syntax::new();
        let tail = &text[at..];
        let word = tail.split(':').next().unwrap_or("").to_ascii_lowercase();
        match word.as_str() {
            "prefix" => {
                if let Some((_, Unit::Prefix(name, iri))) = syn.unit(tail) {
                    let _ = prefixes.add_prefix(name, iri);
                }
            }
            "dataproperty" | "datatype" => {
                if let Some((kind, iri)) = syn.frame_head(tail) {
                    let conv = Conv {
                        build,
                        prefixes: &prefixes,
                        decls: &Declarations::default(),
                        unit: tail,
                    };
                    if let Ok(iri) = conv.iri(&iri) {
                        match kind {
                            FrameKind::DataProperty => decls.data_props.insert(iri),
                            _ => decls.datatypes.insert(iri),
                        };
                    }
                }
            }
            _ => {}
        }
    }
    decls
}
