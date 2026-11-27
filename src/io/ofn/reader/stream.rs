//! A streaming OFN reader.
//!
//! The nom parsers in this module tree work on text that is already in
//! memory. To stream, [`Reader`] first finds where the next construct ends
//! with a cheap scan that tracks only brackets, IRIs, strings and
//! comments, reading more input whenever the construct is incomplete. It
//! then hands exactly that text to the ordinary parser, so memory is bounded
//! by the largest single construct rather than the whole document.
//!
//! Items are yielded in document order: prefixes, the ontology id, imports,
//! ontology annotations, then axioms.
use std::collections::BTreeSet;
use std::io::{BufRead, Read};
use std::marker::PhantomData;

use curie::PrefixMapping;
use nom::Err;

use crate::error::HornedError;
use crate::io::StreamComponent;
use crate::model::*;

use super::error::{Origin, PErr};
use super::lex::{PResult, peek_kw, sp};
use super::{Ctx, axiom, import, iri, ontology_annotation, prefix_decl};

const MIN_CHUNK: usize = 64 * 1024;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Kind {
    /// Nothing but whitespace and comments remains.
    Eof,
    /// The `)` that closes `Ontology(`.
    Close,
    /// `Ontology(`, which is not scanned to its end.
    Open,
    /// `Keyword( ... )`.
    Keyword,
    /// A bare IRI.
    Token,
}

struct Found {
    start: usize,
    end: usize,
    kind: Kind,
}

enum Scanned {
    /// The text ends inside the next element.
    More,
    Found(Found),
}

fn is_delimiter(b: u8) -> bool {
    matches!(
        b,
        b' ' | b'\t' | b'\n' | b'\r' | b'(' | b')' | b'<' | b'>' | b'"' | b'#'
    )
}

/// Skip whitespace and comments from `i`. `None` means a comment runs off
/// the end of the text.
fn skip_space(b: &[u8], mut i: usize, eof: bool) -> Option<usize> {
    loop {
        while i < b.len() && matches!(b[i], b' ' | b'\t' | b'\n' | b'\r') {
            i += 1;
        }
        if i < b.len() && b[i] == b'#' {
            match b[i..].iter().position(|&c| c == b'\n' || c == b'\r') {
                Some(p) => i += p,
                None if eof => return Some(b.len()),
                None => return None,
            }
        } else {
            return Some(i);
        }
    }
}

/// Skip `<...>` starting at `i`, returning the index after the `>`.
fn skip_iri(b: &[u8], i: usize, eof: bool) -> Option<usize> {
    match b[i + 1..].iter().position(|&c| c == b'>') {
        Some(p) => Some(i + 1 + p + 1),
        None if eof => Some(b.len()),
        None => None,
    }
}

/// Skip `"..."` starting at `i`, honouring `\\` and `\"` escapes.
fn skip_string(b: &[u8], mut i: usize, eof: bool) -> Option<usize> {
    i += 1;
    while i < b.len() {
        match b[i] {
            b'\\' => match b.get(i + 1) {
                Some(b'\\') | Some(b'"') => i += 2,
                Some(_) => i += 1,
                None if eof => i += 1,
                None => return None,
            },
            b'"' => return Some(i + 1),
            _ => i += 1,
        }
    }
    if eof { Some(b.len()) } else { None }
}

/// Skip a bracketed construct whose `(` is at `i`, returning the index
/// after the matching `)`.
fn skip_balanced(b: &[u8], mut i: usize, eof: bool) -> Option<usize> {
    let mut depth = 0usize;
    while i < b.len() {
        match b[i] {
            b'(' => {
                depth += 1;
                i += 1;
            }
            b')' => {
                depth -= 1;
                i += 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            b'<' => i = skip_iri(b, i, eof)?,
            b'"' => i = skip_string(b, i, eof)?,
            b'#' => i = skip_space(b, i, eof)?,
            _ => i += 1,
        }
    }
    if eof { Some(b.len()) } else { None }
}

/// Find the next element in `s`. With `top`, `Ontology(` is reported
/// without scanning its contents.
fn scan(s: &str, eof: bool, top: bool) -> Scanned {
    let b = s.as_bytes();
    let found = |start, end, kind| Scanned::Found(Found { start, end, kind });
    let Some(start) = skip_space(b, 0, eof) else {
        return Scanned::More;
    };
    if start == b.len() {
        return if eof {
            found(start, start, Kind::Eof)
        } else {
            Scanned::More
        };
    }
    match b[start] {
        b')' => found(start, start + 1, Kind::Close),
        b'<' => match skip_iri(b, start, eof) {
            Some(end) => found(start, end, Kind::Token),
            None => Scanned::More,
        },
        b'"' => match skip_string(b, start, eof) {
            Some(end) => found(start, end, Kind::Token),
            None => Scanned::More,
        },
        _ => {
            let mut j = start;
            while j < b.len() && !is_delimiter(b[j]) {
                j += 1;
            }
            if j == b.len() && !eof {
                return Scanned::More;
            }
            let Some(k) = skip_space(b, j, eof) else {
                return Scanned::More;
            };
            if k == b.len() && !eof {
                return Scanned::More;
            }
            if k < b.len() && b[k] == b'(' {
                if top && &b[start..j] == b"Ontology" {
                    return found(start, k + 1, Kind::Open);
                }
                match skip_balanced(b, k, eof) {
                    Some(end) => found(start, end, Kind::Keyword),
                    None => Scanned::More,
                }
            } else {
                found(start, j.max(start + 1), Kind::Token)
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Phase {
    Top,
    Header,
    Imports,
    Annotations,
    Axioms,
    Trailer,
    Done,
}

/// Streams an OFN document as [`StreamComponent`]s, one construct at a
/// time. See the [module documentation](self).
pub struct Reader<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> {
    input: R,
    build: B,
    mapping: PrefixMapping,
    /// Text read but not yet discarded, always valid UTF-8.
    buf: String,
    /// Start of the unparsed text in `buf`.
    pos: usize,
    /// Where `buf` starts in the document.
    origin: Origin,
    /// The start of a UTF-8 sequence cut by the end of the last read.
    tail: Vec<u8>,
    eof: bool,
    started: bool,
    phase: Phase,
    min_chunk: usize,
    _marker: PhantomData<A>,
}

impl<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> Reader<A, R, B> {
    /// A reader over `bufread`. No I/O happens until the first item is
    /// requested.
    pub fn new(bufread: R, config: crate::io::ParserConfiguration<A, B>) -> Self {
        Reader {
            input: bufread,
            build: config.build,
            mapping: PrefixMapping::default(),
            buf: String::new(),
            pos: 0,
            origin: Origin::default(),
            tail: Vec::new(),
            eof: false,
            started: false,
            phase: Phase::Top,
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

    /// The prefixes read so far.
    pub fn prefixes(&self) -> &PrefixMapping {
        &self.mapping
    }

    pub fn into_prefixes(self) -> PrefixMapping {
        self.mapping
    }

    /// Drop the text already parsed, then read more.
    fn fill(&mut self) -> Result<(), HornedError> {
        if !self.started {
            self.started = true;
            crate::io::skip_bom(&mut self.input)?;
        }
        let consumed = &self.buf[..self.pos];
        self.origin = self.origin.advance(consumed);
        self.buf.drain(..self.pos);
        self.pos = 0;

        // Grow geometrically so an element larger than the buffer is
        // rescanned only a logarithmic number of times.
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

    /// The next element of the document, reading as needed.
    fn peek_element(&mut self, top: bool) -> Result<Found, HornedError> {
        loop {
            match scan(&self.buf[self.pos..], self.eof, top) {
                Scanned::More => self.fill()?,
                Scanned::Found(f) => {
                    return Ok(Found {
                        start: self.pos + f.start,
                        end: self.pos + f.end,
                        kind: f.kind,
                    });
                }
            }
        }
    }

    fn error(&self, e: Err<PErr<'_>>) -> HornedError {
        match e {
            Err::Error(e) | Err::Failure(e) => e.into_horned(&self.buf, self.origin),
            Err::Incomplete(_) => HornedError::invalid("incomplete input"),
        }
    }

    fn error_at(&self, at: usize, expected: &'static str) -> HornedError {
        self.error(Err::Failure(PErr::expected(&self.buf[at..], expected)))
    }

    /// Parse the element ending at `end` with `f`.
    fn parse<T>(
        &self,
        end: usize,
        f: impl for<'x> FnOnce(&Ctx<'_, A>, &'x str) -> PResult<'x, T>,
    ) -> Result<T, HornedError> {
        let ctx = Ctx {
            build: self.build.as_ref(),
            mapping: &self.mapping,
            depth: std::cell::Cell::new(0),
        };
        match f(&ctx, &self.buf[self.pos..end]) {
            Ok((rest, v)) if sp(rest).is_empty() => Ok(v),
            Ok((rest, _)) => Err(self.error_at_ptr(rest, "end of element")),
            Err(e) => Err(self.error(e)),
        }
    }

    fn error_at_ptr(&self, rest: &str, expected: &'static str) -> HornedError {
        self.error(Err::Failure(PErr::expected(sp(rest), expected)))
    }

    /// Skip Unicode whitespace that is not OFN whitespace, which the
    /// in-memory reader removes by trimming the document.
    fn skip_unicode_space(&mut self, at: usize) -> bool {
        match self.buf[at..].chars().next() {
            Some(c) if c.is_whitespace() => {
                self.pos = at + c.len_utf8();
                true
            }
            _ => false,
        }
    }

    fn component<C: Into<Component<A>>>(c: C) -> StreamComponent<AnnotatedComponent<A>> {
        StreamComponent::Component(AnnotatedComponent::new(c, BTreeSet::new()))
    }

    fn next_item(&mut self) -> Result<Option<StreamComponent<AnnotatedComponent<A>>>, HornedError> {
        loop {
            match self.phase {
                Phase::Done => return Ok(None),

                Phase::Top => {
                    let f = self.peek_element(true)?;
                    if f.kind != Kind::Open
                        && self.skip_unicode_space(f.start)
                        && f.kind == Kind::Eof
                    {
                        continue;
                    }
                    match f.kind {
                        Kind::Open => {
                            self.pos = f.end;
                            self.phase = Phase::Header;
                        }
                        Kind::Keyword if peek_kw(&self.buf[f.start..f.end]) == Some("Prefix") => {
                            let (name, iri) = match prefix_decl(&self.buf[f.start..f.end]) {
                                Ok((_, (n, i))) => (n.to_string(), i.to_string()),
                                Err(e) => return Err(self.error(e)),
                            };
                            self.mapping.add_prefix(&name, &iri).map_err(|_| {
                                HornedError::invalid_at(
                                    "invalid prefix declaration",
                                    crate::error::Location::BytePosition(
                                        (self.origin.offset + f.start) as u64,
                                    ),
                                )
                            })?;
                            self.pos = f.end;
                            return Ok(Some(StreamComponent::Prefix(name, iri)));
                        }
                        _ => return Err(self.error_at(f.start, "`Prefix` or `Ontology`")),
                    }
                }

                Phase::Header => {
                    let mut id = OntologyID::default();
                    for stage in 0..2 {
                        let f = self.peek_element(false)?;
                        if f.kind != Kind::Token {
                            break;
                        }
                        let x = self.parse(f.end, iri)?;
                        self.pos = f.end;
                        if stage == 0 {
                            id.iri = Some(x);
                        } else {
                            id.viri = Some(x);
                        }
                    }
                    self.phase = Phase::Imports;
                    // An anonymous ontology has no OntologyID.
                    if id.iri.is_some() {
                        return Ok(Some(Self::component(id)));
                    }
                }

                Phase::Imports | Phase::Annotations | Phase::Axioms => {
                    let f = self.peek_element(false)?;
                    match f.kind {
                        Kind::Close => {
                            self.pos = f.end;
                            self.phase = Phase::Trailer;
                        }
                        Kind::Eof | Kind::Open => {
                            return Err(self.error(Err::Failure(
                                PErr::expected(&self.buf[f.start..], "axiom or `)`")
                                    .within("Ontology"),
                            )));
                        }
                        Kind::Keyword | Kind::Token => {
                            let kw = peek_kw(&self.buf[f.start..f.end]);
                            let item = match (self.phase, kw) {
                                (Phase::Imports, Some("Import")) => {
                                    Self::component(self.parse(f.end, import)?)
                                }
                                (Phase::Imports | Phase::Annotations, Some("Annotation")) => {
                                    self.phase = Phase::Annotations;
                                    Self::component(self.parse(f.end, ontology_annotation)?)
                                }
                                _ => {
                                    self.phase = Phase::Axioms;
                                    StreamComponent::Component(self.parse(f.end, axiom)?)
                                }
                            };
                            self.pos = f.end;
                            return Ok(Some(item));
                        }
                    }
                }

                Phase::Trailer => {
                    let f = self.peek_element(false)?;
                    if self.skip_unicode_space(f.start) {
                        continue;
                    }
                    if f.kind == Kind::Eof {
                        self.phase = Phase::Done;
                        return Ok(None);
                    }
                    return Err(self.error_at(f.start, "end of input"));
                }
            }
        }
    }
}

impl<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> Iterator for Reader<A, R, B> {
    type Item = crate::io::Result<StreamComponent<AnnotatedComponent<A>>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_item() {
            Ok(Some(item)) => Some(Ok(item)),
            Ok(None) => None,
            Err(e) => {
                self.phase = Phase::Done;
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
