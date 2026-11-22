//! A streaming OBO reader.
//!
//! An OBO document is a header followed by stanzas (`[Term]`, `[Typedef]`,
//! `[Instance]`). [`Reader`] reads one stanza's lines at a time, parses
//! them and yields the components, so memory is bounded by the largest
//! stanza and what the final declarations need (a set of IRIs).
//!
//! A stanza ends at a line that starts the next one. A stanza keyword can
//! also be the start of a continuation line, after a backslash at the end
//! of the line before; that is not taken as the end.
use std::collections::{HashMap, VecDeque};
use std::io::BufRead;

use curie::PrefixMapping;

use super::convert::*;
use super::syntax::*;
use crate::error::HornedError;
use crate::io::StreamComponent;
use crate::io::obo::common::{Context, Finalizer};
use crate::io::ofn::reader::error::{Origin, PErr};
use crate::model::*;

/// Parse `buf` with `parse`, reading more text while a quoted string ran
/// off the end of it. Gives the bytes used and the value, which borrows
/// `buf`.
macro_rules! parse_growing {
    ($self:ident, $buf:ident, $parse:expr) => {
        loop {
            reset_ran_off_end();
            let found = ($parse)(&$buf).map(|(rest, v)| ($buf.len() - rest.len(), v));
            if ran_off_end() && $self.lookahead.is_some() {
                drop(found);
                $self.grow(&mut $buf)?;
                continue;
            }
            break found;
        }
    };
}

pub(super) type Item<A> = StreamComponent<AnnotatedComponent<A>>;

#[derive(Clone, Copy, PartialEq, Eq)]
enum Phase {
    Header,
    Frames,
    Finish,
    Done,
}

/// Streams an OBO document as [`StreamComponent`]s, one stanza at a time.
/// See the [module documentation](self).
///
/// A bare `[Typedef]` id with one `xref` is a shorthand for that xref, and
/// a typedef with `is_metadata_tag: true` is an annotation property, and
/// both change how the other frames read. A streaming reader knows only the
/// typedefs it has read, so a use that comes before the typedef is read
/// without them. [`read`](super::read) reads the typedefs first, so it does
/// not have this limit.
///
/// The declarations of entities that are used but never declared, and the
/// labels of the built-in annotation properties, depend on the whole
/// document, so they come at the end of the stream.
pub struct Reader<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> {
    input: R,
    build: B,
    mapping: PrefixMapping,
    phase: Phase,
    /// The first line of the next stanza, already read.
    lookahead: Option<String>,
    /// Text read but not yet parsed. It starts at the start of a line and
    /// holds whole stanzas: a quoted string can run over lines, so a stanza
    /// can be read ahead of where it was thought to end.
    buf: String,
    origin: Origin,
    idspace: HashMap<String, String>,
    default_ns: Option<String>,
    onto_ns: Option<String>,
    typedefs: Typedefs,
    /// `typedefs` is complete, so frames do not add to it.
    typedefs_complete: bool,
    metadata_tags: std::collections::BTreeSet<String>,
    finalizer: Option<Finalizer<A>>,
    pending: VecDeque<Item<A>>,
}

impl<A: ForIRI, R: BufRead, B: AsRef<Build<A>>> Reader<A, R, B> {
    pub fn new(bufread: R, config: crate::io::ParserConfiguration<A, B>) -> Self {
        Reader {
            input: bufread,
            build: config.build,
            mapping: PrefixMapping::default(),
            phase: Phase::Header,
            lookahead: None,
            buf: String::new(),
            origin: Origin::default(),
            idspace: HashMap::new(),
            default_ns: None,
            onto_ns: None,
            typedefs: Typedefs::default(),
            typedefs_complete: false,
            metadata_tags: Default::default(),
            finalizer: Some(Finalizer::default()),
            pending: VecDeque::new(),
        }
    }

    pub fn from_bufread(bufread: R, config: crate::io::ParserConfiguration<A, B>) -> Self {
        Self::new(bufread, config)
    }

    pub fn prefixes(&self) -> &PrefixMapping {
        &self.mapping
    }

    pub fn into_prefixes(self) -> PrefixMapping {
        self.mapping
    }

    /// Use these typedefs, as if every `[Typedef]` frame had been read already.
    pub(super) fn with_typedefs(mut self, typedefs: Typedefs) -> Self {
        self.typedefs = typedefs;
        self.typedefs_complete = true;
        self
    }

    /// The next line with its terminator, decoded leniently as the pest
    /// reader does.
    fn read_line(&mut self) -> Result<Option<String>, HornedError> {
        let mut bytes = Vec::new();
        if self.input.read_until(b'\n', &mut bytes)? == 0 {
            return Ok(None);
        }
        Ok(Some(String::from_utf8_lossy(&bytes).into_owned()))
    }

    /// The text of a unit: `first` and the lines after it up to the line that
    /// starts the next stanza, which is kept in `lookahead`.
    fn read_unit(&mut self, first: Option<String>) -> Result<String, HornedError> {
        let mut unit = first.unwrap_or_default();
        let mut continued = continues(&unit);
        while let Some(line) = self.read_line()? {
            if !continued && starts_frame(&line) {
                self.lookahead = Some(line);
                break;
            }
            continued = continues(&line);
            unit.push_str(&line);
        }
        Ok(unit)
    }

    fn syntax_error(&self, at: &str, what: &'static str, window: &str) -> HornedError {
        PErr::expected(at, what).into_horned(window, self.origin)
    }

    /// Add the next stanza's text to `buf`. False if there is none.
    fn grow(&mut self, buf: &mut String) -> Result<bool, HornedError> {
        let Some(first) = self.lookahead.take() else {
            return Ok(false);
        };
        let more = self.read_unit(Some(first))?;
        buf.push_str(&more);
        Ok(true)
    }

    fn header(&mut self) -> Result<(), HornedError> {
        let mut buf = self.read_unit(None)?;
        let parsed = parse_growing!(self, buf, |s| {
            let (rest, h) = header(s);
            Some((rest, h))
        });
        let Some((used, h)) = parsed else {
            return Err(self.syntax_error(&buf, "header", &buf));
        };
        let (pm, idspace, default_ns, onto_ns) = scan_header(&h);
        self.idspace = idspace;
        self.default_ns = default_ns;
        self.onto_ns = onto_ns;
        if self.typedefs_complete {
            self.metadata_tags = self
                .typedefs
                .metadata_tags(&self.idspace, self.onto_ns.as_deref());
        }
        let mut prefixes: Vec<_> = pm.mappings().collect();
        prefixes.sort();
        for (name, iri) in prefixes {
            self.pending
                .push_back(StreamComponent::Prefix(name.clone(), iri.clone()));
        }
        self.mapping = pm;

        let comps = self.with_context(|ctx| header_components(&h, ctx))?;
        self.emit(comps);
        self.origin = self.origin.advance(&buf[..used]);
        // What the header did not use may start the first stanza: it ends
        // at a line it cannot read, and a comment is also what a stanza may
        // begin with.
        let rest = buf[used..].to_string();
        self.buf = rest;
        Ok(())
    }

    fn emit(&mut self, comps: Vec<AnnotatedComponent<A>>) {
        for ac in comps {
            if let Some(f) = self.finalizer.as_mut() {
                f.observe(&ac);
            }
            self.pending.push_back(StreamComponent::Component(ac));
        }
    }

    /// Run `f` with a conversion context made from what the reader knows.
    fn with_context<T>(&mut self, f: impl FnOnce(&Context<'_, A>) -> T) -> T {
        let ctx = Context {
            build: self.build.as_ref(),
            idspace: std::mem::take(&mut self.idspace),
            default_ns: self.default_ns.take(),
            onto_ns: self.onto_ns.take(),
            rel_map: std::mem::take(&mut self.typedefs.rel_map),
            metadata_tags: std::mem::take(&mut self.metadata_tags),
        };
        let out = f(&ctx);
        self.idspace = ctx.idspace;
        self.default_ns = ctx.default_ns;
        self.onto_ns = ctx.onto_ns;
        self.typedefs.rel_map = ctx.rel_map;
        self.metadata_tags = ctx.metadata_tags;
        out
    }

    fn frame(&mut self) -> Result<(), HornedError> {
        let mut buf = std::mem::take(&mut self.buf);
        if skip(&buf).is_empty() {
            buf.clear();
            if !self.grow(&mut buf)? {
                self.phase = Phase::Finish;
                return Ok(());
            }
        } else if self.lookahead.is_some() && !contains_frame(&buf) {
            // Only blank lines and comments so far.
            self.grow(&mut buf)?;
        }
        let Some((used, frame)) = parse_growing!(self, buf, frame) else {
            return Err(self.syntax_error(&buf, "stanza", &buf));
        };
        if !self.typedefs_complete && frame.kind == FrameKind::Typedef {
            self.typedefs
                .observe(&frame, &self.idspace, self.onto_ns.as_deref());
            self.metadata_tags = self
                .typedefs
                .metadata_tags(&self.idspace, self.onto_ns.as_deref());
        }
        let comps = self.with_context(|ctx| frame_components(&frame, ctx))?;
        self.emit(comps);
        self.origin = self.origin.advance(&buf[..used]);
        buf.drain(..used);
        self.buf = buf;
        Ok(())
    }

    fn next_item(&mut self) -> Result<Option<Item<A>>, HornedError> {
        loop {
            if let Some(it) = self.pending.pop_front() {
                return Ok(Some(it));
            }
            match self.phase {
                Phase::Header => {
                    self.header()?;
                    self.phase = Phase::Frames;
                }
                Phase::Frames => self.frame()?,
                Phase::Finish => {
                    if let Some(f) = self.finalizer.take() {
                        let build = self.build.as_ref();
                        for ac in f.finish(build) {
                            self.pending.push_back(StreamComponent::Component(ac));
                        }
                    }
                    self.phase = Phase::Done;
                }
                Phase::Done => return Ok(None),
            }
        }
    }
}

/// Does the line end in an escaping backslash, which makes the next line a
/// continuation of it?
fn continues(line: &str) -> bool {
    let t = line.trim_end_matches(['\n', '\r']);
    (t.len() - t.trim_end_matches('\\').len()) % 2 == 1
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

/// Does `text` have a line that starts a stanza?
fn contains_frame(text: &str) -> bool {
    text.split_inclusive('\n').any(starts_frame)
}

/// The `[Typedef]` frames of `text`, read ahead of the rest.
pub(super) fn prescan(text: &str) -> Typedefs {
    let mut typedefs = Typedefs::default();
    let (_, header) = header(text);
    let (_, idspace, _, onto_ns) = scan_header(&header);
    let mut pos = 0;
    let mut continued = false;
    for line in text.split_inclusive('\n') {
        if !continued
            && frame_kind(line) == Some(FrameKind::Typedef)
            && let Some((_, f)) = frame(&text[pos..])
        {
            typedefs.observe(&f, &idspace, onto_ns.as_deref());
        }
        continued = continues(line);
        pos += line.len();
    }
    typedefs
}
