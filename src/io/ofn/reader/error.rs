//! Error type for the nom OFN reader.
//!
//! Collects the furthest failure and what was expected there, plus the
//! enclosing construct, and renders them with line, column and a source
//! excerpt.
use std::fmt;

use nom::error::{ContextError, ErrorKind, ParseError};

use crate::error::{HornedError, Location};

/// Where the first byte of a window of text sits in the whole document.
///
/// A streaming reader holds only part of the document, so errors are
/// located relative to the window and shifted by its origin.
#[derive(Debug, Clone, Copy)]
pub struct Origin {
    pub offset: usize,
    /// 1-based line.
    pub line: usize,
    /// 1-based character column.
    pub col: usize,
}

impl Default for Origin {
    fn default() -> Self {
        Origin {
            offset: 0,
            line: 1,
            col: 1,
        }
    }
}

impl Origin {
    /// The origin of whatever follows `skipped`, which starts at `self`.
    pub fn advance(self, skipped: &str) -> Origin {
        let (line, col) = locate(skipped, self, skipped.len());
        Origin {
            offset: self.offset + skipped.len(),
            line,
            col,
        }
    }
}

#[derive(Debug, Clone)]
pub struct PErr<'a> {
    /// The remaining input at the point of failure.
    at: &'a str,
    expected: Vec<&'static str>,
    /// The enclosing construct, such as `SubClassOf`.
    within: Option<&'a str>,
    /// A failure that is not about syntax: message and span length in bytes.
    semantic: Option<(String, usize)>,
}

impl<'a> PErr<'a> {
    pub fn expected(at: &'a str, what: &'static str) -> Self {
        PErr {
            at,
            expected: vec![what],
            within: None,
            semantic: None,
        }
    }

    pub fn semantic(at: &'a str, len: usize, msg: impl Into<String>) -> Self {
        PErr {
            at,
            expected: vec![],
            within: None,
            semantic: Some((msg.into(), len)),
        }
    }

    pub fn within(mut self, construct: &'a str) -> Self {
        if self.within.is_none() {
            self.within = Some(construct);
        }
        self
    }

    /// Convert to a `HornedError`. `window` is the text the parser was
    /// given, which `self.at` points into, and `origin` is where `window`
    /// starts in the document.
    pub fn into_horned(self, window: &str, origin: Origin) -> HornedError {
        let off = (self.at.as_ptr() as usize)
            .saturating_sub(window.as_ptr() as usize)
            .min(window.len());
        let (line, col) = locate(window, origin, off);
        let abs = (origin.offset + off) as u64;
        if let Some((msg, len)) = self.semantic {
            let msg = format!("{msg} (line {line}, column {col})");
            return HornedError::invalid_at(msg, Location::ByteSpan(abs..abs + len as u64));
        }
        let line_start = window[..off].rfind('\n').map_or(0, |p| p + 1);
        let line_end = window[off..]
            .find(['\n', '\r'])
            .map_or(window.len(), |p| off + p);
        HornedError::ParserError(
            Box::new(SyntaxError {
                line,
                col,
                caret: window[line_start..off].chars().count(),
                expected: self.expected,
                within: self.within.map(str::to_string),
                found: found(self.at),
                line_text: window[line_start..line_end].to_string(),
            }),
            Location::BytePosition(abs),
        )
    }
}

fn found(at: &str) -> String {
    let t: String = at.chars().take(24).collect();
    if t.is_empty() {
        "end of input".to_string()
    } else if t.chars().count() < at.chars().count() {
        format!("`{}...`", t.trim_end())
    } else {
        format!("`{t}`")
    }
}

/// 1-based line and character column of byte `off` in `window`.
fn locate(window: &str, origin: Origin, off: usize) -> (usize, usize) {
    let before = &window[..off];
    let newlines = before.bytes().filter(|&b| b == b'\n').count();
    match before.rfind('\n') {
        Some(p) => (origin.line + newlines, before[p + 1..].chars().count() + 1),
        None => (origin.line, origin.col + before.chars().count()),
    }
}

#[derive(Debug)]
pub struct SyntaxError {
    line: usize,
    col: usize,
    caret: usize,
    expected: Vec<&'static str>,
    within: Option<String>,
    found: String,
    line_text: String,
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "syntax error at line {}, column {}", self.line, self.col)?;
        if let Some(w) = &self.within {
            write!(f, " in {w}")?;
        }
        if !self.expected.is_empty() {
            write!(f, ": expected {}", self.expected.join(" or "))?;
        }
        write!(f, ", found {}", self.found)?;
        let text: String = self.line_text.chars().take(120).collect();
        write!(f, "\n  {text}\n  {}^", " ".repeat(self.caret.min(120)))
    }
}

impl std::error::Error for SyntaxError {}

impl<'a> ParseError<&'a str> for PErr<'a> {
    fn from_error_kind(input: &'a str, _kind: ErrorKind) -> Self {
        PErr {
            at: input,
            expected: vec![],
            within: None,
            semantic: None,
        }
    }

    fn append(_input: &'a str, _kind: ErrorKind, other: Self) -> Self {
        other
    }

    fn or(mut self, mut other: Self) -> Self {
        if self.semantic.is_some() {
            return self;
        }
        if other.semantic.is_some() {
            return other;
        }
        match self.at.len().cmp(&other.at.len()) {
            std::cmp::Ordering::Less => self,
            std::cmp::Ordering::Greater => other,
            std::cmp::Ordering::Equal => {
                for e in other.expected.drain(..) {
                    if !self.expected.contains(&e) {
                        self.expected.push(e);
                    }
                }
                self
            }
        }
    }
}

impl<'a> ContextError<&'a str> for PErr<'a> {
    fn add_context(input: &'a str, ctx: &'static str, mut other: Self) -> Self {
        if other.expected.is_empty() && other.semantic.is_none() && input.len() == other.at.len() {
            other.expected.push(ctx);
        }
        other
    }
}
