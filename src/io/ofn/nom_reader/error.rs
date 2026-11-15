//! Error type for the nom OFN reader.
//!
//! Collects the furthest failure and what was expected there, plus the
//! enclosing construct, and renders them with line, column and a source
//! excerpt.
use std::fmt;

use nom::error::{ContextError, ErrorKind, ParseError};

use crate::error::{HornedError, Location};

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

    pub fn into_horned(self, doc: &str) -> HornedError {
        let offset = doc.len() - self.at.len();
        if let Some((msg, len)) = self.semantic {
            let (line, col) = line_col(doc, offset);
            let msg = format!("{msg} (line {line}, column {col})");
            return HornedError::invalid_at(
                msg,
                Location::ByteSpan(offset as u64..(offset + len) as u64),
            );
        }
        let (line, col) = line_col(doc, offset);
        let line_text = doc[offset - (col - 1).min(offset)..]
            .lines()
            .next()
            .unwrap_or("");
        HornedError::ParserError(
            Box::new(SyntaxError {
                line,
                col,
                expected: self.expected,
                within: self.within.map(str::to_string),
                found: found(self.at),
                line_text: line_text.to_string(),
            }),
            Location::BytePosition(offset as u64),
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

/// 1-based line and character column of a byte offset.
fn line_col(doc: &str, offset: usize) -> (usize, usize) {
    let before = &doc[..offset];
    let line = before.matches('\n').count() + 1;
    let start = before.rfind('\n').map(|p| p + 1).unwrap_or(0);
    (line, before[start..].chars().count() + 1)
}

#[derive(Debug)]
pub struct SyntaxError {
    line: usize,
    col: usize,
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
        write!(
            f,
            "\n  {text}\n  {}^",
            " ".repeat(self.col.saturating_sub(1).min(120))
        )
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
