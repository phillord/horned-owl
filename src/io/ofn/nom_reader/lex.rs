//! Lexical level: whitespace, punctuation, keywords, IRIs, literals.
//!
//! Every token parser skips leading whitespace and comments itself.
use nom::bytes::complete::take_while1;
use nom::character::complete::char;
use nom::combinator::cut;
use nom::error::context;
use nom::{Err, IResult, Parser};

use super::error::PErr;

pub type PResult<'a, T> = IResult<&'a str, T, PErr<'a>>;

fn err<'a, T>(at: &'a str, what: &'static str) -> PResult<'a, T> {
    Err(Err::Error(PErr::expected(at, what)))
}

/// Skip whitespace and `#` comments.
pub fn sp(mut i: &str) -> &str {
    loop {
        i = i.trim_start_matches([' ', '\t', '\n', '\r']);
        match i.strip_prefix('#') {
            Some(r) => i = r.find(['\n', '\r']).map_or("", |p| &r[p..]),
            None => return i,
        }
    }
}

pub fn lparen(i: &str) -> PResult<'_, ()> {
    let i = sp(i);
    context("`(`", char('(')).parse(i).map(|(r, _)| (r, ()))
}

pub fn rparen(i: &str) -> PResult<'_, ()> {
    let i = sp(i);
    context("`)`", char(')')).parse(i).map(|(r, _)| (r, ()))
}

/// Is the next token `)`?
pub fn at_rparen(i: &str) -> bool {
    sp(i).starts_with(')')
}

/// The keyword that starts the next construct, if the next token is
/// `Keyword (`. Does not consume anything.
pub fn peek_kw(i: &str) -> Option<&str> {
    let i = sp(i);
    let end = i
        .find(|c: char| !c.is_ascii_alphabetic())
        .unwrap_or(i.len());
    if end == 0 {
        return None;
    }
    sp(&i[end..]).starts_with('(').then(|| &i[..end])
}

/// Consume `Keyword (`, returning the keyword.
pub fn keyword(i: &str) -> PResult<'_, &str> {
    match peek_kw(i) {
        Some(kw) => {
            let i = sp(i);
            let (r, _) = lparen(&i[kw.len()..])?;
            Ok((r, kw))
        }
        None => err(sp(i), "keyword"),
    }
}

/// Consume a specific `Keyword (`.
pub fn expect_kw<'a>(i: &'a str, kw: &'static str) -> PResult<'a, ()> {
    match peek_kw(i) {
        Some(k) if k == kw => keyword(i).map(|(r, _)| (r, ())),
        _ => err(sp(i), kw),
    }
}

/// Consume a single `=`.
pub fn equals(i: &str) -> PResult<'_, ()> {
    let i = sp(i);
    context("`=`", char('=')).parse(i).map(|(r, _)| (r, ()))
}

// ---------------------------------------------------------------------------
// IRIs

fn pn_chars_base(c: char) -> bool {
    c.is_ascii_alphabetic()
        || matches!(c,
            '\u{00C0}'..='\u{00D6}'
            | '\u{00D8}'..='\u{00F6}'
            | '\u{00F8}'..='\u{02FF}'
            | '\u{0370}'..='\u{037D}'
            | '\u{037F}'..='\u{1FFF}'
            | '\u{200C}'..='\u{200D}'
            | '\u{2070}'..='\u{218F}'
            | '\u{2C00}'..='\u{2FEF}'
            | '\u{3001}'..='\u{D7FF}'
            | '\u{F900}'..='\u{FDCF}'
            | '\u{FDF0}'..='\u{FFFD}'
            | '\u{10000}'..='\u{EFFFF}')
}

fn pn_chars_u(c: char) -> bool {
    pn_chars_base(c) || c == '_'
}

fn pn_chars(c: char) -> bool {
    pn_chars_u(c)
        || c == '-'
        || c.is_ascii_digit()
        || matches!(c, '\u{00B7}' | '\u{0300}'..='\u{036F}' | '\u{203F}'..='\u{2040}')
}

/// Byte length of `(("." PN_CHARS) | PN_CHARS)*` at the start of `s`.
fn pn_tail(s: &str) -> usize {
    let mut chars = s.char_indices().peekable();
    let mut end = 0;
    while let Some((p, c)) = chars.next() {
        if c == '.' {
            match chars.peek() {
                Some(&(_, n)) if pn_chars(n) => {
                    chars.next();
                    end = p + 1 + n.len_utf8();
                }
                _ => break,
            }
        } else if pn_chars(c) {
            end = p + c.len_utf8();
        } else {
            break;
        }
    }
    end
}

/// Byte length of a blank node label / local name.
fn pn_local(s: &str) -> Option<usize> {
    let c = s.chars().next()?;
    if !(pn_chars_u(c) || c.is_ascii_digit()) {
        return None;
    }
    let first = c.len_utf8();
    Some(first + pn_tail(&s[first..]))
}

/// `prefix:`; returns the prefix, possibly empty.
pub fn pname_ns(i: &str) -> PResult<'_, &str> {
    let i = sp(i);
    let plen = match i.chars().next() {
        Some(':') => 0,
        Some(c) if pn_chars_base(c) => c.len_utf8() + pn_tail(&i[c.len_utf8()..]),
        _ => return err(i, "prefix name"),
    };
    match i[plen..].strip_prefix(':') {
        Some(r) => Ok((r, &i[..plen])),
        None => err(&i[plen..], "`:`"),
    }
}

/// A prefixed name, returned as `(prefix, local, whole, rest)`.
fn pname(i: &str) -> PResult<'_, (&str, &str, &str)> {
    let start = sp(i);
    let (r, prefix) = pname_ns(i)?;
    match pn_local(r) {
        Some(n) => Ok((
            &r[n..],
            (prefix, &r[..n], &start[..start.len() - r.len() + n]),
        )),
        None => err(r, "local name"),
    }
}

/// A blank node label `_:name`, returned whole.
pub fn blank_node(i: &str) -> PResult<'_, &str> {
    let i = sp(i);
    let r = match i.strip_prefix("_:") {
        Some(r) => r,
        None => return err(i, "blank node `_:name`"),
    };
    match pn_local(r) {
        Some(n) => Ok((&r[n..], &i[..2 + n])),
        None => err(r, "blank node label"),
    }
}

pub fn at_blank_node(i: &str) -> bool {
    sp(i).starts_with("_:")
}

fn is_ucs_char(c: char) -> bool {
    let u = c as u32;
    (0xA0..=0xD7FF).contains(&u)
        || (0xE000..=0xFDCF).contains(&u)
        || (0xFDF0..=0xFFEF).contains(&u)
        || (u >= 0x10000 && (u & 0xFFFF) < 0xFFFE && u <= 0x10FFFD)
}

/// Check the body of `<...>` against an RFC 3987 absolute IRI.
///
/// This accepts the characters the RFC allows without checking the finer
/// structure of the authority and path.
fn valid_iri(s: &str) -> bool {
    let b = s.as_bytes();
    if !b.first().is_some_and(u8::is_ascii_alphabetic) {
        return false;
    }
    let scheme = b
        .iter()
        .position(|c| !(c.is_ascii_alphanumeric() || matches!(c, b'+' | b'-' | b'.')));
    let Some(colon) = scheme.filter(|&p| b[p] == b':') else {
        return false;
    };
    let rest = &s[colon + 1..];
    let rb = rest.as_bytes();
    let mut hash = false;
    let mut k = 0;
    while k < rb.len() {
        let c = rb[k];
        if c < 0x80 {
            match c {
                b'%' => {
                    if !(rb.get(k + 1).is_some_and(u8::is_ascii_hexdigit)
                        && rb.get(k + 2).is_some_and(u8::is_ascii_hexdigit))
                    {
                        return false;
                    }
                    k += 3;
                    continue;
                }
                b'#' => {
                    if hash {
                        return false;
                    }
                    hash = true;
                }
                c if c.is_ascii_alphanumeric()
                    || matches!(
                        c,
                        b'-' | b'.'
                            | b'_'
                            | b'~'
                            | b'!'
                            | b'$'
                            | b'&'
                            | b'\''
                            | b'('
                            | b')'
                            | b'*'
                            | b'+'
                            | b','
                            | b';'
                            | b'='
                            | b':'
                            | b'/'
                            | b'?'
                            | b'@'
                            | b'['
                            | b']'
                    ) => {}
                _ => return false,
            }
            k += 1;
        } else {
            let ch = rest[k..].chars().next().unwrap();
            if !is_ucs_char(ch) {
                return false;
            }
            k += ch.len_utf8();
        }
    }
    true
}

/// `<...>`, returning the text between the brackets.
pub fn full_iri(i: &str) -> PResult<'_, &str> {
    let i = sp(i);
    let Some(r) = i.strip_prefix('<') else {
        return err(i, "IRI");
    };
    match r.find('>') {
        Some(n) if valid_iri(&r[..n]) => Ok((&r[n + 1..], &r[..n])),
        Some(n) => Err(Err::Failure(PErr::semantic(
            i,
            n + 2,
            "not a valid absolute IRI",
        ))),
        None => err(r, "`>`"),
    }
}

/// An IRI as written: either `<full>` or `prefix:local`.
pub enum RawIri<'a> {
    Full(&'a str),
    Prefixed(&'a str, &'a str, &'a str),
}

pub fn raw_iri(i: &str) -> PResult<'_, RawIri<'_>> {
    let i = sp(i);
    if i.starts_with('<') {
        full_iri(i).map(|(r, s)| (r, RawIri::Full(s)))
    } else {
        pname(i)
            .map(|(r, (p, l, w))| (r, RawIri::Prefixed(p, l, w)))
            .map_err(|e| e.map(|e| relabel(e, i)))
    }
}

/// An IRI was wanted, so say so rather than naming the failed piece.
fn relabel<'a>(_e: PErr<'a>, at: &'a str) -> PErr<'a> {
    PErr::expected(at, "IRI")
}

// ---------------------------------------------------------------------------
// Literals

/// `"..."`, returning the unescaped text.
pub fn quoted_string(i: &str) -> PResult<'_, String> {
    let i = sp(i);
    let Some(r) = i.strip_prefix('"') else {
        return err(i, "quoted string");
    };
    let mut escaped = false;
    let mut k = 0;
    let end = loop {
        match r[k..].find(['"', '\\']) {
            None => return Err(Err::Failure(PErr::expected(i, "closing `\"`"))),
            Some(p) => {
                k += p;
                let b = r.as_bytes();
                if b[k] == b'"' {
                    break k;
                }
                match b.get(k + 1) {
                    Some(b'\\') | Some(b'"') => {
                        escaped = true;
                        k += 2;
                    }
                    _ => k += 1,
                }
            }
        }
    };
    let body = &r[..end];
    let s = if escaped {
        let mut out = String::with_capacity(body.len());
        let mut it = body.chars().peekable();
        while let Some(c) = it.next() {
            if c == '\\' && matches!(it.peek(), Some('\\') | Some('"')) {
                out.push(it.next().unwrap());
            } else {
                out.push(c);
            }
        }
        out
    } else {
        body.to_string()
    };
    Ok((&r[end + 1..], s))
}

/// The part of a language tag after the `@`.
pub fn language_tag(i: &str) -> PResult<'_, &str> {
    let i = sp(i);
    let Some(r) = i.strip_prefix('@') else {
        return err(i, "`@`");
    };
    let (rest, tag) = cut(take_while1(|c: char| c.is_ascii_alphanumeric() || c == '-'))
        .parse(r)
        .map_err(|e: Err<PErr<'_>>| e.map(|_| PErr::expected(r, "language tag")))?;
    let mut subtags = tag.split('-');
    let first = subtags.next().unwrap_or("");
    let ok = (1..=8).contains(&first.len())
        && first.bytes().all(|b| b.is_ascii_alphabetic())
        && subtags.all(|s| (1..=8).contains(&s.len()));
    if ok {
        Ok((rest, tag))
    } else {
        Err(Err::Failure(PErr::semantic(
            r,
            tag.len(),
            "invalid language tag",
        )))
    }
}

pub fn non_negative_integer(i: &str) -> PResult<'_, u32> {
    let i = sp(i);
    let (r, d) = take_while1(|c: char| c.is_ascii_digit())
        .parse(i)
        .map_err(|e: Err<PErr<'_>>| e.map(|_| PErr::expected(i, "non-negative integer")))?;
    match d.parse::<u32>() {
        Ok(n) => Ok((r, n)),
        Err(_) => Err(Err::Failure(PErr::semantic(
            i,
            d.len(),
            "cardinality too large",
        ))),
    }
}
