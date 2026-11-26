//! `<...>` IRI validation, a port of `horned-pest/src/grammars/rfc3987.pest`.
//!
//! The pest grammar is a PEG, so ordered choice commits and repetition
//! does not backtrack. This follows it step for step, so it accepts and
//! rejects exactly the same strings. Alternatives that the RFC leaves
//! unordered are ordered longest first, for example `dec-octet` and `ls32`.

type Idx = Option<usize>;

fn is_ucs_char(c: char) -> bool {
    let u = c as u32;
    (0xA0..=0xD7FF).contains(&u)
        || (0xF900..=0xFDCF).contains(&u)
        || (0xFDF0..=0xFFEF).contains(&u)
        || ((0x10000..=0xDFFFD).contains(&u) && (u & 0xFFFF) <= 0xFFFD)
        || (0xE1000..=0xEFFFD).contains(&u)
}

fn is_private(c: char) -> bool {
    let u = c as u32;
    (0xE000..=0xF8FF).contains(&u)
        || (0xF0000..=0xFFFFD).contains(&u)
        || (0x100000..=0x10FFFD).contains(&u)
}

fn is_unreserved(c: char) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, '-' | '.' | '_' | '~') || is_ucs_char(c)
}

fn is_sub_delim(c: char) -> bool {
    matches!(
        c,
        '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ';' | '=' | ','
    )
}

fn hex(s: &str, i: usize) -> bool {
    s.as_bytes().get(i).is_some_and(u8::is_ascii_hexdigit)
}

/// One character matching `extra` or an unreserved, percent-encoded or
/// sub-delimiter character; returns its length.
fn one(s: &str, i: usize, extra: impl Fn(char) -> bool, sub: bool, pct: bool) -> usize {
    let Some(c) = s[i..].chars().next() else {
        return 0;
    };
    if is_unreserved(c) || (sub && is_sub_delim(c)) || extra(c) {
        c.len_utf8()
    } else if pct && c == '%' && hex(s, i + 1) && hex(s, i + 2) {
        3
    } else {
        0
    }
}

/// Length of the longest run of `one` characters at `i`.
fn run(s: &str, mut i: usize, extra: impl Fn(char) -> bool + Copy, sub: bool, pct: bool) -> usize {
    let start = i;
    loop {
        let n = one(s, i, extra, sub, pct);
        if n == 0 {
            return i - start;
        }
        i += n;
    }
}

fn no(_: char) -> bool {
    false
}

fn ip_char_extra(c: char) -> bool {
    matches!(c, ':' | '@')
}

fn ip_run(s: &str, i: usize) -> usize {
    run(s, i, ip_char_extra, true, true)
}

/// `("/" Segment)*` from `i`.
fn slash_segments(s: &str, mut i: usize) -> usize {
    while s[i..].starts_with('/') {
        i += 1 + ip_run(s, i + 1);
    }
    i
}

fn h16(s: &str, i: usize) -> Idx {
    let n = s.as_bytes()[i..]
        .iter()
        .take(4)
        .take_while(|b| b.is_ascii_hexdigit())
        .count();
    (n > 0).then_some(i + n)
}

fn colon(s: &str, i: usize) -> Idx {
    s[i..].starts_with(':').then_some(i + 1)
}

/// `(H16 ":"){n}`
fn h16_colons(s: &str, mut i: usize, n: usize) -> Idx {
    for _ in 0..n {
        i = colon(s, h16(s, i)?)?;
    }
    Some(i)
}

fn dec_octet(s: &str, i: usize) -> Idx {
    let b = &s.as_bytes()[i..];
    let d = |k: usize| b.get(k).is_some_and(u8::is_ascii_digit);
    let r = |k: usize, lo: u8, hi: u8| b.get(k).is_some_and(|c| (lo..=hi).contains(c));
    let three = (b.starts_with(b"25") && r(2, b'0', b'5'))
        || (b.starts_with(b"2") && r(1, b'0', b'4') && d(2))
        || (b.starts_with(b"1") && d(1) && d(2));
    if three {
        Some(i + 3)
    } else if r(0, b'1', b'9') && d(1) {
        Some(i + 2)
    } else {
        d(0).then_some(i + 1)
    }
}

fn ipv4(s: &str, i: usize) -> Idx {
    let mut i = dec_octet(s, i)?;
    for _ in 0..3 {
        i = dec_octet(s, colon_dot(s, i)?)?;
    }
    Some(i)
}

fn colon_dot(s: &str, i: usize) -> Idx {
    s[i..].starts_with('.').then_some(i + 1)
}

fn ls32(s: &str, i: usize) -> Idx {
    ipv4(s, i).or_else(|| h16(s, i).and_then(|j| colon(s, j)).and_then(|j| h16(s, j)))
}

/// `(((H16 ":" !":"){0,n} H16)?`
fn opt_h16s(s: &str, i: usize, n: usize) -> usize {
    let mut j = i;
    for _ in 0..n {
        match h16(s, j).and_then(|k| colon(s, k)) {
            Some(k) if !s[k..].starts_with(':') => j = k,
            _ => break,
        }
    }
    h16(s, j).unwrap_or(i)
}

fn double_colon(s: &str, i: usize) -> Idx {
    s[i..].starts_with("::").then_some(i + 2)
}

fn ipv6(s: &str, i: usize) -> Idx {
    // 1. (H16 ":"){6} Ls32
    h16_colons(s, i, 6)
        .and_then(|j| ls32(s, j))
        // 2. "::" (H16 ":"){5} Ls32
        .or_else(|| {
            double_colon(s, i)
                .and_then(|j| h16_colons(s, j, 5))
                .and_then(|j| ls32(s, j))
        })
        // 3-5. optional prefix, "::", (H16 ":"){4,3,2}, Ls32
        .or_else(|| {
            (0..3).find_map(|k| {
                let j = double_colon(s, opt_h16s(s, i, k))?;
                ls32(s, h16_colons(s, j, 4 - k)?)
            })
        })
        // 6. optional prefix, "::", H16 ":" Ls32
        .or_else(|| {
            let j = double_colon(s, opt_h16s(s, i, 3))?;
            ls32(s, colon(s, h16(s, j)?)?)
        })
        // 7. optional prefix, "::", Ls32
        .or_else(|| ls32(s, double_colon(s, opt_h16s(s, i, 4))?))
        // 8. optional prefix, "::", H16
        .or_else(|| h16(s, double_colon(s, opt_h16s(s, i, 5))?))
        // 9. optional prefix, "::"
        .or_else(|| double_colon(s, opt_h16s(s, i, 6)))
}

fn ipv_future(s: &str, i: usize) -> Idx {
    let mut j = i;
    if !s[j..].starts_with('v') {
        return None;
    }
    j += 1;
    let n = s.as_bytes()[j..]
        .iter()
        .take_while(|b| b.is_ascii_hexdigit())
        .count();
    if n == 0 {
        return None;
    }
    j += n;
    if !s[j..].starts_with('.') {
        return None;
    }
    j += 1;
    let m = run(s, j, |c| c == ':', true, false);
    (m > 0).then_some(j + m)
}

/// `"[" (IPv6address | IPvFuture) "]"`
fn ip_literal(s: &str, i: usize) -> Idx {
    if !s[i..].starts_with('[') {
        return None;
    }
    let j = ipv6(s, i + 1).or_else(|| ipv_future(s, i + 1))?;
    s[j..].starts_with(']').then_some(j + 1)
}

/// `Authority`, which always matches.
fn authority(s: &str, mut i: usize) -> usize {
    let n = run(s, i, |c| c == ':', true, true);
    if s[i + n..].starts_with('@') {
        i += n + 1;
    }
    i = ip_literal(s, i)
        .or_else(|| ipv4(s, i))
        .unwrap_or_else(|| i + run(s, i, no, true, true));
    if s[i..].starts_with(':') {
        i += 1;
        i += s.as_bytes()[i..]
            .iter()
            .take_while(|b| b.is_ascii_digit())
            .count();
    }
    i
}

/// Length of the `Iri` that starts `s`, if one does. `Iri` ends wherever its
/// repetitions stop, so this is a prefix match.
pub fn iri_len(s: &str) -> Option<usize> {
    let b = s.as_bytes();
    if !b.first().is_some_and(u8::is_ascii_alphabetic) {
        return None;
    }
    let mut i = 1;
    while i < b.len() && (b[i].is_ascii_alphanumeric() || matches!(b[i], b'+' | b'-' | b'.')) {
        i += 1;
    }
    if !s[i..].starts_with(':') {
        return None;
    }
    i += 1;

    // IriHierPart
    if s[i..].starts_with("//") {
        i = authority(s, i + 2);
        i = slash_segments(s, i);
    } else if s[i..].starts_with('/') {
        i += 1;
        let n = ip_run(s, i);
        if n > 0 {
            i = slash_segments(s, i + n);
        }
    } else {
        let n = ip_run(s, i);
        if n > 0 {
            i = slash_segments(s, i + n);
        }
    }
    Some(query_fragment(s, i))
}

/// `("?" Query)? ("#" Fragment)?` from `i`; returns the end.
fn query_fragment(s: &str, mut i: usize) -> usize {
    if s[i..].starts_with('?') {
        i += 1;
        i += run(
            s,
            i,
            |c| ip_char_extra(c) || c == '/' || c == '?' || is_private(c),
            true,
            true,
        );
    }
    if s[i..].starts_with('#') {
        i += 1;
        i += run(
            s,
            i,
            |c| ip_char_extra(c) || c == '/' || c == '?',
            true,
            true,
        );
    }
    i
}

/// Length of the OBO `UrlId` that starts `s`: a scheme, `://`, an authority
/// and at least one `/segment`, then the optional query and fragment.
pub fn url_id_len(s: &str) -> Option<usize> {
    let b = s.as_bytes();
    if !b.first().is_some_and(u8::is_ascii_alphabetic) {
        return None;
    }
    let mut i = 1;
    while i < b.len() && (b[i].is_ascii_alphanumeric() || matches!(b[i], b'+' | b'-' | b'.')) {
        i += 1;
    }
    if !s[i..].starts_with("://") {
        return None;
    }
    i = authority(s, i + 3);
    let after = slash_segments(s, i);
    if after == i {
        return None;
    }
    Some(query_fragment(s, after))
}

/// Does the text between `<` and `>` match `Iri` in full?
pub fn is_iri(s: &str) -> bool {
    iri_len(s) == Some(s.len())
}
