//! Language tag recognition, a literal port of `grammars/bcp47.pest`.
//!
//! The pest grammar is a PEG, so ordered choice commits and repetition is
//! greedy without backtracking. This follows it step for step, so it
//! consumes exactly the same prefix, including where that departs from the
//! RFC: the guards on ExtLang, Script and Region exist only because of that
//! greediness.

type Idx = Option<usize>;

fn at(s: &str, i: usize) -> Option<u8> {
    s.as_bytes().get(i).copied()
}

fn lit(s: &str, i: usize, l: &str) -> Idx {
    s[i..].starts_with(l).then(|| i + l.len())
}

/// Between `min` and `max` bytes satisfying `p`, as many as possible.
fn count(s: &str, mut i: usize, min: usize, max: usize, p: fn(&u8) -> bool) -> Idx {
    let mut n = 0;
    while n < max && at(s, i).is_some_and(|b| p(&b)) {
        i += 1;
        n += 1;
    }
    (n >= min).then_some(i)
}

fn alpha(b: &u8) -> bool {
    b.is_ascii_alphabetic()
}

fn alnum(b: &u8) -> bool {
    b.is_ascii_alphanumeric()
}

fn digit(b: &u8) -> bool {
    b.is_ascii_digit()
}

/// `!p` at `i`.
fn not(s: &str, i: usize, p: fn(&u8) -> bool) -> Idx {
    (!at(s, i).is_some_and(|b| p(&b))).then_some(i)
}

/// `("-" ~ rule)?`
fn opt_dash(s: &str, i: usize, rule: fn(&str, usize) -> Idx) -> usize {
    lit(s, i, "-").and_then(|j| rule(s, j)).unwrap_or(i)
}

/// `("-" ~ rule)*`
fn many_dash(s: &str, mut i: usize, rule: fn(&str, usize) -> Idx) -> usize {
    while let Some(j) = lit(s, i, "-").and_then(|j| rule(s, j)) {
        i = j;
    }
    i
}

fn ext_lang_subtag(s: &str, i: usize) -> Idx {
    count(s, i, 3, 3, alpha).and_then(|j| not(s, j, alpha))
}

fn ext_lang(s: &str, i: usize) -> Idx {
    let mut i = ext_lang_subtag(s, i)?;
    for _ in 0..2 {
        match lit(s, i, "-").and_then(|j| ext_lang_subtag(s, j)) {
            Some(j) => i = j,
            None => break,
        }
    }
    Some(i)
}

fn language(s: &str, i: usize) -> Idx {
    if let Some(j) = count(s, i, 2, 3, alpha) {
        return Some(opt_dash(s, j, ext_lang));
    }
    count(s, i, 4, 4, alpha).or_else(|| count(s, i, 5, 5, alpha))
}

fn script(s: &str, i: usize) -> Idx {
    count(s, i, 4, 4, alpha).and_then(|j| not(s, j, alnum))
}

fn region(s: &str, i: usize) -> Idx {
    count(s, i, 2, 2, alpha)
        .or_else(|| count(s, i, 3, 3, digit))
        .and_then(|j| not(s, j, alnum))
}

fn variant(s: &str, i: usize) -> Idx {
    count(s, i, 5, 8, alnum)
        .or_else(|| count(s, i, 1, 1, digit).and_then(|j| count(s, j, 3, 3, alnum)))
}

fn singleton(s: &str, i: usize) -> Idx {
    match at(s, i)? {
        b'0'..=b'9' | b'A'..=b'W' | b'Y'..=b'Z' | b'a'..=b'w' | b'y'..=b'z' => Some(i + 1),
        _ => None,
    }
}

fn extension(s: &str, i: usize) -> Idx {
    let mut i = singleton(s, i)?;
    let mut any = false;
    while let Some(j) = lit(s, i, "-").and_then(|j| count(s, j, 2, 8, alnum)) {
        i = j;
        any = true;
    }
    any.then_some(i)
}

fn private_use(s: &str, i: usize) -> Idx {
    let mut i = lit(s, i, "x")?;
    let mut any = false;
    while let Some(j) = lit(s, i, "-").and_then(|j| count(s, j, 1, 8, alnum)) {
        i = j;
        any = true;
    }
    any.then_some(i)
}

fn lang_tag(s: &str, i: usize) -> Idx {
    let i = language(s, i)?;
    let i = opt_dash(s, i, script);
    let i = opt_dash(s, i, region);
    let i = many_dash(s, i, variant);
    let i = many_dash(s, i, extension);
    Some(opt_dash(s, i, private_use))
}

fn grand_fathered(s: &str, i: usize) -> Idx {
    const TAGS: &[&str] = &[
        "en-GB-oed",
        "i-ami",
        "i-bnn",
        "i-default",
        "i-enochian",
        "i-hak",
        "i-klingon",
        "i-lux",
        "i-mingo",
        "i-navajo",
        "i-pwn",
        "i-tao",
        "i-tay",
        "i-tsu",
        "sgn-BE-FR",
        "sgn-BE-NL",
        "sgn-CH-DE",
        "art-lojban",
        "cel-gaulish",
        "no-bok",
        "no-nyn",
        "zh-guoyu",
        "zh-hakka",
        "zh-min",
        "zh-min-nan",
        "zh-xiang",
    ];
    TAGS.iter().find_map(|t| lit(s, i, t))
}

/// The length of the language tag at the start of `s`, as `BCP47_LanguageTag`
/// would match it.
pub fn language_tag_len(s: &str) -> Option<usize> {
    lang_tag(s, 0)
        .or_else(|| private_use(s, 0))
        .or_else(|| grand_fathered(s, 0))
}
