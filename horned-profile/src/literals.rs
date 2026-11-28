//! Lexical-space checks for typed literals.
//!
//! OWL 2 Structural Specification §5.7: the lexical form of each literal in
//! an OWL 2 DL ontology must belong to the lexical space of its datatype.
//! Only datatypes whose XSD lexical space is simple to check without extra
//! dependencies are validated: `xsd:boolean`, `xsd:decimal`, the
//! `xsd:integer` family and `xsd:dateTime`/`xsd:dateTimeStamp`. Every other
//! datatype is assumed well-formed.

const XSD: &str = "http://www.w3.org/2001/XMLSchema#";

/// Returns `false` only if `lexical` is certainly not in the lexical space
/// of the datatype `datatype_iri`.
pub(crate) fn is_well_formed(lexical: &str, datatype_iri: &str) -> bool {
    let Some(local) = datatype_iri.strip_prefix(XSD) else {
        return true;
    };
    // These datatypes all have whiteSpace=collapse, which is applied
    // before the lexical mapping.
    let v = lexical.trim_matches([' ', '\t', '\n', '\r']);
    match local {
        "boolean" => matches!(v, "true" | "false" | "1" | "0"),
        "decimal" => is_decimal(v),
        "integer" => integer(v).is_some(),
        "nonNegativeInteger" => integer(v).is_some_and(|(neg, zero)| !neg || zero),
        "positiveInteger" => integer(v).is_some_and(|(neg, zero)| !neg && !zero),
        "nonPositiveInteger" => integer(v).is_some_and(|(neg, zero)| neg || zero),
        "negativeInteger" => integer(v).is_some_and(|(neg, zero)| neg && !zero),
        "long" => in_range(v, i64::MIN as i128, i64::MAX as i128),
        "int" => in_range(v, i32::MIN as i128, i32::MAX as i128),
        "short" => in_range(v, i16::MIN as i128, i16::MAX as i128),
        "byte" => in_range(v, i8::MIN as i128, i8::MAX as i128),
        "unsignedLong" => in_range(v, 0, u64::MAX as i128),
        "unsignedInt" => in_range(v, 0, u32::MAX as i128),
        "unsignedShort" => in_range(v, 0, u16::MAX as i128),
        "unsignedByte" => in_range(v, 0, u8::MAX as i128),
        "dateTime" => is_date_time(v, false),
        "dateTimeStamp" => is_date_time(v, true),
        _ => true,
    }
}

/// Parses `[+-]?[0-9]+`, returning (negative, is_zero).
fn integer(v: &str) -> Option<(bool, bool)> {
    let (neg, digits) = match v.as_bytes().first()? {
        b'-' => (true, &v[1..]),
        b'+' => (false, &v[1..]),
        _ => (false, v),
    };
    if digits.is_empty() || !digits.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    Some((neg, digits.bytes().all(|b| b == b'0')))
}

fn in_range(v: &str, lo: i128, hi: i128) -> bool {
    if integer(v).is_none() {
        return false;
    }
    let digits = v.trim_start_matches(['+', '-']).trim_start_matches('0');
    // Anything longer than i128 can hold is out of every range here.
    if digits.len() > 30 {
        return false;
    }
    let n: i128 = if digits.is_empty() {
        0
    } else {
        digits.parse().unwrap()
    };
    let n = if v.starts_with('-') { -n } else { n };
    (lo..=hi).contains(&n)
}

/// `[+-]? ([0-9]+ ('.' [0-9]*)? | '.' [0-9]+)`
fn is_decimal(v: &str) -> bool {
    let body = v.strip_prefix(['+', '-']).unwrap_or(v);
    let (int, frac) = match body.split_once('.') {
        Some((i, f)) => (i, Some(f)),
        None => (body, None),
    };
    let digits = |s: &str| s.bytes().all(|b| b.is_ascii_digit());
    digits(int) && frac.is_none_or(digits) && !(int.is_empty() && frac.is_none_or(str::is_empty))
}

fn is_date_time(v: &str, tz_required: bool) -> bool {
    let b = v.as_bytes();
    let mut i = 0;
    if b.first() == Some(&b'-') {
        i = 1;
    }
    let ystart = i;
    while i < b.len() && b[i].is_ascii_digit() {
        i += 1;
    }
    let year = &v[ystart..i];
    if year.len() < 4 || (year.len() > 4 && year.starts_with('0')) {
        return false;
    }
    let rest = &v[i..];
    let Some(rest) = rest.strip_prefix('-') else {
        return false;
    };
    let fixed = |s: &str, n: usize| -> Option<u32> {
        (s.len() >= n && s.as_bytes()[..n].iter().all(u8::is_ascii_digit))
            .then(|| s[..n].parse().unwrap())
    };
    let Some(month) = fixed(rest, 2) else {
        return false;
    };
    let Some(rest) = rest[2..].strip_prefix('-') else {
        return false;
    };
    let Some(day) = fixed(rest, 2) else {
        return false;
    };
    let Some(rest) = rest[2..].strip_prefix('T') else {
        return false;
    };
    // Leap years depend on the year modulo 400, which the last four digits
    // determine.
    let ymod: u32 = year[year.len() - 4..].parse().unwrap();
    let leap = ymod % 4 == 0 && (ymod % 100 != 0 || (ymod / 100) % 4 == 0);
    let max_day = match month {
        1 | 3 | 5 | 7 | 8 | 10 | 12 => 31,
        4 | 6 | 9 | 11 => 30,
        2 if leap => 29,
        2 => 28,
        _ => return false,
    };
    if day < 1 || day > max_day {
        return false;
    }
    let (Some(hour), Some(rest)) = (fixed(rest, 2), rest.get(2..)) else {
        return false;
    };
    let Some(rest) = rest.strip_prefix(':') else {
        return false;
    };
    let Some(minute) = fixed(rest, 2) else {
        return false;
    };
    let Some(rest) = rest[2..].strip_prefix(':') else {
        return false;
    };
    let Some(second) = fixed(rest, 2) else {
        return false;
    };
    let mut rest = &rest[2..];
    let mut frac_nonzero = false;
    if let Some(f) = rest.strip_prefix('.') {
        let n = f.bytes().take_while(u8::is_ascii_digit).count();
        if n == 0 {
            return false;
        }
        frac_nonzero = f[..n].bytes().any(|b| b != b'0');
        rest = &f[n..];
    }
    if minute > 59 || second > 59 {
        return false;
    }
    // 24:00:00 is the only hour 24 permitted.
    if hour > 24 || (hour == 24 && (minute != 0 || second != 0 || frac_nonzero)) {
        return false;
    }
    match rest {
        "" => !tz_required,
        "Z" => true,
        tz => {
            let Some(t) = tz.strip_prefix(['+', '-']) else {
                return false;
            };
            let (Some(h), Some(m)) = (fixed(t, 2), t.get(3..).and_then(|m| fixed(m, 2))) else {
                return false;
            };
            t.len() == 5 && t.as_bytes()[2] == b':' && m <= 59 && (h < 14 || (h == 14 && m == 0))
        }
    }
}

#[cfg(test)]
mod test {
    use super::is_well_formed as ok;

    const X: &str = "http://www.w3.org/2001/XMLSchema#";

    fn t(local: &str, v: &str) -> bool {
        ok(v, &format!("{X}{local}"))
    }

    #[test]
    fn date_time() {
        assert!(!t("dateTime", "26 dec 2014"));
        assert!(!t("dateTime", ""));
        assert!(!t("dateTime", "2014-12-26"));
        assert!(!t("dateTime", "2014-13-26T00:00:00"));
        assert!(!t("dateTime", "2014-02-29T00:00:00"));
        assert!(!t("dateTime", "2014-12-26T25:00:00"));
        assert!(!t("dateTime", "2014-12-26T24:00:01"));
        assert!(!t("dateTime", "2014-12-26T10:00:00+15:00"));
        assert!(!t("dateTimeStamp", "2014-12-26T10:00:00"));
        assert!(t("dateTime", "2014-12-26T10:00:00"));
        assert!(t("dateTime", "2014-12-26T10:00:00.25Z"));
        assert!(t("dateTime", "2012-02-29T10:00:00-05:00"));
        assert!(t("dateTime", "2014-12-26T24:00:00"));
        assert!(t("dateTime", "-0044-03-15T12:00:00+14:00"));
        assert!(t("dateTime", "12014-12-26T10:00:00"));
        assert!(t("dateTime", " 2014-12-26T10:00:00 "));
        assert!(t("dateTimeStamp", "2014-12-26T10:00:00Z"));
    }

    #[test]
    fn numbers_and_booleans() {
        assert!(t("integer", "-007"));
        assert!(t("integer", "+5"));
        assert!(!t("integer", "5.0"));
        assert!(!t("integer", ""));
        assert!(!t("integer", "abc"));
        assert!(t("nonNegativeInteger", "0"));
        assert!(t("nonNegativeInteger", "-0"));
        assert!(!t("nonNegativeInteger", "-1"));
        assert!(!t("positiveInteger", "0"));
        assert!(t("byte", "127"));
        assert!(!t("byte", "128"));
        assert!(t("unsignedLong", "18446744073709551615"));
        assert!(!t("unsignedLong", "18446744073709551616"));
        assert!(t("decimal", "-.5"));
        assert!(t("decimal", "5."));
        assert!(!t("decimal", "."));
        assert!(!t("decimal", "1e3"));
        assert!(t("boolean", "1"));
        assert!(!t("boolean", "True"));
    }

    #[test]
    fn unvalidated_datatypes_are_accepted() {
        assert!(t("date", "whenever"));
        assert!(ok("anything", "http://example.org/custom"));
    }
}
