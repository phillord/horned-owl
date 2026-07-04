use std::fmt::{Display, Error, Formatter};
use std::marker::PhantomData;

use curie::PrefixMapping;

use crate::model::*;

/// OWL elements renderable in Manchester syntax.
pub trait AsManchester<A: ForIRI> {
    fn as_manchester(&self) -> Manchester<'_, Self, A> {
        Manchester(self, None, PhantomData)
    }
    fn as_manchester_with_prefixes<'t>(
        &'t self,
        prefix: &'t PrefixMapping,
    ) -> Manchester<'t, Self, A> {
        Manchester(self, Some(prefix), PhantomData)
    }
}

/// Lazy `Display` wrapper for a Manchester-rendered element.
#[derive(Debug)]
pub struct Manchester<'t, T: ?Sized, A: ForIRI>(&'t T, Option<&'t PrefixMapping>, PhantomData<A>);

impl<'t, T, A> Display for Manchester<'t, &'t T, A>
where
    Manchester<'t, T, A>: Display,
    A: ForIRI,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        Manchester(*self.0, self.1, PhantomData).fmt(f)
    }
}

/// Return true iff `local` is a valid Manchester PnLocal-ish name:
/// non-empty, first char is a letter or `_` (a PN_LOCAL start char — NOT a
/// digit, `-`, or `.`), every char is alphanumeric or one of `_`, `-`, `.`,
/// and it does not end with `.`.
///
/// This mirrors the guard used in `write_iri` / `render_iri_to_string` and
/// must be kept in sync with both sites. Rejecting a leading `-`/`.` matters:
/// e.g. a version IRI ending `…/o-viri` shrinks to a bare `-viri`, which the
/// reader cannot re-parse — emit the full `<iri>` form instead.
#[inline]
fn is_valid_manchester_local(local: &str) -> bool {
    local
        .chars()
        .next()
        .is_some_and(|c| c.is_ascii_alphabetic() || c == '_')
        && local
            .chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '_' | '-' | '.'))
        && !local.ends_with('.')
}

/// Render an IRI to a `String`: abbreviated `prefix:local` (or bare `local`
/// for the default prefix) if a prefix matches AND the local name is a valid
/// Manchester PnLocal-ish name; otherwise `<full-iri>`.
///
/// This is the single canonical abbreviation logic shared by all IRI rendering
/// sites: the `Display` path (`write_iri`), the `String`-building path
/// (`write_iri_to_string`), and the frame-subject renderer in `mod.rs`.
pub(crate) fn render_iri_to_string(iri: &str, pm: Option<&PrefixMapping>) -> String {
    if let Some(pm) = pm
        && let Ok(curie) = pm.shrink_iri(iri)
    {
        let s = curie.to_string();
        // The local name is everything after the first ':' (curie prefixes never
        // contain a ':').
        let local = s.split_once(':').map_or(s.as_str(), |(_, l)| l);
        // Only abbreviate when the local name is a valid Manchester local name.
        // A version IRI like `http://ex/o/1.0.0` shrinks to `ex:o/1.0.0`, whose
        // `/` is NOT valid — emitting that abbreviation produces output the reader
        // cannot re-parse.  A namespace without a name separator (e.g.
        // `http://e/onto`) shrinks `http://e/onto#A` to `#A`, which is also
        // invalid — emit the full `<iri>` form instead.
        if is_valid_manchester_local(local) {
            // `add_prefix("", ns)` stores "" in the prefix *mapping* (not the
            // default slot), so `shrink_iri` returns a `Curie { prefix: Some(""), .. }`
            // which `Display` formats as ":local". Strip the leading ':' to get the
            // bare local name for the default prefix.
            return if let Some(stripped) = s.strip_prefix(':') {
                stripped.to_owned()
            } else {
                s
            };
        }
        // else: invalid local (digit-leading, empty, contains '#'/'/'/…, ends with '.')
        // — fall through to the full IRI form.
    }
    format!("<{iri}>")
}

/// Render an IRI: abbreviated `prefix:local` if a prefix matches, else `<iri>`.
/// Delegates to `render_iri_to_string` for the canonical validity check.
fn write_iri(
    iri: &str,
    prefix: Option<&PrefixMapping>,
    f: &mut Formatter<'_>,
) -> Result<(), Error> {
    f.write_str(&render_iri_to_string(iri, prefix))
}

impl<A: ForIRI> Display for Manchester<'_, IRI<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write_iri(self.0.as_ref(), self.1, f)
    }
}
impl<A: ForIRI> AsManchester<A> for IRI<A> {}

impl<A: ForIRI> Display for Manchester<'_, Class<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for Class<A> {}

// ---------------------------------------------------------------------------

/// Write a string literal while escaping `"` and `\` characters per §2.5:
/// `quotedString ::= '"' (\" | \\ | not(" or \))* '"'`
///
/// Uses `char_indices()` (byte offsets) — NOT `chars().enumerate()`
/// (character indices) — so that multibyte UTF-8 sequences are sliced
/// correctly.
fn quote(mut s: &str, f: &mut Formatter<'_>) -> Result<(), Error> {
    f.write_str("\"")?;
    while let Some((byte_i, c)) = s.char_indices().find(|(_, c)| *c == '\\' || *c == '"') {
        f.write_str(&s[..byte_i])?;
        match c {
            '\\' => f.write_str("\\\\")?,
            '"' => f.write_str("\\\"")?,
            _ => unreachable!(),
        }
        s = &s[byte_i + c.len_utf8()..];
    }
    f.write_str(s)?;
    f.write_str("\"")
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, ObjectProperty<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for ObjectProperty<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, ObjectPropertyExpression<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use ObjectPropertyExpression::*;
        match self.0 {
            ObjectProperty(op) => Manchester(op, self.1, PhantomData::<A>).fmt(f),
            InverseObjectProperty(op) => {
                write!(f, "inverse ({})", Manchester(op, self.1, PhantomData::<A>))
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for ObjectPropertyExpression<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, DataProperty<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for DataProperty<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, NamedIndividual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for NamedIndividual<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, AnonymousIndividual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "_:{}", self.0.0.borrow())
    }
}
impl<A: ForIRI> AsManchester<A> for AnonymousIndividual<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, Individual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Individual::*;
        match self.0 {
            Named(i) => Manchester(i, self.1, PhantomData::<A>).fmt(f),
            Anonymous(i) => Manchester(i, self.1, PhantomData::<A>).fmt(f),
        }
    }
}
impl<A: ForIRI> AsManchester<A> for Individual<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, Datatype<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for Datatype<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, Literal<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self.0 {
            Literal::Simple { literal } => quote(literal, f),
            Literal::Language { literal, lang } => {
                quote(literal, f)?;
                write!(f, "@{lang}")
            }
            Literal::Datatype {
                literal,
                datatype_iri,
            } => {
                quote(literal, f)?;
                write!(
                    f,
                    "^^{}",
                    Manchester(datatype_iri, self.1, PhantomData::<A>)
                )
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for Literal<A> {}

// ---------------------------------------------------------------------------
// DataRange — facets + operator precedence

/// Map each `Facet` variant to its W3C OWL 2 Manchester Syntax symbol.
fn facet_symbol(f: &crate::vocab::Facet) -> &'static str {
    use crate::vocab::Facet::*;
    match f {
        MinInclusive => ">=",
        MaxInclusive => "<=",
        MinExclusive => ">",
        MaxExclusive => "<",
        Length => "length",
        MinLength => "minLength",
        MaxLength => "maxLength",
        Pattern => "pattern",
        LangRange => "langRange",
        TotalDigits => "totalDigits",
        FractionDigits => "fractionDigits",
    }
}

/// Precedence for `DataRange` operators.
/// Tightest → loosest: atoms/restrictions (3) > `and` (2) > `or` (1).
fn dr_prec<A: ForIRI>(dr: &DataRange<A>) -> u8 {
    match dr {
        DataRange::DataUnionOf(_) => 1,
        DataRange::DataIntersectionOf(_) => 2,
        _ => 3,
    }
}

/// Render `inner` as an operand requiring at least `need` precedence.
/// Parenthesizes when `inner` binds looser than `need`.
fn dr_operand<A: ForIRI>(
    inner: &DataRange<A>,
    need: u8,
    pm: Option<&PrefixMapping>,
    f: &mut Formatter<'_>,
) -> Result<(), Error> {
    if dr_prec(inner) < need {
        write!(f, "({})", Manchester(inner, pm, PhantomData::<A>))
    } else {
        write!(f, "{}", Manchester(inner, pm, PhantomData::<A>))
    }
}

impl<A: ForIRI> Display for Manchester<'_, DataRange<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use DataRange::*;
        let pm = self.1;
        match self.0 {
            Datatype(dt) => Manchester(dt, pm, PhantomData::<A>).fmt(f),
            DataIntersectionOf(drs) => {
                let mut first = true;
                for dr in drs {
                    if !first {
                        write!(f, " and ")?;
                    }
                    first = false;
                    dr_operand(dr, 2, pm, f)?;
                }
                Ok(())
            }
            DataUnionOf(drs) => {
                let mut first = true;
                for dr in drs {
                    if !first {
                        write!(f, " or ")?;
                    }
                    first = false;
                    dr_operand(dr, 1, pm, f)?;
                }
                Ok(())
            }
            DataComplementOf(dr) => {
                write!(f, "not ")?;
                dr_operand(dr.as_ref(), 3, pm, f)
            }
            DataOneOf(lits) => {
                write!(f, "{{ ")?;
                let mut first = true;
                for l in lits {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    Manchester(l, pm, PhantomData::<A>).fmt(f)?;
                }
                write!(f, " }}")
            }
            DatatypeRestriction(dt, frs) => {
                Manchester(dt, pm, PhantomData::<A>).fmt(f)?;
                write!(f, "[")?;
                let mut first = true;
                for fr in frs {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{} ", facet_symbol(&fr.f))?;
                    Manchester(&fr.l, pm, PhantomData::<A>).fmt(f)?;
                }
                write!(f, "]")
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for DataRange<A> {}

// ---------------------------------------------------------------------------
// ClassExpression — Manchester operand parenthesization
//
// We parenthesize EVERY operand that is not a bare atomic class — matching
// OWL-API's own Manchester renderer (`and (not (r some C))`, `and ({…})`,
// `r some (D and E)`). OWL-API's Manchester PARSER desyncs on real ontologies
// when compound operands are left unparenthesized (e.g. an `ObjectOneOf {…}` or
// `not (r some C)` as an `and` operand), even though each construct parses in
// isolation — so we conservatively bracket non-atoms. Parentheses are
// structurally transparent on read, so this preserves the round-trip.

/// An operand needs no parentheses only when it is a bare named class.
fn ce_is_atom<A: ForIRI>(ce: &ClassExpression<A>) -> bool {
    matches!(ce, ClassExpression::Class(_))
}

/// Render `inner` as a sub-expression operand, bracketing it unless it is a
/// bare atomic class (OWL-API-compatible parenthesization).
fn ce_operand<A: ForIRI>(
    inner: &ClassExpression<A>,
    pm: Option<&PrefixMapping>,
    f: &mut Formatter<'_>,
) -> Result<(), Error> {
    if ce_is_atom(inner) {
        write!(f, "{}", Manchester(inner, pm, PhantomData::<A>))
    } else {
        write!(f, "({})", Manchester(inner, pm, PhantomData::<A>))
    }
}

impl<A: ForIRI> Display for Manchester<'_, ClassExpression<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use ClassExpression::*;
        let pm = self.1;
        match self.0 {
            Class(c) => Manchester(c, pm, PhantomData::<A>).fmt(f),

            ObjectIntersectionOf(operands) => {
                let mut first = true;
                for ce in operands {
                    if !first {
                        write!(f, " and ")?;
                    }
                    first = false;
                    ce_operand(ce, pm, f)?;
                }
                Ok(())
            }

            ObjectUnionOf(operands) => {
                let mut first = true;
                for ce in operands {
                    if !first {
                        write!(f, " or ")?;
                    }
                    first = false;
                    ce_operand(ce, pm, f)?;
                }
                Ok(())
            }

            ObjectComplementOf(bce) => {
                write!(f, "not ")?;
                ce_operand(bce.as_ref(), pm, f)
            }

            ObjectOneOf(individuals) => {
                write!(f, "{{")?;
                let mut first = true;
                for i in individuals {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    Manchester(i, pm, PhantomData::<A>).fmt(f)?;
                }
                write!(f, "}}")
            }

            ObjectSomeValuesFrom { ope, bce } => {
                write!(f, "{} some ", Manchester(ope, pm, PhantomData::<A>))?;
                ce_operand(bce.as_ref(), pm, f)
            }

            ObjectAllValuesFrom { ope, bce } => {
                write!(f, "{} only ", Manchester(ope, pm, PhantomData::<A>))?;
                ce_operand(bce.as_ref(), pm, f)
            }

            ObjectHasValue { ope, i } => {
                write!(
                    f,
                    "{} value {}",
                    Manchester(ope, pm, PhantomData::<A>),
                    Manchester(i, pm, PhantomData::<A>)
                )
            }

            ObjectHasSelf(ope) => {
                write!(f, "{} Self", Manchester(ope, pm, PhantomData::<A>))
            }

            ObjectMinCardinality { n, ope, bce } => {
                write!(f, "{} min {} ", Manchester(ope, pm, PhantomData::<A>), n)?;
                ce_operand(bce.as_ref(), pm, f)
            }

            ObjectMaxCardinality { n, ope, bce } => {
                write!(f, "{} max {} ", Manchester(ope, pm, PhantomData::<A>), n)?;
                ce_operand(bce.as_ref(), pm, f)
            }

            ObjectExactCardinality { n, ope, bce } => {
                write!(
                    f,
                    "{} exactly {} ",
                    Manchester(ope, pm, PhantomData::<A>),
                    n
                )?;
                ce_operand(bce.as_ref(), pm, f)
            }

            DataSomeValuesFrom { dp, dr } => {
                write!(
                    f,
                    "{} some {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }

            DataAllValuesFrom { dp, dr } => {
                write!(
                    f,
                    "{} only {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }

            DataHasValue { dp, l } => {
                write!(
                    f,
                    "{} value {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    Manchester(l, pm, PhantomData::<A>)
                )
            }

            DataMinCardinality { n, dp, dr } => {
                write!(
                    f,
                    "{} min {} {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    n,
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }

            DataMaxCardinality { n, dp, dr } => {
                write!(
                    f,
                    "{} max {} {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    n,
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }

            DataExactCardinality { n, dp, dr } => {
                write!(
                    f,
                    "{} exactly {} {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    n,
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for ClassExpression<A> {}

// ---------------------------------------------------------------------------
// Component — per-axiom Manchester rendering
//
// The ~20 common logical axioms get bespoke Manchester clauses (SWRL rules are
// emitted natively by the `write()` driver as `Rule:` lines); the rest
// (structural/meta/annotation) fall back to OWL FUNCTIONAL syntax via
// `AsFunctional`.  That fallback is NOT valid Manchester — it is a readable,
// lossless stopgap for variants with no implemented Manchester form (Import,
// HasKey, OntologyAnnotation, annotation axioms, …).  Native Manchester for the
// common ones (Import:, header Annotations:) is a pre-upstream-PR follow-up.

// ---------------------------------------------------------------------------
// SWRL atoms and arguments (for native `Rule:` output).
// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, Variable<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "?{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for Variable<A> {}

impl<A: ForIRI> Display for Manchester<'_, IArgument<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self.0 {
            IArgument::Variable(v) => write!(f, "{}", Manchester(v, self.1, PhantomData::<A>)),
            IArgument::Individual(i) => write!(f, "{}", Manchester(i, self.1, PhantomData::<A>)),
        }
    }
}
impl<A: ForIRI> AsManchester<A> for IArgument<A> {}

impl<A: ForIRI> Display for Manchester<'_, DArgument<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self.0 {
            DArgument::Variable(v) => write!(f, "{}", Manchester(v, self.1, PhantomData::<A>)),
            DArgument::Literal(l) => write!(f, "{}", Manchester(l, self.1, PhantomData::<A>)),
        }
    }
}
impl<A: ForIRI> AsManchester<A> for DArgument<A> {}

impl<A: ForIRI> Display for Manchester<'_, Atom<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let pm = self.1;
        macro_rules! m {
            ($e:expr) => {
                Manchester($e, pm, PhantomData::<A>)
            };
        }
        match self.0 {
            // A compound class expression must be parenthesised so the trailing
            // `(arg)` is not mis-bound — e.g. `(o:A and o:B)(?x)`.
            Atom::ClassAtom { pred, arg } => {
                if matches!(pred, ClassExpression::Class(_)) {
                    write!(f, "{}({})", m!(pred), m!(arg))
                } else {
                    write!(f, "({})({})", m!(pred), m!(arg))
                }
            }
            Atom::DataRangeAtom { pred, arg } => write!(f, "{}({})", m!(pred), m!(arg)),
            Atom::ObjectPropertyAtom { pred, args } => {
                write!(f, "{}({}, {})", m!(pred), m!(&args.0), m!(&args.1))
            }
            Atom::DataPropertyAtom { pred, args } => {
                write!(f, "{}({}, {})", m!(pred), m!(&args.0), m!(&args.1))
            }
            Atom::BuiltInAtom { pred, args } => {
                // OWL API's Manchester `Rule:` grammar accepts a prefixed name
                // only for a *known* swrlb built-in; an arbitrary built-in IRI is
                // rejected as a CURIE (e.g. `o:y(...)`) and must be written in
                // full `<IRI>` form. Render the predicate with no prefix mapping
                // so it is always the full IRI, which the reference parser also
                // accepts for the standard swrlb built-ins.
                write!(f, "{}(", Manchester(pred, None, PhantomData::<A>))?;
                for (i, a) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", m!(a))?;
                }
                write!(f, ")")
            }
            Atom::SameIndividualAtom(a, b) => write!(f, "SameAs({}, {})", m!(a), m!(b)),
            Atom::DifferentIndividualsAtom(a, b) => {
                write!(f, "DifferentFrom({}, {})", m!(a), m!(b))
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for Atom<A> {}

impl<A: ForIRI> Display for Manchester<'_, Component<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use crate::io::ofn::writer::AsFunctional as _;
        let pm = self.1;

        // Shorthand: wrap any `&T` that already has Manchester<T,A>: Display.
        macro_rules! m {
            ($e:expr) => {
                Manchester($e, pm, PhantomData::<A>)
            };
        }

        // Render a `Vec<T>` where Manchester<T,A>: Display, joining with `sep`.
        // Writes nothing for empty vecs.
        macro_rules! join_vec {
            ($vec:expr, $sep:expr) => {{
                let mut first = true;
                for item in ($vec).iter() {
                    if !first {
                        write!(f, $sep)?;
                    }
                    first = false;
                    write!(f, "{}", m!(item))?;
                }
            }};
        }

        match self.0 {
            // --- Class axioms ---
            Component::SubClassOf(ax) => {
                write!(f, "{} SubClassOf {}", m!(&ax.sub), m!(&ax.sup))
            }
            Component::EquivalentClasses(ax) => {
                // ax.0 is Vec<ClassExpression<A>>
                let mut it = ax.0.iter();
                if let Some(first) = it.next() {
                    write!(f, "{}", m!(first))?;
                    for item in it {
                        write!(f, " EquivalentTo {}", m!(item))?;
                    }
                }
                Ok(())
            }
            Component::DisjointClasses(ax) => {
                // Pairwise semantics: chain (`A DisjointWith B DisjointWith C`) only
                // conveys {A,B} and {B,C}, dropping {A,C}.  For 3+ members use the
                // first-member + comma-list form (`A DisjointWith B, C`) which
                // unambiguously lists all members.  Binary case is identical either way.
                let mut it = ax.0.iter();
                if let Some(first) = it.next() {
                    write!(f, "{}", m!(first))?;
                    let rest: Vec<_> = it.collect();
                    if !rest.is_empty() {
                        write!(f, " DisjointWith ")?;
                        let mut first_rest = true;
                        for item in &rest {
                            if !first_rest {
                                write!(f, ", ")?;
                            }
                            first_rest = false;
                            write!(f, "{}", m!(*item))?;
                        }
                    }
                }
                Ok(())
            }
            Component::DisjointUnion(ax) => {
                // ax.0 = Class, ax.1 = Vec<ClassExpression>
                write!(f, "{} DisjointUnionOf ", m!(&ax.0))?;
                join_vec!(&ax.1, ", ");
                Ok(())
            }

            // --- Object property axioms ---
            Component::SubObjectPropertyOf(ax) => {
                // ax.sub: SubObjectPropertyExpression — render inline (no Manchester impl for it)
                // Note for upstream PR: `p o q SubPropertyOf r` is the readable infix form used
                // here; strict Manchester frame syntax writes `r SubPropertyChain: p o q` instead.
                let sub_str = match &ax.sub {
                    SubObjectPropertyExpression::ObjectPropertyChain(chain) => chain
                        .iter()
                        .map(|p| m!(p).to_string())
                        .collect::<Vec<_>>()
                        .join(" o "),
                    SubObjectPropertyExpression::ObjectPropertyExpression(ope) => {
                        m!(ope).to_string()
                    }
                };
                write!(f, "{sub_str} SubPropertyOf {}", m!(&ax.sup))
            }
            Component::EquivalentObjectProperties(ax) => {
                let mut it = ax.0.iter();
                if let Some(first) = it.next() {
                    write!(f, "{}", m!(first))?;
                    for item in it {
                        write!(f, " EquivalentTo {}", m!(item))?;
                    }
                }
                Ok(())
            }
            Component::DisjointObjectProperties(ax) => {
                // Pairwise semantics: use first-member + comma-list for 3+ members
                // to convey all pairs (not just consecutive pairs from chaining).
                let mut it = ax.0.iter();
                if let Some(first) = it.next() {
                    write!(f, "{}", m!(first))?;
                    let rest: Vec<_> = it.collect();
                    if !rest.is_empty() {
                        write!(f, " DisjointWith ")?;
                        let mut first_rest = true;
                        for item in &rest {
                            if !first_rest {
                                write!(f, ", ")?;
                            }
                            first_rest = false;
                            write!(f, "{}", m!(*item))?;
                        }
                    }
                }
                Ok(())
            }
            Component::InverseObjectProperties(ax) => {
                // ax.0 and ax.1 are ObjectProperty (not expression)
                write!(f, "{} InverseOf {}", m!(&ax.0), m!(&ax.1))
            }
            Component::ObjectPropertyDomain(ax) => {
                write!(f, "{} Domain {}", m!(&ax.ope), m!(&ax.ce))
            }
            Component::ObjectPropertyRange(ax) => {
                write!(f, "{} Range {}", m!(&ax.ope), m!(&ax.ce))
            }
            Component::FunctionalObjectProperty(ax) => {
                write!(f, "{} Characteristics: Functional", m!(&ax.0))
            }
            Component::InverseFunctionalObjectProperty(ax) => {
                write!(f, "{} Characteristics: InverseFunctional", m!(&ax.0))
            }
            Component::ReflexiveObjectProperty(ax) => {
                write!(f, "{} Characteristics: Reflexive", m!(&ax.0))
            }
            Component::IrreflexiveObjectProperty(ax) => {
                write!(f, "{} Characteristics: Irreflexive", m!(&ax.0))
            }
            Component::SymmetricObjectProperty(ax) => {
                write!(f, "{} Characteristics: Symmetric", m!(&ax.0))
            }
            Component::AsymmetricObjectProperty(ax) => {
                write!(f, "{} Characteristics: Asymmetric", m!(&ax.0))
            }
            Component::TransitiveObjectProperty(ax) => {
                write!(f, "{} Characteristics: Transitive", m!(&ax.0))
            }

            // --- Data property axioms ---
            Component::SubDataPropertyOf(ax) => {
                write!(f, "{} SubPropertyOf {}", m!(&ax.sub), m!(&ax.sup))
            }
            Component::EquivalentDataProperties(ax) => {
                let mut it = ax.0.iter();
                if let Some(first) = it.next() {
                    write!(f, "{}", m!(first))?;
                    for item in it {
                        write!(f, " EquivalentTo {}", m!(item))?;
                    }
                }
                Ok(())
            }
            Component::DisjointDataProperties(ax) => {
                // Pairwise semantics: use first-member + comma-list for 3+ members
                // to convey all pairs (not just consecutive pairs from chaining).
                let mut it = ax.0.iter();
                if let Some(first) = it.next() {
                    write!(f, "{}", m!(first))?;
                    let rest: Vec<_> = it.collect();
                    if !rest.is_empty() {
                        write!(f, " DisjointWith ")?;
                        let mut first_rest = true;
                        for item in &rest {
                            if !first_rest {
                                write!(f, ", ")?;
                            }
                            first_rest = false;
                            write!(f, "{}", m!(*item))?;
                        }
                    }
                }
                Ok(())
            }
            Component::DataPropertyDomain(ax) => {
                write!(f, "{} Domain {}", m!(&ax.dp), m!(&ax.ce))
            }
            Component::DataPropertyRange(ax) => {
                write!(f, "{} Range {}", m!(&ax.dp), m!(&ax.dr))
            }
            Component::FunctionalDataProperty(ax) => {
                write!(f, "{} Characteristics: Functional", m!(&ax.0))
            }

            // --- Assertion axioms ---
            Component::ClassAssertion(ax) => {
                write!(f, "{} Type {}", m!(&ax.i), m!(&ax.ce))
            }
            Component::ObjectPropertyAssertion(ax) => {
                write!(f, "{} {} {}", m!(&ax.from), m!(&ax.ope), m!(&ax.to))
            }
            Component::NegativeObjectPropertyAssertion(ax) => {
                write!(f, "{} not {} {}", m!(&ax.from), m!(&ax.ope), m!(&ax.to))
            }
            Component::DataPropertyAssertion(ax) => {
                write!(f, "{} {} {}", m!(&ax.from), m!(&ax.dp), m!(&ax.to))
            }
            Component::NegativeDataPropertyAssertion(ax) => {
                write!(f, "{} not {} {}", m!(&ax.from), m!(&ax.dp), m!(&ax.to))
            }
            Component::SameIndividual(ax) => {
                let mut it = ax.0.iter();
                if let Some(first) = it.next() {
                    write!(f, "{}", m!(first))?;
                    for item in it {
                        write!(f, " SameAs {}", m!(item))?;
                    }
                }
                Ok(())
            }
            Component::DifferentIndividuals(ax) => {
                // Pairwise semantics: use first-member + comma-list for 3+ members
                // to convey all pairs (not just consecutive pairs from chaining).
                let mut it = ax.0.iter();
                if let Some(first) = it.next() {
                    write!(f, "{}", m!(first))?;
                    let rest: Vec<_> = it.collect();
                    if !rest.is_empty() {
                        write!(f, " DifferentFrom ")?;
                        let mut first_rest = true;
                        for item in &rest {
                            if !first_rest {
                                write!(f, ", ")?;
                            }
                            first_rest = false;
                            write!(f, "{}", m!(*item))?;
                        }
                    }
                }
                Ok(())
            }

            // --- Fallback: structural/meta/annotation/SWRL/declarations/HasKey/
            //               DatatypeDefinition — use functional syntax (always valid,
            //               rarely appear in justifications).
            other => write!(f, "{}", other.as_functional()),
        }
    }
}
impl<A: ForIRI> AsManchester<A> for Component<A> {}

// ---------------------------------------------------------------------------
// Annotation helpers

/// Render a single `Annotation` as `<ap-iri> <value>` for Manchester syntax.
///
/// Renders any §2.5 annotation value: `Literal`, `IRI`, or
/// `AnonymousIndividual` (`AnnotationTarget = { Literal | IRI |
/// AnonymousIndividual }`). Anonymous values render as `_:label`.
pub(crate) fn annotation_to_manchester<A: ForIRI>(
    ann: &Annotation<A>,
    pm: &PrefixMapping,
) -> String {
    let ap_str = write_iri_to_string(ann.ap.0.as_ref(), Some(pm));
    let av_str = match &ann.av {
        AnnotationValue::Literal(lit) => Manchester(lit, Some(pm), PhantomData::<A>).to_string(),
        // Render an IRI VALUE as a full `<…>` IRI, never an abbreviated CURIE.
        // OWL-API's Manchester parser expects a literal in the annotation-value
        // position when the annotation property is also used as an object/data
        // property (OBO punning, e.g. RO relations / `skos:exactMatch`), so it
        // rejects an abbreviated-CURIE value there; a full IRI is unambiguous.
        // OWL-API's own renderer does the same. (The property in `ap_str` keeps
        // its abbreviation — only the value position is affected.)
        AnnotationValue::IRI(iri) => format!("<{}>", iri.as_ref()),
        // §2.5 `AnnotationTarget = { Literal | IRI | AnonymousIndividual }`:
        // render an anonymous-individual value as `_:label` (the
        // `AnonymousIndividual` Display already produces `_:id`).
        AnnotationValue::AnonymousIndividual(ai) => {
            Manchester(ai, Some(pm), PhantomData::<A>).to_string()
        }
    };
    // §2.5 `AnnotationEntry = { Annotations? annotationProperty annotationTarget }`:
    // an annotation may itself be annotated (OWL 2 annotated annotations), rendered
    // as a leading `Annotations: <nested entries>` before this entry's `ap av`.
    if ann.ann.is_empty() {
        format!("{ap_str} {av_str}")
    } else {
        let nested = ann
            .ann
            .iter()
            .map(|a| annotation_to_manchester(a, pm))
            .collect::<Vec<_>>()
            .join(", ");
        format!("Annotations: {nested} {ap_str} {av_str}")
    }
}

/// Render an IRI string to a String using prefix abbreviation.
/// Delegates to `render_iri_to_string` for the canonical validity check.
fn write_iri_to_string(iri: &str, pm: Option<&PrefixMapping>) -> String {
    render_iri_to_string(iri, pm)
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Build;

    #[test]
    fn renders_named_class() {
        let b = Build::new_rc();
        let c = b.class("http://example.org/Dog");
        assert_eq!(c.as_manchester().to_string(), "<http://example.org/Dog>");
    }

    #[test]
    fn renders_class_with_prefix() {
        let b = Build::new_rc();
        let c = b.class("http://example.org/Dog");
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("ex", "http://example.org/").unwrap();
        assert_eq!(c.as_manchester_with_prefixes(&pm).to_string(), "ex:Dog");
    }

    #[test]
    fn slash_in_local_falls_back_to_full_iri() {
        // A local name containing `/` (e.g. a version IRI) is NOT a valid
        // Manchester local name, so it must render as a full `<iri>` rather
        // than an unreadable `ex:a/b` abbreviation.
        let b = Build::new_rc();
        let c = b.class("http://ex/a/b");
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        assert_eq!(
            c.as_manchester_with_prefixes(&pm).to_string(),
            "<http://ex/a/b>"
        );
    }

    #[test]
    fn renders_object_property_and_inverse() {
        let b = Build::new_rc();
        let p = b.object_property("http://example.org/hasParent");
        assert_eq!(
            ObjectPropertyExpression::ObjectProperty(p.clone())
                .as_manchester()
                .to_string(),
            "<http://example.org/hasParent>"
        );
        assert_eq!(
            ObjectPropertyExpression::InverseObjectProperty(p)
                .as_manchester()
                .to_string(),
            "inverse (<http://example.org/hasParent>)"
        );
    }

    #[test]
    fn renders_individual_and_literals() {
        let b = Build::new_rc();
        let i = Individual::Named(b.named_individual("http://example.org/fido"));
        assert_eq!(i.as_manchester().to_string(), "<http://example.org/fido>");

        let typed = Literal::Datatype {
            literal: "5".to_string(),
            datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
        };
        assert_eq!(
            typed.as_manchester().to_string(),
            "\"5\"^^<http://www.w3.org/2001/XMLSchema#integer>"
        );

        let lang = Literal::<RcStr>::Language {
            literal: "hello".to_string(),
            lang: "en".to_string(),
        };
        assert_eq!(lang.as_manchester().to_string(), "\"hello\"@en");

        let simple = Literal::<RcStr>::Simple {
            literal: "plain".to_string(),
        };
        assert_eq!(simple.as_manchester().to_string(), "\"plain\"");
    }

    #[test]
    fn renders_data_ranges_and_facets() {
        use crate::vocab::Facet;
        let b = Build::new_rc();
        let int = b.datatype("http://www.w3.org/2001/XMLSchema#integer");
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let m = |dr: &DataRange<_>| dr.as_manchester_with_prefixes(&pm).to_string();

        // bare datatype
        assert_eq!(m(&DataRange::Datatype(int.clone())), "xsd:integer");

        // xsd:integer[>= "0"^^xsd:integer]
        let fr = FacetRestriction {
            f: Facet::MinInclusive,
            l: Literal::Datatype {
                literal: "0".to_string(),
                datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
            },
        };
        assert_eq!(
            m(&DataRange::DatatypeRestriction(int.clone(), vec![fr])),
            "xsd:integer[>= \"0\"^^xsd:integer]"
        );

        // {1, 2} enumeration
        let one = Literal::Datatype {
            literal: "1".into(),
            datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
        };
        let two = Literal::Datatype {
            literal: "2".into(),
            datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
        };
        assert_eq!(
            m(&DataRange::DataOneOf(vec![one, two])),
            "{ \"1\"^^xsd:integer, \"2\"^^xsd:integer }"
        );

        // DataIntersectionOf precedence: union-inside-intersection → parens
        let int_dr = DataRange::Datatype(int.clone());
        let string_dt = b.datatype("http://www.w3.org/2001/XMLSchema#string");
        let str_dr = DataRange::Datatype(string_dt);
        let union = DataRange::DataUnionOf(vec![int_dr.clone(), str_dr.clone()]);
        // union inside intersection must be parenthesized
        assert_eq!(
            m(&DataRange::DataIntersectionOf(vec![union, int_dr.clone()])),
            "(xsd:integer or xsd:string) and xsd:integer"
        );

        // DataComplementOf a union → parens
        let union2 = DataRange::DataUnionOf(vec![int_dr.clone(), str_dr.clone()]);
        assert_eq!(
            m(&DataRange::DataComplementOf(Box::new(union2))),
            "not (xsd:integer or xsd:string)"
        );
    }

    #[test]
    fn renders_axioms_per_line() {
        let b = Build::new_rc();
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("", "http://t/").unwrap();
        let m = |c: &Component<_>| c.as_manchester_with_prefixes(&pm).to_string();

        let a = ClassExpression::Class(b.class("http://t/A"));
        let cc = ClassExpression::Class(b.class("http://t/C"));
        assert_eq!(
            m(&Component::SubClassOf(SubClassOf {
                sub: a.clone(),
                sup: cc.clone()
            })),
            "A SubClassOf C"
        );

        let x = Individual::Named(b.named_individual("http://t/x"));
        assert_eq!(
            m(&Component::ClassAssertion(ClassAssertion {
                ce: a.clone(),
                i: x.clone()
            })),
            "x Type A"
        );

        let r = b.object_property("http://t/r");
        let y = Individual::Named(b.named_individual("http://t/y"));
        assert_eq!(
            m(&Component::ObjectPropertyAssertion(
                ObjectPropertyAssertion {
                    ope: ObjectPropertyExpression::ObjectProperty(r.clone()),
                    from: x.clone(),
                    to: y,
                }
            )),
            "x r y"
        );

        assert_eq!(
            m(&Component::DisjointClasses(DisjointClasses(vec![
                a.clone(),
                cc.clone()
            ]))),
            "A DisjointWith C"
        );
    }

    #[test]
    fn renders_nary_disjoint_completely() {
        let b = Build::new_rc();
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("", "http://t/").unwrap();
        let m = |c: &Component<_>| c.as_manchester_with_prefixes(&pm).to_string();
        let ce = |n: &str| ClassExpression::Class(b.class(format!("http://t/{n}")));
        // 3-member DisjointClasses must convey all members, not a lossy chain.
        let s = m(&Component::DisjointClasses(DisjointClasses(vec![
            ce("A"),
            ce("B"),
            ce("C"),
        ])));
        assert_eq!(
            s, "A DisjointWith B, C",
            "n-ary disjoint must list all members; got {s}"
        );
        // binary unchanged
        let s2 = m(&Component::DisjointClasses(DisjointClasses(vec![
            ce("A"),
            ce("B"),
        ])));
        assert_eq!(s2, "A DisjointWith B");
    }

    #[test]
    fn renders_class_expressions_with_precedence() {
        let b = Build::new_rc();
        let a = ClassExpression::Class(b.class("http://t/A"));
        let c = ClassExpression::Class(b.class("http://t/C"));
        let d = ClassExpression::Class(b.class("http://t/D"));
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("", "http://t/").unwrap(); // default prefix → bare local names
        let m = |ce: &ClassExpression<_>| ce.as_manchester_with_prefixes(&pm).to_string();

        assert_eq!(
            m(&ClassExpression::ObjectIntersectionOf(vec![
                a.clone(),
                c.clone()
            ])),
            "A and C"
        );
        assert_eq!(
            m(&ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()])),
            "A or C"
        );
        assert_eq!(
            m(&ClassExpression::ObjectComplementOf(Box::new(a.clone()))),
            "not A"
        );

        // every non-atomic operand is parenthesized (OWL-API-compatible), so the
        // `and` sub-expression under `or` is bracketed even though precedence
        // would not strictly require it.
        let cd = ClassExpression::ObjectIntersectionOf(vec![c.clone(), d.clone()]);
        assert_eq!(
            m(&ClassExpression::ObjectUnionOf(vec![a.clone(), cd])),
            "A or (C and D)"
        );
        // or under and → MUST parenthesize
        let ac = ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]);
        assert_eq!(
            m(&ClassExpression::ObjectIntersectionOf(vec![ac, d.clone()])),
            "(A or C) and D"
        );
        // not over an `or` → parenthesized
        let aorc = ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]);
        assert_eq!(
            m(&ClassExpression::ObjectComplementOf(Box::new(aorc))),
            "not (A or C)"
        );

        let r = ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r"));
        assert_eq!(
            m(&ClassExpression::ObjectSomeValuesFrom {
                ope: r.clone(),
                bce: Box::new(a.clone())
            }),
            "r some A"
        );
        assert_eq!(
            m(&ClassExpression::ObjectAllValuesFrom {
                ope: r.clone(),
                bce: Box::new(a.clone())
            }),
            "r only A"
        );
        assert_eq!(
            m(&ClassExpression::ObjectMinCardinality {
                n: 2,
                ope: r.clone(),
                bce: Box::new(a.clone())
            }),
            "r min 2 A"
        );
        // filler that's an `or` under a restriction → parens
        let aorc2 = ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]);
        assert_eq!(
            m(&ClassExpression::ObjectSomeValuesFrom {
                ope: r,
                bce: Box::new(aorc2)
            }),
            "r some (A or C)"
        );
    }
}
