use std::collections::BTreeSet;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::fmt::Write;

use curie::PrefixMapping;
use enum_meta::Meta;

use crate::model::*;
use crate::vocab::Facet;

/// Check whether `c` is a `SPARQL_PnCharsBase` character, as defined by
/// `src/grammars/sparql.pest` (mirrors the SPARQL 1.0 `PN_CHARS_BASE`
/// production that OFN's `AbbreviatedIRI` local names are built on).
fn is_pn_chars_base(c: char) -> bool {
    c.is_ascii_alphabetic()
        || ('\u{00C0}'..='\u{00D6}').contains(&c)
        || ('\u{00D8}'..='\u{00F6}').contains(&c)
        || ('\u{00F8}'..='\u{02FF}').contains(&c)
        || ('\u{0370}'..='\u{037D}').contains(&c)
        || ('\u{037F}'..='\u{1FFF}').contains(&c)
        || ('\u{200C}'..='\u{200D}').contains(&c)
        || ('\u{2070}'..='\u{218F}').contains(&c)
        || ('\u{2C00}'..='\u{2FEF}').contains(&c)
        || ('\u{3001}'..='\u{D7FF}').contains(&c)
        || ('\u{F900}'..='\u{FDCF}').contains(&c)
        || ('\u{FDF0}'..='\u{FFFD}').contains(&c)
        || ('\u{10000}'..='\u{EFFFF}').contains(&c)
}

/// `SPARQL_PnCharsU`: `SPARQL_PnCharsBase` plus `_`.
fn is_pn_chars_u(c: char) -> bool {
    is_pn_chars_base(c) || c == '_'
}

/// `SPARQL_PnChars`: `SPARQL_PnCharsU` plus `-`, ASCII digits, and a handful
/// of extra code points the grammar allows mid-name.
fn is_pn_chars(c: char) -> bool {
    is_pn_chars_u(c)
        || c == '-'
        || c.is_ascii_digit()
        || c == '\u{00B7}'
        || ('\u{0300}'..='\u{036F}').contains(&c)
        || ('\u{203F}'..='\u{2040}').contains(&c)
}

/// Check whether `s` is usable as an OFN `AbbreviatedIRI` local name (the
/// `SPARQL_PnLocal` production: a leading `PnCharsU`/digit followed by any
/// number of `PnChars` or `.`, and not ending in `.`).
///
/// This is conservative rather than a byte-for-byte match of the grammar
/// (e.g. it allows consecutive `.` characters mid-name, which the grammar's
/// `("." ~ PnChars)*` repetition technically doesn't); false negatives here
/// just mean an abbreviation opportunity is missed in favour of the always-
/// correct `<full IRI>` form, whereas a false positive would write out an
/// unparseable `AbbreviatedIRI`, so erring conservative in that direction
/// would be the wrong trade-off.
fn is_valid_pn_local(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if is_pn_chars_u(c) || c.is_ascii_digit() => {}
        _ => return false,
    }
    if s.ends_with('.') {
        return false;
    }
    chars.all(|c| is_pn_chars(c) || c == '.')
}

/// Check whether `c` must be percent-encoded before it can appear inside an
/// OFN `<...>` full IRI.
///
/// horned-owl's IRI type is just an interned string -- it does not validate
/// that the text is a legal RFC 3987 IRI on construction (readers for XML-
/// and RDF-based formats hand IRI attribute/node text straight through, and
/// those formats don't require it to already be percent-encoded). Real-world
/// ontologies exploit this laxity: e.g. a fragment of `KB-CH[R]-8-5`, with a
/// literal `[`/`]` pair, is valid as an XML attribute value but is not a
/// legal `RFC3987_IriFragment` per `src/grammars/rfc3987.pest` (`[`/`]` are
/// gen-delims, only legal inside the authority's `IP-literal` production).
/// Writing such text raw into `<...>` therefore produces OFN the writer's
/// own reader then rejects.
///
/// This list is deliberately conservative rather than a full RFC 3987
/// legality check (which is position-dependent -- e.g. `:` and `/` are fine
/// in most positions but not all): it covers the ASCII "gen-delims and
/// unwise" characters that are never legal in an IRI's path/query/fragment
/// (`[`, `]`, `<`, `>`, `"`, space, backslash, backtick, `^`, `{`, `|`, `}`)
/// plus control characters, all of which are unconditionally illegal
/// anywhere in an IRI. It does not attempt to flag characters that are only
/// sometimes illegal (like a stray `#` or `?`), so a small number of
/// pathological IRIs could still round-trip incorrectly -- but false
/// negatives here just leave already-broken input broken, whereas encoding
/// too aggressively would mangle otherwise-valid IRIs, so erring toward the
/// conservative list is the safer trade-off (mirrors the same reasoning in
/// `is_valid_pn_local` below).
fn needs_iri_percent_encoding(c: char) -> bool {
    matches!(
        c,
        '[' | ']' | '<' | '>' | '"' | ' ' | '\\' | '`' | '^' | '{' | '|' | '}'
    ) || c.is_control()
}

/// Percent-encode any character in `s` that [`needs_iri_percent_encoding`]
/// flags, leaving everything else (including any `%XX` sequences already
/// present) untouched. Returns a borrowed `Cow` when nothing needed
/// encoding, so the common case (already-legal IRIs) allocates nothing.
fn percent_encode_iri(s: &str) -> std::borrow::Cow<'_, str> {
    if !s.chars().any(needs_iri_percent_encoding) {
        return std::borrow::Cow::Borrowed(s);
    }
    let mut out = String::with_capacity(s.len());
    let mut buf = [0u8; 4];
    for c in s.chars() {
        if needs_iri_percent_encoding(c) {
            for b in c.encode_utf8(&mut buf).as_bytes() {
                out.push('%');
                out.push_str(&format!("{b:02X}"));
            }
        } else {
            out.push(c);
        }
    }
    std::borrow::Cow::Owned(out)
}

/// Write a string literal while escaping `"` and `\` characters.
fn quote(mut s: &str, f: &mut Formatter<'_>) -> Result<(), Error> {
    f.write_str("\"")?;
    // `char_indices` yields byte offsets so the slices below land on char
    // boundaries even when earlier characters are multi-byte. `'"'` and `'\\'`
    // are both single-byte ASCII, so `i + 1` is always a valid boundary too.
    while let Some((i, c)) = s.char_indices().find(|(_, c)| *c == '\\' || *c == '"') {
        f.write_str(&s[..i])?;
        match c {
            '\\' => f.write_str("\\\\")?,
            '"' => f.write_str("\\\"")?,
            _ => unreachable!(),
        }
        s = &s[i + 1..];
    }
    f.write_str(s)?;
    f.write_str("\"")
}

/// A trait for OWL elements that can be rendered in OWL Functional syntax.
pub trait AsFunctional<A: ForIRI> {
    /// Get a handle for displaying the element in functional syntax.
    ///
    /// Instead of returning a `String`, this method returns an opaque struct
    /// that implements `Display`, which can be used to write to a file without
    /// having to build a fully-serialized string first, or to just get a string
    /// with the `ToString` implementation.
    ///
    fn as_functional(&self) -> Functional<'_, Self, A> {
        Functional(self, None, None)
    }

    /// Get a handle for displaying the element, using the given context.
    ///
    /// Pass around a `PrefixMapping`, allowing the functional representation
    /// to be written using abbreviated IRIs when possible.
    ///
    fn as_functional_with_prefixes<'t>(
        &'t self,
        prefix: &'t PrefixMapping,
    ) -> Functional<'t, Self, A> {
        Functional(self, Some(prefix), None)
    }
}

/// A wrapper for displaying an OWL2 element in functional syntax.
#[derive(Debug)]
pub struct Functional<'t, T: ?Sized, A: ForIRI>(
    /// The element to display
    &'t T,
    /// An eventual context to use (for IRI prefixes)
    Option<&'t PrefixMapping>,
    /// An eventual set of annotations (to render inside axioms)
    Option<&'t BTreeSet<Annotation<A>>>,
);

impl<'t, T, A> Display for Functional<'t, &'t T, A>
where
    Functional<'t, T, A>: Display,
    A: ForIRI,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        Functional(*self.0, self.1, self.2).fmt(f)
    }
}

// ---------------------------------------------------------------------------

macro_rules! derive_vec {
    ($A:ident, $t:ty) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, Vec<$t>, $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                for (i, x) in self.0.iter().enumerate() {
                    if i != 0 {
                        f.write_str(" ")?;
                    }
                    write!(f, "{}", Functional(x, self.1, None))?;
                }
                Ok(())
            }
        }
    };
}

derive_vec!(A, ClassExpression<A>);
derive_vec!(A, DataRange<A>);
derive_vec!(A, Individual<A>);
derive_vec!(A, ObjectPropertyExpression<A>);
derive_vec!(A, FacetRestriction<A>);
derive_vec!(A, Literal<A>);
derive_vec!(A, DataProperty<A>);
derive_vec!(A, Atom<A>);
derive_vec!(A, DArgument<A>);
derive_vec!(A, IArgument<A>);

// ---------------------------------------------------------------------------

macro_rules! derive_tuple1 {
    ($A:ident, $t:ty) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, (&$t,), $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                write!(f, "{}", Functional(self.0.0, self.1, None),)
            }
        }
    };
}

derive_tuple1!(A, IRI<A>);
derive_tuple1!(A, DataProperty<A>);
derive_tuple1!(A, ObjectPropertyExpression<A>);
derive_tuple1!(A, Vec<Individual<A>>);
derive_tuple1!(A, Vec<ClassExpression<A>>);
derive_tuple1!(A, Vec<DataProperty<A>>);
derive_tuple1!(A, Vec<ObjectPropertyExpression<A>>);

macro_rules! derive_tuple2 {
    ($A:ident, $t1:ty, $t2:ty) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, (&$t1, &$t2), $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                write!(
                    f,
                    "{} {}",
                    Functional(self.0.0, self.1, None),
                    Functional(self.0.1, self.1, None),
                )
            }
        }
    };
}

derive_tuple2!(A, IRI<A>, IRI<A>);
derive_tuple2!(A, IArgument<A>, IArgument<A>);
derive_tuple2!(A, DArgument<A>, DArgument<A>);
derive_tuple2!(A, Class<A>, Vec<ClassExpression<A>>);
derive_tuple2!(A, Datatype<A>, DataRange<A>);
derive_tuple2!(A, ClassExpression<A>, Individual<A>);
derive_tuple2!(A, ObjectProperty<A>, ObjectProperty<A>);
derive_tuple2!(A, ObjectPropertyExpression<A>, ClassExpression<A>);
derive_tuple2!(A, AnnotationProperty<A>, AnnotationValue<A>);
derive_tuple2!(A, AnnotationProperty<A>, IRI<A>);
derive_tuple2!(A, ClassExpression<A>, ClassExpression<A>);
derive_tuple2!(A, AnnotationProperty<A>, AnnotationProperty<A>);
derive_tuple2!(A, DataProperty<A>, DataProperty<A>);
derive_tuple2!(A, DataProperty<A>, DataRange<A>);
derive_tuple2!(A, DataProperty<A>, ClassExpression<A>);
derive_tuple2!(
    A,
    SubObjectPropertyExpression<A>,
    ObjectPropertyExpression<A>
);

macro_rules! derive_tuple3 {
    ($A:ident, $t1:ty, $t2:ty, $t3:ty) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, (&$t1, &$t2, &$t3), $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                write!(
                    f,
                    "{} {} {}",
                    Functional(self.0.0, self.1, None),
                    Functional(self.0.1, self.1, None),
                    Functional(self.0.2, self.1, None),
                )
            }
        }
    };
}

derive_tuple3!(A, DataProperty<A>, Individual<A>, Literal<A>);
derive_tuple3!(A, ObjectPropertyExpression<A>, Individual<A>, Individual<A>);

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, BTreeSet<Annotation<A>>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for (i, x) in self.0.iter().enumerate() {
            if i != 0 {
                f.write_str(" ")?;
            }
            write!(f, "{}", Functional(x, self.1, None))?;
        }
        Ok(())
    }
}

// ---------------------------------------------------------------------------

macro_rules! derive_declaration {
    ($A:ident, $ty:ty, $inner:ty, $name:ident) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, $ty, $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                if let Some(annotations) = self.2 {
                    write!(
                        f,
                        concat!("Declaration({} ", stringify!($name), "({}))"),
                        Functional(annotations, self.1, None),
                        Functional(&self.0.0, self.1, None)
                    )
                } else {
                    write!(
                        f,
                        concat!("Declaration(", stringify!($name), "({}))"),
                        Functional(&self.0.0, self.1, None)
                    )
                }
            }
        }

        impl<$A: ForIRI> AsFunctional<$A> for $ty {}
    };
}

derive_declaration!(A, DeclareClass<A>, Class<A>, Class);
derive_declaration!(
    A,
    DeclareAnnotationProperty<A>,
    AnnotationProperty<A>,
    AnnotationProperty
);
derive_declaration!(
    A,
    DeclareObjectProperty<A>,
    ObjectProperty<A>,
    ObjectProperty
);
derive_declaration!(A, DeclareDataProperty<A>, DataProperty<A>, DataProperty);
derive_declaration!(
    A,
    DeclareNamedIndividual<A>,
    NamedIndividual<A>,
    NamedIndividual
);
derive_declaration!(A, DeclareDatatype<A>, Datatype<A>, Datatype);

// ---------------------------------------------------------------------------

macro_rules! derive_wrapper {
    ($A:ident, $ty:ty) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, $ty, $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                write!(f, "{}", Functional(&self.0.0, self.1, None))
            }
        }

        impl<$A: ForIRI> AsFunctional<$A> for $ty {}
    };
}

derive_wrapper!(A, AnnotationProperty<A>);
derive_wrapper!(A, Class<A>);
derive_wrapper!(A, DataProperty<A>);
derive_wrapper!(A, Datatype<A>);
derive_wrapper!(A, NamedIndividual<A>);
derive_wrapper!(A, OntologyAnnotation<A>);
derive_wrapper!(A, ObjectProperty<A>);

// ---------------------------------------------------------------------------

macro_rules! derive_axiom {
    ($A:ident, $ty:ty, $name:ident ( $($field:tt),* )) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, $ty, $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                if let Some(annotations) = self.2 {
                    write!(
                        f,
                        concat!(stringify!($name), "({} {})"),
                        Functional(annotations, self.1, None),
                        Functional(&($(&self.0.$field,)*), self.1, None)
                    )
                } else {
                    write!(
                        f,
                        concat!(stringify!($name), "({})"),
                        Functional(&($(&self.0.$field,)*), self.1, None)
                    )
                }
            }
        }

        impl<$A: ForIRI> AsFunctional<$A> for $ty {}
    };
}

impl<'a, A: ForIRI> Display for Functional<'a, Annotation<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if self.0.ann.is_empty() {
            write!(
                f,
                "Annotation({} {})",
                Functional(&self.0.ap, self.1, None),
                Functional(&self.0.av, self.1, None),
            )
        } else {
            write!(
                f,
                "Annotation({} {} {})",
                Functional(&self.0.ann, self.1, None),
                Functional(&self.0.ap, self.1, None),
                Functional(&self.0.av, self.1, None),
            )
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for Annotation<A> {}
derive_axiom!(
    A,
    AnnotationPropertyRange<A>,
    AnnotationPropertyRange(ap, iri)
);
derive_axiom!(
    A,
    AnnotationPropertyDomain<A>,
    AnnotationPropertyDomain(ap, iri)
);
derive_axiom!(A, AsymmetricObjectProperty<A>, AsymmetricObjectProperty(0));
derive_axiom!(A, ClassAssertion<A>, ClassAssertion(ce, i));
derive_axiom!(
    A,
    DataPropertyAssertion<A>,
    DataPropertyAssertion(dp, from, to)
);
derive_axiom!(A, DataPropertyDomain<A>, DataPropertyDomain(dp, ce));
derive_axiom!(A, DataPropertyRange<A>, DataPropertyRange(dp, dr));
derive_axiom!(A, DatatypeDefinition<A>, DatatypeDefinition(kind, range));
derive_axiom!(A, DifferentIndividuals<A>, DifferentIndividuals(0));
derive_axiom!(A, DisjointClasses<A>, DisjointClasses(0));
derive_axiom!(A, DisjointDataProperties<A>, DisjointDataProperties(0));
derive_axiom!(A, DisjointObjectProperties<A>, DisjointObjectProperties(0));
derive_axiom!(A, DisjointUnion<A>, DisjointUnion(0, 1));
derive_axiom!(A, EquivalentClasses<A>, EquivalentClasses(0));
derive_axiom!(A, EquivalentDataProperties<A>, EquivalentDataProperties(0));
derive_axiom!(
    A,
    EquivalentObjectProperties<A>,
    EquivalentObjectProperties(0)
);
derive_axiom!(A, FunctionalObjectProperty<A>, FunctionalObjectProperty(0));
derive_axiom!(A, FunctionalDataProperty<A>, FunctionalDataProperty(0));
derive_axiom!(A, Import<A>, Import(0));
derive_axiom!(
    A,
    InverseFunctionalObjectProperty<A>,
    InverseFunctionalObjectProperty(0)
);
derive_axiom!(A, InverseObjectProperties<A>, InverseObjectProperties(0, 1));
derive_axiom!(
    A,
    IrreflexiveObjectProperty<A>,
    IrreflexiveObjectProperty(0)
);
derive_axiom!(
    A,
    NegativeDataPropertyAssertion<A>,
    NegativeDataPropertyAssertion(dp, from, to)
);
derive_axiom!(
    A,
    NegativeObjectPropertyAssertion<A>,
    NegativeObjectPropertyAssertion(ope, from, to)
);
derive_axiom!(
    A,
    ObjectPropertyAssertion<A>,
    ObjectPropertyAssertion(ope, from, to)
);
derive_axiom!(A, ObjectPropertyDomain<A>, ObjectPropertyDomain(ope, ce));
derive_axiom!(A, ObjectPropertyRange<A>, ObjectPropertyRange(ope, ce));
derive_axiom!(A, ReflexiveObjectProperty<A>, ReflexiveObjectProperty(0));
derive_axiom!(A, SameIndividual<A>, SameIndividual(0));
derive_axiom!(A, SubClassOf<A>, SubClassOf(sub, sup));
derive_axiom!(
    A,
    SubAnnotationPropertyOf<A>,
    SubAnnotationPropertyOf(sub, sup)
);
derive_axiom!(A, SubDataPropertyOf<A>, SubDataPropertyOf(sub, sup));
derive_axiom!(A, SubObjectPropertyOf<A>, SubObjectPropertyOf(sub, sup));
derive_axiom!(A, SymmetricObjectProperty<A>, SymmetricObjectProperty(0));
derive_axiom!(A, TransitiveObjectProperty<A>, TransitiveObjectProperty(0));

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, AnnotatedComponent<A>, A> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        if !self.0.ann.is_empty() {
            Functional(&self.0.component, self.1, Some(&self.0.ann)).fmt(f)
        } else {
            Functional(&self.0.component, self.1, None).fmt(f)
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for AnnotatedComponent<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, AnnotationAssertion<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if let Some(annotations) = self.2 {
            write!(
                f,
                "AnnotationAssertion({} {} {} {})",
                Functional(annotations, self.1, None),
                Functional(&self.0.ann.ap, self.1, None),
                Functional(&self.0.subject, self.1, None),
                Functional(&self.0.ann.av, self.1, None),
            )
        } else {
            write!(
                f,
                "AnnotationAssertion({} {} {})",
                Functional(&self.0.ann.ap, self.1, None),
                Functional(&self.0.subject, self.1, None),
                Functional(&self.0.ann.av, self.1, None),
            )
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for AnnotationAssertion<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, AnnotationSubject<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use AnnotationSubject::*;
        match &self.0 {
            IRI(iri) => Functional(iri, self.1, None).fmt(f),
            AnonymousIndividual(anon) => Functional(anon, self.1, None).fmt(f),
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for AnnotationSubject<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, AnnotationValue<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use AnnotationValue::*;
        match &self.0 {
            Literal(lit) => Functional(lit, self.1, None).fmt(f),
            IRI(iri) => Functional(iri, self.1, None).fmt(f),
            AnonymousIndividual(ai) => Functional(ai, self.1, None).fmt(f),
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for AnnotationValue<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, AnonymousIndividual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        // Functional syntax requires the `_:` blank-node prefix. Generated
        // labels (e.g. from the RDF reader) are bare, while labels parsed from
        // functional/Manchester input already carry it, so add it only when
        // absent to avoid double-prefixing.
        let label = self.0.0.borrow();
        if label.starts_with("_:") {
            write!(f, "{}", label)
        } else {
            write!(f, "_:{}", label)
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for AnonymousIndividual<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, Atom<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Atom::*;
        match self.0 {
            BuiltInAtom { pred, args } => {
                write!(
                    f,
                    "BuiltInAtom({} {})",
                    Functional(&pred, self.1, None),
                    Functional(&args, self.1, None),
                )
            }
            ClassAtom { pred, arg } => {
                write!(
                    f,
                    "ClassAtom({} {})",
                    Functional(&pred, self.1, None),
                    Functional(&arg, self.1, None),
                )
            }
            DataPropertyAtom { pred, args } => {
                write!(
                    f,
                    "DataPropertyAtom({} {})",
                    Functional(&pred, self.1, None),
                    Functional(&(&args.0, &args.1), self.1, None),
                )
            }
            DataRangeAtom { pred, arg } => {
                write!(
                    f,
                    "DataRangeAtom({} {})",
                    Functional(&pred, self.1, None),
                    Functional(&arg, self.1, None),
                )
            }
            DifferentIndividualsAtom(i1, i2) => {
                write!(
                    f,
                    "DifferentIndividualsAtom({} {})",
                    Functional(&i1, self.1, None),
                    Functional(&i2, self.1, None),
                )
            }
            ObjectPropertyAtom { pred, args } => {
                write!(
                    f,
                    "ObjectPropertyAtom({} {})",
                    Functional(&pred, self.1, None),
                    Functional(&(&args.0, &args.1), self.1, None),
                )
            }
            SameIndividualAtom(i1, i2) => {
                write!(
                    f,
                    "SameIndividualAtom({} {})",
                    Functional(&i1, self.1, None),
                    Functional(&i2, self.1, None),
                )
            }
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for Atom<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, Component<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        macro_rules! enum_impl {
            ($($variant:ident,)*) => {
                match self.0 {
                    $(Component::$variant(axiom) => {
                        Functional(&axiom, self.1, self.2).fmt(f)
                    }),*
                }
            }
        }
        enum_impl!(
            OntologyID,
            DocIRI,
            OntologyAnnotation,
            Import,
            DeclareClass,
            DeclareObjectProperty,
            DeclareAnnotationProperty,
            DeclareDataProperty,
            DeclareNamedIndividual,
            DeclareDatatype,
            SubClassOf,
            EquivalentClasses,
            DisjointClasses,
            DisjointUnion,
            SubObjectPropertyOf,
            EquivalentObjectProperties,
            DisjointObjectProperties,
            InverseObjectProperties,
            ObjectPropertyDomain,
            ObjectPropertyRange,
            FunctionalObjectProperty,
            InverseFunctionalObjectProperty,
            ReflexiveObjectProperty,
            IrreflexiveObjectProperty,
            SymmetricObjectProperty,
            AsymmetricObjectProperty,
            TransitiveObjectProperty,
            SubDataPropertyOf,
            EquivalentDataProperties,
            DisjointDataProperties,
            DataPropertyDomain,
            DataPropertyRange,
            FunctionalDataProperty,
            DatatypeDefinition,
            HasKey,
            SameIndividual,
            DifferentIndividuals,
            ClassAssertion,
            ObjectPropertyAssertion,
            NegativeObjectPropertyAssertion,
            DataPropertyAssertion,
            NegativeDataPropertyAssertion,
            AnnotationAssertion,
            SubAnnotationPropertyOf,
            AnnotationPropertyDomain,
            AnnotationPropertyRange,
            Rule,
        )
    }
}

impl<A: ForIRI> AsFunctional<A> for Component<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, ClassExpression<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use ClassExpression::*;
        macro_rules! object_cardinality {
            ($name:literal, $n:ident, $ope:ident, $bce:ident, $self:ident, $f:ident) => {
                match $bce.as_ref() {
                    ClassExpression::Class(cls)
                        if cls.0.as_ref() == crate::vocab::OWL::Thing.as_ref() =>
                    {
                        write!(
                            f,
                            concat!($name, "({} {})"),
                            $n,
                            Functional($ope, $self.1, None),
                        )
                    }
                    _ => {
                        write!(
                            f,
                            concat!($name, "({} {} {})"),
                            $n,
                            Functional($ope, $self.1, None),
                            Functional($bce.as_ref(), $self.1, None)
                        )
                    }
                }
            };
        }
        macro_rules! data_cardinality {
            ($name:literal, $n:ident, $dp:ident, $dr:ident, $self:ident, $f:ident) => {
                match $dr {
                    DataRange::Datatype(dt)
                        if dt.0.as_ref() == crate::vocab::OWL2Datatype::Literal.as_ref() =>
                    {
                        write!(
                            f,
                            concat!($name, "({} {})"),
                            $n,
                            Functional($dp, $self.1, None),
                        )
                    }
                    _ => {
                        write!(
                            f,
                            concat!($name, "({} {} {})"),
                            $n,
                            Functional($dp, $self.1, None),
                            Functional($dr, $self.1, None)
                        )
                    }
                }
            };
        }
        match self.0 {
            Class(exp) => Functional(exp, self.1, None).fmt(f),
            ObjectIntersectionOf(classes) => {
                write!(
                    f,
                    "ObjectIntersectionOf({})",
                    Functional(classes, self.1, None)
                )
            }
            ObjectUnionOf(classes) => {
                write!(f, "ObjectUnionOf({})", Functional(classes, self.1, None))
            }
            ObjectComplementOf(class) => {
                write!(
                    f,
                    "ObjectComplementOf({})",
                    Functional(class.as_ref(), self.1, None)
                )
            }
            ObjectOneOf(individuals) => {
                write!(f, "ObjectOneOf({})", Functional(individuals, self.1, None))
            }
            ObjectSomeValuesFrom { ope, bce } => {
                write!(
                    f,
                    "ObjectSomeValuesFrom({} {})",
                    Functional(ope, self.1, None),
                    Functional(bce.as_ref(), self.1, None)
                )
            }
            ObjectAllValuesFrom { ope, bce } => {
                write!(
                    f,
                    "ObjectAllValuesFrom({} {})",
                    Functional(ope, self.1, None),
                    Functional(bce.as_ref(), self.1, None)
                )
            }
            ObjectHasValue { ope, i } => {
                write!(
                    f,
                    "ObjectHasValue({} {})",
                    Functional(ope, self.1, None),
                    Functional(i, self.1, None)
                )
            }
            ObjectHasSelf(ope) => {
                write!(f, "ObjectHasSelf({})", Functional(ope, self.1, None))
            }
            ObjectMinCardinality { n, ope, bce } => {
                object_cardinality!("ObjectMinCardinality", n, ope, bce, self, f)
            }
            ObjectMaxCardinality { n, ope, bce } => {
                object_cardinality!("ObjectMaxCardinality", n, ope, bce, self, f)
            }
            ObjectExactCardinality { n, ope, bce } => {
                object_cardinality!("ObjectExactCardinality", n, ope, bce, self, f)
            }
            DataSomeValuesFrom { dp, dr } => {
                write!(
                    f,
                    "DataSomeValuesFrom({} {})",
                    Functional(dp, self.1, None),
                    Functional(dr, self.1, None)
                )
            }
            DataAllValuesFrom { dp, dr } => {
                write!(
                    f,
                    "DataAllValuesFrom({} {})",
                    Functional(dp, self.1, None),
                    Functional(dr, self.1, None)
                )
            }
            DataHasValue { dp, l } => {
                write!(
                    f,
                    "DataHasValue({} {})",
                    Functional(dp, self.1, None),
                    Functional(l, self.1, None)
                )
            }
            DataMinCardinality { n, dp, dr } => {
                data_cardinality!("DataMinCardinality", n, dp, dr, self, f)
            }
            DataMaxCardinality { n, dp, dr } => {
                data_cardinality!("DataMaxCardinality", n, dp, dr, self, f)
            }
            DataExactCardinality { n, dp, dr } => {
                data_cardinality!("DataExactCardinality", n, dp, dr, self, f)
            }
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for ClassExpression<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, DataRange<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use DataRange::*;
        match self.0 {
            Datatype(dt) => Functional(dt, self.1, None).fmt(f),
            DataIntersectionOf(dts) => {
                write!(f, "DataIntersectionOf({})", Functional(dts, self.1, None))
            }
            DataUnionOf(dts) => {
                write!(f, "DataUnionOf({})", Functional(dts, self.1, None))
            }
            DataComplementOf(dt) => {
                write!(
                    f,
                    "DataComplementOf({})",
                    Functional(dt.as_ref(), self.1, None)
                )
            }
            DataOneOf(lits) => {
                write!(f, "DataOneOf({})", Functional(lits, self.1, None))
            }
            DatatypeRestriction(dt, frs) => {
                write!(
                    f,
                    "DatatypeRestriction({} {})",
                    Functional(dt, self.1, None),
                    Functional(frs, self.1, None)
                )
            }
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for DataRange<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, DArgument<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use DArgument::*;
        match self.0 {
            Literal(l) => Functional(l, self.1, None).fmt(f),
            Variable(v) => Functional(v, self.1, None).fmt(f),
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for DArgument<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, Facet, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let iri = self.0.meta();
        Functional::<_, String>(iri, self.1, None).fmt(f)
    }
}

impl<A: ForIRI> AsFunctional<A> for Facet {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, FacetRestriction<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(
            f,
            "{} {}",
            Functional::<Facet, String>(&self.0.f, self.1, None),
            Functional(&self.0.l, self.1, None)
        )
    }
}

impl<A: ForIRI> AsFunctional<A> for FacetRestriction<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, HasKey<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "HasKey({} ", Functional(&self.0.ce, self.1, None))?;

        f.write_str("(")?;
        let mut n = 0;
        for pe in self.0.vpe.iter() {
            if let PropertyExpression::ObjectPropertyExpression(ope) = pe {
                if n != 0 {
                    f.write_str(" ")?;
                }
                Functional(ope, self.1, None).fmt(f)?;
                n += 1
            }
        }
        f.write_str(") ")?;

        f.write_str("(")?;
        let mut n = 0;
        for pe in self.0.vpe.iter() {
            if let PropertyExpression::DataProperty(dp) = pe {
                if n != 0 {
                    f.write_str(" ")?;
                }
                Functional(dp, self.1, None).fmt(f)?;
                n += 1
            }
        }
        f.write_str("))")
    }
}

impl<A: ForIRI> AsFunctional<A> for HasKey<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, IArgument<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use IArgument::*;
        match self.0 {
            Individual(i) => Functional(i, self.1, None).fmt(f),
            Variable(v) => Functional(v, self.1, None).fmt(f),
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for IArgument<A> {}

// ---------------------------------------------------------------------------

/// Abbreviate `iri` against `prefixes` as `prefix:local`, if possible.
///
/// This does not delegate to `curie::PrefixMapping::shrink_iri` because that
/// method special-cases a match against the mapping's "default" IRI (set via
/// `PrefixMapping::set_default`, as the OWL/XML reader does for an
/// empty-name `<Prefix name="" IRI="..."/>`) by returning a `Curie` with
/// `prefix: None` -- and `curie::Curie`'s `Display` renders that as the bare
/// local part with **no colon at all**, which is not a valid OFN
/// `AbbreviatedIRI` (`SPARQL_PnameLn` always requires the colon, even for an
/// empty prefix name, i.e. `:local`). It can also hand back a local part
/// that starts with `#` (when the matched prefix IRI has no trailing `/`/`#`
/// but the full IRI does), which is never valid in this position either way.
/// See https://github.com/phillord/horned-owl/issues/230.
///
/// Instead this walks `prefixes.mappings()` directly (the same
/// insertion-ordered, first-match-wins semantics `shrink_iri` itself uses
/// for its non-default branch) and only accepts a match whose local part
/// passes [`is_valid_pn_local`] (the full `SPARQL_PnLocal` character-class
/// check -- not just the default-prefix case above, but also rejecting any
/// other character illegal in `PN_LOCAL` anywhere in the local part, e.g. a
/// literal `/`, per issue #231), always emitting the colon -- so an
/// empty-name prefix match correctly becomes `:local`, not `local`. A
/// mapping that was only ever `set_default`-ed, with no corresponding
/// `add_prefix("", ...)`, is not visible via `mappings()` and so is never
/// abbreviated by this function; such IRIs are written out in full instead,
/// which is always valid, if slightly more verbose.
fn shrink_iri_for_ofn<'a>(prefixes: &'a PrefixMapping, iri: &str) -> Option<(&'a str, String)> {
    for (name, value) in prefixes.mappings() {
        if let Some(local) = iri.strip_prefix(value.as_str())
            && is_valid_pn_local(local)
        {
            return Some((name.as_str(), local.to_string()));
        }
    }
    None
}

impl<A: ForIRI> Display for Functional<'_, IRI<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if let Some(prefixes) = self.1.as_ref() {
            match shrink_iri_for_ofn(prefixes, self.0) {
                Some((name, local)) => write!(f, "{name}:{local}"),
                None => write!(f, "<{}>", percent_encode_iri(self.0)),
            }
        } else {
            write!(f, "<{}>", percent_encode_iri(self.0))
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for IRI<A> {}

// ---------------------------------------------------------------------------

// impl<'a, A: ForIRI> Display for Functional<'a, IRIString, A> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
//         if let Some(prefixes) = self.1.as_ref() {
//             match prefixes.shrink_iri(self.0.as_ref()) {
//                 Err(_) => write!(f, "<{}>", self.0.as_ref()),
//                 Ok(curie) => write!(f, "{}", curie),
//             }
//         } else {
//             write!(f, "<{}>", self.0.as_ref())
//         }
//     }
// }

// impl<A: ForIRI> AsFunctional<A> for IRIString {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, Individual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Individual::*;
        match self.0 {
            Named(i) => Functional(i, self.1, None).fmt(f),
            Anonymous(i) => Functional(i, self.1, None).fmt(f),
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for Individual<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, Literal<A>, A> {
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
                write!(f, "^^{}", Functional(datatype_iri, self.1, None))
            }
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for Literal<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, ObjectPropertyExpression<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use ObjectPropertyExpression::*;
        match self.0 {
            ObjectProperty(op) => Functional(op, self.1, None).fmt(f),
            InverseObjectProperty(op) => {
                write!(f, "ObjectInverseOf({})", Functional(op, self.1, None))
            }
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for ObjectPropertyExpression<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, Rule<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if let Some(annotations) = self.2 {
            write!(f, "DLSafeRule({}", Functional(annotations, self.1, None))?;
        } else {
            write!(f, "DLSafeRule(")?;
        }

        f.write_str("Body(")?;
        for atom in self.0.body.iter() {
            Functional(&atom, self.1, None).fmt(f)?;
        }
        f.write_char(')')?;

        f.write_str("Head(")?;
        for atom in self.0.head.iter() {
            Functional(&atom, self.1, None).fmt(f)?;
        }
        f.write_char(')')?;
        f.write_char(')')
    }
}

impl<A: ForIRI> AsFunctional<A> for Rule<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, SubObjectPropertyExpression<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use SubObjectPropertyExpression::*;
        match self.0 {
            ObjectPropertyExpression(ope) => Functional(ope, self.1, None).fmt(f),
            ObjectPropertyChain(chain) => {
                write!(
                    f,
                    "ObjectPropertyChain({})",
                    Functional(chain, self.1, None)
                )
            }
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for SubObjectPropertyExpression<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, Variable<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "Variable({})", Functional(&self.0.0, self.1, None))
    }
}

impl<A: ForIRI> AsFunctional<A> for Variable<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, curie::PrefixMapping, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        for (name, value) in self.0.mappings() {
            writeln!(f, "Prefix({name}:=<{}>)", percent_encode_iri(value))?;
        }
        Ok(())
    }
}

impl<A: ForIRI> AsFunctional<A> for curie::PrefixMapping {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, OntologyID<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match (&self.0.iri, &self.0.viri) {
            (Some(x), Some(y)) => Functional(&(x, y), self.1, None).fmt(f),
            (None, Some(y)) => Functional(y, self.1, None).fmt(f),
            (Some(x), None) => Functional(x, self.1, None).fmt(f),
            (None, None) => Ok(()),
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for OntologyID<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, DocIRI<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        Functional(&self.0.0, self.1, None).fmt(f)
    }
}

impl<A: ForIRI> AsFunctional<A> for DocIRI<A> {}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {

    use super::*;
    use std::iter::FromIterator;

    #[test]
    fn test_ofn_declareclass() {
        let build = Build::new_arc();
        let decl = DeclareClass(build.class("http://purl.obolibrary.org/obo/BFO_0000001"));
        let ofn = format!("{}", decl.as_functional());
        assert_eq!(
            "Declaration(Class(<http://purl.obolibrary.org/obo/BFO_0000001>))",
            &ofn
        );
    }

    #[test]
    fn test_ofn_literal_simple() {
        let lit = Literal::<String>::Simple {
            literal: String::from("test"),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(r#""test""#, &ofn);

        let lit = Literal::<String>::Simple {
            literal: String::from("test\""),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(r#""test\"""#, &ofn);

        let lit = Literal::<String>::Simple {
            literal: String::from("test\\"),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(r#""test\\""#, &ofn);
    }

    #[test]
    fn test_ofn_literal_multibyte_escape() {
        // A multi-byte character preceding an escaped `"` or `\` must not cause
        // a byte-vs-char index mismatch while slicing (regression: panicked at
        // a non-char boundary, e.g. inside `é` or a combining mark).
        let lit = Literal::<String>::Simple {
            literal: String::from("café\""),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(r#""café\"""#, &ofn);

        let lit = Literal::<String>::Simple {
            literal: String::from("素面\\x"),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(r#""素面\\x""#, &ofn);
    }

    #[test]
    fn test_ofn_anonymous_individual_nodeid() {
        let build = Build::new_arc();

        // Generated anonymous individuals (e.g. from the RDF reader, via
        // `anon_renumbered`) hold a BARE label; functional syntax requires the
        // `_:` blank-node prefix, so it must be added.
        let anon = build.anon("anon000007");
        assert_eq!("_:anon000007", format!("{}", anon.as_functional()));

        // A label that already carries `_:` (e.g. parsed from functional/
        // Manchester input) must not be double-prefixed.
        let anon = build.anon("_:x1");
        assert_eq!("_:x1", format!("{}", anon.as_functional()));
    }

    #[test]
    fn test_ofn_literal_language() {
        let lit = Literal::<String>::Language {
            literal: String::from("hello"),
            lang: String::from("en"),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(r#""hello"@en"#, &ofn);
    }

    #[test]
    fn test_ofn_literal_datatype() {
        let build = Build::new_arc();
        let lit = Literal::Datatype {
            literal: String::from("hello"),
            datatype_iri: build.iri("http://www.w3.org/2001/XMLSchema#string"),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(
            r#""hello"^^<http://www.w3.org/2001/XMLSchema#string>"#,
            &ofn
        );
    }

    #[test]
    fn test_ofn_import() {
        let build = Build::new_arc();
        let import = Import(build.iri("http://example.com/"));
        let ofn = format!("{}", import.as_functional());
        assert_eq!("Import(<http://example.com/>)", ofn);
    }

    #[test]
    fn test_ofn_curie() {
        let build = Build::new_arc();
        let mut prefixes = curie::PrefixMapping::default();
        prefixes
            .add_prefix("obo", "http://purl.obolibrary.org/obo/")
            .ok();

        let decl = DeclareClass(build.class("http://purl.obolibrary.org/obo/BFO_0000001"));
        let ofn = format!("{}", decl.as_functional_with_prefixes(&prefixes));
        assert_eq!("Declaration(Class(obo:BFO_0000001))", ofn);

        let decl = DeclareClass(build.class("http://xmlns.com/foaf/0.1/Person"));
        let ofn = format!("{}", decl.as_functional_with_prefixes(&prefixes));
        assert_eq!(
            "Declaration(Class(<http://xmlns.com/foaf/0.1/Person>))",
            ofn
        );
    }

    // Regression test for https://github.com/phillord/horned-owl/issues/230
    // An empty/default CURIE prefix (as the OWL/XML reader creates via
    // `add_prefix("", iri)` + `set_default(iri)` for a `<Prefix name=""
    // IRI="..."/>` element) must still abbreviate with the leading colon
    // `:local` -- not the bare `local` that `curie::Curie`'s own `Display`
    // produces for a `prefix: None` match, which isn't a valid OFN
    // `AbbreviatedIRI` at all.
    #[test]
    fn test_ofn_curie_empty_prefix() {
        let build = Build::new_arc();
        let mut prefixes = curie::PrefixMapping::default();
        prefixes.add_prefix("", "http://identifiers.org/mamo#").ok();
        prefixes.set_default("http://identifiers.org/mamo#");

        let decl = DeclareClass(build.class("http://identifiers.org/mamo#MAMO_0000207"));
        let ofn = format!("{}", decl.as_functional_with_prefixes(&prefixes));
        assert_eq!("Declaration(Class(:MAMO_0000207))", ofn);
    }

    // Same bug, but for the shape seen in the `MAMO` corpus ontology itself:
    // the default-prefix IRI has no trailing `/` or `#` separator, so the
    // leftover local part starts with `#` (from the entity IRI's own
    // fragment separator). `#` is not a legal `PN_LOCAL` character in OFN's
    // grammar, so this must fall back to the full `<IRI>` form rather than
    // emit `:#MAMO_0000207` (still invalid) or the pre-fix `#MAMO_0000207`
    // (invalid for a second, independent reason -- no colon at all).
    #[test]
    fn test_ofn_curie_empty_prefix_no_separator_falls_back_to_full_iri() {
        let build = Build::new_arc();
        let mut prefixes = curie::PrefixMapping::default();
        prefixes.add_prefix("", "http://identifiers.org/mamo").ok();
        prefixes.set_default("http://identifiers.org/mamo");

        let decl = DeclareClass(build.class("http://identifiers.org/mamo#MAMO_0000207"));
        let ofn = format!("{}", decl.as_functional_with_prefixes(&prefixes));
        assert_eq!(
            "Declaration(Class(<http://identifiers.org/mamo#MAMO_0000207>))",
            ofn
        );
    }

    // Regression test for https://github.com/phillord/horned-owl/issues/231
    //
    // A local name that matches a registered prefix but contains a
    // character illegal in OFN's PN_LOCAL (here a literal `/`) must not be
    // abbreviated -- the abbreviated form would not be valid OFN and would
    // fail to reread. It must fall back to the `<full IRI>` form instead,
    // exactly as if no prefix had matched at all.
    #[test]
    fn test_ofn_curie_invalid_pn_local_falls_back_to_full_iri() {
        let build = Build::new_arc();
        let mut prefixes = curie::PrefixMapping::default();
        prefixes.add_prefix("", "http://example.org/mini#").ok();

        let decl = DeclareClass(build.class("http://example.org/mini#Direct/Indirect"));
        let ofn = format!("{}", decl.as_functional_with_prefixes(&prefixes));
        assert_eq!(
            "Declaration(Class(<http://example.org/mini#Direct/Indirect>))",
            ofn
        );

        // A local name with only legal PN_LOCAL characters is unaffected
        // and is still abbreviated as before.
        let decl = DeclareClass(build.class("http://example.org/mini#Direct"));
        let ofn = format!("{}", decl.as_functional_with_prefixes(&prefixes));
        assert_eq!("Declaration(Class(:Direct))", ofn);
    }

    // Regression test for https://github.com/phillord/horned-owl/issues/234
    //
    // A literal '[' or ']' in an IRI is legal in an XML attribute (so real-
    // world OWL/XML ontologies contain it -- e.g. a fragment like
    // "KB-CH[R]-8-5") but is not legal, unescaped, inside OFN's `<...>`
    // FullIRI production (per src/grammars/rfc3987.pest, '[' and ']' are
    // gen-delims only permitted inside the authority's IP-literal). Writing
    // such an IRI raw produces OFN that horned-owl's own reader then
    // rejects on reread. Both the full-IRI form (no prefix match) and the
    // `Prefix(name:=<...>)` declaration line must percent-encode it.
    #[test]
    fn test_ofn_iri_with_illegal_characters_is_percent_encoded() {
        let build = Build::new_arc();

        // No prefixes in scope: full <...> form must be percent-encoded.
        let decl = DeclareClass(build.class("http://example.org/KB-CH[R]-8-5"));
        let ofn = format!("{}", decl.as_functional());
        assert_eq!(
            "Declaration(Class(<http://example.org/KB-CH%5BR%5D-8-5>))",
            ofn
        );

        // The round-tripped text must be re-parseable as OFN.
        let reparsed: Result<(crate::ontology::set::SetOntology<RcStr>, _), _> =
            crate::io::ofn::reader::read(
                std::io::Cursor::new(format!(
                    "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\n{ofn}\n)"
                )),
                Default::default(),
            );
        assert!(reparsed.is_ok(), "reparse failed: {reparsed:?}");

        // A `Prefix(name:=<...>)` declaration line (rendered directly from
        // a PrefixMapping, as happens for the recovered mapping from an
        // OWL/XML source) must also percent-encode illegal characters.
        let mut prefixes = curie::PrefixMapping::default();
        prefixes
            .add_prefix("R", "http://example.org/KB-CH[R]-8-5")
            .ok();
        let rendered = format!(
            "{}",
            Functional::<curie::PrefixMapping, RcStr>(&prefixes, None, None)
        );
        assert_eq!(
            "Prefix(R:=<http://example.org/KB-CH%5BR%5D-8-5>)\n",
            rendered
        );
    }

    #[test]
    fn test_annotated_axiom() {
        let build = Build::new_arc();
        let mut prefixes = curie::PrefixMapping::default();
        prefixes
            .add_prefix("obo", "http://purl.obolibrary.org/obo/")
            .ok();
        prefixes
            .add_prefix("oboInOwl", "http://www.geneontology.org/formats/oboInOwl#")
            .ok();

        let component = EquivalentClasses(vec![
            ClassExpression::Class(build.class("http://purl.obolibrary.org/obo/HAO_0000935")),
            ClassExpression::Class(build.class("http://purl.obolibrary.org/obo/HAO_0000933")),
        ]);
        let annotated = AnnotatedComponent {
            component: Component::EquivalentClasses(component),
            ann: BTreeSet::from_iter([Annotation {
                ap: build
                    .annotation_property("http://www.geneontology.org/formats/oboInOwl#hasDbXref"),
                av: AnnotationValue::Literal(Literal::Simple {
                    literal: "http://api.hymao.org/api/ref/67791".into(),
                }),
                ann: Default::default(),
            }]),
        };

        let ofn = annotated.as_functional_with_prefixes(&prefixes).to_string();
        assert_eq!(
            ofn,
            r#"EquivalentClasses(Annotation(oboInOwl:hasDbXref "http://api.hymao.org/api/ref/67791") obo:HAO_0000935 obo:HAO_0000933)"#
        )
    }
}
