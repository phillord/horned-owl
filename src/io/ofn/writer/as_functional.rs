use std::collections::BTreeSet;
use std::fmt::Display;
use std::fmt::Error;
use std::fmt::Formatter;
use std::fmt::Write;

use curie::PrefixMapping;
use enum_meta::Meta;

use crate::model::*;
use crate::vocab::Facet;

/// The datatype a bare quoted literal already denotes in OWL 2.
const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";

/// Whether `^^xsd:string` is written out explicitly. OWLAPI leaves it implicit
/// (see the `Literal::Datatype` arm below), which is what ROBOT's output shows —
/// but the OWLAPI bundled by some other tools does render it, and reproducing
/// such a tool's file byte for byte needs the explicit form. Off by default.
static WRITE_XSD_STRING: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Set whether the functional writer renders `^^xsd:string` explicitly.
pub fn set_write_xsd_string(on: bool) {
    WRITE_XSD_STRING.store(on, std::sync::atomic::Ordering::Relaxed);
}

fn write_xsd_string() -> bool {
    WRITE_XSD_STRING.load(std::sync::atomic::Ordering::Relaxed)
}

/// Whether `c` must be percent-encoded before it can appear inside an OFN
/// `<...>` full IRI.
///
/// A conservative, position-independent subset of RFC 3987's illegal
/// characters (gen-delims/unwise chars plus controls) -- under-flagging is
/// safe here, since it only leaves already-broken input broken; the real
/// risk is flagging too much and mangling a valid IRI. See #234.
fn needs_iri_percent_encoding(c: char) -> bool {
    matches!(
        c,
        '[' | ']' | '<' | '>' | '"' | ' ' | '\\' | '`' | '^' | '{' | '|' | '}'
    ) || c.is_control()
}

/// Percent-encodes every character [`needs_iri_percent_encoding`] flags in
/// `s`, leaving the rest untouched.
pub(super) fn percent_encode_iri(s: &str) -> std::borrow::Cow<'_, str> {
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
    // `char_indices` yields *byte* offsets, so slicing stays on char
    // boundaries even when the string contains multi-byte UTF-8 characters.
    // (Using `chars().enumerate()` here gives a char index and panics when a
    // multi-byte char precedes a `"`/`\\`, e.g. Greek letters in a definition.)
    while let Some((i, c)) = s.char_indices().find(|(_, c)| *c == '\\' || *c == '"') {
        f.write_str(&s[..i])?;
        match c {
            '\\' => f.write_str("\\\\")?,
            '"' => f.write_str("\\\"")?,
            _ => unreachable!(),
        }
        s = &s[i + c.len_utf8()..];
    }
    f.write_str(s)?;
    f.write_str("\"")
}

/// Which of OWL API's renderings of an object a rendering follows.
#[derive(Clone, Copy)]
pub enum Style<'t> {
    /// A functional-syntax document's: the functional renderer writing an
    /// object in the frame it stands in.
    Document,
    /// OWL API's `toString()`. A literal typed `xsd:string` says so; an IRI
    /// an axiom names as an object (an annotation's subject or value, an
    /// annotation property's domain or range) is written in full; a
    /// cardinality restriction writes its filler, `owl:Thing` and
    /// `rdfs:Literal` included; an intersection or union of one operand, and
    /// an axiom of a set of fewer than two members, is written as it stands;
    /// a facet restriction is
    /// `facetRestriction(minInclusive "1"^^xsd:integer)`; a rule keeps its
    /// atoms in their order, writes `Body(…) Head(…)` apart and names its
    /// same- and different-individual atoms `SameAsAtom` and
    /// `DifferentFromAtom`; and a same-individuals pair keeps its order.
    Simple,
    /// The functional renderer writing an object outside any frame, with every
    /// entity, and every IRI an axiom names as an object (an annotation's
    /// subject or value, an annotation property's domain or range, a rule's
    /// variable or built-in), written as the function writes that IRI. A
    /// literal typed `xsd:string` leaves the type implicit, and a
    /// same-individuals pair, and a rule's body or head of two atoms, is
    /// written second member first. Inside a declaration every such name
    /// stands in its entity type, an IRI named as an object in `Class(…)`:
    /// `Declaration(Annotation(AnnotationProperty(<p>) Class(<v>)) Class(<A>))`.
    Named(&'t dyn Fn(&str) -> String),
}

impl std::fmt::Debug for Style<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str(match self {
            Style::Document => "Document",
            Style::Simple => "Simple",
            Style::Named(_) => "Named",
        })
    }
}

/// What a rendering abbreviates IRIs with, and the [`Style`] it follows.
#[derive(Clone, Copy, Debug)]
pub struct Context<'t> {
    prefixes: Option<&'t PrefixMapping>,
    style: Style<'t>,
    /// Whether a [`Style::Named`] name stands in its entity type, as inside a
    /// declaration.
    typed: bool,
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
        Functional(self, Context { prefixes: None, style: Style::Document, typed: false }, None)
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
        Functional(self, Context { prefixes: Some(prefix), style: Style::Document, typed: false }, None)
    }

    /// Get a handle for displaying the element as `style` renders it, with
    /// IRIs abbreviated by `prefix`.
    fn as_functional_styled<'t>(
        &'t self,
        prefix: &'t PrefixMapping,
        style: Style<'t>,
    ) -> Functional<'t, Self, A> {
        Functional(self, Context { prefixes: Some(prefix), style, typed: false }, None)
    }
}

/// A wrapper for displaying an OWL2 element in functional syntax.
#[derive(Debug)]
pub struct Functional<'t, T: ?Sized, A: ForIRI>(
    /// The element to display
    &'t T,
    /// The prefixes IRIs are abbreviated with, and the rendering's style
    Context<'t>,
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
derive_tuple2!(A, ObjectPropertyExpression<A>, ObjectPropertyExpression<A>);
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
        // OWLAPI renders an axiom's/entity's annotations in `compareTo` order, not
        // the model's `BTreeSet` order. The only place the two diverge for OBO
        // content is the annotation *value*: OWLAPI's type index orders IRI (0) <
        // anonymous individual (1007) < literal (4008), whereas horned-owl's
        // `AnnotationValue` enum orders literal first. Re-sort with the OWLAPI key
        // so e.g. `Annotation(hasDbXref <orcid>)` precedes `Annotation(hasDbXref
        // "PMID:…")`, matching ROBOT.
        let mut anns: Vec<&Annotation<A>> = self.0.iter().collect();
        anns.sort_by(owlapi_annotation_cmp);
        for (i, x) in anns.iter().enumerate() {
            if i != 0 {
                f.write_str(" ")?;
            }
            write!(f, "{}", Functional(*x, self.1, None))?;
        }
        Ok(())
    }
}

/// OWLAPI's annotation-value type index: IRI < anonymous individual < literal.
fn annotation_value_rank<A: ForIRI>(v: &AnnotationValue<A>) -> u8 {
    match v {
        AnnotationValue::IRI(_) => 0,
        AnnotationValue::AnonymousIndividual(_) => 1,
        AnnotationValue::Literal(_) => 2,
    }
}

/// Compare two annotations the way OWLAPI's `OWLAnnotation.compareTo` does:
/// property first, then value (by value-type index, then value content). Every
/// leaf uses OWLAPI's own key — `IRI.compareTo` splits at the NCName suffix, and
/// a literal compares on datatype before lexical form — so an annotation set
/// orders the same way whether it hangs off an axiom or is an axiom itself.
fn owlapi_annotation_cmp<A: ForIRI>(a: &&Annotation<A>, b: &&Annotation<A>) -> std::cmp::Ordering {
    use super::{owlapi_iri_cmp, owlapi_literal_cmp};
    owlapi_iri_cmp(a.ap.0.as_ref(), b.ap.0.as_ref())
        .then_with(|| annotation_value_rank(&a.av).cmp(&annotation_value_rank(&b.av)))
        .then_with(|| match (&a.av, &b.av) {
            (AnnotationValue::IRI(x), AnnotationValue::IRI(y)) => {
                owlapi_iri_cmp(x.as_ref(), y.as_ref())
            }
            (AnnotationValue::Literal(x), AnnotationValue::Literal(y)) => owlapi_literal_cmp(x, y),
            _ => a.av.cmp(&b.av),
        })
}

// ---------------------------------------------------------------------------

macro_rules! derive_declaration {
    ($A:ident, $ty:ty, $inner:ty, $name:ident) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, $ty, $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                if let Some(annotations) = self.2 {
                    let typed = Context { typed: matches!(self.1.style, Style::Named(_)), ..self.1 };
                    write!(
                        f,
                        concat!("Declaration({} ", stringify!($name), "({}))"),
                        Functional(annotations, typed, None),
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

derive_wrapper!(A, OntologyAnnotation<A>);

/// An entity's IRI, or a rule's variable or built-in, whose entity type is
/// `kind`: as a [`Style::Named`] rendering names it, and otherwise as any IRI
/// is written.
fn entity_iri<A: ForIRI>(
    iri: &IRI<A>,
    kind: &str,
    ctx: Context<'_>,
    f: &mut Formatter<'_>,
) -> Result<(), Error> {
    match ctx.style {
        Style::Named(names) if ctx.typed => write!(f, "{kind}({})", names(iri.as_ref())),
        Style::Named(names) => f.write_str(&names(iri.as_ref())),
        Style::Document | Style::Simple => Functional(iri, ctx, None).fmt(f),
    }
}

/// An IRI an axiom names as an object: an annotation's subject or value, or
/// an annotation property's domain or range. [`Style::Named`] names it as a
/// class, and [`Style::Simple`] writes it in full.
fn object_iri<A: ForIRI>(iri: &IRI<A>, ctx: Context<'_>, f: &mut Formatter<'_>) -> Result<(), Error> {
    match ctx.style {
        Style::Simple => write!(f, "<{}>", iri.as_ref() as &str),
        Style::Document | Style::Named(_) => entity_iri(iri, "Class", ctx, f),
    }
}

macro_rules! derive_entity {
    ($A:ident, $ty:ty, $kind:ident) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, $ty, $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                entity_iri(&self.0.0, stringify!($kind), self.1, f)
            }
        }

        impl<$A: ForIRI> AsFunctional<$A> for $ty {}
    };
}

derive_entity!(A, AnnotationProperty<A>, AnnotationProperty);
derive_entity!(A, Class<A>, Class);
derive_entity!(A, DataProperty<A>, DataProperty);
derive_entity!(A, Datatype<A>, Datatype);
derive_entity!(A, NamedIndividual<A>, NamedIndividual);
derive_entity!(A, ObjectProperty<A>, ObjectProperty);

// ---------------------------------------------------------------------------

/// Like `derive_axiom!`, but for a single-field `Vec<T>` axiom whose OWL2
/// functional-syntax grammar rule requires >= 2 operands. Real-world RDF can
/// produce a shorter vec here (e.g. a degenerate `owl:AllDifferent` with one
/// `owl:distinctMembers` entry), which is semantically vacuous -- writing it
/// out anyway would produce `DifferentIndividuals(<one-iri>)`, syntax our
/// own reader rejects. Drop the axiom instead of echoing unparseable output,
/// except in [`Style::Simple`], which writes the axiom as it stands.
macro_rules! derive_nary_axiom {
    ($A:ident, $ty:ty, $name:ident) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, $ty, $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                if self.0.0.len() < 2 && !matches!(self.1.style, Style::Simple) {
                    return Ok(());
                }
                if let Some(annotations) = self.2 {
                    write!(
                        f,
                        concat!(stringify!($name), "({} {})"),
                        Functional(annotations, self.1, None),
                        Functional(&self.0.0, self.1, None)
                    )
                } else {
                    write!(
                        f,
                        concat!(stringify!($name), "({})"),
                        Functional(&self.0.0, self.1, None)
                    )
                }
            }
        }

        impl<$A: ForIRI> AsFunctional<$A> for $ty {}
    };
}

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
macro_rules! derive_annotation_property_iri_axiom {
    ($A:ident, $ty:ty, $name:ident) => {
        impl<'a, $A: ForIRI> Display for Functional<'a, $ty, $A> {
            fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
                f.write_str(concat!(stringify!($name), "("))?;
                if let Some(annotations) = self.2 {
                    write!(f, "{} ", Functional(annotations, self.1, None))?;
                }
                write!(f, "{} ", Functional(&self.0.ap, self.1, None))?;
                object_iri(&self.0.iri, self.1, f)?;
                f.write_str(")")
            }
        }

        impl<$A: ForIRI> AsFunctional<$A> for $ty {}
    };
}

derive_annotation_property_iri_axiom!(A, AnnotationPropertyRange<A>, AnnotationPropertyRange);
derive_annotation_property_iri_axiom!(A, AnnotationPropertyDomain<A>, AnnotationPropertyDomain);
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
derive_nary_axiom!(A, DifferentIndividuals<A>, DifferentIndividuals);
derive_nary_axiom!(A, DisjointClasses<A>, DisjointClasses);
derive_nary_axiom!(A, DisjointDataProperties<A>, DisjointDataProperties);
derive_nary_axiom!(A, DisjointObjectProperties<A>, DisjointObjectProperties);
derive_nary_axiom!(A, EquivalentClasses<A>, EquivalentClasses);
derive_nary_axiom!(A, EquivalentDataProperties<A>, EquivalentDataProperties);
derive_nary_axiom!(A, EquivalentObjectProperties<A>, EquivalentObjectProperties);

// A disjoint union is written whatever the number of its members: one member
// makes `DisjointUnion(:U :B)` and none `DisjointUnion(:U )`, each of them an
// axiom with a meaning of its own (`U ≡ B`, `U ≡ owl:Nothing`).
impl<'a, A: ForIRI> Display for Functional<'a, DisjointUnion<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if let Some(annotations) = self.2 {
            write!(
                f,
                "DisjointUnion({} {} {})",
                Functional(annotations, self.1, None),
                Functional(&self.0.0, self.1, None),
                Functional(&self.0.1, self.1, None)
            )
        } else {
            write!(
                f,
                "DisjointUnion({} {})",
                Functional(&self.0.0, self.1, None),
                Functional(&self.0.1, self.1, None)
            )
        }
    }
}
impl<A: ForIRI> AsFunctional<A> for DisjointUnion<A> {}
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
/// The positions of a collection's `len` members in the order they are
/// written: as they stand, except that a pair is written second member first
/// unless its first member is the entity whose frame is being written
/// (`first_is_focus`).
fn written_order(len: usize, first_is_focus: bool) -> Vec<usize> {
    if len == 2 && !first_is_focus {
        vec![1, 0]
    } else {
        (0..len).collect()
    }
}

// The frame a same-individuals axiom stands in is its first named member's, so
// a pair whose first member is anonymous is written turned round.
impl<'a, A: ForIRI> Display for Functional<'a, SameIndividual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        let members = &self.0.0;
        if members.len() < 2 && !matches!(self.1.style, Style::Simple) {
            return Ok(());
        }
        let order = match self.1.style {
            Style::Document => written_order(members.len(), matches!(members[0], Individual::Named(_))),
            Style::Simple => (0..members.len()).collect(),
            Style::Named(_) => written_order(members.len(), false),
        };
        let members: Vec<Individual<A>> = order.iter().map(|&i| members[i].clone()).collect();
        match self.2 {
            Some(annotations) => write!(
                f,
                "SameIndividual({} {})",
                Functional(annotations, self.1, None),
                Functional(&members, self.1, None)
            ),
            None => write!(f, "SameIndividual({})", Functional(&members, self.1, None)),
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for SameIndividual<A> {}
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
            IRI(iri) => object_iri(iri, self.1, f),
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
            IRI(iri) => object_iri(iri, self.1, f),
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
                f.write_str("BuiltInAtom(")?;
                entity_iri(pred, "Class", self.1, f)?;
                write!(f, " {})", Functional(&args, self.1, None))
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
                    "DataPropertyAtom({} {} {})",
                    Functional(&pred, self.1, None),
                    Functional(&args.0, self.1, None),
                    Functional(&args.1, self.1, None),
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
                let name = match self.1.style {
                    Style::Simple => "DifferentFromAtom",
                    Style::Document | Style::Named(_) => "DifferentIndividualsAtom",
                };
                write!(
                    f,
                    "{name}({} {})",
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
                let name = match self.1.style {
                    Style::Simple => "SameAsAtom",
                    Style::Document | Style::Named(_) => "SameIndividualAtom",
                };
                write!(
                    f,
                    "{name}({} {})",
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
        let simple = matches!(self.1.style, Style::Simple);
        macro_rules! object_cardinality {
            ($name:literal, $n:ident, $ope:ident, $bce:ident, $self:ident, $f:ident) => {
                match $bce.as_ref() {
                    ClassExpression::Class(cls)
                        if cls.0.as_ref() == crate::vocab::OWL::Thing.as_ref()
                            && !matches!($self.1.style, Style::Simple) =>
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
                        if dt.0.as_ref() == crate::vocab::OWL2Datatype::Literal.as_ref()
                            && !matches!($self.1.style, Style::Simple) =>
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
            // A single-operand intersection/union is just that operand --
            // the OFN grammar requires >= 2, so wrapping it verbatim would
            // write output its own reader rejects (#235). [`Style::Simple`]
            // writes it as it stands.
            ObjectIntersectionOf(classes) if classes.len() == 1 && !simple => {
                Functional(&classes[0], self.1, None).fmt(f)
            }
            ObjectIntersectionOf(classes) => {
                write!(
                    f,
                    "ObjectIntersectionOf({})",
                    Functional(classes, self.1, None)
                )
            }
            ObjectUnionOf(classes) if classes.len() == 1 && !simple => {
                Functional(&classes[0], self.1, None).fmt(f)
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
        let simple = matches!(self.1.style, Style::Simple);
        match self.0 {
            Datatype(dt) => Functional(dt, self.1, None).fmt(f),
            // As for classes, a single-operand intersection or union is just
            // that operand, but as it stands in [`Style::Simple`].
            DataIntersectionOf(dts) if dts.len() == 1 && !simple => Functional(&dts[0], self.1, None).fmt(f),
            DataUnionOf(dts) if dts.len() == 1 && !simple => Functional(&dts[0], self.1, None).fmt(f),
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
        match self.1.style {
            Style::Simple => {
                let iri: &str = self.0.f.meta().as_ref();
                let name = iri.rsplit_once('#').map_or(iri, |(_, name)| name);
                write!(f, "facetRestriction({name} {})", Functional(&self.0.l, self.1, None))
            }
            Style::Document | Style::Named(_) => write!(
                f,
                "{} {}",
                Functional::<Facet, String>(&self.0.f, self.1, None),
                Functional(&self.0.l, self.1, None)
            ),
        }
    }
}

impl<A: ForIRI> AsFunctional<A> for FacetRestriction<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Functional<'_, HasKey<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        f.write_str("HasKey(")?;
        if let Some(annotations) = self.2 {
            write!(f, "{} ", Functional(annotations, self.1, None))?;
        }
        write!(f, "{} ", Functional(&self.0.ce, self.1, None))?;

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

impl<A: ForIRI> Display for Functional<'_, IRI<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        if let Some(prefixes) = self.1.prefixes {
            // Longest-valid-match abbreviation (OWLAPI semantics), not
            // `curie::shrink_iri`'s first-declared match — so `obo:` and a more
            // specific `uberon:` can both be declared and each IRI abbreviates to
            // its most specific valid CURIE, falling back to the full IRI when
            // none is valid.
            if let Some((prefix, local)) = super::shrink_valid(prefixes, self.0.as_ref()) {
                return write!(f, "{prefix}:{local}");
            }
        }
        write!(f, "<{}>", percent_encode_iri(self.0))
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
                // `xsd:string` is the datatype a bare quoted literal already
                // denotes in OWL 2, and OWLAPI's functional renderer leaves it
                // implicit — ROBOT's own functional output of an OBO-parsed
                // ontology, whose literals are all `OWLLiteralImplString`, carries
                // no `^^xsd:string` at all. Writing it out would also preserve a
                // distinction across the file that OWLAPI loses there, which is
                // not the same document.
                let typed = match self.1.style {
                    Style::Document => write_xsd_string(),
                    Style::Simple => true,
                    Style::Named(_) => false,
                };
                if datatype_iri.as_ref() != XSD_STRING || typed {
                    write!(f, "^^{}", Functional(datatype_iri, self.1, None))?;
                }
                Ok(())
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
        // OWLAPI separates the rule's annotations and each atom with a space, and
        // writes `Body(…)Head(…)` adjacent:
        //
        //     DLSafeRule(Annotation(…) Body(ClassAtom(…) ObjectPropertyAtom(…))Head(…))
        //
        // Everything here ran together, which put every SWRL rule in OBA's
        // `imports/merged_import.owl` a byte off ROBOT's.
        if let Some(annotations) = self.2 {
            write!(f, "DLSafeRule({} ", Functional(annotations, self.1, None))?;
        } else {
            write!(f, "DLSafeRule(")?;
        }

        // An atom is never the entity of a frame, so a two-atom body or head is
        // written in the opposite order to the one stored ([`written_order`]).
        // UBERON's three rules are the visible case: the RDF list in
        // `mirror/uberon.owl` runs `BFO_0000050(x,y)`, `BSPO_0000120(y,z)`, and the
        // body is written `Body(BSPO_0000120(y,z) BFO_0000050(x,y))`. Reading that
        // back and writing it again swaps it once more: the order is the writer's,
        // not the model's.
        let simple = matches!(self.1.style, Style::Simple);
        let write_atoms = |f: &mut Formatter<'_>, atoms: &[crate::model::Atom<A>]| {
            let order: Vec<usize> = if simple {
                (0..atoms.len()).collect()
            } else {
                written_order(atoms.len(), false)
            };
            for (i, &ix) in order.iter().enumerate() {
                if i > 0 {
                    f.write_char(' ')?;
                }
                Functional(&atoms[ix], self.1, None).fmt(f)?;
            }
            Ok::<(), Error>(())
        };

        f.write_str("Body(")?;
        write_atoms(f, &self.0.body)?;
        f.write_char(')')?;
        if simple {
            f.write_char(' ')?;
        }

        f.write_str("Head(")?;
        write_atoms(f, &self.0.head)?;
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
        f.write_str("Variable(")?;
        entity_iri(&self.0.0, "Class", self.1, f)?;
        f.write_str(")")
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

    /// The two renderings of one object OWL API gives beside a document's:
    /// `toString()`, and the functional renderer outside any frame naming
    /// every entity and object IRI by a function.
    #[test]
    fn each_style_renders_an_object_as_its_renderer_does() {
        let build = Build::new_rc();
        let ex = |name: &str| format!("http://example.org/s#{name}");
        let mut prefixes = PrefixMapping::default();
        prefixes.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#").unwrap();
        prefixes.add_prefix("owl", "http://www.w3.org/2002/07/owl#").unwrap();
        prefixes.add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#").unwrap();
        let names = |iri: &str| format!("<{}>[{}]", iri, iri.rsplit('#').next().unwrap());
        let render = |c: &Component<RcStr>, style: Style<'_>| {
            AnnotatedComponent { component: c.clone(), ann: BTreeSet::new() }
                .as_functional_styled(&prefixes, style)
                .to_string()
        };
        let var = |n: &str| IArgument::Variable(Variable(build.iri(ex(n))));

        let rule: Component<RcStr> = Rule {
            body: vec![
                Atom::ClassAtom { pred: ClassExpression::Class(build.class(ex("A"))), arg: var("v") },
                Atom::BuiltInAtom {
                    pred: build.iri(ex("gt")),
                    args: vec![DArgument::Literal(Literal::Datatype {
                        literal: "5".into(),
                        datatype_iri: build.iri("http://www.w3.org/2001/XMLSchema#integer"),
                    })],
                },
            ],
            head: vec![
                Atom::ClassAtom { pred: ClassExpression::Class(build.class(ex("B"))), arg: var("v") },
                Atom::SameIndividualAtom(var("v"), var("w")),
            ],
        }
        .into();
        assert_eq!(
            render(&rule, Style::Simple),
            "DLSafeRule(Body(ClassAtom(<http://example.org/s#A> Variable(<http://example.org/s#v>)) \
             BuiltInAtom(<http://example.org/s#gt> \"5\"^^xsd:integer)) \
             Head(ClassAtom(<http://example.org/s#B> Variable(<http://example.org/s#v>)) \
             SameAsAtom(Variable(<http://example.org/s#v>) Variable(<http://example.org/s#w>))))"
        );
        assert_eq!(
            render(&rule, Style::Named(&names)),
            "DLSafeRule(Body(BuiltInAtom(<http://example.org/s#gt>[gt] \"5\"^^xsd:integer) \
             ClassAtom(<http://example.org/s#A>[A] Variable(<http://example.org/s#v>[v])))\
             Head(SameIndividualAtom(Variable(<http://example.org/s#v>[v]) Variable(<http://example.org/s#w>[w])) \
             ClassAtom(<http://example.org/s#B>[B] Variable(<http://example.org/s#v>[v]))))"
        );

        let typed = Literal::Datatype {
            literal: "x".into(),
            datatype_iri: build.iri("http://www.w3.org/2001/XMLSchema#string"),
        };
        let restriction: Component<RcStr> = SubClassOf {
            sub: ClassExpression::Class(build.class(ex("A"))),
            sup: ClassExpression::DataSomeValuesFrom {
                dp: build.data_property(ex("d")),
                dr: DataRange::DatatypeRestriction(
                    build.datatype("http://www.w3.org/2001/XMLSchema#integer"),
                    vec![FacetRestriction {
                        f: Facet::MinInclusive,
                        l: Literal::Datatype {
                            literal: "1".into(),
                            datatype_iri: build.iri("http://www.w3.org/2001/XMLSchema#integer"),
                        },
                    }],
                ),
            },
        }
        .into();
        assert_eq!(
            render(&restriction, Style::Simple),
            "SubClassOf(<http://example.org/s#A> DataSomeValuesFrom(<http://example.org/s#d> \
             DatatypeRestriction(xsd:integer facetRestriction(minInclusive \"1\"^^xsd:integer))))"
        );
        assert_eq!(
            render(&restriction, Style::Named(&names)),
            "SubClassOf(<http://example.org/s#A>[A] DataSomeValuesFrom(<http://example.org/s#d>[d] \
             DatatypeRestriction(<http://www.w3.org/2001/XMLSchema#integer>[integer] \
             xsd:minInclusive \"1\"^^xsd:integer)))"
        );

        let assertion: Component<RcStr> = AnnotationAssertion {
            subject: AnnotationSubject::IRI(build.iri(ex("A"))),
            ann: Annotation {
                ap: build.annotation_property(ex("ap")),
                av: AnnotationValue::Literal(typed),
                ann: BTreeSet::new(),
            },
        }
        .into();
        assert_eq!(
            render(&assertion, Style::Simple),
            "AnnotationAssertion(<http://example.org/s#ap> <http://example.org/s#A> \"x\"^^xsd:string)"
        );
        assert_eq!(render(&assertion, Style::Named(&names)), "AnnotationAssertion(<http://example.org/s#ap>[ap] <http://example.org/s#A>[A] \"x\")");

        let range: Component<RcStr> =
            AnnotationPropertyRange { ap: build.annotation_property(ex("ap")), iri: build.iri(ex("B")) }.into();
        assert_eq!(render(&range, Style::Simple), "AnnotationPropertyRange(<http://example.org/s#ap> <http://example.org/s#B>)");
        assert_eq!(render(&range, Style::Named(&names)), "AnnotationPropertyRange(<http://example.org/s#ap>[ap] <http://example.org/s#B>[B])");

        let same: Component<RcStr> = SameIndividual(vec![
            Individual::Named(build.named_individual(ex("i"))),
            Individual::Named(build.named_individual(ex("j"))),
        ])
        .into();
        assert_eq!(render(&same, Style::Simple), "SameIndividual(<http://example.org/s#i> <http://example.org/s#j>)");
        assert_eq!(render(&same, Style::Document), "SameIndividual(<http://example.org/s#i> <http://example.org/s#j>)");
        assert_eq!(render(&same, Style::Named(&names)), "SameIndividual(<http://example.org/s#j>[j] <http://example.org/s#i>[i])");

        let string_range: Component<RcStr> = AnnotationPropertyRange {
            ap: build.annotation_property(ex("ap")),
            iri: build.iri("http://www.w3.org/2001/XMLSchema#string"),
        }
        .into();
        assert_eq!(
            render(&string_range, Style::Simple),
            "AnnotationPropertyRange(<http://example.org/s#ap> <http://www.w3.org/2001/XMLSchema#string>)"
        );
        assert_eq!(render(&string_range, Style::Document), "AnnotationPropertyRange(<http://example.org/s#ap> xsd:string)");

        let cardinalities: Component<RcStr> = SubClassOf {
            sub: ClassExpression::ObjectMinCardinality {
                n: 1,
                ope: ObjectPropertyExpression::ObjectProperty(build.object_property(ex("p"))),
                bce: Box::new(ClassExpression::Class(build.class("http://www.w3.org/2002/07/owl#Thing"))),
            },
            sup: ClassExpression::DataMaxCardinality {
                n: 2,
                dp: build.data_property(ex("d")),
                dr: DataRange::Datatype(build.datatype("http://www.w3.org/2000/01/rdf-schema#Literal")),
            },
        }
        .into();
        assert_eq!(
            render(&cardinalities, Style::Simple),
            "SubClassOf(ObjectMinCardinality(1 <http://example.org/s#p> owl:Thing) \
             DataMaxCardinality(2 <http://example.org/s#d> rdfs:Literal))"
        );
        assert_eq!(
            render(&cardinalities, Style::Document),
            "SubClassOf(ObjectMinCardinality(1 <http://example.org/s#p>) DataMaxCardinality(2 <http://example.org/s#d>))"
        );
        assert_eq!(
            render(&cardinalities, Style::Named(&names)),
            "SubClassOf(ObjectMinCardinality(1 <http://example.org/s#p>[p]) DataMaxCardinality(2 <http://example.org/s#d>[d]))"
        );

        let one_operand: Component<RcStr> = EquivalentClasses(vec![
            ClassExpression::Class(build.class(ex("A"))),
            ClassExpression::ObjectUnionOf(vec![ClassExpression::Class(build.class(ex("B")))]),
        ])
        .into();
        assert_eq!(
            render(&one_operand, Style::Simple),
            "EquivalentClasses(<http://example.org/s#A> ObjectUnionOf(<http://example.org/s#B>))"
        );
        assert_eq!(
            render(&one_operand, Style::Named(&names)),
            "EquivalentClasses(<http://example.org/s#A>[A] <http://example.org/s#B>[B])"
        );
        let one_range: Component<RcStr> = DataPropertyRange {
            dp: build.data_property(ex("d")),
            dr: DataRange::DataUnionOf(vec![DataRange::Datatype(build.datatype("http://www.w3.org/2001/XMLSchema#integer"))]),
        }
        .into();
        assert_eq!(render(&one_range, Style::Simple), "DataPropertyRange(<http://example.org/s#d> DataUnionOf(xsd:integer))");
        assert_eq!(render(&one_range, Style::Document), "DataPropertyRange(<http://example.org/s#d> xsd:integer)");

        let one_member: Component<RcStr> =
            DifferentIndividuals(vec![Individual::Named(build.named_individual(ex("j")))]).into();
        assert_eq!(render(&one_member, Style::Simple), "DifferentIndividuals(<http://example.org/s#j>)");
        assert_eq!(render(&one_member, Style::Named(&names)), "");
        let one_same: Component<RcStr> = SameIndividual(vec![Individual::Named(build.named_individual(ex("j")))]).into();
        assert_eq!(render(&one_same, Style::Simple), "SameIndividual(<http://example.org/s#j>)");
        assert_eq!(render(&one_same, Style::Document), "");

        let declaration = AnnotatedComponent {
            component: Component::DeclareClass(DeclareClass(build.class(ex("A")))),
            ann: BTreeSet::from([Annotation {
                ap: build.annotation_property(ex("ap")),
                av: AnnotationValue::IRI(build.iri(ex("v"))),
                ann: BTreeSet::new(),
            }]),
        };
        let render_declaration = |style: Style<'_>| declaration.as_functional_styled(&prefixes, style).to_string();
        assert_eq!(
            render_declaration(Style::Named(&names)),
            "Declaration(Annotation(AnnotationProperty(<http://example.org/s#ap>[ap]) Class(<http://example.org/s#v>[v])) \
             Class(<http://example.org/s#A>[A]))"
        );
        assert_eq!(
            render_declaration(Style::Simple),
            "Declaration(Annotation(<http://example.org/s#ap> <http://example.org/s#v>) Class(<http://example.org/s#A>))"
        );
        assert_eq!(
            render_declaration(Style::Document),
            "Declaration(Annotation(<http://example.org/s#ap> <http://example.org/s#v>) Class(<http://example.org/s#A>))"
        );
    }

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
    fn test_ofn_nary_individual_axiom_below_min_arity_is_dropped() {
        // OWL2 functional syntax requires >= 2 operands for
        // DifferentIndividuals/SameIndividual, but real-world RDF can
        // produce a one-member `owl:AllDifferent` (e.g. a degenerate
        // `owl:distinctMembers` list). Writing it verbatim would produce
        // `DifferentIndividuals(<one-iri>)`, which our own reader rejects
        // (grammar requires `Individual{2, }`) -- the axiom must be dropped
        // instead.
        let build = Build::new_arc();
        let i = build.named_individual("http://example.com/i");

        let different = DifferentIndividuals(vec![i.clone().into()]);
        assert_eq!("", format!("{}", different.as_functional()));

        let same = SameIndividual(vec![i.into()]);
        assert_eq!("", format!("{}", same.as_functional()));
    }

    #[test]
    fn test_ofn_nary_individual_axiom_at_min_arity_is_written() {
        let build = Build::new_arc();
        let i1 = build.named_individual("http://example.com/i1");
        let i2 = build.named_individual("http://example.com/i2");

        let different = DifferentIndividuals(vec![i1.into(), i2.into()]);
        assert_eq!(
            "DifferentIndividuals(<http://example.com/i1> <http://example.com/i2>)",
            format!("{}", different.as_functional())
        );
    }

    /// A pair of same individuals whose first member is anonymous stands in no
    /// named member's frame, and is written turned round; a pair led by a named
    /// member, and three or more members, are written as they stand.
    #[test]
    fn test_ofn_same_individual_pair_led_by_anonymous_is_turned() {
        let build = Build::new_arc();
        let named =
            |s: &str| Individual::from(build.named_individual(format!("http://example.com/{s}")));
        let anon = |s: &str| Individual::from(build.anon(s));
        let written =
            |members: Vec<Individual<_>>| format!("{}", SameIndividual(members).as_functional());
        assert_eq!(
            written(vec![anon("_:a"), anon("_:b")]),
            "SameIndividual(_:b _:a)"
        );
        assert_eq!(
            written(vec![anon("_:a"), named("n")]),
            "SameIndividual(<http://example.com/n> _:a)"
        );
        assert_eq!(
            written(vec![named("n"), anon("_:a")]),
            "SameIndividual(<http://example.com/n> _:a)"
        );
        assert_eq!(
            written(vec![anon("_:a"), anon("_:b"), anon("_:c")]),
            "SameIndividual(_:a _:b _:c)"
        );
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

    /// `xsd:string` is the datatype a bare quoted literal already has, so the
    /// writer leaves it implicit unless `set_write_xsd_string` turns it on. The
    /// flag is process-global, so this asserts the default rather than toggling it
    /// underneath whatever else the test binary is running in parallel.
    #[test]
    fn test_ofn_literal_datatype_xsd_string_is_implicit() {
        let build = Build::new_arc();
        let lit = Literal::Datatype {
            literal: String::from("hello"),
            datatype_iri: build.iri("http://www.w3.org/2001/XMLSchema#string"),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(r#""hello""#, &ofn);
    }

    /// Every other datatype is still written out.
    #[test]
    fn test_ofn_literal_datatype() {
        let build = Build::new_arc();
        let lit = Literal::Datatype {
            literal: String::from("42"),
            datatype_iri: build.iri("http://www.w3.org/2001/XMLSchema#integer"),
        };
        let ofn = format!("{}", lit.as_functional());
        assert_eq!(r#""42"^^<http://www.w3.org/2001/XMLSchema#integer>"#, &ofn);
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

    // Regression test for #230: an empty/default CURIE prefix must still
    // abbreviate with the leading colon (`:local`, not bare `local`).
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

    // Regression test for #230: a leftover local part starting with `#`
    // (no trailing separator on the default-prefix IRI) falls back to the
    // full `<IRI>` form instead of emitting an invalid local part.
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

    // Regression test for #148: `eg` is inserted before `egc`, and both are
    // valid OFN syntax, so the validity check alone can't save this --
    // insertion-order-first-match would pick the wrong one.
    #[test]
    fn test_ofn_prefers_longest_matching_prefix() {
        let build = Build::new_arc();
        let mut prefixes = curie::PrefixMapping::default();
        prefixes.add_prefix("eg", "http://example.com/AB").ok();
        prefixes.add_prefix("egc", "http://example.com/ABC").ok();

        let decl = DeclareClass(build.class("http://example.com/ABCDEF"));
        let ofn = format!("{}", decl.as_functional_with_prefixes(&prefixes));
        assert_eq!("Declaration(Class(egc:DEF))", ofn);
    }

    // Regression test for #234: a literal '[' or ']' (legal in an XML
    // attribute, so real-world OWL/XML ontologies contain it, but not legal
    // unescaped in OFN's <...> FullIRI) must be percent-encoded, in both the
    // full-IRI form and a Prefix(name:=<...>) declaration line.
    #[test]
    fn test_ofn_iri_with_illegal_characters_is_percent_encoded() {
        let build = Build::new_arc();

        let decl = DeclareClass(build.class("http://example.org/KB-CH[R]-8-5"));
        let ofn = format!("{}", decl.as_functional());
        assert_eq!(
            "Declaration(Class(<http://example.org/KB-CH%5BR%5D-8-5>))",
            ofn
        );

        let reparsed: Result<(crate::ontology::set::SetOntology<RcStr>, _), _> =
            crate::io::ofn::reader::read(
                &mut std::io::Cursor::new(format!(
                    "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\n{ofn}\n)"
                )),
                Default::default(),
            );
        assert!(reparsed.is_ok(), "reparse failed: {reparsed:?}");

        let mut prefixes = curie::PrefixMapping::default();
        prefixes
            .add_prefix("R", "http://example.org/KB-CH[R]-8-5")
            .ok();
        let rendered = format!(
            "{}",
            Functional::<curie::PrefixMapping, RcStr>(&prefixes, Context { prefixes: None, style: Style::Document, typed: false }, None)
        );
        assert_eq!(
            "Prefix(R:=<http://example.org/KB-CH%5BR%5D-8-5>)\n",
            rendered
        );
    }

    #[test]
    fn test_ofn_single_operand_intersection_and_union_degrade_to_operand() {
        // https://github.com/phillord/horned-owl/issues/235
        // ofn.pest's ClassExpression{2,} requires >= 2 operands, but the RDF
        // reader can build a single-operand ObjectIntersectionOf/ObjectUnionOf
        // from a real-world (if spec-invalid) owl:intersectionOf/unionOf RDF
        // list with only one member -- e.g. the BCS7 corpus file (turtle) has
        // `[] a owl:Class ; rdfs:subClassOf cst:R7_Stage_IV ;
        // owl:intersectionOf ( cst:M1 ) .`. Writing that verbatim as
        // `ObjectIntersectionOf(<...M1>)` produces output the OFN reader's
        // own grammar then rejects. A single-operand intersection/union is
        // just that operand, so the writer should degrade to it directly.
        let build = Build::new_arc();
        let m1 = ClassExpression::Class(build.class("http://ex/M1"));

        let intersection = ClassExpression::ObjectIntersectionOf(vec![m1.clone()]);
        let ofn = format!("{}", intersection.as_functional());
        assert_eq!("<http://ex/M1>", ofn);

        let union = ClassExpression::ObjectUnionOf(vec![m1]);
        let ofn = format!("{}", union.as_functional());
        assert_eq!("<http://ex/M1>", ofn);

        let sub_class_of = SubClassOf {
            sup: build.class("http://ex/R7_Stage_IV").into(),
            sub: union,
        };
        let ofn = format!("{}", sub_class_of.as_functional());
        let reparsed: Result<(crate::ontology::set::SetOntology<RcStr>, _), _> =
            crate::io::ofn::reader::read(
                &mut std::io::Cursor::new(format!(
                    "Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\n{ofn}\n)"
                )),
                Default::default(),
            );
        assert!(reparsed.is_ok(), "reparse failed: {reparsed:?}");

        // So is a single-operand data intersection or union; an empty one
        // keeps its keyword.
        let integer = DataRange::Datatype(build.datatype("http://www.w3.org/2001/XMLSchema#integer"));
        let union = DataRange::DataUnionOf(vec![integer.clone()]);
        assert_eq!("<http://www.w3.org/2001/XMLSchema#integer>", format!("{}", union.as_functional()));
        let intersection = DataRange::DataIntersectionOf(vec![integer]);
        assert_eq!("<http://www.w3.org/2001/XMLSchema#integer>", format!("{}", intersection.as_functional()));
        assert_eq!("DataUnionOf()", format!("{}", DataRange::<RcStr>::DataUnionOf(vec![]).as_functional()));
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
