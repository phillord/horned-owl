//! A streamable representation of an ontology: [`StreamComponent`] and the
//! [`StreamOntology`] trait it flows through.
//!
//! [`Ontology`](crate::model::Ontology) already streams for free via its
//! `IntoIterator` supertrait -- the one thing it can't carry is
//! [`PrefixMapping`], since that lives outside the `Component` model
//! entirely. [`StreamComponent`] widens the item type to cover both.

use crate::model::ForIRI;
use crate::ontology::indexed::ForIndex;

/// One item streamed out of an ontology document: either a logical
/// component (axiom, annotation, `OntologyID`, `DocIRI`, `Import`, ...) or a
/// `(prefix_name, iri)` pair, e.g. OWL/XML's `<Prefix name="owl"
/// IRI="..."/>`, OFN's `Prefix(owl:=<...>)`, OMN's `Prefix: owl: <...>`,
/// OBO's `idspace: OWL ...`.
///
/// Generic over `AA` alone, not `AA: ForIndex<A>` -- the relationship to `A`
/// belongs on whoever actually uses it (`StreamOntology`, below), not on
/// this type itself, which never calls a `ForIndex` method.
#[derive(Clone, Debug, PartialEq)]
pub enum StreamComponent<AA> {
    Component(AA),
    Prefix(String, String),
}

/// A lazily-produced ontology: something that yields [`StreamComponent`]s
/// one at a time instead of materializing an
/// [`Ontology`](crate::model::Ontology). Blanket-implemented for any
/// matching iterator, so a format's own concrete reader satisfies it
/// directly with no wrapping.
pub trait StreamOntology<A: ForIRI, AA: ForIndex<A>>:
    Iterator<Item = crate::io::Result<StreamComponent<AA>>>
{
}

impl<A, AA, T> StreamOntology<A, AA> for T
where
    A: ForIRI,
    AA: ForIndex<A>,
    T: Iterator<Item = crate::io::Result<StreamComponent<AA>>>,
{
}
