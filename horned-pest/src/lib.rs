//! The original pest-based readers for OWL Functional Syntax, Manchester
//! Syntax and OBO.
//!
//! These are effectively deprecated: the readers in `horned_owl::io::{ofn,
//! omn, obo}::reader` accept the same languages, are faster, and can stream.
//! This crate is kept as a reference implementation, as the baseline the
//! differential tests compare the main readers against, and for the
//! compile-time syntax check in `horned-macro`.

pub mod obo;
pub mod ofn;
pub mod omn;

use horned_owl::error::{HornedError, Location};
use pest::RuleType;

/// Convert a pest error into a [`HornedError`], keeping its location.
pub(crate) fn pest_error<R: RuleType + 'static>(e: pest::error::Error<R>) -> HornedError {
    let location = match &e.location {
        pest::error::InputLocation::Pos(x) => Location::BytePosition((*x).try_into().unwrap()),
        pest::error::InputLocation::Span((x, y)) => {
            Location::ByteSpan((*x).try_into().unwrap()..(*y).try_into().unwrap())
        }
    };
    HornedError::ParserError(e.into(), location)
}

/// The location of a pest span.
pub(crate) fn span_location(span: pest::Span<'_>) -> Location {
    Location::ByteSpan(span.start().try_into().unwrap()..span.end().try_into().unwrap())
}

#[cfg(test)]
pub(crate) mod test_support {
    use std::io::BufRead;

    use curie::PrefixMapping;
    use horned_owl::model::{RcAnnotatedComponent, RcStr};
    use horned_owl::ontology::component_mapped::ComponentMappedOntology;

    /// Read an OWL/XML document, panicking on failure.
    pub fn read_ok<R: BufRead>(
        bufread: &mut R,
    ) -> (
        ComponentMappedOntology<RcStr, RcAnnotatedComponent>,
        PrefixMapping,
    ) {
        horned_owl::io::owx::reader::read(bufread, Default::default())
            .unwrap_or_else(|e| panic!("Expected ontology, got failure:{e:?}"))
    }
}
