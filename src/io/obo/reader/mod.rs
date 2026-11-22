//! An OBO reader written with hand-written parsers.
//!
//! It accepts the same language as the original pest reader (the `horned-pest` crate) and
//! builds the same components, but parses one stanza at a time, so it can
//! stream. See [`Reader`].
use std::io::BufRead;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::io::StreamComponent;
use crate::model::*;

mod convert;
mod stream;
mod syntax;
#[cfg(test)]
mod tests;

pub use stream::{Reader, read_to_stream};

/// Read an ontology in OBO format.
///
/// The `[Typedef]` frames are read first, as the pest reader does, so a
/// relation may be used before the frame that defines it.
pub fn read<
    A: ForIRI,
    B: AsRef<Build<A>>,
    O: MutableOntology<A> + Ontology<A> + Default,
    R: BufRead,
>(
    mut bufread: R,
    config: crate::io::ParserConfiguration<A, B>,
) -> Result<(O, PrefixMapping), HornedError> {
    // Lenient by default, as the pest reader: a stray non-UTF-8 byte does
    // not abort the read.
    let mut bytes = Vec::new();
    bufread.read_to_end(&mut bytes)?;
    let doc = String::from_utf8_lossy(&bytes);
    let typedefs = stream::prescan(&doc);
    let mut reader = Reader::new(doc.as_bytes(), config).with_typedefs(typedefs);
    let mut ont = O::default();
    for item in reader.by_ref() {
        if let StreamComponent::Component(ac) = item? {
            ont.insert(ac);
        }
    }
    Ok((ont, reader.into_prefixes()))
}
