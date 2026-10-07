//! Moved to [`crate::io::closure_reader`], which is no longer RDF only.
//!
//! `#[deprecated]` on a module does not reach items it merely re-exports,
//! so each item is deprecated individually.
#![allow(deprecated)]

use crate::error::HornedError;
use crate::io::IncompleteParse;
use crate::io::RDFParserConfiguration;
use crate::io::rdf::reader::RDFOntology;
use crate::model::Build;
use crate::model::ForIRI;
use crate::model::IRI;
use crate::ontology::indexed::ForIndex;

#[deprecated(note = "use `horned_owl::io::closure_reader::ClosureOntologyParser`")]
pub type ClosureOntologyParser<A, AA, O, B = Build<A>> =
    crate::io::closure_reader::ClosureOntologyParser<A, AA, O, B>;

#[deprecated(note = "use `horned_owl::io::closure_reader::read`")]
#[allow(clippy::type_complexity)]
pub fn read<A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>, B: AsRef<Build<A>> + Clone>(
    iri: &IRI<A>,
    config: RDFParserConfiguration<A, B>,
) -> Result<(O, IncompleteParse<A>), HornedError> {
    crate::io::closure_reader::read(iri, config)
}

#[deprecated(note = "use `horned_owl::io::closure_reader::read_to_closure`")]
#[allow(clippy::type_complexity)]
pub fn read_to_closure<
    A: ForIRI,
    AA: ForIndex<A>,
    O: RDFOntology<A, AA>,
    B: AsRef<Build<A>> + Clone,
>(
    iri: &IRI<A>,
    config: RDFParserConfiguration<A, B>,
) -> Result<Vec<(O, IncompleteParse<A>)>, HornedError> {
    crate::io::closure_reader::read_to_closure(iri, config)
}
