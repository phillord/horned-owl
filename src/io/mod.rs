//! Parsers and Renders for OWL Ontologies

pub mod owx;
pub mod rdf;

use curie::PrefixMapping;

use crate::{ontology::{axiom_mapped::AxiomMappedOntology, set::SetOntology}, model::ForIRI};
use crate::ontology::indexed::ForIndex;
use self::rdf::reader::{IncompleteParse, RDFOntology};

pub enum ResourceType {
    OWX,
    RDF,
}

pub enum ParserOutput<A: ForIRI, AA: ForIndex<A>> {
    OWXParser(SetOntology<A>, PrefixMapping),
    RDFParser(RDFOntology<A, AA>, IncompleteParse<A>),
}

impl<A: ForIRI, AA: ForIndex<A>> ParserOutput<A, AA> {
    pub fn decompose(self) -> (SetOntology<A>, Option<PrefixMapping>, Option<IncompleteParse<A>>) {
        match self {
            ParserOutput::OWXParser(o, m) => (o, Some(m), None),
            ParserOutput::RDFParser(o, i) => (o.into(), None, Some(i)),
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<(SetOntology<A>, PrefixMapping)> for ParserOutput<A, AA> {
    fn from(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OWXParser(sop.0, sop.1)
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<(RDFOntology<A, AA>, IncompleteParse<A>)> for ParserOutput<A, AA> {
    fn from(rop: (RDFOntology<A, AA>, IncompleteParse<A>)) -> ParserOutput<A, AA> {
        ParserOutput::RDFParser(rop.0, rop.1)
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ParserOutput<A, AA>> for SetOntology<A> {
    fn from(p: ParserOutput<A, AA>) -> SetOntology<A> {
        match p {
            ParserOutput::OWXParser(so, _) => so,
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
        }
    }
}

impl<A: ForIRI, AA:ForIndex<A>> From<ParserOutput<A, AA>> for AxiomMappedOntology<A, AA> {
    fn from(p: ParserOutput<A, AA>) -> AxiomMappedOntology<A, AA> {
        match p {
            ParserOutput::OWXParser(so, _) => so.into(),
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
        }
    }
}
