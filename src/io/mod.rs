//! Parsers and Renders for OWL Ontologies

pub mod owx;
pub mod rdf;

use curie::PrefixMapping;

use crate::{ontology::{axiom_mapped::AxiomMappedOntology, set::SetOntology}, model::ForIRI};

use self::rdf::reader::{IncompleteParse, RDFOntology};

pub enum ResourceType {
    OWX,
    RDF,
}

pub enum ParserOutput<A: ForIRI> {
    OWXParser(SetOntology<A>, PrefixMapping),
    RDFParser(RDFOntology<A>, IncompleteParse<A>),
}

impl<A: ForIRI> ParserOutput<A> {
    pub fn decompose(self) -> (SetOntology<A>, Option<PrefixMapping>, Option<IncompleteParse<A>>) {
        match self {
            ParserOutput::OWXParser(o, m) => (o, Some(m), None),
            ParserOutput::RDFParser(o, i) => (o.into(), None, Some(i)),
        }
    }
}

impl<A: ForIRI> From<(SetOntology<A>, PrefixMapping)> for ParserOutput<A> {
    fn from(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A> {
        ParserOutput::OWXParser(sop.0, sop.1)
    }
}

impl<A: ForIRI> From<(RDFOntology<A>, IncompleteParse<A>)> for ParserOutput<A> {
    fn from(rop: (RDFOntology<A>, IncompleteParse<A>)) -> ParserOutput<A> {
        ParserOutput::RDFParser(rop.0, rop.1)
    }
}

impl<A: ForIRI> From<ParserOutput<A>> for SetOntology<A> {
    fn from(p: ParserOutput<A>) -> SetOntology<A> {
        match p {
            ParserOutput::OWXParser(so, _) => so,
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
        }
    }
}

impl<A: ForIRI> From<ParserOutput<A>> for AxiomMappedOntology<A> {
    fn from(p: ParserOutput<A>) -> AxiomMappedOntology<A> {
        match p {
            ParserOutput::OWXParser(so, _) => so.into(),
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
        }
    }
}
