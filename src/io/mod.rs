//! Parsers and renderers for several of the ontology formats listed in the
//! [W3C recommendation](https://www.w3.org/TR/owl2-overview/#Syntaxes).

pub mod ofn;
pub mod owx;
pub mod rdf;

use curie::PrefixMapping;

use self::rdf::reader::{ConcreteRDFOntology, IncompleteParse};
use crate::ontology::indexed::ForIndex;
use crate::{
    model::ForIRI,
    ontology::{component_mapped::ComponentMappedOntology, set::SetOntology},
};

pub enum ResourceType {
    OFN,
    OWX,
    RDF,
}

pub enum ParserOutput<A: ForIRI, AA: ForIndex<A>> {
    OFNParser(SetOntology<A>, PrefixMapping),
    OWXParser(SetOntology<A>, PrefixMapping),
    RDFParser(ConcreteRDFOntology<A, AA>, IncompleteParse<A>),
}

impl<A: ForIRI, AA: ForIndex<A>> ParserOutput<A, AA> {
    pub fn ofn(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OFNParser(sop.0, sop.1)
    }

    pub fn owx(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OWXParser(sop.0, sop.1)
    }

    pub fn rdf(rop: (ConcreteRDFOntology<A, AA>, IncompleteParse<A>)) -> ParserOutput<A, AA> {
        ParserOutput::RDFParser(rop.0, rop.1)
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct ParserConfiguration {
    // Shared Config will go here
    pub rdf: RDFParserConfiguration,
    pub owx: OWXParserConfiguration,
}

#[derive(Clone, Copy, Debug, Default)]
pub struct OWXParserConfiguration {}

#[derive(Clone, Copy, Debug, Default)]
pub struct RDFParserConfiguration {
    pub lax: bool,
}

impl<A: ForIRI, AA: ForIndex<A>> ParserOutput<A, AA> {
    pub fn decompose(
        self,
    ) -> (
        SetOntology<A>,
        Option<PrefixMapping>,
        Option<IncompleteParse<A>>,
    ) {
        match self {
            ParserOutput::OFNParser(o, m) => (o, Some(m), None),
            ParserOutput::OWXParser(o, m) => (o, Some(m), None),
            ParserOutput::RDFParser(o, i) => (o.into(), None, Some(i)),
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ParserOutput<A, AA>> for SetOntology<A> {
    fn from(p: ParserOutput<A, AA>) -> SetOntology<A> {
        match p {
            ParserOutput::OFNParser(so, _) => so,
            ParserOutput::OWXParser(so, _) => so,
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ParserOutput<A, AA>> for ComponentMappedOntology<A, AA> {
    fn from(p: ParserOutput<A, AA>) -> ComponentMappedOntology<A, AA> {
        match p {
            ParserOutput::OFNParser(so, _) => so.into(),
            ParserOutput::OWXParser(so, _) => so.into(),
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
        }
    }
}
