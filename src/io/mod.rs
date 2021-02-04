//! Parsers and Renders for OWL Ontologies

pub mod owx;
pub mod rdf;


use curie::PrefixMapping;

use crate::ontology
    ::{axiom_mapped::AxiomMappedOntology,
       set::SetOntology};

use self::rdf::reader::{IncompleteParse, RDFOntology};


pub enum ResourceType{OWX, RDF}

pub enum ParserOutput
{
    OWXParser(SetOntology, PrefixMapping),
    RDFParser(RDFOntology, IncompleteParse),
}

impl ParserOutput {
    pub fn decompose(self) -> (SetOntology,
                               Option<PrefixMapping>,
                               Option<IncompleteParse>)
    {
        match self {
            ParserOutput::OWXParser(o, m) => {
                (o, Some(m), None)
            }
            ParserOutput::RDFParser(o, i) => {
                (o.into(), None, Some(i))
            }
        }
    }
}

impl From<(SetOntology, PrefixMapping)> for ParserOutput{
    fn from(sop: (SetOntology, PrefixMapping)) -> ParserOutput {
        ParserOutput::OWXParser(sop.0, sop.1)
    }
}

impl From<(RDFOntology, IncompleteParse)> for ParserOutput{
    fn from(rop: (RDFOntology, IncompleteParse)) -> ParserOutput {
        ParserOutput::RDFParser(rop.0, rop.1)
    }
}

impl From<ParserOutput> for SetOntology {
    fn from(p: ParserOutput) -> SetOntology {
        match p {
            ParserOutput::OWXParser(so, _) => so,
            ParserOutput::RDFParser(rdfo, _) => rdfo.into()
        }
    }
}

impl From<ParserOutput> for AxiomMappedOntology {
    fn from(p: ParserOutput) -> AxiomMappedOntology {
        match p {
            ParserOutput::OWXParser(so, _) => so.into(),
            ParserOutput::RDFParser(rdfo,_) => rdfo.into()
        }
    }
}
