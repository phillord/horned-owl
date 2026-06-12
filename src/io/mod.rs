//! Parsers and renderers for several of the ontology formats listed in the
//! [W3C recommendation](https://www.w3.org/TR/owl2-overview/#Syntaxes).

pub mod ofn;
pub mod omn;
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
    OMN,
}

#[allow(clippy::large_enum_variant)]
pub enum ParserOutput<A: ForIRI, AA: ForIndex<A>> {
    OFNParser(SetOntology<A>, PrefixMapping),
    OWXParser(SetOntology<A>, PrefixMapping),
    RDFParser(ConcreteRDFOntology<A, AA>, IncompleteParse<A>),
    OMNParser(SetOntology<A>, PrefixMapping),
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

    pub fn omn(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OMNParser(sop.0, sop.1)
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
    pub format: Option<oxrdfio::RdfFormat>,
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
            ParserOutput::RDFParser(o, i) => {
                (o.into(), None, if i.is_complete() { None } else { Some(i) })
            }
            ParserOutput::OMNParser(o, m) => (o, Some(m), None),
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ParserOutput<A, AA>> for SetOntology<A> {
    fn from(p: ParserOutput<A, AA>) -> SetOntology<A> {
        match p {
            ParserOutput::OFNParser(so, _) => so,
            ParserOutput::OWXParser(so, _) => so,
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
            ParserOutput::OMNParser(so, _) => so,
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ParserOutput<A, AA>> for ComponentMappedOntology<A, AA> {
    fn from(p: ParserOutput<A, AA>) -> ComponentMappedOntology<A, AA> {
        match p {
            ParserOutput::OFNParser(so, _) => so.into(),
            ParserOutput::OWXParser(so, _) => so.into(),
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
            ParserOutput::OMNParser(so, _) => so.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn omn_parser_output_constructs_and_decomposes() {
        use super::*;
        use crate::ontology::set::SetOntology;
        type Idx = std::rc::Rc<crate::model::AnnotatedComponent<std::rc::Rc<str>>>;
        let o = SetOntology::<std::rc::Rc<str>>::new_rc();
        let pm = curie::PrefixMapping::default();
        let out: ParserOutput<std::rc::Rc<str>, Idx> = ParserOutput::omn((o, pm));
        assert!(matches!(out, ParserOutput::OMNParser(_, _)));
    }
}
