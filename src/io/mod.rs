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
    use std::{os::unix::fs::PermissionsExt, path::PathBuf};

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

    // Ensure bubo exists in the dev location during tests
    pub fn bubo_ensure() -> std::path::PathBuf {
        let local = PathBuf::from("dev/bubo-0.4.0");

        if !local.exists() {
            println!("Downloading bubo 0.4.0 from GitHub...");
            let status = std::process::Command::new("wget")
                .args([
                    "https://github.com/phillord/tawny-bubo/releases/download/0.4.0/bubo-0.4.0",
                    "-O",
                    "dev/bubo-0.4.0",
                ])
                .status()
                .expect("failed to run wget");
            assert!(status.success(), "failed to download bubo");

            std::fs::set_permissions(&local, std::fs::Permissions::from_mode(0o755))
                .expect("failed to set bubo executable");
        }

        local
    }

    pub fn run_bubo_reparse<F>(format: &str, parse_fn: F) -> Result<(), Box<dyn std::error::Error>>
    where
        F: Fn(&std::path::Path),
    {
        use std::fs::{create_dir_all, read_dir, remove_dir_all};

        let src_dir = format!("./src/ont/{format}");
        let tmp_dir = format!("./tmp/{format}");

        create_dir_all(&tmp_dir)?;

        for entry in read_dir(&src_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() {
                parse_fn(&path);
            }
        }

        let bubo = bubo_ensure();
        let output = std::process::Command::new("java")
            .arg("-jar")
            .arg(bubo.into_os_string())
            .arg("./dev/reparse-all.clj")
            .arg(format)
            .output()?;

        if !output.status.success() {
            let out = String::from_utf8(output.stdout).unwrap();
            assert!(false, "Bubo reparse failed: {out}");
        }

        remove_dir_all(&tmp_dir)?;
        Ok(())
    }
}
