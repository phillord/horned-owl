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

/// The input format to use when parsing an ontology file.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InputFormat {
    /// Detect the format from file content, ignoring the extension.
    Guess,
    OFN,
    OWX,
    OMN,
    /// An RDF-family format. `None` means detect the sub-format from the
    /// extension; `Some` pins a specific serialization.
    Rdf(Option<oxrdfio::RdfFormat>),
}

impl std::str::FromStr for InputFormat {
    type Err = ();

    /// Accepts `"guess"`, `"ofn"`, `"owx"`, `"omn"`, and any extension
    /// recognised by [`oxrdfio::RdfFormat::from_extension`] plus `"owl"`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "guess" => Ok(Self::Guess),
            "ofn" => Ok(Self::OFN),
            "owx" => Ok(Self::OWX),
            "omn" => Ok(Self::OMN),
            "owl" => Ok(Self::Rdf(Some(oxrdfio::RdfFormat::RdfXml))),
            other => oxrdfio::RdfFormat::from_extension(other)
                .map(|f| Self::Rdf(Some(f)))
                .ok_or(()),
        }
    }
}

#[allow(clippy::large_enum_variant)]
pub enum ParserOutput<A: ForIRI, AA: ForIndex<A>> {
    OFNParser(SetOntology<A>, PrefixMapping),
    OWXParser(SetOntology<A>, PrefixMapping),
    RDFParser(ConcreteRDFOntology<A, AA>, IncompleteParse<A>),
    OMNParser(SetOntology<A>, PrefixMapping),
}

impl<A: ForIRI, AA: ForIndex<A>> ParserOutput<A, AA> {
    pub fn resource_type(&self) -> ResourceType {
        match self {
            ParserOutput::OFNParser(..) => ResourceType::OFN,
            ParserOutput::OWXParser(..) => ResourceType::OWX,
            ParserOutput::RDFParser(..) => ResourceType::RDF,
            ParserOutput::OMNParser(..) => ResourceType::OMN,
        }
    }

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

#[derive(Clone, Copy, Debug)]
pub struct ParserConfiguration {
    /// In lax mode, parsers tolerate content that would otherwise be a
    /// parse error -- see individual readers for exactly what this
    /// relaxes -- instead of rejecting it.
    ///
    /// Currently only the RDF and OWX readers consult this flag; the
    /// OFN and OMN readers do not yet have a lax mode, so setting it
    /// has no effect on those formats.
    pub lax: bool,
    /// The maximum number of bytes to read from a single remote
    /// (`http`/`https`) IRI resolution, such as when following an
    /// `owl:imports` closure. Defaults to `u64::MAX` (no limit),
    /// matching the unbounded behaviour of pre-3.x `ureq`. Lower this
    /// if resolving IRIs from untrusted sources where an oversized
    /// response could exhaust memory.
    pub remote_body_limit: u64,
    /// If set, no network access is attempted at all during parsing --
    /// resolving an IRI (e.g. following an `owl:imports` closure) that
    /// isn't available locally fails with an error instead of falling
    /// back to a remote fetch. `remote_body_limit` still bounds a fetch
    /// in size if one happens; this instead prevents one happening at
    /// all. Defaults to `false`.
    pub local_only: bool,
    pub rdf: RDFParserConfiguration,
    pub owx: OWXParserConfiguration,
    /// Override format detection. When set, this takes precedence over the
    /// file extension. `InputFormat::Guess` triggers content sniffing.
    pub input_format: Option<InputFormat>,
}

impl Default for ParserConfiguration {
    fn default() -> Self {
        ParserConfiguration {
            lax: false,
            remote_body_limit: u64::MAX,
            local_only: false,
            rdf: RDFParserConfiguration::default(),
            owx: OWXParserConfiguration::default(),
            input_format: None,
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct OWXParserConfiguration {}

#[derive(Clone, Copy, Debug, Default)]
pub struct RDFParserConfiguration {
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

/// Detect the serialization format of an OWL document from its content.
///
/// The detection logic is adapted from
/// [`horned-roundtrip`](https://github.com/micheldumontier/horned-roundtrip)
/// by Michel Dumontier et al., used under the MIT licence.
///
/// Returns `(ResourceType, rdf_format)` where `rdf_format` is set for RDF
/// variants (RDF/XML, Turtle, N-Triples) and `None` for OWX, OFN, and OMN.
/// Returns `None` when the format cannot be determined from the content.
pub fn detect_format(bytes: &[u8]) -> Option<(ResourceType, Option<oxrdfio::RdfFormat>)> {
    let s = String::from_utf8_lossy(bytes);
    let s = s.strip_prefix('\u{feff}').unwrap_or(&s);
    let trimmed = s.trim_start();

    if trimmed.starts_with('<') {
        // N-Triples / full-IRI-subject Turtle: `<iri> <iri>` on the first line.
        if !trimmed.starts_with("<?")
            && !trimmed.starts_with("<!")
            && trimmed.lines().next().is_some_and(|l| l.contains("> <"))
        {
            return Some((ResourceType::RDF, Some(oxrdfio::RdfFormat::Turtle)));
        }
        // XML: sniff the root element name.
        if let Some(root) = first_xml_element(trimmed) {
            let local = root.rsplit(':').next().unwrap_or(root);
            if local.eq_ignore_ascii_case("RDF") {
                return Some((ResourceType::RDF, Some(oxrdfio::RdfFormat::RdfXml)));
            }
            if local.eq_ignore_ascii_case("Ontology") {
                return Some((ResourceType::OWX, None));
            }
        }
        return None;
    }

    // Text-syntax formats: skip blank lines and `#` comments.
    for line in trimmed.lines() {
        let l = line.trim_start();
        if l.is_empty() || l.starts_with('#') {
            continue;
        }
        let lower = l.to_ascii_lowercase();
        if lower.starts_with("@prefix") || lower.starts_with("@base") {
            return Some((ResourceType::RDF, Some(oxrdfio::RdfFormat::Turtle)));
        }
        if l.starts_with('<') && !l.starts_with("<?") && !l.starts_with("<!") && l.contains("> <") {
            return Some((ResourceType::RDF, Some(oxrdfio::RdfFormat::Turtle)));
        }
        if l.starts_with("Prefix:") || l.starts_with("Ontology:") {
            return Some((ResourceType::OMN, None));
        }
        if l.starts_with("Prefix(") || l.starts_with("Ontology(") {
            return Some((ResourceType::OFN, None));
        }
        break;
    }
    None
}

fn first_xml_element(s: &str) -> Option<&str> {
    let mut rest = s;
    loop {
        let lt = rest.find('<')?;
        rest = &rest[lt..];
        if rest.starts_with("<?") {
            rest = &rest[rest.find("?>")? + 2..];
            continue;
        }
        if rest.starts_with("<!--") {
            rest = &rest[rest.find("-->")? + 3..];
            continue;
        }
        if rest.starts_with("<!") {
            rest = &rest[rest.find('>')? + 1..];
            continue;
        }
        let name = rest[1..]
            .split(|c: char| c.is_whitespace() || c == '>' || c == '/')
            .next()?;
        return Some(name);
    }
}

#[cfg(test)]
mod tests {
    use std::{os::unix::fs::PermissionsExt, path::PathBuf};

    #[test]
    fn detect_format_rdf_xml() {
        let (rt, fmt) = super::detect_format(b"<?xml version=\"1.0\"?>\n<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">").unwrap();
        assert!(matches!(rt, super::ResourceType::RDF));
        assert_eq!(fmt, Some(oxrdfio::RdfFormat::RdfXml));
    }

    #[test]
    fn detect_format_owl_xml() {
        let (rt, fmt) = super::detect_format(
            b"<?xml version=\"1.0\"?>\n<Ontology xmlns=\"http://www.w3.org/2002/07/owl#\">",
        )
        .unwrap();
        assert!(matches!(rt, super::ResourceType::OWX));
        assert_eq!(fmt, None);
    }

    #[test]
    fn detect_format_turtle() {
        let (rt, fmt) =
            super::detect_format(b"@prefix owl: <http://www.w3.org/2002/07/owl#> .\n").unwrap();
        assert!(matches!(rt, super::ResourceType::RDF));
        assert_eq!(fmt, Some(oxrdfio::RdfFormat::Turtle));
    }

    #[test]
    fn detect_format_ntriples() {
        let (rt, fmt) =
            super::detect_format(b"<http://ex/s> <http://ex/p> <http://ex/o> .\n").unwrap();
        assert!(matches!(rt, super::ResourceType::RDF));
        assert_eq!(fmt, Some(oxrdfio::RdfFormat::Turtle));
    }

    #[test]
    fn detect_format_ofn() {
        let (rt, fmt) =
            super::detect_format(b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>)").unwrap();
        assert!(matches!(rt, super::ResourceType::OFN));
        assert_eq!(fmt, None);
    }

    #[test]
    fn detect_format_omn() {
        let (rt, fmt) =
            super::detect_format(b"Prefix: : <http://ex/>\nOntology: <http://ex/o>").unwrap();
        assert!(matches!(rt, super::ResourceType::OMN));
        assert_eq!(fmt, None);
    }

    #[test]
    fn detect_format_bom_and_comments() {
        let (rt, _) = super::detect_format("\u{feff}Ontology: <http://ex/o>".as_bytes()).unwrap();
        assert!(matches!(rt, super::ResourceType::OMN));

        let (rt, _) = super::detect_format(b"# a comment\n@prefix : <http://ex/> .").unwrap();
        assert!(matches!(rt, super::ResourceType::RDF));
    }

    #[test]
    fn detect_format_unknown() {
        assert!(super::detect_format(b"format-version: 1.4\n[Term]").is_none());
    }

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
        use std::sync::OnceLock;

        static BUBO_PATH: OnceLock<PathBuf> = OnceLock::new();

        BUBO_PATH
            .get_or_init(|| {
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
            })
            .clone()
    }

    pub fn run_bubo_reparse<F>(format: &str, parse_fn: F) -> Result<(), Box<dyn std::error::Error>>
    where
        F: Fn(&std::path::Path, &mut dyn std::io::Write),
    {
        use std::fs::{File, create_dir_all, read_dir, remove_dir_all};
        use std::io::{BufWriter, Write};
        use std::path::Path;

        let src_dir = format!("./src/ont/{format}");
        let tmp_dir = format!("./tmp/{format}");

        create_dir_all(&tmp_dir)?;

        for entry in read_dir(&src_dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_file() {
                let out_file = File::create(Path::new(&tmp_dir).join(path.file_name().unwrap()))?;
                let mut buf_writer = BufWriter::new(out_file);
                parse_fn(&path, &mut buf_writer);
                buf_writer.flush()?;
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
