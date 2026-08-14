//! Parsers and renderers for several of the ontology formats listed in the
//! [W3C recommendation](https://www.w3.org/TR/owl2-overview/#Syntaxes).

#[cfg(feature = "obo")]
pub mod obo;
#[cfg(feature = "ofn")]
pub mod ofn;
#[cfg(feature = "omn")]
pub mod omn;
#[cfg(feature = "owx")]
pub mod owx;
#[cfg(feature = "rdf")]
pub mod rdf;

use curie::PrefixMapping;

#[cfg(feature = "rdf")]
use self::rdf::reader::{ConcreteRDFOntology, IncompleteParse};
use crate::ontology::indexed::ForIndex;
use crate::{
    model::ForIRI,
    ontology::{component_mapped::ComponentMappedOntology, set::SetOntology},
};

/// Shrink `iri` against `mapping`, preferring the *longest* matching named
/// prefix, unlike [`curie::PrefixMapping::shrink_iri`]'s insertion-order
/// first-match. Matches OWL API's convention (#148).
///
/// The default prefix's value isn't exposed by `curie::PrefixMapping`, so it
/// can't be length-compared here; any named-prefix match wins over it, and
/// it's used only as a fallback when no named prefix matches.
pub(crate) fn shrink_iri_longest_match<'a>(
    mapping: &'a PrefixMapping,
    iri: &'a str,
) -> Option<curie::Curie<'a>> {
    mapping
        .mappings()
        .filter_map(|(name, value)| {
            iri.strip_prefix(value.as_str())
                .map(|local| (name.as_str(), value.len(), local))
        })
        .max_by_key(|(_, len, _)| *len)
        .map(|(name, _, local)| curie::Curie::new(Some(name), local))
        .or_else(|| mapping.shrink_iri(iri).ok())
}

pub enum ResourceType {
    #[cfg(feature = "ofn")]
    OFN,
    #[cfg(feature = "owx")]
    OWX,
    #[cfg(feature = "rdf")]
    RDF,
    #[cfg(feature = "omn")]
    OMN,
    #[cfg(feature = "obo")]
    OBO,
}

/// The input format to use when parsing an ontology file.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InputFormat {
    /// Detect the format from file content, ignoring the extension.
    Guess,
    #[cfg(feature = "ofn")]
    OFN,
    #[cfg(feature = "owx")]
    OWX,
    #[cfg(feature = "omn")]
    OMN,
    #[cfg(feature = "obo")]
    OBO,
    /// An RDF-family format. `None` means detect the sub-format from the
    /// extension; `Some` pins a specific serialization.
    #[cfg(feature = "rdf")]
    Rdf(Option<oxrdfio::RdfFormat>),
}

impl std::str::FromStr for InputFormat {
    type Err = ();

    /// Accepts `"guess"`, `"ofn"`, `"owx"`, `"omn"`, `"obo"`, and any extension
    /// recognised by [`oxrdfio::RdfFormat::from_extension`] plus `"owl"`
    /// (whichever of these this build's features actually support).
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "guess" => Ok(Self::Guess),
            #[cfg(feature = "ofn")]
            "ofn" => Ok(Self::OFN),
            #[cfg(feature = "owx")]
            "owx" => Ok(Self::OWX),
            #[cfg(feature = "omn")]
            "omn" => Ok(Self::OMN),
            #[cfg(feature = "obo")]
            "obo" => Ok(Self::OBO),
            #[cfg(feature = "rdf")]
            "owl" => Ok(Self::Rdf(Some(oxrdfio::RdfFormat::RdfXml))),
            #[cfg(feature = "rdf")]
            other => oxrdfio::RdfFormat::from_extension(other)
                .map(|f| Self::Rdf(Some(f)))
                .ok_or(()),
            #[cfg(not(feature = "rdf"))]
            _ => Err(()),
        }
    }
}

#[allow(clippy::large_enum_variant)]
pub enum ParserOutput<A: ForIRI, AA: ForIndex<A>> {
    #[cfg(feature = "ofn")]
    OFNParser(SetOntology<A>, PrefixMapping),
    #[cfg(feature = "owx")]
    OWXParser(SetOntology<A>, PrefixMapping),
    #[cfg(feature = "rdf")]
    RDFParser(ConcreteRDFOntology<A, AA>, IncompleteParse<A>),
    #[cfg(feature = "omn")]
    OMNParser(SetOntology<A>, PrefixMapping),
    #[cfg(feature = "obo")]
    OBOParser(SetOntology<A>, PrefixMapping),
    // `AA` (the annotated-component index type) is otherwise only used by
    // RDFParser -- without this, disabling "rdf" alone (or every format at
    // once, the `PhantomData<A>` covering that degenerate case too) would
    // leave a type parameter unused and the type wouldn't compile.
    // Uninhabited (Infallible), so it adds nothing at runtime and can never
    // actually be constructed or matched; it only exists to keep
    // `ParserOutput<A, AA>`'s signature stable across every feature
    // combination, so callers never need to be feature-aware about which
    // formats are enabled.
    #[cfg(not(feature = "rdf"))]
    #[doc(hidden)]
    __Phantom(
        std::convert::Infallible,
        std::marker::PhantomData<AA>,
        std::marker::PhantomData<A>,
    ),
}

impl<A: ForIRI, AA: ForIndex<A>> ParserOutput<A, AA> {
    pub fn resource_type(&self) -> ResourceType {
        match self {
            #[cfg(feature = "ofn")]
            ParserOutput::OFNParser(..) => ResourceType::OFN,
            #[cfg(feature = "owx")]
            ParserOutput::OWXParser(..) => ResourceType::OWX,
            #[cfg(feature = "rdf")]
            ParserOutput::RDFParser(..) => ResourceType::RDF,
            #[cfg(feature = "omn")]
            ParserOutput::OMNParser(..) => ResourceType::OMN,
            #[cfg(feature = "obo")]
            ParserOutput::OBOParser(..) => ResourceType::OBO,
            #[cfg(not(feature = "rdf"))]
            ParserOutput::__Phantom(inf, ..) => match *inf {},
        }
    }

    #[cfg(feature = "ofn")]
    pub fn ofn(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OFNParser(sop.0, sop.1)
    }

    #[cfg(feature = "owx")]
    pub fn owx(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OWXParser(sop.0, sop.1)
    }

    #[cfg(feature = "rdf")]
    pub fn rdf(rop: (ConcreteRDFOntology<A, AA>, IncompleteParse<A>)) -> ParserOutput<A, AA> {
        ParserOutput::RDFParser(rop.0, rop.1)
    }

    #[cfg(feature = "omn")]
    pub fn omn(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OMNParser(sop.0, sop.1)
    }

    #[cfg(feature = "obo")]
    pub fn obo(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OBOParser(sop.0, sop.1)
    }
}

#[derive(Clone, Debug)]
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
    // Gated: `RDFParserConfiguration` names `oxrdfio::RdfFormat` directly,
    // which doesn't exist as a type when "rdf" is off. `owx`'s config below
    // stays unconditional -- it's an empty struct with no such coupling, so
    // there's nothing to gain by threading a second conditional field
    // through every `ParserConfiguration` construction site for symmetry.
    #[cfg(feature = "rdf")]
    pub rdf: RDFParserConfiguration,
    pub owx: OWXParserConfiguration,
    /// Override format detection. When set, this takes precedence over the
    /// file extension. `InputFormat::Guess` triggers content sniffing.
    pub input_format: Option<InputFormat>,
    /// An OASIS XML Catalog (see the [`horned_catalog`] crate) to
    /// consult when resolving an IRI to local content, such as when
    /// following an `owl:imports` closure. Checked before the
    /// heuristic path-guessing in [`crate::resolve::localize_iri`] and
    /// before any remote fallback -- an explicit catalog mapping is a
    /// stronger signal than either. `None` (the default) disables
    /// catalog-based resolution entirely.
    ///
    /// This is an `Rc` rather than a plain `&Catalog` reference so that
    /// `ParserConfiguration` doesn't need a lifetime parameter --
    /// it's cloned (a cheap refcount bump) each time a parse recurses
    /// into an import.
    pub catalog: Option<std::rc::Rc<horned_catalog::Catalog>>,
}

impl Default for ParserConfiguration {
    fn default() -> Self {
        ParserConfiguration {
            lax: false,
            remote_body_limit: u64::MAX,
            local_only: false,
            #[cfg(feature = "rdf")]
            rdf: RDFParserConfiguration::default(),
            owx: OWXParserConfiguration::default(),
            input_format: None,
            catalog: None,
        }
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct OWXParserConfiguration {}

#[cfg(feature = "rdf")]
#[derive(Clone, Copy, Debug, Default)]
pub struct RDFParserConfiguration {
    pub format: Option<oxrdfio::RdfFormat>,
}

// `IncompleteParse` is genuinely RDF-specific (its fields are built from
// rdf::reader's own `Term`/`PosTriple`/`Atom` types), but `decompose()`
// below names it in a signature every format shares. This uninhabited stub
// keeps that signature stable regardless of which formats are enabled: the
// non-RDF match arms in `decompose()` already always return `None` for
// this slot, so nothing ever needs to construct one.
#[cfg(not(feature = "rdf"))]
pub struct IncompleteParse<A: ForIRI>(std::marker::PhantomData<A>, std::convert::Infallible);

impl<A: ForIRI, AA: ForIndex<A>> ParserOutput<A, AA> {
    pub fn decompose(
        self,
    ) -> (
        SetOntology<A>,
        Option<PrefixMapping>,
        Option<IncompleteParse<A>>,
    ) {
        match self {
            #[cfg(feature = "ofn")]
            ParserOutput::OFNParser(o, m) => (o, Some(m), None),
            #[cfg(feature = "owx")]
            ParserOutput::OWXParser(o, m) => (o, Some(m), None),
            #[cfg(feature = "rdf")]
            ParserOutput::RDFParser(o, i) => {
                (o.into(), None, if i.is_complete() { None } else { Some(i) })
            }
            #[cfg(feature = "omn")]
            ParserOutput::OMNParser(o, m) => (o, Some(m), None),
            #[cfg(feature = "obo")]
            ParserOutput::OBOParser(o, m) => (o, Some(m), None),
            #[cfg(not(feature = "rdf"))]
            ParserOutput::__Phantom(inf, ..) => match inf {},
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ParserOutput<A, AA>> for SetOntology<A> {
    fn from(p: ParserOutput<A, AA>) -> SetOntology<A> {
        match p {
            #[cfg(feature = "ofn")]
            ParserOutput::OFNParser(so, _) => so,
            #[cfg(feature = "owx")]
            ParserOutput::OWXParser(so, _) => so,
            #[cfg(feature = "rdf")]
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
            #[cfg(feature = "omn")]
            ParserOutput::OMNParser(so, _) => so,
            #[cfg(feature = "obo")]
            ParserOutput::OBOParser(so, _) => so,
            #[cfg(not(feature = "rdf"))]
            ParserOutput::__Phantom(inf, ..) => match inf {},
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> From<ParserOutput<A, AA>> for ComponentMappedOntology<A, AA> {
    fn from(p: ParserOutput<A, AA>) -> ComponentMappedOntology<A, AA> {
        match p {
            #[cfg(feature = "ofn")]
            ParserOutput::OFNParser(so, _) => so.into(),
            #[cfg(feature = "owx")]
            ParserOutput::OWXParser(so, _) => so.into(),
            #[cfg(feature = "rdf")]
            ParserOutput::RDFParser(rdfo, _) => rdfo.into(),
            #[cfg(feature = "omn")]
            ParserOutput::OMNParser(so, _) => so.into(),
            #[cfg(feature = "obo")]
            ParserOutput::OBOParser(so, _) => so.into(),
            #[cfg(not(feature = "rdf"))]
            ParserOutput::__Phantom(inf, ..) => match inf {},
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
/// variants (RDF/XML, Turtle, N-Triples) and `None` for OWX, OFN, OMN, and OBO.
/// Returns `None` when the format cannot be determined from the content, or
/// when it's recognised but this build's features don't support it.
pub fn detect_format(bytes: &[u8]) -> Option<(ResourceType, Option<DetectedRdfFormat>)> {
    let s = String::from_utf8_lossy(bytes);
    let s = s.strip_prefix('\u{feff}').unwrap_or(&s);
    let trimmed = s.trim_start();

    if trimmed.starts_with('<') {
        // N-Triples / full-IRI-subject Turtle: `<iri> <iri>` on the first line.
        #[cfg(feature = "rdf")]
        if !trimmed.starts_with("<?")
            && !trimmed.starts_with("<!")
            && trimmed.lines().next().is_some_and(|l| l.contains("> <"))
        {
            return Some((ResourceType::RDF, Some(oxrdfio::RdfFormat::Turtle)));
        }
        // XML: sniff the root element name.
        #[cfg(any(feature = "rdf", feature = "owx"))]
        if let Some(root) = first_xml_element(trimmed) {
            let local = root.rsplit(':').next().unwrap_or(root);
            #[cfg(feature = "rdf")]
            if local.eq_ignore_ascii_case("RDF") {
                return Some((ResourceType::RDF, Some(oxrdfio::RdfFormat::RdfXml)));
            }
            #[cfg(feature = "owx")]
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
        #[cfg(feature = "rdf")]
        if lower.starts_with("@prefix") || lower.starts_with("@base") {
            return Some((ResourceType::RDF, Some(oxrdfio::RdfFormat::Turtle)));
        }
        #[cfg(feature = "rdf")]
        if l.starts_with('<') && !l.starts_with("<?") && !l.starts_with("<!") && l.contains("> <") {
            return Some((ResourceType::RDF, Some(oxrdfio::RdfFormat::Turtle)));
        }
        #[cfg(feature = "omn")]
        if l.starts_with("Prefix:") || l.starts_with("Ontology:") {
            return Some((ResourceType::OMN, None));
        }
        #[cfg(feature = "ofn")]
        if l.starts_with("Prefix(") || l.starts_with("Ontology(") {
            return Some((ResourceType::OFN, None));
        }
        // OBO flat-file: the conventional first line is `format-version:`, but a
        // header-less document may open directly with a stanza header.
        #[cfg(feature = "obo")]
        if l.starts_with("format-version:")
            || l.starts_with("[Term]")
            || l.starts_with("[Typedef]")
            || l.starts_with("[Instance]")
        {
            return Some((ResourceType::OBO, None));
        }
        break;
    }
    None
}

/// The RDF sub-format slot in [`detect_format`]'s return type. A real
/// `oxrdfio::RdfFormat` when "rdf" is enabled; otherwise an uninhabited
/// stub, since nothing can ever detect an RDF sub-format without the RDF
/// feature to begin with -- this just keeps `detect_format`'s signature
/// stable across feature combinations rather than changing shape.
#[cfg(feature = "rdf")]
pub type DetectedRdfFormat = oxrdfio::RdfFormat;
#[cfg(not(feature = "rdf"))]
pub type DetectedRdfFormat = std::convert::Infallible;

#[cfg(any(feature = "rdf", feature = "owx"))]
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
    fn detect_format_obo() {
        let (rt, fmt) = super::detect_format(b"format-version: 1.4\n[Term]").unwrap();
        assert!(matches!(rt, super::ResourceType::OBO));
        assert_eq!(fmt, None);

        // A header-less document opening on a stanza is still OBO.
        let (rt, _) = super::detect_format(b"[Term]\nid: GO:0008150\n").unwrap();
        assert!(matches!(rt, super::ResourceType::OBO));
    }

    #[test]
    fn detect_format_unknown() {
        assert!(super::detect_format(b"lorem ipsum dolor\n").is_none());
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
            panic!("Bubo reparse failed: {out}");
        }

        remove_dir_all(&tmp_dir)?;
        Ok(())
    }
}
