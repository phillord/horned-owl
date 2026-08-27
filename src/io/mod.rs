//! Parsers and renderers for several of the ontology formats listed in the
//! [W3C recommendation](https://www.w3.org/TR/owl2-overview/#Syntaxes).

pub mod obo;
pub mod ofn;
pub mod omn;
pub mod owx;
pub mod rdf;
pub mod stream;

use curie::PrefixMapping;

use self::rdf::reader::{ConcreteRDFOntology, IncompleteParse};
use crate::error::HornedError;
use crate::ontology::indexed::ForIndex;
use crate::{
    model::{Build, ForIRI, IRI},
    ontology::{component_mapped::ComponentMappedOntology, set::SetOntology},
};

pub use stream::{StreamComponent, StreamOntology};

/// The result type every IO reader/writer in this module returns.
pub type Result<T> = std::result::Result<T, HornedError>;

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
    OFN,
    OWX,
    RDF,
    OMN,
    OBO,
}

/// The input format to use when parsing an ontology file.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum InputFormat {
    /// Detect the format from file content, ignoring the extension.
    Guess,
    OFN,
    OWX,
    OMN,
    OBO,
    /// An RDF-family format. `None` means detect the sub-format from the
    /// extension; `Some` pins a specific serialization.
    Rdf(Option<oxrdfio::RdfFormat>),
}

impl std::str::FromStr for InputFormat {
    type Err = ();

    /// Accepts `"guess"`, `"ofn"`, `"owx"`, `"omn"`, `"obo"`, and any extension
    /// recognised by [`oxrdfio::RdfFormat::from_extension`] plus `"owl"`.
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "guess" => Ok(Self::Guess),
            "ofn" => Ok(Self::OFN),
            "owx" => Ok(Self::OWX),
            "omn" => Ok(Self::OMN),
            "obo" => Ok(Self::OBO),
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
    OBOParser(SetOntology<A>, PrefixMapping),
}

impl<A: ForIRI, AA: ForIndex<A>> ParserOutput<A, AA> {
    pub fn resource_type(&self) -> ResourceType {
        match self {
            ParserOutput::OFNParser(..) => ResourceType::OFN,
            ParserOutput::OWXParser(..) => ResourceType::OWX,
            ParserOutput::RDFParser(..) => ResourceType::RDF,
            ParserOutput::OMNParser(..) => ResourceType::OMN,
            ParserOutput::OBOParser(..) => ResourceType::OBO,
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

    pub fn obo(sop: (SetOntology<A>, PrefixMapping)) -> ParserOutput<A, AA> {
        ParserOutput::OBOParser(sop.0, sop.1)
    }
}

/// std has no blanket `impl<T> AsRef<T> for T`, so `ParserConfiguration`'s
/// owned-`Build` case needs this impl to satisfy its `B: AsRef<Build<A>>`
/// bound; the borrowed case (`&Build<A>`) already satisfies it via std's
/// blanket `impl<T: AsRef<U>> AsRef<U> for &T`.
impl<A: ForIRI> AsRef<Build<A>> for Build<A> {
    fn as_ref(&self) -> &Build<A> {
        self
    }
}

/// Settings shared by every format's `read`. Generic over how it holds its
/// `Build` (`B`): `&Build<A>` to share one interning table across several
/// parses, or an owned `Build<A>` for a one-off parse with nothing to
/// share it with.
#[derive(Clone, Debug)]
pub struct ParserConfiguration<A: ForIRI, B: AsRef<Build<A>> = Build<A>> {
    /// The `Build` used for all IRI creation during this parse. Only the
    /// `&Build<A>` form is `Clone` -- `Build<A>` itself deliberately isn't,
    /// since cloning it would duplicate its interning table instead of
    /// sharing it.
    pub build: B,
    /// In lax mode, parsers tolerate content that would otherwise be a
    /// parse error instead of rejecting it -- see individual readers for
    /// exactly what this relaxes. Only the RDF and OWX readers consult
    /// this flag; OFN and OMN ignore it.
    pub lax: bool,
    /// Maximum bytes to read from a single remote (`http`/`https`) IRI
    /// resolution, e.g. following an `owl:imports` closure. Default
    /// `u64::MAX` (no limit); lower it when resolving untrusted IRIs where
    /// an oversized response could exhaust memory.
    pub remote_body_limit: u64,
    /// If set, no network access is attempted at all during parsing --
    /// resolving an IRI not available locally fails instead of falling
    /// back to a remote fetch. `remote_body_limit` only bounds a fetch's
    /// size if one happens; this prevents one happening at all. Default
    /// `false`.
    pub local_only: bool,
    /// Overrides format detection when set, taking precedence over the
    /// file extension. `InputFormat::Guess` triggers content sniffing.
    pub input_format: Option<InputFormat>,
    /// An OASIS XML Catalog (see the [`horned_catalog`] crate) consulted
    /// when resolving an IRI to local content, before both the heuristic
    /// path-guessing in [`crate::resolve::localize_iri`] and any remote
    /// fallback. `None` (the default) disables catalog-based resolution.
    /// An `Rc`, not a borrow like `build`: a `Catalog` is read-only with
    /// no interning identity to preserve, so a cheap refcount-bump clone
    /// per recursive parse is enough.
    pub catalog: Option<std::rc::Rc<horned_catalog::Catalog>>,
    _marker: std::marker::PhantomData<A>,
}

impl<A: ForIRI, B: AsRef<Build<A>>> ParserConfiguration<A, B> {
    /// A `ParserConfiguration` using `build` and every other setting at its
    /// default. `build` can be a shared `&Build<A>` or an owned `Build<A>`.
    pub fn new(build: B) -> Self {
        ParserConfiguration {
            build,
            lax: false,
            remote_body_limit: u64::MAX,
            local_only: false,
            input_format: None,
            catalog: None,
            _marker: std::marker::PhantomData,
        }
    }
}

impl<A: ForIRI + Default> Default for ParserConfiguration<A, Build<A>> {
    /// A `ParserConfiguration` with a fresh, private `Build<A>` -- for a
    /// one-off parse that has no `Build` to share. Use
    /// [`ParserConfiguration::new`] with a `&Build<A>` instead when the
    /// interning table needs to be shared with other parses.
    fn default() -> Self {
        ParserConfiguration::new(Build::default())
    }
}

/// RDF-specific parser settings, wrapping the settings shared by every
/// format. RDF is the only format with a format-specific setting
/// (`format`), so it's the only one with a wrapper struct at all.
#[derive(Clone, Debug)]
pub struct RDFParserConfiguration<A: ForIRI, B: AsRef<Build<A>> = Build<A>> {
    pub common: ParserConfiguration<A, B>,
    /// The RDF syntax to parse as. `None` defaults to RDF/XML.
    pub format: Option<oxrdfio::RdfFormat>,
}

impl<A: ForIRI, B: AsRef<Build<A>>> From<ParserConfiguration<A, B>>
    for RDFParserConfiguration<A, B>
{
    fn from(common: ParserConfiguration<A, B>) -> Self {
        RDFParserConfiguration {
            common,
            format: None,
        }
    }
}

impl<A: ForIRI + Default> Default for RDFParserConfiguration<A, Build<A>> {
    fn default() -> Self {
        ParserConfiguration::default().into()
    }
}

/// Resolve `iri` to its content and wrap it as a `BufRead`, for a
/// `from_doc_iri` constructor. Every format's `from_doc_iri` is a
/// one-liner over this -- `Self::from_bufread(resolve_doc_iri(iri,
/// &config)?, config)`, or `Self::from_bufread(&mut resolve_doc_iri(iri,
/// &config)?, config)` for a reader that fully drains its `&mut R` before
/// returning (see `owx::reader::Reader::from_doc_iri` and
/// `rdf::reader::OntologyParser::from_doc_iri` respectively).
pub(crate) fn resolve_doc_iri<A: ForIRI, B: AsRef<Build<A>>>(
    iri: &IRI<A>,
    config: &ParserConfiguration<A, B>,
) -> Result<std::io::Cursor<String>> {
    Ok(std::io::Cursor::new(crate::resolve::strict_resolve_iri(
        iri,
        config.remote_body_limit,
        config.local_only,
    )?))
}

/// `mapping`'s entries as `StreamComponent::Prefix` items. `AA` is
/// unconstrained since `Prefix` carries no `AA`-typed payload.
pub(crate) fn prefix_stream<'a, A: ForIRI, AA: ForIndex<A>>(
    mapping: &'a PrefixMapping,
) -> impl Iterator<Item = Result<StreamComponent<AA>>> + 'a {
    mapping
        .mappings()
        .map(|(name, iri)| Ok(StreamComponent::Prefix(name.to_string(), iri.to_string())))
}

/// `ont`'s components as a `StreamComponent` stream, in `ComponentKind`
/// order. Clones `ont`'s own `AA` handle per item, not always
/// `AnnotatedComponent<A>` -- cheap when `AA` is `Rc`/`Arc`-backed.
pub(crate) fn component_stream<'a, A: ForIRI, AA: ForIndex<A>>(
    ont: &'a ComponentMappedOntology<A, AA>,
) -> impl Iterator<Item = Result<StreamComponent<AA>>> + 'a {
    ont.i()
        .iter()
        .cloned()
        .map(|ac| Ok(StreamComponent::Component(ac)))
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
            ParserOutput::OBOParser(o, m) => (o, Some(m), None),
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
            ParserOutput::OBOParser(so, _) => so,
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
            ParserOutput::OBOParser(so, _) => so.into(),
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
        // OBO flat-file: the conventional first line is `format-version:`, but a
        // header-less document may open directly with a stanza header.
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

    #[test]
    fn prefix_stream_then_component_stream_matches_write_composition() {
        use super::*;
        use crate::model::{Build, DeclareClass, MutableOntology, RcAnnotatedComponent, RcStr};
        use crate::ontology::set::SetOntology;

        let b = Build::new_rc();
        let mut so: SetOntology<RcStr> = SetOntology::new_rc();
        so.insert(DeclareClass(b.class("http://example.com/A")));
        let cmo: ComponentMappedOntology<RcStr, RcAnnotatedComponent> =
            so.iter().cloned().collect();

        let mut mapping = curie::PrefixMapping::default();
        mapping.add_prefix("ex", "http://example.com/").unwrap();

        // OFN/OMN's composition order (Prefix before components); OWX
        // instead splits component_stream around prefix_stream to put
        // OntologyID first, since OWX serializes ontologyIRI/versionIRI as
        // XML attributes on <Ontology>, which must precede any child
        // element. component_stream yields the source ontology's own AA
        // (here RcAnnotatedComponent), so prefix_stream's own AA must match
        // that to chain.
        let items: std::result::Result<Vec<_>, _> =
            prefix_stream::<RcStr, RcAnnotatedComponent>(&mapping)
                .chain(component_stream(&cmo))
                .collect();
        let items = items.unwrap();

        assert!(
            matches!(&items[0], StreamComponent::Prefix(p, i) if p == "ex" && i == "http://example.com/")
        );
        assert!(matches!(&items[1], StreamComponent::Component(_)));
        assert_eq!(items.len(), 2);
    }

    #[test]
    fn resolve_doc_iri_surfaces_the_underlying_resolve_error() {
        use super::*;
        use crate::model::Build;

        let b = Build::new_rc();
        let config = ParserConfiguration::new(&b);
        let iri = b.iri("file:///no/such/path/does-not-exist.owl");

        // Exercises the plumbing (remote_body_limit/local_only threaded
        // through), not network access -- a nonexistent local path is
        // enough to confirm the error surfaces rather than panicking.
        assert!(resolve_doc_iri(&iri, &config).is_err());
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

    pub fn run_bubo_reparse<F>(
        format: &str,
        parse_fn: F,
    ) -> std::result::Result<(), Box<dyn std::error::Error>>
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
