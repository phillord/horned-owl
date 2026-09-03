//! Support for Horned command line programmes

use horned_owl::{
    error::HornedError,
    io::{InputFormat, ParserConfiguration, ParserOutput, RDFParserConfiguration, ResourceType},
    model::{Build, ForIRI, IRI, MutableOntology, OntologyID, RcAnnotatedComponent, RcStr},
    ontology::{
        component_mapped::{ComponentMappedOntology, RcComponentMappedOntology},
        indexed::ForIndex,
        set::SetOntology,
    },
    resolve::{localize_iri_favored, path_to_file_iri, strict_resolve_iri},
};

use std::{
    fs::File,
    io::{BufReader, Write as StdWrite},
    path::{Path, PathBuf},
    str::FromStr,
};

pub mod error {
    use super::*;

    pub fn error_missing_input() -> HornedError {
        HornedError::CommandError("Command requires an INPUT parameter".to_string())
    }
}

/// This binary's version, combined with the horned-owl library version it
/// was compiled against -- e.g. `"2.0.0 (horned-owl 2.0.0)"`. Used as the
/// `clap::App::version` for every horned-bin binary so `--version` reports
/// something meaningful instead of a stale hardcoded literal (see
/// https://github.com/phillord/horned-owl/issues/219).
pub fn version_string() -> &'static str {
    static VERSION: std::sync::OnceLock<String> = std::sync::OnceLock::new();
    VERSION.get_or_init(|| {
        format!(
            "{} (horned-owl {})",
            env!("CARGO_PKG_VERSION"),
            horned_owl::VERSION
        )
    })
}

/// The `oxrdfio::RdfFormat` that `extension` denotes, if any. `"owl"`
/// is horned-owl's own long-standing alias for RDF/XML; every other
/// extension is whatever [`oxrdfio::RdfFormat::from_extension`]
/// recognises (`ttl`, `nt`, `nq`, `trig`, `json`/`jsonld`, `n3`,
/// `rdf`, `xml`).
fn rdf_format_for_extension(extension: &str) -> Option<oxrdfio::RdfFormat> {
    if extension == "owl" {
        Some(oxrdfio::RdfFormat::RdfXml)
    } else {
        oxrdfio::RdfFormat::from_extension(extension)
    }
}

pub fn write<A: ForIRI, AA: ForIndex<A>, W: StdWrite>(
    format: &str,
    write: W,
    ont: &ComponentMappedOntology<A, AA>,
) -> Result<W, HornedError> {
    match format {
        "owx" => horned_owl::io::owx::writer::write(write, ont, None),
        "ofn" => horned_owl::io::ofn::writer::write(write, ont, None),
        "omn" => horned_owl::io::omn::writer::write(write, ont, None),
        "obo" => horned_owl::io::obo::writer::write(write, ont, None),
        _ => horned_owl::io::rdf::writer::write_to_rdf_format(write, ont, format),
    }
}

pub fn path_type<A: ForIRI, B: AsRef<Build<A>>>(
    path: &Path,
    config: &ParserConfiguration<A, B>,
) -> Option<ResourceType> {
    match config.input_format {
        Some(InputFormat::OFN) => return Some(ResourceType::OFN),
        Some(InputFormat::OWX) => return Some(ResourceType::OWX),
        Some(InputFormat::OMN) => return Some(ResourceType::OMN),
        Some(InputFormat::OBO) => return Some(ResourceType::OBO),
        Some(InputFormat::Rdf(_)) => return Some(ResourceType::RDF),
        Some(InputFormat::Guess) => return detect_from_path(path).map(|(rt, _)| rt),
        None => {}
    }
    match path.extension().and_then(|s| s.to_str()) {
        Some("ofn") => Some(ResourceType::OFN),
        Some("owx") => Some(ResourceType::OWX),
        Some("omn") => Some(ResourceType::OMN),
        Some("obo") => Some(ResourceType::OBO),
        Some(ext) if rdf_format_for_extension(ext).is_some() => Some(ResourceType::RDF),
        _ => detect_from_path(path).map(|(rt, _)| rt),
    }
}

/// Peek at the first 512 bytes of a file and use content sniffing as a
/// fallback when the extension is missing or unrecognised.
fn detect_from_path(path: &Path) -> Option<(ResourceType, Option<oxrdfio::RdfFormat>)> {
    use std::io::Read;
    let mut buf = [0u8; 512];
    let n = File::open(path).ok()?.read(&mut buf).ok()?;
    horned_owl::io::detect_format(&buf[..n])
}

pub fn parse_path<B: AsRef<Build<RcStr>> + Clone>(
    path: &Path,
    config: ParserConfiguration<RcStr, B>,
) -> Result<ParserOutput<RcStr, RcAnnotatedComponent>, HornedError> {
    Ok(match path_type(path, &config) {
        Some(ResourceType::OFN) => {
            let file = File::open(path)?;
            let mut bufreader = BufReader::new(file);
            ParserOutput::ofn(horned_owl::io::ofn::reader::read(&mut bufreader, config)?)
        }
        Some(ResourceType::OWX) => {
            let file = File::open(path)?;
            let mut bufreader = BufReader::new(file);
            ParserOutput::owx(horned_owl::io::owx::reader::read(&mut bufreader, config)?)
        }
        Some(ResourceType::OMN) => {
            let file = File::open(path)?;
            let mut bufreader = BufReader::new(file);
            ParserOutput::omn(horned_owl::io::omn::read(&mut bufreader, config)?)
        }
        Some(ResourceType::OBO) => {
            let file = File::open(path)?;
            let mut bufreader = BufReader::new(file);
            ParserOutput::obo(horned_owl::io::obo::read(&mut bufreader, config)?)
        }
        Some(ResourceType::RDF) => {
            let iri = horned_owl::resolve::path_to_file_iri(config.build.as_ref(), path);
            ParserOutput::rdf(horned_owl::io::rdf::closure_reader::read(
                &iri,
                with_detected_rdf_format(path, config.into()),
            )?)
        }
        None => {
            return Err(HornedError::CommandError(format!(
                "Cannot parse a file of this format: {path:?}"
            )));
        }
    })
}

/// Fill in `config.format` from `path`'s extension or content, unless the
/// caller already set one explicitly.
pub fn with_detected_rdf_format<A: ForIRI, B: AsRef<Build<A>>>(
    path: &Path,
    mut config: RDFParserConfiguration<A, B>,
) -> RDFParserConfiguration<A, B> {
    if config.format.is_none() {
        config.format = match config.common.input_format {
            Some(InputFormat::Rdf(fmt)) => fmt,
            Some(InputFormat::Guess) => detect_from_path(path).and_then(|(_, fmt)| fmt),
            _ => path
                .extension()
                .and_then(|s| s.to_str())
                .and_then(rdf_format_for_extension)
                .or_else(|| detect_from_path(path).and_then(|(_, fmt)| fmt)),
        };
    }
    config
}

/// Parse but only as far as the imports, if that makes sense.
pub fn parse_imports<B: AsRef<Build<RcStr>>>(
    path: &Path,
    config: ParserConfiguration<RcStr, B>,
) -> Result<ParserOutput<RcStr, RcAnnotatedComponent>, HornedError> {
    let file = File::open(path)?;
    let mut bufreader = BufReader::new(file);
    Ok(match path_type(path, &config) {
        Some(ResourceType::OFN) => {
            ParserOutput::ofn(horned_owl::io::owx::reader::read(&mut bufreader, config)?)
        }
        Some(ResourceType::OWX) => {
            ParserOutput::owx(horned_owl::io::owx::reader::read(&mut bufreader, config)?)
        }
        Some(ResourceType::OMN) => {
            // Manchester has no imports-only parse; read the whole document.
            ParserOutput::omn(horned_owl::io::omn::read(&mut bufreader, config)?)
        }
        Some(ResourceType::OBO) => {
            // OBO has no imports-only parse; read the whole document.
            ParserOutput::obo(horned_owl::io::obo::read(&mut bufreader, config)?)
        }
        Some(ResourceType::RDF) => {
            let config = with_detected_rdf_format(path, config.into());
            let mut p = horned_owl::io::rdf::reader::parser_with_build(&mut bufreader, config)?;
            p.parse_imports()?;
            ParserOutput::rdf(p.as_ontology_and_incomplete())
        }
        None => {
            return Err(HornedError::CommandError(format!(
                "Cannot parse a file of this format: {path:?}"
            )));
        }
    })
}

pub fn materialize<B: AsRef<Build<RcStr>> + Clone>(
    file_or_iri: &str,
    config: ParserConfiguration<RcStr, B>,
) -> Result<Vec<IRI<RcStr>>, HornedError> {
    let mut v = vec![];
    let b = Build::new();

    // We need to determine at this point whether we have an IRI or a file location, already.
    let parsed = oxiri::Iri::parse(file_or_iri);

    // If it is an IRI then we need to run ensure_local on it to bring it local
    // If it is a file location, then we just turn it into a path buf
    // Can we just do this with parse_iri method from OxIri?

    let file_pathbuf = match parsed {
        Result::Ok(_) => ensure_local(
            &b.iri(file_or_iri),
            None,
            config.remote_body_limit,
            config.local_only,
        )?,
        Result::Err(_) => PathBuf::from_str(file_or_iri).expect("Result is infallable"),
    };

    materialize_1(&file_pathbuf, config, &mut v, true)?;
    Ok(v)
}

fn ensure_local(
    iri: &IRI<RcStr>,
    relative_doc_iri: Option<&IRI<RcStr>>,
    remote_body_limit: u64,
    local_only: bool,
) -> Result<PathBuf, HornedError> {
    let local_path = localize_iri_favored(iri, relative_doc_iri);

    if !local_path.exists() {
        println!("Retrieving Ontology: {}", iri);
        let imported_data = strict_resolve_iri(iri, remote_body_limit, local_only)?;
        println!("Saving to {}", local_path.display());
        let mut file = File::create(&local_path)?;
        file.write_all(imported_data.as_bytes())?;
    } else {
        println!("Already Present: {}", local_path.display());
    }
    Ok(local_path)
}

fn materialize_1<'a, B: AsRef<Build<RcStr>> + Clone>(
    file_location: &PathBuf,
    config: ParserConfiguration<RcStr, B>,
    done: &'a mut Vec<IRI<RcStr>>,
    recurse: bool,
) -> Result<&'a mut Vec<IRI<RcStr>>, HornedError> {
    println!("Parsing: {}", file_location.display());
    let amont: RcComponentMappedOntology =
        parse_imports(Path::new(file_location), config.clone())?.into();
    let import = amont.i().import();

    let doc_iri = path_to_file_iri(config.build.as_ref(), file_location.as_path());
    // Get all the imports
    for i in import {
        if !done.contains(&i.0) {
            done.push(i.0.clone());
            let local_path = ensure_local(
                &i.0,
                Some(&doc_iri),
                config.remote_body_limit,
                config.local_only,
            )?;

            if recurse {
                materialize_1(&local_path, config.clone(), done, true)?;
            }
        } else {
            println!("Already materialized: {}", i.0);
        }
    }

    Ok(done)
}

pub fn generate_big_owl<W: StdWrite>(size: isize, format: &str, w: W) -> Result<W, HornedError> {
    let b = Build::new_rc();
    let mut o = SetOntology::new_rc();

    o.insert(OntologyID {
        iri: Some(b.iri("http://www.example.com/iri")),
        viri: None,
    });

    for i in 1..size + 1 {
        o.declare(b.class(format!("https://www.example.com/o{}", i)));
    }

    let amo: RcComponentMappedOntology = o.into();
    write(format, w, &amo)
}

pub mod naming {
    use horned_owl::model::ComponentKind;
    use horned_owl::model::ComponentKind::*;

    pub fn name(axk: &ComponentKind) -> &'static str {
        match axk {
            OntologyID => "Ontology ID",
            DocIRI => "Doc IRI",
            OntologyAnnotation => "Ontology Annotation",
            Import => "Import",
            DeclareClass => "Declare Class",
            DeclareObjectProperty => "Declare Object Property",
            DeclareAnnotationProperty => "Declare Annotation Property",
            DeclareDataProperty => "Declare Data Property",
            DeclareNamedIndividual => "Declare Named Individual",
            DeclareDatatype => "Declare Datatype",
            SubClassOf => "Sub-Class Of",
            EquivalentClasses => "Equivalent Classes",
            DisjointClasses => "Disjoint Classes",
            DisjointUnion => "Disjoint Union",
            SubObjectPropertyOf => "Sub Object Property Of",
            EquivalentObjectProperties => "Equivalent Object Properties",
            DisjointObjectProperties => "Disjoint Object Properties",
            InverseObjectProperties => "Inverse Object Properties",
            ObjectPropertyDomain => "Object Property Domain",
            ObjectPropertyRange => "Object Property Range",
            FunctionalObjectProperty => "Functional Object Property",
            InverseFunctionalObjectProperty => "Inverse Functional Object Property",
            ReflexiveObjectProperty => "Reflexive Object Property",
            IrreflexiveObjectProperty => "Irreflexive Object Property",
            SymmetricObjectProperty => "Symmetric Object Property",
            AsymmetricObjectProperty => "Asymmetric Object Property",
            TransitiveObjectProperty => "Transitive Object Property",
            SubDataPropertyOf => "Sub Data Property Of",
            EquivalentDataProperties => "Equivalent Data Properties",
            DisjointDataProperties => "Disjoint Data Properties",
            DataPropertyDomain => "Data Property Domain",
            DataPropertyRange => "Data Property Range",
            FunctionalDataProperty => "Functional Data Property",
            DatatypeDefinition => "Datatype Definition",
            HasKey => "Has Key",
            SameIndividual => "Same Individual",
            DifferentIndividuals => "Different Individuals",
            ClassAssertion => "Class Assertion",
            ObjectPropertyAssertion => "Object Property Assertion",
            NegativeObjectPropertyAssertion => "Negative Object Property Assertion",
            DataPropertyAssertion => "Data Property Assertion",
            NegativeDataPropertyAssertion => "Negative Data Property Assertion",
            AnnotationAssertion => "Annotation Assertion",
            SubAnnotationPropertyOf => "Sub Annotation Property Of",
            AnnotationPropertyDomain => "Annotation Property Domain",
            AnnotationPropertyRange => "Annotation Property Range",
            Rule => "Rule",
        }
    }
}

pub mod validation {
    use horned_owl::{io::rdf::reader::IncompleteParse, model::ForIRI};

    pub fn write_incomplete<T: ForIRI>(incomplete: IncompleteParse<T>) {
        println!("\n\nIncompleted Parsed");
        println!("\tSimple Triples: {:#?}", incomplete.simple);
        println!("\tbnode: {:#?}", incomplete.bnode);
        println!("\tsequences: {:#?}", incomplete.bnode_seq);
        println!("\tClass Expressions: {:#?}", incomplete.class_expression);
        println!(
            "\tObject Property Expressions: {:#?}",
            incomplete.object_property_expression
        );
        println!("\tData Range: {:#?}", incomplete.data_range);
        println!("\tAnnotations: {:#?}", incomplete.ann_map);
    }
}

pub mod summary {

    use horned_owl::{
        model::{AnnotatedComponent, ComponentKind, HigherKinded},
        ontology::{component_mapped::RcComponentMappedOntology, indexed::IterableOntologyIndex},
    };
    use indexmap::map::IndexMap;
    use std::borrow::Borrow;

    #[derive(Debug)]
    pub struct SummaryStatistics {
        pub logical_axiom: usize,
        pub annotation_axiom: usize,
        pub meta_comp: usize,
        pub axiom_type: IndexMap<ComponentKind, usize>,
    }

    impl SummaryStatistics {
        pub fn with_axiom_types(&self) -> impl Iterator<Item = (&ComponentKind, &usize)> + '_ {
            self.axiom_type.iter().filter(|&(_, v)| v > &0)
        }
    }

    pub fn summarize<O: Into<RcComponentMappedOntology>>(ont: O) -> SummaryStatistics
    where
        O:,
    {
        let ont: RcComponentMappedOntology = ont.into();
        SummaryStatistics {
            logical_axiom: ont
                .i()
                .iter()
                .filter(|&c| {
                    let c: &AnnotatedComponent<_> = c.borrow();
                    c.is_axiom()
                })
                .count(),
            annotation_axiom: ont
                .i()
                .iter()
                .map(|aa| {
                    let aa: &AnnotatedComponent<_> = aa.borrow();
                    aa.ann.len()
                })
                .sum::<usize>(),
            meta_comp: ont
                .i()
                .iter()
                .filter(|&c| {
                    let c: &AnnotatedComponent<_> = c.borrow();
                    c.is_meta()
                })
                .count(),
            axiom_type: axiom_types(ont),
        }
    }

    fn axiom_types<O: Into<RcComponentMappedOntology>>(ont: O) -> IndexMap<ComponentKind, usize> {
        let ont: RcComponentMappedOntology = ont.into();
        let mut im = IndexMap::new();
        for ax in ComponentKind::all_kinds() {
            im.insert(ax, ont.i().component(ax).count());
        }

        im
    }
}

pub mod config {
    use clap::App;
    use clap::ArgAction;
    use clap::ArgMatches;
    use horned_owl::io::{InputFormat, ParserConfiguration};
    use horned_owl::model::{Build, ForIRI};

    /// Add parser-config options as *global* args on the unified `horned`
    /// binary's top-level App (see `horned.rs`) -- with `global(true)`,
    /// clap makes them available on every subcommand's own `ArgMatches`
    /// regardless of whether the flag is given before or after the
    /// subcommand name. Not called by the standalone single-subcommand
    /// binaries (`horned-parse` etc): almost every subcommand parses
    /// something, so these options belong on the shared `horned
    /// <subcommand>` front door rather than duplicated per binary --
    /// mirrors how `git` only offers most flags on `git <subcommand>`,
    /// not on the individual `git-<subcommand>` binaries.
    pub fn parser_app_global(app: App<'static>) -> App<'static> {
        app.arg(
            clap::arg!(--"lax")
                .required(false)
                .global(true)
                .action(ArgAction::SetTrue)
                .help("Parse in a lax manner"),
        )
        .arg(
            clap::arg!(--"remote-body-limit" <BYTES>)
                .required(false)
                .global(true)
                .value_parser(clap::value_parser!(u64))
                .help(
                    "Maximum bytes to read from a remote IRI resolution \
                     (e.g. while following owl:imports); unbounded if not given",
                ),
        )
        .arg(
            clap::arg!(--"local-only")
                .required(false)
                .global(true)
                .action(ArgAction::SetTrue)
                .help(
                    "Never access the network -- fail instead of resolving \
                     an IRI (e.g. an owl:imports target) remotely",
                ),
        )
        .arg(
            clap::arg!(--"input-format" <FORMAT>)
                .required(false)
                .global(true)
                .help(
                    "Override input format detection. Accepted values: \
                     owl, rdf, xml (RDF/XML), ttl (Turtle), nt (N-Triples), \
                     owx (OWL/XML), ofn (Functional Syntax), omn (Manchester), \
                     guess (detect from content)",
                ),
        )
    }

    /// `lax`/`remote-body-limit`/`local-only` are only registered on the
    /// unified `horned` binary (see `parser_app_global`), not on the
    /// standalone `horned-*` binaries -- so on those, `matches` won't have
    /// these arg ids defined at all. `try_get_one` reports that as `Err`,
    /// same as "not provided" reports `Ok(None)`; either way we fall back
    /// to the off/unbounded default, whereas `get_one` panics on an
    /// undefined id.
    pub fn parser_config<'a, A: ForIRI>(
        matches: &ArgMatches,
        build: &'a Build<A>,
    ) -> ParserConfiguration<A, &'a Build<A>> {
        let mut config = ParserConfiguration::new(build);
        config.lax = matches
            .try_get_one::<bool>("lax")
            .ok()
            .flatten()
            .copied()
            .unwrap_or(false);
        config.remote_body_limit = matches
            .try_get_one::<u64>("remote-body-limit")
            .ok()
            .flatten()
            .copied()
            .unwrap_or(u64::MAX);
        config.local_only = matches
            .try_get_one::<bool>("local-only")
            .ok()
            .flatten()
            .copied()
            .unwrap_or(false);
        config.input_format = matches
            .try_get_one::<String>("input-format")
            .ok()
            .flatten()
            .and_then(|s| s.parse::<InputFormat>().ok());
        config
    }
}
