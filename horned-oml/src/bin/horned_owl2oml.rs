//! Convert OWL to OML, with the options of `owl2oml` from opencaesar/owl-adapter.
//!
//! Like `owl2oml`, this converts the OWL that `oml2owl` makes: the `oml:*`
//! annotations say what each ontology and term was.

use std::collections::HashMap;
use std::error::Error;
use std::fs::{self, File};
use std::io::BufReader;
use std::path::{Path, PathBuf};

use clap::{App, Arg};
use horned_catalog::Catalog;
use horned_oml::catalog::files_with_extensions;
use horned_oml::writer::{Config, OntologyInfo, write};
use horned_owl::io::rdf::reader::read as read_rdf;
use horned_owl::io::{ParserConfiguration, RDFParserConfiguration};
use horned_owl::model::{AnnotatedComponent, Build, Component, RcStr};
use horned_owl::ontology::set::SetOntology;
use oxrdfio::RdfFormat;

/// The input extensions that can be read.
const INPUT_EXTENSIONS: [&str; 11] =
    ["owl", "rdf", "xml", "ttl", "n3", "nt", "trig", "nq", "ofn", "fss", "owx"];

/// The single-dash long options of `owl2oml`, which clap does not take.
fn long_options(args: impl Iterator<Item = String>) -> Vec<String> {
    args.map(|a| match a.as_str() {
        "-if" => "--input-file-extension".to_string(),
        "-of" => "--output-file-extension".to_string(),
        _ => a,
    })
    .collect()
}

fn app() -> App<'static> {
    App::new("horned-owl2oml")
        .version(env!("CARGO_PKG_VERSION"))
        .about("Convert OWL to OML")
        .arg(
            Arg::new("input-catalog-path")
                .short('i')
                .long("input-catalog-path")
                .takes_value(true)
                .required(true)
                .help("Path of the input OWL catalog"),
        )
        .arg(
            Arg::new("output-catalog-path")
                .short('o')
                .long("output-catalog-path")
                .takes_value(true)
                .required(true)
                .help("Path of the output OML catalog, made if it does not exist"),
        )
        .arg(
            Arg::new("source-path")
                .short('s')
                .long("source-path")
                .takes_value(true)
                .multiple_values(true)
                .multiple_occurrences(true)
                .help("Paths of source OML folders that can be updated (default: any)"),
        )
        .arg(
            Arg::new("input-file-extension")
                .long("input-file-extension")
                .takes_value(true)
                .multiple_values(true)
                .multiple_occurrences(true)
                .default_value("ttl")
                .help(
                    "Extensions for the input OWL files (-if; default ttl, options: owl, rdf, \
                     xml, ttl, n3, nt, trig, nq, ofn, fss, owx)",
                ),
        )
        .arg(
            Arg::new("output-file-extension")
                .long("output-file-extension")
                .takes_value(true)
                .default_value("oml")
                .help("Extension for the output OML files (-of; only oml)"),
        )
        .arg(
            Arg::new("lax")
                .long("lax")
                .help("Leave out the axioms that OML cannot say instead of failing"),
        )
        .arg(
            Arg::new("debug")
                .short('d')
                .long("debug")
                .help("Shows debug logging statements"),
        )
}

fn main() {
    match run() {
        Ok(true) => {}
        Ok(false) => std::process::exit(1),
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1);
        }
    }
}

/// Read an OWL document, in the format that its extension says.
fn read_owl(path: &Path) -> Result<SetOntology<RcStr>, Box<dyn Error>> {
    let extension = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    let rdf = match extension {
        "owl" | "rdf" | "xml" => Some(RdfFormat::RdfXml),
        "ttl" => Some(RdfFormat::Turtle),
        "n3" => Some(RdfFormat::N3),
        "nt" => Some(RdfFormat::NTriples),
        "trig" => Some(RdfFormat::TriG),
        "nq" => Some(RdfFormat::NQuads),
        _ => None,
    };
    let mut reader = BufReader::new(File::open(path)?);
    let config = ParserConfiguration::<RcStr, Build<RcStr>>::default();
    Ok(match (rdf, extension) {
        (Some(format), _) => {
            let mut config: RDFParserConfiguration<RcStr, Build<RcStr>> = config.into();
            config.format = Some(format);
            let (o, _) = read_rdf::<RcStr, AnnotatedComponent<RcStr>, _, _>(&mut reader, config)?;
            o.into()
        }
        (None, "ofn" | "fss") => horned_owl::io::ofn::reader::read(&mut reader, config)?.0,
        (None, "owx") => horned_owl::io::owx::reader::read(&mut reader, config)?.0,
        _ => return Err(format!("{}: not a format of OWL that we read", path.display()).into()),
    })
}

/// The ontology IRI, if it has one.
fn ontology_iri(o: &SetOntology<RcStr>) -> Option<String> {
    o.iter().find_map(|ac| match &ac.component {
        Component::OntologyID(id) => id.iri.as_ref().map(|i| i.to_string()),
        _ => None,
    })
}

/// Where the OML for an ontology goes: through the output catalog if there
/// is one, otherwise laid out by IRI without its scheme beside it.
enum Output {
    Catalog(Catalog),
    Layout(PathBuf),
}

impl Output {
    fn path(&self, iri: &str) -> Result<PathBuf, Box<dyn Error>> {
        match self {
            Output::Catalog(c) => {
                let base = c
                    .resolve(iri)
                    .ok_or_else(|| format!("the output catalog does not map {iri}"))?;
                // The extension is appended: ontology names may contain dots.
                Ok(PathBuf::from(format!("{}.oml", base.display())))
            }
            Output::Layout(dir) => {
                let (_, rest) = iri
                    .split_once("://")
                    .ok_or_else(|| format!("{iri} is not an IRI with a scheme"))?;
                Ok(dir.join(format!("{rest}.oml")))
            }
        }
    }
}

fn run() -> Result<bool, Box<dyn Error>> {
    let matches = app().get_matches_from(long_options(std::env::args()));
    let debug = matches.is_present("debug");
    let of = matches.value_of("output-file-extension").unwrap();
    if of != "oml" {
        return Err(format!("Invalid output file extension {of}, expected oml").into());
    }
    let extensions: Vec<&str> = matches.values_of("input-file-extension").unwrap().collect();
    for e in &extensions {
        if !INPUT_EXTENSIONS.contains(e) {
            return Err(format!(
                "Invalid input file extension {e}, expected one of {}",
                INPUT_EXTENSIONS.join(", ")
            )
            .into());
        }
    }
    let config = Config {
        lax: matches.is_present("lax"),
    };
    let sources: Vec<PathBuf> = matches
        .values_of("source-path")
        .map(|v| v.filter_map(|p| std::path::absolute(p).ok()).collect())
        .unwrap_or_default();

    let catalog = Catalog::from_path(matches.value_of("input-catalog-path").unwrap())?;
    // A catalog is not an ontology, whatever its extension.
    let files: Vec<PathBuf> = files_with_extensions(&catalog, &extensions)
        .into_iter()
        .filter(|f| f.file_name().is_none_or(|n| n != "catalog.xml"))
        .collect();

    // The ontologies are all read first: an ontology's imports say what it is.
    let mut ontologies = vec![];
    let mut infos: HashMap<String, OntologyInfo> = HashMap::new();
    for file in &files {
        if debug {
            eprintln!("Loading {}", file.display());
        }
        let o = read_owl(file)?;
        if let (Some(iri), Some(info)) = (ontology_iri(&o), OntologyInfo::of(&o)) {
            infos.insert(iri, info);
        }
        ontologies.push((file, o));
    }

    let output_catalog = Path::new(matches.value_of("output-catalog-path").unwrap());
    let output_dir = output_catalog.parent().unwrap_or(Path::new("."));
    let output = if output_catalog.exists() {
        Output::Catalog(Catalog::from_path(output_catalog)?)
    } else {
        Output::Layout(output_dir.to_path_buf())
    };

    let imports = |iri: &str| infos.get(iri).cloned();
    let (mut saved, mut ok, mut schemes) = (0, true, std::collections::BTreeSet::new());
    for (file, o) in &ontologies {
        let Some(iri) = ontology_iri(o) else {
            eprintln!("Skipping {}: it has no ontology IRI", file.display());
            continue;
        };
        if !infos.contains_key(&iri) {
            eprintln!("Skipping {iri}: it has no oml:type annotation, so its kind is not known");
            continue;
        }
        let target = output.path(&iri)?;
        if !sources.is_empty()
            && !std::path::absolute(&target).is_ok_and(|t| sources.iter().any(|s| t.starts_with(s)))
        {
            if debug {
                eprintln!("Not saving {}: not in a source path", target.display());
            }
            continue;
        }
        if let Some((scheme, _)) = iri.split_once("://") {
            schemes.insert(scheme.to_string());
        }
        match write(o, &imports, config) {
            Ok(text) => {
                if debug {
                    eprintln!("Saving: {}", target.display());
                }
                if let Some(parent) = target.parent() {
                    fs::create_dir_all(parent)?;
                }
                fs::write(&target, text)?;
                saved += 1;
            }
            Err(e) => {
                eprintln!("{iri}: {e}");
                ok = false;
            }
        }
    }
    println!("{saved} oml file(s) are saved");

    if matches!(output, Output::Layout(_)) {
        let mut catalog_xml = String::from(
            "<?xml version='1.0'?>\n<catalog xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\" prefer=\"public\">\n",
        );
        for scheme in schemes {
            catalog_xml +=
                &format!("\t<rewriteURI uriStartString=\"{scheme}://\" rewritePrefix=\"./\" />\n");
        }
        catalog_xml += "</catalog>";
        fs::create_dir_all(output_dir)?;
        fs::write(output_catalog, catalog_xml)?;
    }
    Ok(ok)
}
