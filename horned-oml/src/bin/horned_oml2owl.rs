//! Convert OML to OWL, with the options of `oml2owl` from opencaesar/owl-adapter.

use std::collections::BTreeSet;
use std::error::Error;
use std::fs::{self, File};
use std::io::Write;
use std::path::{Path, PathBuf};

use clap::{App, Arg};
use horned_catalog::Catalog;
use horned_oml::catalog::{CatalogResolver, oml_files};
use horned_oml::reader::{
    CachingResolver, Config, OmlAnnotations, Parsed, Resolver, declare_used_entities,
    import_closure, is_built_in_ontology, read_parsed,
};
use horned_oml::validate::{Severity, validate_parsed};
use horned_owl::model::{Build, RcStr};
use horned_owl::ontology::component_mapped::RcComponentMappedOntology;
use horned_owl::ontology::set::SetOntology;

const EXTENSIONS: [&str; 12] =
    ["owl", "rdf", "xml", "rj", "ttl", "n3", "nt", "trig", "nq", "trix", "jsonld", "fss"];

/// The single-dash long options of `oml2owl`, which clap does not take.
fn long_options(args: impl Iterator<Item = String>) -> Vec<String> {
    args.map(|a| match a.as_str() {
        "-an" => "--oml-annotations".to_string(),
        "-rl" => "--generate-rules".to_string(),
        _ => a,
    })
    .collect()
}

fn app() -> App<'static> {
    App::new("horned-oml2owl")
        .version(env!("CARGO_PKG_VERSION"))
        .about("Convert OML to OWL")
        .arg(
            Arg::new("input-catalog-path")
                .short('i')
                .long("input-catalog-path")
                .takes_value(true)
                .required(true)
                .help("Path of the input OML catalog"),
        )
        .arg(
            Arg::new("root-ontology-iri")
                .short('r')
                .long("root-ontology-iri")
                .takes_value(true)
                .help("Root ontology IRI: convert only it and what it imports"),
        )
        .arg(
            Arg::new("output-catalog-path")
                .short('o')
                .long("output-catalog-path")
                .takes_value(true)
                .required(true)
                .help("Path of the output OWL catalog"),
        )
        .arg(
            Arg::new("output-file-extension")
                .short('f')
                .long("output-file-extension")
                .takes_value(true)
                .default_value("owl")
                .help(
                    "Extension for the output OWL files (owl, rdf, xml, rj, ttl, n3, nt, trig, \
                     nq, trix, jsonld, fss)",
                ),
        )
        .arg(
            Arg::new("disjoint-unions")
                .short('u')
                .long("disjoint-unions")
                .help("Whether to create disjoint union axioms"),
        )
        .arg(
            Arg::new("annotations-on-axioms")
                .short('a')
                .long("annotations-on-axioms")
                .help("Whether to emit annotations on axioms"),
        )
        .arg(
            Arg::new("generate-rules")
                .long("generate-rules")
                .help("Jena rules files (-rl): not supported"),
        )
        .arg(
            Arg::new("oml-annotations")
                .long("oml-annotations")
                .takes_value(true)
                .possible_values(["generate", "suppress"])
                .default_value("generate")
                .help("How to process OML annotations (-an)"),
        )
        .arg(
            Arg::new("debug")
                .short('d')
                .long("debug")
                .help("Shows debug logging statements"),
        )
}

/// An ontology to convert.
struct Input {
    path: Option<PathBuf>,
    text: String,
}

fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn Error>> {
    let matches = app().get_matches_from(long_options(std::env::args()));
    if matches.is_present("generate-rules") {
        return Err("Jena rules files (-rl) are not supported".into());
    }
    let debug = matches.is_present("debug");
    let extension = matches.value_of("output-file-extension").unwrap();
    if !EXTENSIONS.contains(&extension) {
        return Err(format!(
            "Invalid file extension {extension}, expected one of {}",
            EXTENSIONS.join(", ")
        )
        .into());
    }
    let config = Config {
        disjoint_unions: matches.is_present("disjoint-unions"),
        annotations_on_axioms: matches.is_present("annotations-on-axioms"),
        oml_annotations: match matches.value_of("oml-annotations") {
            Some("suppress") => OmlAnnotations::Suppress,
            _ => OmlAnnotations::Generate,
        },
    };

    let catalog_path = Path::new(matches.value_of("input-catalog-path").unwrap());
    let catalog = Catalog::from_path(catalog_path)?;
    let resolver = CachingResolver::new(CatalogResolver::from_path(catalog_path)?);

    let inputs = match matches.value_of("root-ontology-iri") {
        Some(root) => closure_inputs(root, &resolver)?,
        None => file_inputs(&catalog)?,
    };

    // Each document is parsed once, for its terms, its checks and its
    // conversion.
    let parsed = inputs
        .iter()
        .map(|i| Parsed::new(&i.text))
        .collect::<Result<Vec<_>, _>>()?;
    let iris: Vec<String> = parsed
        .iter()
        .map(|p| resolver.terms_of_parsed(p).iri().to_string())
        .collect();

    // Everything is checked before anything is converted.
    let started = std::time::Instant::now();
    let mut problems = vec![];
    for ((input, parsed), iri) in inputs.iter().zip(&parsed).zip(&iris) {
        if debug {
            eprintln!("Validating: {iri}");
        }
        let Some(path) = &input.path else { continue };
        for d in validate_parsed(parsed, &resolver, path)? {
            if d.severity == Severity::Error {
                problems.push(format!("{}: {d}", path.display()));
            }
        }
    }
    if debug {
        eprintln!("Validated in {:?}", started.elapsed());
    }
    if !problems.is_empty() {
        return Err(format!("\n{}", problems.join("\n")).into());
    }

    let output_catalog = Path::new(matches.value_of("output-catalog-path").unwrap());
    let output_dir = output_catalog.parent().unwrap_or(Path::new("."));
    let mut schemes = BTreeSet::new();
    let mut saved = 0;
    for (parsed, iri) in parsed.iter().zip(&iris) {
        if is_built_in_ontology(iri) {
            continue;
        }
        let (scheme, rest) = iri
            .split_once("://")
            .ok_or_else(|| format!("{iri} is not an IRI with a scheme"))?;
        schemes.insert(scheme.to_string());
        let file = output_dir.join(format!("{rest}.{extension}"));
        if debug {
            eprintln!("Creating: {}", file.display());
        }
        let build = Build::new_rc();
        let timer = std::time::Instant::now();
        let (set, prefixes): (SetOntology<RcStr>, _) =
            read_parsed(parsed, &build, &resolver, config)?;
        let mut set = set;
        declare_used_entities(&mut set);
        let ontology: RcComponentMappedOntology = set.into();
        if debug {
            eprintln!("Converted in {:?}", timer.elapsed());
        }
        if let Some(parent) = file.parent() {
            fs::create_dir_all(parent)?;
        }
        let timer = std::time::Instant::now();
        write(extension, File::create(&file)?, &ontology, &prefixes)?;
        if debug {
            eprintln!("Written in {:?}", timer.elapsed());
        }
        saved += 1;
    }
    println!("{saved} owl file(s) are saved");

    let mut catalog_xml = String::from(
        "<?xml version='1.0'?>\n<catalog xmlns=\"urn:oasis:names:tc:entity:xmlns:xml:catalog\" prefer=\"public\">\n",
    );
    for scheme in schemes {
        catalog_xml +=
            &format!("\t<rewriteURI uriStartString=\"{scheme}://\" rewritePrefix=\"./\" />\n");
    }
    catalog_xml += "</catalog>";
    fs::write(output_catalog, catalog_xml)?;
    Ok(())
}

/// The root ontology and everything it imports.
fn closure_inputs(
    root: &str,
    resolver: &CachingResolver<CatalogResolver>,
) -> Result<Vec<Input>, Box<dyn Error>> {
    let mut inputs = vec![];
    for iri in import_closure(root.trim_end_matches(['#', '/']), resolver) {
        match resolver.load(&iri) {
            Some(text) => inputs.push(Input {
                path: resolver.path(&iri),
                text,
            }),
            None if is_built_in_ontology(&iri) => {}
            None => return Err(format!("cannot find the ontology {iri}").into()),
        }
    }
    Ok(inputs)
}

/// Every OML file that the catalog covers.
fn file_inputs(catalog: &Catalog) -> Result<Vec<Input>, Box<dyn Error>> {
    oml_files(catalog)
        .into_iter()
        .map(|path| {
            let text = fs::read_to_string(&path)?;
            Ok(Input {
                path: Some(path),
                text,
            })
        })
        .collect()
}

fn write<W: Write>(
    extension: &str,
    out: W,
    ontology: &RcComponentMappedOntology,
    prefixes: &curie::PrefixMapping,
) -> Result<W, Box<dyn Error>> {
    Ok(match extension {
        "fss" => horned_owl::io::ofn::writer::write(out, ontology, Some(prefixes))?,
        other => horned_owl::io::rdf::writer::write_to_rdf_format(out, ontology, other)?,
    })
}
