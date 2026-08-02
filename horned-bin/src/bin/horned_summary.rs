extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::{
    config::parser_config, naming::name, parse_path, summary::summarize, with_detected_rdf_format,
};
use horned_owl::io::ResourceType;

use horned_owl::error::HornedError;

use std::path::Path;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-summary").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    App::new(name)
        .version(horned_bin::version_string())
        .about("Summary Statistics for an OWL file.")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches
        .value_of("INPUT")
        .ok_or_else(|| HornedError::CommandError("A file name must be specified".to_string()))?;

    let config = parser_config(matches);
    let parsed = parse_path(Path::new(input), config.clone())?;
    let resource_type = parsed.resource_type();
    let rdf_format = with_detected_rdf_format(Path::new(input), config)
        .rdf
        .format;
    let (ont, p, i) = parsed.decompose();

    let summary = summarize(ont);
    println!("Ontology has:");
    println!("\tLogical Components: {}", summary.logical_axiom);
    println!("\tAnnotation Components: {}", summary.annotation_axiom);
    println!("\tMeta Components: {}", summary.meta_comp);
    println!();
    println!("Detailed");

    for (axk, size) in summary.with_axiom_types() {
        println!("\t{0:<40} | {1:<5}", name(axk), size);
    }

    if let Some(p) = p {
        println!("\nPrefixes");
        for i in p.mappings() {
            println!("\t{}: {}", i.0, i.1);
        }
    }

    if let Some(i) = i {
        println!("\n\nIncompleted Parsed");
        println!("\tSimple Triples: {}", i.simple.len());
        println!("\tBnode: {}", i.bnode.len());
        println!("\tSequences: {}", i.bnode_seq.len());
        println!("\tClass Expressions: {}", i.class_expression.len());
        println!(
            "\tObject Property Expressions: {}",
            i.object_property_expression.len()
        );
        println!("\tData Range: {}", i.data_range.len());
        println!("\tAtom: {}", i.atom.len());
        println!("\tAnnotations: {}", i.ann_map.len())
    }

    let (english, mime) = format_names(&resource_type, rdf_format);
    println!("\nParse Format: {english}");
    println!("Mime Type: {mime}");

    Ok(())
}

fn format_names(
    resource_type: &ResourceType,
    rdf_format: Option<oxrdfio::RdfFormat>,
) -> (&'static str, &'static str) {
    match resource_type {
        ResourceType::OFN => ("OWL Functional Syntax", "text/owl-functional"),
        ResourceType::OWX => ("OWL/XML", "application/owl+xml"),
        ResourceType::OMN => ("Manchester Syntax", "text/owl-manchester"),
        ResourceType::OBO => ("OBO Flat-File Format", "text/obo"),
        ResourceType::RDF => match rdf_format {
            Some(f) => (f.name(), f.media_type()),
            None => ("RDF/XML", "application/rdf+xml"),
        },
    }
}
