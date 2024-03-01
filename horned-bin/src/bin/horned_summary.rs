extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;


use horned_bin::{
    parse_path,
    config::{parser_app, parser_config},
    naming::name,
    summary::summarize
};

use horned_owl::error::HornedError;

use std::path::Path;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-summary").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    parser_app(
        App::new(name)
            .version("0.1")
            .about("Summary Statistics for an OWL file.")
            .author("Phillip Lord")
            .arg(
                Arg::with_name("INPUT")
                    .help("Sets the input file to use")
                    .required(true)
            )
    )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches.value_of("INPUT").ok_or_else(|| HornedError::CommandError(
        "A file name must be specified".to_string(),
    ))?;

    let config = parser_config(matches);
    let (ont, p, i) = parse_path(Path::new(input), config)?.decompose();

    let summary = summarize(ont);
    println!("Ontology has:");
    println!("\tLogical Components: {}", summary.logical_axiom);
    println!("\tAnnotation Components: {}", summary.annotation_axiom);
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
        println!("\tbnode: {}", i.bnode.len());
        println!("\tsequences: {}", i.bnode_seq.len());
        println!("\tClass Expressions: {}", i.class_expression.len());
        println!(
            "\tObject Property Expressions: {}",
            i.object_property_expression.len()
        );
        println!("\tData Range: {}", i.data_range.len());
        println!("\tAnnotations: {}", i.ann_map.len())
    }

    Ok(())
}
