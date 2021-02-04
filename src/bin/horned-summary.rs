extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::{command::naming::name};
use horned_owl::command::{parse_path, summary::summarize};
use horned_owl::error::CommandError;

use std::path::Path;

fn main() -> Result<(), Error> {
    let matches = App::new("horned-summary")
        .version("0.1")
        .about("Summary Statistics for an OWL file.")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();

    matcher(&matches)
}

fn matcher(matches: &ArgMatches) -> Result<(), Error> {
    let input = matches
        .value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let (ont, p, i) = parse_path(Path::new(input))?.decompose();

    let summary = summarize(ont);
    println!("Ontology has:");
    println!("\tLogical Axioms: {}", summary.logical_axiom);
    println!("\tAnnotation Axioms: {}", summary.annotation_axiom);
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
        println!("\tObject Property Expressions: {}", i.object_property_expression.len());
        println!("\tData Range: {}", i.data_range.len());
        println!("\tAnnotations: {}", i.ann_map.len())
    }

    Ok(())
}
