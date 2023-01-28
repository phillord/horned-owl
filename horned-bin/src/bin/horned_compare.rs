extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::command::naming::name;
use horned_owl::command::{parse_path, summary::summarize};
use horned_owl::command::config::{parser_app, parser_config};
use horned_owl::error::HornedError;

use std::path::Path;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-compare").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    parser_app(
        App::new(name)
            .version("0.1")
            .about("Compare two OWL files")
            .author("Phillip Lord")
            .arg(
                Arg::with_name("INPUT-A")
                    .help("Sets the input file to use")
                    .required(true)
                    .index(1),
            )
            .arg(
                Arg::with_name("INPUT-B")
                    .help("Sets the input file to use")
                    .required(true)
                    .index(2),
            )
    )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let config = parser_config(matches);

    let input_a = matches.value_of("INPUT-A").ok_or_else(|| HornedError::CommandError(
        "A file name must be specified".to_string(),
    ))?;

    let input_b = matches.value_of("INPUT-B").ok_or_else(|| HornedError::CommandError(
        "A file name must be specified".to_string(),
    ))?;

    let (ont_a, p_a, i_a) = parse_path(Path::new(input_a), config)?.decompose();
    let (ont_b, p_b, i_b) = parse_path(Path::new(input_b), config)?.decompose();


    let summary_a = summarize(ont_a);
    let summary_b = summarize(ont_b);

    println!("Ontology\t\t\t\t\tA\t\tB");
    println!("\tLogical Axioms:\t\t\t\t{}\t\t{}", summary_a.logical_axiom, summary_b.logical_axiom);
    println!("\tAnnotation Axioms:\t\t\t{}\t\t{}", summary_a.annotation_axiom, summary_b.annotation_axiom);
    println!();
    println!("Detailed");

    for ((axk, size_a), (_, size_b)) in summary_a.with_axiom_types().zip(summary_b.with_axiom_types()) {
        println!("\t{0:<40}{1:<5}\t{2:<5}", name(axk), size_a, size_b);
    }

    if let Some(p_a) = p_a {
        println!("\nPrefixes A");
        for i in p_a.mappings() {
            println!("\t{}: {}", i.0, i.1);
        }
    }

    if let Some(p_b) = p_b {
        println!("\nPrefixes B");
        for i in p_b.mappings() {
            println!("\t{}: {}", i.0, i.1);
        }
    }

    if let Some(i_a) = i_a {
        println!("\n\nIncompleted Parsed (A)");
        println!("\tSimple Triples: {}", i_a.simple.len());
        println!("\tbnode: {}", i_a.bnode.len());
        println!("\tsequences: {}", i_a.bnode_seq.len());
        println!("\tClass Expressions: {}", i_a.class_expression.len());
        println!(
            "\tObject Property Expressions: {}",
            i_a.object_property_expression.len()
        );
        println!("\tData Range: {}", i_a.data_range.len());
        println!("\tAnnotations: {}", i_a.ann_map.len())
    }

    if let Some(i_b) = i_b {
        println!("\n\nIncompleted Parsed (B)");
        println!("\tSimple Triples: {}", i_b.simple.len());
        println!("\tbnode: {}", i_b.bnode.len());
        println!("\tsequences: {}", i_b.bnode_seq.len());
        println!("\tClass Expressions: {}", i_b.class_expression.len());
        println!(
            "\tObject Property Expressions: {}",
            i_b.object_property_expression.len()
        );
        println!("\tData Range: {}", i_b.data_range.len());
        println!("\tAnnotations: {}", i_b.ann_map.len())
    }

    Ok(())
}
