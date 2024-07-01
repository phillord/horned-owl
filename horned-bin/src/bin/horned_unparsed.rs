extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::config::{parser_app, parser_config};
use horned_owl::error::HornedError;
use horned_owl::io::rdf::reader::ConcreteRDFOntology;
use horned_owl::model::{RcAnnotatedComponent, RcStr};

use std::{fs::File, io::BufReader, path::Path};

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-unparsed").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    parser_app(
        App::new(name)
            .version("0.1")
            .about("Show unparsed OWL RDF.")
            .author("Phillip Lord")
            .arg(
                Arg::with_name("INPUT")
                    .help("Sets the input file to use")
                    .required(true)
                    .index(1),
            ),
    )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches.value_of("INPUT").ok_or_else(|| {
        HornedError::CommandError("Command requires an INPUT argument".to_string())
    })?;

    let (_ont, incomplete): (ConcreteRDFOntology<RcStr, RcAnnotatedComponent>, _) =
        horned_owl::io::rdf::reader::read(
            &mut BufReader::new(File::open(Path::new(input))?),
            parser_config(matches),
        )?;

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

    Ok(())
}
