extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::error::CommandError;

use std::{fs::File, io::BufReader, path::Path};

fn main() -> Result<(), Error> {
    let matches = App::new("horned-unparsed")
        .version("0.1")
        .about("Show unparsed OWL RDF.")
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

    let (_ont, incomplete) = horned_owl::io::rdf::reader::read(
        &mut BufReader::new(
            File::open(
                Path::new(input)
            )?
        )
    )?.into();

    println!("\n\nIncompleted Parsed");
    println!("\tSimple Triples: {:#?}", incomplete.simple);
    println!("\tbnode: {:#?}", incomplete.bnode);
    println!("\tsequences: {:#?}", incomplete.bnode_seq);
    println!("\tClass Expressions: {:#?}", incomplete.class_expression);
    println!("\tObject Property Expressions: {:#?}", incomplete.object_property_expression);
    println!("\tData Range: {:#?}", incomplete.data_range);
    println!("\tAnnotations: {:#?}", incomplete.ann_map);

    Ok(())
}
