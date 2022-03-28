extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::error::underlying;
use horned_owl::error::CommandError;
use horned_owl::io::rdf::reader::RDFOntology;


use std::rc::Rc;
use std::{fs::File, io::BufReader, path::Path};

#[allow(dead_code)]
fn main() -> Result<(), CommandError> {
    let matches = app("horned-unparsed").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static, 'static> {
    App::new(name)
        .version("0.1")
        .about("Show unparsed OWL RDF.")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), CommandError> {
    let input = matches
        .value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let (_ont, incomplete):(RDFOntology<Rc<str>>,_)
        = horned_owl::io::rdf::reader::read(&mut BufReader::new(
        File::open(Path::new(input)).map_err(underlying)?,
    ))
    .map_err(underlying)?
    .into();

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
