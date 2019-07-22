extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::command::naming::name;
use horned_owl::command::summary::summarize;
use horned_owl::error::CommandError;
use horned_owl::io::reader::read;

use std::io::BufReader;
use std::fs::File;

fn main() -> Result<(),Error> {
    let matches =
        App::new("horned-summary")
        .version("0.1")
        .about("Summary Statistics for an OWL file.")
        .author("Phillip Lord")
        .arg(Arg::with_name("INPUT")
             .help("Sets the input file to use")
             .required(true)
             .index(1))
        .get_matches();

    matcher(&matches)
}

fn matcher(matches:&ArgMatches) -> Result<(),Error>{
    let input = matches.value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let file = File::open(input).unwrap();
    let mut bufreader = BufReader::new(file);
    let (ont,_) = read(&mut bufreader)?;

    let summary = summarize(&ont);
    println!("Ontology has:");
    println!("Logical Axioms: {}", summary.logical_axiom);
    println!("Annotation Axioms: {}", summary.annotation_axiom);
    println!();
    println!("Detailed");

    for (axk, size) in summary.with_axiom_types() {
        println!("\t{0:<40} | {1:<5}", name(axk), size);
    }


    Ok(())
}
