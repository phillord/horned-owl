extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::error::CommandError;

use rio_api::parser::TriplesParser;

use std::fs::File;
use std::io::BufReader;

fn main() -> Result<(), Error> {
    let matches = App::new("horned-parse")
        .version("0.1")
        .about("Parse an OWL File and dump the data structures")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();

    matcher(matches)
}

fn matcher(matches: ArgMatches) -> Result<(), Error> {
    let input = matches
        .value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let file = File::open(input)?;
    let bufreader = BufReader::new(file);
    let v: Vec<Result<String, Error>>
        = rio_xml::RdfXmlParser::new(bufreader, None).into_iter(
            |rio_triple| Ok(
                format!("{}\n\t{}\n\t{}",
                        rio_triple.subject,
                        rio_triple.predicate,
                        rio_triple.object
                )
            )
    ).collect();

    for t in v {
        println!("{}", t.unwrap());
    }

    Ok(())
}
