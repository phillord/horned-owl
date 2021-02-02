extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::{command::parse_path, error::CommandError};

use std::{collections::HashMap, path::Path};

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

    let r = parse_path(Path::new(input))?;

    match r {
        horned_owl::io::ParserOutput::OWXParser(ont, map) => {
            let hash_map: HashMap<&String, &String> = map.mappings().collect();
            println!("Ontology:\n{:?}\n\nMapping:\n{:?}", ont, hash_map);
            Ok(())

        }
        horned_owl::io::ParserOutput::RDFParser(ont, inc) => {
            println!("Ontology:\n{:?}\n\nIncomplete Parse:\n{:?}",ont, inc);
            Ok(())
        }
    }
}
