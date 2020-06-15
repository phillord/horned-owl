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

    let (ont, mapping) = parse_path(Path::new(input))?;

    let hash_map: HashMap<&String, &String> = mapping.mappings().collect();
    println!("Ontology:\n{:?}\n\nMapping:\n{:?}", ont, hash_map);
    Ok(())
}
