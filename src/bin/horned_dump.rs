extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::{command::parse_path, error::CommandError, ontology::set::SetOntology};

use std::{collections::HashMap, path::Path};

#[allow(dead_code)]
fn main() -> Result<(), CommandError> {
    let matches = app("horned-dump").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static, 'static> {
    App::new(name)
        .version("0.1")
        .about("Parse an OWL File and dump the data structures")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .arg(Arg::with_name("incomplete").long("incomplete").short("l"))
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), CommandError> {
    let input = matches
        .value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let r = parse_path(Path::new(input))?;

    match r {
        horned_owl::io::ParserOutput::OWXParser(ont, map) => {
            let hash_map: HashMap<&String, &String> = map.mappings().collect();
            println!("Ontology:\n{:#?}\n\nMapping:\n{:#?}", ont, hash_map);
            Ok(())
        }
        horned_owl::io::ParserOutput::RDFParser(ont, inc) => {
            if !matches.is_present("incomplete") {
                let so: SetOntology = ont.into();
                println!("Ontology:\n{:#?}", so);
            }

            println!("Incomplete Parse:\n{:#?}", inc);
            Ok(())
        }
    }
}
