extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::{
    config::{parser_app, parser_config},
    parse_path,
};

use horned_owl::{error::HornedError, ontology::set::SetOntology};

use std::{collections::HashMap, path::Path};

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-dump").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    parser_app(
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
            .arg(Arg::with_name("incomplete").long("incomplete").short('l')),
    )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches
        .value_of("INPUT")
        .ok_or_else(|| HornedError::CommandError("A file name must be specified".to_string()))?;

    let r = parse_path(Path::new(input), parser_config(matches))?;

    match r {
        horned_owl::io::ParserOutput::OFNParser(ont, map) => {
            let hash_map: HashMap<&String, &String> = map.mappings().collect();
            println!("Ontology:\n{:#?}\n\nMapping:\n{:#?}", ont, hash_map);
            Ok(())
        }
        horned_owl::io::ParserOutput::OWXParser(ont, map) => {
            let hash_map: HashMap<&String, &String> = map.mappings().collect();
            println!("Ontology:\n{:#?}\n\nMapping:\n{:#?}", ont, hash_map);
            Ok(())
        }
        horned_owl::io::ParserOutput::RDFParser(ont, inc) => {
            if !matches.is_present("incomplete") {
                let so: SetOntology<_> = ont.into();
                println!("Ontology:\n{:#?}", so);
            }

            println!("Incomplete Parse:\n{:#?}", inc);
            Ok(())
        }
    }
}
