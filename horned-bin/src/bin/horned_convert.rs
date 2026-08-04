use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::{config::parser_config, parse_path, write};

use horned_owl::error::HornedError;
use horned_owl::ontology::component_mapped::RcComponentMappedOntology;

use std::{fs::File, io::stdout, path::Path};

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-convert").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    App::new(name)
        .version(horned_bin::version_string())
        .about("Convert an OWL Ontology between formats")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("to")
                .long("to")
                .takes_value(true)
                .required(true)
                .help(
                    "The format to convert to: owx, ofn, omn, owl, \
                     or any RDF syntax oxrdfio supports (ttl, nt, nq, trig, jsonld, n3)",
                ),
        )
        .arg(
            Arg::with_name("to-file")
                .long("to-file")
                .takes_value(true)
                .help("Write the converted output to this file instead of stdout"),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches.value_of("INPUT").unwrap();
    let to = matches.value_of("to").unwrap();

    let res = parse_path(Path::new(input), parser_config(matches))?;
    let amo: RcComponentMappedOntology = res.into();

    match matches.value_of("to-file") {
        Some(to_file) => {
            write(to, File::create(to_file)?, &amo)?;
        }
        None => {
            write(to, stdout(), &amo)?;
            // Finish off nicely
            println!();
        }
    }

    Ok(())
}
