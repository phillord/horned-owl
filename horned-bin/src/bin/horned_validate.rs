extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::{
    config::{parser_app, parser_config},
    parse_path,
};

use horned_owl::error::HornedError;

use std::path::Path;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-validate").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    parser_app(
        App::new(name)
            .version("0.1")
            .about("Validates an ontology against the OWL2 specification")
            .author("Filippo De Bortoli")
            .arg(
                Arg::with_name("INPUT")
                    .help("Sets the input file to use")
                    .required(true)
                    .index(1),
            ),
    )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches
        .value_of("INPUT")
        .ok_or_else(horned_bin::error::error_missing_input)?;

    let incomplete = parse_path(Path::new(input), parser_config(matches))?
        .decompose()
        .2;

    if let Some(incomplete) = incomplete {
        horned_bin::validation::write_incomplete(incomplete);
        Err(HornedError::CommandError("Validation failed".to_string()))
    } else {
        println!("Validation was successful.");
        Ok(())
    }
}
