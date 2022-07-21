extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::command::parse_path;
use horned_owl::error::HornedError;
use horned_owl::command::config::{parser_app, parser_config};

use std::path::Path;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-parse").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    parser_app(
        App::new(name)
            .version("0.1")
            .about("Parse an OWL File")
            .author("Phillip Lord")
            .arg(
                Arg::with_name("INPUT")
                    .help("Sets the input file to use")
                    .required(true)
                    .index(1),
            )
    )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches.value_of("INPUT").ok_or_else(|| HornedError::CommandError(
        "Command requires an INPUT parameter".to_string(),
    ))?;

    parse_path(
        Path::new(input),
        parser_config(matches)
    )?;

    println!("Parse Complete: {:?}", input);
    Ok(())
}
