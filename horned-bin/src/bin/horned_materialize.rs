extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::{
    config::{parser_app, parser_config},
    materialize,
};

use horned_owl::error::HornedError;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-materialize").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    parser_app(
        App::new(name)
            .version("0.1")
            .about("Parse an OWL file and download all the imports.")
            .author("Phillip Lord")
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
        .ok_or_else(|| HornedError::CommandError("Command requires a file argument".to_string()))?;

    let v = materialize(input, parser_config(matches))?;

    println!("Materialized");
    for i in v {
        println!("\t{:?}", i);
    }

    Ok(())
}
