extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::command::parse_path;
use horned_owl::error::CommandError;

use std::path::Path;

#[allow(dead_code)]
fn main() -> Result<(), CommandError> {
    let matches = app("horned-parse").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static, 'static> {
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
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), CommandError> {
    let input = matches
        .value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    parse_path(Path::new(input))?;

    println!("Parse Complete: {:?}", input);
    Ok(())
}

