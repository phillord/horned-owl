use clap::App;
use clap::ArgMatches;

use horned_owl::error::HornedError;

mod horned_big;
mod horned_dump;
mod horned_materialize;
mod horned_parse;
mod horned_round;
mod horned_summary;
mod horned_triples;
mod horned_unparsed;

fn main() -> Result<(), HornedError> {
    let matches = app().get_matches();
    matcher(matches)
}

fn app() -> App<'static> {
    App::new("horned")
        .version("0.1")
        .about("Command Line tools for OWL Ontologies")
        .author("Filippo De Bortoli <filippo.de_bortoli@tu-dresden.de>")
        .subcommand(horned_big::app("big"))
        .subcommand(horned_dump::app("dump"))
        .subcommand(horned_materialize::app("materialize"))
        .subcommand(horned_parse::app("parse"))
        .subcommand(horned_round::app("round"))
        .subcommand(horned_summary::app("summary"))
        .subcommand(horned_triples::app("triples"))
        .subcommand(horned_unparsed::app("unparsed"))
}

fn matcher(matches: ArgMatches) -> Result<(), HornedError> {
    match matches.subcommand().unwrap() {
        ("big", submatches) => horned_big::matcher(submatches),
        ("dump", submatches) => horned_dump::matcher(submatches),
        ("materialize", submatches) => horned_materialize::matcher(submatches),
        ("parse", submatches) => horned_parse::matcher(submatches),
        ("round", submatches) => horned_round::matcher(submatches),
        ("summary", submatches) => horned_summary::matcher(submatches),
        ("triples", submatches) => horned_triples::matcher(submatches),
        ("unparsed", submatches) => horned_unparsed::matcher(submatches),
        _ => todo!(),
    }
}
