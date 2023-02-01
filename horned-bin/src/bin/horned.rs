use clap::App;
use clap::ArgMatches;

use horned_owl::error::HornedError;

mod horned_big;
mod horned_compare;
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
        .version("0.2")
        .about("Command Line tools for OWL Ontologies")
        .author("Filippo De Bortoli <filippo.de_bortoli@tu-dresden.de>")
        .subcommand_required(true)
        .arg_required_else_help(true)
        .subcommand(horned_big::app("big"))
        .subcommand(horned_compare::app("compare"))
        .subcommand(horned_dump::app("dump"))
        .subcommand(horned_materialize::app("materialize"))
        .subcommand(horned_parse::app("parse"))
        .subcommand(horned_round::app("round"))
        .subcommand(horned_summary::app("summary"))
        .subcommand(horned_triples::app("triples"))
        .subcommand(horned_unparsed::app("unparsed"))
}

fn matcher(matches: ArgMatches) -> Result<(), HornedError> {
    if let Some((name, submatches)) = matches.subcommand() {
        match name {
            "big" => horned_big::matcher(submatches),
            "compare" => horned_compare::matcher(submatches),
            "dump" => horned_dump::matcher(submatches),
            "materialize" => horned_materialize::matcher(submatches),
            "parse" => horned_parse::matcher(submatches),
            "round" => horned_round::matcher(submatches),
            "summary" => horned_summary::matcher(submatches),
            "triples" => horned_triples::matcher(submatches),
            "unparsed" => horned_unparsed::matcher(submatches),
            _ => todo!(),
        }
    } else {
        Ok(())
    }
}
