use clap::App;
use clap::ArgMatches;

use horned_owl::error::HornedError;

mod horned_big;
mod horned_compare;
mod horned_convert;
mod horned_dump;
mod horned_materialize;
mod horned_parse;
mod horned_round;
mod horned_summary;
mod horned_triples;
mod horned_unparsed;
mod horned_validate;

fn main() -> Result<(), HornedError> {
    let matches = app().get_matches();
    matcher(matches)
}

fn app() -> App<'static> {
    horned_bin::config::parser_app_global(
        App::new("horned")
            .version("0.3")
            .about("Command Line tools for OWL Ontologies")
            .author("Filippo De Bortoli <filippo.de_bortoli@tu-dresden.de>,\nPhillip Lord <phillip.lord@newcastle.ac.uk ")
            .subcommand_required(true)
            .arg_required_else_help(true),
    )
    .subcommand(horned_big::app("big"))
    .subcommand(horned_compare::app("compare"))
    .subcommand(horned_convert::app("convert"))
    .subcommand(horned_dump::app("dump"))
    .subcommand(horned_materialize::app("materialize"))
    .subcommand(horned_parse::app("parse"))
    .subcommand(horned_round::app("round"))
    .subcommand(horned_summary::app("summary"))
    .subcommand(horned_triples::app("triples"))
    .subcommand(horned_unparsed::app("unparsed"))
    .subcommand(horned_validate::app("validate"))
}

fn matcher(matches: ArgMatches) -> Result<(), HornedError> {
    if let Some((name, submatches)) = matches.subcommand() {
        match name {
            "big" => horned_big::matcher(submatches),
            "compare" => horned_compare::matcher(submatches),
            "convert" => horned_convert::matcher(submatches),
            "dump" => horned_dump::matcher(submatches),
            "materialize" => horned_materialize::matcher(submatches),
            "parse" => horned_parse::matcher(submatches),
            "round" => horned_round::matcher(submatches),
            "summary" => horned_summary::matcher(submatches),
            "triples" => horned_triples::matcher(submatches),
            "unparsed" => horned_unparsed::matcher(submatches),
            "validate" => horned_validate::matcher(submatches),
            _ => unreachable!("clap guarantees name is one of the registered subcommands"),
        }
    } else {
        Ok(())
    }
}
