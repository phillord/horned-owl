extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::command::parse_path;
use horned_owl::error::HornedError;
use horned_owl::ontology::axiom_mapped::RcAxiomMappedOntology;

use std::{io::stdout, path::Path};

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-round").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    App::new(name)
        .version("0.1")
        .about("Parse and Render an OWL Ontology")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches.value_of("INPUT").unwrap();

    let res = parse_path(Path::new(input))?;

    let rtn = match res {
        horned_owl::io::ParserOutput::OWXParser(so, pm) => {
            let amo: RcAxiomMappedOntology = so.into();
            horned_owl::io::owx::writer::write(&mut stdout(), &amo, Some(&pm))
        }
        horned_owl::io::ParserOutput::RDFParser(rdfo, _ip) => {
            horned_owl::io::rdf::writer::write(&mut stdout(), &rdfo.into())
        }
    };
    // Finish off nicely
    println!();

    rtn
}
