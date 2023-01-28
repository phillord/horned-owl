use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::error::HornedError;
use horned_owl::io::owx::writer::write;
use horned_owl::model::Build;
use horned_owl::model::MutableOntology;
use horned_owl::ontology::axiom_mapped::RcAxiomMappedOntology;
use horned_owl::ontology::set::SetOntology;

use std::io::stdout;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-big").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    App::new(name)
        .version("0.1")
        .about("Generate a big OWL file for testing")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("SIZE")
                .help("The number of classes the file should have")
                .required(true)
                .index(1),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let size: isize =
        matches
            .value_of("SIZE")
            .unwrap()
            .parse()
            .map_err(|_| HornedError::CommandError("Cannot parse SIZE as an integer".to_string()))?;

    let b = Build::new_rc();
    let mut o = SetOntology::new_rc();

    for i in 1..size + 1 {
        o.declare(b.class(format!("https://www.example.com/o{}", i)));
    }

    let amo: RcAxiomMappedOntology = o.into();
    write(&mut stdout(), &amo, None)
}
