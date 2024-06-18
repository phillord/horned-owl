use clap::arg;
use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::write;

use horned_owl::error::HornedError;
use horned_owl::model::Build;
use horned_owl::model::MutableOntology;
use horned_owl::model::OntologyID;
use horned_owl::ontology::component_mapped::RcComponentMappedOntology;
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
            arg!(
                --format <FORMAT> "Which format to write to"
            )
            .default_value("owl")
            .required(false),
        )
        .arg(
            Arg::with_name("SIZE")
                .help("The number of classes the file should have")
                .required(true)
                .index(1),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let size: isize =
        matches.value_of("SIZE").unwrap().parse().map_err(|_| {
            HornedError::CommandError("Cannot parse SIZE as an integer".to_string())
        })?;

    let b = Build::new_rc();
    let mut o = SetOntology::new_rc();

    o.insert(OntologyID {
        iri: Some(b.iri("http://www.example.com/iri")),
        viri: None,
    });

    for i in 1..size + 1 {
        o.declare(b.class(format!("https://www.example.com/o{}", i)));
    }

    let amo: RcComponentMappedOntology = o.into();
    write(
        matches.value_of("format").expect("oops"),
        &mut stdout(),
        &amo,
    )
}
