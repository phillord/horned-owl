extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::command::parse_path;
use horned_owl::error::underlying;
use horned_owl::error::CommandError;
use horned_owl::ontology::axiom_mapped::RcAxiomMappedOntology;

use std::{io::stdout, path::Path};

#[allow(dead_code)]
fn main() -> Result<(), CommandError> {
    let matches = app("horned-round").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static, 'static> {
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

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), CommandError> {
    let input = matches.value_of("INPUT").unwrap();

    let res = parse_path(Path::new(input)).map_err(underlying)?;

    let rtn = match res {
        horned_owl::io::ParserOutput::OWXParser(so, pm) => {
            let amo:RcAxiomMappedOntology = so.into();
            horned_owl::io::owx::writer::write(&mut stdout(), &amo, Some(&pm))
                .map_err(underlying)
        }
        horned_owl::io::ParserOutput::RDFParser(rdfo, _ip) => {
            horned_owl::io::rdf::writer::write(&mut stdout(), &rdfo.into()).map_err(underlying)
        }
    };
    // Finish off nicely
    println!("");

    rtn
}

#[cfg(test)]
mod test {
    use assert_cmd::prelude::*; // Add methods on commands
    use std::process::Command; // Run programs

    #[test]
    fn integration_round_ontology_xml() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("horned-round")?;

        // and.owx is the output from bubo so will not be the
        // syntactically comparible
        cmd.arg("./src/ont/owl-xml/and.owx");
        cmd.assert().success();

        Ok(())
    }

    #[test]
    #[ignore]
    fn integration_round_ontology_rdf() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("horned-round")?;

        // and.owx is the output from bubo so will not be the
        // syntactically comparible
        cmd.arg("./src/ont/owl-rdf/and.owl");
        cmd.assert().success();

        Ok(())
    }
}
