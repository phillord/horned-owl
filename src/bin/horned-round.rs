extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;


use horned_owl::command::parse_path;

use std::{io::{stdout}, path::Path};

fn main() -> Result<(), Error> {
    let matches = App::new("horned-round")
        .version("0.1")
        .about("Parse and Render an OWL Ontology")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .get_matches();

    matcher(matches)
}

fn matcher(matches: ArgMatches) -> Result<(), Error> {
    let input = matches.value_of("INPUT").unwrap();

    let res = parse_path(Path::new(input))?;

    match res {
        horned_owl::io::ParserOutput::OWXParser(so, pm) => {
            horned_owl::io::owx::writer::write(
                &mut stdout(), &so.into(), Some(&pm)
            )
        }
        horned_owl::io::ParserOutput::RDFParser(rdfo, _ip) => {
            horned_owl::io::rdf::writer::write(
                &mut stdout(), &rdfo.into()
            )
        }
    }
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
