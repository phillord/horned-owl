extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::{command::materialize, error::CommandError};

fn main() -> Result<(), Error> {
    let matches = App::new("horned-materialize")
        .version("0.1")
        .about("Parse an OWL file and download all the imports.")
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
    let input = matches
        .value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let v = materialize(input)?;
    println!("Materialized");
    for i in v {
        println!("\t{:?}", i);
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use assert_cmd::prelude::*; // Add methods on commands
    use predicates::prelude::*; // Used for writing assertions
    use std::{path::Path, process::Command}; // Run programs

    #[test]
    #[ignore]
    fn integration_run() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("horned-materialize")?;

        cmd.assert()
            .failure()
            .stderr(
                predicate::str::contains(
                    "The following required arguments were not provided"));

        Ok(())
    }

    #[test]
    #[ignore]
    fn integration_ont_with_bfo() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("horned-materialize")?;


        let predicate_fn = predicate::path::exists();
        assert!(!predicate_fn.eval(Path::new("./tmp/bfo.owl")));

        cmd.arg("./tmp/ont-with-bfo.owl");
        cmd.assert()
            .success();

        assert!(predicate_fn.eval(Path::new("./tmp/bfo.owl")));

        Ok(())
    }

}
