extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::{command::materialize, error::CommandError};

#[allow(dead_code)]
fn main() -> Result<(), CommandError> {
    let matches = app("horned-materialize").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static, 'static> {
    App::new(name)
        .version("0.1")
        .about("Parse an OWL file and download all the imports.")
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

        cmd.assert().failure().stderr(predicate::str::contains(
            "The following required arguments were not provided",
        ));

        Ok(())
    }

    #[test]
    #[ignore]
    fn integration_ont_with_bfo() -> Result<(), Box<dyn std::error::Error>> {
        let mut cmd = Command::cargo_bin("horned-materialize")?;

        let predicate_fn = predicate::path::exists();
        assert!(!predicate_fn.eval(Path::new("./tmp/bfo.owl")));

        cmd.arg("./tmp/ont-with-bfo.owl");
        cmd.assert().success();

        assert!(predicate_fn.eval(Path::new("./tmp/bfo.owl")));

        Ok(())
    }
}
