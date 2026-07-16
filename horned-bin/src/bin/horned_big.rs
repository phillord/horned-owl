use clap::App;
use clap::Arg;
use clap::ArgMatches;
use clap::arg;

use horned_bin::generate_big_owl;
use horned_owl::error::HornedError;

use std::io::stdout;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-big").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    App::new(name)
        .version(horned_bin::version_string())
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

    let format = matches.value_of("format").expect("oops");
    generate_big_owl(size, format, &mut stdout())?;
    Ok(())
}
