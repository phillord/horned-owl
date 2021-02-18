extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::error::CommandError;

use rio_api::{formatter::TriplesFormatter, model::Triple, parser::TriplesParser};
use rio_xml::RdfXmlFormatter;

use std::{fs::File, io::stdout};
use std::io::BufReader;

fn main() -> Result<(), Error> {
    let matches = App::new("horned-triples")
        .version("0.1")
        .about("Parse RDF and dump the triples")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("round")
                .long("round")
                .help("Dump the triples")
                .required(false)
        )
        .get_matches();

    matcher(matches)
}

fn matcher(matches: ArgMatches) -> Result<(), Error> {
    let input = matches
        .value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let file = File::open(input)?;
    let bufreader = BufReader::new(file);
    let v: Vec<Result<String, Error>>
        = rio_xml::RdfXmlParser::new(bufreader, None).into_iter(
            |rio_triple| Ok(
                format!("{}\n\t{}\n\t{}",
                        rio_triple.subject,
                        rio_triple.predicate,
                        rio_triple.object
                )
            )
    ).collect();

    for t in v {
        println!("{}", t.unwrap());
    }

    if matches.is_present("round") {
        println!("\n");
        let b = stdout();
        let mut f = RdfXmlFormatter::with_indentation(&b, 4)?;
        let file = File::open(input)?;
        let bufreader = BufReader::new(file);
        let _: Vec<Result<_, _>>
            = rio_xml::RdfXmlParser::new(bufreader, None).into_iter(
                |rio_triple| f.format(&rio_triple)
            ).collect();
        f.finish()?;
    }

    Ok(())
}
