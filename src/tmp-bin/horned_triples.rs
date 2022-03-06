extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::error::CommandError;

use horned_owl::error::underlying;
use rio_api::parser::TriplesParser;

use std::io::BufReader;
use std::{fs::File, io::stdout};

#[allow(dead_code)]
fn main() -> Result<(), CommandError> {
    let matches = app("horned-triples").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static, 'static> {
    App::new(name)
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
                .required(false),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), CommandError> {
    let input = matches
        .value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let file = File::open(input).map_err(underlying)?;
    let bufreader = BufReader::new(file);
    let v: Vec<Result<String, CommandError>> = rio_xml::RdfXmlParser::new(bufreader, None)
        .into_iter(|rio_triple| {
            Ok(format!(
                "{}\n\t{}\n\t{}",
                rio_triple.subject, rio_triple.predicate, rio_triple.object
            ))
        })
        .collect();

    for t in v {
        println!("{}", t.unwrap());
    }

    if matches.is_present("round") {
        println!("\nRoundTripping:\n");
        let b = stdout();
        //let b = std::io::sink();

        let mut p = indexmap::IndexMap::new();
        p.insert(
            "http://www.w3.org/2002/07/owl#".to_string(),
            "owl".to_string(),
        );
        p.insert(
            "http://www.w3.org/XML/1998/namespace".to_string(),
            "xml".to_string(),
        );
        p.insert(
            "http://www.w3.org/2001/XMLSchema#".to_string(),
            "xsd".to_string(),
        );
        p.insert(
            "http://www.w3.org/2000/01/rdf-schema#".to_string(),
            "rdfs".to_string(),
        );

        let mut f: pretty_rdf::PrettyRdfXmlFormatter<String, _> =
            pretty_rdf::PrettyRdfXmlFormatter::new(
                b,
                pretty_rdf::ChunkedRdfXmlFormatterConfig::all(),
            )
            .map_err(underlying)?;
        //let mut f = rio_xml::RdfXmlFormatter::with_indentation(&b, 4)?;
        let file = File::open(input).map_err(underlying)?;
        let bufreader = BufReader::new(file);
        let _: Vec<Result<_, _>> = rio_xml::RdfXmlParser::new(bufreader, None)
            .into_iter(|rio_triple| {
                let t = rio_triple.into();
                f.format(t)
            })
            .collect();
        f.finish().map_err(underlying)?;
    }

    Ok(())
}
