extern crate clap;
extern crate horned_owl;

use clap::arg;
use clap::App;
use clap::Arg;


use clap::ArgMatches;

use horned_owl::error::HornedError;

use rio_api::parser::TriplesParser;

use std::io::BufReader;
use std::{fs::File, io::stdout};

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-triples").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
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
            Arg::with_name("filter")
                .long("filter")
                .takes_value(true)
                .help("Only triples which match given string")
                .required(false),
        )
        .arg(
            Arg::with_name("round")
                .long("round")
                .help("Dump the triples")
                .required(false),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches.value_of("INPUT").ok_or_else(|| HornedError::CommandError(
        "A file name must be specified".to_string(),
    ))?;

    let filter = matches.value_of("filter");

    let file = File::open(input)?;
    let bufreader = BufReader::new(file);
    let v: Vec<String> = rio_xml::RdfXmlParser::new(bufreader, None)
        .into_iter(|rio_triple| {
            Ok(
                (
                    format!("{}", rio_triple.subject),
                    format!("{}", rio_triple.predicate),
                    format!("{}", rio_triple.object),
                )
            )
        })
        .map(|t:Result<_, HornedError>| -> (String,String,String) {t.unwrap()})
        .filter(|t|
                if let Some(f) = filter {
                    t.0.contains(f) ||
                        t.1.contains(f) ||
                        t.2.contains(f)
                }
                else{
                    true
                })
        .map(|t| format!("{}\n\t{}\n\t{}", t.0, t.1, t.2))
        .collect();

    for t in v {
        println!("{}", t);
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
            )?;
        //let mut f = rio_xml::RdfXmlFormatter::with_indentation(&b, 4)?;
        let file = File::open(input)?;
        let bufreader = BufReader::new(file);
        let _: Vec<Result<_, _>> = rio_xml::RdfXmlParser::new(bufreader, None)
            .into_iter(|rio_triple| {
                let t = rio_triple.into();
                f.format(t)
            })
            .collect();
        f.finish()?;
    }

    Ok(())
}
