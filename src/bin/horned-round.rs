extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::io::reader::read;
use horned_owl::io::writer::write;

use std::io::BufReader;
use std::fs::File;
use std::io::stdout;


fn main() -> Result<(),Error>{
    let matches =
        App::new("horned-round")
        .version("0.1")
        .about("Parse and Render an OWL Ontology")
        .author("Phillip Lord")
        .arg(Arg::with_name("INPUT")
             .help("Sets the input file to use")
             .required(true)
             .index(1))
        .get_matches();

    matcher(matches)
}

fn matcher(matches:ArgMatches) -> Result<(),Error>{
    let input = matches.value_of("INPUT").unwrap();

    let file = File::open(input).unwrap();
    let mut bufreader = BufReader::new(file);
    let (o,p) = read(&mut bufreader)?;

    Ok(write(&mut stdout(),&o,Some(&p)))
}
