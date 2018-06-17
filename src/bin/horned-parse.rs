extern crate clap;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_owl::io::reader::read;

use std::io::BufReader;
use std::fs::File;

fn main() {
    let matches =
        App::new("horned-parse")
        .version("0.1")
        .about("Parse an OWL File")
        .author("Phillip Lord")
        .arg(Arg::with_name("INPUT")
             .help("Sets the input file to use")
             .required(true)
             .index(1))
        .get_matches();

    matcher(matches);
}

fn matcher(matches:ArgMatches){
    let input = matches.value_of("INPUT").unwrap();

    let file = File::open(input).unwrap();
    let mut bufreader = BufReader::new(file);
    let (_,_) = read(&mut bufreader);

    println!("Parse Complete: {:?}", input);
}
