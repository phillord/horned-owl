extern crate clap;
extern crate failure;
extern crate horned_owl;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use failure::Error;

use horned_owl::error::CommandError;

use std::io::BufReader;
use std::fs::File;
use std::rc::Rc;

use sophia::term::Term;

type SpTerm = Term<Rc<str>>;


fn p_tup(triple: &[SpTerm; 3]) {
    println!(
        "{}\n\t{}\n\t{}",
        &triple[0].n3(),
        &triple[1].n3(),
        &triple[2].n3()
    );
}


fn main() -> Result<(),Error> {
    let matches =
        App::new("horned-parse")
        .version("0.1")
        .about("Parse an OWL File and dump the data structures")
        .author("Phillip Lord")
        .arg(Arg::with_name("INPUT")
             .help("Sets the input file to use")
             .required(true)
             .index(1))
        .get_matches();

    matcher(matches)
}

fn matcher(matches:ArgMatches) -> Result<(),Error> {
    let input = matches.value_of("INPUT")
        .ok_or(CommandError::MissingArgument)?;

    let file = File::open(input)?;
    let bufreader = BufReader::new(file);
    let parser = sophia::parser::xml::Config::default();
    let triple_iter = parser.parse_bufread(bufreader);

    for i in triple_iter {
        p_tup(&i.unwrap());
    }

    Ok(())
}
