extern crate clap;
extern crate horned_owl;
extern crate horned_profile;

use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::{config::parser_config, parse_path};

use horned_owl::error::HornedError;
use horned_profile::{Profile, ProfileReport};

use std::path::Path;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-profile-check").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    App::new(name)
        .version(horned_bin::version_string())
        .about("Checks which OWL 2 profile(s) (EL/QL/RL/DL) an ontology conforms to")
        .author("Phillip Lord")
        .arg(
            Arg::with_name("INPUT")
                .help("Sets the input file to use")
                .required(true)
                .index(1),
        )
        .arg(
            Arg::with_name("profile")
                .long("profile")
                .takes_value(true)
                .default_value("all")
                .help("Which profile to check: dl, el, ql, rl, or all (default)"),
        )
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches
        .value_of("INPUT")
        .ok_or_else(horned_bin::error::error_missing_input)?;

    let o = parse_path(Path::new(input), parser_config(matches))?
        .decompose()
        .0;

    match matches.value_of("profile").unwrap_or("all") {
        "all" => {
            for p in [Profile::OWL2DL, Profile::EL, Profile::QL, Profile::RL] {
                print_summary(profile_name(p), &horned_profile::check(&o, p));
            }
            Ok(())
        }
        other => {
            let profile = parse_profile(other)?;
            let report = horned_profile::check(&o, profile);
            if report.is_conformant() {
                println!("Conformant to {}.", profile_name(profile));
                Ok(())
            } else {
                for v in report.violations() {
                    println!("{v:?}");
                }
                Err(HornedError::CommandError(format!(
                    "Not conformant to {}: {} violation(s)",
                    profile_name(profile),
                    report.violations().len()
                )))
            }
        }
    }
}

fn print_summary<A: horned_owl::model::ForIRI>(name: &str, report: &ProfileReport<A>) {
    if report.is_conformant() {
        println!("{name}: conformant");
    } else {
        println!("{name}: {} violation(s)", report.violations().len());
    }
}

fn parse_profile(s: &str) -> Result<Profile, HornedError> {
    match s {
        "dl" => Ok(Profile::OWL2DL),
        "el" => Ok(Profile::EL),
        "ql" => Ok(Profile::QL),
        "rl" => Ok(Profile::RL),
        other => Err(HornedError::CommandError(format!(
            "Unknown profile '{other}': expected one of dl, el, ql, rl, all"
        ))),
    }
}

fn profile_name(p: Profile) -> &'static str {
    match p {
        Profile::OWL2DL => "OWL 2 DL",
        Profile::EL => "OWL 2 EL",
        Profile::QL => "OWL 2 QL",
        Profile::RL => "OWL 2 RL",
    }
}
