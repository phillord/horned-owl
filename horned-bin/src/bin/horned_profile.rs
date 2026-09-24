use clap::App;
use clap::Arg;
use clap::ArgMatches;

use horned_bin::{config::parser_config, parse_path};

use horned_owl::error::HornedError;
use horned_owl::model::Build;
use horned_profile::{Profile, ProfileReport, Violation};

use std::collections::BTreeMap;
use std::path::Path;

#[allow(dead_code)]
fn main() -> Result<(), HornedError> {
    let matches = app("horned-profile").get_matches();
    matcher(&matches)
}

pub(crate) fn app(name: &str) -> App<'static> {
    App::new(name)
        .version(horned_bin::version_string())
        .about("Reports which OWL 2 profile(s) (EL/QL/RL/DL) an ontology conforms to")
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
        .arg(Arg::with_name("short").long("short").help(
            "Only print which profile(s) the ontology conforms to, \
             without a violation breakdown",
        ))
}

pub(crate) fn matcher(matches: &ArgMatches) -> Result<(), HornedError> {
    let input = matches
        .value_of("INPUT")
        .ok_or_else(horned_bin::error::error_missing_input)?;

    let b = Build::new();
    let o = parse_path(Path::new(input), parser_config(matches, &b))?
        .decompose()
        .0;

    let profiles = match matches.value_of("profile").unwrap_or("all") {
        "all" => vec![Profile::OWL2DL, Profile::EL, Profile::QL, Profile::RL],
        other => vec![parse_profile(other)?],
    };

    let short = matches.is_present("short");

    for p in profiles {
        let report = horned_profile::check(&o, p);
        if short {
            print_short(profile_name(p), &report);
        } else {
            print_report(profile_name(p), &report);
        }
    }

    Ok(())
}

fn print_short<A: horned_owl::model::ForIRI>(name: &str, report: &ProfileReport<A>) {
    println!(
        "{name}: {}",
        if report.is_conformant() {
            "conformant"
        } else {
            "not conformant"
        }
    );
}

fn print_report<A: horned_owl::model::ForIRI>(name: &str, report: &ProfileReport<A>) {
    if report.is_conformant() {
        println!("{name}: conformant");
        return;
    }

    println!("{name}: {} violation(s)", report.violations().len());

    let mut counts: BTreeMap<&'static str, usize> = BTreeMap::new();
    for v in report.violations() {
        *counts.entry(violation_kind(v)).or_insert(0) += 1;
    }
    for (kind, count) in counts {
        println!("  {kind}: {count}");
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

/// The `Violation` variant's name, for grouping a report's violations by kind.
fn violation_kind<A: horned_owl::model::ForIRI>(v: &Violation<A>) -> &'static str {
    match v {
        Violation::UseOfNonAtomicClassExpression { .. } => "UseOfNonAtomicClassExpression",
        Violation::UseOfNonSubClassExpression { .. } => "UseOfNonSubClassExpression",
        Violation::UseOfNonSuperClassExpression { .. } => "UseOfNonSuperClassExpression",
        Violation::UseOfIllegalClassExpression { .. } => "UseOfIllegalClassExpression",
        Violation::UseOfClassExpressionWithTooFewOperands { .. } => {
            "UseOfClassExpressionWithTooFewOperands"
        }
        Violation::UseOfDataRangeWithTooFewOperands { .. } => "UseOfDataRangeWithTooFewOperands",
        Violation::UseOfBuiltinDatatypeInDatatypeDefinition { .. } => {
            "UseOfBuiltinDatatypeInDatatypeDefinition"
        }
        Violation::UseOfNonSimplePropertyInObjectHasSelf { .. } => {
            "UseOfNonSimplePropertyInObjectHasSelf"
        }
        Violation::UseOfNonSimplePropertyInCardinalityRestriction { .. } => {
            "UseOfNonSimplePropertyInCardinalityRestriction"
        }
        Violation::UseOfNonSimplePropertyInDisjointPropertiesAxiom { .. } => {
            "UseOfNonSimplePropertyInDisjointPropertiesAxiom"
        }
        Violation::UseOfNonSimplePropertyInIrreflexivePropertyAxiom { .. } => {
            "UseOfNonSimplePropertyInIrreflexivePropertyAxiom"
        }
        Violation::UseOfNonSimplePropertyInAsymmetricPropertyAxiom { .. } => {
            "UseOfNonSimplePropertyInAsymmetricPropertyAxiom"
        }
        Violation::UseOfNonSimplePropertyInFunctionalPropertyAxiom { .. } => {
            "UseOfNonSimplePropertyInFunctionalPropertyAxiom"
        }
        Violation::UseOfNonSimplePropertyInInverseFunctionalPropertyAxiom { .. } => {
            "UseOfNonSimplePropertyInInverseFunctionalPropertyAxiom"
        }
        Violation::UseOfPropertyInChainCausingCycle { .. } => "UseOfPropertyInChainCausingCycle",
        Violation::UseOfUndeclaredClass { .. } => "UseOfUndeclaredClass",
        Violation::UseOfUndeclaredObjectProperty { .. } => "UseOfUndeclaredObjectProperty",
        Violation::UseOfUndeclaredDataProperty { .. } => "UseOfUndeclaredDataProperty",
        Violation::UseOfUndeclaredAnnotationProperty { .. } => "UseOfUndeclaredAnnotationProperty",
        Violation::UseOfUndeclaredDatatype { .. } => "UseOfUndeclaredDatatype",
        Violation::UseOfIllegalPunning { .. } => "UseOfIllegalPunning",
        Violation::UseOfReservedVocabulary { .. } => "UseOfReservedVocabulary",
        Violation::UseOfDataOneOfWithMultipleLiterals { .. } => {
            "UseOfDataOneOfWithMultipleLiterals"
        }
        Violation::UseOfObjectPropertyInverse { .. } => "UseOfObjectPropertyInverse",
        Violation::UseOfIllegalAxiomKind { .. } => "UseOfIllegalAxiomKind",
    }
}
