//! An OWL Manchester Syntax reader written with hand-written parsers.
//!
//! It accepts the same language as the original pest reader (the `horned-pest` crate) and
//! builds the same components, but parses one unit (frame, rule, `Misc`
//! axiom) at a time, so it can stream. See [`Reader`].
use std::io::BufRead;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::io::StreamComponent;
use crate::io::ofn::reader::error::{Origin, PErr};
use crate::model::*;

mod convert;
mod stream;
mod syntax;
#[cfg(test)]
mod tests;

pub use stream::{Reader, read_to_stream};

use convert::{Conv, Declarations};
use syntax::{Syntax, sp};

/// Read an ontology in Manchester Syntax.
///
/// The declarations of data properties and datatypes are collected first, as
/// the pest reader does, so a restriction may come before the frame that
/// declares its property.
pub fn read<
    A: ForIRI,
    B: AsRef<Build<A>>,
    O: MutableOntology<A> + Ontology<A> + Default,
    R: BufRead,
>(
    mut bufread: R,
    config: crate::io::ParserConfiguration<A, B>,
) -> Result<(O, PrefixMapping), HornedError> {
    let mut doc = String::new();
    crate::io::skip_bom(&mut bufread)?;
    bufread.read_to_string(&mut doc)?;
    let decls = stream::prescan(&doc, config.build.as_ref());
    let mut reader = Reader::new(std::io::empty(), config)
        .with_text(doc)
        .with_declarations(decls);
    let mut ont = O::default();
    for item in reader.by_ref() {
        if let StreamComponent::Component(ac) = item? {
            ont.insert(ac);
        }
    }
    Ok((ont, reader.into_prefixes()))
}

/// Parse a Manchester Syntax class expression from a string.
pub fn parse_class_expression<A: ForIRI>(
    s: &str,
    pm: &PrefixMapping,
    build: &Build<A>,
) -> Result<ClassExpression<A>, HornedError> {
    let syn = Syntax::new();
    let to_error = |at: &str, expected: &[&'static str]| {
        let mut e = PErr::expected(at, expected.first().copied().unwrap_or("class expression"));
        for w in expected.iter().skip(1) {
            e = nom::error::ParseError::or(e, PErr::expected(at, w));
        }
        e.into_horned(s, Origin::default())
    };
    let Some((rest, desc)) = syn.description(s) else {
        let (at, exp) = syn.furthest().unwrap_or((s, vec![]));
        return Err(to_error(at, &exp));
    };
    if !sp(rest).is_empty() {
        let (at, exp) = match syn.furthest() {
            Some((at, exp)) if at.len() <= sp(rest).len() => (at, exp),
            _ => (sp(rest), vec!["end of class expression"]),
        };
        return Err(to_error(at, &exp));
    }
    let decls = Declarations::default();
    let conv = Conv {
        build,
        prefixes: pm,
        decls: &decls,
        unit: s,
    };
    conv.description(&desc)
        .map_err(|e| e.into_horned(s, Origin::default()))
}
