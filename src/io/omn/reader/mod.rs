pub mod from_pair;
pub mod lexer;

pub use from_pair::{Context, FromPair};
pub use lexer::{ManchesterLexer, Rule};

use crate::error::HornedError;
use crate::model::{Build, ClassExpression, ForIRI};

/// Parse a Manchester Syntax class expression from a string.
///
/// `pm` provides prefix expansions for abbreviated IRIs (`prefix:local`);
/// `build` is the IRI intern arena.
pub fn parse_class_expression<A: ForIRI>(
    s: &str,
    pm: &curie::PrefixMapping,
    build: &Build<A>,
) -> Result<ClassExpression<A>, HornedError> {
    // ClassExpressionDocument = _{ SOI ~ Description ~ EOI }
    // The silent rule is transparent: lex() yields Description first, then EOI.
    let description = ManchesterLexer::lex(Rule::ClassExpressionDocument, s)?
        .next()
        .ok_or_else(|| HornedError::invalid("empty class expression"))?;
    let ctx = Context::new(build, pm);
    ClassExpression::from_pair(description, &ctx)
}
