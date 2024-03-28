use std::io::BufRead;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::io::ParserConfiguration;
use crate::model::Build;
use crate::model::ForIRI;
use crate::model::RcStr;
use crate::ontology::set::SetOntology;

mod from_pair;
mod lexer;

use self::from_pair::FromPair;
use self::lexer::OwlFunctionalLexer;
use self::lexer::Rule;

struct Context<'a, A: ForIRI> {
    build: &'a Build<A>,
    mapping: &'a PrefixMapping,
}

impl<'a, A: ForIRI> Context<'a, A> {
    fn new(build: &'a Build<A>, mapping: &'a PrefixMapping) -> Self {
        Self { build, mapping }
    }
}

pub fn read<R: BufRead>(
    mut bufread: R,
    _config: ParserConfiguration,
) -> Result<(SetOntology<RcStr>, PrefixMapping), HornedError> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

pub fn read_with_build<A: ForIRI, R: BufRead>(
    mut bufread: R,
    build: &Build<A>,
) -> Result<(SetOntology<A>, PrefixMapping), HornedError> {
    let prefixes = PrefixMapping::default();
    let ctx = Context::new(build, &prefixes);

    // FIXME: implement iterative parser (this is possible in )
    let mut doc = String::new();
    bufread.read_to_string(&mut doc);
    let pair = OwlFunctionalLexer::lex(Rule::OntologyDocument, doc.trim())?
        .next()
        .unwrap();

    FromPair::from_pair(pair, &ctx)
}
