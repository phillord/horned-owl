pub mod from_pair;
pub mod lexer;

pub use from_pair::{Context, FromPair};
pub use lexer::{ManchesterLexer, Rule};

use std::io::BufRead;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::io::ParserConfiguration;
use crate::model::{Build, ClassExpression, ForIRI, MutableOntology, Ontology};

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

/// Read a whole ontology from a Manchester Syntax document, using a fresh IRI
/// `Build`.  Mirrors `io::ofn::reader::read`.
///
/// The `# General axioms` block emitted by the writer for components lacking a
/// native Manchester form (in OWL functional syntax) is NOT parsed — see the
/// P3 limitations note.
pub fn read<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default, R: BufRead>(
    bufread: R,
    _config: ParserConfiguration,
) -> Result<(O, PrefixMapping), HornedError> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

/// Read a whole ontology, interning IRIs into the supplied `build`.
pub fn read_with_build<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default, R: BufRead>(
    mut bufread: R,
    build: &Build<A>,
) -> Result<(O, PrefixMapping), HornedError> {
    let mut doc = String::new();
    bufread.read_to_string(&mut doc)?;

    let document = ManchesterLexer::lex(Rule::ManchesterDocument, doc.trim())?
        .next()
        .ok_or_else(|| HornedError::invalid("empty Manchester document"))?;

    // Collect the document's children so we can make two passes.
    let children: Vec<_> = document.into_inner().collect();

    // Pass 1: build the prefix mapping from PrefixDeclaration children.
    let prefixes = from_pair::prefixes_from_decls(
        children
            .iter()
            .filter(|p| p.as_rule() == Rule::PrefixDeclaration)
            .cloned(),
    )?;

    // Pass 2: build the ontology under a prefix-aware context.
    let ctx = Context::new(build, &prefixes);
    let mut ontology: O = Default::default();
    let mut ontology_id = crate::model::OntologyID::default();
    let mut header_present = false;

    for child in children {
        match child.as_rule() {
            Rule::PrefixDeclaration | Rule::EOI => {}
            Rule::OntologyHeader => {
                header_present = true;
                if let Some(iri_pair) = child.into_inner().next() {
                    ontology_id.iri = Some(crate::model::IRI::from_pair(iri_pair, &ctx)?);
                }
            }
            Rule::Frame => from_pair::insert_frame(child, &ctx, &mut ontology)?,
            rule => unreachable!("unexpected document child: {:?}", rule),
        }
    }
    // Only insert an OntologyID when the document actually carried an
    // `Ontology:` header. A fresh `SetOntology` does NOT seed one, and the
    // writer omits the header for a default (empty) OntologyID — so inserting
    // `OntologyID::default()` unconditionally (as ofn does) would add a
    // spurious `Component::OntologyID(None, None)` and break the round-trip
    // against a hand-built ontology that never declared an ID.
    if header_present {
        ontology.insert(ontology_id);
    }

    Ok((ontology, prefixes))
}
