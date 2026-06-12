//! OWL Manchester Syntax reader.
//!
//! Parses prefix declarations, an optional `Ontology:` header, and the six
//! entity frames (`Class:`, `ObjectProperty:`, `DataProperty:`,
//! `AnnotationProperty:`, `Individual:`, `Datatype:`) into a mutable ontology.
//! It is the structural inverse of [`crate::io::omn::write`].
//!
//! ## Known limitations (P3)
//! - The writer's trailing `# General axioms` block (OWL functional syntax for
//!   components with no native Manchester form — `Import`, `HasKey`,
//!   `OntologyAnnotation`, axiom annotations, SWRL rules, property chains,
//!   n-ary axioms over anonymous subjects) is NOT parsed. The `#` line itself
//!   is consumed as a `COMMENT`, but the functional-syntax lines beneath it
//!   match no `Frame` rule, so a document carrying a non-empty misc block is
//!   REJECTED at EOI (a hard parse error) rather than partially parsed. Such
//!   documents do not round-trip; the round-trip corpus avoids misc-only
//!   axioms.
//! - Frame headers conflate declaration and reference: every frame yields a
//!   `Declare*` axiom, so an entity used without an explicit declaration gains
//!   one on round-trip. Declarations are non-logical (entailment-neutral).
//! - n-ary `EquivalentTo:`/`DisjointWith:`/`SameAs:`/`DifferentFrom:` lists are
//!   read as a SINGLE n-ary axiom with the frame subject prepended (the exact
//!   inverse of the writer), not OWL-API's pairwise expansion.
//! - A bare local name as a frame subject or IRI (emitted by the writer only
//!   when a default `""` prefix is registered) is not lexable; use `<full>` or
//!   `prefix:local`. Round-tripping requires a non-default prefix.
//! - `Annotations:` clauses are not parsed (the writer does not emit them).
//! - **Keyword / CURIE-prefix collision (correctness gap, MUST FIX BEFORE the
//!   upstream PR).** Manchester keywords (`not`, `and`, `or`, `some`, `only`,
//!   `value`, `min`, `max`, `exactly`, `Self`, `inverse`, and the facet words)
//!   are matched without a name-boundary, so an *abbreviated* CURIE whose
//!   prefix begins with a keyword is silently mis-parsed — e.g. `notation:foo`
//!   lexes as `not` + `ation:foo`, and `andx:bar` as `and` + `x:bar`. Full
//!   `<...>` IRIs are immune (they start with `<`). This reader round-trips the
//!   **writer's own output** completely (the writer never emits such CURIEs),
//!   but it is therefore NOT yet a general hand-written-Manchester parser. The
//!   fix is maximal-munch boundary anchoring on every keyword token —
//!   `@{ ^"not" ~ !PnChar }` rather than a trailing-whitespace guard (which
//!   would break `not(C and D)`) — applied across BOTH the P2 class-expression
//!   rules and the P3 frame rules, with a per-keyword negative test and the
//!   full P2 round-trip suite as regression. See the pre-upstream-PR list.

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
