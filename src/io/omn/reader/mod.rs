//! OWL Manchester Syntax reader.
//!
//! Parses prefix declarations, an optional `Ontology:` header, and the six
//! entity frames (`Class:`, `ObjectProperty:`, `DataProperty:`,
//! `AnnotationProperty:`, `Individual:`, `Datatype:`) into a mutable ontology.
//! It is the structural inverse of [`crate::io::omn::write`].
//!
//! ## Known limitations
//! - The writer's trailing `# General axioms` block (OWL functional syntax for
//!   genuinely-inexpressible components — general anonymous-subject class axioms,
//!   SWRL rules) is **skipped with a warning** (its axioms are dropped, not
//!   round-tripped). The frame portion of the document is parsed normally.
//! - Frame headers conflate declaration and reference: every frame yields a
//!   `Declare*` axiom, so an entity used without an explicit declaration gains
//!   one on round-trip. Declarations are non-logical (entailment-neutral).
//! - n-ary `EquivalentTo:`/`DisjointWith:`/`SameAs:`/`DifferentFrom:` lists are
//!   read as a SINGLE n-ary axiom with the frame subject prepended (the exact
//!   inverse of the writer), not OWL-API's pairwise expansion.
//! - A bare local name as a frame subject or IRI (emitted by the writer only
//!   when a default `""` prefix is registered) is not lexable; use `<full>` or
//!   `prefix:local`. Round-tripping requires a non-default prefix.
//! - **`HasKey:` object-vs-data key conflation.** Manchester `HasKey:` provides
//!   no lexical distinction between object and data property keys. Data-property
//!   keys are read back as `ObjectPropertyExpression` members; a round-trip
//!   containing data-property keys will not reconstruct the original component.
//!   Use object-property-only key lists to guarantee round-trip fidelity.
//! - **Entity annotations on an IRI heading no frame are dropped.** An
//!   `AnnotationAssertion` whose subject IRI does not correspond to any declared
//!   entity frame is emitted to the `# General axioms` misc block (and thus
//!   skipped by the reader). Only annotations on entities with a corresponding
//!   `Class:` / `ObjectProperty:` / … frame round-trip.
//! - **Anonymous-individual annotation values are not rendered.** An
//!   `AnnotationValue::AnonymousIndividual` has no Manchester literal form and
//!   is routed to the misc block (dropped on read).
//! - **Axiom annotations on misc-routed axioms are dropped.** Complex-LHS
//!   `SubClassOf`, anonymous-subject assertions, `DatatypeDefinition`, and SWRL
//!   rules cannot be expressed in a Manchester frame clause, so they land in the
//!   misc block. The misc fallback emits the bare OWL functional-syntax
//!   component with no annotation; the annotation is silently lost.
//! - **Annotation nesting is not representable.** The horned-owl model has no
//!   `ann` field on `Annotation`; nested annotations (annotation-on-annotation)
//!   are discarded by both the OFN and OMN readers and are never preserved.
//! - **FIXED (commit e7a2b83): keyword / CURIE-prefix collision.** Manchester
//!   keywords (`not`, `and`, `or`, `some`, `only`, `value`, `min`, `max`,
//!   `exactly`, `Self`, `inverse`, and the facet words) now carry a
//!   `!( SPARQL_PnChars | ":" )` maximal-munch boundary so a CURIE whose prefix
//!   begins with a keyword (e.g. `notation:Foo`) is no longer mis-split.

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
/// native Manchester form is skipped with a warning — see the limitations note
/// in the module doc.
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

    for child in children {
        match child.as_rule() {
            Rule::PrefixDeclaration | Rule::EOI => {}
            Rule::OntologyHeader => {
                // OntologyHeader = { ^"Ontology:" ~ IRI? ~ ImportDeclaration* ~ Annotations* }
                // Iterate children: optional IRI, then zero or more ImportDeclaration,
                // then zero or more Annotations (ontology annotations).
                // GATE: insert OntologyID only when an IRI/version was present —
                // NOT merely because the `Ontology:` keyword appeared. A bare
                // `Ontology:` (emitted to host imports/annotations when there is no
                // ontology IRI) must NOT inject a spurious OntologyID(None,None).
                let mut oid = crate::model::OntologyID::default();
                let mut has_id = false;
                for h in child.into_inner() {
                    match h.as_rule() {
                        Rule::IRI => {
                            oid.iri = Some(crate::model::IRI::from_pair(h, &ctx)?);
                            has_id = true;
                        }
                        Rule::ImportDeclaration => {
                            let iri_pair = h.into_inner().next().unwrap();
                            ontology.insert(crate::model::Import(crate::model::IRI::from_pair(
                                iri_pair, &ctx,
                            )?));
                        }
                        Rule::Annotations => {
                            for ann in from_pair::parse_annotations(h, &ctx)? {
                                ontology.insert(crate::model::OntologyAnnotation(ann));
                            }
                        }
                        rule => {
                            unreachable!("unexpected ontology-header child: {:?}", rule)
                        }
                    }
                }
                if has_id {
                    ontology.insert(oid);
                }
            }
            Rule::Frame => from_pair::insert_frame(child, &ctx, &mut ontology)?,
            Rule::GeneralAxiomBlock => {
                let body = child.as_str();
                let n = body
                    .lines()
                    .filter(|l| {
                        !l.trim().is_empty() && !l.trim_start().starts_with("# General axioms")
                    })
                    .count();
                eprintln!(
                    "warning: omn reader skipped {n} axiom(s) in the non-Manchester \
                     `# General axioms` block (components with no Manchester form)"
                );
            }
            rule => unreachable!("unexpected document child: {:?}", rule),
        }
    }

    Ok((ontology, prefixes))
}
