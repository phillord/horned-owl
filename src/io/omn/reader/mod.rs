//! OWL Manchester Syntax reader.
//!
//! Targets the full **W3C OWL 2 Manchester Syntax §2.5** grammar — a *general*
//! Manchester reader, not merely the inverse of [`crate::io::omn::write`]. It
//! consumes any valid §2.5 document (OWL-API / ROBOT / Protégé output included),
//! validated against the OWL-API oracle across the rustdl corpus (pizza, family,
//! go-basic, sio/ro/bibtex modules, etc.) — every measured ontology parses fully
//! except where it contains a construct §2.5 cannot express (see residuals).
//!
//! Supported §2.5 surface:
//! - prefix declarations and the `Ontology:` header, including the optional
//!   **version IRI** (`Ontology: <oiri> <viri>`);
//! - the six entity frames (`Class:`, `ObjectProperty:`, `DataProperty:`,
//!   `AnnotationProperty:`, `Individual:`, `Datatype:`), with full clause sets;
//! - **datatype definitions** (`Datatype: D EquivalentTo: <dataRange>`);
//! - **full data ranges** (`and` / `or` / `not` / `{ oneOf }` / parenthesised /
//!   facet `[ … ]` restrictions), not just bare datatypes + a single facet;
//! - **all six §2.5 literal forms** — typed (`"v"^^dt`), string (with/without
//!   language tag), and the bare numeric forms `integerLiteral` / `decimalLiteral`
//!   / `floatingPointLiteral` (the latter requires the §2.5 `f`/`F` suffix),
//!   typed respectively as `xsd:integer` / `xsd:decimal` / `xsd:float`;
//! - the top-level **misc axiom section** (`EquivalentClasses:`,
//!   `DisjointClasses:`, `EquivalentProperties:`, `DisjointProperties:`,
//!   `SameIndividual:`, `DifferentIndividuals:`) over arbitrary expressions;
//! - full per-item `annotatedList`s (each comma-list element may carry its own
//!   leading `Annotations:`);
//! - nested annotation-on-annotation (parsed and **preserved** in
//!   `Annotation::ann`, matching the OFN reader);
//! - **SWRL `Rule:`** rules (`body -> head`), with class / object-property /
//!   data-property / data-range / built-in / same- and different-individual
//!   atoms (atom kinds disambiguated by arity/type and the declaration pre-pass);
//! - **inverse object-property frame subjects** (`ObjectProperty: inverse(p)`);
//! - **annotated declarations** (a leading `Annotations:` before a frame subject
//!   annotates the declaration axiom);
//! - **anonymous (blank-node) individuals** `_:id` as frame subjects, `Facts:`
//!   targets, list members, and annotation values;
//! - bare local names as frame subjects / IRIs.
//!
//! ## Residual constructs the reader cannot represent
//!
//! All residuals are either inherent (no §2.5 form exists) or a horned-owl model
//! limit — none is a §2.5 reader gap. SWRL `Rule:`, inverse-headed property
//! frames (`ObjectProperty: inverse(p)`), annotated declarations, and
//! anonymous-subject annotation assertions (`Individual: _:id`) are all fully
//! read *and* written natively, so they round-trip.
//! - **Complex-LHS general class axioms** — a `SubClassOf` whose subject is a
//!   complex expression has no §2.5 frame form; the writer emits it to the
//!   trailing `# General axioms` functional-syntax block, which the reader
//!   **skips with a warning**. Inherent (no §2.5 form).
//! - **Writer normalisation (round-trip only):** the reader reads §2.5
//!   `annotatedList`s correctly — a leading clause-level annotation binds the
//!   FIRST list item only, and each post-comma `Annotations:` binds the
//!   following item only. The *writer*, however, emits one clause per axiom, so
//!   a multi-item annotated list is re-serialised as separate single-item
//!   clauses. This is lossless (every axiom + its own annotations is preserved),
//!   just structurally normalised.
//! - Frame headers conflate declaration and reference: every frame yields a
//!   `Declare*` axiom, so an entity used without an explicit declaration gains
//!   one on round-trip. Declarations are non-logical (entailment-neutral).
//! - n-ary `EquivalentTo:`/`DisjointWith:`/`SameAs:`/`DifferentFrom:` lists are
//!   read as a SINGLE n-ary axiom with the frame subject prepended (the exact
//!   inverse of the writer), not OWL-API's pairwise expansion.
//! - A bare local name emitted by the writer only when a default `""` prefix is
//!   registered is not lexable; use `<full>` or `prefix:local`. Round-tripping a
//!   bare name requires a non-default prefix.
//! - **`HasKey:` object-vs-data key conflation.** Manchester `HasKey:` provides
//!   no lexical distinction between object and data property keys. Data-property
//!   keys are read back as `ObjectPropertyExpression` members; a round-trip
//!   containing data-property keys will not reconstruct the original component.
//!   Use object-property-only key lists to guarantee round-trip fidelity.
//! - **Data-property restrictions parse as OBJECT restrictions (silent).** The
//!   grammar's data-property restriction arms are dead PEG productions (a data
//!   property and an object property are lexically identical), so a restriction
//!   such as `dp some xsd:integer` is parsed as an `ObjectSomeValuesFrom` over a
//!   `Class`-typed datatype IRI, with no error. Pre-existing (predates the frame
//!   reader); disambiguation is deferred to a future phase.
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
pub fn read<
    A: ForIRI,
    B: AsRef<Build<A>>,
    O: MutableOntology<A> + Ontology<A> + Default,
    R: BufRead,
>(
    bufread: R,
    config: ParserConfiguration<A, B>,
) -> Result<(O, PrefixMapping), HornedError> {
    read_with_build(bufread, config.build.as_ref())
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

    // Pass 1.5: collect DataProperty / Datatype declarations so that HasKey
    // keys, Misc EquivalentProperties/DisjointProperties lists, and bare-IRI
    // restriction fillers can be typed correctly in pass 2.  We clone the
    // pairs (pass 2 owns the originals); IRI resolution uses the prefix
    // mapping built above.
    let declarations =
        from_pair::declarations_from_frames(children.iter().cloned(), build, &prefixes);

    // Pass 2: build the ontology under a prefix-aware, declaration-aware context.
    let ctx = Context::with_decls(build, &prefixes, &declarations);
    let mut ontology: O = Default::default();

    for child in children {
        match child.as_rule() {
            Rule::PrefixDeclaration | Rule::EOI => {}
            Rule::OntologyHeader => {
                // OntologyHeader = { ^"Ontology:" ~ ( OntologyIRI ~ VersionIRI? )?
                //                    ~ ImportDeclaration* ~ Annotations* }
                // Iterate children: optional OntologyIRI then optional VersionIRI,
                // then zero or more ImportDeclaration, then zero or more
                // Annotations (ontology annotations).
                // GATE: insert OntologyID only when an IRI/version was present —
                // NOT merely because the `Ontology:` keyword appeared. A bare
                // `Ontology:` (emitted to host imports/annotations when there is no
                // ontology IRI) must NOT inject a spurious OntologyID(None,None).
                let mut oid = crate::model::OntologyID::default();
                let mut has_id = false;
                for h in child.into_inner() {
                    match h.as_rule() {
                        Rule::OntologyIRI => {
                            let iri_pair = h.into_inner().next().unwrap();
                            oid.iri = Some(crate::model::IRI::from_pair(iri_pair, &ctx)?);
                            has_id = true;
                        }
                        Rule::VersionIRI => {
                            let iri_pair = h.into_inner().next().unwrap();
                            oid.viri = Some(crate::model::IRI::from_pair(iri_pair, &ctx)?);
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
            Rule::Misc => from_pair::insert_misc(child, &ctx, &mut ontology)?,
            Rule::GeneralAxiomBlock => {
                // The writer emits genuinely-inexpressible components (SWRL
                // rules, anonymous-subject class axioms, annotation assertions
                // on undeclared subjects, ...) as full-IRI OWL functional-syntax
                // lines under a `# General axioms` marker. Delegate them back to
                // the functional-syntax reader — sharing our IRI `build` so IRIs
                // intern consistently — rather than dropping them, so the block
                // round-trips instead of silently losing axioms.
                let body = child.as_str();
                let axioms = body.strip_prefix("# General axioms").unwrap_or(body).trim();
                if !axioms.is_empty() {
                    // Wrap the bare axiom lines in a minimal anonymous
                    // `Ontology(...)` document. The fallback is always rendered
                    // with fully-qualified IRIs, so no prefix declarations are
                    // needed.
                    let wrapped = format!("Ontology(\n{axioms}\n)");
                    let parsed: Result<(crate::ontology::set::SetOntology<A>, _), _> =
                        crate::io::ofn::reader::read(
                            std::io::Cursor::new(wrapped.into_bytes()),
                            crate::io::ParserConfiguration::new(build),
                        );
                    match parsed {
                        Ok((block_ont, _)) => {
                            for ac in block_ont {
                                // The synthetic `Ontology(...)` wrapper yields an
                                // empty `OntologyID`/`DocIRI`; the block carries
                                // only axioms, so drop any ontology-identity
                                // component it introduces.
                                if matches!(
                                    ac.component,
                                    crate::model::Component::OntologyID(_)
                                        | crate::model::Component::DocIRI(_)
                                ) {
                                    continue;
                                }
                                ontology.insert(ac);
                            }
                        }
                        // Never turn a previously-readable document into a hard
                        // error: if the fallback block cannot be parsed (e.g. it
                        // contains functional syntax the writer emitted but the
                        // reader cannot yet round-trip), warn and skip it, the
                        // pre-delegation behaviour.
                        Err(e) => {
                            let n = axioms.lines().filter(|l| !l.trim().is_empty()).count();
                            eprintln!(
                                "warning: omn reader could not parse the {n}-line \
                                 `# General axioms` block ({e}); skipping it"
                            );
                        }
                    }
                }
            }
            rule => unreachable!("unexpected document child: {:?}", rule),
        }
    }

    Ok((ontology, prefixes))
}
