//! Mapping from lexed OBO pest pairs to horned-owl components.
//!
//! Follows the OBO 1.4 → OWL 2 mapping defined by the OWL-API `oboformat`
//! writer, which is the acceptance oracle for this reader (issue #181):
//! `compare(read(.obo), read(published ROBOT .owl))` over real ontologies.
//!
//! Mapping decisions are seeded from `owlmake/src/io/obo.rs` (@jamesamcl) and
//! cross-checked against `fastobo-owl` (@althonos); where the two disagree, the
//! divergence is pinned by an oracle test.
//!
//! STATUS: header + `[Term]` frames are mapped (the common metadata + logical
//! surface). `[Typedef]` and `[Instance]` frames, trailing `{qualifier}` axiom
//! annotations, and the `treat-xrefs-*` macros are still TODO.

use std::collections::{BTreeSet, HashMap};

use curie::PrefixMapping;
use pest::iterators::Pair;

pub use horned_owl::io::obo::common::*;

use super::lexer::Rule;
use horned_owl::error::HornedError;
use horned_owl::model::{
    AnnotatedComponent, Annotation, AnnotationAssertion, AnnotationSubject, AnnotationValue,
    AsymmetricObjectProperty, Build, ClassAssertion, ClassExpression, Component,
    DeclareAnnotationProperty, DeclareClass, DeclareNamedIndividual, DeclareObjectProperty,
    DisjointClasses, EquivalentClasses, ForIRI, FunctionalObjectProperty, IRI, Import, Individual,
    InverseFunctionalObjectProperty, InverseObjectProperties, Literal, NamedIndividual,
    ObjectProperty, ObjectPropertyAssertion, ObjectPropertyDomain, ObjectPropertyExpression,
    ObjectPropertyRange, OntologyID, ReflexiveObjectProperty, SubAnnotationPropertyOf, SubClassOf,
    SubObjectPropertyExpression, SubObjectPropertyOf, SymmetricObjectProperty,
    TransitiveObjectProperty,
};

// --- namespaces the OBO→OWL mapping relies on ------------------------------

/// A clause line's clause pair plus the `(key, value)` pairs of its trailing
/// `{qualifier}` block (found in the `EOL`), values OBO-unescaped.
fn split_line(line: Pair<'_, Rule>) -> (Pair<'_, Rule>, Vec<(String, String)>) {
    let mut it = line.into_inner();
    let clause = it.next().expect("clause line has a clause");
    let mut quals = Vec::new();
    for eol in it {
        for p in eol.into_inner() {
            if p.as_rule() == Rule::QualifierList {
                for q in p.into_inner() {
                    // Qualifier = QualifierId "=" QuotedString
                    let mut qi = q.into_inner();
                    if let (Some(k), Some(v)) = (qi.next(), qi.next()) {
                        quals.push((k.as_str().to_string(), unquote(v.as_str())));
                    }
                }
            }
        }
    }
    (clause, quals)
}

/// Iterate `(clause, qualifier-annotations)` for each clause line of a frame.
fn clause_lines(
    inner: pest::iterators::Pairs<'_, Rule>,
    line_rule: Rule,
) -> impl Iterator<Item = (Pair<'_, Rule>, Vec<(String, String)>)> + '_ {
    inner
        .filter(move |p| p.as_rule() == line_rule)
        .map(split_line)
}

/// The children of a `HeaderClause` / `*Clause` pair: the leading `*Tag` pair
/// followed by its value pairs. Returns `(tag_rule, values)`.
fn split_clause(clause: Pair<'_, Rule>) -> (Rule, Vec<Pair<'_, Rule>>) {
    let mut inner = clause.into_inner();
    let tag = inner.next().expect("clause has a leading tag");
    (tag.as_rule(), inner.collect())
}

// --- header ----------------------------------------------------------------

/// Pass 1: scan the header for the prefix mapping (`idspace:` over the implicit
/// prefixes), the idspace expansions, and the `default-namespace`.
pub fn scan_header<A: ForIRI>(
    header: &Pair<'_, Rule>,
) -> (
    PrefixMapping,
    HashMap<String, String>,
    Option<String>,
    Option<String>,
) {
    let mut pm = obo_prefixes();
    let mut idspace = HashMap::new();
    let mut default_ns = None;
    let mut onto_ns = None;

    for clause in header.clone().into_inner() {
        if clause.as_rule() != Rule::HeaderClause {
            continue;
        }
        let (tag, values) = split_clause(clause);
        match tag {
            Rule::IdspaceTag => {
                // IdspaceTag ~ IdPrefix ~ Iri ~ QuotedString?
                if let (Some(prefix), Some(iri)) = (values.first(), values.get(1)) {
                    let (prefix, iri) = (prefix.as_str().to_string(), iri.as_str().to_string());
                    pm.add_prefix(&prefix, &iri).ok();
                    idspace.insert(prefix, iri);
                }
            }
            Rule::DefaultNamespaceTag => {
                default_ns = values.first().map(|v| v.as_str().trim().to_string());
            }
            Rule::OntologyTag => {
                onto_ns = values.first().map(|v| v.as_str().trim()).map(|o| {
                    if is_http_iri(o) {
                        format!("{o}#")
                    } else {
                        format!("{OBO_BASE}{o}#")
                    }
                });
            }
            _ => {}
        }
    }
    (pm, idspace, default_ns, onto_ns)
}

/// Pass 2: map the header frame to ontology-level components.
pub fn header_to_components<A: ForIRI>(
    header: Pair<'_, Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    let b = ctx.build;
    let mut out = Vec::new();
    let mut onto_iri: Option<String> = None;
    // data-version can precede or follow the ontology: clause, so its value
    // is collected here and only turned into a versionIRI once both are
    // known, rather than emitted eagerly like the other header clauses.
    let mut data_version: Option<String> = None;

    for clause in header.into_inner() {
        if clause.as_rule() != Rule::HeaderClause {
            continue;
        }
        let (tag, values) = split_clause(clause);
        let val = |i: usize| values.get(i).map(|p| p.as_str().trim());
        match tag {
            Rule::OntologyTag => {
                if let Some(o) = val(0) {
                    onto_iri = Some(if is_http_iri(o) {
                        o.to_string()
                    } else {
                        format!("{OBO_BASE}{o}.owl")
                    });
                }
            }
            Rule::DataVersionTag => {
                data_version = val(0).map(String::from);
            }
            Rule::ImportTag => {
                if let Some(i) = val(0) {
                    out.push(component(Import(ctx.expand(i))));
                }
            }
            Rule::FormatVersionTag => {
                if let Some(v) = val(0) {
                    out.push(ont_ann(b, &format!("{OIO}hasOBOFormatVersion"), v));
                }
            }
            Rule::DefaultNamespaceTag => {
                if let Some(v) = val(0) {
                    out.push(ont_ann(b, &format!("{OIO}default-namespace"), v));
                }
            }
            Rule::RemarkTag => {
                if let Some(v) = val(0) {
                    out.push(ont_ann(b, RDFS_COMMENT, &unescape(v)));
                }
            }
            // The OBO 1.4 escape hatch (spec 5.0.4): arbitrary OWL axioms in
            // functional syntax, for anything that doesn't map onto a native
            // OBO stanza. Parse failures are swallowed, not propagated -- this
            // reader is lenient by design (see module doc), and a malformed
            // escape-hatch payload shouldn't take down an otherwise-good read.
            Rule::OwlAxiomsTag => {
                if let Some(v) = val(0) {
                    let text = unescape(v);
                    let config = horned_owl::io::ParserConfiguration::new(Build::<A>::new());
                    if let Ok((embedded, _)) =
                        crate::ofn::read::<A, _, horned_owl::ontology::set::SetOntology<A>, _>(
                            &mut text.as_bytes(),
                            config,
                        )
                    {
                        // The wrapping `Ontology(...)` is always anonymous
                        // here; its empty OntologyID is not real content.
                        out.extend(
                            embedded
                                .into_iter()
                                .filter(|ac| !matches!(ac.component, Component::OntologyID(_))),
                        );
                    }
                }
            }
            // TODO(oracle): subsetdef / synonymtypedef declarations +
            // SubAnnotationPropertyOf; treat-xrefs-* macros; property_value;
            // date/saved-by/auto-generated-by.
            _ => {}
        }
    }

    if let Some(iri) = onto_iri {
        let viri = data_version.map(|v| {
            b.iri(if is_http_iri(&v) {
                v
            } else {
                format!("{OBO_BASE}{v}")
            })
        });
        out.push(component(OntologyID {
            iri: Some(b.iri(iri)),
            viri,
        }));
    }
    Ok(out)
}

// --- entity dispatch -------------------------------------------------------

/// Dispatch a single `[Term]` / `[Typedef]` / `[Instance]` entity frame.
pub fn entity_to_components<A: ForIRI>(
    frame: Pair<'_, Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    match frame.as_rule() {
        Rule::TermFrame => term_to_components(frame, ctx),
        Rule::TypedefFrame => typedef_to_components(frame, ctx),
        Rule::InstanceFrame => instance_to_components(frame, ctx),
        other => Err(HornedError::invalid(format!(
            "unexpected OBO entity frame: {other:?}"
        ))),
    }
}

/// A subject-scoped metadata clause shared by `[Term]`, `[Typedef]` and
/// `[Instance]` frames (name, def, synonym, xref, comment, subset,
/// obsolescence, provenance). Returns the single annotation assertion it maps
/// to, or `None` if `tag` is not one of these clauses.
fn meta_assertion<A: ForIRI>(
    tag: Rule,
    values: &[Pair<'_, Rule>],
    subject: &IRI<A>,
    ctx: &Context<'_, A>,
) -> Option<AnnotatedComponent<A>> {
    let b = ctx.build;
    let val = |i: usize| values.get(i).map(|p| p.as_str());
    match tag {
        Rule::NameTag => Some(assertion(
            subject,
            lit_ann(b, RDFS_LABEL, &unescape(val(0)?.trim())),
        )),
        Rule::NamespaceTag => Some(assertion(
            subject,
            lit_ann(b, &format!("{OIO}hasOBONamespace"), val(0)?.trim()),
        )),
        Rule::CommentTag => Some(assertion(
            subject,
            lit_ann(b, RDFS_COMMENT, &unescape(val(0)?.trim())),
        )),
        Rule::DefTag => {
            // Def clause wraps a single `Definition = QuotedString ~ XrefList`.
            let parts: Vec<_> = values.first()?.clone().into_inner().collect();
            let text = unquote(parts.first()?.as_str());
            let dbxrefs = parts.get(1).map(dbxref_anns(b)).unwrap_or_default();
            Some(AnnotatedComponent::new(
                AnnotationAssertion {
                    subject: AnnotationSubject::IRI(subject.clone()),
                    ann: lit_ann(b, IAO_DEF, &text),
                },
                dbxrefs.into_iter().collect(),
            ))
        }
        Rule::SynonymTag => {
            let parts: Vec<_> = values.first()?.clone().into_inner().collect();
            Some(synonym_assertion(ctx, subject, &parts))
        }
        // OBO 1.2 legacy synonyms (`exact_synonym: "x" [xrefs]`): scope is the
        // tag, value is `QuotedString ~ XrefList?`. Mapped like the modern form.
        Rule::ExactSynonymTag
        | Rule::NarrowSynonymTag
        | Rule::BroadSynonymTag
        | Rule::RelatedSynonymTag => {
            let prop = match tag {
                Rule::ExactSynonymTag => "hasExactSynonym",
                Rule::NarrowSynonymTag => "hasNarrowSynonym",
                Rule::BroadSynonymTag => "hasBroadSynonym",
                _ => "hasRelatedSynonym",
            };
            let parts: Vec<_> = values.first()?.clone().into_inner().collect();
            let text = unquote(parts.first()?.as_str());
            let dbxrefs = parts.get(1).map(dbxref_anns(b)).unwrap_or_default();
            Some(AnnotatedComponent::new(
                AnnotationAssertion {
                    subject: AnnotationSubject::IRI(subject.clone()),
                    ann: lit_ann(b, &format!("{OIO}{prop}"), &text),
                },
                dbxrefs.into_iter().collect(),
            ))
        }
        Rule::XrefTag => {
            // Xref clause wraps a single `Xref = Id ~ QuotedString?`. A trailing
            // quoted description becomes an rdfs:label AXIOM annotation on the
            // hasDbXref assertion (matching oboformat/ROBOT), not a nested one.
            let parts: Vec<_> = values.first()?.clone().into_inner().collect();
            let ann = lit_ann(
                b,
                &format!("{OIO}hasDbXref"),
                &unescape(parts.first()?.as_str().trim()),
            );
            let axiom_anns: BTreeSet<Annotation<A>> = parts
                .get(1)
                .map(|desc| lit_ann(b, RDFS_LABEL, &unquote(desc.as_str())))
                .into_iter()
                .collect();
            Some(AnnotatedComponent::new(
                AnnotationAssertion {
                    subject: AnnotationSubject::IRI(subject.clone()),
                    ann,
                },
                axiom_anns,
            ))
        }
        Rule::SubsetTag => Some(assertion(
            subject,
            iri_ann(b, &format!("{OIO}inSubset"), ctx.expand(val(0)?.trim())),
        )),
        Rule::IsObsoleteTag => (val(0)?.trim() == "true").then(|| {
            assertion(
                subject,
                Annotation {
                    ap: b.annotation_property(OWL_DEPRECATED),
                    av: AnnotationValue::Literal(Literal::Datatype {
                        literal: "true".to_string(),
                        datatype_iri: b.iri(XSD_BOOLEAN),
                    }),
                    ann: Default::default(),
                },
            )
        }),
        Rule::ReplacedByTag => Some(assertion(
            subject,
            iri_ann(b, IAO_TERM_REPLACED_BY, ctx.expand(val(0)?)),
        )),
        Rule::ConsiderTag => Some(assertion(
            subject,
            iri_ann(b, &format!("{OIO}consider"), ctx.expand(val(0)?)),
        )),
        Rule::CreatedByTag => Some(assertion(
            subject,
            lit_ann(b, &format!("{OIO}created_by"), val(0)?.trim()),
        )),
        Rule::CreationDateTag => Some(assertion(
            subject,
            lit_ann(b, &format!("{OIO}creation_date"), val(0)?.trim()),
        )),
        Rule::PropertyValueTag => {
            // property_value is a common clause mapped to an AnnotationAssertion
            // in every frame (verified against ROBOT on a Term): the resource
            // form carries an IRI value, the literal form a (typed) literal.
            let pv = values.first()?.clone().into_inner().next()?;
            let parts: Vec<_> = pv.clone().into_inner().collect();
            let ap = b.annotation_property(ctx.expand(parts.first()?.as_str()));
            let av = match pv.as_rule() {
                Rule::ResourcePropertyValue => {
                    AnnotationValue::IRI(ctx.expand(parts.get(1)?.as_str()))
                }
                Rule::LiteralPropertyValue => {
                    let litpair = parts.get(1)?;
                    let literal = if litpair.as_rule() == Rule::QuotedString {
                        unquote(litpair.as_str())
                    } else {
                        litpair.as_str().to_string()
                    };
                    let dt = parts.get(2)?.as_str();
                    // A plain xsd:string is a simple literal (ROBOT drops the type).
                    if dt == "xsd:string" {
                        AnnotationValue::Literal(Literal::Simple { literal })
                    } else {
                        AnnotationValue::Literal(Literal::Datatype {
                            literal,
                            datatype_iri: b.iri(expand_datatype(dt, ctx)),
                        })
                    }
                }
                _ => return None,
            };
            Some(assertion(
                subject,
                Annotation {
                    ap,
                    av,
                    ann: Default::default(),
                },
            ))
        }
        _ => None,
    }
}

/// `[Term]` → DeclareClass + shared metadata annotations + is_a / relationship
/// logical axioms.
fn term_to_components<A: ForIRI>(
    frame: Pair<'_, Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    let b = ctx.build;
    let mut out = Vec::new();
    let mut inner = frame.into_inner();

    // TermFrame = "[Term]" "id:" ClassId EOL (TermClauseLine | ...)*
    // A comment line before the `id:` line is also a pair of the frame.
    let id = inner
        .find(|p| p.as_rule() != Rule::Comment)
        .ok_or_else(|| HornedError::invalid("[Term] frame without id"))?;
    let id = id.as_str().trim();
    let iri = ctx.expand(id);
    out.push(component(DeclareClass(b.class(iri.clone()))));
    out.push(assertion(&iri, lit_ann(b, &format!("{OIO}id"), id)));

    let mut has_namespace = false;
    // intersection_of / union_of clauses are collected across the frame and
    // combined into a single EquivalentClasses axiom (oboformat/ROBOT).
    let mut intersection: Vec<ClassExpression<A>> = Vec::new();
    let mut union: Vec<ClassExpression<A>> = Vec::new();
    for (clause, quals) in clause_lines(inner, Rule::TermClauseLine) {
        let (tag, values) = split_clause(clause);
        has_namespace |= tag == Rule::NamespaceTag;
        // A gci_relation/gci_filler qualifier makes is_a/relationship a GCI whose
        // subject is `C ⊓ (gci_rel some gci_filler)` rather than plain `C`.
        let subject = || {
            gci_subject(ctx, &iri, &quals)
                .unwrap_or_else(|| ClassExpression::Class(b.class(iri.clone())))
        };
        let qa = qual_anns(ctx, &quals);
        if let Some(c) = meta_assertion(tag, &values, &iri, ctx) {
            out.push(with_anns(c, qa));
            continue;
        }
        let val = |i: usize| values.get(i).map(|p| p.as_str());
        match tag {
            Rule::IsATag => {
                if let Some(parent) = val(0) {
                    out.push(component_ann(
                        SubClassOf {
                            sub: subject(),
                            sup: ClassExpression::Class(ctx.class(parent)),
                        },
                        qa,
                    ));
                }
            }
            Rule::RelationshipTag => {
                // RelationshipTag ~ RelationId ~ ClassId
                if let (Some(rel), Some(filler)) = (val(0), val(1)) {
                    let rel_iri = ctx.expand_rel(rel);
                    if ctx.metadata_tags.contains(rel_iri.as_ref()) {
                        // A metadata-tag relation is an annotation assertion, not
                        // an existential (oboformat/ROBOT).
                        out.push(with_anns(
                            assertion(&iri, iri_ann(b, rel_iri.as_ref(), ctx.expand(filler))),
                            qa,
                        ));
                    } else {
                        out.push(component_ann(
                            SubClassOf {
                                sub: subject(),
                                sup: ClassExpression::ObjectSomeValuesFrom {
                                    ope: ObjectPropertyExpression::ObjectProperty(ObjectProperty(
                                        rel_iri,
                                    )),
                                    bce: Box::new(ClassExpression::Class(ctx.class(filler))),
                                },
                            },
                            qa,
                        ));
                    }
                }
            }
            Rule::AltIdTag => {
                // hasAlternativeId on the term + the alt id materialised as a
                // deprecated class merged into (replaced_by) this term.
                if let Some(alt) = val(0) {
                    out.push(assertion(
                        &iri,
                        lit_ann(b, &format!("{OIO}hasAlternativeId"), alt.trim()),
                    ));
                    let alt_iri = ctx.expand(alt.trim());
                    // Real files carry free text here (`CHEBI:1, CHEBI:2`), which
                    // names no single class to deprecate.
                    if alt_iri != iri && !alt.trim().contains(char::is_whitespace) {
                        out.push(component(DeclareClass(b.class(alt_iri.clone()))));
                        out.push(assertion(
                            &alt_iri,
                            Annotation {
                                ap: b.annotation_property(OWL_DEPRECATED),
                                av: AnnotationValue::Literal(Literal::Datatype {
                                    literal: "true".to_string(),
                                    datatype_iri: b.iri(XSD_BOOLEAN),
                                }),
                                ann: Default::default(),
                            },
                        ));
                        out.push(assertion(
                            &alt_iri,
                            iri_ann(b, IAO_TERM_REPLACED_BY, iri.clone()),
                        ));
                        out.push(assertion(
                            &alt_iri,
                            iri_ann(b, IAO_OBSOLESCENCE_REASON, b.iri(IAO_TERMS_MERGED)),
                        ));
                    }
                }
            }
            Rule::IntersectionOfTag => {
                // ((RelationId ~ ClassId) | ClassId): a genus (Class) or a
                // differentia (R some filler).
                intersection.push(match (val(0), val(1)) {
                    (Some(rel), Some(filler)) => ClassExpression::ObjectSomeValuesFrom {
                        ope: ope(ctx, rel),
                        bce: Box::new(ClassExpression::Class(ctx.class(filler))),
                    },
                    (Some(genus), None) => ClassExpression::Class(ctx.class(genus)),
                    _ => continue,
                });
            }
            Rule::UnionOfTag => {
                if let Some(c) = val(0) {
                    union.push(ClassExpression::Class(ctx.class(c)));
                }
            }
            Rule::EquivalentToTag => {
                if let Some(c) = val(0) {
                    out.push(component_ann(
                        EquivalentClasses(vec![
                            ClassExpression::Class(b.class(iri.clone())),
                            ClassExpression::Class(ctx.class(c)),
                        ]),
                        qa,
                    ));
                }
            }
            Rule::DisjointFromTag => {
                if let Some(c) = val(0) {
                    out.push(component_ann(
                        DisjointClasses(vec![
                            ClassExpression::Class(b.class(iri.clone())),
                            ClassExpression::Class(ctx.class(c)),
                        ]),
                        qa,
                    ));
                }
            }
            // TODO(oracle): alt_id (obsolescence classes), GCIs
            // (gci_relation/gci_filler qualifiers), is_anonymous, builtin;
            // trailing {qualifier} axiom annotations.
            _ => {}
        }
    }

    // A genus-differentia definition (intersection_of) or a union_of both map to
    // an EquivalentClasses between the class and the combined expression.
    if !intersection.is_empty() {
        out.push(component(EquivalentClasses(vec![
            ClassExpression::Class(b.class(iri.clone())),
            ClassExpression::ObjectIntersectionOf(intersection),
        ])));
    }
    if !union.is_empty() {
        out.push(component(EquivalentClasses(vec![
            ClassExpression::Class(b.class(iri.clone())),
            ClassExpression::ObjectUnionOf(union),
        ])));
    }

    // default-namespace applies to a term that declares no namespace of its own.
    if !has_namespace {
        if let Some(ns) = &ctx.default_ns {
            out.push(assertion(
                &iri,
                lit_ann(b, &format!("{OIO}hasOBONamespace"), ns),
            ));
        }
    }
    Ok(out)
}

/// `[Typedef]` → DeclareObjectProperty + shared metadata annotations +
/// property characteristics / domain / range / is_a / inverse_of.
fn typedef_to_components<A: ForIRI>(
    frame: Pair<'_, Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    let b = ctx.build;
    let mut out = Vec::new();
    let mut inner = frame.into_inner();

    // A comment line before the `id:` line is also a pair of the frame.
    let id = inner
        .find(|p| p.as_rule() != Rule::Comment)
        .ok_or_else(|| HornedError::invalid("[Typedef] frame without id"))?;
    let id = id.as_str().trim();
    // A shorthand typedef resolves to its xref IRI; its bare id survives as the
    // oboInOwl:id and an oboInOwl:shorthand annotation (oboformat/ROBOT).
    let iri = ctx.expand_rel(id);
    // is_metadata_tag: true → the typedef is an ANNOTATION property, so its uses
    // are annotations, not logical relations (oboformat/ROBOT).
    let is_meta = ctx.metadata_tags.contains(iri.as_ref());
    if is_meta {
        out.push(component(DeclareAnnotationProperty(
            b.annotation_property(iri.clone()),
        )));
    } else {
        out.push(component(DeclareObjectProperty(
            b.object_property(iri.clone()),
        )));
    }
    out.push(assertion(&iri, lit_ann(b, &format!("{OIO}id"), id)));
    if ctx.rel_map.contains_key(id) {
        out.push(assertion(&iri, lit_ann(b, &format!("{OIO}shorthand"), id)));
    }

    let mut has_namespace = false;
    for (clause, quals) in clause_lines(inner, Rule::TypedefClauseLine) {
        let (tag, values) = split_clause(clause);
        has_namespace |= tag == Rule::NamespaceTag;
        let qa = qual_anns(ctx, &quals);
        if let Some(c) = meta_assertion(tag, &values, &iri, ctx) {
            out.push(with_anns(c, qa));
            continue;
        }
        let val = |i: usize| values.get(i).map(|p| p.as_str());
        match tag {
            Rule::IsMetadataTagTag => {
                if val(0).map(str::trim) == Some("true") {
                    out.push(assertion(
                        &iri,
                        Annotation {
                            ap: b.annotation_property(format!("{OIO}is_metadata_tag").as_str()),
                            av: AnnotationValue::Literal(Literal::Datatype {
                                literal: "true".to_string(),
                                datatype_iri: b.iri(XSD_BOOLEAN),
                            }),
                            ann: Default::default(),
                        },
                    ));
                }
            }
            // A metadata-tag typedef is an annotation property: its is_a is a
            // sub-annotation-property axiom, and the object-property axioms below
            // do not apply.
            Rule::IsATag if is_meta => {
                if let Some(sup) = val(0) {
                    out.push(component_ann(
                        SubAnnotationPropertyOf {
                            sub: b.annotation_property(iri.clone()),
                            sup: b.annotation_property(ctx.expand_rel(sup)),
                        },
                        qa,
                    ));
                }
            }
            _ if is_meta => {} // skip object-property axioms for metadata tags
            Rule::IsATag => {
                if let Some(sup) = val(0) {
                    out.push(component_ann(
                        SubObjectPropertyOf {
                            sub: SubObjectPropertyExpression::ObjectPropertyExpression(ope(
                                ctx, id,
                            )),
                            sup: ope(ctx, sup),
                        },
                        qa,
                    ));
                }
            }
            Rule::InverseOfTag => {
                if let Some(other) = val(0) {
                    out.push(component_ann(
                        InverseObjectProperties(
                            ObjectProperty(iri.clone()),
                            ObjectProperty(ctx.expand_rel(other)),
                        ),
                        qa,
                    ));
                }
            }
            Rule::DomainTag => {
                if let Some(c) = val(0) {
                    out.push(component_ann(
                        ObjectPropertyDomain {
                            ope: ope(ctx, id),
                            ce: ClassExpression::Class(ctx.class(c)),
                        },
                        qa,
                    ));
                }
            }
            Rule::RangeTag => {
                if let Some(c) = val(0) {
                    out.push(component_ann(
                        ObjectPropertyRange {
                            ope: ope(ctx, id),
                            ce: ClassExpression::Class(ctx.class(c)),
                        },
                        qa,
                    ));
                }
            }
            // Boolean property characteristics: a `true` becomes the OWL axiom;
            // a `false` (which OWL cannot assert) is preserved as an oboInOwl
            // annotation echoing the tag, matching oboformat/ROBOT.
            Rule::IsTransitiveTag
            | Rule::IsSymmetricTag
            | Rule::IsReflexiveTag
            | Rule::IsAsymmetricTag
            | Rule::IsFunctionalTag
            | Rule::IsInverseFunctionalTag => {
                let is_true = val(0).map(str::trim) == Some("true");
                if is_true {
                    let p = ope(ctx, id);
                    out.push(match tag {
                        Rule::IsTransitiveTag => component_ann(TransitiveObjectProperty(p), qa),
                        Rule::IsSymmetricTag => component_ann(SymmetricObjectProperty(p), qa),
                        Rule::IsReflexiveTag => component_ann(ReflexiveObjectProperty(p), qa),
                        Rule::IsAsymmetricTag => component_ann(AsymmetricObjectProperty(p), qa),
                        Rule::IsFunctionalTag => component_ann(FunctionalObjectProperty(p), qa),
                        _ => component_ann(InverseFunctionalObjectProperty(p), qa),
                    });
                } else {
                    out.push(assertion(
                        &iri,
                        Annotation {
                            ap: b.annotation_property(format!("{OIO}{}", char_tag_local(tag))),
                            av: AnnotationValue::Literal(Literal::Datatype {
                                literal: "false".to_string(),
                                datatype_iri: b.iri(XSD_BOOLEAN),
                            }),
                            ann: Default::default(),
                        },
                    ));
                }
            }
            // TODO(oracle): holds_over_chain / equivalent_to_chain (property
            // chains), transitive_over, disjoint_from/equivalent_to, is_a to a
            // relation shorthand (xref-driven rel_map), is_metadata_tag ->
            // AnnotationProperty, property_value, {qualifier} anns.
            _ => {}
        }
    }

    if !has_namespace {
        if let Some(ns) = &ctx.default_ns {
            out.push(assertion(
                &iri,
                lit_ann(b, &format!("{OIO}hasOBONamespace"), ns),
            ));
        }
    }
    Ok(out)
}

/// `[Instance]` → DeclareNamedIndividual + shared metadata annotations +
/// instance_of (ClassAssertion) + property_value / relationship (object- or
/// data-property assertions).
///
/// NB: oboformat/ROBOT do not support `[Instance]` frames, so this mapping has
/// no tool oracle; it follows the OBO 1.4 → OWL individual mapping and is
/// covered by unit tests. A resource `property_value` is read as an
/// ObjectPropertyAssertion and a literal one as a DataPropertyAssertion.
fn instance_to_components<A: ForIRI>(
    frame: Pair<'_, Rule>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    let b = ctx.build;
    let mut out = Vec::new();
    let mut inner = frame.into_inner();

    // A comment line before the `id:` line is also a pair of the frame.
    let id = inner
        .find(|p| p.as_rule() != Rule::Comment)
        .ok_or_else(|| HornedError::invalid("[Instance] frame without id"))?;
    let id = id.as_str().trim();
    let iri = ctx.expand(id);
    out.push(component(DeclareNamedIndividual(
        b.named_individual(iri.clone()),
    )));
    out.push(assertion(&iri, lit_ann(b, &format!("{OIO}id"), id)));
    let this = || Individual::Named(NamedIndividual(iri.clone()));

    let mut has_namespace = false;
    for (clause, quals) in clause_lines(inner, Rule::InstanceClauseLine) {
        let (tag, values) = split_clause(clause);
        has_namespace |= tag == Rule::NamespaceTag;
        let qa = qual_anns(ctx, &quals);
        if let Some(c) = meta_assertion(tag, &values, &iri, ctx) {
            out.push(with_anns(c, qa));
            continue;
        }
        let val = |i: usize| values.get(i).map(|p| p.as_str());
        match tag {
            Rule::InstanceOfTag => {
                if let Some(c) = val(0) {
                    out.push(component_ann(
                        ClassAssertion {
                            ce: ClassExpression::Class(ctx.class(c)),
                            i: this(),
                        },
                        qa,
                    ));
                }
            }
            Rule::RelationshipTag => {
                // RelationshipTag ~ RelationId ~ InstanceId. Per spec §5.5 an
                // instance-frame relationship is an object PropertyAssertion
                // between two individuals (unlike a Term relationship, which is
                // an existential SubClassOf). No ROBOT oracle (see fn doc).
                if let (Some(rel), Some(target)) = (val(0), val(1)) {
                    out.push(component_ann(
                        ObjectPropertyAssertion {
                            ope: ope(ctx, rel),
                            from: this(),
                            to: Individual::Named(NamedIndividual(ctx.expand(target))),
                        },
                        qa,
                    ));
                }
            }
            _ => {}
        }
    }

    if !has_namespace {
        if let Some(ns) = &ctx.default_ns {
            out.push(assertion(
                &iri,
                lit_ann(b, &format!("{OIO}hasOBONamespace"), ns),
            ));
        }
    }
    Ok(out)
}

/// The oboInOwl local name echoing a boolean property-characteristic tag.
fn char_tag_local(tag: Rule) -> &'static str {
    match tag {
        Rule::IsTransitiveTag => "is_transitive",
        Rule::IsSymmetricTag => "is_symmetric",
        Rule::IsReflexiveTag => "is_reflexive",
        Rule::IsAsymmetricTag => "is_asymmetric",
        Rule::IsFunctionalTag => "is_functional",
        Rule::IsInverseFunctionalTag => "is_inverse_functional",
        _ => "",
    }
}

/// Scan the `[Typedef]` frames for relation shorthands: a bare (unprefixed) id
/// with exactly one `xref` maps to that xref's IRI (oboformat's shorthand rule),
/// so all relation uses of the bare id resolve to the canonical relation.
pub fn build_rel_map(
    children: &[Pair<'_, Rule>],
    idspace: &HashMap<String, String>,
    onto_ns: Option<&str>,
) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for entity in children.iter().filter(|p| p.as_rule() == Rule::EntityFrame) {
        let Some(frame) = entity.clone().into_inner().next() else {
            continue;
        };
        if frame.as_rule() != Rule::TypedefFrame {
            continue;
        }
        let mut inner = frame.into_inner();
        let Some(id) = inner.find(|p| p.as_rule() != Rule::Comment) else {
            continue;
        };
        let id = id.as_str().trim();
        if id.contains(':') || id.starts_with("http") {
            continue; // not a bare shorthand id
        }
        let xrefs: Vec<String> = clauses(inner, Rule::TypedefClauseLine)
            .filter_map(|clause| {
                let (tag, values) = split_clause(clause);
                (tag == Rule::XrefTag)
                    .then(|| values.first()?.clone().into_inner().next())
                    .flatten()
                    .map(|xid| xid.as_str().trim().to_string())
            })
            .collect();
        if let [xref] = xrefs.as_slice() {
            map.insert(id.to_string(), expand_id_with(xref, idspace, onto_ns));
        }
    }
    map
}

/// Scan `[Typedef]` frames for `is_metadata_tag: true`, returning the resolved
/// IRIs of those properties (they map to annotation properties).
pub fn build_metadata_tags(
    children: &[Pair<'_, Rule>],
    idspace: &HashMap<String, String>,
    onto_ns: Option<&str>,
    rel_map: &HashMap<String, String>,
) -> BTreeSet<String> {
    let mut tags = BTreeSet::new();
    for entity in children.iter().filter(|p| p.as_rule() == Rule::EntityFrame) {
        let Some(frame) = entity.clone().into_inner().next() else {
            continue;
        };
        if frame.as_rule() != Rule::TypedefFrame {
            continue;
        }
        let mut inner = frame.into_inner();
        let Some(id) = inner.find(|p| p.as_rule() != Rule::Comment) else {
            continue;
        };
        let id = id.as_str().trim();
        let is_meta = clauses(inner, Rule::TypedefClauseLine).any(|clause| {
            let (tag, values) = split_clause(clause);
            tag == Rule::IsMetadataTagTag
                && values.first().map(|v| v.as_str().trim()) == Some("true")
        });
        if is_meta {
            let iri = match rel_map.get(id) {
                Some(x) => x.clone(),
                None => expand_id_with(id, idspace, onto_ns),
            };
            tags.insert(iri);
        }
    }
    tags
}

/// Iterate the clause pairs of a frame: for each `*ClauseLine` yield its inner
/// `*Clause` (the trailing `EOL`/qualifier sibling is skipped for now).
fn clauses(
    inner: pest::iterators::Pairs<'_, Rule>,
    line_rule: Rule,
) -> impl Iterator<Item = Pair<'_, Rule>> {
    inner
        .filter(move |p| p.as_rule() == line_rule)
        .filter_map(|line| line.into_inner().next())
}

/// `synonym: "text" SCOPE [TYPE] [xrefs]` → `oboInOwl:has{Scope}Synonym` with
/// the dbxref list (and synonym-type) as axiom annotations.
fn synonym_assertion<A: ForIRI>(
    ctx: &Context<'_, A>,
    subject: &IRI<A>,
    values: &[Pair<'_, Rule>],
) -> AnnotatedComponent<A> {
    let b = ctx.build;
    // Synonym = QuotedString ~ SynonymScopeSingle ~ (XrefList | SynonymTypeId ~ XrefList)
    let text = values
        .first()
        .map(|p| unquote(p.as_str()))
        .unwrap_or_default();
    let scope = values
        .get(1)
        .map(|p| p.as_str().trim())
        .unwrap_or("RELATED");
    let prop = match scope {
        "EXACT" => "hasExactSynonym",
        "NARROW" => "hasNarrowSynonym",
        "BROAD" => "hasBroadSynonym",
        _ => "hasRelatedSynonym",
    };
    let mut axiom_anns: Vec<Annotation<A>> = Vec::new();
    for p in &values[2.min(values.len())..] {
        match p.as_rule() {
            Rule::XrefList => axiom_anns.extend(dbxref_anns(b)(p)),
            Rule::SynonymTypeId => {
                axiom_anns.push(iri_ann(
                    b,
                    &format!("{OIO}hasSynonymType"),
                    ctx.expand(p.as_str()),
                ));
            }
            _ => {}
        }
    }
    AnnotatedComponent::new(
        AnnotationAssertion {
            subject: AnnotationSubject::IRI(subject.clone()),
            ann: lit_ann(b, &format!("{OIO}{prop}"), &text),
        },
        axiom_anns.into_iter().collect(),
    )
}

/// Build `oboInOwl:hasDbXref` axiom annotations from an `XrefList` pair.
fn dbxref_anns<A: ForIRI>(b: &Build<A>) -> impl Fn(&Pair<'_, Rule>) -> Vec<Annotation<A>> + '_ {
    move |xreflist: &Pair<'_, Rule>| {
        xreflist
            .clone()
            .into_inner()
            .filter(|p| p.as_rule() == Rule::XrefListItem)
            .filter_map(|item| item.into_inner().next()) // XrefId
            .map(|id| lit_ann(b, &format!("{OIO}hasDbXref"), &unescape(id.as_str().trim())))
            .collect()
    }
}

/// Build the prefix mapping from a header (idspace over the implicit prefixes).
/// Retained for the reader's pass-1 prefix extraction.
pub fn prefixes_from_header<A: ForIRI>(
    header: &Pair<'_, Rule>,
) -> Result<PrefixMapping, HornedError> {
    Ok(scan_header::<A>(header).0)
}
