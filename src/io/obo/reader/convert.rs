//! Maps the OBO syntax tree to components.
//!
//! This is `reader::from_pair` over [`syntax`](super::syntax) types rather
//! than pest pairs, and makes the same decisions. What `from_pair` does for
//! the whole document at once (the relation shorthand and metadata tag
//! scans, the final declarations and labels) is split here so a reader can
//! do it a frame at a time: see [`Typedefs`] and
//! [`Finalizer`](crate::io::obo::common::Finalizer).
use std::collections::{BTreeSet, HashMap};

use curie::PrefixMapping;

use super::syntax::*;
use crate::error::HornedError;
use crate::io::obo::common::{
    Context, IAO_DEF, IAO_OBSOLESCENCE_REASON, IAO_TERM_REPLACED_BY, IAO_TERMS_MERGED, OBO_BASE,
    OIO, OWL_DEPRECATED, RDFS_COMMENT, RDFS_LABEL, XSD_BOOLEAN, assertion, component,
    component_ann, expand_datatype, expand_id_with, gci_subject, iri_ann, is_http_iri, lit_ann,
    obo_prefixes, ont_ann, ope, qual_anns, unescape, unquote, with_anns,
};
use crate::model::*;

fn v0<'a>(v: &Val<'a>) -> Option<&'a str> {
    match v {
        Val::One(s) | Val::Two(s, _) => Some(s),
        _ => None,
    }
}

fn v1<'a>(v: &Val<'a>) -> Option<&'a str> {
    match v {
        Val::Two(_, s) => Some(s),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Header

/// The prefix mapping, `idspace:` expansions, `default-namespace` and the
/// ontology-local namespace the header declares.
pub(super) fn scan_header(
    h: &Header<'_>,
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
    for c in &h.clauses {
        match c.tag {
            Tag::Idspace => {
                if let (Some(prefix), Some(iri)) = (c.val, c.val2) {
                    pm.add_prefix(prefix, iri).ok();
                    idspace.insert(prefix.to_string(), iri.to_string());
                }
            }
            Tag::DefaultNamespace => {
                default_ns = c.val.map(|v| v.trim().to_string());
            }
            Tag::Ontology => {
                onto_ns = c.val.map(str::trim).map(|o| {
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

/// The ontology-level components of the header.
pub(super) fn header_components<A: ForIRI>(
    h: &Header<'_>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    let b = ctx.build;
    let mut out = Vec::new();
    let mut onto_iri: Option<String> = None;
    let mut data_version: Option<String> = None;

    for c in &h.clauses {
        let val = c.val.map(str::trim);
        match c.tag {
            Tag::Ontology => {
                if let Some(o) = val {
                    onto_iri = Some(if is_http_iri(o) {
                        o.to_string()
                    } else {
                        format!("{OBO_BASE}{o}.owl")
                    });
                }
            }
            Tag::DataVersion => data_version = val.map(String::from),
            Tag::Import => {
                if let Some(i) = val {
                    out.push(component(Import(ctx.expand(i))));
                }
            }
            Tag::FormatVersion => {
                if let Some(v) = val {
                    out.push(ont_ann(b, &format!("{OIO}hasOBOFormatVersion"), v));
                }
            }
            Tag::DefaultNamespace => {
                if let Some(v) = val {
                    out.push(ont_ann(b, &format!("{OIO}default-namespace"), v));
                }
            }
            Tag::Remark => {
                if let Some(v) = val {
                    out.push(ont_ann(b, RDFS_COMMENT, &unescape(v)));
                }
            }
            // The OBO 1.4 escape hatch: OWL axioms in functional syntax.
            // Parse failures are swallowed, as in the pest reader.
            Tag::OwlAxioms => {
                if let Some(v) = val {
                    let text = unescape(v);
                    let config = crate::io::ParserConfiguration::new(Build::<A>::new());
                    if let Ok((embedded, _)) = crate::io::ofn::reader::read::<
                        A,
                        _,
                        crate::ontology::set::SetOntology<A>,
                        _,
                    >(&mut text.as_bytes(), config)
                    {
                        out.extend(
                            embedded
                                .into_iter()
                                .filter(|ac| !matches!(ac.component, Component::OntologyID(_))),
                        );
                    }
                }
            }
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

// ---------------------------------------------------------------------------
// Relation shorthands and metadata tags

/// What the `[Typedef]` frames seen so far say about how relation ids
/// resolve: bare ids with a single `xref` are shorthands for that xref, and
/// `is_metadata_tag: true` ones are annotation properties.
#[derive(Default)]
pub(super) struct Typedefs {
    pub rel_map: HashMap<String, String>,
    meta_ids: Vec<String>,
}

impl Typedefs {
    pub fn observe(
        &mut self,
        f: &Frame<'_>,
        idspace: &HashMap<String, String>,
        onto_ns: Option<&str>,
    ) {
        if f.kind != FrameKind::Typedef {
            return;
        }
        let id = f.id.trim();
        if !id.contains(':') && !id.starts_with("http") {
            let xrefs: Vec<String> = f
                .lines
                .iter()
                .filter_map(|l| match (&l.tag, &l.val) {
                    (Tag::Xref, Val::Xref { id, .. }) => Some(id.trim().to_string()),
                    _ => None,
                })
                .collect();
            if let [xref] = xrefs.as_slice() {
                self.rel_map
                    .insert(id.to_string(), expand_id_with(xref, idspace, onto_ns));
            }
        }
        let is_meta = f
            .lines
            .iter()
            .any(|l| l.tag == Tag::IsMetadata && v0(&l.val).map(str::trim) == Some("true"));
        if is_meta {
            self.meta_ids.push(id.to_string());
        }
    }

    /// The IRIs of the metadata tags, under the shorthands seen so far.
    pub fn metadata_tags(
        &self,
        idspace: &HashMap<String, String>,
        onto_ns: Option<&str>,
    ) -> BTreeSet<String> {
        self.meta_ids
            .iter()
            .map(|id| match self.rel_map.get(id) {
                Some(x) => x.clone(),
                None => expand_id_with(id, idspace, onto_ns),
            })
            .collect()
    }
}

// ---------------------------------------------------------------------------
// Frames

/// The components of one entity frame.
pub(super) fn frame_components<A: ForIRI>(
    f: &Frame<'_>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    match f.kind {
        FrameKind::Term => term(f, ctx),
        FrameKind::Typedef => typedef(f, ctx),
        FrameKind::Instance => instance(f, ctx),
    }
}

fn quals_of(l: &Line<'_>) -> Vec<(String, String)> {
    l.quals
        .iter()
        .map(|(k, v)| (k.to_string(), unquote(v)))
        .collect()
}

fn dbxrefs<A: ForIRI>(b: &Build<A>, list: Option<&str>) -> Vec<Annotation<A>> {
    list.map(|l| {
        xref_ids(l)
            .into_iter()
            .map(|id| lit_ann(b, &format!("{OIO}hasDbXref"), &unescape(id.trim())))
            .collect()
    })
    .unwrap_or_default()
}

/// A subject-scoped metadata clause shared by all three kinds of frame.
fn meta_assertion<A: ForIRI>(
    line: &Line<'_>,
    subject: &IRI<A>,
    ctx: &Context<'_, A>,
) -> Option<AnnotatedComponent<A>> {
    let b = ctx.build;
    let val = v0(&line.val);
    match line.tag {
        Tag::Name => Some(assertion(
            subject,
            lit_ann(b, RDFS_LABEL, &unescape(val?.trim())),
        )),
        Tag::Namespace => Some(assertion(
            subject,
            lit_ann(b, &format!("{OIO}hasOBONamespace"), val?.trim()),
        )),
        Tag::Comment => Some(assertion(
            subject,
            lit_ann(b, RDFS_COMMENT, &unescape(val?.trim())),
        )),
        Tag::Def => {
            let Val::Def { text, xrefs } = &line.val else {
                return None;
            };
            Some(AnnotatedComponent::new(
                AnnotationAssertion {
                    subject: AnnotationSubject::IRI(subject.clone()),
                    ann: lit_ann(b, IAO_DEF, &unquote(text)),
                },
                dbxrefs(b, *xrefs).into_iter().collect(),
            ))
        }
        Tag::Synonym => {
            let Val::Syn {
                text,
                scope,
                ty,
                xrefs,
            } = &line.val
            else {
                return None;
            };
            let prop = match scope.trim() {
                "EXACT" => "hasExactSynonym",
                "NARROW" => "hasNarrowSynonym",
                "BROAD" => "hasBroadSynonym",
                _ => "hasRelatedSynonym",
            };
            let mut axiom_anns = dbxrefs(b, *xrefs);
            if let Some(ty) = ty {
                axiom_anns.push(iri_ann(b, &format!("{OIO}hasSynonymType"), ctx.expand(ty)));
            }
            Some(AnnotatedComponent::new(
                AnnotationAssertion {
                    subject: AnnotationSubject::IRI(subject.clone()),
                    ann: lit_ann(b, &format!("{OIO}{prop}"), &unquote(text)),
                },
                axiom_anns.into_iter().collect(),
            ))
        }
        Tag::ExactSynonym | Tag::NarrowSynonym | Tag::BroadSynonym | Tag::RelatedSynonym => {
            let prop = match line.tag {
                Tag::ExactSynonym => "hasExactSynonym",
                Tag::NarrowSynonym => "hasNarrowSynonym",
                Tag::BroadSynonym => "hasBroadSynonym",
                _ => "hasRelatedSynonym",
            };
            let Val::Def { text, xrefs } = &line.val else {
                return None;
            };
            Some(AnnotatedComponent::new(
                AnnotationAssertion {
                    subject: AnnotationSubject::IRI(subject.clone()),
                    ann: lit_ann(b, &format!("{OIO}{prop}"), &unquote(text)),
                },
                dbxrefs(b, *xrefs).into_iter().collect(),
            ))
        }
        Tag::Xref => {
            let Val::Xref { id, desc } = &line.val else {
                return None;
            };
            let ann = lit_ann(b, &format!("{OIO}hasDbXref"), &unescape(id.trim()));
            let axiom_anns: BTreeSet<Annotation<A>> = desc
                .map(|d| lit_ann(b, RDFS_LABEL, &unquote(d)))
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
        Tag::Subset => Some(assertion(
            subject,
            iri_ann(b, &format!("{OIO}inSubset"), ctx.expand(val?.trim())),
        )),
        Tag::IsObsolete => (val?.trim() == "true").then(|| {
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
        Tag::ReplacedBy => Some(assertion(
            subject,
            iri_ann(b, IAO_TERM_REPLACED_BY, ctx.expand(val?)),
        )),
        Tag::Consider => Some(assertion(
            subject,
            iri_ann(b, &format!("{OIO}consider"), ctx.expand(val?)),
        )),
        Tag::CreatedBy => Some(assertion(
            subject,
            lit_ann(b, &format!("{OIO}created_by"), val?.trim()),
        )),
        Tag::CreationDate => Some(assertion(
            subject,
            lit_ann(b, &format!("{OIO}creation_date"), val?.trim()),
        )),
        Tag::PropertyValue => {
            let (ap, av) = match &line.val {
                Val::PvResource { rel, id } => (
                    b.annotation_property(ctx.expand(rel)),
                    AnnotationValue::IRI(ctx.expand(id)),
                ),
                Val::PvLiteral {
                    rel,
                    lit,
                    quoted,
                    datatype,
                } => {
                    let literal = if *quoted {
                        unquote(lit)
                    } else {
                        lit.to_string()
                    };
                    let av = if *datatype == "xsd:string" {
                        AnnotationValue::Literal(Literal::Simple { literal })
                    } else {
                        AnnotationValue::Literal(Literal::Datatype {
                            literal,
                            datatype_iri: b.iri(expand_datatype(datatype, ctx)),
                        })
                    };
                    (b.annotation_property(ctx.expand(rel)), av)
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

fn default_namespace<A: ForIRI>(
    out: &mut Vec<AnnotatedComponent<A>>,
    iri: &IRI<A>,
    has_namespace: bool,
    ctx: &Context<'_, A>,
) {
    if !has_namespace && let Some(ns) = &ctx.default_ns {
        out.push(assertion(
            iri,
            lit_ann(ctx.build, &format!("{OIO}hasOBONamespace"), ns),
        ));
    }
}

fn is_true_annotation<A: ForIRI>(b: &Build<A>, ap: String, value: &str) -> Annotation<A> {
    Annotation {
        ap: b.annotation_property(ap.as_str()),
        av: AnnotationValue::Literal(Literal::Datatype {
            literal: value.to_string(),
            datatype_iri: b.iri(XSD_BOOLEAN),
        }),
        ann: Default::default(),
    }
}

fn term<A: ForIRI>(
    f: &Frame<'_>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    let b = ctx.build;
    let mut out = Vec::new();
    let id = f.id.trim();
    let iri = ctx.expand(id);
    out.push(component(DeclareClass(b.class(iri.clone()))));
    out.push(assertion(&iri, lit_ann(b, &format!("{OIO}id"), id)));

    let mut has_namespace = false;
    let mut intersection: Vec<ClassExpression<A>> = Vec::new();
    let mut union: Vec<ClassExpression<A>> = Vec::new();
    for line in &f.lines {
        let tag = line.tag;
        has_namespace |= tag == Tag::Namespace;
        let quals = quals_of(line);
        let subject = || {
            gci_subject(ctx, &iri, &quals)
                .unwrap_or_else(|| ClassExpression::Class(b.class(iri.clone())))
        };
        let qa = qual_anns(ctx, &quals);
        if let Some(c) = meta_assertion(line, &iri, ctx) {
            out.push(with_anns(c, qa));
            continue;
        }
        let (val0, val1) = (v0(&line.val), v1(&line.val));
        match tag {
            Tag::IsA => {
                if let Some(parent) = val0 {
                    out.push(component_ann(
                        SubClassOf {
                            sub: subject(),
                            sup: ClassExpression::Class(ctx.class(parent)),
                        },
                        qa,
                    ));
                }
            }
            Tag::Relationship => {
                if let (Some(rel), Some(filler)) = (val0, val1) {
                    let rel_iri = ctx.expand_rel(rel);
                    if ctx.metadata_tags.contains(rel_iri.as_ref()) {
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
            Tag::AltId => {
                if let Some(alt) = val0 {
                    out.push(assertion(
                        &iri,
                        lit_ann(b, &format!("{OIO}hasAlternativeId"), alt.trim()),
                    ));
                    let alt_iri = ctx.expand(alt.trim());
                    if alt_iri != iri && !alt.trim().contains(char::is_whitespace) {
                        out.push(component(DeclareClass(b.class(alt_iri.clone()))));
                        out.push(assertion(
                            &alt_iri,
                            is_true_annotation(b, OWL_DEPRECATED.to_string(), "true"),
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
            Tag::IntersectionOf => {
                intersection.push(match (val0, val1) {
                    (Some(rel), Some(filler)) => ClassExpression::ObjectSomeValuesFrom {
                        ope: ope(ctx, rel),
                        bce: Box::new(ClassExpression::Class(ctx.class(filler))),
                    },
                    (Some(genus), None) => ClassExpression::Class(ctx.class(genus)),
                    _ => continue,
                });
            }
            Tag::UnionOf => {
                if let Some(c) = val0 {
                    union.push(ClassExpression::Class(ctx.class(c)));
                }
            }
            Tag::EquivalentTo => {
                if let Some(c) = val0 {
                    out.push(component_ann(
                        EquivalentClasses(vec![
                            ClassExpression::Class(b.class(iri.clone())),
                            ClassExpression::Class(ctx.class(c)),
                        ]),
                        qa,
                    ));
                }
            }
            Tag::DisjointFrom => {
                if let Some(c) = val0 {
                    out.push(component_ann(
                        DisjointClasses(vec![
                            ClassExpression::Class(b.class(iri.clone())),
                            ClassExpression::Class(ctx.class(c)),
                        ]),
                        qa,
                    ));
                }
            }
            _ => {}
        }
    }

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
    default_namespace(&mut out, &iri, has_namespace, ctx);
    Ok(out)
}

/// The oboInOwl local name echoing a boolean property-characteristic tag.
fn char_tag_local(tag: Tag) -> &'static str {
    match tag {
        Tag::IsTransitive => "is_transitive",
        Tag::IsSymmetric => "is_symmetric",
        Tag::IsReflexive => "is_reflexive",
        Tag::IsAsymmetric => "is_asymmetric",
        Tag::IsFunctional => "is_functional",
        Tag::IsInverseFunctional => "is_inverse_functional",
        _ => "",
    }
}

fn typedef<A: ForIRI>(
    f: &Frame<'_>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    let b = ctx.build;
    let mut out = Vec::new();
    let id = f.id.trim();
    let iri = ctx.expand_rel(id);
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
    for line in &f.lines {
        let tag = line.tag;
        has_namespace |= tag == Tag::Namespace;
        let quals = quals_of(line);
        let qa = qual_anns(ctx, &quals);
        if let Some(c) = meta_assertion(line, &iri, ctx) {
            out.push(with_anns(c, qa));
            continue;
        }
        let val0 = v0(&line.val);
        match tag {
            Tag::IsMetadata => {
                if val0.map(str::trim) == Some("true") {
                    out.push(assertion(
                        &iri,
                        is_true_annotation(b, format!("{OIO}is_metadata_tag"), "true"),
                    ));
                }
            }
            Tag::IsA if is_meta => {
                if let Some(sup) = val0 {
                    out.push(component_ann(
                        SubAnnotationPropertyOf {
                            sub: b.annotation_property(iri.clone()),
                            sup: b.annotation_property(ctx.expand_rel(sup)),
                        },
                        qa,
                    ));
                }
            }
            _ if is_meta => {}
            Tag::IsA => {
                if let Some(sup) = val0 {
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
            Tag::InverseOf => {
                if let Some(other) = val0 {
                    out.push(component_ann(
                        InverseObjectProperties(
                            ObjectProperty(iri.clone()),
                            ObjectProperty(ctx.expand_rel(other)),
                        ),
                        qa,
                    ));
                }
            }
            Tag::Domain => {
                if let Some(c) = val0 {
                    out.push(component_ann(
                        ObjectPropertyDomain {
                            ope: ope(ctx, id),
                            ce: ClassExpression::Class(ctx.class(c)),
                        },
                        qa,
                    ));
                }
            }
            Tag::Range => {
                if let Some(c) = val0 {
                    out.push(component_ann(
                        ObjectPropertyRange {
                            ope: ope(ctx, id),
                            ce: ClassExpression::Class(ctx.class(c)),
                        },
                        qa,
                    ));
                }
            }
            Tag::IsTransitive
            | Tag::IsSymmetric
            | Tag::IsReflexive
            | Tag::IsAsymmetric
            | Tag::IsFunctional
            | Tag::IsInverseFunctional => {
                if val0.map(str::trim) == Some("true") {
                    let p = ope(ctx, id);
                    out.push(match tag {
                        Tag::IsTransitive => component_ann(TransitiveObjectProperty(p), qa),
                        Tag::IsSymmetric => component_ann(SymmetricObjectProperty(p), qa),
                        Tag::IsReflexive => component_ann(ReflexiveObjectProperty(p), qa),
                        Tag::IsAsymmetric => component_ann(AsymmetricObjectProperty(p), qa),
                        Tag::IsFunctional => component_ann(FunctionalObjectProperty(p), qa),
                        _ => component_ann(InverseFunctionalObjectProperty(p), qa),
                    });
                } else {
                    out.push(assertion(
                        &iri,
                        is_true_annotation(b, format!("{OIO}{}", char_tag_local(tag)), "false"),
                    ));
                }
            }
            _ => {}
        }
    }
    default_namespace(&mut out, &iri, has_namespace, ctx);
    Ok(out)
}

fn instance<A: ForIRI>(
    f: &Frame<'_>,
    ctx: &Context<'_, A>,
) -> Result<Vec<AnnotatedComponent<A>>, HornedError> {
    let b = ctx.build;
    let mut out = Vec::new();
    let id = f.id.trim();
    let iri = ctx.expand(id);
    out.push(component(DeclareNamedIndividual(
        b.named_individual(iri.clone()),
    )));
    out.push(assertion(&iri, lit_ann(b, &format!("{OIO}id"), id)));
    let this = || Individual::Named(NamedIndividual(iri.clone()));

    let mut has_namespace = false;
    for line in &f.lines {
        let tag = line.tag;
        has_namespace |= tag == Tag::Namespace;
        let quals = quals_of(line);
        let qa = qual_anns(ctx, &quals);
        if let Some(c) = meta_assertion(line, &iri, ctx) {
            out.push(with_anns(c, qa));
            continue;
        }
        let (val0, val1) = (v0(&line.val), v1(&line.val));
        match tag {
            Tag::InstanceOf => {
                if let Some(c) = val0 {
                    out.push(component_ann(
                        ClassAssertion {
                            ce: ClassExpression::Class(ctx.class(c)),
                            i: this(),
                        },
                        qa,
                    ));
                }
            }
            Tag::Relationship => {
                if let (Some(rel), Some(target)) = (val0, val1) {
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
    default_namespace(&mut out, &iri, has_namespace, ctx);
    Ok(out)
}
