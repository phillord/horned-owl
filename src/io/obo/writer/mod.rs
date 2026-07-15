//! OBO flat-file format 1.4 writer.
//!
//! Renders a horned-owl ontology back to OBO 1.4 for the OBO-expressible
//! fragment, the inverse of [`crate::io::obo::reader`], giving read/write
//! round-trip (issue #181).
//!
//! Strategy: the reader stamps every real stanza with an `oboInOwl:id`
//! annotation, so an entity gets a stanza here iff it has one. Declarations and
//! built-in property labels that the reader's finalisation passes synthesise
//! (referenced-entity declarations, `oboInOwl:*` labels) are NOT emitted — the
//! reader re-derives them, so round-trip stays stable. Correctness is checked
//! by `read(write(read(x))) == read(x)` over the oracle corpus (see tests).

use std::collections::BTreeMap;
use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::{
    AnnotatedComponent, AnnotationValue, ClassExpression as CE, Component, ForIRI, Individual,
    Literal, ObjectPropertyExpression as OPE,
};
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;

const OBO: &str = "http://purl.obolibrary.org/obo/";
const OIO: &str = "http://www.geneontology.org/formats/oboInOwl#";
const RDFS_LABEL: &str = "http://www.w3.org/2000/01/rdf-schema#label";
const RDFS_COMMENT: &str = "http://www.w3.org/2000/01/rdf-schema#comment";
const IAO_DEF: &str = "http://purl.obolibrary.org/obo/IAO_0000115";
const IAO_REPLACED_BY: &str = "http://purl.obolibrary.org/obo/IAO_0100001";
const OWL_DEPRECATED: &str = "http://www.w3.org/2002/07/owl#deprecated";

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Kind {
    Term,
    Typedef,
    Instance,
}

/// A stanza under construction: its OBO id and its (unordered) clause lines.
#[derive(Default)]
struct Stanza {
    id: String,
    clauses: Vec<String>,
}

/// Write an ontology to `write` in OBO flat-file format 1.4.
pub fn write<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<W, HornedError> {
    // Ontology IRI drives the `#`-namespace used to compress bare-name ids.
    let mut onto_iri: Option<String> = None;
    for ac in ont.iter() {
        if let Component::OntologyID(o) = &ac.component {
            if let Some(i) = &o.iri {
                onto_iri = Some(i.to_string());
            }
        }
    }
    let onto_ns = onto_iri
        .as_deref()
        .and_then(|i| i.strip_prefix(OBO))
        .and_then(|r| r.strip_suffix(".owl"))
        .map(|o| format!("{OBO}{o}#"));

    // The non-implicit prefixes are the document's `idspace:` declarations. They
    // must be emitted (so re-read resolves those CURIEs the same way) and used
    // to compress matching IRIs back to `PREFIX:local`. Longest URL first so the
    // most specific idspace wins. Implicit OBO prefixes are never emitted.
    const IMPLICIT: [&str; 8] = ["obo", "oboInOwl", "xsd", "rdf", "rdfs", "owl", "dc", "xml"];
    let mut idspaces: Vec<(String, String)> = mapping
        .map(|m| {
            m.mappings()
                .filter(|(p, _)| !IMPLICIT.contains(&p.as_str()))
                .map(|(p, u)| (p.clone(), u.clone()))
                .collect()
        })
        .unwrap_or_default();
    idspaces.sort_by(|a, b| b.1.len().cmp(&a.1.len()));
    let cz = |iri: &str| compress(iri, onto_ns.as_deref(), &idspaces);

    // Pass 1: the stanza entities (those with an oboInOwl:id) and their kinds.
    let mut ids: BTreeMap<String, String> = BTreeMap::new();
    let mut kinds: BTreeMap<String, Kind> = BTreeMap::new();
    for ac in ont.iter() {
        match &ac.component {
            Component::AnnotationAssertion(a) if a.ann.ap.0.as_ref() == format!("{OIO}id") => {
                if let (crate::model::AnnotationSubject::IRI(s), AnnotationValue::Literal(l)) =
                    (&a.subject, &a.ann.av)
                {
                    ids.insert(s.as_ref().to_string(), literal_text(l));
                }
            }
            Component::DeclareClass(d) => {
                kinds.insert(d.0.0.as_ref().to_string(), Kind::Term);
            }
            Component::DeclareObjectProperty(d) => {
                kinds.insert(d.0.0.as_ref().to_string(), Kind::Typedef);
            }
            // An annotation property with an oboInOwl:id is a metadata-tag
            // [Typedef] (only those get a stanza); others have no id.
            Component::DeclareAnnotationProperty(d) => {
                kinds
                    .entry(d.0.0.as_ref().to_string())
                    .or_insert(Kind::Typedef);
            }
            Component::DeclareNamedIndividual(d) => {
                kinds.insert(d.0.0.as_ref().to_string(), Kind::Instance);
            }
            _ => {}
        }
    }

    let mut stanzas: BTreeMap<(Kind, String), Stanza> = BTreeMap::new();
    let mut header: Vec<String> = Vec::new();

    let key_of = |iri: &str| -> Option<(Kind, String)> {
        let id = ids.get(iri)?;
        let kind = *kinds.get(iri).unwrap_or(&Kind::Term);
        Some((kind, id.clone()))
    };

    // Every entity with an oboInOwl:id is a stanza, even one with no further
    // clauses (e.g. a `[Typedef]` that is just an id) — pre-create it so a bare
    // stanza is not dropped when the clause loop finds nothing to attach.
    for (iri, id) in &ids {
        let kind = *kinds.get(iri).unwrap_or(&Kind::Term);
        stanzas.entry((kind, id.clone())).or_insert_with(|| Stanza {
            id: id.clone(),
            clauses: Vec::new(),
        });
    }

    for ac in ont.iter() {
        for (owner_iri, line) in clause_lines(ac, &cz) {
            if let Some(key) = key_of(&owner_iri) {
                let s = stanzas.entry(key.clone()).or_default();
                s.id = key.1;
                s.clauses.push(line);
            }
        }
        if let Some(line) = header_line(ac, &cz) {
            header.push(line);
        }
    }

    // Emit header, then stanzas grouped Term / Typedef / Instance.
    for (p, u) in &idspaces {
        header.push(format!("idspace: {p} {u}"));
    }
    header.sort();
    header.dedup();
    for h in &header {
        writeln!(write, "{h}")?;
    }
    for ((kind, _), s) in &stanzas {
        let tag = match kind {
            Kind::Term => "Term",
            Kind::Typedef => "Typedef",
            Kind::Instance => "Instance",
        };
        writeln!(write, "\n[{tag}]")?;
        writeln!(write, "id: {}", s.id)?;
        let mut clauses = s.clauses.clone();
        clauses.sort();
        for c in clauses {
            writeln!(write, "{c}")?;
        }
    }
    Ok(write)
}

/// Header-level component → header line, or `None`.
fn header_line<A: ForIRI>(
    ac: &AnnotatedComponent<A>,
    cz: &impl Fn(&str) -> String,
) -> Option<String> {
    match &ac.component {
        Component::OntologyID(o) => {
            let iri = o.iri.as_ref()?.to_string();
            let ont = iri
                .strip_prefix(OBO)
                .and_then(|r| r.strip_suffix(".owl"))
                .map(String::from)
                .unwrap_or(iri);
            Some(format!("ontology: {ont}"))
        }
        Component::Import(i) => Some(format!("import: {}", i.0)),
        Component::OntologyAnnotation(oa) => {
            let ap = oa.0.ap.0.as_ref();
            let v = value_text(&oa.0.av, cz);
            match ap {
                _ if ap == format!("{OIO}hasOBOFormatVersion") => {
                    Some(format!("format-version: {v}"))
                }
                _ if ap == format!("{OIO}default-namespace") => {
                    Some(format!("default-namespace: {v}"))
                }
                _ if ap == RDFS_COMMENT => Some(format!("remark: {v}")),
                _ => None,
            }
        }
        _ => None,
    }
}

/// A component → the `(owner-IRI, clause-line)` pairs it contributes to a stanza.
fn clause_lines<A: ForIRI>(
    ac: &AnnotatedComponent<A>,
    cz: &impl Fn(&str) -> String,
) -> Vec<(String, String)> {
    match &ac.component {
        Component::AnnotationAssertion(a) => {
            let subj = match &a.subject {
                crate::model::AnnotationSubject::IRI(i) => i.as_ref().to_string(),
                _ => return vec![],
            };
            annotation_clause(a.ann.ap.0.as_ref(), &a.ann.av, &ac.ann, cz)
                .map(|l| vec![(subj, l)])
                .unwrap_or_default()
        }
        Component::SubClassOf(s) => {
            // The subject is a plain class, or a GCI subject
            // `C ⊓ (gci_rel some gci_filler)` → recover the gci_* qualifiers.
            let (owner, gci) = match &s.sub {
                CE::Class(c) => (c.0.as_ref().to_string(), vec![]),
                CE::ObjectIntersectionOf(v) if v.len() == 2 => match (&v[0], &v[1]) {
                    (
                        CE::Class(c),
                        CE::ObjectSomeValuesFrom {
                            ope: OPE::ObjectProperty(gr),
                            bce,
                        },
                    ) => {
                        let CE::Class(gf) = bce.as_ref() else {
                            return vec![];
                        };
                        (
                            c.0.as_ref().to_string(),
                            vec![
                                ("gci_relation".to_string(), cz(gr.0.as_ref())),
                                ("gci_filler".to_string(), cz(gf.0.as_ref())),
                            ],
                        )
                    }
                    _ => return vec![],
                },
                _ => return vec![],
            };
            let quals = qualifiers(ac, &gci, cz);
            match &s.sup {
                CE::Class(p) => vec![(owner, format!("is_a: {}{quals}", cz(p.0.as_ref())))],
                CE::ObjectSomeValuesFrom {
                    ope: OPE::ObjectProperty(r),
                    bce,
                } => {
                    if let CE::Class(f) = bce.as_ref() {
                        vec![(
                            owner,
                            format!(
                                "relationship: {} {}{quals}",
                                cz(r.0.as_ref()),
                                cz(f.0.as_ref())
                            ),
                        )]
                    } else {
                        vec![]
                    }
                }
                _ => vec![],
            }
        }
        Component::EquivalentClasses(e) if e.0.len() == 2 => {
            let CE::Class(c) = &e.0[0] else { return vec![] };
            let owner = c.0.as_ref().to_string();
            match &e.0[1] {
                CE::Class(d) => vec![(owner, format!("equivalent_to: {}", cz(d.0.as_ref())))],
                // intersection_of / union_of are multiple lines building ONE
                // order-sensitive axiom; emit them as a single block so the
                // stanza's clause sort keeps the operands in Vec order.
                CE::ObjectIntersectionOf(v) => {
                    let lines: Vec<String> = v
                        .iter()
                        .filter_map(|op| Some(format!("intersection_of: {}", operand(op, cz)?)))
                        .collect();
                    vec![(owner, lines.join("\n"))]
                }
                CE::ObjectUnionOf(v) => {
                    let lines: Vec<String> = v
                        .iter()
                        .filter_map(|op| match op {
                            CE::Class(x) => Some(format!("union_of: {}", cz(x.0.as_ref()))),
                            _ => None,
                        })
                        .collect();
                    vec![(owner, lines.join("\n"))]
                }
                _ => vec![],
            }
        }
        Component::DisjointClasses(d) if d.0.len() == 2 => {
            if let (CE::Class(a), CE::Class(b)) = (&d.0[0], &d.0[1]) {
                vec![(
                    a.0.as_ref().to_string(),
                    format!("disjoint_from: {}", cz(b.0.as_ref())),
                )]
            } else {
                vec![]
            }
        }
        Component::SubObjectPropertyOf(s) => {
            use crate::model::SubObjectPropertyExpression as SOPE;
            if let (
                SOPE::ObjectPropertyExpression(OPE::ObjectProperty(sub)),
                OPE::ObjectProperty(sup),
            ) = (&s.sub, &s.sup)
            {
                vec![(
                    sub.0.as_ref().to_string(),
                    format!("is_a: {}", cz(sup.0.as_ref())),
                )]
            } else {
                vec![]
            }
        }
        Component::InverseObjectProperties(a) => {
            vec![(
                a.0.0.as_ref().to_string(),
                format!("inverse_of: {}", cz(a.1.0.as_ref())),
            )]
        }
        Component::ObjectPropertyDomain(d) => op_class(&d.ope, &d.ce, "domain", cz),
        Component::ObjectPropertyRange(r) => op_class(&r.ope, &r.ce, "range", cz),
        Component::TransitiveObjectProperty(p) => characteristic(&p.0, "is_transitive"),
        Component::SymmetricObjectProperty(p) => characteristic(&p.0, "is_symmetric"),
        Component::ReflexiveObjectProperty(p) => characteristic(&p.0, "is_reflexive"),
        Component::AsymmetricObjectProperty(p) => characteristic(&p.0, "is_asymmetric"),
        Component::FunctionalObjectProperty(p) => characteristic(&p.0, "is_functional"),
        Component::InverseFunctionalObjectProperty(p) => {
            characteristic(&p.0, "is_inverse_functional")
        }
        Component::ClassAssertion(a) => {
            if let (CE::Class(c), Individual::Named(i)) = (&a.ce, &a.i) {
                vec![(
                    i.0.as_ref().to_string(),
                    format!("instance_of: {}", cz(c.0.as_ref())),
                )]
            } else {
                vec![]
            }
        }
        Component::ObjectPropertyAssertion(a) => {
            if let (OPE::ObjectProperty(r), Individual::Named(from), Individual::Named(to)) =
                (&a.ope, &a.from, &a.to)
            {
                vec![(
                    from.0.as_ref().to_string(),
                    format!("relationship: {} {}", cz(r.0.as_ref()), cz(to.0.as_ref())),
                )]
            } else {
                vec![]
            }
        }
        _ => vec![],
    }
}

/// An intersection_of operand: a genus (Class) or a differentia (R some F).
fn operand<A: ForIRI>(op: &CE<A>, cz: &impl Fn(&str) -> String) -> Option<String> {
    match op {
        CE::Class(c) => Some(cz(c.0.as_ref())),
        CE::ObjectSomeValuesFrom { ope, bce } => {
            if let (OPE::ObjectProperty(r), CE::Class(f)) = (ope, bce.as_ref()) {
                Some(format!("{} {}", cz(r.0.as_ref()), cz(f.0.as_ref())))
            } else {
                None
            }
        }
        _ => None,
    }
}

fn op_class<A: ForIRI>(
    ope: &OPE<A>,
    ce: &CE<A>,
    tag: &str,
    cz: &impl Fn(&str) -> String,
) -> Vec<(String, String)> {
    if let (OPE::ObjectProperty(p), CE::Class(c)) = (ope, ce) {
        vec![(
            p.0.as_ref().to_string(),
            format!("{tag}: {}", cz(c.0.as_ref())),
        )]
    } else {
        vec![]
    }
}

fn characteristic<A: ForIRI>(ope: &OPE<A>, tag: &str) -> Vec<(String, String)> {
    if let OPE::ObjectProperty(p) = ope {
        vec![(p.0.as_ref().to_string(), format!("{tag}: true"))]
    } else {
        vec![]
    }
}

/// Map an annotation on an entity to its OBO clause line (or `None` to skip:
/// `oboInOwl:id` and `oboInOwl:shorthand` are regenerated by the reader).
fn annotation_clause<A: ForIRI>(
    ap: &str,
    av: &AnnotationValue<A>,
    axiom_ann: &std::collections::BTreeSet<crate::model::Annotation<A>>,
    cz: &impl Fn(&str) -> String,
) -> Option<String> {
    // def and synonym require a `[xref…]` list in the grammar even when empty,
    // so the bracket is always emitted (a source `def: "x" []` has no dbxref
    // annotations, but must still round-trip to `def: "x" []`).
    let dbxrefs = collect_dbxrefs(axiom_ann);
    let brack = format!(" [{}]", dbxrefs.join(", "));
    let text = av_lit(av);
    // A synonym's type (`hasSynonymType`, an IRI) sits between the scope and the
    // `[xref…]` list; without it the synonym round-trips lossily.
    let syn_type = axiom_ann
        .iter()
        .find(|a| a.ap.0.as_ref() == format!("{OIO}hasSynonymType"))
        .and_then(|a| av_iri(&a.av, cz))
        .map(|t| format!(" {t}"))
        .unwrap_or_default();
    let scope = |s: &str| {
        Some(format!(
            "synonym: \"{}\" {s}{syn_type}{}",
            esc_quoted(&av_lit(av)?),
            brack
        ))
    };
    // A single xref's description is carried as an `rdfs:label` axiom annotation.
    let xref_desc = axiom_ann
        .iter()
        .find(|a| a.ap.0.as_ref() == RDFS_LABEL)
        .and_then(|a| av_lit(&a.av))
        .map(|d| format!(" \"{}\"", esc_quoted(&d)))
        .unwrap_or_default();
    // Axiom annotations not consumed as structure (dbxref list / synonym type /
    // xref description) are the clause's trailing `{qualifier}` block.
    let consumed =
        [format!("{OIO}hasDbXref"), format!("{OIO}hasSynonymType"), RDFS_LABEL.to_string()];
    let quals = meta_quals(axiom_ann, &consumed, cz);
    let base = match ap {
        _ if ap == RDFS_LABEL => format!("name: {}", esc_unquoted(&text?)),
        _ if ap == RDFS_COMMENT => format!("comment: {}", esc_unquoted(&text?)),
        _ if ap == IAO_DEF => format!("def: \"{}\"{brack}", esc_quoted(&text?)),
        _ if ap == format!("{OIO}hasOBONamespace") => {
            format!("namespace: {}", esc_unquoted(&text?))
        }
        _ if ap == format!("{OIO}hasAlternativeId") => format!("alt_id: {}", esc_unquoted(&text?)),
        _ if ap == format!("{OIO}is_metadata_tag") => "is_metadata_tag: true".to_string(),
        _ if ap == format!("{OIO}hasDbXref") => {
            format!("xref: {}{xref_desc}", esc_unquoted(&text?))
        }
        _ if ap == format!("{OIO}created_by") => format!("created_by: {}", esc_unquoted(&text?)),
        _ if ap == format!("{OIO}creation_date") => {
            format!("creation_date: {}", esc_unquoted(&text?))
        }
        _ if ap == format!("{OIO}hasExactSynonym") => scope("EXACT")?,
        _ if ap == format!("{OIO}hasNarrowSynonym") => scope("NARROW")?,
        _ if ap == format!("{OIO}hasBroadSynonym") => scope("BROAD")?,
        _ if ap == format!("{OIO}hasRelatedSynonym") => scope("RELATED")?,
        _ if ap == format!("{OIO}inSubset") => format!("subset: {}", av_iri(av, cz)?),
        _ if ap == format!("{OIO}consider") => format!("consider: {}", av_iri(av, cz)?),
        _ if ap == IAO_REPLACED_BY => format!("replaced_by: {}", av_iri(av, cz)?),
        _ if ap == OWL_DEPRECATED => "is_obsolete: true".to_string(),
        _ if ap == format!("{OIO}id") || ap == format!("{OIO}shorthand") => return None,
        // property_value: relation + IRI target or (typed) literal.
        _ => match av {
            AnnotationValue::IRI(i) => format!("property_value: {} {}", cz(ap), cz(i.as_ref())),
            AnnotationValue::Literal(Literal::Datatype {
                literal,
                datatype_iri,
            }) => format!(
                "property_value: {} \"{}\" {}",
                cz(ap),
                esc_quoted(literal),
                cz(datatype_iri.as_ref())
            ),
            AnnotationValue::Literal(l) => {
                format!(
                    "property_value: {} \"{}\" xsd:string",
                    cz(ap),
                    esc_quoted(&literal_text(l))
                )
            }
            _ => return None,
        },
    };
    Some(format!("{base}{quals}"))
}

/// The trailing `{key="value", …}` block for a meta clause: every axiom
/// annotation whose property is not in `consumed` (those are rendered as the
/// clause's dbxref list / synonym type / xref description instead).
fn meta_quals<A: ForIRI>(
    anns: &std::collections::BTreeSet<crate::model::Annotation<A>>,
    consumed: &[String],
    cz: &impl Fn(&str) -> String,
) -> String {
    let mut qs: Vec<String> = anns
        .iter()
        .filter(|a| !consumed.iter().any(|c| c == a.ap.0.as_ref()))
        .filter_map(|a| {
            Some(format!(
                "{}=\"{}\"",
                short_key(a.ap.0.as_ref(), cz),
                esc_quoted(&av_lit(&a.av)?)
            ))
        })
        .collect();
    qs.sort();
    if qs.is_empty() {
        String::new()
    } else {
        format!(" {{{}}}", qs.join(", "))
    }
}

fn collect_dbxrefs<A: ForIRI>(
    anns: &std::collections::BTreeSet<crate::model::Annotation<A>>,
) -> Vec<String> {
    anns.iter()
        .filter(|a| a.ap.0.as_ref() == format!("{OIO}hasDbXref"))
        .filter_map(|a| av_lit(&a.av).as_deref().map(esc_xref))
        .collect()
}

/// Escape a dbxref id for the `[…]` list: `,` and `]` delimit the list and `\`
/// is the escape char, so all three are backslash-escaped (the reader unescapes
/// them). Without this, a dbxref containing a comma re-reads as two xrefs.
fn esc_xref(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace(',', "\\,")
        .replace(']', "\\]")
}

/// Trailing `{key="value"}` qualifier block from an axiom's annotations.
fn qualifiers<A: ForIRI>(
    ac: &AnnotatedComponent<A>,
    extra: &[(String, String)],
    cz: &impl Fn(&str) -> String,
) -> String {
    let mut qs: Vec<String> = ac
        .ann
        .iter()
        .filter_map(|a| {
            let key = short_key(a.ap.0.as_ref(), cz);
            Some(format!("{key}=\"{}\"", av_lit(&a.av)?))
        })
        .collect();
    qs.extend(extra.iter().map(|(k, v)| format!("{k}=\"{v}\"")));
    qs.sort();
    if qs.is_empty() {
        String::new()
    } else {
        format!(" {{{}}}", qs.join(", "))
    }
}

/// A qualifier key: an oboInOwl-local property is written bare, else compressed.
fn short_key(ap: &str, cz: &impl Fn(&str) -> String) -> String {
    ap.strip_prefix(OIO)
        .map(String::from)
        .unwrap_or_else(|| cz(ap))
}

fn av_lit<A: ForIRI>(av: &AnnotationValue<A>) -> Option<String> {
    match av {
        AnnotationValue::Literal(l) => Some(literal_text(l)),
        _ => None,
    }
}

fn av_iri<A: ForIRI>(av: &AnnotationValue<A>, cz: &impl Fn(&str) -> String) -> Option<String> {
    match av {
        AnnotationValue::IRI(i) => Some(cz(i.as_ref())),
        _ => None,
    }
}

/// Escape a value for an OBO **quoted** string (`def`/`synonym`/property_value
/// literal): a bare `"` ends the string, so `\` and `"` must be escaped, plus
/// the whitespace escapes. Reversed by the reader's `unescape`.
fn esc_quoted(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
}

/// Escape a value for an OBO **unquoted** clause (`name`/`comment`/…): the
/// grammar's OboChar treats `\`, `!` (comment) and `{` (qualifier) as special,
/// and the value runs to end-of-line, so those plus newlines/tabs are escaped.
fn esc_unquoted(s: &str) -> String {
    s.replace('\\', "\\\\")
        .replace('\n', "\\n")
        .replace('\t', "\\t")
        .replace('!', "\\!")
        .replace('{', "\\{")
}

fn literal_text<A: ForIRI>(l: &Literal<A>) -> String {
    match l {
        Literal::Simple { literal } => literal.clone(),
        Literal::Language { literal, .. } => literal.clone(),
        Literal::Datatype { literal, .. } => literal.clone(),
    }
}

fn value_text<A: ForIRI>(av: &AnnotationValue<A>, cz: &impl Fn(&str) -> String) -> String {
    match av {
        AnnotationValue::Literal(l) => literal_text(l),
        AnnotationValue::IRI(i) => cz(i.as_ref()),
        AnnotationValue::AnonymousIndividual(a) => a.0.as_ref().to_string(),
    }
}

/// Compress an IRI to an OBO id: the inverse of the reader's `expand`.
fn compress(iri: &str, onto_ns: Option<&str>, idspaces: &[(String, String)]) -> String {
    // A declared idspace wins (most specific first, already sorted by URL length).
    for (pre, url) in idspaces {
        if let Some(local) = iri.strip_prefix(url.as_str()) {
            return format!("{pre}:{local}");
        }
    }
    if let Some(ns) = onto_ns {
        if let Some(local) = iri.strip_prefix(ns) {
            return local.to_string(); // ontology-native bare name
        }
    }
    if let Some(rest) = iri.strip_prefix(OBO) {
        if let Some((pre, local)) = rest.split_once('_') {
            if !pre.is_empty() && !local.is_empty() && !local.contains('_') {
                return format!("{pre}:{local}");
            }
        }
        // No unambiguous `PREFIX:LOCAL` (local has underscores, or no `_` at
        // all): emitting the bare `rest` would re-read via the ontology `#`
        // namespace to a different IRI, so emit the full IRI (a URL id, which
        // round-trips exactly). e.g. `obo/OBO_REL_has_quality`.
        return iri.to_string();
    }
    for (ns, p) in [
        ("http://www.w3.org/2001/XMLSchema#", "xsd"),
        ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf"),
        ("http://www.w3.org/2000/01/rdf-schema#", "rdfs"),
        ("http://www.w3.org/2002/07/owl#", "owl"),
    ] {
        if let Some(local) = iri.strip_prefix(ns) {
            return format!("{p}:{local}");
        }
    }
    iri.to_string()
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::fs::read_dir;
    use std::path::PathBuf;

    use crate::model::{AnnotatedComponent, RcStr};
    use crate::ontology::component_mapped::ComponentMappedOntology;
    use crate::ontology::set::SetOntology;

    fn read(s: &str) -> SetOntology<RcStr> {
        crate::io::obo::reader::read::<RcStr, SetOntology<RcStr>, _>(
            s.as_bytes(),
            Default::default(),
        )
        .unwrap()
        .0
    }

    fn axioms(ont: &SetOntology<RcStr>) -> BTreeSet<String> {
        ont.iter().map(|ac| format!("{ac:?}")).collect()
    }

    /// read(write(read(x))) == read(x) over every fixture in the oracle corpus.
    #[test]
    fn round_trip_corpus() {
        let mut failures = Vec::new();
        for entry in read_dir("./src/ont/obo").unwrap() {
            let path: PathBuf = entry.unwrap().path();
            if path.extension().is_none_or(|e| e != "obo") {
                continue;
            }
            let doc = std::fs::read_to_string(&path).unwrap();
            let (a, prefixes) = crate::io::obo::reader::read::<RcStr, SetOntology<RcStr>, _>(
                doc.as_bytes(),
                Default::default(),
            )
            .unwrap();
            let cmo: ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>> = a.clone().into();
            let out = super::write(Vec::new(), &cmo, Some(&prefixes)).unwrap();
            let text = String::from_utf8(out).unwrap();
            let b = read(&text);

            let (sa, sb) = (axioms(&a), axioms(&b));
            let name = path.file_name().unwrap().to_string_lossy().to_string();
            if sa != sb {
                let lost: Vec<_> = sa.difference(&sb).cloned().collect();
                let gained: Vec<_> = sb.difference(&sa).cloned().collect();
                println!("\n=== {name} NOT stable ===\n--- OBO written ---\n{text}");
                for l in &lost {
                    println!("  lost:   {l}");
                }
                for g in &gained {
                    println!("  gained: {g}");
                }
                failures.push(name);
            } else {
                println!("{name}: round-trip stable ({} axioms)", sa.len());
            }
        }
        assert!(failures.is_empty(), "round-trip failed for: {failures:?}");
    }

    /// alt_id round-trips: the writer emits `alt_id:` from hasAlternativeId and
    /// omits the materialised deprecated stub (the reader regenerates it).
    /// (Kept out of the oracle corpus: our reader emits two builtin-metadata
    /// annotations ROBOT does not for alt_id stubs.)
    #[test]
    fn alt_id_round_trips() {
        let doc = "format-version: 1.2\nontology: t\n\n\
                   [Term]\nid: GO:0001\nname: c\nalt_id: GO:0002\n";
        let a = read(doc);
        let cmo: ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>> = a.clone().into();
        let out = super::write(Vec::new(), &cmo, None).unwrap();
        let b = read(&String::from_utf8(out).unwrap());
        assert_eq!(axioms(&a), axioms(&b), "alt_id must round-trip");
        // and the written form uses `alt_id:`, not property_value
        let text = String::from_utf8(super::write(Vec::new(), &cmo, None).unwrap()).unwrap();
        assert!(text.contains("alt_id: GO:0002"), "got:\n{text}");
    }
}
