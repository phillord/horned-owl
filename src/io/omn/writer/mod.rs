use std::collections::BTreeMap;
use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::Annotation;
use crate::model::AnnotationSubject;
use crate::model::Atom;
use crate::model::Component;
use crate::model::ComponentKind;
use crate::model::ForIRI;
use crate::model::ObjectPropertyExpression;
use crate::model::SubObjectPropertyExpression;
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;

pub mod as_manchester;
pub use as_manchester::{AsManchester, Manchester};

// ---------------------------------------------------------------------------
// Frame key: identifies the subject entity of a frame.
// ---------------------------------------------------------------------------

/// The kind of entity a frame is headed by.
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum FrameKind {
    Class,
    ObjectProperty,
    DataProperty,
    AnnotationProperty,
    Individual,
    Datatype,
}

/// A frame accumulates clause lines for a single named entity.
#[derive(Clone, Debug)]
struct Frame {
    kind: FrameKind,
    /// IRI string of the subject entity (used as display key and BTreeMap key).
    subject_iri: String,
    /// Rendered clause strings, e.g. `"SubClassOf: <http://t/B>"`.
    clauses: Vec<String>,
}

// ---------------------------------------------------------------------------
// Write a whole-ontology Manchester document.
// ---------------------------------------------------------------------------

/// Write an ontology to `write` in OWL
/// [Manchester Syntax](https://www.w3.org/TR/2012/REC-owl2-manchester-syntax-20121211/),
/// using the given `PrefixMapping`.
///
/// The output is a frame-grouped document: prefix declarations, a conformant
/// `Ontology:` header (with nested `Import:` and `Annotations:` sub-lines when
/// present), then one frame per named entity grouping all axioms whose subject
/// is that entity.  Entity annotations (`AnnotationAssertion` with a named-IRI
/// subject) are rendered as `Annotations:` clauses inside the entity's frame.
/// Axioms that do not have a clean named-entity subject (n-ary
/// equivalences/disjunctions over anonymous subjects, SWRL rules, etc.) are
/// emitted as free-standing lines in a trailing `# General axioms` section.
///
/// **Note on the `# General axioms` section:** genuinely-inexpressible components
/// (general anonymous-subject class axioms, SWRL `Rule`, anonymous-subject
/// annotation values) are serialised in **OWL functional syntax** as a stopgap.
/// Those lines are NOT valid Manchester syntax.
pub fn write<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<W, HornedError> {
    let default_mapper = PrefixMapping::default();
    let mapping = mapping.unwrap_or(&default_mapper);

    // -----------------------------------------------------------------------
    // 1. Prefix declarations  (Manchester: `Prefix: prefix: <iri>`)
    // -----------------------------------------------------------------------
    for (name, value) in mapping.mappings() {
        writeln!(write, "Prefix: {name}: <{value}>")?;
    }

    // -----------------------------------------------------------------------
    // misc is declared early so the Ontology: header block below can push
    // anonymous-individual-valued OntologyAnnotation components into it.
    // -----------------------------------------------------------------------
    let mut misc: Vec<String> = Vec::new();

    // Native Manchester top-level `Misc:` lines (§2.5) for non-frameable n-ary
    // axioms (members not all named).  Emitted BEFORE the `# General axioms`
    // block so they parse as `Misc` on re-read (the GeneralAxiomBlock rule
    // swallows everything to EOF).
    let mut misc_axioms: Vec<String> = Vec::new();

    // Complex-LHS GCI frames: `Class: <complexExpr>\n    SubClassOf: <sup>`
    // blocks for SubClassOf axioms whose `sub` is not a named Class.  These
    // are emitted as complete stand-alone frame texts (already fully rendered,
    // including the leading blank line), bypassing the normal `subject_iri` /
    // `render_iri_to_string` path so the complex expression is never mangled.
    // They are collected separately from `frames` (which is keyed by named-IRI
    // subject) and emitted after all named-entity frames but BEFORE the
    // `# General axioms` block (GeneralAxiomBlock swallows to EOF).
    let mut complex_gci_frames: Vec<String> = Vec::new();

    // -----------------------------------------------------------------------
    // 2. Conformant Ontology: header (IRI + nested Import: + Annotations:)
    //    W3C Manchester puts imports and ontology annotations INSIDE the
    //    Ontology: frame, not at top level. §2.5's AnnotationTarget admits
    //    Literal | IRI | AnonymousIndividual, so anon-valued annotations
    //    render natively (`_:label`).
    // -----------------------------------------------------------------------
    {
        let (header_iri, header_viri): (
            Option<crate::model::IRI<A>>,
            Option<crate::model::IRI<A>>,
        ) = {
            let mut id_iter = ont.i().component_for_kind(ComponentKind::OntologyID);
            if let Some(ac) = id_iter.next()
                && let Component::OntologyID(oid) = &ac.component
            {
                (oid.iri.clone(), oid.viri.clone())
            } else {
                (None, None)
            }
        };
        let imports: Vec<crate::model::IRI<A>> = ont
            .i()
            .component_for_kind(ComponentKind::Import)
            .filter_map(|ac| {
                if let Component::Import(imp) = &ac.component {
                    Some(imp.0.clone())
                } else {
                    None
                }
            })
            .collect();
        // §2.5 AnnotationTarget admits Literal | IRI | AnonymousIndividual, so
        // every ontology annotation (anon values included) renders natively.
        let conformant_ont_anns: Vec<Annotation<A>> = ont
            .i()
            .component_for_kind(ComponentKind::OntologyAnnotation)
            .filter_map(|ac| {
                if let Component::OntologyAnnotation(oa) = &ac.component {
                    Some(oa.0.clone())
                } else {
                    None
                }
            })
            .collect();
        if header_iri.is_some() || !imports.is_empty() || !conformant_ont_anns.is_empty() {
            writeln!(write)?;
            match &header_iri {
                Some(iri) => match &header_viri {
                    Some(viri) => writeln!(
                        write,
                        "Ontology: {} {}",
                        iri.as_manchester_with_prefixes(mapping),
                        viri.as_manchester_with_prefixes(mapping)
                    )?,
                    None => writeln!(
                        write,
                        "Ontology: {}",
                        iri.as_manchester_with_prefixes(mapping)
                    )?,
                },
                None => writeln!(write, "Ontology:")?,
            }
            for imp in &imports {
                writeln!(
                    write,
                    "    Import: {}",
                    imp.as_manchester_with_prefixes(mapping)
                )?;
            }
            for ann in &conformant_ont_anns {
                writeln!(
                    write,
                    "    Annotations: {}",
                    as_manchester::annotation_to_manchester(ann, mapping)
                )?;
            }
        }
    }

    // -----------------------------------------------------------------------
    // 3. Bucket axioms into frames and a misc list.
    //    Key: (FrameKind, subject_iri_string)
    // -----------------------------------------------------------------------
    let mut frames: BTreeMap<(FrameKind, String), Frame> = BTreeMap::new();

    // Helper macro: ensure frame exists and push a clause line.
    macro_rules! push_clause {
        ($fkind:expr, $subject_iri:expr, $clause:expr) => {{
            let key = ($fkind, $subject_iri.to_string());
            let frame = frames.entry(key).or_insert_with(|| Frame {
                kind: $fkind,
                subject_iri: $subject_iri.to_string(),
                clauses: Vec::new(),
            });
            frame.clauses.push($clause);
        }};
    }

    // Helper macro: ensure an empty frame header exists (for Declare* axioms).
    macro_rules! ensure_frame {
        ($fkind:expr, $subject_iri:expr) => {{
            let key = ($fkind, $subject_iri.to_string());
            frames.entry(key).or_insert_with(|| Frame {
                kind: $fkind,
                subject_iri: $subject_iri.to_string(),
                clauses: Vec::new(),
            });
        }};
    }

    // Helper: produce the frame-key string for an individual subject.
    // Named individuals use their IRI string; anonymous individuals use `_:<label>`
    // (matching the Manchester syntax the reader expects when re-parsing the frame).
    fn individual_subject_key<A: ForIRI>(i: &crate::model::Individual<A>) -> String {
        match i {
            crate::model::Individual::Named(ni) => ni.0.as_ref().to_string(),
            crate::model::Individual::Anonymous(ai) => format!("_:{}", ai.0.as_ref()),
        }
    }

    // Helper: extract the raw-property IRI from a simple ObjectPropertyExpression,
    // returning None for InverseObjectProperty (which falls to misc).
    fn ope_iri<A: ForIRI>(ope: &ObjectPropertyExpression<A>) -> Option<&str> {
        if let ObjectPropertyExpression::ObjectProperty(p) = ope {
            Some(p.0.as_ref())
        } else {
            None
        }
    }

    // Helper: produce an optional `Annotations: <entries> ` prefix string for a
    // clause when the `AnnotatedComponent` carries a non-empty annotation set.
    // Returns an empty string when there are no annotations (common case).
    fn ann_prefix<A: ForIRI>(
        ann: &std::collections::BTreeSet<Annotation<A>>,
        pm: &PrefixMapping,
    ) -> String {
        if ann.is_empty() {
            return String::new();
        }
        let entries: Vec<String> = ann
            .iter()
            .map(|a| as_manchester::annotation_to_manchester(a, pm))
            .collect();
        format!("Annotations: {} ", entries.join(", "))
    }

    for kind in ComponentKind::all_kinds() {
        if kind == ComponentKind::OntologyID
            || kind == ComponentKind::DocIRI
            || kind == ComponentKind::Import
            || kind == ComponentKind::OntologyAnnotation
            || kind == ComponentKind::AnnotationAssertion
        {
            continue;
        }
        for ac in ont.i().component_for_kind(kind) {
            let pm = mapping;

            match &ac.component {
                // ---- Declarations ----
                Component::DeclareClass(ax) => {
                    ensure_frame!(FrameKind::Class, ax.0.0.as_ref());
                }
                Component::DeclareObjectProperty(ax) => {
                    ensure_frame!(FrameKind::ObjectProperty, ax.0.0.as_ref());
                }
                Component::DeclareDataProperty(ax) => {
                    ensure_frame!(FrameKind::DataProperty, ax.0.0.as_ref());
                }
                Component::DeclareAnnotationProperty(ax) => {
                    ensure_frame!(FrameKind::AnnotationProperty, ax.0.0.as_ref());
                }
                Component::DeclareNamedIndividual(ax) => {
                    ensure_frame!(FrameKind::Individual, ax.0.0.as_ref());
                }
                Component::DeclareDatatype(ax) => {
                    ensure_frame!(FrameKind::Datatype, ax.0.0.as_ref());
                }

                // ---- Class axioms ----
                Component::SubClassOf(ax) => {
                    if let crate::model::ClassExpression::Class(c) = &ax.sub {
                        let clause = format!(
                            "SubClassOf: {}{}",
                            ann_prefix(&ac.ann, pm),
                            ax.sup.as_manchester_with_prefixes(pm)
                        );
                        push_clause!(FrameKind::Class, c.0.as_ref(), clause);
                    } else {
                        // Complex-LHS GCI: emit as `Class: <complexExpr>` frame.
                        // The subject is rendered as a Manchester class expression
                        // (not an IRI), so we accumulate the full block verbatim
                        // and bypass the named-entity frame machinery entirely.
                        let sub_rendered = ax.sub.as_manchester_with_prefixes(pm).to_string();
                        let sup_rendered = format!(
                            "SubClassOf: {}{}",
                            ann_prefix(&ac.ann, pm),
                            ax.sup.as_manchester_with_prefixes(pm)
                        );
                        complex_gci_frames
                            .push(format!("\nClass: {sub_rendered}\n    {sup_rendered}"));
                    }
                }
                Component::EquivalentClasses(ax) => {
                    if let Some(crate::model::ClassExpression::Class(c)) = ax.0.first() {
                        let others: Vec<String> =
                            ax.0.iter()
                                .skip(1)
                                .map(|ce| ce.as_manchester_with_prefixes(pm).to_string())
                                .collect();
                        if !others.is_empty() {
                            let clause = format!(
                                "EquivalentTo: {}{}",
                                ann_prefix(&ac.ann, pm),
                                others.join(", ")
                            );
                            push_clause!(FrameKind::Class, c.0.as_ref(), clause);
                        }
                    } else if !ax.0.is_empty() {
                        let members: Vec<String> =
                            ax.0.iter()
                                .map(|ce| ce.as_manchester_with_prefixes(pm).to_string())
                                .collect();
                        misc_axioms.push(format!(
                            "EquivalentClasses: {}{}",
                            ann_prefix(&ac.ann, pm),
                            members.join(", ")
                        ));
                    }
                }
                Component::DisjointClasses(ax) => {
                    if let Some(crate::model::ClassExpression::Class(c)) = ax.0.first() {
                        let others: Vec<String> =
                            ax.0.iter()
                                .skip(1)
                                .map(|ce| ce.as_manchester_with_prefixes(pm).to_string())
                                .collect();
                        if !others.is_empty() {
                            let clause = format!(
                                "DisjointWith: {}{}",
                                ann_prefix(&ac.ann, pm),
                                others.join(", ")
                            );
                            push_clause!(FrameKind::Class, c.0.as_ref(), clause);
                        }
                    } else if !ax.0.is_empty() {
                        let members: Vec<String> =
                            ax.0.iter()
                                .map(|ce| ce.as_manchester_with_prefixes(pm).to_string())
                                .collect();
                        misc_axioms.push(format!(
                            "DisjointClasses: {}{}",
                            ann_prefix(&ac.ann, pm),
                            members.join(", ")
                        ));
                    }
                }
                Component::DisjointUnion(ax) => {
                    let members: Vec<String> =
                        ax.1.iter()
                            .map(|ce| ce.as_manchester_with_prefixes(pm).to_string())
                            .collect();
                    let clause = format!(
                        "DisjointUnionOf: {}{}",
                        ann_prefix(&ac.ann, pm),
                        members.join(", ")
                    );
                    push_clause!(FrameKind::Class, ax.0.0.as_ref(), clause);
                }

                // ---- Object property axioms ----
                Component::SubObjectPropertyOf(ax) => match &ax.sub {
                    SubObjectPropertyExpression::ObjectPropertyExpression(ope) => {
                        if let Some(iri) = ope_iri(ope) {
                            let clause = format!(
                                "SubPropertyOf: {}{}",
                                ann_prefix(&ac.ann, pm),
                                ax.sup.as_manchester_with_prefixes(pm)
                            );
                            push_clause!(FrameKind::ObjectProperty, iri, clause);
                        } else {
                            misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                        }
                    }
                    SubObjectPropertyExpression::ObjectPropertyChain(chain) => {
                        if let Some(iri) = ope_iri(&ax.sup) {
                            let rendered = chain
                                .iter()
                                .map(|o| o.as_manchester_with_prefixes(pm).to_string())
                                .collect::<Vec<_>>()
                                .join(" o ");
                            push_clause!(
                                FrameKind::ObjectProperty,
                                iri,
                                format!(
                                    "SubPropertyChain: {}{}",
                                    ann_prefix(&ac.ann, pm),
                                    rendered
                                )
                            );
                        } else {
                            misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                        }
                    }
                },
                Component::EquivalentObjectProperties(ax) => {
                    if let Some(ope) = ax.0.first() {
                        if let Some(iri) = ope_iri(ope) {
                            let others: Vec<String> =
                                ax.0.iter()
                                    .skip(1)
                                    .map(|o| o.as_manchester_with_prefixes(pm).to_string())
                                    .collect();
                            if !others.is_empty() {
                                let clause = format!(
                                    "EquivalentTo: {}{}",
                                    ann_prefix(&ac.ann, pm),
                                    others.join(", ")
                                );
                                push_clause!(FrameKind::ObjectProperty, iri, clause);
                            }
                        } else {
                            // First member not a named property → no frame subject.
                            // Native `Misc` keyword is `EquivalentProperties:` (object form).
                            let members: Vec<String> =
                                ax.0.iter()
                                    .map(|o| o.as_manchester_with_prefixes(pm).to_string())
                                    .collect();
                            misc_axioms.push(format!(
                                "EquivalentProperties: {}{}",
                                ann_prefix(&ac.ann, pm),
                                members.join(", ")
                            ));
                        }
                    }
                    // empty member list (ax.0.first() == None) is vacuous → dropped,
                    // matching the class/individual n-ary arms.
                }
                Component::DisjointObjectProperties(ax) => {
                    if let Some(ope) = ax.0.first() {
                        if let Some(iri) = ope_iri(ope) {
                            let others: Vec<String> =
                                ax.0.iter()
                                    .skip(1)
                                    .map(|o| o.as_manchester_with_prefixes(pm).to_string())
                                    .collect();
                            if !others.is_empty() {
                                let clause = format!(
                                    "DisjointWith: {}{}",
                                    ann_prefix(&ac.ann, pm),
                                    others.join(", ")
                                );
                                push_clause!(FrameKind::ObjectProperty, iri, clause);
                            }
                        } else {
                            // First member not a named property → no frame subject.
                            // Native `Misc` keyword is `DisjointProperties:` (object form).
                            let members: Vec<String> =
                                ax.0.iter()
                                    .map(|o| o.as_manchester_with_prefixes(pm).to_string())
                                    .collect();
                            misc_axioms.push(format!(
                                "DisjointProperties: {}{}",
                                ann_prefix(&ac.ann, pm),
                                members.join(", ")
                            ));
                        }
                    }
                    // empty member list (ax.0.first() == None) is vacuous → dropped,
                    // matching the class/individual n-ary arms.
                }
                Component::InverseObjectProperties(ax) => {
                    // ax.0 and ax.1 are ObjectProperty (not expression).
                    let clause = format!(
                        "InverseOf: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.1.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::ObjectProperty, ax.0.0.as_ref(), clause);
                }
                Component::ObjectPropertyDomain(ax) => {
                    if let Some(iri) = ope_iri(&ax.ope) {
                        let clause = format!(
                            "Domain: {}{}",
                            ann_prefix(&ac.ann, pm),
                            ax.ce.as_manchester_with_prefixes(pm)
                        );
                        push_clause!(FrameKind::ObjectProperty, iri, clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::ObjectPropertyRange(ax) => {
                    if let Some(iri) = ope_iri(&ax.ope) {
                        let clause = format!(
                            "Range: {}{}",
                            ann_prefix(&ac.ann, pm),
                            ax.ce.as_manchester_with_prefixes(pm)
                        );
                        push_clause!(FrameKind::ObjectProperty, iri, clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::FunctionalObjectProperty(ax) => {
                    if let Some(iri) = ope_iri(&ax.0) {
                        push_clause!(
                            FrameKind::ObjectProperty,
                            iri,
                            format!("Characteristics: {}Functional", ann_prefix(&ac.ann, pm))
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::InverseFunctionalObjectProperty(ax) => {
                    if let Some(iri) = ope_iri(&ax.0) {
                        push_clause!(
                            FrameKind::ObjectProperty,
                            iri,
                            format!(
                                "Characteristics: {}InverseFunctional",
                                ann_prefix(&ac.ann, pm)
                            )
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::ReflexiveObjectProperty(ax) => {
                    if let Some(iri) = ope_iri(&ax.0) {
                        push_clause!(
                            FrameKind::ObjectProperty,
                            iri,
                            format!("Characteristics: {}Reflexive", ann_prefix(&ac.ann, pm))
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::IrreflexiveObjectProperty(ax) => {
                    if let Some(iri) = ope_iri(&ax.0) {
                        push_clause!(
                            FrameKind::ObjectProperty,
                            iri,
                            format!("Characteristics: {}Irreflexive", ann_prefix(&ac.ann, pm))
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::SymmetricObjectProperty(ax) => {
                    if let Some(iri) = ope_iri(&ax.0) {
                        push_clause!(
                            FrameKind::ObjectProperty,
                            iri,
                            format!("Characteristics: {}Symmetric", ann_prefix(&ac.ann, pm))
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::AsymmetricObjectProperty(ax) => {
                    if let Some(iri) = ope_iri(&ax.0) {
                        push_clause!(
                            FrameKind::ObjectProperty,
                            iri,
                            format!("Characteristics: {}Asymmetric", ann_prefix(&ac.ann, pm))
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::TransitiveObjectProperty(ax) => {
                    if let Some(iri) = ope_iri(&ax.0) {
                        push_clause!(
                            FrameKind::ObjectProperty,
                            iri,
                            format!("Characteristics: {}Transitive", ann_prefix(&ac.ann, pm))
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }

                // ---- Data property axioms ----
                Component::SubDataPropertyOf(ax) => {
                    let clause = format!(
                        "SubPropertyOf: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.sup.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::DataProperty, ax.sub.0.as_ref(), clause);
                }
                Component::EquivalentDataProperties(ax) => {
                    if let Some(first) = ax.0.first() {
                        let others: Vec<String> =
                            ax.0.iter()
                                .skip(1)
                                .map(|dp| dp.as_manchester_with_prefixes(pm).to_string())
                                .collect();
                        if !others.is_empty() {
                            let clause = format!(
                                "EquivalentTo: {}{}",
                                ann_prefix(&ac.ann, pm),
                                others.join(", ")
                            );
                            push_clause!(FrameKind::DataProperty, first.0.as_ref(), clause);
                        }
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::DisjointDataProperties(ax) => {
                    if let Some(first) = ax.0.first() {
                        let others: Vec<String> =
                            ax.0.iter()
                                .skip(1)
                                .map(|dp| dp.as_manchester_with_prefixes(pm).to_string())
                                .collect();
                        if !others.is_empty() {
                            let clause = format!(
                                "DisjointWith: {}{}",
                                ann_prefix(&ac.ann, pm),
                                others.join(", ")
                            );
                            push_clause!(FrameKind::DataProperty, first.0.as_ref(), clause);
                        }
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::DataPropertyDomain(ax) => {
                    let clause = format!(
                        "Domain: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.ce.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::DataProperty, ax.dp.0.as_ref(), clause);
                }
                Component::DataPropertyRange(ax) => {
                    let clause = format!(
                        "Range: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.dr.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::DataProperty, ax.dp.0.as_ref(), clause);
                }
                Component::FunctionalDataProperty(ax) => {
                    push_clause!(
                        FrameKind::DataProperty,
                        ax.0.0.as_ref(),
                        format!("Characteristics: {}Functional", ann_prefix(&ac.ann, pm))
                    );
                }

                // ---- Assertion axioms ----
                Component::ClassAssertion(ax) => {
                    let clause = format!(
                        "Types: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.ce.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::Individual, individual_subject_key(&ax.i), clause);
                }
                Component::ObjectPropertyAssertion(ax) => {
                    let clause = format!(
                        "Facts: {}{} {}",
                        ann_prefix(&ac.ann, pm),
                        ax.ope.as_manchester_with_prefixes(pm),
                        ax.to.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(
                        FrameKind::Individual,
                        individual_subject_key(&ax.from),
                        clause
                    );
                }
                Component::NegativeObjectPropertyAssertion(ax) => {
                    let clause = format!(
                        "Facts: {}not {} {}",
                        ann_prefix(&ac.ann, pm),
                        ax.ope.as_manchester_with_prefixes(pm),
                        ax.to.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(
                        FrameKind::Individual,
                        individual_subject_key(&ax.from),
                        clause
                    );
                }
                Component::DataPropertyAssertion(ax) => {
                    let clause = format!(
                        "Facts: {}{} {}",
                        ann_prefix(&ac.ann, pm),
                        ax.dp.as_manchester_with_prefixes(pm),
                        ax.to.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(
                        FrameKind::Individual,
                        individual_subject_key(&ax.from),
                        clause
                    );
                }
                Component::NegativeDataPropertyAssertion(ax) => {
                    let clause = format!(
                        "Facts: {}not {} {}",
                        ann_prefix(&ac.ann, pm),
                        ax.dp.as_manchester_with_prefixes(pm),
                        ax.to.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(
                        FrameKind::Individual,
                        individual_subject_key(&ax.from),
                        clause
                    );
                }
                Component::SameIndividual(ax) => {
                    if let Some(crate::model::Individual::Named(ni)) = ax.0.first() {
                        let others: Vec<String> =
                            ax.0.iter()
                                .skip(1)
                                .map(|i| i.as_manchester_with_prefixes(pm).to_string())
                                .collect();
                        if !others.is_empty() {
                            let clause =
                                format!("SameAs: {}{}", ann_prefix(&ac.ann, pm), others.join(", "));
                            push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                        }
                    } else {
                        // First member anonymous (no frame subject) or empty list.
                        // The Manchester `Individual` rule cannot re-parse an
                        // anonymous individual (`_:id`), so a native `SameIndividual:`
                        // Misc line carrying one would FAIL on read. Keep the
                        // functional `# General axioms` fallback (skip-and-warn).
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::DifferentIndividuals(ax) => {
                    if let Some(crate::model::Individual::Named(ni)) = ax.0.first() {
                        let others: Vec<String> =
                            ax.0.iter()
                                .skip(1)
                                .map(|i| i.as_manchester_with_prefixes(pm).to_string())
                                .collect();
                        if !others.is_empty() {
                            let clause = format!(
                                "DifferentFrom: {}{}",
                                ann_prefix(&ac.ann, pm),
                                others.join(", ")
                            );
                            push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                        }
                    } else {
                        // First member anonymous (no frame subject) or empty list.
                        // The Manchester `Individual` rule cannot re-parse an
                        // anonymous individual (`_:id`), so a native
                        // `DifferentIndividuals:` Misc line carrying one would FAIL
                        // on read. Keep the functional `# General axioms` fallback.
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }

                // ---- Annotation property axioms ----
                Component::SubAnnotationPropertyOf(ax) => {
                    let clause = format!(
                        "SubPropertyOf: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.sup.0.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::AnnotationProperty, ax.sub.0.as_ref(), clause);
                }
                Component::AnnotationPropertyDomain(ax) => {
                    let clause = format!(
                        "Domain: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.iri.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::AnnotationProperty, ax.ap.0.as_ref(), clause);
                }
                Component::AnnotationPropertyRange(ax) => {
                    let clause = format!(
                        "Range: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.iri.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::AnnotationProperty, ax.ap.0.as_ref(), clause);
                }

                // ---- HasKey ----
                Component::HasKey(ax) => {
                    if let crate::model::ClassExpression::Class(c) = &ax.ce {
                        let parts: Vec<String> = ax
                            .vpe
                            .iter()
                            .map(|pe| match pe {
                                crate::model::PropertyExpression::ObjectPropertyExpression(ope) => {
                                    ope.as_manchester_with_prefixes(pm).to_string()
                                }
                                crate::model::PropertyExpression::DataProperty(dp) => {
                                    dp.as_manchester_with_prefixes(pm).to_string()
                                }
                                crate::model::PropertyExpression::AnnotationProperty(ap) => {
                                    ap.as_manchester_with_prefixes(pm).to_string()
                                }
                            })
                            .collect();
                        push_clause!(
                            FrameKind::Class,
                            c.0.as_ref(),
                            format!("HasKey: {}{}", ann_prefix(&ac.ann, pm), parts.join(", "))
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }

                // ---- Datatype definition ----
                Component::DatatypeDefinition(ax) => {
                    let clause = format!(
                        "EquivalentTo: {}{}",
                        ann_prefix(&ac.ann, pm),
                        ax.range.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::Datatype, ax.kind.0.as_ref(), clause);
                }

                // ---- SWRL rules: native `Rule: body -> head` ----
                Component::Rule(rule) => {
                    let atoms = |list: &[Atom<A>]| {
                        list.iter()
                            .map(|a| a.as_manchester_with_prefixes(pm).to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    };
                    misc_axioms.push(format!(
                        "Rule: {}{} -> {}",
                        ann_prefix(&ac.ann, pm),
                        atoms(&rule.body),
                        atoms(&rule.head),
                    ));
                }

                // ---- Misc / fallback ----
                // Anonymous-subject axioms, etc. (no native Manchester form yet).
                _ => {
                    misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                }
            }
        }
    }

    // -----------------------------------------------------------------------
    // 3b. POST-PASS: entity annotations (AnnotationAssertion with named IRI
    //     subject).  Runs AFTER the main loop so every declaration/axiom frame
    //     already exists in `frames`.  A main-loop arm would leak to misc when
    //     the AnnotationAssertion kind is visited before the subject's Declare.
    // -----------------------------------------------------------------------
    for ac in ont
        .i()
        .component_for_kind(ComponentKind::AnnotationAssertion)
    {
        if let Component::AnnotationAssertion(aa) = &ac.component {
            // §2.5 AnnotationTarget admits anon VALUES (`_:label`), so the value
            // is always renderable. An anon SUBJECT, however, is not re-emitted
            // as a frame here (scoped follow-up) → route to misc.
            if let AnnotationSubject::IRI(subj_iri) = &aa.subject {
                let clause = format!(
                    "Annotations: {}",
                    as_manchester::annotation_to_manchester(&aa.ann, mapping)
                );
                // Attach to the existing frame whose subject_iri matches.
                if let Some(frame) = frames
                    .values_mut()
                    .find(|fr| fr.subject_iri == subj_iri.as_ref())
                {
                    frame.clauses.push(clause);
                } else {
                    // Orphan: no frame heads this IRI → not Manchester-expressible.
                    misc.push(
                        ac.component
                            .as_manchester_with_prefixes(mapping)
                            .to_string(),
                    );
                }
            } else {
                // AnonymousIndividual subject → misc (anon-subject emission is a
                // documented follow-up; the reader accepts anon subjects).
                misc.push(
                    ac.component
                        .as_manchester_with_prefixes(mapping)
                        .to_string(),
                );
            }
        }
    }

    // -----------------------------------------------------------------------
    // 4. Emit frames, sorted by (FrameKind, subject_iri).
    // -----------------------------------------------------------------------
    let frame_keyword = |fk: &FrameKind| match fk {
        FrameKind::Class => "Class",
        FrameKind::ObjectProperty => "ObjectProperty",
        FrameKind::DataProperty => "DataProperty",
        FrameKind::AnnotationProperty => "AnnotationProperty",
        FrameKind::Individual => "Individual",
        FrameKind::Datatype => "Datatype",
    };

    for ((_fk, _iri), frame) in &frames {
        // Render the subject IRI with prefix abbreviation.
        // Anonymous-individual keys start with `_:` and must be emitted verbatim
        // (`_:label`), not run through `shrink_iri` (which would produce `<_:label>`
        // — a named individual, wrong type on re-read).
        let subject_display = {
            let iri_str: &str = &frame.subject_iri;
            if iri_str.starts_with("_:") {
                // Anonymous-individual keys must be emitted verbatim; running
                // them through render_iri_to_string would produce `<_:label>`
                // (a named individual), which is the wrong type on re-read.
                iri_str.to_string()
            } else {
                // Delegate to the canonical IRI renderer: only abbreviates when
                // the local name is a valid Manchester PnLocal-ish name, else
                // emits the full `<iri>` form.  This is the same check used by
                // the clause-operand path (write_iri / render_iri_to_string), so
                // frame subjects and clause operands now behave identically.
                as_manchester::render_iri_to_string(iri_str, Some(mapping))
            }
        };

        writeln!(write)?;
        writeln!(write, "{}: {subject_display}", frame_keyword(&frame.kind))?;
        // Emit standalone entity `Annotations:` clauses FIRST, before the logical
        // clauses — matching OWL-API's canonical frame layout. OWL-API's Manchester
        // parser desyncs when a logical clause whose value ends in an ObjectOneOf
        // `{…}` is immediately followed by an `Annotations:` clause; emitting the
        // annotations first avoids that adjacency. (Axiom-annotation clauses begin
        // with their logical keyword, e.g. `SubClassOf: Annotations: …`, so the
        // `starts_with("Annotations:")` test selects only entity annotations.)
        for clause in &frame.clauses {
            if clause.starts_with("Annotations:") {
                writeln!(write, "    {clause}")?;
            }
        }
        for clause in &frame.clauses {
            if !clause.starts_with("Annotations:") {
                writeln!(write, "    {clause}")?;
            }
        }
    }

    // -----------------------------------------------------------------------
    // 4b-pre. Emit complex-LHS GCI frames (SubClassOf with complex sub).
    //     Each entry is a fully-rendered `\nClass: <expr>\n    SubClassOf: <sup>`
    //     block collected above.  Must precede `# General axioms` (which
    //     GeneralAxiomBlock swallows to EOF) and also precede the `Misc:` block
    //     for the same reason.
    // -----------------------------------------------------------------------
    for block in &complex_gci_frames {
        writeln!(write, "{block}")?;
    }

    // -----------------------------------------------------------------------
    // 4b. Emit native Manchester top-level `Misc` axioms (§2.5) — DisjointClasses:
    //     / EquivalentClasses: / EquivalentProperties: / DisjointProperties: /
    //     SameIndividual: / DifferentIndividuals:.  These are valid Manchester and
    //     MUST precede the `# General axioms` marker (GeneralAxiomBlock swallows
    //     everything to EOF, so a Misc line after it would be silently eaten on
    //     read).
    // -----------------------------------------------------------------------
    if !misc_axioms.is_empty() {
        writeln!(write)?;
        for line in &misc_axioms {
            writeln!(write, "{line}")?;
        }
    }

    // -----------------------------------------------------------------------
    // 5. Emit misc / general axioms.
    //    Lines here may be in OWL functional syntax (see `as_manchester.rs`
    //    Component impl) for genuinely-inexpressible components (general
    //    anonymous-subject class axioms, SWRL rules) — they are NOT valid
    //    Manchester syntax.
    // -----------------------------------------------------------------------
    if !misc.is_empty() {
        writeln!(write)?;
        // # functional-syntax fallback: some lines below use OWL functional
        // syntax (not Manchester) for Component variants lacking a native form.
        writeln!(write, "# General axioms")?;
        for line in &misc {
            writeln!(write, "{line}")?;
        }
    }

    Ok(write)
}

// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::*;
    use crate::ontology::component_mapped::ComponentMappedOntology;
    use crate::ontology::set::SetOntology;
    use test_generator::test_resources;

    type TestOnt = ComponentMappedOntology<
        std::rc::Rc<str>,
        std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
    >;

    fn into_amo(o: SetOntology<std::rc::Rc<str>>) -> TestOnt {
        o.into()
    }

    // Conventional read -> write -> read round-trip over a corpus of OWL-API /
    // Tawny-OWL generated Manchester fixtures (matching the `roundtrip_resource`
    // tests in the ofn / owx / rdf writers): the re-parsed ontology and prefix
    // mapping must equal the originals.
    #[test_resources("src/ont/owl-manchester/*.omn")]
    fn roundtrip_resource(resource: &str) {
        let reader = std::fs::File::open(resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::omn::reader::read(reader, Default::default()).unwrap();

        let mut writer = Vec::new();
        crate::io::omn::write(&mut writer, &ont, Some(&prefixes)).unwrap();

        let (ont2, prefixes2): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::omn::reader::read(std::io::Cursor::new(&writer), Default::default()).unwrap();

        assert_eq!(prefixes, prefixes2, "prefix mapping differ");
        assert_eq!(ont, ont2, "ontologies differ");
    }

    // Constructs that PARSE but do not yet round-trip losslessly: SWRL rules are
    // emitted via the functional `# General axioms` fallback (no native `Rule:`
    // output), and inverse-headed property frames, annotated declarations and
    // anonymous annotation values are not yet re-emitted in native Manchester.
    // We assert only that the writer's output re-parses without error
    // (parse-stability), pinning these as writer follow-ups rather than reader
    // gaps. (Mirrors owx's `roundtrip_nonround_resource`.)
    #[test_resources("src/ont/owl-manchester/nonround/*.omn")]
    fn roundtrip_nonround_resource(resource: &str) {
        let reader = std::fs::File::open(resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::omn::reader::read(reader, Default::default()).unwrap();

        let mut writer = Vec::new();
        crate::io::omn::write(&mut writer, &ont, Some(&prefixes)).unwrap();

        let _: (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::omn::reader::read(std::io::Cursor::new(&writer), Default::default())
                .expect("writer output must re-parse without error");
    }

    #[test]
    fn misc_axioms_precede_general_axioms_block() {
        // A complex-member DisjointClasses → native `DisjointClasses:` Misc line.
        // A complex-LHS SubClassOf → `Class: <expr>` frame (FIX-7; no longer
        // goes to `# General axioms`).
        // Verify: DisjointClasses: Misc line appears BEFORE any `Class:` frame
        // for the complex GCI subject.
        let b = Build::new_rc();
        let some = |r: &str, c: &str| ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property(r)),
            bce: Box::new(ClassExpression::Class(b.class(c))),
        };
        let mut o = SetOntology::new_rc();
        o.insert(DisjointClasses(vec![
            some("http://t/r", "http://t/A"),
            some("http://t/s", "http://t/B"),
        ]));
        // complex-LHS SubClassOf → `Class: <expr>` frame after FIX-7
        o.insert(SubClassOf {
            sub: some("http://t/r", "http://t/A"),
            sup: ClassExpression::Class(b.class("http://t/C")),
        });
        let amo = into_amo(o);
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, None).unwrap();
        let s = String::from_utf8(out).unwrap();
        // After FIX-7 the SubClassOf no longer produces a `# General axioms` block.
        assert!(
            !s.contains("# General axioms"),
            "complex-LHS SubClassOf must no longer go to # General axioms, got:\n{s}"
        );
        // The DisjointClasses Misc line is still emitted natively.
        assert!(
            s.contains("DisjointClasses:"),
            "native DisjointClasses: Misc line must still be present, got:\n{s}"
        );
        // The complex SubClassOf is emitted as a `Class: <expr>` frame.
        assert!(
            s.contains("SubClassOf:"),
            "complex-LHS SubClassOf must appear as a SubClassOf: clause in a Class: frame, got:\n{s}"
        );
    }

    /// FIX-7: SubClassOf with a complex `sub` is emitted as a `Class: <expr>`
    /// frame and round-trips correctly (read → write → read = same components).
    #[test]
    fn complex_lhs_subclassof_emits_class_frame_and_roundtrips() {
        use crate::io::omn::read_with_build;
        use std::io::BufReader;

        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("", "http://e/").unwrap();
        let r_some_c = ClassExpression::ObjectSomeValuesFrom {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://e/r")),
            bce: Box::new(ClassExpression::Class(b.class("http://e/C"))),
        };
        let mut o = SetOntology::new_rc();
        let ax = SubClassOf {
            sub: r_some_c,
            sup: ClassExpression::Class(b.class("http://e/D")),
        };
        o.insert(ax);
        let amo: TestOnt = o.clone().into();
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(out.clone()).unwrap();

        // Must NOT fall to `# General axioms`.
        assert!(
            !s.contains("# General axioms"),
            "complex-LHS SubClassOf must not go to # General axioms, got:\n{s}"
        );
        // Must emit a `Class: ` frame whose subject is the rendered complex expr.
        assert!(s.contains("Class: "), "expected a Class: frame, got:\n{s}");
        // The `SubClassOf:` clause must appear inside it.
        assert!(
            s.contains("SubClassOf:"),
            "expected a SubClassOf: clause, got:\n{s}"
        );
        // The subject line must contain the complex expression, not an IRI.
        // Expected: `Class: r some C` (using prefix abbreviation).
        assert!(
            s.lines()
                .any(|l| l.starts_with("Class: ") && l.contains("some")),
            "expected 'Class: ... some ...' subject line, got:\n{s}"
        );

        // Round-trip: read → write → read must yield component-equal result.
        let (ont2, pm2): (crate::ontology::set::SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&out[..]), &b)
                .unwrap_or_else(|e| panic!("round-trip re-parse failed: {e}\n---\n{s}"));
        let mut out2 = Vec::<u8>::new();
        let amo2: TestOnt = ont2.into();
        write(&mut out2, &amo2, Some(&pm2)).unwrap();
        let (ont3, _): (crate::ontology::set::SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&out2[..]), &b)
                .unwrap_or_else(|e| panic!("second round-trip re-parse failed: {e}"));

        // Component sets must be equal after one round-trip (write → read).
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            amo2.i().iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "round-trip mismatch:\n---written---\n{s}");

        // And stable after a second round-trip.
        let got2: std::collections::BTreeSet<_> =
            ont3.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig, got2,
            "second round-trip mismatch:\n---written---\n{s}"
        );

        // Named-subject regression: a normal SubClassOf(named, named) must still
        // emit as a Class: <subject> frame clause (not a complex GCI frame).
        let mut o_named = SetOntology::new_rc();
        o_named.insert(SubClassOf {
            sub: ClassExpression::Class(b.class("http://e/A")),
            sup: ClassExpression::Class(b.class("http://e/B")),
        });
        let amo_named: TestOnt = o_named.into();
        let mut out_named = Vec::<u8>::new();
        write(&mut out_named, &amo_named, Some(&pm)).unwrap();
        let s_named = String::from_utf8(out_named).unwrap();
        assert!(
            s_named.lines().any(|l| l.starts_with("Class: A")),
            "named-subject SubClassOf must still emit as 'Class: A' frame, got:\n{s_named}"
        );
    }

    #[test]
    fn writes_grouped_frames() {
        let b = Build::new_rc();
        let mut o = SetOntology::new_rc();
        o.insert(DeclareClass(b.class("http://t/A")));
        o.insert(SubClassOf {
            sub: ClassExpression::Class(b.class("http://t/A")),
            sup: ClassExpression::Class(b.class("http://t/B")),
        });
        let amo = into_amo(o);
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, None).unwrap();
        let s = String::from_utf8(out).unwrap();
        assert!(s.contains("Class:"), "got:\n{s}");
        assert!(s.contains("SubClassOf:"), "got:\n{s}");
        assert!(s.contains("http://t/A"), "got:\n{s}");
        assert!(s.contains("http://t/B"), "got:\n{s}");
    }

    #[test]
    fn writes_object_property_frame() {
        let b = Build::new_rc();
        let mut o = SetOntology::new_rc();
        o.insert(DeclareObjectProperty(b.object_property("http://t/r")));
        o.insert(ObjectPropertyDomain {
            ope: ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r")),
            ce: ClassExpression::Class(b.class("http://t/A")),
        });
        let amo = into_amo(o);
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, None).unwrap();
        let s = String::from_utf8(out).unwrap();
        assert!(s.contains("ObjectProperty:"), "got:\n{s}");
        assert!(s.contains("Domain:"), "got:\n{s}");
        assert!(s.contains("http://t/r"), "got:\n{s}");
        assert!(s.contains("http://t/A"), "got:\n{s}");
    }

    #[test]
    fn writes_individual_frame() {
        let b = Build::new_rc();
        let mut o = SetOntology::new_rc();
        o.insert(DeclareNamedIndividual(b.named_individual("http://t/a")));
        o.insert(ClassAssertion {
            i: Individual::Named(b.named_individual("http://t/a")),
            ce: ClassExpression::Class(b.class("http://t/A")),
        });
        let amo = into_amo(o);
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, None).unwrap();
        let s = String::from_utf8(out).unwrap();
        assert!(s.contains("Individual:"), "got:\n{s}");
        assert!(s.contains("Types:"), "got:\n{s}");
        assert!(s.contains("http://t/a"), "got:\n{s}");
        assert!(s.contains("http://t/A"), "got:\n{s}");
    }

    #[test]
    fn writes_prefix_declarations() {
        let b = Build::new_rc();
        let mut o = SetOntology::new_rc();
        o.insert(DeclareClass(b.class("http://t/A")));
        let amo = into_amo(o);
        let mut pm = PrefixMapping::default();
        pm.add_prefix("", "http://t/").unwrap();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(out).unwrap();
        assert!(s.contains("Prefix:"), "got:\n{s}");
        assert!(s.contains("xsd:"), "got:\n{s}");
        // With default prefix, class A should be abbreviated as bare local name
        assert!(s.contains("Class: A"), "got:\n{s}");
    }

    /// Frame subjects whose default-prefix namespace lacks a name separator (no
    /// trailing `#` or `/`) must be emitted as full `<IRI>` rather than an
    /// invalid local such as `#Animal`.  That local is not a valid Manchester
    /// PnLocal and makes the writer's own output unparseable.
    ///
    /// Regression guard: when the namespace DOES end with a separator, the
    /// frame subject is still abbreviated to the bare local name.
    #[test]
    fn frame_subject_no_separator_namespace_emits_full_iri() {
        let b = Build::new_rc();
        // Default namespace WITHOUT a trailing separator — mimics koala.owl.
        let mut pm = PrefixMapping::default();
        pm.add_prefix("", "http://e/onto").unwrap(); // no trailing '#' or '/'
        let mut o = SetOntology::new_rc();
        // Class IRI = "http://e/onto#A" — local part is "#A" (starts with '#')
        o.insert(DeclareClass(b.class("http://e/onto#A")));
        o.insert(SubClassOf {
            sub: ClassExpression::Class(b.class("http://e/onto#A")),
            sup: ClassExpression::Class(b.class("http://e/onto#B")),
        });
        let amo = into_amo(o.clone());
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(out.clone()).unwrap();

        // The frame subject must be the full IRI, not the invalid local "#A".
        assert!(
            s.contains("Class: <http://e/onto#A>"),
            "expected full IRI frame subject, got:\n{s}"
        );
        assert!(
            !s.contains("Class: #A"),
            "invalid local '#A' must not appear as a frame subject, got:\n{s}"
        );

        // The writer's output must re-parse without error.
        use crate::io::omn::read_with_build;
        use std::io::BufReader;
        let (parsed, _): (crate::ontology::set::SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&out[..]), &b)
                .unwrap_or_else(|e| panic!("re-parse of writer output failed: {e}\n---\n{s}"));
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(
            orig, got,
            "round-trip mismatch for no-separator namespace\n{s}"
        );
    }

    /// Regression: when the namespace DOES end with a separator, frame subjects
    /// are still abbreviated to the bare local name (e.g. `Class: A`, not `Class: <http://t/A>`).
    #[test]
    fn frame_subject_separator_namespace_still_abbreviates() {
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("", "http://t/").unwrap(); // has trailing '/'
        let mut o = SetOntology::new_rc();
        o.insert(DeclareClass(b.class("http://t/A")));
        let amo = into_amo(o);
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(out).unwrap();
        assert!(
            s.contains("Class: A"),
            "expected abbreviated 'Class: A' for separator-namespace, got:\n{s}"
        );
        assert!(
            !s.contains("Class: <http://t/A>"),
            "should abbreviate when local is valid, got:\n{s}"
        );
    }

    /// An AnnotationAssertion whose annotation VALUE is an AnonymousIndividual
    /// (with a NAMED subject) renders natively as an inline `Annotations: … _:id`
    /// clause under the subject's frame (§2.5 AnnotationTarget admits anon values).
    #[test]
    fn anon_annotation_value_renders_inline_natively() {
        let b = Build::new_rc();
        let mut pm = PrefixMapping::default();
        pm.add_prefix("ex", "http://ex/").unwrap();
        pm.add_prefix("rdfs", "http://www.w3.org/2000/01/rdf-schema#")
            .unwrap();
        let mut o = SetOntology::new_rc();
        // Declare a class so a frame for ex:A exists.
        o.insert(DeclareClass(b.class("http://ex/A")));
        // AnnotationAssertion: ex:A  rdfs:comment  _:anon_x
        let ap = b.annotation_property("http://www.w3.org/2000/01/rdf-schema#comment");
        let anon_val = b.anon("anon_x");
        o.insert(AnnotationAssertion {
            subject: AnnotationSubject::IRI(b.iri("http://ex/A")),
            ann: Annotation {
                ap,
                av: crate::model::AnnotationValue::AnonymousIndividual(anon_val),
                ann: Default::default(),
            },
        });
        let amo = into_amo(o.clone());
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, Some(&pm)).unwrap();
        let s = String::from_utf8(out.clone()).unwrap();

        // The anon value renders natively as an inline `Annotations: … _:anon_x`
        // clause under the ex:A class frame.
        assert!(
            s.lines()
                .any(|l| l.trim().starts_with("Annotations:") && l.contains("_:anon_x")),
            "expected an inline `Annotations: … _:anon_x` clause, got:\n{s}"
        );

        // End-to-end: the reader must consume the natively-rendered anon value.
        use crate::io::omn::read_with_build;
        use std::io::BufReader;
        let (parsed, _): (SetOntology<_>, PrefixMapping) =
            read_with_build(BufReader::new(&out[..]), &b).unwrap();
        let orig: std::collections::BTreeSet<_> = o.iter().map(|ac| ac.component.clone()).collect();
        let got: std::collections::BTreeSet<_> =
            parsed.iter().map(|ac| ac.component.clone()).collect();
        assert_eq!(orig, got, "anon annotation value did not round-trip\n{s}");
    }
}
