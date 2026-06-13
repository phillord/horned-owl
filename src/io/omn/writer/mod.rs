use std::collections::BTreeMap;
use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::Annotation;
use crate::model::AnnotationSubject;
use crate::model::AnnotationValue;
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

    // -----------------------------------------------------------------------
    // 2. Conformant Ontology: header (IRI + nested Import: + Annotations:)
    //    W3C Manchester puts imports and ontology annotations INSIDE the
    //    Ontology: frame, not at top level.
    //
    //    Guard: OntologyAnnotation whose value is AnonymousIndividual cannot be
    //    rendered in Manchester `Annotations:` syntax (the grammar's
    //    AnnotationTarget = { Literal | IRI }). Route those to misc instead.
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
        // Separate conformant (Literal/IRI) from anon-valued ontology annotations.
        let mut conformant_ont_anns: Vec<Annotation<A>> = Vec::new();
        for ac in ont
            .i()
            .component_for_kind(ComponentKind::OntologyAnnotation)
        {
            if let Component::OntologyAnnotation(oa) = &ac.component {
                if matches!(oa.0.av, AnnotationValue::AnonymousIndividual(_)) {
                    // Anon values are not expressible in Manchester AnnotationTarget.
                    misc.push(
                        ac.component
                            .as_manchester_with_prefixes(mapping)
                            .to_string(),
                    );
                } else {
                    conformant_ont_anns.push(oa.0.clone());
                }
            }
        }
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
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
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
                    if let crate::model::Individual::Named(ni) = &ax.i {
                        let clause = format!(
                            "Types: {}{}",
                            ann_prefix(&ac.ann, pm),
                            ax.ce.as_manchester_with_prefixes(pm)
                        );
                        push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::ObjectPropertyAssertion(ax) => {
                    if let crate::model::Individual::Named(ni) = &ax.from {
                        let clause = format!(
                            "Facts: {}{} {}",
                            ann_prefix(&ac.ann, pm),
                            ax.ope.as_manchester_with_prefixes(pm),
                            ax.to.as_manchester_with_prefixes(pm)
                        );
                        push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::NegativeObjectPropertyAssertion(ax) => {
                    if let crate::model::Individual::Named(ni) = &ax.from {
                        let clause = format!(
                            "Facts: {}not {} {}",
                            ann_prefix(&ac.ann, pm),
                            ax.ope.as_manchester_with_prefixes(pm),
                            ax.to.as_manchester_with_prefixes(pm)
                        );
                        push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::DataPropertyAssertion(ax) => {
                    if let crate::model::Individual::Named(ni) = &ax.from {
                        let clause = format!(
                            "Facts: {}{} {}",
                            ann_prefix(&ac.ann, pm),
                            ax.dp.as_manchester_with_prefixes(pm),
                            ax.to.as_manchester_with_prefixes(pm)
                        );
                        push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::NegativeDataPropertyAssertion(ax) => {
                    if let crate::model::Individual::Named(ni) = &ax.from {
                        let clause = format!(
                            "Facts: {}not {} {}",
                            ann_prefix(&ac.ann, pm),
                            ax.dp.as_manchester_with_prefixes(pm),
                            ax.to.as_manchester_with_prefixes(pm)
                        );
                        push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
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

                // ---- Misc / fallback ----
                // SWRL rules, anonymous-subject axioms, etc.
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
            // Guard: the Manchester grammar's AnnotationTarget = { Literal | IRI }
            // cannot represent an AnonymousIndividual annotation VALUE.  Route the
            // whole component to misc rather than emitting an unparseable `_:id` clause.
            let anon_value = matches!(aa.ann.av, AnnotationValue::AnonymousIndividual(_));
            if let AnnotationSubject::IRI(subj_iri) = &aa.subject {
                if anon_value {
                    // Anon value → not expressible in Manchester; route to misc.
                    misc.push(
                        ac.component
                            .as_manchester_with_prefixes(mapping)
                            .to_string(),
                    );
                } else {
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
                }
            } else {
                // AnonymousIndividual subject → misc.
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
        let subject_display = {
            let iri_str: &str = &frame.subject_iri;
            if let Ok(curie) = mapping.shrink_iri(iri_str) {
                let s = curie.to_string();
                if let Some(local) = s.strip_prefix(':') {
                    local.to_string()
                } else {
                    s
                }
            } else {
                format!("<{}>", frame.subject_iri)
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

    type TestOnt = ComponentMappedOntology<
        std::rc::Rc<str>,
        std::rc::Rc<AnnotatedComponent<std::rc::Rc<str>>>,
    >;

    fn into_amo(o: SetOntology<std::rc::Rc<str>>) -> TestOnt {
        o.into()
    }

    #[test]
    fn misc_axioms_precede_general_axioms_block() {
        // A complex-member DisjointClasses → native `DisjointClasses:` Misc line.
        // A complex-LHS SubClassOf → functional `# General axioms` fallback.
        // The Misc line MUST appear BEFORE the `# General axioms` marker (else the
        // GeneralAxiomBlock rule swallows it on re-read).
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
        // complex-LHS SubClassOf → functional fallback block
        o.insert(SubClassOf {
            sub: some("http://t/r", "http://t/A"),
            sup: ClassExpression::Class(b.class("http://t/C")),
        });
        let amo = into_amo(o);
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, None).unwrap();
        let s = String::from_utf8(out).unwrap();
        let disjoint_pos = s
            .find("DisjointClasses:")
            .expect("native Misc line present");
        let general_pos = s.find("# General axioms").expect("fallback block present");
        assert!(
            disjoint_pos < general_pos,
            "DisjointClasses: Misc line must precede `# General axioms`, got:\n{s}"
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

    /// An AnnotationAssertion whose annotation VALUE is an AnonymousIndividual
    /// MUST NOT produce an inline `Annotations: … _:id` clause (which the reader
    /// grammar cannot parse).  The component must instead appear in the
    /// `# General axioms` misc section.
    #[test]
    fn anon_annotation_value_routes_to_misc_not_inline() {
        let b = Build::new_rc();
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
                av: AnnotationValue::AnonymousIndividual(anon_val),
            },
        });
        let amo = into_amo(o);
        let mut out = Vec::<u8>::new();
        write(&mut out, &amo, None).unwrap();
        let s = String::from_utf8(out).unwrap();

        // The anon value must NOT appear as an inline Annotations: clause.
        // Specifically, no line should be both an Annotations: clause AND contain `_:`.
        for line in s.lines() {
            let trimmed = line.trim();
            if trimmed.starts_with("Annotations:") {
                assert!(
                    !trimmed.contains("_:"),
                    "found inline `_:` in an Annotations: clause — should have been routed to misc.\nFull output:\n{s}"
                );
            }
        }

        // The component must appear in the # General axioms misc section.
        assert!(
            s.contains("# General axioms"),
            "expected misc section for anon-value annotation, got:\n{s}"
        );
        assert!(
            s.contains("anon_x"),
            "expected anon individual id in misc section, got:\n{s}"
        );
    }
}
