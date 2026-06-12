use std::collections::BTreeMap;
use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
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
/// The output is a frame-grouped document: prefix declarations, an optional
/// `Ontology:` header, then one frame per named entity grouping all axioms
/// whose subject is that entity.  Axioms that do not have a clean named-entity
/// subject (n-ary equivalences/disjunctions, property chains, etc.) are
/// emitted as free-standing lines in a trailing `# General axioms` section.
///
/// **Note on the `# General axioms` section:** components that lack a native
/// Manchester rendering (e.g. `Import`, `HasKey`, `OntologyAnnotation`,
/// annotation axioms, SWRL `Rule`) are serialised in **OWL functional syntax**
/// as a stopgap.  Those lines are NOT valid Manchester syntax.  A fully
/// Manchester-conformant document (with `Import:`, `Annotations:`, etc.)
/// awaits native handling of those variants and is a pre-upstream-PR follow-up.
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
    // 1b. Import directives (Manchester: `Import: <iri>`)
    // -----------------------------------------------------------------------
    for ac in ont.i().component_for_kind(ComponentKind::Import) {
        if let Component::Import(imp) = &ac.component {
            writeln!(
                write,
                "Import: {}",
                imp.0.as_manchester_with_prefixes(mapping)
            )?;
        }
    }

    // -----------------------------------------------------------------------
    // 2. Ontology: header
    // -----------------------------------------------------------------------
    {
        let mut id_iter = ont.i().component_for_kind(ComponentKind::OntologyID);
        if let Some(ac) = id_iter.next()
            && let Component::OntologyID(oid) = &ac.component
            && let Some(iri) = &oid.iri
        {
            writeln!(write)?;
            writeln!(
                write,
                "Ontology: {}",
                iri.as_manchester_with_prefixes(mapping)
            )?;
        }
    }

    // -----------------------------------------------------------------------
    // 3. Bucket axioms into frames and a misc list.
    //    Key: (FrameKind, subject_iri_string)
    // -----------------------------------------------------------------------
    let mut frames: BTreeMap<(FrameKind, String), Frame> = BTreeMap::new();
    let mut misc: Vec<String> = Vec::new();

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

    for kind in ComponentKind::all_kinds() {
        if kind == ComponentKind::OntologyID
            || kind == ComponentKind::DocIRI
            || kind == ComponentKind::Import
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
                        let clause =
                            format!("SubClassOf: {}", ax.sup.as_manchester_with_prefixes(pm));
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
                            let clause = format!("EquivalentTo: {}", others.join(", "));
                            push_clause!(FrameKind::Class, c.0.as_ref(), clause);
                        }
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
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
                            let clause = format!("DisjointWith: {}", others.join(", "));
                            push_clause!(FrameKind::Class, c.0.as_ref(), clause);
                        }
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::DisjointUnion(ax) => {
                    let members: Vec<String> =
                        ax.1.iter()
                            .map(|ce| ce.as_manchester_with_prefixes(pm).to_string())
                            .collect();
                    let clause = format!("DisjointUnionOf: {}", members.join(", "));
                    push_clause!(FrameKind::Class, ax.0.0.as_ref(), clause);
                }

                // ---- Object property axioms ----
                Component::SubObjectPropertyOf(ax) => match &ax.sub {
                    SubObjectPropertyExpression::ObjectPropertyExpression(ope) => {
                        if let Some(iri) = ope_iri(ope) {
                            let clause = format!(
                                "SubPropertyOf: {}",
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
                                format!("SubPropertyChain: {rendered}")
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
                                let clause = format!("EquivalentTo: {}", others.join(", "));
                                push_clause!(FrameKind::ObjectProperty, iri, clause);
                            }
                        } else {
                            misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                        }
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
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
                                let clause = format!("DisjointWith: {}", others.join(", "));
                                push_clause!(FrameKind::ObjectProperty, iri, clause);
                            }
                        } else {
                            misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                        }
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::InverseObjectProperties(ax) => {
                    // ax.0 and ax.1 are ObjectProperty (not expression).
                    let clause = format!("InverseOf: {}", ax.1.as_manchester_with_prefixes(pm));
                    push_clause!(FrameKind::ObjectProperty, ax.0.0.as_ref(), clause);
                }
                Component::ObjectPropertyDomain(ax) => {
                    if let Some(iri) = ope_iri(&ax.ope) {
                        let clause = format!("Domain: {}", ax.ce.as_manchester_with_prefixes(pm));
                        push_clause!(FrameKind::ObjectProperty, iri, clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::ObjectPropertyRange(ax) => {
                    if let Some(iri) = ope_iri(&ax.ope) {
                        let clause = format!("Range: {}", ax.ce.as_manchester_with_prefixes(pm));
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
                            "Characteristics: Functional".to_string()
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
                            "Characteristics: InverseFunctional".to_string()
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
                            "Characteristics: Reflexive".to_string()
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
                            "Characteristics: Irreflexive".to_string()
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
                            "Characteristics: Symmetric".to_string()
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
                            "Characteristics: Asymmetric".to_string()
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
                            "Characteristics: Transitive".to_string()
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }

                // ---- Data property axioms ----
                Component::SubDataPropertyOf(ax) => {
                    let clause =
                        format!("SubPropertyOf: {}", ax.sup.as_manchester_with_prefixes(pm));
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
                            let clause = format!("EquivalentTo: {}", others.join(", "));
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
                            let clause = format!("DisjointWith: {}", others.join(", "));
                            push_clause!(FrameKind::DataProperty, first.0.as_ref(), clause);
                        }
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::DataPropertyDomain(ax) => {
                    let clause = format!("Domain: {}", ax.ce.as_manchester_with_prefixes(pm));
                    push_clause!(FrameKind::DataProperty, ax.dp.0.as_ref(), clause);
                }
                Component::DataPropertyRange(ax) => {
                    let clause = format!("Range: {}", ax.dr.as_manchester_with_prefixes(pm));
                    push_clause!(FrameKind::DataProperty, ax.dp.0.as_ref(), clause);
                }
                Component::FunctionalDataProperty(ax) => {
                    push_clause!(
                        FrameKind::DataProperty,
                        ax.0.0.as_ref(),
                        "Characteristics: Functional".to_string()
                    );
                }

                // ---- Assertion axioms ----
                Component::ClassAssertion(ax) => {
                    if let crate::model::Individual::Named(ni) = &ax.i {
                        let clause = format!("Types: {}", ax.ce.as_manchester_with_prefixes(pm));
                        push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }
                Component::ObjectPropertyAssertion(ax) => {
                    if let crate::model::Individual::Named(ni) = &ax.from {
                        let clause = format!(
                            "Facts: {} {}",
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
                            "Facts: not {} {}",
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
                            "Facts: {} {}",
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
                            "Facts: not {} {}",
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
                            let clause = format!("SameAs: {}", others.join(", "));
                            push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                        }
                    } else {
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
                            let clause = format!("DifferentFrom: {}", others.join(", "));
                            push_clause!(FrameKind::Individual, ni.0.as_ref(), clause);
                        }
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }

                // ---- Annotation property axioms ----
                Component::SubAnnotationPropertyOf(ax) => {
                    let clause = format!(
                        "SubPropertyOf: {}",
                        ax.sup.0.as_manchester_with_prefixes(pm)
                    );
                    push_clause!(FrameKind::AnnotationProperty, ax.sub.0.as_ref(), clause);
                }
                Component::AnnotationPropertyDomain(ax) => {
                    let clause = format!("Domain: {}", ax.iri.as_manchester_with_prefixes(pm));
                    push_clause!(FrameKind::AnnotationProperty, ax.ap.0.as_ref(), clause);
                }
                Component::AnnotationPropertyRange(ax) => {
                    let clause = format!("Range: {}", ax.iri.as_manchester_with_prefixes(pm));
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
                            format!("HasKey: {}", parts.join(", "))
                        );
                    } else {
                        misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                    }
                }

                // ---- Misc / fallback ----
                // OntologyAnnotation, DatatypeDefinition,
                // SWRL rules, annotations on axioms, etc.
                _ => {
                    misc.push(ac.component.as_manchester_with_prefixes(pm).to_string());
                }
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
        for clause in &frame.clauses {
            writeln!(write, "    {clause}")?;
        }
    }

    // -----------------------------------------------------------------------
    // 5. Emit misc / general axioms.
    //    Lines here may be in OWL functional syntax (see `as_manchester.rs`
    //    Component impl) for variants with no Manchester form yet — they are
    //    NOT valid Manchester.  Pre-upstream-PR follow-up: native Import: /
    //    Annotations: rendering.
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
}
