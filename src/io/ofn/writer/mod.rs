use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::AnnotationSubject;
use crate::model::AnnotationValue;
use crate::model::ClassExpression;
use crate::model::Component;
use crate::model::ComponentKind;
use crate::model::ForIRI;
use crate::model::Individual;
use crate::model::Literal;
use crate::model::ObjectPropertyExpression;
use crate::model::SubObjectPropertyExpression;
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;

mod as_functional;

pub use self::as_functional::AsFunctional;
pub use self::as_functional::Functional;

const RDFS_LABEL: &str = "http://www.w3.org/2000/01/rdf-schema#label";

/// The entity-type "sections" written by the OWLAPI/ROBOT functional-syntax
/// renderer, in output order. Each tuple is `(section banner, per-entity label)`
/// and its index is the entity's *rank* (used both to group the leading
/// `Declaration(...)` block and to route axioms to their owning entity).
const SECTIONS: [(&str, &str); 6] = [
    ("Classes", "Class"),
    ("Object Properties", "Object Property"),
    ("Data Properties", "Data Property"),
    ("Annotation Properties", "Annotation Property"),
    ("Datatypes", "Datatype"),
    ("Named Individuals", "Individual"),
];

/// Write an Ontology to `write`, using the given `PrefixMapping`.
///
/// The ontology is written in the grouped, commented OWL
/// [Functional-Style](https://www.w3.org/TR/2012/REC-owl2-syntax-20121211/)
/// syntax produced by the OWLAPI (and hence by ROBOT and dosdp-tools): a fixed
/// prefix block, an ontology header with the version IRI and annotations on
/// their own lines, a leading block of every `Declaration(...)`, then a
/// `#   Classes` / `#   Object Properties` / … section for each entity type,
/// each entity introduced by a `# Class: <IRI> (label)` comment followed by its
/// axioms. This makes owlmake output byte-comparable with ROBOT's.
pub fn write<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<W, HornedError> {
    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => &default_mapper,
    };

    // Ensure we have a single OntologyID in the ontology.
    let optional_id = {
        let mut components = ont.i().component_for_kind(ComponentKind::OntologyID);
        let component = components.next();
        if components.next().is_some() {
            return Err(HornedError::invalid("multiple ontology IDs found"));
        }
        component.map(|c| {
            if let Component::OntologyID(ontology_id) = &c.component {
                ontology_id
            } else {
                unreachable!()
            }
        })
    };
    let ont_iri = optional_id
        .and_then(|id| id.iri.as_ref())
        .map(|i| i.as_ref().to_string());
    let version_iri = optional_id
        .and_then(|id| id.viri.as_ref())
        .map(|i| i.as_ref().to_string());

    // --- Prefixes (canonical OWLAPI order: default, owl, rdf, xml, xsd, rdfs) ---
    write_prefixes(&mut write, mapping)?;

    // --- Ontology header ---
    write!(write, "\n\nOntology(")?;
    if let Some(oi) = &ont_iri {
        write!(write, "<{oi}>")?;
    }
    writeln!(write)?;
    if let Some(vi) = &version_iri {
        writeln!(write, "<{vi}>")?;
    }
    // Imports first (functional syntax requires them before axioms), then the
    // ontology annotations, each on its own line.
    {
        let mut imports = ont
            .i()
            .component_for_kind(ComponentKind::Import)
            .map(|c| c.as_functional_with_prefixes(mapping).to_string())
            .collect::<Vec<_>>();
        imports.sort();
        for i in &imports {
            writeln!(write, "{i}")?;
        }
    }
    {
        let mut annos = ont
            .i()
            .component_for_kind(ComponentKind::OntologyAnnotation)
            .map(|c| c.as_functional_with_prefixes(mapping).to_string())
            .collect::<Vec<_>>();
        annos.sort();
        for a in &annos {
            writeln!(write, "{a}")?;
        }
    }
    // Blank line separating the header from the body.
    writeln!(write)?;

    // --- Pass 1: declarations, entity ranks, and rdfs:labels ---
    let mut declarations: Vec<(usize, String, String)> = Vec::new();
    let mut entity_rank: HashMap<String, usize> = HashMap::new();
    let mut labels: HashMap<String, String> = HashMap::new();
    for ac in ont.iter() {
        if let Some((rank, iri)) = declaration_info(&ac.component) {
            entity_rank.insert(iri.clone(), rank);
            declarations.push((
                rank,
                iri,
                ac.as_functional_with_prefixes(mapping).to_string(),
            ));
        } else if let Component::AnnotationAssertion(aa) = &ac.component {
            if aa.ann.ap.0.as_ref() == RDFS_LABEL {
                if let (AnnotationSubject::IRI(subj), AnnotationValue::Literal(lit)) =
                    (&aa.subject, &aa.ann.av)
                {
                    labels
                        .entry(subj.as_ref().to_string())
                        .or_insert_with(|| literal_text(lit));
                }
            }
        }
    }
    declarations.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
    for (_, _, rendered) in &declarations {
        writeln!(write, "{rendered}")?;
    }

    // --- Pass 2: route each non-declaration axiom to its owning entity ---
    // Annotation-assertion blocks are keyed by (rank, entity IRI); logical-axiom
    // blocks likewise. Both are sorted on their rendering before emission.
    let mut ann_blocks: HashMap<(usize, String), Vec<(String, String)>> = HashMap::new();
    let mut axiom_blocks: HashMap<(usize, String), Vec<String>> = HashMap::new();
    let mut leftover: Vec<String> = Vec::new();

    for ac in ont.iter() {
        match &ac.component {
            // Handled in the header / leading block already.
            Component::OntologyID(_)
            | Component::DocIRI(_)
            | Component::Import(_)
            | Component::OntologyAnnotation(_) => {}
            _ if declaration_info(&ac.component).is_some() => {}

            Component::AnnotationAssertion(aa) => {
                let rendered = ac.as_functional_with_prefixes(mapping).to_string();
                if let AnnotationSubject::IRI(subj) = &aa.subject {
                    let subj = subj.as_ref().to_string();
                    if let Some(&rank) = entity_rank.get(&subj) {
                        ann_blocks
                            .entry((rank, subj))
                            .or_default()
                            .push((aa.ann.ap.0.as_ref().to_string(), rendered));
                        continue;
                    }
                }
                leftover.push(rendered);
            }

            other => {
                let rendered = ac.as_functional_with_prefixes(mapping).to_string();
                match axiom_owner(other) {
                    Some(key) => axiom_blocks.entry(key).or_default().push(rendered),
                    None => leftover.push(rendered),
                }
            }
        }
    }

    // --- Emit each non-empty entity section ---
    for (rank, (section, label)) in SECTIONS.iter().enumerate() {
        let mut iris: BTreeSet<&str> = BTreeSet::new();
        for (r, iri) in ann_blocks.keys() {
            if *r == rank {
                iris.insert(iri.as_str());
            }
        }
        for (r, iri) in axiom_blocks.keys() {
            if *r == rank {
                iris.insert(iri.as_str());
            }
        }
        if iris.is_empty() {
            continue;
        }

        write!(
            write,
            "\n\n\n############################\n#   {section}\n############################\n"
        )?;

        for iri in iris {
            write!(write, "\n# {label}: <{iri}>")?;
            if let Some(lbl) = labels.get(iri) {
                write!(write, " ({lbl})")?;
            }
            writeln!(write)?;
            writeln!(write)?;

            let key = (rank, iri.to_string());
            if let Some(anns) = ann_blocks.get(&key) {
                let mut anns = anns.clone();
                anns.sort();
                for (_, rendered) in &anns {
                    writeln!(write, "{rendered}")?;
                }
            }
            if let Some(axs) = axiom_blocks.get(&key) {
                let mut axs = axs.clone();
                axs.sort();
                for rendered in &axs {
                    writeln!(write, "{rendered}")?;
                }
            }
        }
    }

    // --- Anything we could not attribute to an entity (does not happen for OBO
    // pattern files) is written verbatim before the close so no axiom is lost.
    if !leftover.is_empty() {
        leftover.sort();
        writeln!(write)?;
        for rendered in &leftover {
            writeln!(write, "{rendered}")?;
        }
    }

    // Close the ontology (matching OWLAPI's trailing blank lines, no final EOL).
    write!(write, "\n\n)")?;

    Ok(write)
}

/// Emit the `Prefix(...)` block in OWLAPI's canonical order: the default `:`
/// prefix first, then `owl`, `rdf`, `xml`, `xsd`, `rdfs`, then any remaining
/// prefixes sorted by name. Every prefix present in `mapping` is emitted, so
/// round-tripping through the reader preserves the prefix set.
fn write_prefixes<W: Write>(write: &mut W, mapping: &PrefixMapping) -> Result<(), HornedError> {
    const CANONICAL: [&str; 6] = ["", "owl", "rdf", "xml", "xsd", "rdfs"];
    let entries: Vec<(&str, &str)> = mapping
        .mappings()
        .map(|(k, v)| (k.as_str(), v.as_str()))
        .collect();

    for name in CANONICAL {
        if let Some((_, value)) = entries.iter().find(|(k, _)| *k == name) {
            writeln!(write, "Prefix({name}:=<{value}>)")?;
        }
    }
    let mut rest: Vec<(&str, &str)> = entries
        .iter()
        .copied()
        .filter(|(k, _)| !CANONICAL.contains(k))
        .collect();
    rest.sort();
    for (name, value) in rest {
        writeln!(write, "Prefix({name}:=<{value}>)")?;
    }
    Ok(())
}

/// The literal's lexical form (dropping any language tag / datatype).
fn literal_text<A: ForIRI>(lit: &Literal<A>) -> String {
    match lit {
        Literal::Simple { literal }
        | Literal::Language { literal, .. }
        | Literal::Datatype { literal, .. } => literal.clone(),
    }
}

/// If `comp` is an entity declaration, return its `(section rank, IRI)`.
fn declaration_info<A: ForIRI>(comp: &Component<A>) -> Option<(usize, String)> {
    Some(match comp {
        Component::DeclareClass(e) => (0, e.0 .0.as_ref().to_string()),
        Component::DeclareObjectProperty(e) => (1, e.0 .0.as_ref().to_string()),
        Component::DeclareDataProperty(e) => (2, e.0 .0.as_ref().to_string()),
        Component::DeclareAnnotationProperty(e) => (3, e.0 .0.as_ref().to_string()),
        Component::DeclareDatatype(e) => (4, e.0 .0.as_ref().to_string()),
        Component::DeclareNamedIndividual(e) => (5, e.0 .0.as_ref().to_string()),
        _ => return None,
    })
}

fn ce_class<A: ForIRI>(ce: &ClassExpression<A>) -> Option<String> {
    match ce {
        ClassExpression::Class(c) => Some(c.0.as_ref().to_string()),
        _ => None,
    }
}

fn ope_named<A: ForIRI>(ope: &ObjectPropertyExpression<A>) -> Option<String> {
    ope.as_property().map(|p| p.0.as_ref().to_string())
}

fn ind_named<A: ForIRI>(i: &Individual<A>) -> Option<String> {
    match i {
        Individual::Named(n) => Some(n.0.as_ref().to_string()),
        Individual::Anonymous(_) => None,
    }
}

/// The entity that "owns" a logical axiom, as `(section rank, IRI)`, matching
/// how the OWLAPI groups axioms under the entity that is their subject. Returns
/// `None` for axioms with no named subject (they are written verbatim so that
/// nothing is dropped).
fn axiom_owner<A: ForIRI>(comp: &Component<A>) -> Option<(usize, String)> {
    use Component::*;
    match comp {
        // Class axioms (rank 0)
        SubClassOf(ax) => ce_class(&ax.sub).map(|i| (0, i)),
        EquivalentClasses(ax) => ax.0.iter().find_map(ce_class).map(|i| (0, i)),
        DisjointClasses(ax) => ax.0.iter().find_map(ce_class).map(|i| (0, i)),
        DisjointUnion(ax) => Some((0, ax.0 .0.as_ref().to_string())),
        HasKey(ax) => ce_class(&ax.ce).map(|i| (0, i)),

        // Object-property axioms (rank 1)
        SubObjectPropertyOf(ax) => match &ax.sub {
            SubObjectPropertyExpression::ObjectPropertyExpression(ope) => {
                ope_named(ope).map(|i| (1, i))
            }
            SubObjectPropertyExpression::ObjectPropertyChain(_) => None,
        },
        EquivalentObjectProperties(ax) => ax.0.iter().find_map(ope_named).map(|i| (1, i)),
        DisjointObjectProperties(ax) => ax.0.iter().find_map(ope_named).map(|i| (1, i)),
        InverseObjectProperties(ax) => {
            ope_named(&ax.0).or_else(|| ope_named(&ax.1)).map(|i| (1, i))
        }
        ObjectPropertyDomain(ax) => ope_named(&ax.ope).map(|i| (1, i)),
        ObjectPropertyRange(ax) => ope_named(&ax.ope).map(|i| (1, i)),
        FunctionalObjectProperty(ax) => ope_named(&ax.0).map(|i| (1, i)),
        InverseFunctionalObjectProperty(ax) => ope_named(&ax.0).map(|i| (1, i)),
        ReflexiveObjectProperty(ax) => ope_named(&ax.0).map(|i| (1, i)),
        IrreflexiveObjectProperty(ax) => ope_named(&ax.0).map(|i| (1, i)),
        SymmetricObjectProperty(ax) => ope_named(&ax.0).map(|i| (1, i)),
        AsymmetricObjectProperty(ax) => ope_named(&ax.0).map(|i| (1, i)),
        TransitiveObjectProperty(ax) => ope_named(&ax.0).map(|i| (1, i)),

        // Data-property axioms (rank 2)
        SubDataPropertyOf(ax) => Some((2, ax.sub.0.as_ref().to_string())),
        EquivalentDataProperties(ax) => ax.0.first().map(|d| (2, d.0.as_ref().to_string())),
        DisjointDataProperties(ax) => ax.0.first().map(|d| (2, d.0.as_ref().to_string())),
        DataPropertyDomain(ax) => Some((2, ax.dp.0.as_ref().to_string())),
        DataPropertyRange(ax) => Some((2, ax.dp.0.as_ref().to_string())),
        FunctionalDataProperty(ax) => Some((2, ax.0 .0.as_ref().to_string())),

        // Annotation-property axioms (rank 3)
        SubAnnotationPropertyOf(ax) => Some((3, ax.sub.0.as_ref().to_string())),
        AnnotationPropertyDomain(ax) => Some((3, ax.ap.0.as_ref().to_string())),
        AnnotationPropertyRange(ax) => Some((3, ax.ap.0.as_ref().to_string())),

        // Datatype axioms (rank 4)
        DatatypeDefinition(ax) => Some((4, ax.kind.0.as_ref().to_string())),

        // Individual axioms (rank 5)
        SameIndividual(ax) => ax.0.iter().find_map(ind_named).map(|i| (5, i)),
        DifferentIndividuals(ax) => ax.0.iter().find_map(ind_named).map(|i| (5, i)),
        ClassAssertion(ax) => ind_named(&ax.i).map(|i| (5, i)),
        ObjectPropertyAssertion(ax) => ind_named(&ax.from).map(|i| (5, i)),
        NegativeObjectPropertyAssertion(ax) => ind_named(&ax.from).map(|i| (5, i)),
        DataPropertyAssertion(ax) => ind_named(&ax.from).map(|i| (5, i)),
        NegativeDataPropertyAssertion(ax) => ind_named(&ax.from).map(|i| (5, i)),

        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::model::AnnotatedComponent;
    use crate::model::RcStr;

    use pretty_assertions::assert_eq;
    use test_generator::test_resources;

    #[test_resources("src/ont/owl-functional/*.ofn")]
    fn roundtrip_resource(resource: &str) {
        let reader = std::fs::File::open(resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(reader, Default::default()).unwrap();

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, Some(&prefixes)).unwrap();

        let (ont2, prefixes2) =
            crate::io::ofn::reader::read(std::io::Cursor::new(&writer), Default::default())
                .unwrap();

        assert_eq!(prefixes, prefixes2, "prefix mapping differ");
        assert_eq!(ont, ont2, "ontologies differ");
    }
}
