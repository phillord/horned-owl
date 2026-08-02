use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::AnnotatedComponent;
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
    write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<W, HornedError> {
    write_with_labels(write, ont, mapping, None, None)
}

/// Like [`write`], but with two extras that let a caller reproduce ROBOT's output
/// for an import-bearing edit file without merging the closure:
///
/// * `extra_labels` — an external `entity IRI → label` map consulted for the
///   `# Class: … (label)` banner comments when the ontology itself carries no
///   `rdfs:label` for an entity (OWLAPI resolves banner labels across the whole
///   closure while serialising only the root).
/// * `import_order` — the import IRIs in the order they should be written. The
///   in-memory ontology is an unordered set, so it cannot preserve the document's
///   import order on its own; a caller that knows it (e.g. from the source file)
///   passes it here. Imports absent from the list keep their default (sorted)
///   order after the listed ones.
pub fn write_with_labels<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
    extra_labels: Option<&HashMap<String, String>>,
    import_order: Option<&[String]>,
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
        // The ontology is an unordered set, so `component_for_kind` yields imports
        // in IRI order. When the caller supplies the document's `import_order`,
        // reorder to match it (ROBOT preserves the source order); otherwise keep
        // the default order.
        let mut imports: Vec<(String, String)> = ont
            .i()
            .component_for_kind(ComponentKind::Import)
            .filter_map(|c| match &c.component {
                Component::Import(imp) => {
                    Some((imp.0.as_ref().to_string(), c.as_functional_with_prefixes(mapping).to_string()))
                }
                _ => None,
            })
            .collect();
        if let Some(order) = import_order {
            imports.sort_by_key(|(iri, _)| order.iter().position(|x| x == iri).unwrap_or(usize::MAX));
        }
        for (_, rendered) in &imports {
            writeln!(write, "{rendered}")?;
        }
    }
    {
        // OWLAPI writes the ontology annotations in `compareTo` order (by full
        // property IRI, then value) — NOT by rendered CURIE, so e.g. `obo:` (which
        // expands to purl.obolibrary.org) sorts before `dc:` (purl.org). Sorting
        // the components by their natural `Ord` reproduces that.
        let mut annos: Vec<&AnnotatedComponent<A>> = ont
            .i()
            .component_for_kind(ComponentKind::OntologyAnnotation)
            .collect();
        annos.sort_by(owlapi_ont_annotation_cmp);
        for a in &annos {
            writeln!(write, "{}", a.as_functional_with_prefixes(mapping))?;
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
                    // LAST label wins, as OWLAPI's short-form provider builds
                    // its map by overwriting. GO_0051705 carries both
                    // "multi-organism behavior" and "obsolete multi-organism
                    // behavior", and ROBOT's banner shows the latter.
                    labels.insert(subj.as_ref().to_string(), literal_text(lit));
                }
            }
        }
    }
    // OWLAPI orders entities by `IRI.compareTo` — NAMESPACE then remainder, not
    // the whole string. `…/obo/MF#manifestationOf` has namespace `…/obo/MF#`,
    // which sorts after the plain `…/obo/` shared by every `RO_…`/`GO_…`; a
    // whole-string compare put it before them.
    declarations.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| owlapi_iri_cmp(&a.1, &b.1)));
    for (_, _, rendered) in &declarations {
        writeln!(write, "{rendered}")?;
    }

    // Which entity-type sections have a non-empty *signature*. OWLAPI's
    // `writeSortedEntities` emits a trailing blank line for every type whose
    // signature is non-empty — even one with no banner (no entity carrying
    // axioms), e.g. datatypes that appear only inside typed literals. Ranks:
    // Class=0, OP=1, DataProp=2, AP=3, Datatype=4, Individual=5.
    let mut sig_nonempty = [false; 6];
    for &rank in entity_rank.values() {
        sig_nonempty[rank] = true;
    }
    if !sig_nonempty[4] {
        // A typed literal anywhere puts its datatype (≥ xsd:string) in the
        // signature, so the Datatypes section is non-empty even without a
        // datatype declaration.
        for ac in ont.iter() {
            if let Component::AnnotationAssertion(aa) = &ac.component {
                if matches!(aa.ann.av, AnnotationValue::Literal(_)) {
                    sig_nonempty[4] = true;
                    break;
                }
            }
        }
    }

    // --- Pass 2: route each non-declaration axiom to its owning entity ---
    // Annotation-assertion blocks are keyed by (rank, entity IRI); logical-axiom
    // blocks likewise. Both are sorted on their rendering before emission.
    let mut ann_blocks: HashMap<(usize, String), Vec<&AnnotatedComponent<A>>> = HashMap::new();
    let mut axiom_blocks: HashMap<(usize, String), Vec<&AnnotatedComponent<A>>> = HashMap::new();
    let mut leftover: Vec<&AnnotatedComponent<A>> = Vec::new();

    for ac in ont.iter() {
        match &ac.component {
            // Handled in the header / leading block already.
            Component::OntologyID(_)
            | Component::DocIRI(_)
            | Component::Import(_)
            | Component::OntologyAnnotation(_) => {}
            _ if declaration_info(&ac.component).is_some() => {}

            Component::AnnotationAssertion(aa) => {
                if let AnnotationSubject::IRI(subj) = &aa.subject {
                    let subj = subj.as_ref().to_string();
                    if let Some(&rank) = entity_rank.get(&subj) {
                        ann_blocks.entry((rank, subj)).or_default().push(ac);
                        continue;
                    }
                }
                leftover.push(ac);
            }

            // OWLAPI writes n-ary DisjointClasses (>2 operands) and
            // DifferentIndividuals as general axioms at the end, not under an
            // entity (writeEntity2 skips them).
            Component::DisjointClasses(d) if d.0.len() > 2 => leftover.push(ac),
            Component::DifferentIndividuals(_) => leftover.push(ac),

            other => match axiom_owner(other) {
                // Store the component itself, not its rendering, so the block can
                // be ordered by OWLAPI's structural axiom order (below) rather than
                // lexically by rendered string.
                Some(key) => axiom_blocks.entry(key).or_default().push(ac),
                None => leftover.push(ac),
            },
        }
    }

    // Any entity that carries axioms is in the signature too, even without its
    // own declaration — so its section must not be skipped (which would drop the
    // axioms). Mark those ranks non-empty now that the blocks are built.
    for (r, _) in ann_blocks.keys().chain(axiom_blocks.keys()) {
        sig_nonempty[*r] = true;
    }

    // --- Emit each non-empty entity section ---
    // OWLAPI's FunctionalSyntaxObjectRenderer writes the axiom sections in the
    // order Annotation Properties, Object Properties, Data Properties, Datatypes,
    // Classes, Named Individuals — NOT the rank order used for the leading
    // Declaration block (Classes first). `SECTION_EMIT_ORDER` maps emission
    // position → section rank (Class=0, OP=1, DataProp=2, AP=3, Datatype=4, Ind=5).
    const SECTION_EMIT_ORDER: [usize; 6] = [3, 1, 2, 4, 0, 5];
    for &rank in SECTION_EMIT_ORDER.iter() {
        // OWLAPI's `writeSortedEntities` does nothing for a type with an empty
        // signature, and emits a trailing blank line for one that is non-empty.
        if !sig_nonempty[rank] {
            continue;
        }
        let (section, label) = SECTIONS[rank];
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

        // The banner + entities are written only when some entity of this type
        // carries axioms; a signature-only type (e.g. Datatypes) emits no banner.
        if !iris.is_empty() {
            // Banner with a single trailing blank line, no leading blanks.
            write!(
                write,
                "############################\n#   {section}\n############################\n\n"
            )?;

            for iri in iris {
                // OWLAPI banner: `# Class: <curie> (<label-or-curie>)`, then a blank.
                let short = short_form(mapping, iri);
                let display = labels
                    .get(iri)
                    .or_else(|| extra_labels.and_then(|m| m.get(iri)))
                    .cloned()
                    .unwrap_or_else(|| short.clone());
                writeln!(write, "# {label}: {short} ({display})")?;
                writeln!(write)?;

                let key = (rank, iri.to_string());
                if let Some(anns) = ann_blocks.get(&key) {
                    // OWLAPI writes an entity's annotation assertions before its
                    // logical axioms, sorted by `compareTo` (property, then value,
                    // then the assertion's own annotations).
                    let mut anns = anns.clone();
                    anns.sort_by(owlapi_axiom_cmp);
                    for ac in &anns {
                        let rendered = ac.as_functional_with_prefixes(mapping).to_string();
                        writeln!(write, "{rendered}")?;
                    }
                }
                if let Some(axs) = axiom_blocks.get(&key) {
                    // OWLAPI orders an entity's axioms by axiom-type index, then
                    // structurally (a named superclass before an anonymous
                    // restriction, etc.) — NOT lexically.
                    let mut axs = axs.clone();
                    axs.sort_by(owlapi_axiom_cmp);
                    for ac in &axs {
                        let rendered = ac.as_functional_with_prefixes(mapping).to_string();
                        writeln!(write, "{rendered}")?;
                    }
                }
                // Trailing blank line after every entity.
                writeln!(write)?;
            }
        }
        // `writeSortedEntities` trailing blank line (for every non-empty-signature
        // type, whether or not it produced a banner).
        writeln!(write)?;
    }

    // --- Remaining axioms: general class axioms (GCIs), n-ary DisjointClasses and
    //     DifferentIndividuals — everything not attributed to an entity — sorted
    //     structurally, then the closing bracket immediately (no trailing blank). ---
    leftover.sort_by(owlapi_general_cmp);
    for ac in &leftover {
        let rendered = ac.as_functional_with_prefixes(mapping).to_string();
        writeln!(write, "{rendered}")?;
    }

    write!(write, ")")?;

    Ok(write)
}

/// Emit the `Prefix(...)` block in the mapping's own order. The reader records
/// prefixes in document order (`curie::PrefixMapping` is insertion-ordered), and
/// OWLAPI/ROBOT preserve that order on a convert round-trip, so emitting the
/// mapping verbatim reproduces the source document's prefix block.
fn write_prefixes<W: Write>(write: &mut W, mapping: &PrefixMapping) -> Result<(), HornedError> {
    for (name, value) in mapping.mappings() {
        writeln!(write, "Prefix({name}:=<{value}>)")?;
    }
    Ok(())
}

/// Abbreviate `iri` to `(prefix, local)` using the LONGEST declared namespace
/// that is a prefix of `iri` and leaves a valid CURIE local part. This is OWLAPI
/// semantics — the most specific prefix wins, independent of declaration order
/// (`curie::shrink_iri` returns the *first* declared match, which is not the same
/// thing). The empty-string prefix renders the default `:local`. Returns `None`
/// when no declared prefix yields a valid CURIE, so the caller writes `<IRI>`.
pub(crate) fn shrink_valid<'a>(mapping: &'a PrefixMapping, iri: &'a str) -> Option<(&'a str, &'a str)> {
    let mut best: Option<(&str, &str)> = None;
    for (prefix, ns) in mapping.mappings() {
        if let Some(local) = iri.strip_prefix(ns.as_str()) {
            if is_valid_curie_local(local)
                && best.map_or(true, |(_, blocal)| local.len() < blocal.len())
            {
                best = Some((prefix.as_str(), local));
            }
        }
    }
    best
}

/// The banner/short form of `iri`: its CURIE if one is available, else the full
/// IRI (no angle brackets), matching OWLAPI's `# Class: obo:CL_0000000` headers.
pub(crate) fn short_form(mapping: &PrefixMapping, iri: &str) -> String {
    match shrink_valid(mapping, iri) {
        Some((prefix, local)) => format!("{prefix}:{local}"),
        // OWLAPI's banner renders the IRI exactly as the body does, and a full IRI
        // there is angle-bracketed. A document whose format carries no prefixes —
        // anything ROBOT built with `query --update` — has a full IRI in EVERY
        // banner, e.g. `# Class: <http://…> (label)`.
        None => format!("<{iri}>"),
    }
}

/// Whether `local` is a legal CURIE local part (PNAME_LN, conservatively): no
/// characters that would break re-parsing and no leading `-`/`.`.
///
/// OWLAPI decides this with `XMLUtils.getNCNameSuffix`: an IRI is abbreviated only
/// when its tail is a valid NCName, so anything carrying a delimiter — e.g. ENVO's
/// `http://en.wikipedia.org/wiki/Front_(oceanography)` — is written out in full
/// even with `wikipedia:` declared. Abbreviating it here produced
/// `wikipedia:Front_(oceanography)`, whose `(` closes the enclosing
/// `AnnotationAssertion(` and leaves a document no functional-syntax parser can
/// read back.
pub(crate) fn is_valid_curie_local(local: &str) -> bool {
    !local.is_empty()
        && !local.contains(['/', '#', ' ', ':'])
        && !local.contains(|c: char| c.is_whitespace() || NON_NCNAME.contains(&c))
        && !local.starts_with('-')
        && !local.starts_with('.')
}

/// Delimiters and punctuation that are not NCName characters, so OWLAPI never
/// leaves one inside an abbreviated IRI. `%` is included: it is legal in a SPARQL
/// PN_LOCAL escape but not in an NCName, so OWLAPI writes those IRIs in full too.
const NON_NCNAME: &[char] = &[
    '(', ')', '[', ']', '{', '}', '<', '>', '"', '\'', '`', '\\', ',', ';', '|', '^', '?', '=',
    '&', '@', '!', '*', '~', '+', '$', '%',
];

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

/// OWLAPI's `AxiomType.getIndex()` for a component — the primary key OWLAPI uses
/// to order the axioms within an entity (and the general axioms at the end):
/// EquivalentClasses (1) before SubClassOf (2), etc. horned-owl's own `Component`
/// variant order differs, so this table restores OWLAPI's order. Declarations and
/// ontology-meta components never reach the axiom-ordering path.
fn owlapi_axiom_index<A: ForIRI>(comp: &Component<A>) -> u8 {
    use Component::*;
    match comp {
        EquivalentClasses(_) => 1,
        SubClassOf(_) => 2,
        DisjointClasses(_) => 3,
        DisjointUnion(_) => 4,
        ClassAssertion(_) => 5,
        SameIndividual(_) => 6,
        DifferentIndividuals(_) => 7,
        ObjectPropertyAssertion(_) => 8,
        NegativeObjectPropertyAssertion(_) => 9,
        DataPropertyAssertion(_) => 10,
        NegativeDataPropertyAssertion(_) => 11,
        EquivalentObjectProperties(_) => 12,
        SubObjectPropertyOf(ax) => match &ax.sub {
            SubObjectPropertyExpression::ObjectPropertyChain(_) => 25,
            SubObjectPropertyExpression::ObjectPropertyExpression(_) => 13,
        },
        InverseObjectProperties(_) => 14,
        FunctionalObjectProperty(_) => 15,
        InverseFunctionalObjectProperty(_) => 16,
        SymmetricObjectProperty(_) => 17,
        AsymmetricObjectProperty(_) => 18,
        TransitiveObjectProperty(_) => 19,
        ReflexiveObjectProperty(_) => 20,
        IrreflexiveObjectProperty(_) => 21,
        ObjectPropertyDomain(_) => 22,
        ObjectPropertyRange(_) => 23,
        DisjointObjectProperties(_) => 24,
        EquivalentDataProperties(_) => 26,
        SubDataPropertyOf(_) => 27,
        FunctionalDataProperty(_) => 28,
        DataPropertyDomain(_) => 29,
        DataPropertyRange(_) => 30,
        DisjointDataProperties(_) => 31,
        HasKey(_) => 32,
        AnnotationAssertion(_) => 34,
        SubAnnotationPropertyOf(_) => 35,
        AnnotationPropertyRange(_) => 36,
        AnnotationPropertyDomain(_) => 37,
        DatatypeDefinition(_) => 38,
        _ => 0,
    }
}

/// Order two axioms as OWLAPI's `compareTo` does: by axiom-type index first, then
/// by structural content (for which horned-owl's derived `Ord` already matches —
/// e.g. a named superclass sorts before an anonymous class expression).

/// OWLAPI's `OWLObjectTypeIndexProvider` index for a class expression:
/// `CLASS_EXPRESSION_TYPE_INDEX_BASE` (3000) + the visitor's ordinal, and
/// `ENTITY_TYPE_INDEX_BASE + 1` for a named class. Read off owlapi4's
/// `OWLObjectTypeIndexProvider`, not guessed.
fn owlapi_ce_index<A: ForIRI>(ce: &ClassExpression<A>) -> u32 {
    use ClassExpression::*;
    match ce {
        Class(_) => 1001,
        ObjectIntersectionOf(_) => 3001,
        ObjectUnionOf(_) => 3002,
        ObjectComplementOf(_) => 3003,
        ObjectOneOf(_) => 3004,
        ObjectSomeValuesFrom { .. } => 3005,
        ObjectAllValuesFrom { .. } => 3006,
        ObjectHasValue { .. } => 3007,
        ObjectMinCardinality { .. } => 3008,
        ObjectExactCardinality { .. } => 3009,
        ObjectMaxCardinality { .. } => 3010,
        ObjectHasSelf(_) => 3011,
        DataSomeValuesFrom { .. } => 3012,
        DataAllValuesFrom { .. } => 3013,
        DataHasValue { .. } => 3014,
        DataMinCardinality { .. } => 3015,
        DataExactCardinality { .. } => 3016,
        DataMaxCardinality { .. } => 3017,
    }
}

/// OWLAPI's `IRI.compareTo`: namespace first, then remainder — NOT the whole
/// string. The split is the NCName suffix, so `…/obo/GO_1` and `…/obo/GO_2`
/// share a namespace and compare on the local part alone.
fn owlapi_iri_cmp(a: &str, b: &str) -> Ordering {
    let split = |s: &str| -> (usize, ) {
        let idx = s
            .rfind(|c: char| c == '/' || c == '#' || c == ':')
            .map(|i| i + 1)
            .unwrap_or(0);
        (idx,)
    };
    let (ai,) = split(a);
    let (bi,) = split(b);
    a[..ai].cmp(&b[..bi]).then_with(|| a[ai..].cmp(&b[bi..]))
}

fn owlapi_ope_cmp<A: ForIRI>(
    a: &ObjectPropertyExpression<A>,
    b: &ObjectPropertyExpression<A>,
) -> Ordering {
    use ObjectPropertyExpression::*;
    let idx = |o: &ObjectPropertyExpression<A>| match o {
        ObjectProperty(_) => 1002u32,
        InverseObjectProperty(_) => 1003,
    };
    idx(a).cmp(&idx(b)).then_with(|| match (a, b) {
        (ObjectProperty(x), ObjectProperty(y)) => owlapi_iri_cmp(x.0.as_ref(), y.0.as_ref()),
        (InverseObjectProperty(x), InverseObjectProperty(y)) => {
            owlapi_iri_cmp(x.0.as_ref(), y.0.as_ref())
        }
        _ => Ordering::Equal,
    })
}

/// OWLAPI's `compareSets`: both collections are sorted, compared element-wise,
/// and the shorter one loses only if every shared element is equal.
fn owlapi_ce_set_cmp<A: ForIRI>(a: &[ClassExpression<A>], b: &[ClassExpression<A>]) -> Ordering {
    let mut xa: Vec<&ClassExpression<A>> = a.iter().collect();
    let mut xb: Vec<&ClassExpression<A>> = b.iter().collect();
    xa.sort_by(|p, q| owlapi_ce_cmp(p, q));
    xb.sort_by(|p, q| owlapi_ce_cmp(p, q));
    for (p, q) in xa.iter().zip(xb.iter()) {
        let c = owlapi_ce_cmp(p, q);
        if c != Ordering::Equal {
            return c;
        }
    }
    xa.len().cmp(&xb.len())
}

/// OWLAPI's `OWLObject.compareTo` restricted to class expressions: type index
/// first, then `compareObjectOfSameType` — a quantified restriction compares its
/// PROPERTY then its FILLER, an n-ary boolean compares its operand SET.
fn owlapi_ce_cmp<A: ForIRI>(a: &ClassExpression<A>, b: &ClassExpression<A>) -> Ordering {
    use ClassExpression::*;
    let c = owlapi_ce_index(a).cmp(&owlapi_ce_index(b));
    if c != Ordering::Equal {
        return c;
    }
    match (a, b) {
        (Class(x), Class(y)) => owlapi_iri_cmp(x.0.as_ref(), y.0.as_ref()),
        (ObjectIntersectionOf(x), ObjectIntersectionOf(y))
        | (ObjectUnionOf(x), ObjectUnionOf(y)) => owlapi_ce_set_cmp(x, y),
        (ObjectComplementOf(x), ObjectComplementOf(y)) => owlapi_ce_cmp(x, y),
        (
            ObjectSomeValuesFrom { ope: p1, bce: f1 },
            ObjectSomeValuesFrom { ope: p2, bce: f2 },
        )
        | (ObjectAllValuesFrom { ope: p1, bce: f1 }, ObjectAllValuesFrom { ope: p2, bce: f2 }) => {
            owlapi_ope_cmp(p1, p2).then_with(|| owlapi_ce_cmp(f1, f2))
        }
        (
            ObjectMinCardinality { n: n1, ope: p1, bce: f1 },
            ObjectMinCardinality { n: n2, ope: p2, bce: f2 },
        )
        | (
            ObjectMaxCardinality { n: n1, ope: p1, bce: f1 },
            ObjectMaxCardinality { n: n2, ope: p2, bce: f2 },
        )
        | (
            ObjectExactCardinality { n: n1, ope: p1, bce: f1 },
            ObjectExactCardinality { n: n2, ope: p2, bce: f2 },
        ) => owlapi_ope_cmp(p1, p2)
            .then_with(|| n1.cmp(n2))
            .then_with(|| owlapi_ce_cmp(f1, f2)),
        (ObjectHasSelf(p1), ObjectHasSelf(p2)) => owlapi_ope_cmp(p1, p2),
        // Anything else (individuals, data ranges, literals) keeps horned's own
        // structural order — no MONDO general axiom reaches these arms.
        _ => Ordering::Equal,
    }
}

/// OWLAPI's ordering for the axioms that end up in the general (leftover)
/// section: `SubClassOf` compares its SUBCLASS then its superclass, the n-ary
/// class axioms compare their operand sets. Falls back to horned's derived
/// order for anything else, which is what this used to do for everything —
/// leaving MONDO's `imports/merged_import.owl` with ~200 lines of general class
/// axioms in the wrong order once their content finally matched.
fn owlapi_general_cmp<A: ForIRI>(
    a: &&AnnotatedComponent<A>,
    b: &&AnnotatedComponent<A>,
) -> Ordering {
    use Component::*;
    let c = owlapi_axiom_index(&a.component).cmp(&owlapi_axiom_index(&b.component));
    if c != Ordering::Equal {
        return c;
    }
    match (&a.component, &b.component) {
        (SubClassOf(x), SubClassOf(y)) => owlapi_ce_cmp(&x.sub, &y.sub)
            .then_with(|| owlapi_ce_cmp(&x.sup, &y.sup))
            .then_with(|| a.cmp(b)),
        (EquivalentClasses(x), EquivalentClasses(y)) => {
            owlapi_ce_set_cmp(&x.0, &y.0).then_with(|| a.cmp(b))
        }
        (DisjointClasses(x), DisjointClasses(y)) => {
            owlapi_ce_set_cmp(&x.0, &y.0).then_with(|| a.cmp(b))
        }
        // `OWLSubPropertyChainOfAxiomImpl`: the CHAIN element-wise (in order —
        // a chain is a list, not a set), then its length, then the super
        // property. These reach the general section because a chain axiom has
        // no named subject to file it under.
        (SubObjectPropertyOf(x), SubObjectPropertyOf(y)) => {
            use crate::model::SubObjectPropertyExpression as SOPE;
            match (&x.sub, &y.sub) {
                (SOPE::ObjectPropertyChain(c1), SOPE::ObjectPropertyChain(c2)) => {
                    let mut o = Ordering::Equal;
                    for (p, q) in c1.iter().zip(c2.iter()) {
                        o = owlapi_ope_cmp(p, q);
                        if o != Ordering::Equal {
                            break;
                        }
                    }
                    o.then_with(|| c1.len().cmp(&c2.len()))
                        .then_with(|| owlapi_ope_cmp(&x.sup, &y.sup))
                        .then_with(|| a.cmp(b))
                }
                _ => a.cmp(b),
            }
        }
        _ => a.cmp(b),
    }
}

/// OWLAPI's ordering for an ONTOLOGY annotation: property IRI, then value.
/// `OWLAnnotationValue.compareTo` is `OWLObject.compareTo`, so the value's TYPE
/// INDEX comes first — and `IRI` is index 0 while a literal is
/// `DATA_TYPE_INDEX_BASE`+ — so every IRI-valued annotation sorts before every
/// literal-valued one, whatever the strings say. horned's derived `Ord` compares
/// the rendered value instead, which interleaved MONDO's `dc:source` IRIs with
/// its `^^xsd:anyURI` literals.
fn owlapi_ont_annotation_cmp<A: ForIRI>(
    a: &&AnnotatedComponent<A>,
    b: &&AnnotatedComponent<A>,
) -> Ordering {
    fn ann<A: ForIRI>(c: &AnnotatedComponent<A>) -> Option<&crate::model::Annotation<A>> {
        match &c.component {
            Component::OntologyAnnotation(oa) => Some(&oa.0),
            _ => None,
        }
    }
    let (Some(x), Some(y)) = (ann(a), ann(b)) else { return a.cmp(b) };
    let vi = |v: &AnnotationValue<A>| match v {
        AnnotationValue::IRI(_) => 0u32,
        AnnotationValue::AnonymousIndividual(_) => 1007,
        AnnotationValue::Literal(_) => 4000,
    };
    owlapi_iri_cmp(x.ap.0.as_ref(), y.ap.0.as_ref())
        .then_with(|| vi(&x.av).cmp(&vi(&y.av)))
        .then_with(|| match (&x.av, &y.av) {
            (AnnotationValue::IRI(p), AnnotationValue::IRI(q)) => {
                owlapi_iri_cmp(p.as_ref(), q.as_ref())
            }
            (AnnotationValue::Literal(p), AnnotationValue::Literal(q)) => owlapi_literal_cmp(p, q),
            _ => a.cmp(b),
        })
        .then_with(|| a.cmp(b))
}

/// OWLAPI's `OWLLiteralImpl.compareObjectOfSameType`: the DATATYPE IRI first,
/// then the lexical form. An untyped literal is `xsd:string` and a
/// language-tagged one is `rdf:PlainLiteral`. Comparing the rendered text
/// instead put MONDO's seven `^^xsd:anyURI` ontology sources after its plain
/// ones, where `anyURI` < `string` puts them first.
fn owlapi_literal_cmp<A: ForIRI>(a: &Literal<A>, b: &Literal<A>) -> Ordering {
    const XSD_STRING: &str = "http://www.w3.org/2001/XMLSchema#string";
    const RDF_PLAIN: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral";
    let dt = |l: &'_ Literal<A>| -> String {
        match l {
            Literal::Simple { .. } => XSD_STRING.to_string(),
            Literal::Language { .. } => RDF_PLAIN.to_string(),
            Literal::Datatype { datatype_iri, .. } => datatype_iri.as_ref().to_string(),
        }
    };
    let lex = |l: &'_ Literal<A>| -> String {
        match l {
            Literal::Simple { literal }
            | Literal::Language { literal, .. }
            | Literal::Datatype { literal, .. } => literal.clone(),
        }
    };
    owlapi_iri_cmp(&dt(a), &dt(b)).then_with(|| lex(a).cmp(&lex(b)))
}

fn owlapi_axiom_cmp<A: ForIRI>(a: &&AnnotatedComponent<A>, b: &&AnnotatedComponent<A>) -> std::cmp::Ordering {
    owlapi_axiom_index(&a.component)
        .cmp(&owlapi_axiom_index(&b.component))
        .then_with(|| a.cmp(b))
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
    use rstest::rstest;
    use std::path::PathBuf;

    #[rstest]
    fn roundtrip_resource(#[files("src/ont/owl-functional/*.ofn")] resource: PathBuf) {
        let reader = std::fs::File::open(&resource)
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

    // Regression test for https://github.com/phillord/horned-owl/issues/175
    // Annotations on Annotation (annotationAnnotations in OWL 2 spec) are
    // silently discarded because Annotation lacks an `ann` field. A round-trip
    // ont==ont2 comparison would pass (both drops are identical), so we check
    // the written string directly instead.
    #[test]
    fn roundtrip_nested_annotation_on_annotation() {
        let resource = "src/ont/owl-functional/manual/nested-annotation-on-annotation.ofn";
        let reader = std::fs::File::open(resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(reader, Default::default()).unwrap();

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, Some(&prefixes)).unwrap();
        let output = String::from_utf8(writer).unwrap();

        assert!(
            output.contains("Annotation(Annotation("),
            "nested annotation was lost in round-trip:\n{output}"
        );
    }

    #[cfg(test)]
    mod bubo_test {
        use crate::io::ofn::writer::test::*;
        use crate::io::ofn::writer::write;

        use std::fs::File;
        use std::io::BufReader;
        use std::path::Path;

        fn parse_then_output(in_file: &Path, out: &mut dyn std::io::Write) {
            let reader = BufReader::new(File::open(in_file).unwrap());
            let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
                crate::io::ofn::reader::read(reader, Default::default()).unwrap();

            write(out, &ont, Some(&prefixes)).ok().unwrap();
        }

        #[test]
        fn reparse_ofn() -> Result<(), Box<dyn std::error::Error>> {
            crate::io::tests::run_bubo_reparse("owl-functional", parse_then_output)
        }
    }
}
