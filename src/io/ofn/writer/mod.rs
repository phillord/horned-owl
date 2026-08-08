use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::Atom;
use crate::model::DArgument;
use crate::model::IArgument;
use crate::model::Rule;
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

pub use self::as_functional::set_write_xsd_string;
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
    write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
    extra_labels: Option<&HashMap<String, String>>,
    import_order: Option<&[String]>,
) -> Result<W, HornedError> {
    write_full(write, ont, mapping, extra_labels, import_order, None)
}

/// Like [`write_with_labels`], plus `closure_declared`: the entity IRIs declared
/// anywhere in the ontology's imports closure.
///
/// OWLAPI synthesises a `Declaration(...)` for every signature entity that has
/// none of its own, but skips any entity `isDeclared(…, INCLUDED)` — declared in
/// the closure. Serialising an import-bearing ontology therefore adds nothing,
/// while serialising the same ontology with its imports stripped adds one
/// declaration per entity that lost its declaring import. Pass the closure's
/// declared entities to reproduce that exactly; pass `None` and no declarations
/// are added to an ontology that still has imports.
pub fn write_full<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
    extra_labels: Option<&HashMap<String, String>>,
    import_order: Option<&[String]>,
    closure_declared: Option<&std::collections::HashSet<String>>,
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
    // Keyed by (rank, IRI), not IRI alone: OWLAPI declares per *entity*, so an IRI
    // legally punned as both a class and an annotation property needs a
    // declaration for whichever of the two it lacks.
    let mut declared: std::collections::HashSet<(usize, String)> = std::collections::HashSet::new();
    // The banner label of an entity with more than one `rdfs:label` is decided by
    // OWLAPI's `AnnotationValueShortFormProvider`, which walks
    // `getAnnotationAssertionAxioms(iri)` and keeps the FIRST label it sees:
    // `AnnotationLanguageFilter.visit(OWLLiteral)` sets `lastLangMatchIndex = 0`
    // with an empty preferred-language map, and the axiom visit is guarded by
    // `lastLangMatchIndex > 0`, so every later assertion is skipped.
    //
    // That set is a `java.util.HashSet` of the subject's annotation assertions, so
    // "first" is bucket order over the axiom `hashCode` — reproducible, not
    // per-JVM: `robot convert` run twice over one file gives byte-identical
    // banners. Collect every label per subject, plus how many annotation
    // assertions the subject has (which sizes the table), and pick afterwards.
    let mut label_lits: HashMap<String, Vec<(&Literal<A>, bool)>> = HashMap::new();
    let mut subject_ann_count: HashMap<String, usize> = HashMap::new();
    for ac in ont.iter() {
        if let Some((rank, iri)) = declaration_info(&ac.component) {
            entity_rank.insert(iri.clone(), rank);
            declared.insert((rank, iri.clone()));
            declarations.push((
                rank,
                iri,
                ac.as_functional_with_prefixes(mapping).to_string(),
            ));
        } else if let Component::AnnotationAssertion(aa) = &ac.component {
            if let AnnotationSubject::IRI(subj) = &aa.subject {
                *subject_ann_count.entry(subj.as_ref().to_string()).or_insert(0) += 1;
                if aa.ann.ap.0.as_ref() == RDFS_LABEL {
                    if let AnnotationValue::Literal(lit) = &aa.ann.av {
                        label_lits
                            .entry(subj.as_ref().to_string())
                            .or_default()
                            .push((lit, !ac.ann.is_empty()));
                    }
                }
            }
        }
    }
    let labels: HashMap<String, String> = label_lits
        .iter()
        .filter_map(|(subj, lits)| {
            let cap = owlapi_set_cap(subject_ann_count.get(subj).copied().unwrap_or(1).max(1));
            pick_banner_label(subj, lits, cap).map(|l| (subj.clone(), literal_text(l)))
        })
        .collect();
    // OWLAPI groups the output by SIGNATURE, not by declaration: an entity used
    // in an axiom whose `Declaration(...)` lives in an imported ontology is still
    // in `ontology.get<Kind>InSignature()`, so it still gets its own
    // `# Object Property: <IRI> (label)` banner and carries its annotation
    // assertions. HPO's `hp-edit.owl` is the case in point — it declares no
    // object property at all (BFO/RO come from `merged_import.owl`), yet ROBOT's
    // conversion of it opens a full `#   Object Properties` section.
    let signature = signature_kinds(ont);
    for (iri, kinds) in &signature {
        if entity_rank.contains_key(iri) {
            continue;
        }
        // An IRI used as more than one kind is punned; with no declaration to
        // disambiguate, take the lowest-ranked kind so the entity still lands in
        // a section rather than the leftover block.
        if let Some(rank) = (0..6).find(|r| kinds & (1 << r) != 0) {
            entity_rank.insert(iri.clone(), rank);
        }
    }

    // `FunctionalSyntaxObjectRenderer.writeDeclarations` synthesises a
    // `Declaration(...)` for any signature entity that has none of its own —
    // unless the entity is built in, is illegally punned, or is declared
    // somewhere in the imports closure. That last check is why converting an
    // edit file adds nothing (its undeclared entities are declared in the
    // imports) while merging the closure away and re-serialising adds one
    // declaration per entity that lost its declaring import: `remove --select
    // imports` on `hp-edit.owl` is followed by 2192 new declarations.
    //
    // `closure_declared` carries that closure when a caller has resolved it. With
    // no closure supplied we cannot answer `isDeclared(entity, INCLUDED)` for an
    // ontology that still has imports, so nothing is added there — matching ROBOT
    // for every import-bearing file, and differing only for a signature entity
    // declared in no ontology at all.
    let has_imports = ont
        .i()
        .component_for_kind(ComponentKind::Import)
        .next()
        .is_some();
    if !has_imports || closure_declared.is_some() {
        let illegal = illegal_punnings(&signature);
        for (iri, kinds) in &signature {
            if illegal.contains(iri.as_str()) || closure_declared.is_some_and(|c| c.contains(iri)) {
                continue;
            }
            for rank in 0..6 {
                if kinds & (1 << rank) == 0
                    || declared.contains(&(rank, iri.clone()))
                    || is_builtin_entity(rank, iri)
                {
                    continue;
                }
                let abbreviated = match shrink_valid(mapping, iri) {
                    Some((prefix, local)) => format!("{prefix}:{local}"),
                    None => format!("<{iri}>"),
                };
                let rendered =
                    format!("Declaration({}({abbreviated}))", DECL_KEYWORD[rank]);
                declarations.push((rank, iri.clone(), rendered));
            }
        }
    }

    // The Declaration block is `sortOptionally(ontology.getSignature())`, i.e.
    // `OWLObject.compareTo`, which compares the TYPE INDEX before the structure.
    // Those indices are not the section ranks: read off owlapi4's
    // `OWLObjectTypeIndexProvider`, Class is 1001, ObjectProperty 1002,
    // DataProperty 1004, NamedIndividual 1005, AnnotationProperty 1006 and
    // Datatype 4001 — so individuals precede annotation properties and datatypes
    // come last, where the rank order puts annotation properties third. OBA's
    // `imports/merged_import.owl` is the file that shows it, being the one ODK
    // artefact in functional syntax with both individuals and annotation
    // properties in its signature.
    const DECL_TYPE_INDEX: [u32; 6] = [1001, 1002, 1004, 1006, 4001, 1005];
    // OWLAPI orders entities by `IRI.compareTo` — NAMESPACE then remainder, not
    // the whole string. `…/obo/MF#manifestationOf` has namespace `…/obo/MF#`,
    // which sorts after the plain `…/obo/` shared by every `RO_…`/`GO_…`; a
    // whole-string compare put it before them.
    declarations.sort_by(|a, b| {
        DECL_TYPE_INDEX[a.0]
            .cmp(&DECL_TYPE_INDEX[b.0])
            .then_with(|| owlapi_iri_cmp(&a.1, &b.1))
    });
    for (_, _, rendered) in &declarations {
        writeln!(write, "{rendered}")?;
    }

    // Which entity-type sections have a non-empty *signature*. OWLAPI's
    // `writeSortedEntities` emits a trailing blank line for every type whose
    // signature is non-empty — even one with no banner (no entity carrying
    // axioms), e.g. datatypes that appear only inside typed literals. Ranks:
    // Class=0, OP=1, DataProp=2, AP=3, Datatype=4, Individual=5.
    let mut sig_nonempty = [false; 6];
    for kinds in signature.values() {
        for rank in 0..6 {
            if kinds & (1 << rank) != 0 {
                sig_nonempty[rank] = true;
            }
        }
    }
    if !sig_nonempty[4] {
        // A typed literal anywhere puts its datatype (≥ xsd:string) in the
        // signature, so the Datatypes section is non-empty even without a
        // datatype declaration. An ONTOLOGY annotation counts too: an otherwise
        // empty `definitions.owl` carrying only `Annotation(owl:versionInfo …)`
        // still gets the Datatypes blank line from that literal's xsd:string.
        for ac in ont.iter() {
            let lit = match &ac.component {
                Component::AnnotationAssertion(aa) => matches!(aa.ann.av, AnnotationValue::Literal(_)),
                Component::OntologyAnnotation(oa) => {
                    matches!(oa.0.av, AnnotationValue::Literal(_))
                }
                _ => false,
            };
            if lit {
                sig_nonempty[4] = true;
                break;
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
        // `writeSortedEntities` orders each section with `sortOptionally`, i.e.
        // `OWLObject.compareTo` → `IRI.compareTo`, which compares NAMESPACE then
        // remainder — not the whole string. So `…/obo/valid_for_gocam` (namespace
        // `…/obo/`) precedes `…/obo/chebi/3_STAR` (namespace `…/obo/chebi/`)
        // even though `c` < `v` lexically. A `BTreeSet<&str>` got that backwards.
        let mut iris: Vec<&str> = Vec::new();
        for (r, iri) in ann_blocks.keys().chain(axiom_blocks.keys()) {
            if *r == rank {
                iris.push(iri.as_str());
            }
        }
        iris.sort_by(|a, b| owlapi_iri_cmp(a, b));
        iris.dedup();

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
                    // logical axioms, sorted by `compareTo`.
                    let mut anns = anns.clone();
                    anns.sort_by(owlapi_ann_assertion_cmp);
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
    // A QName, not an NCName. `DefaultPrefixManager.getPrefixIRIIgnoreQName`
    // first looks the IRI's NAMESPACE up in the reverse map — the part before
    // its longest NCName suffix, which never holds a colon — and only when that
    // misses does it fall back to testing the tail after a declared namespace
    // with `XMLUtils.isQName`. A QName is an NCName, or two NCNames joined by
    // ONE colon, so an abbreviated local part may carry a single interior colon:
    // FoodOn's `schema:image` provenance is `wikipedia:User:Lupin` in ROBOT's
    // own functional output, and writing it out in full is a byte difference.
    match local.split_once(':') {
        // A second colon leaves a non-NCName on the right, which is `isQName`'s
        // `if (foundColon) return false`.
        Some((prefix, rest)) => is_ncname(prefix) && is_ncname(rest),
        None => is_ncname(local),
    }
}

/// Whether `s` is an NCName (conservatively): non-empty, no delimiter or
/// punctuation XML forbids in a name, and no leading `-`/`.`.
fn is_ncname(s: &str) -> bool {
    !s.is_empty()
        && !s.contains(['/', '#', ' ', ':'])
        && !s.contains(|c: char| c.is_whitespace() || NON_NCNAME.contains(&c))
        && !s.starts_with('-')
        && !s.starts_with('.')
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
/// The `Declaration(...)` keyword for each section rank.
const DECL_KEYWORD: [&str; 6] = [
    "Class",
    "ObjectProperty",
    "DataProperty",
    "AnnotationProperty",
    "Datatype",
    "NamedIndividual",
];

/// Every entity in the ontology's signature, mapped to a bitmask of the section
/// ranks it occurs as (`1 << rank`). More than one bit set means the IRI is
/// punned. Ontology annotations count: `hp-edit.owl` uses `dc:creator` only in
/// its `Ontology(...)` header, and ROBOT declares it.
fn signature_kinds<A: ForIRI, AA: ForIndex<A>>(
    ont: &ComponentMappedOntology<A, AA>,
) -> std::collections::BTreeMap<String, u8> {
    use crate::model::{
        AnnotationProperty, Class, DataProperty, Datatype, NamedIndividual, ObjectProperty,
    };
    use crate::visitor::immutable::{Visit, Walk};

    #[derive(Default)]
    struct Scan(std::collections::BTreeMap<String, u8>);
    impl Scan {
        fn mark(&mut self, iri: &str, rank: usize) {
            *self.0.entry(iri.to_string()).or_insert(0) |= 1 << rank;
        }
    }
    impl<A: ForIRI> Visit<A> for Scan {
        fn visit_class(&mut self, e: &Class<A>) {
            self.mark(e.0.as_ref(), 0)
        }
        fn visit_object_property(&mut self, e: &ObjectProperty<A>) {
            self.mark(e.0.as_ref(), 1)
        }
        fn visit_data_property(&mut self, e: &DataProperty<A>) {
            self.mark(e.0.as_ref(), 2)
        }
        fn visit_annotation_property(&mut self, e: &AnnotationProperty<A>) {
            self.mark(e.0.as_ref(), 3)
        }
        fn visit_datatype(&mut self, e: &Datatype<A>) {
            self.mark(e.0.as_ref(), 4)
        }
        fn visit_named_individual(&mut self, e: &NamedIndividual<A>) {
            self.mark(e.0.as_ref(), 5)
        }
    }

    let mut walk = Walk::new(Scan::default());
    for ac in ont.iter() {
        walk.annotated_component(ac);
    }
    walk.into_visit().0
}

/// OWLAPI's `OWLDocumentFormatImpl.determineIllegalPunnings`: an IRI used as both
/// an object and an annotation property — or data/annotation, data/object, or
/// datatype/class — is illegally punned, and the renderer adds no declaration for
/// it. Individuals never make a punning illegal.
fn illegal_punnings(
    sig: &std::collections::BTreeMap<String, u8>,
) -> std::collections::HashSet<&str> {
    const CLASS: u8 = 1 << 0;
    const OP: u8 = 1 << 1;
    const DP: u8 = 1 << 2;
    const AP: u8 = 1 << 3;
    const DT: u8 = 1 << 4;
    sig.iter()
        .filter(|(_, k)| {
            let k = **k;
            (k & OP != 0 && k & AP != 0)
                || (k & DP != 0 && k & AP != 0)
                || (k & DP != 0 && k & OP != 0)
                || (k & DT != 0 && k & CLASS != 0)
        })
        .map(|(iri, _)| iri.as_str())
        .collect()
}

/// OWLAPI's `OWLEntity.isBuiltIn()`, which differs by entity kind: `owl:Thing`
/// and `owl:Nothing` for classes, the top/bottom properties, a fixed list of
/// annotation properties (`OWLRDFVocabulary.BUILT_IN_ANNOTATION_PROPERTY_IRIS`),
/// and the OWL 2 datatype map for datatypes. Individuals are never built in.
///
/// The datatype case is taken as "in one of the four schema namespaces" rather
/// than the enumerated `OWL2Datatype` map: every member of that map is in one of
/// them, and a user-defined datatype minted inside them would be malformed.
fn is_builtin_entity(rank: usize, iri: &str) -> bool {
    const OWL: &str = "http://www.w3.org/2002/07/owl#";
    const RDFS: &str = "http://www.w3.org/2000/01/rdf-schema#";
    const RDF: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
    match rank {
        0 => iri == "http://www.w3.org/2002/07/owl#Thing"
            || iri == "http://www.w3.org/2002/07/owl#Nothing",
        1 => {
            iri == "http://www.w3.org/2002/07/owl#topObjectProperty"
                || iri == "http://www.w3.org/2002/07/owl#bottomObjectProperty"
        }
        2 => {
            iri == "http://www.w3.org/2002/07/owl#topDataProperty"
                || iri == "http://www.w3.org/2002/07/owl#bottomDataProperty"
        }
        3 => matches!(
            iri,
            "http://www.w3.org/2000/01/rdf-schema#label"
                | "http://www.w3.org/2000/01/rdf-schema#comment"
                | "http://www.w3.org/2000/01/rdf-schema#seeAlso"
                | "http://www.w3.org/2000/01/rdf-schema#isDefinedBy"
                | "http://www.w3.org/2002/07/owl#versionInfo"
                | "http://www.w3.org/2002/07/owl#backwardCompatibleWith"
                | "http://www.w3.org/2002/07/owl#priorVersion"
                | "http://www.w3.org/2002/07/owl#incompatibleWith"
                | "http://www.w3.org/2002/07/owl#deprecated"
        ),
        4 => {
            iri.starts_with(XSD)
                || iri.starts_with(RDF)
                || iri.starts_with(RDFS)
                || iri.starts_with(OWL)
        }
        _ => false,
    }
}

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
        // `AxiomType.SWRL_RULE` sits between `HAS_KEY` and `ANNOTATION_ASSERTION`,
        // which is why ROBOT writes UBERON's three `DLSafeRule`s at the very end of
        // the leftover block, after the property chains. Defaulting them to 0 put
        // them at the front of it.
        Rule(_) => 33,
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

/// `java.lang.String.hashCode` — over UTF-16 code units, so a non-BMP character
/// contributes its two surrogates.
fn java_hash(s: &str) -> i32 {
    let mut h: i32 = 0;
    for u in s.encode_utf16() {
        h = h.wrapping_mul(31).wrapping_add(u as i32);
    }
    h
}

fn xml_name_start(c: u32) -> bool {
    c == b':' as u32
        || (b'A' as u32..=b'Z' as u32).contains(&c)
        || c == b'_' as u32
        || (b'a' as u32..=b'z' as u32).contains(&c)
        || (0xC0..=0xD6).contains(&c)
        || (0xD8..=0xF6).contains(&c)
        || (0xF8..=0x2FF).contains(&c)
        || (0x370..=0x37D).contains(&c)
        || (0x37F..=0x1FFF).contains(&c)
        || (0x200C..=0x200D).contains(&c)
        || (0x2070..=0x218F).contains(&c)
        || (0x2C00..=0x2FEF).contains(&c)
        || (0x3001..=0xD7FF).contains(&c)
        || (0xF900..=0xFDCF).contains(&c)
        || (0xFDF0..=0xFFFD).contains(&c)
        || (0x10000..=0xEFFFF).contains(&c)
}

fn xml_name_char(c: u32) -> bool {
    xml_name_start(c)
        || c == b'-' as u32
        || c == b'.' as u32
        || (b'0' as u32..=b'9' as u32).contains(&c)
        || c == 0xB7
        || (0x0300..=0x036F).contains(&c)
        || (0x203F..=0x2040).contains(&c)
}

/// OWLAPI `XMLUtils.getNCNameSuffixIndex`: where the local part begins, or `None`
/// when the whole string is the namespace.
fn ncname_suffix_index(s: &str) -> Option<usize> {
    let b = s.as_bytes();
    if b.len() > 1 && b[0] == b'_' && b[1] == b':' {
        return None;
    }
    let mut index = None;
    for (i, ch) in s.char_indices().rev() {
        let cp = ch as u32;
        if cp != ':' as u32 && xml_name_start(cp) {
            index = Some(i);
        }
        if !(cp != ':' as u32 && xml_name_char(cp)) {
            break;
        }
    }
    index
}

/// OWLAPI `IRI.hashCode` = `namespace.hashCode() + remainder.hashCode()`.
fn owlapi_iri_hash(iri: &str) -> i32 {
    match ncname_suffix_index(iri) {
        Some(i) => java_hash(&iri[..i]).wrapping_add(java_hash(&iri[i..])),
        None => java_hash(iri),
    }
}

/// `OWLLiteralImplPlain.hashCode` — an untyped literal, with or without a
/// language tag.
fn owlapi_plain_literal_hash(value: &str, lang: &str) -> i32 {
    let base = (3231644899u32 as i32).wrapping_add(java_hash(value).wrapping_mul(65536));
    if lang.is_empty() {
        base
    } else {
        base.wrapping_mul(37).wrapping_add(java_hash(lang))
    }
}

/// `OWLAnnotationAssertionAxiomImpl.hashCode` for an UNANNOTATED `rdfs:label`
/// assertion: the axiom-type seed, then subject, property, value, and the (empty)
/// annotation collection.
fn owlapi_label_axiom_hash(subj: &str, value: &str, lang: &str) -> i32 {
    let mut h: i32 = 739;
    h = h.wrapping_mul(31).wrapping_add(owlapi_iri_hash(subj));
    h = h
        .wrapping_mul(31)
        .wrapping_add(owlapi_iri_hash(RDFS_LABEL).wrapping_add(188077));
    h = h.wrapping_mul(31).wrapping_add(owlapi_plain_literal_hash(value, lang));
    h.wrapping_mul(31)
}

/// Table size of a default `java.util.HashSet` after `n` incremental adds.
fn owlapi_set_cap(n: usize) -> usize {
    let mut cap = 16usize;
    while n * 4 > cap * 3 {
        cap <<= 1;
    }
    cap
}

/// `java.util.HashMap`'s bucket for a hash in a table of `cap`.
fn owlapi_bucket(hash: i32, cap: usize) -> usize {
    let h = hash as u32;
    ((h ^ (h >> 16)) as usize) & (cap - 1)
}

/// The `rdfs:label` OWLAPI's short-form provider reaches first — the one whose
/// assertion lands in the lowest bucket of the subject's annotation-assertion set.
///
/// The bucket rule is applied only when it is unambiguous: every label assertion
/// is unannotated (so the annotation-collection hash is 0) and untyped (the only
/// literal kind whose hash is reproduced here), and no two land in the same
/// bucket — a within-bucket tie is broken by OWLAPI's parse-insertion order, which
/// nothing here can recover. Otherwise fall back to the first in OWLAPI's own
/// `compareTo` order, which is at least deterministic.
fn pick_banner_label<'a, A: ForIRI>(
    subj: &str,
    lits: &[(&'a Literal<A>, bool)],
    cap: usize,
) -> Option<&'a Literal<A>> {
    if lits.is_empty() {
        return None;
    }
    if lits.len() == 1 {
        return Some(lits[0].0);
    }
    fn plain<A: ForIRI>(l: &Literal<A>) -> Option<(&str, &str)> {
        match l {
            Literal::Simple { literal } => Some((literal.as_str(), "")),
            Literal::Language { literal, lang } => Some((literal.as_str(), lang.as_str())),
            Literal::Datatype { .. } => None,
        }
    }
    if lits.iter().all(|(l, annotated)| !annotated && plain(l).is_some()) {
        let mut ranked: Vec<(usize, &'a Literal<A>)> = lits
            .iter()
            .map(|(l, _)| {
                let (v, lang) = plain(l).unwrap();
                (owlapi_bucket(owlapi_label_axiom_hash(subj, v, lang), cap), *l)
            })
            .collect();
        ranked.sort_by_key(|(b, _)| *b);
        if ranked[0].0 != ranked[1].0 {
            return Some(ranked[0].1);
        }
    }
    lits.iter()
        .map(|(l, _)| *l)
        .min_by(|a, b| owlapi_literal_cmp(a, b))
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
        (Rule(x), Rule(y)) => owlapi_rule_cmp(x, y).then_with(|| a.cmp(b)),
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

/// OWLAPI's `OWLAnnotationAssertionAxiomImpl.compareObjectOfSameType`: the
/// SUBJECT, then the PROPERTY, then the VALUE — and nothing else, so two
/// assertions differing only in their own annotations compare equal and a stable
/// sort leaves them where they were.
///
/// The derived `Ord` stood in for this and got the value wrong: it compares
/// `Literal` by VARIANT (`Simple` before `Language`), where OWLAPI compares the
/// literal's DATATYPE IRI first — and `rdf:PlainLiteral` (a language-tagged
/// literal) sorts before `xsd:string` (an untyped one) because `…/1999/…` sorts
/// before `…/2001/…`. So every entity carrying both an `@en` label and a plain
/// one came out in the other order.
fn owlapi_ann_assertion_cmp<A: ForIRI>(
    a: &&AnnotatedComponent<A>,
    b: &&AnnotatedComponent<A>,
) -> Ordering {
    fn aa<A: ForIRI>(c: &AnnotatedComponent<A>) -> Option<&crate::model::AnnotationAssertion<A>> {
        match &c.component {
            Component::AnnotationAssertion(aa) => Some(aa),
            _ => None,
        }
    }
    let (Some(x), Some(y)) = (aa(a), aa(b)) else { return owlapi_axiom_cmp(a, b) };
    let subj = |s: &AnnotationSubject<A>| match s {
        AnnotationSubject::IRI(i) => i.as_ref().to_string(),
        AnnotationSubject::AnonymousIndividual(n) => n.0.as_ref().to_string(),
    };
    // `OWLAnnotationValue` is an `OWLObject`, so unequal types compare by type
    // index before structure: IRI 0, anonymous individual 1007, literal 4000+.
    let vi = |v: &AnnotationValue<A>| match v {
        AnnotationValue::IRI(_) => 0u32,
        AnnotationValue::AnonymousIndividual(_) => 1007,
        AnnotationValue::Literal(_) => 4000,
    };
    owlapi_iri_cmp(&subj(&x.subject), &subj(&y.subject))
        .then_with(|| owlapi_iri_cmp(x.ann.ap.0.as_ref(), y.ann.ap.0.as_ref()))
        .then_with(|| vi(&x.ann.av).cmp(&vi(&y.ann.av)))
        .then_with(|| match (&x.ann.av, &y.ann.av) {
            (AnnotationValue::IRI(p), AnnotationValue::IRI(q)) => {
                owlapi_iri_cmp(p.as_ref(), q.as_ref())
            }
            (AnnotationValue::Literal(p), AnnotationValue::Literal(q)) => owlapi_literal_cmp(p, q),
            _ => Ordering::Equal,
        })
        // Deterministic tie-break where OWLAPI's comparator returns 0 (two
        // assertions differing only in their own annotations); OWLAPI keeps the
        // set's iteration order there, which is not reproducible.
        .then_with(|| a.cmp(b))
}

/// OWLAPI's `OWLLiteralImpl.compareObjectOfSameType`: the DATATYPE IRI first,
/// then the lexical form. An untyped literal is `xsd:string` and a
/// language-tagged one is `rdf:PlainLiteral`. Comparing the rendered text
/// instead put MONDO's seven `^^xsd:anyURI` ontology sources after its plain
/// ones, where `anyURI` < `string` puts them first.
fn owlapi_literal_cmp<A: ForIRI>(a: &Literal<A>, b: &Literal<A>) -> Ordering {
    const RDF_PLAIN: &str = "http://www.w3.org/1999/02/22-rdf-syntax-ns#PlainLiteral";
    // An UNTYPED literal is `rdf:PlainLiteral`, with or without a language tag:
    // OWLAPI's RDF parser builds `OWLLiteralImplPlain` for both, and every
    // literal in these documents has been through RDF/XML (ROBOT writes the
    // merged mirror as RDF/XML and reads it back, and an `xsd:string` survives
    // that trip as a bare RDF 1.1 literal). Calling the untagged one `xsd:string`
    // split each entity's synonyms into two runs, where ROBOT interleaves them:
    // `"beef mince"`, `"beef mince"@en`, `"ground beef"@en`, `"hamburger meat"`.
    // A literal carrying an EXPLICIT datatype keys as that datatype — including
    // `xsd:string`, which reaches us only from a parser that really did type it
    // (owlmake's OBO reader), and which must keep sorting after `xsd:anyURI`.
    let dt = |l: &'_ Literal<A>| -> String {
        match l {
            Literal::Simple { .. } | Literal::Language { .. } => RDF_PLAIN.to_string(),
            Literal::Datatype { datatype_iri, .. } => datatype_iri.as_ref().to_string(),
        }
    };
    fn lex<A: ForIRI>(l: &Literal<A>) -> &str {
        match l {
            Literal::Simple { literal }
            | Literal::Language { literal, .. }
            | Literal::Datatype { literal, .. } => literal.as_str(),
        }
    }
    // …then the lexical form, then the LANGUAGE — `OWLLiteralImplPlain`'s third
    // key, which is what puts `"beef mince"` (no tag) before `"beef mince"@en`.
    fn lang<A: ForIRI>(l: &Literal<A>) -> &str {
        match l {
            Literal::Language { lang, .. } => lang.as_str(),
            _ => "",
        }
    }
    owlapi_iri_cmp(&dt(a), &dt(b))
        .then_with(|| lex(a).cmp(lex(b)))
        .then_with(|| lang(a).cmp(lang(b)))
}

fn owlapi_axiom_cmp<A: ForIRI>(a: &&AnnotatedComponent<A>, b: &&AnnotatedComponent<A>) -> std::cmp::Ordering {
    owlapi_axiom_index(&a.component)
        .cmp(&owlapi_axiom_index(&b.component))
        .then_with(|| match (&a.component, &b.component) {
            (Component::Rule(x), Component::Rule(y)) => owlapi_rule_cmp(x, y),
            _ => a.cmp(b),
        })
}

/// `SWRLRuleImpl.compareObjectOfSameType`: the BODY atom sets first, then the
/// HEAD atom sets — each `compareSets`, so sorted and compared element-wise. The
/// rule's own annotations play no part, which is why RO's annotated rules
/// interleave with its bare ones instead of grouping.
fn owlapi_rule_cmp<A: ForIRI>(a: &Rule<A>, b: &Rule<A>) -> Ordering {
    owlapi_atom_set_cmp(&a.body, &b.body).then_with(|| owlapi_atom_set_cmp(&a.head, &b.head))
}

fn owlapi_atom_set_cmp<A: ForIRI>(a: &[Atom<A>], b: &[Atom<A>]) -> Ordering {
    let mut xa: Vec<&Atom<A>> = a.iter().collect();
    let mut xb: Vec<&Atom<A>> = b.iter().collect();
    xa.sort_by(|p, q| owlapi_atom_cmp(p, q));
    xb.sort_by(|p, q| owlapi_atom_cmp(p, q));
    for (p, q) in xa.iter().zip(xb.iter()) {
        let c = owlapi_atom_cmp(p, q);
        if c != Ordering::Equal {
            return c;
        }
    }
    xa.len().cmp(&xb.len())
}

/// `OWLObjectTypeIndexProvider`'s `RULE_OBJECT_TYPE_INDEX_BASE` (6000) plus the
/// visitor ordinal. A class atom therefore sorts before an object-property atom,
/// which is what puts RO's `ClassAtom(BFO_…)`-headed rules first.
fn owlapi_atom_index<A: ForIRI>(atom: &Atom<A>) -> u32 {
    use Atom::*;
    match atom {
        ClassAtom { .. } => 6001,
        DataRangeAtom { .. } => 6002,
        ObjectPropertyAtom { .. } => 6003,
        DataPropertyAtom { .. } => 6004,
        BuiltInAtom { .. } => 6005,
        SameIndividualAtom(..) => 6009,
        DifferentIndividualsAtom(..) => 6010,
    }
}

fn owlapi_atom_cmp<A: ForIRI>(a: &Atom<A>, b: &Atom<A>) -> Ordering {
    use Atom::*;
    let c = owlapi_atom_index(a).cmp(&owlapi_atom_index(b));
    if c != Ordering::Equal {
        return c;
    }
    match (a, b) {
        (ClassAtom { pred: p1, arg: a1 }, ClassAtom { pred: p2, arg: a2 }) => {
            owlapi_ce_cmp(p1, p2).then_with(|| owlapi_iarg_cmp(a1, a2))
        }
        (
            ObjectPropertyAtom { pred: p1, args: (x1, y1) },
            ObjectPropertyAtom { pred: p2, args: (x2, y2) },
        ) => owlapi_ope_cmp(p1, p2)
            .then_with(|| owlapi_iarg_cmp(x1, x2))
            .then_with(|| owlapi_iarg_cmp(y1, y2)),
        (
            DataPropertyAtom { pred: p1, args: (x1, y1) },
            DataPropertyAtom { pred: p2, args: (x2, y2) },
        ) => owlapi_iri_cmp(p1.0.as_ref(), p2.0.as_ref())
            .then_with(|| owlapi_darg_cmp(x1, x2))
            .then_with(|| owlapi_darg_cmp(y1, y2)),
        (BuiltInAtom { pred: p1, args: v1 }, BuiltInAtom { pred: p2, args: v2 }) => {
            let mut c = owlapi_iri_cmp(p1.as_ref(), p2.as_ref());
            for (x, y) in v1.iter().zip(v2.iter()) {
                if c != Ordering::Equal {
                    return c;
                }
                c = owlapi_darg_cmp(x, y);
            }
            c.then_with(|| v1.len().cmp(&v2.len()))
        }
        (SameIndividualAtom(x1, y1), SameIndividualAtom(x2, y2))
        | (DifferentIndividualsAtom(x1, y1), DifferentIndividualsAtom(x2, y2)) => {
            owlapi_iarg_cmp(x1, x2).then_with(|| owlapi_iarg_cmp(y1, y2))
        }
        // A data-range predicate is the one shape whose OWLAPI comparator is not
        // reproduced here; nothing in these ontologies uses one.
        _ => a.cmp(b),
    }
}

/// `SWRLVariable` is `RULE_OBJECT_TYPE_INDEX_BASE + 6` and
/// `SWRLIndividualArgument` is `+ 7`, so a variable sorts before an individual.
fn owlapi_iarg_cmp<A: ForIRI>(a: &IArgument<A>, b: &IArgument<A>) -> Ordering {
    use IArgument::*;
    let idx = |i: &IArgument<A>| match i {
        Variable(_) => 6006u32,
        Individual(_) => 6007,
    };
    idx(a).cmp(&idx(b)).then_with(|| match (a, b) {
        (Variable(x), Variable(y)) => owlapi_iri_cmp(x.0.as_ref(), y.0.as_ref()),
        _ => a.cmp(b),
    })
}

/// `SWRLLiteralArgument` is `RULE_OBJECT_TYPE_INDEX_BASE + 8`, after the variable.
fn owlapi_darg_cmp<A: ForIRI>(a: &DArgument<A>, b: &DArgument<A>) -> Ordering {
    use DArgument::*;
    let idx = |d: &DArgument<A>| match d {
        Variable(_) => 6006u32,
        Literal(_) => 6008,
    };
    idx(a).cmp(&idx(b)).then_with(|| match (a, b) {
        (Variable(x), Variable(y)) => owlapi_iri_cmp(x.0.as_ref(), y.0.as_ref()),
        (Literal(x), Literal(y)) => owlapi_literal_cmp(x, y),
        _ => Ordering::Equal,
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

        let (ont2, prefixes2): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(std::io::Cursor::new(&writer), Default::default())
                .unwrap();

        assert_eq!(prefixes, prefixes2, "prefix mapping differ");
        // A rule's body and head are SETS in OWL — horned stores them as `Vec` to
        // keep a document's order, and this writer permutes a two-atom one to match
        // `FunctionalSyntaxObjectRenderer.write(Collection)`. That is a reordering
        // of a set, not a loss, so compare rules by their atoms sorted.
        assert_eq!(sorted_rules(&ont), sorted_rules(&ont2), "ontologies differ");
    }

    /// The ontology with every rule's body and head atoms sorted, so a rule can be
    /// compared without depending on the order the two are written in.
    fn sorted_rules(
        ont: &ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>,
    ) -> std::collections::BTreeSet<AnnotatedComponent<RcStr>> {
        ont.iter()
            .map(|ac| {
                let mut ac = ac.clone();
                if let Component::Rule(r) = &mut ac.component {
                    r.body.sort();
                    r.head.sort();
                }
                ac
            })
            .collect()
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
