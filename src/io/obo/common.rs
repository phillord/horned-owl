//! Pest-free pieces of the OBO reader: namespaces, the id-expansion
//! `Context`, annotation builders and the whole-document `Finalizer`.
//!
//! Shared by the nom reader and the pest reader in `horned-pest`.

use std::collections::{BTreeSet, HashMap};

use curie::PrefixMapping;

use crate::model::*;

// --- namespaces the OBO→OWL mapping relies on ------------------------------

pub const OBO_BASE: &str = "http://purl.obolibrary.org/obo/";
pub const OIO: &str = "http://www.geneontology.org/formats/oboInOwl#";

/// Whether an OBO `ontology:` value is already an absolute `http(s)` IRI (used
/// as-is) rather than a short id to expand under `OBO_BASE`. Testing a bare
/// `http` prefix wrongly matched short ids like `httptest`/`httpfoo`, leaving a
/// relative ontology IRI.
pub fn is_http_iri(s: &str) -> bool {
    s.starts_with("http://") || s.starts_with("https://")
}
pub const RDFS_LABEL: &str = "http://www.w3.org/2000/01/rdf-schema#label";
pub const RDFS_COMMENT: &str = "http://www.w3.org/2000/01/rdf-schema#comment";
pub const IAO_DEF: &str = "http://purl.obolibrary.org/obo/IAO_0000115";
pub const IAO_TERM_REPLACED_BY: &str = "http://purl.obolibrary.org/obo/IAO_0100001";
pub const IAO_OBSOLESCENCE_REASON: &str = "http://purl.obolibrary.org/obo/IAO_0000231";
pub const IAO_TERMS_MERGED: &str = "http://purl.obolibrary.org/obo/IAO_0000227";
pub const OWL_DEPRECATED: &str = "http://www.w3.org/2002/07/owl#deprecated";
pub const XSD_BOOLEAN: &str = "http://www.w3.org/2001/XMLSchema#boolean";

/// The prefixes an OBO document declares implicitly (OBO 1.4 §5.9.2), matching
/// `fastobo-owl`. `idspace:` header clauses are layered on top by [`scan_header`].
pub fn obo_prefixes() -> PrefixMapping {
    let mut pm = PrefixMapping::default();
    for (p, iri) in [
        ("xsd", "http://www.w3.org/2001/XMLSchema#"),
        ("owl", "http://www.w3.org/2002/07/owl#"),
        ("obo", OBO_BASE),
        ("oboInOwl", OIO),
        ("xml", "http://www.w3.org/XML/1998/namespace"),
        ("rdf", "http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        ("dc", "http://purl.org/dc/elements/1.1/"),
        ("rdfs", "http://www.w3.org/2000/01/rdf-schema#"),
    ] {
        pm.add_prefix(p, iri).ok();
    }
    pm
}

/// Conversion context: the IRI intern arena, the `idspace:` expansions, the
/// header `default-namespace` (applied to terms lacking their own `namespace`),
/// and the ontology-local `#` namespace bare ids resolve into.
pub struct Context<'a, A: ForIRI> {
    pub build: &'a Build<A>,
    pub idspace: HashMap<String, String>,
    pub default_ns: Option<String>,
    /// `obo/<ontology-id>#` — where an unprefixed id resolves, matching
    /// oboformat/ROBOT (e.g. a bare `part_of` → `obo/<ont>#part_of`).
    pub onto_ns: Option<String>,
    /// Relation shorthands: a bare `[Typedef]` id with a single `xref` resolves
    /// to that xref's IRI everywhere it is used (`part_of` → `BFO_0000050`).
    pub rel_map: HashMap<String, String>,
    /// IRIs of `[Typedef]`s declared `is_metadata_tag: true` — these are
    /// annotation properties, so a `relationship:` using one is an annotation
    /// assertion, not an existential (oboformat/ROBOT).
    pub metadata_tags: BTreeSet<String>,
}

impl<'a, A: ForIRI> Context<'a, A> {
    /// Expand an OBO id to an IRI, honouring `idspace:` declarations, resolving
    /// a bare (unprefixed) id into the ontology's own `#` namespace, and falling
    /// back to the OBO PURL convention `PREFIX:LOCAL` ⇄ `.../obo/PREFIX_LOCAL`.
    pub fn expand(&self, id: &str) -> IRI<A> {
        self.build
            .iri(expand_id_with(id, &self.idspace, self.onto_ns.as_deref()))
    }

    /// Expand a relation id, resolving a shorthand via [`Self::rel_map`] first.
    pub fn expand_rel(&self, id: &str) -> IRI<A> {
        match self.rel_map.get(id) {
            Some(iri) => self.build.iri(iri.as_str()),
            None => self.expand(id),
        }
    }

    pub fn class(&self, id: &str) -> Class<A> {
        self.build.class(self.expand(id))
    }
}

/// Expand a bare or `PREFIX:LOCAL` OBO id to the IRI a reader would build for
/// it. `pub(crate)` so the writer can use it as a round-trip check: an
/// `oboInOwl:id` literal inherited from an entity's *original* namespace is
/// only safe to reuse verbatim as this ontology's stanza id if expanding it
/// here, under this document's own idspaces/default namespace, gives back
/// the same IRI (see `writer::compress`, this function's inverse).
pub fn expand_id_with(
    id: &str,
    idspace: &HashMap<String, String>,
    onto_ns: Option<&str>,
) -> String {
    let id = id.trim();
    if id.starts_with("http://") || id.starts_with("https://") {
        return id.to_string();
    }
    match id.split_once(':') {
        Some((pre, local)) => match idspace.get(pre) {
            Some(base) => format!("{base}{local}"),
            // The standard prefixes map to their real namespaces (they are
            // implicitly declared in OBO), not the `obo/PREFIX_LOCAL` PURL —
            // kept in sync with the writer's `compress` so ids round-trip.
            None => match std_prefix(pre) {
                Some(ns) => format!("{ns}{local}"),
                None => format!("{OBO_BASE}{pre}_{local}"),
            },
        },
        // A bare id is ontology-native (oboformat/ROBOT): it lives in the
        // ontology's own `#` namespace, not the generic `obo/` namespace.
        None => match onto_ns {
            Some(ns) => format!("{ns}{id}"),
            None => format!("{OBO_BASE}{id}"),
        },
    }
}

/// The namespace of a standard OBO-implicit prefix, if `pre` is one. Kept in
/// sync with the writer's `compress` for round-trip symmetry.
pub fn std_prefix(pre: &str) -> Option<&'static str> {
    match pre {
        "xsd" => Some("http://www.w3.org/2001/XMLSchema#"),
        "rdf" => Some("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        "rdfs" => Some("http://www.w3.org/2000/01/rdf-schema#"),
        "owl" => Some("http://www.w3.org/2002/07/owl#"),
        _ => None,
    }
}

/// Strip the surrounding quotes from a lexed `QuotedString` and unescape it.
pub fn unquote(s: &str) -> String {
    let inner = s
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(s);
    unescape(inner)
}

/// Unescape an OBO string (`\n`→newline, `\t`→tab, `\W`→space; a backslash
/// before any other char drops the backslash).
pub fn unescape(s: &str) -> String {
    if !s.contains('\\') {
        return s.to_string();
    }
    let mut out = String::with_capacity(s.len());
    let mut escaped = false;
    for c in s.chars() {
        if escaped {
            out.push(match c {
                'n' => '\n',
                't' => '\t',
                'W' => ' ',
                other => other,
            });
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else {
            out.push(c);
        }
    }
    out
}

// --- small builders --------------------------------------------------------

pub fn lit_ann<A: ForIRI>(b: &Build<A>, prop: &str, value: &str) -> Annotation<A> {
    Annotation {
        ap: b.annotation_property(prop),
        av: AnnotationValue::Literal(Literal::Simple {
            literal: value.to_string(),
        }),
        ann: Default::default(),
    }
}

/// OBO has no syntax for a literal's language; the `name`/`comment`
/// qualifier keys are read as English, matching what oboformat/ROBOT write
/// on the way out to OWL.
pub fn en_ann<A: ForIRI>(b: &Build<A>, prop: &str, value: &str) -> Annotation<A> {
    Annotation {
        ap: b.annotation_property(prop),
        av: AnnotationValue::Literal(Literal::Language {
            literal: value.to_string(),
            lang: "en".to_string(),
        }),
        ann: Default::default(),
    }
}

pub fn iri_ann<A: ForIRI>(b: &Build<A>, prop: &str, iri: IRI<A>) -> Annotation<A> {
    Annotation {
        ap: b.annotation_property(prop),
        av: AnnotationValue::IRI(iri),
        ann: Default::default(),
    }
}

pub fn assertion<A: ForIRI>(subject: &IRI<A>, ann: Annotation<A>) -> AnnotatedComponent<A> {
    AnnotatedComponent::new(
        AnnotationAssertion {
            subject: AnnotationSubject::IRI(subject.clone()),
            ann,
        },
        Default::default(),
    )
}

pub fn component<A: ForIRI, C: Into<Component<A>>>(c: C) -> AnnotatedComponent<A> {
    AnnotatedComponent::new(c, Default::default())
}

/// Build a component carrying axiom-level annotations (e.g. from a trailing
/// `{qualifier}` block).
pub fn component_ann<A: ForIRI, C: Into<Component<A>>>(
    c: C,
    anns: Vec<Annotation<A>>,
) -> AnnotatedComponent<A> {
    AnnotatedComponent::new(c, anns.into_iter().collect())
}

/// Add axiom-level annotations to an already-built component.
pub fn with_anns<A: ForIRI>(
    mut ac: AnnotatedComponent<A>,
    anns: Vec<Annotation<A>>,
) -> AnnotatedComponent<A> {
    ac.ann.extend(anns);
    ac
}

/// Map a `{qualifier}` block to axiom annotations. `name`/`comment` are the
/// builtin synonyms also used by the top-level name:/comment: tags, a bare
/// key otherwise lives in the oboInOwl namespace (`source` →
/// `oboInOwl:source`), and a CURIE key expands (matching ROBOT). Structural
/// qualifiers (cardinality, gci_*) are consumed elsewhere and skipped here.
pub fn qual_anns<A: ForIRI>(
    ctx: &Context<'_, A>,
    quals: &[(String, String)],
) -> Vec<Annotation<A>> {
    let b = ctx.build;
    quals
        .iter()
        .filter(|(k, _)| {
            !matches!(
                k.as_str(),
                "cardinality"
                    | "minCardinality"
                    | "maxCardinality"
                    | "min_cardinality"
                    | "max_cardinality"
                    | "gci_relation"
                    | "gci_filler"
            )
        })
        .map(|(k, v)| match k.as_str() {
            "name" => en_ann(b, RDFS_LABEL, v),
            "comment" => en_ann(b, RDFS_COMMENT, v),
            _ if k.contains(':') => lit_ann(b, ctx.expand(k).as_ref(), v),
            _ => lit_ann(b, &format!("{OIO}{k}"), v),
        })
        .collect()
}

/// A `gci_relation` + `gci_filler` qualifier pair turns an `is_a`/`relationship`
/// into a General Class Inclusion: the subject becomes `C ⊓ (gci_rel some
/// gci_filler)`. Returns that intersection subject when both are present.
pub fn gci_subject<A: ForIRI>(
    ctx: &Context<'_, A>,
    class_iri: &IRI<A>,
    quals: &[(String, String)],
) -> Option<ClassExpression<A>> {
    let get = |k: &str| quals.iter().find(|(q, _)| q == k).map(|(_, v)| v.as_str());
    let (rel, filler) = (get("gci_relation")?, get("gci_filler")?);
    Some(ClassExpression::ObjectIntersectionOf(vec![
        ClassExpression::Class(ctx.build.class(class_iri.clone())),
        ClassExpression::ObjectSomeValuesFrom {
            ope: ope(ctx, rel),
            bce: Box::new(ClassExpression::Class(ctx.class(filler))),
        },
    ]))
}

pub fn ont_ann<A: ForIRI>(b: &Build<A>, prop: &str, value: &str) -> AnnotatedComponent<A> {
    component(OntologyAnnotation(lit_ann(b, prop, value)))
}

/// Expand a datatype id: the standard `xsd`/`rdf`/`rdfs`/`owl` prefixes map to
/// their namespaces, everything else via the usual id expansion.
pub fn expand_datatype<A: ForIRI>(dt: &str, ctx: &Context<'_, A>) -> String {
    match dt.split_once(':') {
        Some(("xsd", l)) => format!("http://www.w3.org/2001/XMLSchema#{l}"),
        Some(("rdf", l)) => format!("http://www.w3.org/1999/02/22-rdf-syntax-ns#{l}"),
        Some(("rdfs", l)) => format!("http://www.w3.org/2000/01/rdf-schema#{l}"),
        Some(("owl", l)) => format!("http://www.w3.org/2002/07/owl#{l}"),
        _ => ctx.expand(dt).as_ref().to_string(),
    }
}

/// An object-property expression for an OBO relation id (shorthand-aware).
pub fn ope<A: ForIRI>(ctx: &Context<'_, A>, id: &str) -> ObjectPropertyExpression<A> {
    ObjectPropertyExpression::ObjectProperty(ObjectProperty(ctx.expand_rel(id)))
}

// --- whole-document finalisation -------------------------------------------

/// Apply the passes oboformat/ROBOT run over the whole document once every
/// frame is mapped: label the built-in oboInOwl/IAO properties that are used,
/// then declare every referenced-but-undeclared entity.
pub fn finalize<A: ForIRI>(
    mut comps: Vec<AnnotatedComponent<A>>,
    b: &Build<A>,
) -> Vec<AnnotatedComponent<A>> {
    let mut f = Finalizer::default();
    for ac in &comps {
        f.observe(ac);
    }
    comps.extend(f.finish(b));
    comps
}

/// The whole-document passes of [`finalize`], run incrementally so a reader
/// that streams frames need not keep them: [`observe`](Self::observe) each
/// component, then [`finish`](Self::finish) for the declarations and labels
/// still owed.
///
/// Both passes are computed over the frame-derived components only. In
/// particular, declarations are NOT recomputed over the injected built-in
/// labels: those label the meta-properties with `rdfs:label`, but that does
/// not itself make `rdfs:label` declarable — ROBOT declares `rdfs:label`
/// only when it annotates a real ontology entity (a term/typedef `name:`).
pub struct Finalizer<A: ForIRI> {
    used: BTreeSet<IRI<A>>,
    labelled: BTreeSet<IRI<A>>,
    classes: BTreeSet<IRI<A>>,
    ops: BTreeSet<IRI<A>>,
    aps: BTreeSet<IRI<A>>,
    dps: BTreeSet<IRI<A>>,
    inds: BTreeSet<IRI<A>>,
    declared_c: BTreeSet<IRI<A>>,
    declared_o: BTreeSet<IRI<A>>,
    declared_a: BTreeSet<IRI<A>>,
    declared_d: BTreeSet<IRI<A>>,
    declared_i: BTreeSet<IRI<A>>,
}

impl<A: ForIRI> Default for Finalizer<A> {
    fn default() -> Self {
        Finalizer {
            used: Default::default(),
            labelled: Default::default(),
            classes: Default::default(),
            ops: Default::default(),
            aps: Default::default(),
            dps: Default::default(),
            inds: Default::default(),
            declared_c: Default::default(),
            declared_o: Default::default(),
            declared_a: Default::default(),
            declared_d: Default::default(),
            declared_i: Default::default(),
        }
    }
}

impl<A: ForIRI> Finalizer<A> {
    pub fn observe(&mut self, ac: &AnnotatedComponent<A>) {
        self.observe_labels(ac);
        self.observe_references(ac);
    }

    /// The declarations, then the built-in labels, to add after the last
    /// component.
    pub fn finish(self, b: &Build<A>) -> Vec<AnnotatedComponent<A>> {
        let mut out = self.declarations(b);
        out.extend(self.labels(b));
        out
    }

    fn observe_labels(&mut self, ac: &AnnotatedComponent<A>) {
        for a in &ac.ann {
            collect_aps(a, &mut self.used);
        }
        match &ac.component {
            Component::AnnotationAssertion(ax) => {
                collect_aps(&ax.ann, &mut self.used);
                if ax.ann.ap.0.as_ref() == RDFS_LABEL {
                    if let AnnotationSubject::IRI(i) = &ax.subject {
                        self.labelled.insert(i.clone());
                    }
                }
            }
            Component::OntologyAnnotation(oa) => collect_aps(&oa.0, &mut self.used),
            _ => {}
        }
    }

    /// oboformat/ROBOT attach a canonical `rdfs:label` to each standard
    /// oboInOwl / IAO annotation property that is actually used (e.g.
    /// `hasExactSynonym` → "has_exact_synonym"). Seeded from owlmake's
    /// `add_oboinowl_builtin_labels`.
    fn labels(&self, b: &Build<A>) -> Vec<AnnotatedComponent<A>> {
        let table: [(String, &str); 19] = [
            (format!("{OIO}hasExactSynonym"), "has_exact_synonym"),
            (format!("{OIO}hasNarrowSynonym"), "has_narrow_synonym"),
            (format!("{OIO}hasBroadSynonym"), "has_broad_synonym"),
            (format!("{OIO}hasRelatedSynonym"), "has_related_synonym"),
            (format!("{OIO}hasSynonymType"), "has_synonym_type"),
            (format!("{OIO}hasDbXref"), "database_cross_reference"),
            (format!("{OIO}hasOBONamespace"), "has_obo_namespace"),
            (
                format!("{OIO}hasOBOFormatVersion"),
                "has_obo_format_version",
            ),
            (format!("{OIO}hasAlternativeId"), "has_alternative_id"),
            (format!("{OIO}inSubset"), "in_subset"),
            (format!("{OIO}SubsetProperty"), "subset_property"),
            (format!("{OIO}SynonymTypeProperty"), "synonym_type_property"),
            (format!("{OIO}consider"), "consider"),
            (format!("{OIO}shorthand"), "shorthand"),
            (format!("{OIO}id"), "id"),
            (format!("{OIO}created_by"), "created by"),
            (format!("{OIO}creation_date"), "creation date"),
            (IAO_DEF.to_string(), "definition"),
            (IAO_TERM_REPLACED_BY.to_string(), "term replaced by"),
        ];
        let mut out = Vec::new();
        for (iri, label) in table {
            let i = b.iri(iri.as_str());
            if self.used.contains(&i) && !self.labelled.contains(&i) {
                out.push(assertion(&i, lit_ann(b, RDFS_LABEL, label)));
            }
        }
        out
    }

    fn observe_references(&mut self, ac: &AnnotatedComponent<A>) {
        let (classes, ops, aps, dps, inds) = (
            &mut self.classes,
            &mut self.ops,
            &mut self.aps,
            &mut self.dps,
            &mut self.inds,
        );
        for a in &ac.ann {
            collect_axiom_qualifier_aps(a, aps);
        }
        match &ac.component {
            Component::DeclareClass(d) => {
                self.declared_c.insert(d.0.0.clone());
            }
            Component::DeclareObjectProperty(d) => {
                self.declared_o.insert(d.0.0.clone());
            }
            Component::DeclareAnnotationProperty(d) => {
                self.declared_a.insert(d.0.0.clone());
            }
            Component::DeclareDataProperty(d) => {
                self.declared_d.insert(d.0.0.clone());
            }
            Component::DeclareNamedIndividual(d) => {
                self.declared_i.insert(d.0.0.clone());
            }
            Component::ClassAssertion(a) => {
                walk_ce(&a.ce, classes, ops);
                named(&a.i, inds);
            }
            Component::ObjectPropertyAssertion(a) => {
                op_of(&a.ope, ops);
                named(&a.from, inds);
                named(&a.to, inds);
            }
            Component::DataPropertyAssertion(a) => {
                dps.insert(a.dp.0.clone());
                named(&a.from, inds);
            }
            Component::SubClassOf(s) => {
                walk_ce(&s.sub, classes, ops);
                walk_ce(&s.sup, classes, ops);
            }
            Component::EquivalentClasses(e) => {
                for ce in &e.0 {
                    walk_ce(ce, classes, ops);
                }
            }
            Component::DisjointClasses(d) => {
                for ce in &d.0 {
                    walk_ce(ce, classes, ops);
                }
            }
            Component::SubObjectPropertyOf(s) => {
                op_of(&s.sup, ops);
                match &s.sub {
                    SubObjectPropertyExpression::ObjectPropertyExpression(o) => op_of(o, ops),
                    SubObjectPropertyExpression::ObjectPropertyChain(v) => {
                        for o in v {
                            op_of(o, ops);
                        }
                    }
                }
            }
            Component::InverseObjectProperties(a) => {
                ops.insert(a.0.0.clone());
                ops.insert(a.1.0.clone());
            }
            Component::ObjectPropertyDomain(d) => {
                op_of(&d.ope, ops);
                walk_ce(&d.ce, classes, ops);
            }
            Component::ObjectPropertyRange(r) => {
                op_of(&r.ope, ops);
                walk_ce(&r.ce, classes, ops);
            }
            Component::TransitiveObjectProperty(a) => op_of(&a.0, ops),
            Component::SymmetricObjectProperty(a) => op_of(&a.0, ops),
            Component::ReflexiveObjectProperty(a) => op_of(&a.0, ops),
            Component::AsymmetricObjectProperty(a) => op_of(&a.0, ops),
            Component::FunctionalObjectProperty(a) => op_of(&a.0, ops),
            Component::InverseFunctionalObjectProperty(a) => op_of(&a.0, ops),
            Component::AnnotationAssertion(ax) => collect_aps(&ax.ann, aps),
            Component::OntologyAnnotation(oa) => collect_axiom_qualifier_aps(&oa.0, aps),
            Component::Rule(r) => {
                for atom in r.head.iter().chain(r.body.iter()) {
                    match atom {
                        Atom::ClassAtom { pred, arg } => {
                            walk_ce(pred, classes, ops);
                            iarg(arg, inds);
                        }
                        Atom::ObjectPropertyAtom { pred, args } => {
                            op_of(pred, ops);
                            iarg(&args.0, inds);
                            iarg(&args.1, inds);
                        }
                        Atom::DataPropertyAtom { pred, .. } => {
                            dps.insert(pred.0.clone());
                        }
                        Atom::DifferentIndividualsAtom(a, b) | Atom::SameIndividualAtom(a, b) => {
                            iarg(a, inds);
                            iarg(b, inds);
                        }
                        Atom::BuiltInAtom { .. } | Atom::DataRangeAtom { .. } => {}
                    }
                }
            }
            _ => {}
        }
    }

    /// Declare every class / object-property / annotation-property referenced
    /// by an axiom but not already declared — matching robot/oboformat, which
    /// emit a Declaration for every entity in the signature. Seeded from
    /// owlmake's `declare_referenced_entities`.
    fn declarations(&self, b: &Build<A>) -> Vec<AnnotatedComponent<A>> {
        let mut out = Vec::new();
        for c in self.classes.difference(&self.declared_c) {
            out.push(component(DeclareClass(b.class(c.clone()))));
        }
        for p in self.ops.difference(&self.declared_o) {
            out.push(component(DeclareObjectProperty(
                b.object_property(p.clone()),
            )));
        }
        for p in self.aps.difference(&self.declared_a) {
            out.push(component(DeclareAnnotationProperty(
                b.annotation_property(p.clone()),
            )));
        }
        for p in self.dps.difference(&self.declared_d) {
            out.push(component(DeclareDataProperty(b.data_property(p.clone()))));
        }
        for i in self.inds.difference(&self.declared_i) {
            out.push(component(DeclareNamedIndividual(
                b.named_individual(i.clone()),
            )));
        }
        out
    }
}

fn named<A: ForIRI>(i: &Individual<A>, inds: &mut BTreeSet<IRI<A>>) {
    if let Individual::Named(n) = i {
        inds.insert(n.0.clone());
    }
}

fn iarg<A: ForIRI>(a: &IArgument<A>, inds: &mut BTreeSet<IRI<A>>) {
    if let IArgument::Individual(i) = a {
        named(i, inds);
    }
}

/// Collect every annotation-property IRI a (possibly nested) annotation uses.
fn collect_aps<A: ForIRI>(a: &Annotation<A>, out: &mut BTreeSet<IRI<A>>) {
    out.insert(a.ap.0.clone());
    for n in &a.ann {
        collect_aps(n, out);
    }
}

/// Same as `collect_aps`, but for annotations sourced from an axiom's own
/// `{qualifier}` block (name/comment), or a header `remark:`: oboformat/ROBOT
/// never declare rdfs:label/rdfs:comment there, unlike a `name:`/
/// `property_value:` clause's own AnnotationAssertion, which does get one (see
/// `instance_frame_golden`).
fn collect_axiom_qualifier_aps<A: ForIRI>(a: &Annotation<A>, out: &mut BTreeSet<IRI<A>>) {
    if a.ap.0.as_ref() != RDFS_LABEL && a.ap.0.as_ref() != RDFS_COMMENT {
        out.insert(a.ap.0.clone());
    }
    for n in &a.ann {
        collect_axiom_qualifier_aps(n, out);
    }
}

/// Insert the object-property IRI named by an expression (ignoring `inverse(p)`
/// wrappers, which reference the same property).
fn op_of<A: ForIRI>(ope: &ObjectPropertyExpression<A>, ops: &mut BTreeSet<IRI<A>>) {
    match ope {
        ObjectPropertyExpression::ObjectProperty(p)
        | ObjectPropertyExpression::InverseObjectProperty(p) => {
            ops.insert(p.0.clone());
        }
    }
}

fn walk_ce<A: ForIRI>(
    ce: &ClassExpression<A>,
    classes: &mut BTreeSet<IRI<A>>,
    ops: &mut BTreeSet<IRI<A>>,
) {
    match ce {
        ClassExpression::Class(c) => {
            classes.insert(c.0.clone());
        }
        ClassExpression::ObjectSomeValuesFrom { ope, bce }
        | ClassExpression::ObjectAllValuesFrom { ope, bce } => {
            if let ObjectPropertyExpression::ObjectProperty(p) = ope {
                ops.insert(p.0.clone());
            }
            walk_ce(bce, classes, ops);
        }
        ClassExpression::ObjectIntersectionOf(v) | ClassExpression::ObjectUnionOf(v) => {
            for x in v {
                walk_ce(x, classes, ops);
            }
        }
        ClassExpression::ObjectComplementOf(x) => walk_ce(x, classes, ops),
        _ => {}
    }
}
