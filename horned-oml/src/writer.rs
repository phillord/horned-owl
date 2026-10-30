//! Write horned-owl components as OML.
//!
//! This inverts the mapping of the reader, as the reference `owl2oml` does:
//! the `oml:*` annotations that `oml2owl` writes say what each ontology and
//! term was, and the flat axioms are regrouped into the statements that made
//! them. OML is a subset of OWL, so some axioms have no OML form. By default
//! the writer refuses these; with [`Config::lax`] it leaves them out.

use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};

use horned_owl::model::*;
use horned_owl::ontology::set::SetOntology;
use horned_owl::vocab::Facet;

const OML: &str = "http://opencaesar.io/oml#";
const RDFS_LITERAL: &str = "http://www.w3.org/2000/01/rdf-schema#Literal";
const OWL_THING: &str = "http://www.w3.org/2002/07/owl#Thing";

/// The W3C ontologies that OML imports but `oml2owl` does not convert, with
/// their usual prefixes.
const STANDARD: [(&str, &str); 5] = [
    ("http://www.w3.org/2001/XMLSchema#", "xsd"),
    ("http://www.w3.org/1999/02/22-rdf-syntax-ns#", "rdf"),
    ("http://www.w3.org/2000/01/rdf-schema#", "rdfs"),
    ("http://www.w3.org/2002/07/owl#", "owl"),
    ("http://www.w3.org/2003/11/swrlb#", "swrlb"),
];

const KEYWORDS: &[&str] = &[
    "differentFrom",
    "maxExclusive",
    "maxInclusive",
    "minExclusive",
    "minInclusive",
    "description",
    "irreflexive",
    "annotation",
    "asymmetric",
    "functional",
    "transitive",
    "vocabulary",
    "maxLength",
    "minLength",
    "reflexive",
    "restricts",
    "symmetric",
    "includes",
    "instance",
    "language",
    "property",
    "relation",
    "builtIn",
    "builtin",
    "concept",
    "exactly",
    "extends",
    "forward",
    "inverse",
    "pattern",
    "reverse",
    "aspect",
    "bundle",
    "domain",
    "entity",
    "length",
    "sameAs",
    "scalar",
    "false",
    "oneOf",
    "range",
    "from",
    "rule",
    "self",
    "some",
    "true",
    "uses",
    "all",
    "key",
    "max",
    "min",
    "ref",
    "as",
    "to",
];

#[derive(Debug, thiserror::Error)]
pub enum WriteError {
    #[error("the ontology has no oml:type annotation, so its kind is not known")]
    NotOml,
    #[error("the ontology has no IRI")]
    NoIri,
    #[error("cannot write as OML:\n{}", .0.join("\n"))]
    Unsupported(Vec<String>),
}

/// How the OML is made.
#[derive(Clone, Copy, Debug, Default)]
pub struct Config {
    /// Leave out what OML cannot say, instead of refusing to write.
    pub lax: bool,
}

/// What the writer needs to know about an ontology that the one being
/// written imports. `iri` is the ontology IRI: the namespace without its
/// final `#` or `/`.
pub trait Imports {
    fn info(&self, iri: &str) -> Option<OntologyInfo>;
}

/// Knows no imports, so only the W3C ontologies can be referred to.
pub struct NoImports;

impl Imports for NoImports {
    fn info(&self, _: &str) -> Option<OntologyInfo> {
        None
    }
}

impl<F: Fn(&str) -> Option<OntologyInfo>> Imports for F {
    fn info(&self, iri: &str) -> Option<OntologyInfo> {
        self(iri)
    }
}

/// What an ontology says about itself with its `oml:*` annotations.
#[derive(Clone, Debug, Default)]
pub struct OntologyInfo {
    /// The namespace, with its final `#` or `/`.
    pub ns: String,
    /// `Vocabulary`, `VocabularyBundle`, `Description` or `DescriptionBundle`.
    pub kind: String,
    pub prefix: String,
    /// The `oml:type` of each term, without the namespace. A scalar, scalar
    /// property or annotation property has none, so is recorded as
    /// `Scalar`, `ScalarProperty` or `AnnotationProperty`.
    pub types: HashMap<String, String>,
    /// Relation entity to its forward relation.
    pub forwards: HashMap<String, String>,
    /// Relation entity or relation to its reverse relation.
    pub reverses: HashMap<String, String>,
}

impl OntologyInfo {
    /// None if the ontology has no `oml:type`.
    pub fn of<A: ForIRI>(o: &SetOntology<A>) -> Option<Self> {
        let mut info = OntologyInfo::default();
        let mut iri = None;
        let mut declared = vec![];
        for ac in o.iter() {
            match &ac.component {
                Component::OntologyID(OntologyID { iri: Some(i), .. }) => {
                    iri = Some(i.to_string());
                }
                Component::OntologyAnnotation(OntologyAnnotation(a)) => {
                    match (oml_name(a.ap.0.as_ref()), &a.av) {
                        (Some("type"), AnnotationValue::IRI(v)) => {
                            info.kind = v.to_string().strip_prefix(OML)?.to_string()
                        }
                        (Some("namespace"), AnnotationValue::IRI(v)) => info.ns = v.to_string(),
                        (Some("prefix"), AnnotationValue::Literal(Literal::Simple { literal })) => {
                            info.prefix = literal.clone()
                        }
                        _ => {}
                    }
                }
                Component::Rule(_) => {
                    for a in &ac.ann {
                        if let (
                            Some("name"),
                            AnnotationValue::Literal(Literal::Simple { literal }),
                        ) = (oml_name(a.ap.0.as_ref()), &a.av)
                        {
                            declared.push((literal.clone(), "Rule"));
                        }
                    }
                }
                Component::DeclareDatatype(DeclareDatatype(x)) => {
                    declared.push((x.0.to_string(), "Scalar"));
                }
                Component::DeclareDataProperty(DeclareDataProperty(x)) => {
                    declared.push((x.0.to_string(), "ScalarProperty"));
                }
                Component::DeclareAnnotationProperty(DeclareAnnotationProperty(x)) => {
                    declared.push((x.0.to_string(), "AnnotationProperty"));
                }
                Component::AnnotationAssertion(AnnotationAssertion { subject, ann }) => {
                    let AnnotationSubject::IRI(s) = subject else {
                        continue;
                    };
                    match (oml_name(ann.ap.0.as_ref()), &ann.av) {
                        (Some("type"), AnnotationValue::IRI(v)) => {
                            if let Some(t) = v.to_string().strip_prefix(OML) {
                                info.types.insert(s.to_string(), t.to_string());
                            }
                        }
                        (Some("relationEntity"), AnnotationValue::IRI(v)) => {
                            info.forwards.insert(v.to_string(), s.to_string());
                        }
                        (Some("relationBase"), AnnotationValue::IRI(v)) => {
                            info.reverses.insert(v.to_string(), s.to_string());
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
        if info.kind.is_empty() {
            return None;
        }
        for (iri, kind) in declared {
            info.types.entry(iri).or_insert_with(|| kind.to_string());
        }
        let iri = iri?;
        if info.ns.is_empty() {
            info.ns = format!("{iri}#");
        }
        if info.prefix.is_empty() {
            info.prefix = last_segment(&iri).to_string();
        }
        Some(info)
    }
}

/// Write `o` as OML.
pub fn write<A: ForIRI>(
    o: &SetOntology<A>,
    imports: &dyn Imports,
    config: Config,
) -> Result<String, WriteError> {
    let info = OntologyInfo::of(o).ok_or(WriteError::NotOml)?;
    Writer::new(o, info, imports, config).run()
}

fn oml_name(iri: &str) -> Option<&str> {
    iri.strip_prefix(OML)
}

fn namespace_of(iri: &str) -> &str {
    match iri.rfind(['#', '/']) {
        Some(i) => &iri[..=i],
        None => iri,
    }
}

fn local_name(iri: &str) -> &str {
    &iri[namespace_of(iri).len()..]
}

fn last_segment(iri: &str) -> &str {
    iri.trim_end_matches(['#', '/'])
        .rsplit(['#', '/'])
        .next()
        .unwrap_or(iri)
}

/// Whether `s` is an `idfrag` of the grammar.
fn is_idfrag(s: &str) -> bool {
    let mut chars = s.chars();
    let start = |c: char| c.is_ascii_alphanumeric() || "_-.~%".contains(c);
    chars.next().is_some_and(start) && chars.all(|c| start(c) || c == '$')
}

/// A name that can be declared: an `idfrag`, escaped if it is a keyword.
fn id(s: &str) -> Option<String> {
    if !is_idfrag(s) {
        None
    } else if KEYWORDS.contains(&s) {
        Some(format!("^{s}"))
    } else {
        Some(s.to_string())
    }
}

/// What a class declaration stands for.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum Term {
    Aspect,
    Concept,
    RelationEntity,
    Scalar,
    ScalarProperty,
    AnnotationProperty,
    Relation,
    ConceptInstance,
    RelationInstance,
    BuiltIn,
    Rule,
}

struct ImportLine {
    keyword: &'static str,
    ns: String,
    prefix: String,
}

struct Writer<'a, A: ForIRI> {
    items: Vec<&'a AnnotatedComponent<A>>,
    used: Vec<bool>,
    by_key: HashMap<String, Vec<usize>>,
    config: Config,
    imports: &'a dyn Imports,
    info: OntologyInfo,
    /// Forward and reverse relations to the relation entity or relation
    /// they belong to.
    bases: HashMap<String, String>,
    /// Namespace to the import lines written, and the problems found.
    lines: RefCell<Vec<ImportLine>>,
    problems: RefCell<Vec<String>>,
    infos: RefCell<HashMap<String, Option<OntologyInfo>>>,
    /// The numbers of the anonymous instances written, in the order written.
    anon_log: RefCell<Vec<usize>>,
    /// How deeply anonymous instances are nested where we are writing.
    depth: std::cell::Cell<usize>,
}

impl<'a, A: ForIRI> Writer<'a, A> {
    fn new(
        o: &'a SetOntology<A>,
        info: OntologyInfo,
        imports: &'a dyn Imports,
        config: Config,
    ) -> Self {
        let items: Vec<_> = o.iter().collect();
        let mut by_key: HashMap<String, Vec<usize>> = HashMap::new();
        for (i, ac) in items.iter().enumerate() {
            for key in keys(&ac.component) {
                by_key.entry(key).or_default().push(i);
            }
        }
        let used = vec![false; items.len()];
        Writer {
            items,
            used,
            by_key,
            config,
            imports,
            info,
            bases: HashMap::new(),
            lines: RefCell::new(vec![]),
            problems: RefCell::new(vec![]),
            infos: RefCell::new(HashMap::new()),
            anon_log: RefCell::new(vec![]),
            depth: std::cell::Cell::new(0),
        }
    }

    fn problem(&self, msg: impl Into<String>) {
        self.problems.borrow_mut().push(msg.into());
    }

    /// Take the unused components of `key` that `f` accepts. Components with
    /// annotations of their own cannot be written, so are never taken.
    fn grab<T>(&mut self, key: &str, f: impl Fn(&Component<A>) -> Option<T>) -> Vec<T> {
        let mut out = vec![];
        if let Some(ids) = self.by_key.get(key) {
            for &i in ids {
                if !self.used[i]
                    && self.items[i].ann.is_empty()
                    && let Some(t) = f(&self.items[i].component)
                {
                    self.used[i] = true;
                    out.push(t);
                }
            }
        }
        out
    }

    fn grab_one(&mut self, key: &str, f: impl Fn(&Component<A>) -> bool) -> bool {
        !self.grab(key, |c| f(c).then_some(())).is_empty()
    }

    // The ontology

    fn run(mut self) -> Result<String, WriteError> {
        let kind = self.info.kind.clone();
        let bundle = kind.ends_with("Bundle");
        // The ontology header is described by the markers and these.
        let mut declared_imports = vec![];
        let mut ontology_annotations = vec![];
        for i in 0..self.items.len() {
            let ac = self.items[i];
            match &ac.component {
                Component::OntologyID(_) => self.used[i] = true,
                Component::Import(Import(iri)) => {
                    self.used[i] = true;
                    declared_imports.push(iri.to_string());
                }
                Component::OntologyAnnotation(OntologyAnnotation(a)) => {
                    self.used[i] = true;
                    if oml_name(a.ap.0.as_ref()).is_none() {
                        ontology_annotations.push(a.clone());
                    }
                }
                Component::AnnotationAssertion(AnnotationAssertion { subject, ann }) => {
                    if let (AnnotationSubject::IRI(s), Some(name)) =
                        (subject, oml_name(ann.ap.0.as_ref()))
                    {
                        self.used[i] = true;
                        if let (AnnotationValue::IRI(v), "relationBase") = (&ann.av, name) {
                            self.bases.insert(s.to_string(), v.to_string());
                        }
                    }
                }
                _ => {}
            }
        }
        for iri in declared_imports {
            self.declare_import(&iri);
        }
        let annotations: Vec<String> = ontology_annotations
            .iter()
            .filter_map(|a| self.annotation(a))
            .collect();

        let body = if bundle {
            // The closure axioms are made again from the imports.
            self.used.iter_mut().for_each(|u| *u = true);
            vec![]
        } else if kind == "Vocabulary" {
            self.vocabulary()
        } else if kind == "Description" {
            self.description()
        } else {
            vec![]
        };

        for (i, ac) in self.items.iter().enumerate() {
            if !self.used[i] && !ignorable(&ac.component, &self.info.ns) {
                self.problem(format!("{:?}", ac.component));
            }
        }
        let problems = self.problems.borrow().clone();
        if !problems.is_empty() && !self.config.lax {
            return Err(WriteError::Unsupported(problems));
        }

        let mut out = String::new();
        for a in annotations {
            out += &format!("{a}\n");
        }
        let (word, bundle_word) = match kind.as_str() {
            "Vocabulary" | "VocabularyBundle" => ("vocabulary", bundle),
            _ => ("description", bundle),
        };
        out += &format!(
            "{word}{} <{}> as {} {{\n\n",
            if bundle_word { " bundle" } else { "" },
            self.info.ns,
            id(&self.info.prefix).unwrap_or_else(|| self.info.prefix.clone())
        );
        let mut lines = self.lines.borrow_mut();
        lines.sort_by(|a, b| (a.keyword, &a.ns).cmp(&(b.keyword, &b.ns)));
        for l in lines.iter() {
            out += &format!("\t{} <{}> as {}\n", l.keyword, l.ns, l.prefix);
        }
        if !lines.is_empty() && !body.is_empty() {
            out += "\n";
        }
        out += &body.join("\n");
        out += "}\n";
        Ok(out)
    }

    fn declare_import(&self, iri: &str) {
        let info = self.import_info(&format!("{iri}#"), Some(iri));
        let (ns, imported, prefix) = match &info {
            Some(i) => (i.ns.clone(), i.kind.as_str(), i.prefix.clone()),
            None => {
                self.problem(format!("cannot find the imported ontology {iri}"));
                (format!("{iri}#"), "", last_segment(iri).to_string())
            }
        };
        self.add_import(&ns, imported, prefix);
    }

    fn import_info(&self, ns: &str, iri: Option<&str>) -> Option<OntologyInfo> {
        if let Some(i) = self.infos.borrow().get(ns) {
            return i.clone();
        }
        let iri = iri.unwrap_or(&ns[..ns.len() - 1]);
        let found = self.imports.info(iri);
        self.infos
            .borrow_mut()
            .insert(ns.to_string(), found.clone());
        found
    }

    fn add_import(&self, ns: &str, imported: &str, prefix: String) -> String {
        let keyword = match (self.info.kind.as_str(), imported) {
            ("Vocabulary", "Vocabulary") => "extends",
            ("Description", "Description") => "extends",
            ("VocabularyBundle", "VocabularyBundle") => "extends",
            ("VocabularyBundle", "Vocabulary") => "includes",
            ("DescriptionBundle", "DescriptionBundle") => "extends",
            ("DescriptionBundle", "Description") => "includes",
            _ => "uses",
        };
        let mut lines = self.lines.borrow_mut();
        let own = &self.info.prefix;
        let mut unique = prefix.clone();
        let mut n = 1;
        while unique == *own || lines.iter().any(|l| l.prefix == unique && l.ns != ns) {
            n += 1;
            unique = format!("{prefix}{n}");
        }
        if let Some(l) = lines.iter().find(|l| l.ns == ns) {
            return l.prefix.clone();
        }
        lines.push(ImportLine {
            keyword,
            ns: ns.to_string(),
            prefix: unique.clone(),
        });
        unique
    }

    /// The prefix of a namespace, importing it if it is a W3C one or a
    /// known ontology.
    fn prefix_of(&self, ns: &str) -> Option<String> {
        if let Some(l) = self.lines.borrow().iter().find(|l| l.ns == ns) {
            return Some(l.prefix.clone());
        }
        if let Some((_, p)) = STANDARD.iter().find(|(n, _)| *n == ns) {
            return Some(self.add_import(ns, "Vocabulary", p.to_string()));
        }
        let info = self.import_info(ns, None)?;
        Some(self.add_import(ns, &info.kind, info.prefix.clone()))
    }

    /// A reference to the term `iri`.
    fn reference(&self, iri: &str) -> String {
        let ns = namespace_of(iri);
        let name = local_name(iri);
        if ns == self.info.ns {
            return match id(name) {
                Some(i) => i,
                None => format!("<{iri}>"),
            };
        }
        match self.prefix_of(ns) {
            Some(p) if is_idfrag(name) && is_idfrag(&p) => format!("{p}:{name}"),
            Some(_) => format!("<{iri}>"),
            None => {
                self.problem(format!("{iri} is not in an imported ontology"));
                format!("<{iri}>")
            }
        }
    }

    // Annotations and literals

    fn annotation(&self, a: &Annotation<A>) -> Option<String> {
        if !a.ann.is_empty() {
            self.problem(format!("annotation on an annotation: {a:?}"));
            return None;
        }
        let value = match &a.av {
            AnnotationValue::Literal(l) => self.literal(l),
            AnnotationValue::IRI(i) => self.reference(i.as_ref()),
            other => {
                self.problem(format!("annotation value {other:?}"));
                return None;
            }
        };
        Some(format!("@{} {}", self.reference(a.ap.0.as_ref()), value))
    }

    fn quote(&self, s: &str) -> String {
        if !s.contains(['"', '\\']) {
            format!("\"{s}\"")
        } else if !s.contains(['\'', '\\']) {
            format!("'{s}'")
        } else if !s.contains("\"\"\"") && !s.ends_with('"') {
            format!("\"\"\"{s}\"\"\"")
        } else if !s.contains("'''") && !s.ends_with('\'') {
            format!("'''{s}'''")
        } else {
            self.problem(format!("cannot quote {s:?}"));
            format!("\"{s}\"")
        }
    }

    fn literal(&self, l: &Literal<A>) -> String {
        match l {
            Literal::Simple { literal } => self.quote(literal),
            Literal::Language { literal, lang } => format!("{}${lang}", self.quote(literal)),
            Literal::Datatype {
                literal,
                datatype_iri,
            } => {
                let dt = datatype_iri.to_string();
                let bare = match dt.strip_prefix(STANDARD[0].0) {
                    Some("boolean") => literal == "true" || literal == "false",
                    Some("integer") => is_integer(literal),
                    Some("decimal") => is_decimal(literal),
                    Some("double") => {
                        is_double(literal)
                            && crate::reader::double(literal).is_ok_and(|d| d == *literal)
                    }
                    _ => false,
                };
                if bare {
                    literal.clone()
                } else {
                    format!("{}^^{}", self.quote(literal), self.reference(&dt))
                }
            }
        }
    }

    /// The annotations of `iri`, as lines.
    fn annotations_of(&mut self, iri: &str) -> Vec<String> {
        let all = self.grab(iri, |c| match c {
            Component::AnnotationAssertion(AnnotationAssertion {
                subject: AnnotationSubject::IRI(s),
                ann,
            }) if ann.ann.is_empty() => Some((s.to_string(), ann.clone())),
            _ => None,
        });
        let mut out: Vec<String> = all
            .iter()
            .filter(|(s, _)| s == iri)
            .filter_map(|(_, a)| self.annotation(a))
            .collect();
        out.sort();
        out
    }

    // Vocabulary terms

    fn vocabulary(&mut self) -> Vec<String> {
        let mut terms: BTreeMap<String, Term> = BTreeMap::new();
        for i in 0..self.items.len() {
            let c = &self.items[i].component;
            let (iri, term) = match c {
                Component::DeclareClass(DeclareClass(x)) => {
                    let iri = x.0.to_string();
                    let t = match self.info.types.get(&iri).map(String::as_str) {
                        Some("Concept") => Term::Concept,
                        Some("RelationEntity") => Term::RelationEntity,
                        _ => Term::Aspect,
                    };
                    (iri, t)
                }
                Component::DeclareDatatype(DeclareDatatype(x)) => (x.0.to_string(), Term::Scalar),
                Component::DeclareDataProperty(DeclareDataProperty(x)) => {
                    (x.0.to_string(), Term::ScalarProperty)
                }
                Component::DeclareAnnotationProperty(DeclareAnnotationProperty(x)) => {
                    (x.0.to_string(), Term::AnnotationProperty)
                }
                Component::DeclareObjectProperty(DeclareObjectProperty(x)) => {
                    let iri = x.0.to_string();
                    match self.info.types.get(&iri).map(String::as_str) {
                        // Written with their relation entity or relation.
                        Some("ForwardRelation" | "ReverseRelation") => {
                            if self.local(&iri) {
                                self.used[i] = true;
                            }
                            continue;
                        }
                        _ => (iri, Term::Relation),
                    }
                }
                _ => continue,
            };
            if self.local(&iri) {
                self.used[i] = true;
                terms.insert(iri, term);
            }
        }
        let mut out = vec![];
        let names: Vec<String> = terms.keys().cloned().collect();
        for (iri, term) in terms {
            let (idx, text) = self.tracked(|w| match term {
                Term::Aspect | Term::Concept | Term::RelationEntity => w.entity(&iri, term, false),
                Term::Scalar => w.scalar(&iri, false),
                Term::ScalarProperty => w.scalar_property(&iri, false),
                Term::AnnotationProperty => w.annotation_property(&iri, false),
                Term::Relation => w.relation(&iri, false),
                _ => None,
            });
            out.extend(text.map(|t| (idx, iri, t)));
        }
        out.extend(self.rules().into_iter().map(|(iri, t)| (None, iri, t)));
        out.extend(self.local_builtins(&names));
        let mut statements = order_statements(out);
        statements.extend(self.references_to_imported());
        statements
    }

    /// The rules, which `oml2owl` makes into SWRL rules named by `oml:name`.
    fn rules(&mut self) -> Vec<(String, String)> {
        let mut out = vec![];
        for i in 0..self.items.len() {
            let ac = self.items[i];
            let Component::Rule(rule) = &ac.component else {
                continue;
            };
            if self.used[i] {
                continue;
            }
            let name = ac
                .ann
                .iter()
                .find_map(|a| match (oml_name(a.ap.0.as_ref()), &a.av) {
                    (Some("name"), AnnotationValue::Literal(Literal::Simple { literal })) => {
                        Some(literal.clone())
                    }
                    _ => None,
                });
            let Some(iri) = name else {
                continue;
            };
            self.used[i] = true;
            let mut annotations: Vec<String> = ac
                .ann
                .iter()
                .filter(|a| oml_name(a.ap.0.as_ref()).is_none())
                .filter_map(|a| self.annotation(a))
                .collect();
            annotations.sort();
            let Some(name) = self.local(&iri).then(|| self.name(&iri)).flatten() else {
                self.problem(format!("the rule {iri} is not in this ontology"));
                continue;
            };
            let body = self.atoms(&rule.body);
            let head = self.atoms(&rule.head);
            let block = match (body, head) {
                (Some(b), Some(h)) if !b.is_empty() && !h.is_empty() => {
                    vec![format!("{} -> {}", b.join(" & "), h.join(" & "))]
                }
                (Some(b), Some(h)) if b.is_empty() && h.is_empty() => vec![],
                _ => {
                    self.problem(format!(
                        "the rule {iri} needs atoms before and after the arrow"
                    ));
                    continue;
                }
            };
            out.push((
                iri,
                self.statement(annotations, format!("rule {name}"), block, String::new()),
            ));
        }
        out
    }

    fn atoms(&self, atoms: &[Atom<A>]) -> Option<Vec<String>> {
        let has_source = format!("{OML}hasSource");
        let has_target = format!("{OML}hasTarget");
        let mut out = vec![];
        let mut i = 0;
        while i < atoms.len() {
            // The three atoms that a relation entity predicate stands for.
            if let [
                Atom::ClassAtom {
                    pred: ClassExpression::Class(entity),
                    arg,
                },
                Atom::ObjectPropertyAtom {
                    pred: ObjectPropertyExpression::ObjectProperty(p1),
                    args: (r1, source),
                },
                Atom::ObjectPropertyAtom {
                    pred: ObjectPropertyExpression::ObjectProperty(p2),
                    args: (r2, target),
                },
                ..,
            ] = &atoms[i..]
                && p1.0.as_ref() == has_source
                && p2.0.as_ref() == has_target
                && r1 == arg
                && r2 == arg
            {
                out.push(format!(
                    "{}({}, {}, {})",
                    self.reference(entity.0.as_ref()),
                    self.iargument(source)?,
                    self.iargument(arg)?,
                    self.iargument(target)?
                ));
                i += 3;
                continue;
            }
            out.push(self.atom(&atoms[i])?);
            i += 1;
        }
        Some(out)
    }

    fn atom(&self, atom: &Atom<A>) -> Option<String> {
        Some(match atom {
            Atom::BuiltInAtom { pred, args } => {
                let args: Option<Vec<String>> = args.iter().map(|a| self.dargument(a)).collect();
                format!(
                    "builtIn({}, {})",
                    self.reference(pred.as_ref()),
                    args?.join(", ")
                )
            }
            Atom::ClassAtom {
                pred: ClassExpression::Class(c),
                arg,
            } => format!("{}({})", self.reference(c.0.as_ref()), self.iargument(arg)?),
            Atom::DataRangeAtom {
                pred: DataRange::Datatype(d),
                arg,
            } => format!("{}({})", self.reference(d.0.as_ref()), self.dargument(arg)?),
            Atom::DataPropertyAtom { pred, args } => format!(
                "{}({}, {})",
                self.reference(pred.0.as_ref()),
                self.dargument(&args.0)?,
                self.dargument(&args.1)?
            ),
            Atom::ObjectPropertyAtom {
                pred: ObjectPropertyExpression::ObjectProperty(p),
                args,
            } => format!(
                "{}({}, {})",
                self.reference(p.0.as_ref()),
                self.iargument(&args.0)?,
                self.iargument(&args.1)?
            ),
            Atom::SameIndividualAtom(a, b) => {
                format!("sameAs({}, {})", self.iargument(a)?, self.iargument(b)?)
            }
            Atom::DifferentIndividualsAtom(a, b) => {
                format!(
                    "differentFrom({}, {})",
                    self.iargument(a)?,
                    self.iargument(b)?
                )
            }
            other => {
                self.problem(format!("rule atom {other:?}"));
                return None;
            }
        })
    }

    fn variable(&self, v: &Variable<A>) -> Option<String> {
        let name = v.0.as_ref().strip_prefix("urn:swrl:var#").and_then(id);
        if name.is_none() {
            self.problem(format!("rule variable {v:?}"));
        }
        name
    }

    fn iargument(&self, a: &IArgument<A>) -> Option<String> {
        match a {
            IArgument::Variable(v) => self.variable(v),
            IArgument::Individual(Individual::Named(n)) => Some(self.cross_reference(n.0.as_ref())),
            other => {
                self.problem(format!("rule argument {other:?}"));
                None
            }
        }
    }

    fn dargument(&self, a: &DArgument<A>) -> Option<String> {
        match a {
            DArgument::Variable(v) => self.variable(v),
            DArgument::Literal(l) => Some(self.literal(l)),
        }
    }

    /// A reference that cannot be a bare name, as a rule argument cannot.
    fn cross_reference(&self, iri: &str) -> String {
        let name = local_name(iri);
        if namespace_of(iri) == self.info.ns {
            if is_idfrag(name) && is_idfrag(&self.info.prefix) {
                format!("{}:{name}", self.info.prefix)
            } else {
                format!("<{iri}>")
            }
        } else {
            self.reference(iri)
        }
    }

    /// `builtin` statements: all that `oml2owl` makes of one is annotations.
    fn local_builtins(&mut self, terms: &[String]) -> Vec<(Option<usize>, String, String)> {
        let mut keys: Vec<String> = self
            .by_key
            .iter()
            .filter(|(k, ids)| self.local(k) && ids.iter().any(|&i| !self.used[i]))
            .map(|(k, _)| k.clone())
            .collect();
        keys.sort();
        let mut out = vec![];
        for k in keys {
            if terms.contains(&k) {
                continue;
            }
            let annotations = self.annotations_of(&k);
            if annotations.is_empty() {
                continue;
            }
            let Some(name) = self.name(&k) else {
                continue;
            };
            out.push((
                None,
                k,
                self.statement(
                    annotations,
                    format!("builtin {name}"),
                    vec![],
                    String::new(),
                ),
            ));
        }
        out
    }

    // Descriptions

    fn description(&mut self) -> Vec<String> {
        let mut instances: BTreeMap<String, bool> = BTreeMap::new();
        for i in 0..self.items.len() {
            if let Component::DeclareNamedIndividual(DeclareNamedIndividual(x)) =
                &self.items[i].component
            {
                let iri = x.0.to_string();
                if self.local(&iri) && !self.is_anonymous(&iri) {
                    self.used[i] = true;
                    let relation =
                        self.info.types.get(&iri).map(String::as_str) == Some("RelationInstance");
                    instances.insert(iri, relation);
                }
            }
        }
        let mut out = vec![];
        for (iri, relation) in instances {
            let (idx, text) = self.tracked(|w| w.instance(&iri, relation, false));
            out.extend(text.map(|t| (idx, iri, t)));
        }
        let mut statements = order_statements(out);
        statements.extend(self.references_to_imported());
        statements
    }

    fn is_anonymous(&self, iri: &str) -> bool {
        match self.info.types.get(iri).map(String::as_str) {
            Some(t) => t.starts_with("Anonymous"),
            None => self.local(iri) && is_anonymous_name(iri),
        }
    }

    /// Run `f`, and say which is the first anonymous instance it wrote.
    fn tracked<R>(&mut self, f: impl FnOnce(&mut Self) -> R) -> (Option<usize>, R) {
        let start = self.anon_log.borrow().len();
        let r = f(self);
        let first = self.anon_log.borrow()[start..].iter().copied().min();
        (first, r)
    }

    fn instance(&mut self, iri: &str, relation: bool, is_ref: bool) -> Option<String> {
        let annotations = self.annotations_of(iri);
        let name = self.name_or_reference(iri, is_ref)?;
        let mut types = self.grab(iri, |c| match c {
            Component::ClassAssertion(ClassAssertion {
                ce: ClassExpression::Class(c),
                i: Individual::Named(n),
            }) if n.0.as_ref() == iri => Some(c.0.to_string()),
            _ => None,
        });
        types.sort();
        let mut block = vec![];
        if relation {
            for (word, end) in [("from", "hasSource"), ("to", "hasTarget")] {
                let end = format!("{OML}{end}");
                let found = self.grab(iri, |c| match c {
                    Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                        ope: ObjectPropertyExpression::ObjectProperty(p),
                        from: Individual::Named(n),
                        to: Individual::Named(t),
                    }) if n.0.as_ref() == iri && p.0.as_ref() == end => Some(t.0.to_string()),
                    _ => None,
                });
                if !found.is_empty() {
                    block.push(format!("{word} {}", self.references(found)));
                }
            }
        }
        block.extend(self.value_lines(iri));
        let types = if types.is_empty() {
            String::new()
        } else {
            format!(" : {}", self.references(types))
        };
        let word = if relation {
            "relation instance"
        } else {
            "instance"
        };
        Some(self.statement(
            annotations,
            format!("{}{word} {name}{types}", ref_word(is_ref)),
            block,
            String::new(),
        ))
    }

    /// The property value assertions of an instance, one line for each property.
    fn value_lines(&mut self, owner: &str) -> Vec<String> {
        let mut entries: Vec<(String, Option<usize>, String)> = vec![];
        let data = self.grab(owner, |c| match c {
            Component::DataPropertyAssertion(DataPropertyAssertion {
                dp,
                from: Individual::Named(n),
                to,
            }) if n.0.as_ref() == owner => Some((dp.0.to_string(), to.clone())),
            _ => None,
        });
        for (dp, l) in data {
            entries.push((self.reference(&dp), None, self.literal(&l)));
        }
        let objects = self.grab(owner, |c| match c {
            Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                ope: ObjectPropertyExpression::ObjectProperty(p),
                from: Individual::Named(n),
                to: Individual::Named(t),
            }) if n.0.as_ref() == owner => Some((p.0.to_string(), t.0.to_string())),
            _ => None,
        });
        for (p, t) in objects {
            if self.is_anonymous(&t) {
                let (idx, value) = self.tracked(|w| w.anonymous_value(&p, &t));
                if let Some((property, value)) = value {
                    entries.push((property, idx, value));
                }
            } else {
                entries.push((self.reference(&p), None, self.reference(&t)));
            }
        }
        entries.sort_by(|a, b| (&a.0, &a.2).cmp(&(&b.0, &b.2)));
        let mut lines: Vec<(Option<usize>, String)> = vec![];
        let mut i = 0;
        while i < entries.len() {
            let mut j = i;
            while j < entries.len() && entries[j].0 == entries[i].0 {
                j += 1;
            }
            let mut values: Vec<(Option<usize>, String)> =
                entries[i..j].iter().map(|e| (e.1, e.2.clone())).collect();
            reorder(&mut values);
            let first = values.iter().filter_map(|v| v.0).min();
            let texts: Vec<String> = values.into_iter().map(|v| v.1).collect();
            lines.push((first, format!("{} {}", entries[i].0, texts.join(", "))));
            i = j;
        }
        reorder(&mut lines);
        lines.into_iter().map(|l| l.1).collect()
    }

    /// The property and value text for the anonymous instance `j` that is
    /// related to its owner by `ope`. `oml2owl` relates the owner to an
    /// anonymous relation instance by the inverse of `hasSource` or
    /// `hasTarget`, which OML writes as the forward or reverse relation.
    fn anonymous_value(&mut self, ope: &str, j: &str) -> Option<(String, String)> {
        let Some(number) = anonymous_number(j) else {
            self.problem(format!("{j} is not a numbered anonymous instance"));
            return None;
        };
        self.anon_log.borrow_mut().push(number);
        self.grab_one(j, |c| {
            matches!(c, Component::DeclareNamedIndividual(DeclareNamedIndividual(x)) if x.0.as_ref() == j)
        });
        let types = self.grab(j, |c| match c {
            Component::ClassAssertion(ClassAssertion {
                ce: ClassExpression::Class(c),
                i: Individual::Named(n),
            }) if n.0.as_ref() == j => Some(c.0.to_string()),
            _ => None,
        });
        let source_of = format!("{OML}isSourceOf");
        let target_of = format!("{OML}isTargetOf");
        let relation = ope == source_of || ope == target_of;
        let [entity] = &types[..] else {
            self.problem(format!("the anonymous instance {j} needs exactly one type"));
            return None;
        };
        let (property, value) = if relation {
            let (far, near) = if ope == source_of {
                ("hasTarget", self.forward_of(entity))
            } else {
                ("hasSource", self.reverse_of(entity))
            };
            let far = format!("{OML}{far}");
            let ends = self.grab(j, |c| match c {
                Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                    ope: ObjectPropertyExpression::ObjectProperty(p),
                    from: Individual::Named(n),
                    to: Individual::Named(t),
                }) if n.0.as_ref() == j && p.0.as_ref() == far => Some(t.0.to_string()),
                _ => None,
            });
            let (Some(near), [end]) = (near, &ends[..]) else {
                self.problem(format!("cannot write the anonymous relation instance {j}"));
                return None;
            };
            (self.reference(&near), self.reference(end))
        } else {
            (self.reference(ope), format!(": {}", self.reference(entity)))
        };
        let depth = self.depth.get();
        self.depth.set(depth + 1);
        let lines = self.value_lines(j);
        self.depth.set(depth);
        let indent = format!("\t\t{}", "\t".repeat(depth));
        let block = if lines.is_empty() {
            " []".to_string()
        } else {
            self.block(&lines, &indent)
        };
        Some((property, format!("{value}{block}")))
    }

    fn reverse_of(&self, entity: &str) -> Option<String> {
        if let Some(r) = self.info.reverses.get(entity) {
            return Some(r.clone());
        }
        self.import_info(namespace_of(entity), None)?
            .reverses
            .get(entity)
            .cloned()
    }

    /// `ref` statements for what is said about terms of other ontologies.
    fn references_to_imported(&mut self) -> Vec<String> {
        let mut keys: Vec<String> = self
            .by_key
            .iter()
            .filter(|(k, ids)| !self.local(k) && ids.iter().any(|&i| !self.used[i]))
            .map(|(k, _)| k.clone())
            .collect();
        keys.sort();
        // The forward relation of an imported entity is written with it.
        let mut targets = std::collections::BTreeSet::new();
        for k in keys {
            match self.imported_entity_of_forward(&k) {
                Some(entity) => targets.insert(entity),
                None => targets.insert(k),
            };
        }
        let mut out: Vec<(Option<usize>, String, String)> = vec![];
        for iri in targets {
            let Some(term) = self.imported_term(&iri) else {
                continue;
            };
            let instance = matches!(term, Term::ConceptInstance | Term::RelationInstance);
            if instance != (self.info.kind == "Description") {
                continue;
            }
            let (idx, text) = self.tracked(|w| match term {
                Term::Aspect | Term::Concept | Term::RelationEntity => w.entity(&iri, term, true),
                Term::Scalar => w.scalar(&iri, true),
                Term::ScalarProperty => w.scalar_property(&iri, true),
                Term::AnnotationProperty => w.annotation_property(&iri, true),
                Term::Relation => w.relation(&iri, true),
                Term::ConceptInstance => w.instance(&iri, false, true),
                Term::RelationInstance => w.instance(&iri, true, true),
                Term::BuiltIn => w.annotations_only(&iri, "builtin"),
                Term::Rule => w.annotations_only(&iri, "rule"),
            });
            out.extend(text.map(|t| (idx, iri, t)));
        }
        order_statements(out)
    }

    /// A `ref` statement for a built-in or rule of another ontology, which
    /// can only be annotated.
    fn annotations_only(&mut self, iri: &str, word: &str) -> Option<String> {
        let annotations = self.annotations_of(iri);
        if annotations.is_empty() {
            return None;
        }
        Some(self.statement(
            annotations,
            format!("ref {word} {}", self.reference(iri)),
            vec![],
            String::new(),
        ))
    }

    fn imported_entity_of_forward(&self, iri: &str) -> Option<String> {
        let info = self.import_info(namespace_of(iri), None)?;
        if info.types.get(iri).map(String::as_str) != Some("ForwardRelation") {
            return None;
        }
        info.forwards
            .iter()
            .find(|(_, f)| f.as_str() == iri)
            .map(|(e, _)| e.clone())
    }

    fn imported_term(&self, iri: &str) -> Option<Term> {
        if namespace_of(iri) == STANDARD[4].0 {
            return Some(Term::BuiltIn);
        }
        let info = self.import_info(namespace_of(iri), None)?;
        Some(match info.types.get(iri)?.as_str() {
            "Aspect" => Term::Aspect,
            "Concept" => Term::Concept,
            "RelationEntity" => Term::RelationEntity,
            "Scalar" => Term::Scalar,
            "ScalarProperty" => Term::ScalarProperty,
            "AnnotationProperty" => Term::AnnotationProperty,
            "UnreifiedRelation" => Term::Relation,
            "ConceptInstance" => Term::ConceptInstance,
            "RelationInstance" => Term::RelationInstance,
            "Rule" => Term::Rule,
            _ => return None,
        })
    }

    fn name_or_reference(&self, iri: &str, is_ref: bool) -> Option<String> {
        if is_ref {
            Some(self.reference(iri))
        } else {
            self.name(iri)
        }
    }

    fn local(&self, iri: &str) -> bool {
        iri.starts_with(&self.info.ns) && iri.len() > self.info.ns.len()
    }

    /// The name of a term declared here.
    fn name(&self, iri: &str) -> Option<String> {
        let name = id(local_name(iri));
        if name.is_none() {
            self.problem(format!("{iri} cannot be written as an OML name"));
        }
        name
    }

    /// A statement: annotations, the head, the lines of its block and the rest.
    fn statement(
        &self,
        annotations: Vec<String>,
        head: String,
        block: Vec<String>,
        tail: String,
    ) -> String {
        let mut out = String::new();
        for a in annotations {
            out += &format!("\t{a}\n");
        }
        out += &format!("\t{head}{}{tail}\n", self.block(&block, "\t"));
        out
    }

    fn block(&self, lines: &[String], indent: &str) -> String {
        if lines.is_empty() {
            return String::new();
        }
        let mut out = String::from(" [\n");
        for l in lines {
            out += &format!("{indent}\t{l}\n");
        }
        out += &format!("{indent}]");
        out
    }

    fn annotation_property(&mut self, iri: &str, is_ref: bool) -> Option<String> {
        let annotations = self.annotations_of(iri);
        let name = self.name_or_reference(iri, is_ref)?;
        Some(self.statement(
            annotations,
            format!("{}annotation property {name}", ref_word(is_ref)),
            vec![],
            String::new(),
        ))
    }

    // Scalars

    fn scalar(&mut self, iri: &str, is_ref: bool) -> Option<String> {
        let annotations = self.annotations_of(iri);
        let name = self.name_or_reference(iri, is_ref)?;
        let definitions = self.grab(iri, |c| match c {
            Component::DatatypeDefinition(DatatypeDefinition { kind, range })
                if kind.0.to_string() == iri =>
            {
                Some(range.clone())
            }
            _ => None,
        });
        let mut block = vec![];
        let mut equivalents = vec![];
        for range in definitions {
            match range {
                DataRange::DataOneOf(literals) => {
                    let mut l: Vec<String> = literals.iter().map(|l| self.literal(l)).collect();
                    l.sort();
                    block.push(format!("oneOf {}", l.join(", ")));
                }
                DataRange::Datatype(d) => equivalents.push(self.reference(d.0.as_ref())),
                DataRange::DatatypeRestriction(d, facets) => {
                    let mut fs: Vec<String> = facets.iter().filter_map(|f| self.facet(f)).collect();
                    fs.sort();
                    equivalents.push(format!(
                        "{}{}",
                        self.reference(d.0.as_ref()),
                        self.block(&fs, "\t")
                    ));
                }
                other => self.problem(format!("data range {other:?}")),
            }
        }
        equivalents.sort();
        let tail = if equivalents.is_empty() {
            String::new()
        } else {
            format!(" = {}", equivalents.join(", "))
        };
        Some(self.statement(
            annotations,
            format!("{}scalar {name}", ref_word(is_ref)),
            block,
            tail,
        ))
    }

    fn facet(&self, f: &FacetRestriction<A>) -> Option<String> {
        let value = self.literal(&f.l);
        let unsigned = |word: &str| match &f.l {
            Literal::Datatype { literal, .. } if literal.chars().all(|c| c.is_ascii_digit()) => {
                Some(format!("{word} {literal}"))
            }
            _ => {
                self.problem(format!("facet {word} needs a number: {value}"));
                None
            }
        };
        match f.f {
            Facet::Length => unsigned("length"),
            Facet::MinLength => unsigned("minLength"),
            Facet::MaxLength => unsigned("maxLength"),
            Facet::Pattern => match &f.l {
                Literal::Simple { literal } => Some(format!("pattern {}", self.quote(literal))),
                _ => None,
            },
            Facet::LangRange => match &f.l {
                Literal::Simple { literal } if is_idfrag(literal) => {
                    Some(format!("language {literal}"))
                }
                _ => {
                    self.problem(format!("facet language range {value}"));
                    None
                }
            },
            Facet::MinInclusive => Some(format!("minInclusive {value}")),
            Facet::MinExclusive => Some(format!("minExclusive {value}")),
            Facet::MaxInclusive => Some(format!("maxInclusive {value}")),
            Facet::MaxExclusive => Some(format!("maxExclusive {value}")),
            ref other => {
                self.problem(format!("facet {other:?}"));
                None
            }
        }
    }

    // Properties

    fn scalar_property(&mut self, iri: &str, is_ref: bool) -> Option<String> {
        let annotations = self.annotations_of(iri);
        let name = self.name_or_reference(iri, is_ref)?;
        let domains = self.grab(iri, |c| match c {
            Component::DataPropertyDomain(DataPropertyDomain {
                dp,
                ce: ClassExpression::Class(c),
            }) if dp.0.to_string() == iri => Some(c.0.to_string()),
            _ => None,
        });
        let ranges = self.grab(iri, |c| match c {
            Component::DataPropertyRange(DataPropertyRange {
                dp,
                dr: DataRange::Datatype(d),
            }) if dp.0.to_string() == iri => Some(d.0.to_string()),
            _ => None,
        });
        let functional = self.grab_one(iri, |c| {
            matches!(c, Component::FunctionalDataProperty(FunctionalDataProperty(dp)) if dp.0.to_string() == iri)
        });
        let supers = self.grab(iri, |c| match c {
            Component::SubDataPropertyOf(SubDataPropertyOf { sup, sub })
                if sub.0.to_string() == iri =>
            {
                Some(sup.0.to_string())
            }
            _ => None,
        });
        let equivalents = self.equivalent_properties(iri, |c| match c {
            Component::EquivalentDataProperties(EquivalentDataProperties(v)) => {
                Some(v.iter().map(|d| d.0.to_string()).collect())
            }
            _ => None,
        });
        let mut block = vec![];
        if !domains.is_empty() {
            block.push(format!("domain {}", self.references(domains)));
        }
        if !ranges.is_empty() {
            block.push(format!("range {}", self.references(ranges)));
        }
        if functional {
            block.push("functional".to_string());
        }
        let tail = self.property_tail(supers, equivalents);
        Some(self.statement(
            annotations,
            format!("{}scalar property {name}", ref_word(is_ref)),
            block,
            tail,
        ))
    }

    /// The operands other than `iri` of an equivalence of two properties.
    fn equivalent_properties(
        &mut self,
        iri: &str,
        operands: impl Fn(&Component<A>) -> Option<Vec<String>>,
    ) -> Vec<String> {
        let found = self.grab(iri, |c| operands(c).filter(|v| v.iter().any(|x| x == iri)));
        let mut out = vec![];
        for mut v in found {
            if v.len() != 2 {
                self.problem(format!("equivalent properties {v:?}"));
                continue;
            }
            let at = v.iter().position(|x| x == iri).unwrap();
            v.remove(at);
            out.extend(v);
        }
        out
    }

    fn references(&self, iris: Vec<String>) -> String {
        let mut r: Vec<String> = iris.iter().map(|i| self.reference(i)).collect();
        r.sort();
        r.join(", ")
    }

    fn property_tail(&self, supers: Vec<String>, equivalents: Vec<String>) -> String {
        let mut tail = String::new();
        if !supers.is_empty() {
            tail += &format!(" < {}", self.references(supers));
        }
        if !equivalents.is_empty() {
            tail += &format!(" = {}", self.references(equivalents));
        }
        tail
    }

    /// Domain, range and characteristics of a relation, as `from`, `to` and
    /// the flags.
    fn relation_shape(
        &mut self,
        key: &str,
        base: &str,
    ) -> (Vec<String>, Vec<String>, Vec<&'static str>) {
        let sources = self.grab(key, |c| match c {
            Component::ObjectPropertyDomain(ObjectPropertyDomain {
                ope: ObjectPropertyExpression::ObjectProperty(p),
                ce: ClassExpression::Class(c),
            }) if p.0.to_string() == base => Some(c.0.to_string()),
            _ => None,
        });
        let targets = self.grab(key, |c| match c {
            Component::ObjectPropertyRange(ObjectPropertyRange {
                ope: ObjectPropertyExpression::ObjectProperty(p),
                ce: ClassExpression::Class(c),
            }) if p.0.to_string() == base => Some(c.0.to_string()),
            _ => None,
        });
        let mut flags = vec![];
        for flag in FLAGS {
            if self.grab_one(key, |c| flag_of(c, base) == Some(flag)) {
                flags.push(flag);
            }
        }
        (sources, targets, flags)
    }

    /// The reverse relation of `base`, if there is one, with what follows
    /// from the forward relation: the inverse, the swapped domain and range,
    /// and the flags. `forward` is the property that carries them.
    fn reverse_relation(
        &mut self,
        base: &str,
        forward: &str,
        shape: &(Vec<String>, Vec<String>, Vec<&'static str>),
    ) -> Option<(Vec<String>, String)> {
        let mut reverses: Vec<String> = self
            .bases
            .iter()
            .filter(|(r, b)| b.as_str() == base && self.local(r))
            .map(|(r, _)| r.clone())
            .collect();
        reverses.sort();
        let reverse = reverses.first()?.clone();
        for extra in &reverses[1..] {
            self.problem(format!(
                "{base} has more than one reverse relation: {extra}"
            ));
        }
        for i in 0..self.items.len() {
            if let Component::DeclareObjectProperty(DeclareObjectProperty(x)) =
                &self.items[i].component
                && x.0.to_string() == reverse
            {
                self.used[i] = true;
            }
        }
        let annotations = self.annotations_of(&reverse);
        let inverse = self.grab_one(&reverse, |c| match c {
            Component::InverseObjectProperties(InverseObjectProperties(a, b)) => {
                let (a, b) = (a.0.to_string(), b.0.to_string());
                (a == reverse && b == forward) || (b == reverse && a == forward)
            }
            _ => false,
        });
        if !inverse {
            self.problem(format!("{reverse} is not the inverse of {forward}"));
        }
        let (sources, targets, flags) = shape;
        let (rs, rt, rf) = self.relation_shape(&reverse, &reverse);
        let mut want_s = targets.clone();
        let mut want_t = sources.clone();
        want_s.sort();
        want_t.sort();
        let (mut got_s, mut got_t) = (rs, rt);
        got_s.sort();
        got_t.sort();
        if want_s != got_s || want_t != got_t {
            self.problem(format!(
                "the domain and range of {reverse} are not those of {forward} swapped"
            ));
        }
        let mut want_f: Vec<&str> = flags.iter().map(|f| swapped(f)).collect();
        want_f.sort();
        let mut got_f = rf;
        got_f.sort();
        if want_f != got_f {
            self.problem(format!(
                "the characteristics of {reverse} are not those of {forward}"
            ));
        }
        Some((annotations, reverse))
    }

    fn relation(&mut self, iri: &str, is_ref: bool) -> Option<String> {
        let annotations = self.annotations_of(iri);
        let name = self.name_or_reference(iri, is_ref)?;
        let shape = self.relation_shape(iri, iri);
        let reverse = self.reverse_relation(iri, iri, &shape);
        let supers = self.grab(iri, |c| match c {
            Component::SubObjectPropertyOf(SubObjectPropertyOf {
                sup: ObjectPropertyExpression::ObjectProperty(sup),
                sub:
                    SubObjectPropertyExpression::ObjectPropertyExpression(
                        ObjectPropertyExpression::ObjectProperty(sub),
                    ),
            }) if sub.0.to_string() == iri => Some(sup.0.to_string()),
            _ => None,
        });
        let equivalents = self.equivalent_properties(iri, |c| match c {
            Component::EquivalentObjectProperties(EquivalentObjectProperties(v)) => Some(
                v.iter()
                    .filter_map(|p| match p {
                        ObjectPropertyExpression::ObjectProperty(p) => Some(p.0.to_string()),
                        _ => None,
                    })
                    .collect(),
            ),
            _ => None,
        });
        let (sources, targets, flags) = shape;
        let mut block = vec![];
        if !sources.is_empty() {
            block.push(format!("from {}", self.references(sources)));
        }
        if !targets.is_empty() {
            block.push(format!("to {}", self.references(targets)));
        }
        if let Some((ann, r)) = reverse {
            for a in ann {
                block.push(a);
            }
            block.push(format!("reverse {}", self.name(&r)?));
        }
        block.extend(flags.iter().map(|f| f.to_string()));
        let tail = self.property_tail(supers, equivalents);
        Some(self.statement(
            annotations,
            format!("{}relation {name}", ref_word(is_ref)),
            block,
            tail,
        ))
    }

    // Entities

    fn entity(&mut self, iri: &str, term: Term, is_ref: bool) -> Option<String> {
        let annotations = self.annotations_of(iri);
        let name = self.name_or_reference(iri, is_ref)?;
        let mut block = vec![];

        let mut forward_shape = None;
        let mut forward_iri = None;
        if term == Term::RelationEntity {
            let forward = self.forward_of(iri);
            match forward {
                Some(f) => {
                    let shape = self.relation_shape(&f, &f);
                    forward_iri = Some(f);
                    forward_shape = Some(shape);
                }
                None => self.problem(format!("{iri} has no forward relation")),
            }
        }

        let enumeration = self.instance_enumeration(iri, term);
        if !enumeration.is_empty() {
            block.push(format!("oneOf {}", self.references(enumeration)));
        }

        if let (Some(forward), Some(shape)) = (forward_iri.clone(), &forward_shape) {
            let (sources, targets, flags) = shape;
            if !sources.is_empty() {
                block.push(format!("from {}", self.references(sources.clone())));
            }
            if !targets.is_empty() {
                block.push(format!("to {}", self.references(targets.clone())));
            }
            // The forward and reverse relations of an imported entity are not ours to name.
            if !is_ref {
                let forward_annotations = self.annotations_of(&forward);
                block.extend(forward_annotations);
                block.push(format!("forward {}", self.name(&forward)?));
                if let Some((ann, r)) = self.reverse_relation(iri, &forward, shape) {
                    block.extend(ann);
                    block.push(format!("reverse {}", self.name(&r)?));
                }
            }
            block.extend(flags.iter().map(|f| f.to_string()));
            if !is_ref {
                self.derivation_rule(iri, &forward);
            }
        }

        let keys = self.grab(iri, |c| match c {
            Component::HasKey(HasKey {
                ce: ClassExpression::Class(c),
                vpe,
            }) if c.0.to_string() == iri => Some(vpe.clone()),
            _ => None,
        });
        let mut key_lines = vec![];
        for vpe in keys {
            let mut names: Vec<String> = vpe
                .iter()
                .filter_map(|p| match p {
                    PropertyExpression::ObjectPropertyExpression(
                        ObjectPropertyExpression::ObjectProperty(p),
                    ) => Some(self.reference(p.0.as_ref())),
                    PropertyExpression::DataProperty(p) => Some(self.reference(p.0.as_ref())),
                    PropertyExpression::AnnotationProperty(p) => Some(self.reference(p.0.as_ref())),
                    other => {
                        self.problem(format!("key property {other:?}"));
                        None
                    }
                })
                .collect();
            names.sort();
            key_lines.push(format!("key {}", names.join(", ")));
        }
        key_lines.sort();
        block.extend(key_lines);

        let sups = self.grab(iri, |c| match c {
            Component::SubClassOf(SubClassOf {
                sub: ClassExpression::Class(sub),
                sup,
            }) if sub.0.to_string() == iri => Some(sup.clone()),
            _ => None,
        });
        let mut supers = vec![];
        let mut restrictions = vec![];
        for sup in sups {
            match sup {
                ClassExpression::Class(c) => {
                    let c = c.0.to_string();
                    if let Some(f) = &forward_iri
                        && let Some(sup_forward) = self.forward_of(&c)
                    {
                        self.grab_one(f, |x| {
                            matches!(x, Component::SubObjectPropertyOf(SubObjectPropertyOf {
                                sup: ObjectPropertyExpression::ObjectProperty(s),
                                sub: SubObjectPropertyExpression::ObjectPropertyExpression(
                                    ObjectPropertyExpression::ObjectProperty(b)),
                            }) if s.0.to_string() == sup_forward && b.0.to_string() == *f)
                        });
                    }
                    supers.push(c);
                }
                other => {
                    let (idx, r) = self.tracked(|w| w.restriction(&other));
                    if let Some(r) = r {
                        restrictions.push((idx, r));
                    }
                }
            }
        }
        restrictions.sort_by(|a, b| a.1.cmp(&b.1));
        reorder(&mut restrictions);
        let restrictions: Vec<String> = restrictions.into_iter().map(|r| r.1).collect();
        let mut tail = String::new();
        if !supers.is_empty() || !restrictions.is_empty() {
            tail += " <";
            if !supers.is_empty() {
                tail += &format!(" {}", self.references(supers));
            }
            if !restrictions.is_empty() {
                tail += &self.block(&restrictions, "\t");
            }
        }

        let equivalences = self.grab(iri, |c| match c {
            Component::EquivalentClasses(EquivalentClasses(v))
                if v.iter()
                    .any(|x| matches!(x, ClassExpression::Class(c) if c.0.to_string() == iri)) =>
            {
                Some(v.clone())
            }
            _ => None,
        });
        let mut axioms = vec![];
        for mut v in equivalences {
            if v.len() != 2 {
                self.problem(format!("equivalent classes with {} operands", v.len()));
                continue;
            }
            let at = v
                .iter()
                .position(|x| matches!(x, ClassExpression::Class(c) if c.0.to_string() == iri))
                .unwrap();
            v.remove(at);
            let other = v.remove(0);
            if let Some(a) = self.equivalence_axiom(&other) {
                axioms.push(a);
            }
        }
        axioms.sort();
        if !axioms.is_empty() {
            tail += &format!(" = {}", axioms.join(", "));
        }

        let word = match term {
            Term::Aspect => "aspect",
            Term::Concept => "concept",
            _ => "relation entity",
        };
        Some(self.statement(
            annotations,
            format!("{}{word} {name}", ref_word(is_ref)),
            block,
            tail,
        ))
    }

    /// The individuals of `oneOf` for a concept: the other operand of an
    /// equivalence with an enumeration.
    fn instance_enumeration(&mut self, iri: &str, term: Term) -> Vec<String> {
        if term != Term::Concept {
            return vec![];
        }
        let found = self.grab(iri, |c| match c {
            Component::EquivalentClasses(EquivalentClasses(v))
                if v.len() == 2
                    && v.iter().any(
                        |x| matches!(x, ClassExpression::Class(c) if c.0.to_string() == iri),
                    ) =>
            {
                v.iter().find_map(|x| match x {
                    ClassExpression::ObjectOneOf(is) => Some(is.clone()),
                    _ => None,
                })
            }
            _ => None,
        });
        let mut out = vec![];
        for individuals in found {
            for i in individuals {
                match i {
                    Individual::Named(n) => out.push(n.0.to_string()),
                    other => self.problem(format!("individual {other:?}")),
                }
            }
        }
        out
    }

    fn forward_of(&self, entity: &str) -> Option<String> {
        if let Some(f) = self.info.forwards.get(entity) {
            return Some(f.clone());
        }
        let ns = namespace_of(entity);
        self.import_info(ns, None)?.forwards.get(entity).cloned()
    }

    /// The rule that makes the forward relation hold between the source and
    /// target of an instance of the entity.
    fn derivation_rule(&mut self, entity: &str, forward: &str) {
        let found = self.grab_rule(forward, entity);
        if !found {
            self.problem(format!("no derivation rule for {entity}"));
        }
    }

    fn grab_rule(&mut self, forward: &str, entity: &str) -> bool {
        // Rules are keyed by the property of their head.
        let ids = self.by_key.get(forward).cloned().unwrap_or_default();
        for i in ids {
            if self.used[i] {
                continue;
            }
            if let Component::Rule(horned_owl::model::Rule { head, body }) =
                &self.items[i].component
            {
                let class = body.iter().any(|a| {
                    matches!(a, Atom::ClassAtom { pred: ClassExpression::Class(c), .. }
                        if c.0.to_string() == entity)
                });
                let ends = body
                    .iter()
                    .filter(|a| {
                        matches!(a, Atom::ObjectPropertyAtom {
                            pred: ObjectPropertyExpression::ObjectProperty(p), ..
                        } if p.0.to_string().starts_with(OML))
                    })
                    .count();
                if class && ends == 2 && body.len() == 3 && head.len() == 1 {
                    self.used[i] = true;
                    return true;
                }
            }
        }
        false
    }

    /// One side of an equivalence: classes joined with `&`, then restrictions.
    fn equivalence_axiom(&mut self, other: &ClassExpression<A>) -> Option<String> {
        let parts: Vec<&ClassExpression<A>> = match other {
            ClassExpression::ObjectIntersectionOf(v) => v.iter().collect(),
            single => vec![single],
        };
        let mut classes = vec![];
        let mut restrictions = vec![];
        for p in parts {
            match p {
                ClassExpression::Class(c) => classes.push(c.0.to_string()),
                other => {
                    let (idx, r) = self.tracked(|w| w.restriction(other));
                    restrictions.push((idx, r?));
                }
            }
        }
        classes.sort();
        restrictions.sort_by(|a, b| a.1.cmp(&b.1));
        reorder(&mut restrictions);
        let restrictions: Vec<String> = restrictions.into_iter().map(|r| r.1).collect();
        let mut out: Vec<String> = classes.iter().map(|c| self.reference(c)).collect();
        out.sort();
        let mut text = out.join(" & ");
        let block = self.block(&restrictions, "\t");
        if !text.is_empty() && !block.is_empty() {
            text += &block;
        } else if !block.is_empty() {
            text = block.trim_start().to_string();
        }
        Some(text)
    }

    // Restrictions

    fn restriction(&mut self, ce: &ClassExpression<A>) -> Option<String> {
        if let ClassExpression::ObjectHasValue {
            ope,
            i: Individual::Named(n),
        } = ce
            && self.is_anonymous(n.0.as_ref())
        {
            let ObjectPropertyExpression::ObjectProperty(p) = ope else {
                self.problem("restriction on an inverse property");
                return None;
            };
            let (property, value) = self.anonymous_value(p.0.as_ref(), n.0.as_ref())?;
            return Some(format!("restricts {property} to {value}"));
        }
        self.plain_restriction(ce)
    }

    fn plain_restriction(&self, ce: &ClassExpression<A>) -> Option<String> {
        let p = |ope: &ObjectPropertyExpression<A>| match ope {
            ObjectPropertyExpression::ObjectProperty(p) => Some(self.reference(p.0.as_ref())),
            _ => {
                self.problem("restriction on an inverse property");
                None
            }
        };
        let dp = |d: &DataProperty<A>| self.reference(d.0.as_ref());
        let class = |c: &ClassExpression<A>| match c {
            ClassExpression::Class(c) => Some(c.0.to_string()),
            _ => {
                self.problem(format!("restriction to {c:?}"));
                None
            }
        };
        let range = |dr: &DataRange<A>| match dr {
            DataRange::Datatype(d) => Some(d.0.to_string()),
            _ => {
                self.problem(format!("restriction to {dr:?}"));
                None
            }
        };
        let cardinality = |word: &str, n: &u32, range: Option<String>| match range {
            Some(r) => format!("{word} {n} {}", self.reference(&r)),
            None => format!("{word} {n}"),
        };
        let (name, to) = match ce {
            ClassExpression::ObjectSomeValuesFrom { ope, bce } => {
                (format!("some {}", p(ope)?), self.reference(&class(bce)?))
            }
            ClassExpression::ObjectAllValuesFrom { ope, bce } => {
                (format!("all {}", p(ope)?), self.reference(&class(bce)?))
            }
            ClassExpression::DataSomeValuesFrom { dp: d, dr } => {
                (format!("some {}", dp(d)), self.reference(&range(dr)?))
            }
            ClassExpression::DataAllValuesFrom { dp: d, dr } => {
                (format!("all {}", dp(d)), self.reference(&range(dr)?))
            }
            ClassExpression::ObjectHasSelf(ope) => {
                return Some(format!("restricts {} to self", p(ope)?));
            }
            ClassExpression::ObjectHasValue { ope, i } => {
                return match i {
                    Individual::Named(n) if !is_anonymous_name(n.0.as_ref()) => Some(format!(
                        "restricts {} to {}",
                        p(ope)?,
                        self.reference(n.0.as_ref())
                    )),
                    other => {
                        self.problem(format!("restriction to the individual {other:?}"));
                        None
                    }
                };
            }
            ClassExpression::DataHasValue { dp: d, l } => {
                return Some(format!("restricts {} to {}", dp(d), self.literal(l)));
            }
            ClassExpression::ObjectExactCardinality { n, ope, bce }
            | ClassExpression::ObjectMinCardinality { n, ope, bce }
            | ClassExpression::ObjectMaxCardinality { n, ope, bce } => {
                let word = match ce {
                    ClassExpression::ObjectExactCardinality { .. } => "exactly",
                    ClassExpression::ObjectMinCardinality { .. } => "min",
                    _ => "max",
                };
                let c = class(bce)?;
                let r = (c != OWL_THING).then_some(c);
                return Some(format!(
                    "restricts {} to {}",
                    p(ope)?,
                    cardinality(word, n, r)
                ));
            }
            ClassExpression::DataExactCardinality { n, dp: d, dr }
            | ClassExpression::DataMinCardinality { n, dp: d, dr }
            | ClassExpression::DataMaxCardinality { n, dp: d, dr } => {
                let word = match ce {
                    ClassExpression::DataExactCardinality { .. } => "exactly",
                    ClassExpression::DataMinCardinality { .. } => "min",
                    _ => "max",
                };
                let r = range(dr)?;
                let r = (r != RDFS_LITERAL).then_some(r);
                return Some(format!(
                    "restricts {} to {}",
                    dp(d),
                    cardinality(word, n, r)
                ));
            }
            other => {
                self.problem(format!("class expression {other:?}"));
                return None;
            }
        };
        Some(format!("restricts {name} to {to}"))
    }
}

/// Put the items that contain anonymous instances in the order of the numbers
/// of their instances, as `oml2owl` numbers them in the order that it reads
/// them, leaving the other items where they are.
fn reorder<T: Clone>(items: &mut [(Option<usize>, T)]) {
    let slots: Vec<usize> = (0..items.len()).filter(|&i| items[i].0.is_some()).collect();
    let mut moved: Vec<_> = slots.iter().map(|&i| items[i].clone()).collect();
    moved.sort_by_key(|m| m.0);
    for (slot, m) in slots.into_iter().zip(moved) {
        items[slot] = m;
    }
}

/// Statements by name, except that those with anonymous instances are in
/// the order of the instances.
fn order_statements(mut statements: Vec<(Option<usize>, String, String)>) -> Vec<String> {
    statements.sort_by(|a, b| a.1.cmp(&b.1));
    let mut keyed: Vec<(Option<usize>, String)> =
        statements.into_iter().map(|s| (s.0, s.2)).collect();
    reorder(&mut keyed);
    keyed.into_iter().map(|s| s.1).collect()
}

fn anonymous_number(iri: &str) -> Option<usize> {
    local_name(iri).strip_prefix("_b")?.parse().ok()
}

fn ref_word(is_ref: bool) -> &'static str {
    if is_ref { "ref " } else { "" }
}

fn is_anonymous_name(iri: &str) -> bool {
    local_name(iri)
        .strip_prefix("_b")
        .is_some_and(|n| !n.is_empty() && n.chars().all(|c| c.is_ascii_digit()))
}

fn is_integer(s: &str) -> bool {
    let d = s.strip_prefix('-').unwrap_or(s);
    !d.is_empty() && d.chars().all(|c| c.is_ascii_digit())
}

fn is_decimal(s: &str) -> bool {
    let d = s.strip_prefix('-').unwrap_or(s);
    match d.split_once('.') {
        Some((i, f)) => {
            !i.is_empty()
                && !f.is_empty()
                && i.chars().all(|c| c.is_ascii_digit())
                && f.chars().all(|c| c.is_ascii_digit())
        }
        None => false,
    }
}

fn is_double(s: &str) -> bool {
    let d = s.strip_prefix('-').unwrap_or(s);
    let Some((m, e)) = d.split_once(['e', 'E']) else {
        return false;
    };
    let e = e.strip_prefix(['+', '-']).unwrap_or(e);
    let (i, f) = m.split_once('.').unwrap_or((m, ""));
    !e.is_empty()
        && e.chars().all(|c| c.is_ascii_digit())
        && (!i.is_empty() || !f.is_empty())
        && i.chars().all(|c| c.is_ascii_digit())
        && f.chars().all(|c| c.is_ascii_digit())
}

/// The characteristics of a relation, in the order they are written.
const FLAGS: [&str; 7] = [
    "functional",
    "inverse functional",
    "symmetric",
    "asymmetric",
    "reflexive",
    "irreflexive",
    "transitive",
];

fn swapped(flag: &str) -> &str {
    match flag {
        "functional" => "inverse functional",
        "inverse functional" => "functional",
        other => other,
    }
}

fn flag_of<A: ForIRI>(c: &Component<A>, iri: &str) -> Option<&'static str> {
    let (flag, p) = match c {
        Component::FunctionalObjectProperty(FunctionalObjectProperty(p)) => ("functional", p),
        Component::InverseFunctionalObjectProperty(InverseFunctionalObjectProperty(p)) => {
            ("inverse functional", p)
        }
        Component::SymmetricObjectProperty(SymmetricObjectProperty(p)) => ("symmetric", p),
        Component::AsymmetricObjectProperty(AsymmetricObjectProperty(p)) => ("asymmetric", p),
        Component::ReflexiveObjectProperty(ReflexiveObjectProperty(p)) => ("reflexive", p),
        Component::IrreflexiveObjectProperty(IrreflexiveObjectProperty(p)) => ("irreflexive", p),
        Component::TransitiveObjectProperty(TransitiveObjectProperty(p)) => ("transitive", p),
        _ => return None,
    };
    match p {
        ObjectPropertyExpression::ObjectProperty(p) if p.0.to_string() == iri => Some(flag),
        _ => None,
    }
}

/// Components that say nothing about this ontology: declarations of terms
/// from elsewhere, which the OWL API adds for everything that is used.
fn ignorable<A: ForIRI>(c: &Component<A>, ns: &str) -> bool {
    // Made for anonymous relation instances, whether or not any are left.
    if let Component::InverseObjectProperties(InverseObjectProperties(a, b)) = c {
        return a.0.as_ref().starts_with(OML) && b.0.as_ref().starts_with(OML);
    }
    let iri = match c {
        Component::DeclareClass(DeclareClass(x)) => x.0.to_string(),
        Component::DeclareObjectProperty(DeclareObjectProperty(x)) => x.0.to_string(),
        Component::DeclareDataProperty(DeclareDataProperty(x)) => x.0.to_string(),
        Component::DeclareAnnotationProperty(DeclareAnnotationProperty(x)) => x.0.to_string(),
        Component::DeclareDatatype(DeclareDatatype(x)) => x.0.to_string(),
        Component::DeclareNamedIndividual(DeclareNamedIndividual(x)) => x.0.to_string(),
        _ => return false,
    };
    !iri.starts_with(ns)
}

/// The names by which the components that concern a term can be found.
fn keys<A: ForIRI>(c: &Component<A>) -> Vec<String> {
    let ce = |c: &ClassExpression<A>| match c {
        ClassExpression::Class(c) => Some(c.0.to_string()),
        _ => None,
    };
    let ope = |p: &ObjectPropertyExpression<A>| match p {
        ObjectPropertyExpression::ObjectProperty(p) => Some(p.0.to_string()),
        _ => None,
    };
    match c {
        Component::AnnotationAssertion(AnnotationAssertion {
            subject: AnnotationSubject::IRI(s),
            ..
        }) => vec![s.to_string()],
        Component::SubClassOf(SubClassOf { sub, .. }) => ce(sub).into_iter().collect(),
        Component::EquivalentClasses(EquivalentClasses(v)) => v.iter().filter_map(ce).collect(),
        Component::HasKey(HasKey { ce: c, .. }) => ce(c).into_iter().collect(),
        Component::SubObjectPropertyOf(SubObjectPropertyOf {
            sub: SubObjectPropertyExpression::ObjectPropertyExpression(p),
            ..
        }) => ope(p).into_iter().collect(),
        Component::EquivalentObjectProperties(EquivalentObjectProperties(v)) => {
            v.iter().filter_map(ope).collect()
        }
        Component::InverseObjectProperties(InverseObjectProperties(a, b)) => {
            vec![a.0.to_string(), b.0.to_string()]
        }
        Component::ObjectPropertyDomain(ObjectPropertyDomain { ope: p, .. }) => {
            ope(p).into_iter().collect()
        }
        Component::ObjectPropertyRange(ObjectPropertyRange { ope: p, .. }) => {
            ope(p).into_iter().collect()
        }
        Component::FunctionalObjectProperty(FunctionalObjectProperty(p))
        | Component::InverseFunctionalObjectProperty(InverseFunctionalObjectProperty(p))
        | Component::SymmetricObjectProperty(SymmetricObjectProperty(p))
        | Component::AsymmetricObjectProperty(AsymmetricObjectProperty(p))
        | Component::ReflexiveObjectProperty(ReflexiveObjectProperty(p))
        | Component::IrreflexiveObjectProperty(IrreflexiveObjectProperty(p))
        | Component::TransitiveObjectProperty(TransitiveObjectProperty(p)) => {
            ope(p).into_iter().collect()
        }
        Component::DataPropertyDomain(DataPropertyDomain { dp, .. }) => vec![dp.0.to_string()],
        Component::DataPropertyRange(DataPropertyRange { dp, .. }) => vec![dp.0.to_string()],
        Component::FunctionalDataProperty(FunctionalDataProperty(dp)) => vec![dp.0.to_string()],
        Component::SubDataPropertyOf(SubDataPropertyOf { sub, .. }) => vec![sub.0.to_string()],
        Component::EquivalentDataProperties(EquivalentDataProperties(v)) => {
            v.iter().map(|d| d.0.to_string()).collect()
        }
        Component::DatatypeDefinition(DatatypeDefinition { kind, .. }) => {
            vec![kind.0.to_string()]
        }
        Component::DeclareNamedIndividual(DeclareNamedIndividual(x)) => vec![x.0.to_string()],
        Component::ClassAssertion(ClassAssertion {
            i: Individual::Named(n),
            ..
        }) => vec![n.0.to_string()],
        Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
            from: Individual::Named(n),
            ..
        }) => vec![n.0.to_string()],
        Component::DataPropertyAssertion(DataPropertyAssertion {
            from: Individual::Named(n),
            ..
        }) => vec![n.0.to_string()],
        Component::Rule(horned_owl::model::Rule { head, .. }) => head
            .iter()
            .filter_map(|a| match a {
                Atom::ObjectPropertyAtom {
                    pred: ObjectPropertyExpression::ObjectProperty(p),
                    ..
                } => Some(p.0.to_string()),
                _ => None,
            })
            .collect(),
        _ => vec![],
    }
}
