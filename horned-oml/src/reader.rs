//! Read OML into horned-owl components.
//!
//! The mapping is that of the reference `oml2owl` tool, which defines OML's
//! semantics by translation to OWL.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

use horned_owl::model::*;
use horned_owl::vocab::Facet;
use pest::Parser;
use pest::iterators::Pair;

use crate::catalog::{CatalogResolver, find_catalog};
use crate::closure::{Ce, Taxonomy};
use crate::parser::{OmlParser, Rule};
use curie::PrefixMapping;

const OML: &str = "http://opencaesar.io/oml#";
const XSD: &str = "http://www.w3.org/2001/XMLSchema#";

/// Imports of these are not carried into the OWL.
pub(crate) const BUILT_IN: [&str; 5] = [
    "http://www.w3.org/2001/XMLSchema",
    "http://www.w3.org/1999/02/22-rdf-syntax-ns",
    "http://www.w3.org/2000/01/rdf-schema",
    "http://www.w3.org/2002/07/owl",
    "http://www.w3.org/2003/11/swrlb",
];

#[derive(Debug, thiserror::Error)]
pub enum OmlError {
    #[error("OML syntax error: {0}")]
    Parse(Box<pest::error::Error<Rule>>),
    #[error("{0}")]
    Invalid(String),
}

impl From<pest::error::Error<Rule>> for OmlError {
    fn from(e: pest::error::Error<Rule>) -> Self {
        OmlError::Parse(Box::new(e))
    }
}

fn invalid<T>(msg: impl Into<String>) -> Result<T, OmlError> {
    Err(OmlError::Invalid(msg.into()))
}

/// What a term is, which decides how it is used elsewhere.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Kind {
    Aspect,
    Concept,
    RelationEntity,
    Scalar,
    ScalarProperty,
    AnnotationProperty,
    Relation,
}

/// Finds the OML source of an imported ontology.
///
/// Terms from other ontologies can only be used once it is known what they
/// are, for instance whether a property is a scalar property.
pub trait Resolver {
    /// `iri` is the ontology IRI: its namespace without the final `#` or `/`.
    fn load(&self, iri: &str) -> Option<String>;
}

/// Resolves nothing, so only terms declared in the document are known.
pub struct NoResolver;

impl Resolver for NoResolver {
    fn load(&self, _: &str) -> Option<String> {
        None
    }
}

impl<F: Fn(&str) -> Option<String>> Resolver for F {
    fn load(&self, iri: &str) -> Option<String> {
        self(iri)
    }
}

/// What is known about the terms of an ontology.
#[derive(Default)]
pub(crate) struct Terms {
    pub(crate) kinds: HashMap<String, Kind>,
    /// Every member the ontology declares, whatever its kind.
    pub(crate) names: HashSet<String>,
    /// Relation entity to the IRI of its forward relation.
    forwards: HashMap<String, String>,
    /// A forward or reverse relation to its relation entity, and whether it
    /// is the forward one.
    relations: HashMap<String, (String, bool)>,
    /// Pairs of an entity and one that it specialises or is equivalent to.
    supers: Vec<(String, String)>,
    /// Whether the ontology is a vocabulary.
    vocabulary: bool,
    /// The kind of ontology: `Vocabulary`, `VocabularyBundle`, `Description` or
    /// `DescriptionBundle`.
    pub(crate) ontology: &'static str,
    /// The namespaces it imports.
    imports: Vec<String>,
}

/// Read an OML document into an ontology.
pub fn read<A, O>(text: &str, build: &Build<A>) -> Result<O, OmlError>
where
    A: ForIRI,
    O: MutableOntology<A> + Default,
{
    read_with(text, build, &NoResolver)
}

/// Read an OML document into an ontology, finding imports with `resolver`.
pub fn read_with<A, O>(text: &str, build: &Build<A>, resolver: &dyn Resolver) -> Result<O, OmlError>
where
    A: ForIRI,
    O: MutableOntology<A> + Default,
{
    read_full(text, build, resolver).map(|(o, _)| o)
}

/// Read the OML file at `path`. Imports are found with the nearest
/// `catalog.xml` above it, if there is one.
pub fn read_path<A, O>(path: &Path, build: &Build<A>) -> Result<(O, PrefixMapping), OmlError>
where
    A: ForIRI,
    O: MutableOntology<A> + Default,
{
    let text = std::fs::read_to_string(path).map_err(|e| OmlError::Invalid(e.to_string()))?;
    match find_catalog(path).map(CatalogResolver::from_path) {
        Some(Ok(resolver)) => read_full(&text, build, &resolver),
        Some(Err(e)) => Err(OmlError::Invalid(format!("catalog: {e}"))),
        None => read_full(&text, build, &NoResolver),
    }
}

/// As [`read_with`], also returning the prefixes of the ontology and its
/// imports.
pub fn read_full<A, O>(
    text: &str,
    build: &Build<A>,
    resolver: &dyn Resolver,
) -> Result<(O, PrefixMapping), OmlError>
where
    A: ForIRI,
    O: MutableOntology<A> + Default,
{
    let oml = OmlParser::parse(Rule::oml, text)?.next().unwrap();
    let ontology = oml.into_inner().next().unwrap();
    let mut reader = Reader {
        build,
        resolver,
        ns: String::new(),
        prefixes: HashMap::new(),
        terms: Terms::default(),
        imported: RefCell::new(HashMap::new()),
        anonymous: 0,
        out: O::default(),
    };
    reader.ontology(ontology)?;
    let mut mapping = PrefixMapping::default();
    for (prefix, namespace) in &reader.prefixes {
        // A prefix that curie will not take is left out.
        let _ = mapping.add_prefix(prefix, namespace);
    }
    Ok((reader.out, mapping))
}

struct Reader<'a, A: ForIRI, O> {
    build: &'a Build<A>,
    resolver: &'a dyn Resolver,
    /// The namespace, with its trailing `#` or `/`.
    ns: String,
    /// Prefix to namespace, for this ontology and its imports.
    prefixes: HashMap<String, String>,
    /// The terms declared in this ontology.
    terms: Terms,
    /// The terms of other ontologies, by namespace, loaded as needed.
    imported: RefCell<HashMap<String, Option<Rc<Terms>>>>,
    /// The number of anonymous individuals made so far.
    anonymous: usize,
    out: O,
}

/// The parts of an ontology that are the same whatever is done with it.
pub(crate) struct Header<'i> {
    pub(crate) annotations: Vec<Pair<'i, Rule>>,
    pub(crate) kind: &'static str,
    pub(crate) ns: String,
    pub(crate) prefix: String,
    /// Namespace and prefix of each import.
    pub(crate) imports: Vec<(String, Option<String>)>,
    pub(crate) statements: Vec<Pair<'i, Rule>>,
}

pub(crate) fn header(pair: Pair<Rule>) -> Header {
    let mut annotations = vec![];
    let mut body = None;
    for p in pair.into_inner() {
        match p.as_rule() {
            Rule::annotation => annotations.push(p),
            _ => body = Some(p),
        }
    }
    let body = body.unwrap();
    let vocabulary = body.as_rule() == Rule::vocabulary_box;
    let mut bundle = false;
    let mut ns = String::new();
    let mut prefix = String::new();
    let mut imports = vec![];
    let mut statements = vec![];
    for p in body.into_inner() {
        match p.as_rule() {
            Rule::kw_vocabulary | Rule::kw_description | Rule::kw_as => {}
            Rule::kw_bundle => bundle = true,
            Rule::namespace => ns = p.as_str()[1..p.as_str().len() - 1].to_string(),
            Rule::id => prefix = unescape_id(p.as_str()).to_string(),
            Rule::import => {
                let mut ns = String::new();
                let mut prefix = None;
                for x in p.into_inner() {
                    match x.as_rule() {
                        Rule::namespace => ns = x.as_str()[1..x.as_str().len() - 1].to_string(),
                        Rule::id => prefix = Some(unescape_id(x.as_str()).to_string()),
                        _ => {}
                    }
                }
                imports.push((ns, prefix));
            }
            _ => statements.push(p),
        }
    }
    let kind = match (vocabulary, bundle) {
        (true, false) => "Vocabulary",
        (true, true) => "VocabularyBundle",
        (false, false) => "Description",
        (false, true) => "DescriptionBundle",
    };
    Header {
        annotations,
        kind,
        ns,
        prefix,
        imports,
        statements,
    }
}

/// The terms an ontology declares, without translating it.
pub(crate) fn terms_of(text: &str) -> Option<Terms> {
    let oml = OmlParser::parse(Rule::oml, text).ok()?.next()?;
    Some(terms_from(&header(oml.into_inner().next()?)))
}

/// The terms declared by a parsed ontology.
pub(crate) fn terms_from(h: &Header) -> Terms {
    let mut prefixes: HashMap<String, String> = HashMap::new();
    prefixes.insert(h.prefix.clone(), h.ns.clone());
    for (ns, prefix) in &h.imports {
        if let Some(prefix) = prefix {
            prefixes.insert(prefix.clone(), ns.clone());
        }
    }
    let mut terms = Terms {
        vocabulary: h.kind.starts_with("Vocabulary"),
        ontology: h.kind,
        imports: h.imports.iter().map(|(ns, _)| ns.clone()).collect(),
        ..Terms::default()
    };
    for s in &h.statements {
        register(
            &mut terms,
            &h.ns,
            &|p| resolve_ref(&prefixes, &h.ns, p),
            s.clone(),
        );
    }
    terms
}

/// The IRI a reference stands for, given the prefixes in scope.
pub(crate) fn resolve_ref(
    prefixes: &HashMap<String, String>,
    ns: &str,
    r: Pair<Rule>,
) -> Option<String> {
    let r = if r.as_rule() == Rule::reference {
        r.into_inner().next()?
    } else {
        r
    };
    let t = r.as_str();
    match r.as_rule() {
        Rule::iri => Some(t[1..t.len() - 1].to_string()),
        Rule::qname => {
            let (p, n) = t.split_once(':')?;
            Some(format!(
                "{}{}",
                prefixes.get(unescape_id(p))?,
                unescape_id(n)
            ))
        }
        Rule::id => Some(format!("{ns}{}", unescape_id(t))),
        _ => None,
    }
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

fn first_upper(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        Some(f) => f.to_uppercase().chain(c).collect(),
        None => String::new(),
    }
}

fn inner(pair: Pair<Rule>) -> Vec<Pair<Rule>> {
    pair.into_inner().collect()
}

pub(crate) fn unescape_id(text: &str) -> &str {
    text.strip_prefix('^').unwrap_or(text)
}

impl<'a, A: ForIRI, O: MutableOntology<A>> Reader<'a, A, O> {
    fn add<C: Into<AnnotatedComponent<A>>>(&mut self, c: C) {
        self.out.insert(c);
    }

    fn iri(&self, s: &str) -> IRI<A> {
        self.build.iri(s)
    }

    // Ontology

    fn ontology(&mut self, pair: Pair<Rule>) -> Result<(), OmlError> {
        let h = header(pair);
        self.ns = h.ns.clone();
        self.prefixes.insert(h.prefix.clone(), h.ns.clone());
        for (ns, prefix) in &h.imports {
            if let Some(prefix) = prefix {
                self.prefixes.insert(prefix.clone(), ns.clone());
            }
        }

        let id = self.ns[..self.ns.len() - 1].to_string();
        self.add(OntologyID {
            iri: Some(self.iri(&id)),
            viri: None,
        });
        self.ontology_annotation(
            "type",
            AnnotationValue::IRI(self.iri(&format!("{OML}{}", h.kind))),
        );
        self.ontology_annotation(
            "namespace",
            AnnotationValue::IRI(self.iri(&self.ns.clone())),
        );
        self.ontology_annotation(
            "prefix",
            AnnotationValue::Literal(Literal::Simple { literal: h.prefix }),
        );
        for (ns, _) in &h.imports {
            let iri = &ns[..ns.len() - 1];
            if !BUILT_IN.contains(&iri) {
                self.add(Import(self.iri(iri)));
            }
        }
        for a in h.annotations {
            for ann in self.annotation(a)? {
                self.add(OntologyAnnotation(ann));
            }
        }

        if h.kind == "VocabularyBundle" {
            self.close_bundle(&h.imports)?;
        }

        // Kinds first, as restrictions refer to properties by kind.
        for s in &h.statements {
            self.register(s);
        }
        for s in h.statements {
            self.statement(s)?;
        }
        Ok(())
    }

    /// Close the world of the bundle: make disjoint the concepts and
    /// relation entities that have no subclass in common.
    fn close_bundle(&mut self, imports: &[(String, Option<String>)]) -> Result<(), OmlError> {
        let mut scope: Vec<Rc<Terms>> = vec![];
        let mut seen = std::collections::BTreeSet::new();
        let mut todo: Vec<String> = imports.iter().map(|(ns, _)| ns.clone()).collect();
        while let Some(ns) = todo.pop() {
            if !seen.insert(ns.clone()) {
                continue;
            }
            match self.imported(&ns) {
                Some(t) => {
                    todo.extend(t.imports.iter().cloned());
                    scope.push(t);
                }
                // The W3C vocabularies only have scalars and properties.
                None if BUILT_IN.contains(&&ns[..ns.len() - 1]) => {}
                None => return invalid(format!("cannot find the ontology {ns}")),
            }
        }
        let classes: Vec<String> = scope
            .iter()
            .filter(|t| t.vocabulary)
            .flat_map(|t| t.kinds.iter())
            .filter(|(_, k)| matches!(k, Kind::Concept | Kind::RelationEntity))
            .map(|(iri, _)| iri.clone())
            .collect::<std::collections::BTreeSet<_>>()
            .into_iter()
            .collect();
        let edges: Vec<(String, String)> = scope
            .iter()
            .flat_map(|t| t.supers.iter())
            .map(|(sub, sup)| (sup.clone(), sub.clone()))
            .collect();
        let taxonomy = Taxonomy::new(&classes, &edges)
            .transitive_reduction()
            .root_at(Ce::Universal);
        let (subclasses, disjoint) = taxonomy.closure();
        for (sub, sup) in subclasses {
            self.add(SubClassOf {
                sup: self.class_expression(&sup),
                sub: self.class_expression(&sub),
            });
        }
        for set in disjoint {
            self.add(DisjointClasses(
                set.iter().map(|c| self.class_expression(c)).collect(),
            ));
        }
        Ok(())
    }

    fn class_expression(&self, c: &Ce) -> ClassExpression<A> {
        match c {
            Ce::Universal => self.class_ce("http://www.w3.org/2002/07/owl#Thing"),
            Ce::Empty => self.class_ce("http://www.w3.org/2002/07/owl#Nothing"),
            Ce::Unit(iri) => self.class_ce(iri),
            Ce::Complement(e) => {
                ClassExpression::ObjectComplementOf(Box::new(self.class_expression(e)))
            }
            Ce::Difference(a, b) => self.class_expression(&a.intersection(&b.complement())),
            Ce::Intersection(s) => ClassExpression::ObjectIntersectionOf(
                s.iter().map(|e| self.class_expression(e)).collect(),
            ),
            Ce::Union(s) => {
                ClassExpression::ObjectUnionOf(s.iter().map(|e| self.class_expression(e)).collect())
            }
        }
    }

    fn ontology_annotation(&mut self, name: &str, av: AnnotationValue<A>) {
        let ap = self.build.annotation_property(format!("{OML}{name}"));
        self.add(OntologyAnnotation(Annotation {
            ap,
            av,
            ann: Default::default(),
        }));
    }

    // References

    fn reference(&self, pair: Pair<Rule>) -> Result<String, OmlError> {
        let r = pair.into_inner().next().unwrap();
        self.resolve(r)
    }

    fn resolve(&self, r: Pair<Rule>) -> Result<String, OmlError> {
        let t = r.as_str();
        match r.as_rule() {
            Rule::iri => Ok(t[1..t.len() - 1].to_string()),
            Rule::qname => {
                let (p, n) = t.split_once(':').unwrap();
                match self.prefixes.get(unescape_id(p)) {
                    Some(ns) => Ok(format!("{ns}{}", unescape_id(n))),
                    None => invalid(format!("unknown prefix in {t}")),
                }
            }
            Rule::id => Ok(format!("{}{}", self.ns, unescape_id(t))),
            _ => unreachable!("not a reference: {r:?}"),
        }
    }

    fn imported(&self, ns: &str) -> Option<Rc<Terms>> {
        if let Some(t) = self.imported.borrow().get(ns) {
            return t.clone();
        }
        let loaded = self
            .resolver
            .load(&ns[..ns.len() - 1])
            .and_then(|text| terms_of(&text))
            .map(Rc::new);
        self.imported
            .borrow_mut()
            .insert(ns.to_string(), loaded.clone());
        loaded
    }

    fn kind_of(&self, iri: &str) -> Result<Kind, OmlError> {
        let known = self.terms.kinds.get(iri).copied().or_else(|| {
            self.imported(namespace_of(iri))
                .and_then(|t| t.kinds.get(iri).copied())
        });
        match known {
            Some(k) => Ok(k),
            None => invalid(format!("cannot tell what {iri} is")),
        }
    }

    /// The relation entity of a forward or reverse relation, and whether the
    /// relation is the forward one.
    fn relation_of(&self, relation: &str) -> Result<(String, bool), OmlError> {
        let known = self.terms.relations.get(relation).cloned().or_else(|| {
            self.imported(namespace_of(relation))
                .and_then(|t| t.relations.get(relation).cloned())
        });
        match known {
            Some(r) => Ok(r),
            None => invalid(format!("{relation} is not a forward or reverse relation")),
        }
    }

    fn forward_of(&self, entity: &str) -> Result<String, OmlError> {
        let known = self.terms.forwards.get(entity).cloned().or_else(|| {
            self.imported(namespace_of(entity))
                .and_then(|t| t.forwards.get(entity).cloned())
        });
        match known {
            Some(f) => Ok(f),
            None => invalid(format!("cannot tell the forward relation of {entity}")),
        }
    }

    // Annotations

    fn annotation(&self, pair: Pair<Rule>) -> Result<Vec<Annotation<A>>, OmlError> {
        let mut parts = pair.into_inner();
        let property = self.reference(parts.next().unwrap())?;
        let ap = self.build.annotation_property(property);
        let mut values = vec![];
        for v in parts {
            let v = v.into_inner().next().unwrap();
            values.push(match v.as_rule() {
                Rule::literal => AnnotationValue::Literal(self.literal(v)?),
                _ => AnnotationValue::IRI(self.iri(&self.reference(v)?)),
            });
        }
        if values.is_empty() {
            values.push(AnnotationValue::Literal(self.boolean(true)));
        }
        Ok(values
            .into_iter()
            .map(|av| Annotation {
                ap: ap.clone(),
                av,
                ann: Default::default(),
            })
            .collect())
    }

    fn annotate(&mut self, subject: &str, annotations: Vec<Pair<Rule>>) -> Result<(), OmlError> {
        for a in annotations {
            for ann in self.annotation(a)? {
                self.add(AnnotationAssertion {
                    subject: self.iri(subject).into(),
                    ann,
                });
            }
        }
        Ok(())
    }

    fn type_annotation(&mut self, subject: &str, kind: &str) {
        let ap = self.build.annotation_property(format!("{OML}type"));
        self.add(AnnotationAssertion {
            subject: self.iri(subject).into(),
            ann: Annotation {
                ap,
                av: AnnotationValue::IRI(self.iri(&format!("{OML}{kind}"))),
                ann: Default::default(),
            },
        });
    }

    // Literals

    fn typed(&self, literal: String, datatype: &str) -> Literal<A> {
        Literal::Datatype {
            literal,
            datatype_iri: self.iri(&format!("{XSD}{datatype}")),
        }
    }

    fn boolean(&self, b: bool) -> Literal<A> {
        self.typed(b.to_string(), "boolean")
    }

    fn literal(&self, pair: Pair<Rule>) -> Result<Literal<A>, OmlError> {
        let l = pair.into_inner().next().unwrap();
        let t = l.as_str();
        Ok(match l.as_rule() {
            Rule::boolean_literal => self.boolean(t == "true"),
            Rule::integer_literal => self.typed(t.trim_start_matches('+').to_string(), "integer"),
            Rule::decimal_literal => self.typed(decimal(t), "decimal"),
            Rule::double_literal => self.typed(double(t)?, "double"),
            Rule::quoted_literal => {
                let mut parts = l.into_inner();
                let value = unquote(parts.next().unwrap().as_str());
                match parts.next() {
                    None => Literal::Simple { literal: value },
                    Some(p) if p.as_rule() == Rule::reference => Literal::Datatype {
                        literal: value,
                        datatype_iri: self.iri(&self.reference(p)?),
                    },
                    Some(p) => Literal::Language {
                        literal: value,
                        lang: unescape_id(p.as_str()).to_string(),
                    },
                }
            }
            r => unreachable!("not a literal: {r:?}"),
        })
    }

    fn unsigned(&self, pair: &Pair<Rule>) -> u32 {
        pair.as_str().parse().unwrap_or(u32::MAX)
    }

    fn int_literal(&self, n: u32) -> Literal<A> {
        self.typed(n.to_string(), "integer")
    }

    // Statements

    fn register(&mut self, statement: &Pair<Rule>) {
        let mut terms = std::mem::take(&mut self.terms);
        register(
            &mut terms,
            &self.ns,
            &|p| self.reference(p).ok(),
            statement.clone(),
        );
        self.terms = terms;
    }
}

/// The kind of a term and whether it is a `ref`.
pub(crate) fn shape(pair: &Pair<Rule>) -> Option<(Kind, bool)> {
    let kind = match pair.as_rule() {
        Rule::aspect => Kind::Aspect,
        Rule::concept => Kind::Concept,
        Rule::relation_entity => Kind::RelationEntity,
        Rule::scalar => Kind::Scalar,
        Rule::scalar_property => Kind::ScalarProperty,
        Rule::annotation_property => Kind::AnnotationProperty,
        Rule::unreified_relation => Kind::Relation,
        _ => return None,
    };
    let is_ref = pair
        .clone()
        .into_inner()
        .next()
        .is_some_and(|p| p.as_rule() == Rule::kw_ref);
    Some((kind, is_ref))
}

/// The name (a declared `id`, or a `reference` after `ref`) of a term.
fn name_of<'i>(pair: &Pair<'i, Rule>) -> Option<Pair<'i, Rule>> {
    pair.clone()
        .into_inner()
        .find(|p| matches!(p.as_rule(), Rule::id | Rule::reference))
}

/// Note what a statement declares. `reference` resolves the name of a `ref`.
fn register(
    terms: &mut Terms,
    ns: &str,
    reference: &dyn Fn(Pair<Rule>) -> Option<String>,
    statement: Pair<Rule>,
) {
    let (_, term) = split(statement);
    let shaped = shape(&term);
    let is_ref = shaped.map_or_else(
        || {
            term.clone()
                .into_inner()
                .next()
                .is_some_and(|p| p.as_rule() == Rule::kw_ref)
        },
        |(_, is_ref)| is_ref,
    );
    let Some(name) = name_of(&term) else {
        return;
    };
    let iri = if name.as_rule() == Rule::id {
        format!("{ns}{}", unescape_id(name.as_str()))
    } else {
        match reference(name) {
            Some(iri) => iri,
            None => return,
        }
    };
    if !is_ref {
        terms.names.insert(iri.clone());
    }
    let Some((kind, _)) = shaped else {
        return;
    };
    if !is_ref {
        terms.kinds.insert(iri.clone(), kind);
    }
    let mut forward = None;
    for p in term.into_inner() {
        if matches!(
            p.as_rule(),
            Rule::entity_specialization | Rule::entity_equivalence
        ) {
            for r in p
                .clone()
                .into_inner()
                .flat_map(|x| std::iter::once(x.clone()).chain(x.into_inner()))
                .filter(|x| x.as_rule() == Rule::reference)
            {
                if let Some(sup) = reference(r) {
                    terms.supers.push((iri.clone(), sup));
                }
            }
        }
        let declared = p
            .clone()
            .into_inner()
            .find(|x| x.as_rule() == Rule::id)
            .map(|x| format!("{ns}{}", unescape_id(x.as_str())));
        match (p.as_rule(), declared) {
            (Rule::forward_relation, Some(f)) => {
                terms.names.insert(f.clone());
                terms.kinds.insert(f.clone(), Kind::Relation);
                terms.relations.insert(f.clone(), (iri.clone(), true));
                forward = Some(f);
            }
            (Rule::reverse_relation, Some(r)) => {
                terms.names.insert(r.clone());
                terms.kinds.insert(r.clone(), Kind::Relation);
                terms.relations.insert(r, (iri.clone(), false));
            }
            _ => {}
        }
    }
    // A `ref` without a forward relation of its own takes that of the entity
    // it refers to.
    if kind == Kind::RelationEntity && (!is_ref || forward.is_some()) {
        let forward =
            forward.unwrap_or_else(|| format!("{ns}has{}Forward", first_upper(local_name(&iri))));
        terms.forwards.insert(iri, forward);
    }
}

/// The annotations and the term of a statement.
fn split(statement: Pair<Rule>) -> (Vec<Pair<Rule>>, Pair<Rule>) {
    let mut annotations = vec![];
    let mut term = None;
    for p in statement.into_inner() {
        if p.as_rule() == Rule::annotation {
            annotations.push(p);
        } else {
            term = Some(p);
        }
    }
    (annotations, term.unwrap())
}

fn decimal(t: &str) -> String {
    // Java's BigDecimal.toString, which is what oml2owl writes.
    let t = t.trim_start_matches('+');
    let (neg, t) = match t.strip_prefix('-') {
        Some(rest) => (true, rest),
        None => (false, t),
    };
    let (int, frac) = t.split_once('.').unwrap_or((t, ""));
    let int = if int.is_empty() { "0" } else { int };
    let s = if frac.is_empty() {
        int.to_string()
    } else {
        format!("{int}.{frac}")
    };
    if neg { format!("-{s}") } else { s }
}

fn double(t: &str) -> Result<String, OmlError> {
    let v: f64 = t
        .parse()
        .map_err(|_| OmlError::Invalid(format!("bad double {t}")))?;
    Ok(java_double(v))
}

/// Java's Double.toString, which is what the OWL API writes.
fn java_double(v: f64) -> String {
    let a = v.abs();
    if a != 0.0 && !(1e-3..1e7).contains(&a) {
        let s = format!("{:e}", v);
        let (m, e) = s.split_once('e').unwrap();
        let m = if m.contains('.') {
            m.to_string()
        } else {
            format!("{m}.0")
        };
        format!("{m}E{e}")
    } else if v.fract() == 0.0 {
        format!("{v:.1}")
    } else {
        format!("{v}")
    }
}

/// The text of a string, as the OML tools read it: the quotes are removed
/// and nothing inside is treated as an escape.
fn unquote(t: &str) -> String {
    let triple = t.len() >= 6 && (t.starts_with("\"\"\"") || t.starts_with("\x27\x27\x27"));
    let quote = if triple { 3 } else { 1 };
    t[quote..t.len() - quote].to_string()
}

// Terms

impl<'a, A: ForIRI, O: MutableOntology<A>> Reader<'a, A, O> {
    fn class_ce(&self, iri: &str) -> ClassExpression<A> {
        ClassExpression::Class(self.build.class(iri))
    }

    fn object_property(&self, iri: &str) -> ObjectPropertyExpression<A> {
        ObjectPropertyExpression::ObjectProperty(self.build.object_property(iri))
    }

    fn datatype(&self, iri: &str) -> DataRange<A> {
        DataRange::Datatype(self.build.datatype(iri))
    }

    /// The IRI, whether it is a `ref`, and what follows the name.
    fn named<'i>(
        &self,
        term: &Pair<'i, Rule>,
    ) -> Result<(String, bool, Vec<Pair<'i, Rule>>), OmlError> {
        let is_ref = term
            .clone()
            .into_inner()
            .next()
            .is_some_and(|p| p.as_rule() == Rule::kw_ref);
        let mut name = None;
        let mut rest = vec![];
        for p in term.clone().into_inner() {
            if name.is_none() && matches!(p.as_rule(), Rule::id | Rule::reference) {
                name = Some(p);
            } else if name.is_some() {
                rest.push(p);
            }
        }
        let name = name.unwrap();
        let iri = if name.as_rule() == Rule::id {
            format!("{}{}", self.ns, unescape_id(name.as_str()))
        } else {
            self.reference(name)?
        };
        Ok((iri, is_ref, rest))
    }

    fn statement(&mut self, pair: Pair<Rule>) -> Result<(), OmlError> {
        let (annotations, term) = split(pair);
        let (iri, is_ref, rest) = self.named(&term)?;
        match term.as_rule() {
            Rule::rule => return self.rule(&iri, is_ref, annotations, rest),
            Rule::builtin => return self.annotate(&iri, annotations),
            _ => {}
        }
        self.annotate(&iri, annotations)?;
        match term.as_rule() {
            Rule::aspect => self.entity(Kind::Aspect, &iri, is_ref, rest),
            Rule::concept => self.entity(Kind::Concept, &iri, is_ref, rest),
            Rule::relation_entity => self.relation_entity(&iri, is_ref, rest),
            Rule::scalar => self.scalar(&iri, is_ref, rest),
            Rule::scalar_property => self.scalar_property(&iri, is_ref, rest),
            Rule::annotation_property => {
                if !is_ref {
                    self.add(DeclareAnnotationProperty(
                        self.build.annotation_property(iri.as_str()),
                    ));
                }
                Ok(())
            }
            Rule::unreified_relation => self.unreified_relation(&iri, is_ref, rest),
            Rule::concept_instance => self.concept_instance(&iri, is_ref, rest),
            Rule::relation_instance => self.relation_instance(&iri, is_ref, rest),
            r => invalid(format!("not supported yet: {r:?}")),
        }
    }

    fn references(&self, pair: Pair<Rule>) -> Result<Vec<String>, OmlError> {
        pair.into_inner()
            .filter(|p| p.as_rule() == Rule::reference)
            .map(|p| self.reference(p))
            .collect()
    }

    fn entity(
        &mut self,
        kind: Kind,
        iri: &str,
        is_ref: bool,
        parts: Vec<Pair<Rule>>,
    ) -> Result<(), OmlError> {
        if !is_ref {
            self.add(DeclareClass(self.build.class(iri)));
            let type_name = if kind == Kind::Aspect {
                "Aspect"
            } else {
                "Concept"
            };
            self.type_annotation(iri, type_name);
        }
        for part in parts {
            self.entity_part(iri, None, part)?;
        }
        Ok(())
    }

    /// Axioms from the body of an entity. `forward` is that of a relation
    /// entity, which is specialised along with the entity.
    fn entity_part(
        &mut self,
        iri: &str,
        forward: Option<&str>,
        part: Pair<Rule>,
    ) -> Result<(), OmlError> {
        let this = self.class_ce(iri);
        match part.as_rule() {
            Rule::key_axiom => self.key(iri, part)?,
            Rule::instance_enumeration => {
                let individuals = self
                    .references(part)?
                    .iter()
                    .map(|i| Individual::Named(self.build.named_individual(i.as_str())))
                    .collect::<Vec<_>>();
                if !individuals.is_empty() {
                    self.add(EquivalentClasses(vec![
                        this.clone(),
                        ClassExpression::ObjectOneOf(individuals),
                    ]));
                }
            }
            Rule::entity_specialization => {
                for p in part.into_inner() {
                    match p.as_rule() {
                        Rule::reference => {
                            let sup = self.reference(p)?;
                            self.add(SubClassOf {
                                sup: self.class_ce(&sup),
                                sub: this.clone(),
                            });
                            if let Some(forward) = forward {
                                if self.kind_of(&sup)? == Kind::RelationEntity {
                                    self.add(SubObjectPropertyOf {
                                        sup: self.object_property(&self.forward_of(&sup)?),
                                        sub: SubObjectPropertyExpression::ObjectPropertyExpression(
                                            self.object_property(forward),
                                        ),
                                    });
                                }
                            }
                        }
                        Rule::restriction_block => {
                            for r in self.restrictions(p)? {
                                self.add(SubClassOf {
                                    sup: r,
                                    sub: this.clone(),
                                });
                            }
                        }
                        _ => {}
                    }
                }
            }
            Rule::entity_equivalence => {
                for axiom in part.into_inner() {
                    self.entity_equivalence(&this, axiom)?;
                }
            }
            _ => {}
        }
        Ok(())
    }

    fn relation_entity(
        &mut self,
        iri: &str,
        is_ref: bool,
        parts: Vec<Pair<Rule>>,
    ) -> Result<(), OmlError> {
        let forward = self.forward_of(iri)?;
        if !is_ref {
            self.add(DeclareClass(self.build.class(iri)));
            self.type_annotation(iri, "RelationEntity");
        }
        let mut sources = vec![];
        let mut targets = vec![];
        let mut flags = vec![];
        let mut reverse = None;
        let mut others = vec![];
        for part in parts {
            match part.as_rule() {
                Rule::from_clause => sources = self.references(part)?,
                Rule::to_clause => targets = self.references(part)?,
                Rule::characteristic => flags.push(flag(&part)),
                Rule::forward_relation => {
                    let (annotations, _) = self.relation_name(part);
                    self.annotate(&forward, annotations)?;
                }
                Rule::reverse_relation => reverse = Some(part),
                _ => others.push(part),
            }
        }
        if !is_ref {
            self.add(DeclareObjectProperty(
                self.build.object_property(forward.as_str()),
            ));
            self.type_annotation(&forward, "ForwardRelation");
            let ap = self
                .build
                .annotation_property(format!("{OML}relationEntity"));
            self.add(AnnotationAssertion {
                subject: self.iri(&forward).into(),
                ann: Annotation {
                    ap,
                    av: AnnotationValue::IRI(self.iri(iri)),
                    ann: Default::default(),
                },
            });
            self.derivation_rule(iri, &forward);
        }
        self.relation_base(&forward, &sources, &targets, &flags, false);
        for part in others {
            self.entity_part(iri, Some(&forward), part)?;
        }
        if let Some(r) = reverse {
            self.reverse_relation(iri, &forward, &sources, &targets, &flags, r)?;
        }
        Ok(())
    }

    /// The forward relation holds between source and target of an instance.
    fn derivation_rule(&mut self, entity: &str, forward: &str) {
        let var =
            |name: &str| IArgument::Variable(self.build.variable(format!("urn:swrl:var#{name}")));
        let atom =
            |ope: ObjectPropertyExpression<A>, from: &str, to: &str| Atom::ObjectPropertyAtom {
                pred: ope,
                args: (var(from), var(to)),
            };
        let body = vec![
            Atom::ClassAtom {
                pred: self.class_ce(entity),
                arg: var("r"),
            },
            atom(self.object_property(&format!("{OML}hasSource")), "r", "s"),
            atom(self.object_property(&format!("{OML}hasTarget")), "r", "t"),
        ];
        let head = vec![atom(self.object_property(forward), "s", "t")];
        let name = forward.strip_prefix(self.ns.as_str()).unwrap_or(forward);
        let label = Annotation {
            ap: self
                .build
                .annotation_property("http://www.w3.org/2000/01/rdf-schema#label"),
            av: AnnotationValue::Literal(Literal::Simple {
                literal: format!("{name} derivation"),
            }),
            ann: Default::default(),
        };
        self.out.insert(AnnotatedComponent::new(
            horned_owl::model::Rule { head, body },
            [label].into(),
        ));
    }

    /// The annotations and the name of a forward or reverse relation.
    fn relation_name<'i>(&self, pair: Pair<'i, Rule>) -> (Vec<Pair<'i, Rule>>, Pair<'i, Rule>) {
        let mut annotations = vec![];
        let mut name = None;
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::annotation => annotations.push(p),
                Rule::id => name = Some(p),
                _ => {}
            }
        }
        (annotations, name.unwrap())
    }

    // Instances

    fn concept_instance(
        &mut self,
        iri: &str,
        is_ref: bool,
        parts: Vec<Pair<Rule>>,
    ) -> Result<(), OmlError> {
        self.instance(iri, is_ref, "ConceptInstance");
        self.instance_parts(iri, parts)
    }

    fn relation_instance(
        &mut self,
        iri: &str,
        is_ref: bool,
        parts: Vec<Pair<Rule>>,
    ) -> Result<(), OmlError> {
        self.instance(iri, is_ref, "RelationInstance");
        self.instance_parts(iri, parts)
    }

    fn instance(&mut self, iri: &str, is_ref: bool, kind: &str) {
        if !is_ref {
            self.add(DeclareNamedIndividual(self.build.named_individual(iri)));
        }
        self.type_annotation(iri, kind);
    }

    fn instance_parts(&mut self, iri: &str, parts: Vec<Pair<Rule>>) -> Result<(), OmlError> {
        let this = Individual::Named(self.build.named_individual(iri));
        for part in parts {
            match part.as_rule() {
                Rule::reference => {
                    let t = self.reference(part)?;
                    self.add(ClassAssertion {
                        ce: self.class_ce(&t),
                        i: this.clone(),
                    });
                }
                Rule::from_clause => self.source_target("hasSource", &this, part)?,
                Rule::to_clause => self.source_target("hasTarget", &this, part)?,
                Rule::property_value_assertion => self.property_values(&this, part)?,
                _ => {}
            }
        }
        Ok(())
    }

    fn source_target(
        &mut self,
        property: &str,
        this: &Individual<A>,
        pair: Pair<Rule>,
    ) -> Result<(), OmlError> {
        for i in self.references(pair)? {
            self.add(ObjectPropertyAssertion {
                ope: self.object_property(&format!("{OML}{property}")),
                from: this.clone(),
                to: Individual::Named(self.build.named_individual(i.as_str())),
            });
        }
        Ok(())
    }

    fn property_values(&mut self, this: &Individual<A>, pair: Pair<Rule>) -> Result<(), OmlError> {
        let mut parts = pair.into_inner();
        let property = self.reference(parts.next().unwrap())?;
        for value in parts {
            let v = value.into_inner().next().unwrap();
            match v.as_rule() {
                Rule::literal => self.add(DataPropertyAssertion {
                    dp: self.build.data_property(property.as_str()),
                    from: this.clone(),
                    to: self.literal(v)?,
                }),
                Rule::reference => {
                    let to = self.reference(v)?;
                    self.add(ObjectPropertyAssertion {
                        ope: self.object_property(&property),
                        from: this.clone(),
                        to: Individual::Named(self.build.named_individual(to.as_str())),
                    });
                }
                _ => {
                    let (ope, to) = self.contained(&property, v)?;
                    self.add(ObjectPropertyAssertion {
                        ope,
                        from: this.clone(),
                        to,
                    });
                }
            }
        }
        Ok(())
    }

    // Rules

    fn rule(
        &mut self,
        iri: &str,
        is_ref: bool,
        annotations: Vec<Pair<Rule>>,
        parts: Vec<Pair<Rule>>,
    ) -> Result<(), OmlError> {
        if is_ref {
            return self.annotate(iri, annotations);
        }
        let mut ann = std::collections::BTreeSet::new();
        for a in annotations {
            ann.extend(self.annotation(a)?);
        }
        for (name, kind) in [("name", iri.to_string()), ("type", format!("{OML}Rule"))] {
            ann.insert(Annotation {
                ap: self.build.annotation_property(format!("{OML}{name}")),
                av: AnnotationValue::Literal(Literal::Simple { literal: kind }),
                ann: Default::default(),
            });
        }
        let (mut body, mut head) = (vec![], vec![]);
        for part in parts {
            let target = if part.as_rule() == Rule::antecedent {
                &mut body
            } else {
                &mut head
            };
            for predicate in part.into_inner() {
                target.extend(self.atoms(predicate)?);
            }
        }
        self.out.insert(AnnotatedComponent::new(
            horned_owl::model::Rule { head, body },
            ann,
        ));
        Ok(())
    }

    fn atoms(&self, predicate: Pair<Rule>) -> Result<Vec<Atom<A>>, OmlError> {
        let p = predicate.into_inner().next().unwrap();
        let kind = p.as_rule();
        let mut parts = p.into_inner();
        Ok(match kind {
            Rule::same_as_predicate | Rule::different_from_predicate => {
                let args: Vec<_> = parts.filter(is_argument).collect();
                let a1 = self.iargument(args[0].clone())?;
                let a2 = self.iargument(args[1].clone())?;
                vec![if kind == Rule::same_as_predicate {
                    Atom::SameIndividualAtom(a1, a2)
                } else {
                    Atom::DifferentIndividualsAtom(a1, a2)
                }]
            }
            Rule::built_in_predicate => {
                let builtin =
                    self.reference(parts.find(|p| p.as_rule() == Rule::reference).unwrap())?;
                let args = parts
                    .map(|a| self.dargument(a))
                    .collect::<Result<Vec<_>, _>>()?;
                vec![Atom::BuiltInAtom {
                    pred: self.iri(&builtin),
                    args,
                }]
            }
            Rule::relation_entity_predicate => {
                let entity = self.reference(parts.next().unwrap())?;
                let source = self.iargument(parts.next().unwrap())?;
                let relation = self.iargument(parts.next().unwrap())?;
                let target = self.iargument(parts.next().unwrap())?;
                vec![
                    Atom::ClassAtom {
                        pred: self.class_ce(&entity),
                        arg: relation.clone(),
                    },
                    Atom::ObjectPropertyAtom {
                        pred: self.object_property(&format!("{OML}hasSource")),
                        args: (relation.clone(), source),
                    },
                    Atom::ObjectPropertyAtom {
                        pred: self.object_property(&format!("{OML}hasTarget")),
                        args: (relation, target),
                    },
                ]
            }
            Rule::property_predicate => {
                let property = self.reference(parts.next().unwrap())?;
                let first = parts.next().unwrap();
                let second = parts.next().unwrap();
                let data = match self.kind_of(&property)? {
                    Kind::ScalarProperty => true,
                    Kind::AnnotationProperty => is_literal(&second),
                    _ => false,
                };
                vec![if data {
                    Atom::DataPropertyAtom {
                        pred: self.build.data_property(property.as_str()),
                        args: (self.dargument(first)?, self.dargument(second)?),
                    }
                } else {
                    Atom::ObjectPropertyAtom {
                        pred: self.object_property(&property),
                        args: (self.iargument(first)?, self.iargument(second)?),
                    }
                }]
            }
            _ => {
                let ty = self.reference(parts.next().unwrap())?;
                let arg = parts.next().unwrap();
                vec![if self.kind_of(&ty)? == Kind::Scalar {
                    Atom::DataRangeAtom {
                        pred: self.datatype(&ty),
                        arg: self.dargument(arg)?,
                    }
                } else {
                    Atom::ClassAtom {
                        pred: self.class_ce(&ty),
                        arg: self.iargument(arg)?,
                    }
                }]
            }
        })
    }

    fn variable(&self, id: &Pair<Rule>) -> Variable<A> {
        self.build
            .variable(format!("urn:swrl:var#{}", unescape_id(id.as_str())))
    }

    fn iargument(&self, pair: Pair<Rule>) -> Result<IArgument<A>, OmlError> {
        let a = pair.into_inner().next().unwrap();
        match a.as_rule() {
            Rule::id => Ok(IArgument::Variable(self.variable(&a))),
            Rule::cross_reference => {
                let iri = self.resolve(a.into_inner().next().unwrap())?;
                Ok(IArgument::Individual(Individual::Named(
                    self.build.named_individual(iri.as_str()),
                )))
            }
            _ => invalid("a literal is not an individual"),
        }
    }

    fn dargument(&self, pair: Pair<Rule>) -> Result<DArgument<A>, OmlError> {
        let a = pair.into_inner().next().unwrap();
        match a.as_rule() {
            Rule::id => Ok(DArgument::Variable(self.variable(&a))),
            Rule::literal => Ok(DArgument::Literal(self.literal(a)?)),
            _ => invalid("an individual is not a literal"),
        }
    }

    fn entity_equivalence(
        &mut self,
        this: &ClassExpression<A>,
        axiom: Pair<Rule>,
    ) -> Result<(), OmlError> {
        let mut supers = vec![];
        let mut restrictions = vec![];
        for p in axiom.into_inner() {
            match p.as_rule() {
                Rule::reference => supers.push(self.class_ce(&self.reference(p)?)),
                Rule::restriction_block => restrictions.extend(self.restrictions(p)?),
                _ => {}
            }
        }
        let other = if supers.len() == 1 && restrictions.is_empty() {
            supers.remove(0)
        } else if restrictions.len() == 1 && supers.is_empty() {
            restrictions.remove(0)
        } else {
            supers.extend(restrictions);
            ClassExpression::ObjectIntersectionOf(supers)
        };
        self.add(EquivalentClasses(vec![this.clone(), other]));
        Ok(())
    }

    fn key(&mut self, iri: &str, pair: Pair<Rule>) -> Result<(), OmlError> {
        let mut vpe = vec![];
        for p in self.references(pair)? {
            vpe.push(match self.kind_of(&p)? {
                Kind::ScalarProperty => {
                    PropertyExpression::DataProperty(self.build.data_property(p.as_str()))
                }
                Kind::AnnotationProperty => PropertyExpression::AnnotationProperty(
                    self.build.annotation_property(p.as_str()),
                ),
                _ => PropertyExpression::ObjectPropertyExpression(self.object_property(&p)),
            });
        }
        self.add(HasKey {
            ce: self.class_ce(iri),
            vpe,
        });
        Ok(())
    }

    fn restrictions(&mut self, block: Pair<Rule>) -> Result<Vec<ClassExpression<A>>, OmlError> {
        let mut out = vec![];
        for r in block.into_inner() {
            out.push(self.restriction(r)?);
        }
        Ok(out)
    }

    fn restriction(&mut self, pair: Pair<Rule>) -> Result<ClassExpression<A>, OmlError> {
        let parts = inner(pair);
        let mut it = parts
            .into_iter()
            .filter(|p| p.as_rule() != Rule::kw_restricts);
        let first = it.next().unwrap();
        if matches!(first.as_rule(), Rule::kw_all | Rule::kw_some) {
            let all = first.as_rule() == Rule::kw_all;
            let property = self.reference(it.next().unwrap())?;
            it.next(); // to
            let range = self.reference(it.next().unwrap())?;
            return Ok(match self.kind_of(&property)? {
                Kind::ScalarProperty => {
                    let dp = self.build.data_property(property.as_str());
                    let dr = self.datatype(&range);
                    if all {
                        ClassExpression::DataAllValuesFrom { dp, dr }
                    } else {
                        ClassExpression::DataSomeValuesFrom { dp, dr }
                    }
                }
                _ => {
                    let ope = self.object_property(&property);
                    let bce = Box::new(self.class_ce(&range));
                    if all {
                        ClassExpression::ObjectAllValuesFrom { ope, bce }
                    } else {
                        ClassExpression::ObjectSomeValuesFrom { ope, bce }
                    }
                }
            });
        }
        let property = self.reference(first)?;
        it.next(); // to
        let what = it.next().unwrap();
        let data = self.kind_of(&property)? == Kind::ScalarProperty;
        match what.as_rule() {
            Rule::kw_self => Ok(ClassExpression::ObjectHasSelf(
                self.object_property(&property),
            )),
            Rule::cardinality => self.cardinality(&property, data, what),
            _ => {
                let value = what.into_inner().next().unwrap();
                match value.as_rule() {
                    Rule::literal => Ok(ClassExpression::DataHasValue {
                        dp: self.build.data_property(property.as_str()),
                        l: self.literal(value)?,
                    }),
                    Rule::reference => {
                        let i = self.reference(value)?;
                        Ok(ClassExpression::ObjectHasValue {
                            ope: self.object_property(&property),
                            i: Individual::Named(self.build.named_individual(i.as_str())),
                        })
                    }
                    _ => {
                        let (ope, i) = self.contained(&property, value)?;
                        Ok(ClassExpression::ObjectHasValue { ope, i })
                    }
                }
            }
        }
    }

    /// The property and individual for an anonymous instance given as the
    /// value of `property`. A relation instance is related to its owner by
    /// the inverse of `hasSource` or `hasTarget`.
    fn contained(
        &mut self,
        property: &str,
        value: Pair<Rule>,
    ) -> Result<(ObjectPropertyExpression<A>, Individual<A>), OmlError> {
        if value.as_rule() == Rule::anonymous_concept_instance {
            let i = self.anonymous(value, None)?;
            return Ok((self.object_property(property), i));
        }
        let (entity, forward) = self.relation_of(property)?;
        let (inverse, has) = if forward {
            ("isSourceOf", "hasSource")
        } else {
            ("isTargetOf", "hasTarget")
        };
        self.add(InverseObjectProperties(
            self.build.object_property(format!("{OML}{inverse}")),
            self.build.object_property(format!("{OML}{has}")),
        ));
        let i = self.anonymous(value, Some((entity, forward)))?;
        Ok((self.object_property(&format!("{OML}{inverse}")), i))
    }

    /// An anonymous instance, which oml2owl makes a named individual in the
    /// ontology. `relation` is the relation entity and direction, for a
    /// relation instance.
    fn anonymous(
        &mut self,
        pair: Pair<Rule>,
        relation: Option<(String, bool)>,
    ) -> Result<Individual<A>, OmlError> {
        let iri = format!("{}_b{}", self.ns, self.anonymous);
        self.anonymous += 1;
        self.add(DeclareNamedIndividual(
            self.build.named_individual(iri.as_str()),
        ));
        let this = Individual::Named(self.build.named_individual(iri.as_str()));
        let mut values = vec![];
        if pair.as_rule() == Rule::anonymous_concept_instance {
            self.type_annotation(&iri, "AnonymousConceptInstance");
            let mut typed = false;
            for p in pair.into_inner() {
                if p.as_rule() == Rule::reference {
                    let t = self.reference(p)?;
                    self.add(ClassAssertion {
                        ce: self.class_ce(&t),
                        i: this.clone(),
                    });
                    typed = true;
                } else {
                    values.push(p);
                }
            }
            if !typed {
                return invalid("an anonymous concept instance needs a type");
            }
        } else {
            self.type_annotation(&iri, "AnonymousRelationInstance");
            let (entity, forward) = relation.unwrap();
            self.add(ClassAssertion {
                ce: self.class_ce(&entity),
                i: this.clone(),
            });
            for p in pair.into_inner() {
                if p.as_rule() == Rule::reference {
                    let target = self.reference(p)?;
                    let end = if forward { "hasTarget" } else { "hasSource" };
                    self.add(ObjectPropertyAssertion {
                        ope: self.object_property(&format!("{OML}{end}")),
                        from: this.clone(),
                        to: Individual::Named(self.build.named_individual(target.as_str())),
                    });
                } else {
                    values.push(p);
                }
            }
        }
        for v in values {
            self.property_values(&this, v)?;
        }
        Ok(this)
    }

    fn cardinality(
        &self,
        property: &str,
        data: bool,
        pair: Pair<Rule>,
    ) -> Result<ClassExpression<A>, OmlError> {
        let mut kind = Rule::kw_exactly;
        let mut n = 0;
        let mut range = None;
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::unsigned_integer => n = self.unsigned(&p),
                Rule::reference => range = Some(self.reference(p)?),
                r => kind = r,
            }
        }
        Ok(if data {
            let dp = self.build.data_property(property);
            let dr = match range {
                Some(r) => self.datatype(&r),
                None => self.datatype("http://www.w3.org/2000/01/rdf-schema#Literal"),
            };
            match kind {
                Rule::kw_min => ClassExpression::DataMinCardinality { n, dp, dr },
                Rule::kw_max => ClassExpression::DataMaxCardinality { n, dp, dr },
                _ => ClassExpression::DataExactCardinality { n, dp, dr },
            }
        } else {
            let ope = self.object_property(property);
            let bce = Box::new(match range {
                Some(r) => self.class_ce(&r),
                None => self.class_ce("http://www.w3.org/2002/07/owl#Thing"),
            });
            match kind {
                Rule::kw_min => ClassExpression::ObjectMinCardinality { n, ope, bce },
                Rule::kw_max => ClassExpression::ObjectMaxCardinality { n, ope, bce },
                _ => ClassExpression::ObjectExactCardinality { n, ope, bce },
            }
        })
    }

    fn scalar(&mut self, iri: &str, is_ref: bool, parts: Vec<Pair<Rule>>) -> Result<(), OmlError> {
        let kind = self.build.datatype(iri);
        if !is_ref {
            self.add(DeclareDatatype(kind.clone()));
        }
        for part in parts {
            match part.as_rule() {
                Rule::literal_enumeration => {
                    let literals = part
                        .into_inner()
                        .filter(|p| p.as_rule() == Rule::literal)
                        .map(|p| self.literal(p))
                        .collect::<Result<Vec<_>, _>>()?;
                    if !literals.is_empty() {
                        self.add(DatatypeDefinition {
                            kind: kind.clone(),
                            range: DataRange::DataOneOf(literals),
                        });
                    }
                }
                Rule::scalar_equivalence_axiom => {
                    let mut sup = None;
                    let mut facets = vec![];
                    for p in part.into_inner() {
                        match p.as_rule() {
                            Rule::reference => sup = Some(self.reference(p)?),
                            Rule::facet => facets.push(self.facet(p)?),
                            _ => {}
                        }
                    }
                    let sup = self.build.datatype(sup.unwrap());
                    let range = if facets.is_empty() {
                        DataRange::Datatype(sup)
                    } else {
                        DataRange::DatatypeRestriction(sup, facets)
                    };
                    self.add(DatatypeDefinition {
                        kind: kind.clone(),
                        range,
                    });
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn facet(&self, pair: Pair<Rule>) -> Result<FacetRestriction<A>, OmlError> {
        let parts = inner(pair);
        let word = parts[0].as_rule();
        let value = &parts[1];
        let (f, l) = match word {
            Rule::kw_length => (Facet::Length, self.int_literal(self.unsigned(value))),
            Rule::kw_minLength => (Facet::MinLength, self.int_literal(self.unsigned(value))),
            Rule::kw_maxLength => (Facet::MaxLength, self.int_literal(self.unsigned(value))),
            Rule::kw_pattern => (
                Facet::Pattern,
                Literal::Simple {
                    literal: unquote(value.as_str()),
                },
            ),
            Rule::kw_language => (
                Facet::LangRange,
                Literal::Simple {
                    literal: unescape_id(value.as_str()).to_string(),
                },
            ),
            Rule::kw_minInclusive => (Facet::MinInclusive, self.literal(value.clone())?),
            Rule::kw_minExclusive => (Facet::MinExclusive, self.literal(value.clone())?),
            Rule::kw_maxInclusive => (Facet::MaxInclusive, self.literal(value.clone())?),
            _ => (Facet::MaxExclusive, self.literal(value.clone())?),
        };
        Ok(FacetRestriction { f, l })
    }

    fn scalar_property(
        &mut self,
        iri: &str,
        is_ref: bool,
        parts: Vec<Pair<Rule>>,
    ) -> Result<(), OmlError> {
        let dp = self.build.data_property(iri);
        if !is_ref {
            self.add(DeclareDataProperty(dp.clone()));
        }
        for part in parts {
            match part.as_rule() {
                Rule::domain_clause => {
                    for d in self.references(part)? {
                        self.add(DataPropertyDomain {
                            dp: dp.clone(),
                            ce: self.class_ce(&d),
                        });
                    }
                }
                Rule::range_clause => {
                    for r in self.references(part)? {
                        self.add(DataPropertyRange {
                            dp: dp.clone(),
                            dr: self.datatype(&r),
                        });
                    }
                }
                Rule::kw_functional => self.add(FunctionalDataProperty(dp.clone())),
                Rule::property_specialization => {
                    for s in self.references(part)? {
                        if self.kind_of(&s)? == Kind::ScalarProperty {
                            self.add(SubDataPropertyOf {
                                sup: self.build.data_property(s.as_str()),
                                sub: dp.clone(),
                            });
                        }
                    }
                }
                Rule::property_equivalence => {
                    for s in self.references(part)? {
                        self.add(EquivalentDataProperties(vec![
                            dp.clone(),
                            self.build.data_property(s.as_str()),
                        ]));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn unreified_relation(
        &mut self,
        iri: &str,
        is_ref: bool,
        parts: Vec<Pair<Rule>>,
    ) -> Result<(), OmlError> {
        let op = self.build.object_property(iri);
        if !is_ref {
            self.add(DeclareObjectProperty(op.clone()));
            self.type_annotation(iri, "UnreifiedRelation");
        }
        let mut sources = vec![];
        let mut targets = vec![];
        let mut flags = vec![];
        let mut reverse = None;
        for part in parts {
            match part.as_rule() {
                Rule::from_clause => sources = self.references(part)?,
                Rule::to_clause => targets = self.references(part)?,
                Rule::characteristic => flags.push(
                    part.as_str()
                        .split_whitespace()
                        .collect::<Vec<_>>()
                        .join(" "),
                ),
                Rule::reverse_relation => reverse = Some(part),
                Rule::property_specialization => {
                    for s in self.references(part)? {
                        if self.kind_of(&s)? == Kind::Relation {
                            self.add(SubObjectPropertyOf {
                                sup: self.object_property(&s),
                                sub: SubObjectPropertyExpression::ObjectPropertyExpression(
                                    self.object_property(iri),
                                ),
                            });
                        }
                    }
                }
                Rule::property_equivalence => {
                    for s in self.references(part)? {
                        self.add(EquivalentObjectProperties(vec![
                            self.object_property(iri),
                            self.object_property(&s),
                        ]));
                    }
                }
                _ => {}
            }
        }
        self.relation_base(iri, &sources, &targets, &flags, false);
        if let Some(r) = reverse {
            self.reverse_relation(iri, iri, &sources, &targets, &flags, r)?;
        }
        Ok(())
    }

    /// Domain, range and characteristics of a forward or reverse relation.
    /// For a reverse relation the sources and targets are swapped and the
    /// functional flags exchanged.
    fn relation_base(
        &mut self,
        iri: &str,
        sources: &[String],
        targets: &[String],
        flags: &[String],
        reverse: bool,
    ) {
        let (domain, range) = if reverse {
            (targets, sources)
        } else {
            (sources, targets)
        };
        for d in domain {
            self.add(ObjectPropertyDomain {
                ope: self.object_property(iri),
                ce: self.class_ce(d),
            });
        }
        for r in range {
            self.add(ObjectPropertyRange {
                ope: self.object_property(iri),
                ce: self.class_ce(r),
            });
        }
        for flag in flags {
            let ope = self.object_property(iri);
            match (flag.as_str(), reverse) {
                ("functional", false) | ("inverse functional", true) => {
                    self.add(FunctionalObjectProperty(ope))
                }
                ("inverse functional", false) | ("functional", true) => {
                    self.add(InverseFunctionalObjectProperty(ope))
                }
                ("symmetric", _) => self.add(SymmetricObjectProperty(ope)),
                ("asymmetric", _) => self.add(AsymmetricObjectProperty(ope)),
                ("reflexive", _) => self.add(ReflexiveObjectProperty(ope)),
                ("irreflexive", _) => self.add(IrreflexiveObjectProperty(ope)),
                ("transitive", _) => self.add(TransitiveObjectProperty(ope)),
                _ => {}
            }
        }
    }

    fn reverse_relation(
        &mut self,
        base: &str,
        forward: &str,
        sources: &[String],
        targets: &[String],
        flags: &[String],
        pair: Pair<Rule>,
    ) -> Result<(), OmlError> {
        let (annotations, name) = self.relation_name(pair);
        let reverse = format!("{}{}", self.ns, unescape_id(name.as_str()));
        self.annotate(&reverse, annotations)?;
        self.add(DeclareObjectProperty(
            self.build.object_property(reverse.as_str()),
        ));
        self.type_annotation(&reverse, "ReverseRelation");
        let ap = self.build.annotation_property(format!("{OML}relationBase"));
        self.add(AnnotationAssertion {
            subject: self.iri(&reverse).into(),
            ann: Annotation {
                ap,
                av: AnnotationValue::IRI(self.iri(base)),
                ann: Default::default(),
            },
        });
        self.add(InverseObjectProperties(
            self.build.object_property(reverse.as_str()),
            self.build.object_property(forward),
        ));
        self.relation_base(&reverse, sources, targets, flags, true);
        Ok(())
    }
}

fn flag(pair: &Pair<Rule>) -> String {
    pair.as_str()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn is_argument(p: &Pair<Rule>) -> bool {
    p.as_rule() == Rule::argument
}

fn is_literal(argument: &Pair<Rule>) -> bool {
    argument
        .clone()
        .into_inner()
        .next()
        .is_some_and(|p| p.as_rule() == Rule::literal)
}
