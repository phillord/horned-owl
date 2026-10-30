//! Check OML against the rules of the reference validator.
//!
//! The reader accepts OML that the reference tools reject. This is a
//! separate pass over the parse, so it costs nothing to a caller who does
//! not want it. It finds what can be told from the document alone; the
//! rules that need the terms of other ontologies are not here yet.
//!
//! The rules and their severities are those of `OmlValidator2` in
//! opencaesar/oml. `oml2owl` stops on errors and drops warnings.

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::path::Path;
use std::rc::Rc;

use pest::Parser;
use pest::iterators::Pair;

use crate::catalog::{CatalogResolver, find_catalog};
use crate::parser::{OmlParser, Rule};
use crate::reader::{
    BUILT_IN, NoResolver, OmlError, Resolver, Terms, header, resolve_ref, terms_from, terms_of,
    unescape_id,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Warning,
    Error,
}

/// A problem in a document, with the position of the construct it is about.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    /// One-based.
    pub line: usize,
    /// One-based.
    pub column: usize,
}

impl fmt::Display for Diagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let severity = match self.severity {
            Severity::Warning => "warning",
            Severity::Error => "error",
        };
        write!(
            f,
            "{}:{}: {severity}: {}",
            self.line, self.column, self.message
        )
    }
}

/// Check an OML document. A document that does not parse is an error.
///
/// Nothing is known of the terms of other ontologies, so references to them
/// are not checked; see [`validate_with`].
pub fn validate(text: &str) -> Result<Vec<Diagnostic>, OmlError> {
    validate_with(text, &NoResolver)
}

/// As [`validate`], finding the imports with `resolver`. An import that it
/// cannot find is not an error here: what refers to it goes unchecked.
pub fn validate_with(text: &str, resolver: &dyn Resolver) -> Result<Vec<Diagnostic>, OmlError> {
    let oml = OmlParser::parse(Rule::oml, text)?.next().unwrap();
    let ontology = oml.into_inner().next().unwrap();
    let own = terms_from(&header(ontology.clone()));
    let body = ontology
        .into_inner()
        .find(|p| matches!(p.as_rule(), Rule::vocabulary_box | Rule::description_box))
        .unwrap();
    let mut checker = Checker {
        out: vec![],
        ns: String::new(),
        prefixes: HashMap::new(),
        own,
        resolver,
        imported: RefCell::new(HashMap::new()),
    };
    checker.ontology(body);
    Ok(checker.out)
}

/// Check the OML file at `path`. Imports are found with the nearest
/// `catalog.xml` above it, if there is one.
pub fn validate_path(path: &Path) -> Result<Vec<Diagnostic>, OmlError> {
    let text = std::fs::read_to_string(path).map_err(|e| OmlError::Invalid(e.to_string()))?;
    match find_catalog(path).map(CatalogResolver::from_path) {
        Some(Ok(resolver)) => validate_with(&text, &resolver),
        Some(Err(e)) => Err(OmlError::Invalid(format!("catalog: {e}"))),
        None => validate(&text),
    }
}

struct Checker<'r> {
    out: Vec<Diagnostic>,
    ns: String,
    /// Prefix to namespace, for the ontology and its imports.
    prefixes: HashMap<String, String>,
    /// The terms declared in the document.
    own: Terms,
    resolver: &'r dyn Resolver,
    /// The terms of other ontologies, by namespace, loaded as needed.
    imported: RefCell<HashMap<String, Option<Rc<Terms>>>>,
}

fn has(pair: &Pair<Rule>, kind: Rule) -> bool {
    pair.clone().into_inner().any(|p| p.as_rule() == kind)
}

fn is_ref(term: &Pair<Rule>) -> bool {
    term.clone()
        .into_inner()
        .next()
        .is_some_and(|p| p.as_rule() == Rule::kw_ref)
}

fn namespace(pair: &Pair<Rule>) -> String {
    let t = pair.as_str();
    t[1..t.len() - 1].to_string()
}

/// The name a term is declared or referred to by.
fn name(term: &Pair<Rule>) -> String {
    term.clone()
        .into_inner()
        .find(|p| matches!(p.as_rule(), Rule::id | Rule::reference))
        .map(|p| unescape_id(p.as_str()).to_string())
        .unwrap_or_default()
}

impl Checker<'_> {
    fn report(&mut self, severity: Severity, at: &Pair<Rule>, message: String) {
        let (line, column) = at.as_span().start_pos().line_col();
        self.out.push(Diagnostic {
            severity,
            message,
            line,
            column,
        });
    }

    fn error(&mut self, at: &Pair<Rule>, message: String) {
        self.report(Severity::Error, at, message);
    }

    fn warning(&mut self, at: &Pair<Rule>, message: String) {
        self.report(Severity::Warning, at, message);
    }

    fn ontology(&mut self, body: Pair<Rule>) {
        let mut imports = vec![];
        let mut prefix = None;
        for p in body.clone().into_inner() {
            match p.as_rule() {
                Rule::namespace if self.ns.is_empty() => self.ns = namespace(&p),
                Rule::id if prefix.is_none() => prefix = Some(unescape_id(p.as_str()).to_string()),
                Rule::import => imports.push(p),
                _ => {}
            }
        }
        if let Some(prefix) = prefix {
            self.prefixes.insert(prefix, self.ns.clone());
        }
        for import in &imports {
            if let Some((ns, Some(prefix))) = import_of(import) {
                self.prefixes.insert(prefix, ns);
            }
        }
        self.imports(&body, &imports);
        for p in body.into_inner().flatten() {
            self.node(p);
        }
    }

    /// The terms of the ontology with this namespace, if it can be found.
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

    /// Report a reference to a member that its ontology does not declare.
    fn reference(&mut self, p: &Pair<Rule>) {
        let target = if p.as_rule() == Rule::reference {
            p.clone().into_inner().next().unwrap()
        } else {
            p.clone()
        };
        let text = target.as_str();
        let declared = match target.as_rule() {
            Rule::id => Some(
                self.own
                    .names
                    .contains(&format!("{}{}", self.ns, unescape_id(text))),
            ),
            // A prefix that nothing defines can never be resolved.
            Rule::qname => match resolve_ref(&self.prefixes, &self.ns, target.clone()) {
                Some(iri) => self.declared(&iri),
                None => Some(false),
            },
            _ => resolve_ref(&self.prefixes, &self.ns, target.clone())
                .and_then(|iri| self.declared(&iri)),
        };
        if declared == Some(false) {
            self.error(p, format!("Couldn't resolve reference to '{text}'"));
        }
    }

    /// Whether the ontology of `iri` declares it, if that can be told.
    fn declared(&self, iri: &str) -> Option<bool> {
        let ns = match iri.rfind(['#', '/']) {
            Some(i) => &iri[..=i],
            None => return None,
        };
        if ns == self.ns {
            return Some(self.own.names.contains(iri));
        }
        if BUILT_IN.contains(&&ns[..ns.len() - 1]) {
            return None;
        }
        self.imported(ns).map(|t| t.names.contains(iri))
    }

    fn imports(&mut self, body: &Pair<Rule>, imports: &[Pair<Rule>]) {
        let mut used_prefixes = HashSet::new();
        let mut iris = vec![];
        for p in body.clone().into_inner().flatten() {
            match p.as_rule() {
                Rule::qname => {
                    if let Some((prefix, _)) = p.as_str().split_once(':') {
                        used_prefixes.insert(unescape_id(prefix).to_string());
                    }
                }
                Rule::iri => iris.push(p.as_str().to_string()),
                _ => {}
            }
        }
        let mut count: HashMap<String, usize> = HashMap::new();
        for import in imports {
            if let Some((ns, _)) = import_of(import) {
                *count.entry(ns).or_default() += 1;
            }
        }
        for import in imports {
            let Some((ns, prefix)) = import_of(import) else {
                continue;
            };
            if count[&ns] > 1 {
                self.warning(import, format!("Import <{ns}> is a duplicate"));
            }
            if ns == self.ns {
                self.warning(import, format!("Import <{ns}> is to self"));
            }
            if let Some(prefix) = prefix {
                let used = used_prefixes.contains(&prefix)
                    || iris.iter().any(|i| i.starts_with(&format!("<{ns}")));
                if !used {
                    self.warning(import, format!("Import <{ns}> with prefix is not used"));
                }
            }
        }
    }

    fn node(&mut self, p: Pair<Rule>) {
        match p.as_rule() {
            Rule::reference | Rule::cross_reference => self.reference(&p),
            Rule::relation_entity => self.relation_entity(&p),
            Rule::unreified_relation => {
                if is_ref(&p) && has(&p, Rule::reverse_relation) {
                    self.error(&p, no_reverse(&p));
                }
            }
            Rule::rule => self.rule(&p),
            Rule::relation_instance => self.relation_instance(&p),
            Rule::relation_entity_predicate => self.relation_entity_predicate(&p),
            Rule::property_predicate => self.property_predicate(&p),
            _ => {}
        }
        if matches!(
            p.as_rule(),
            Rule::aspect
                | Rule::concept
                | Rule::relation_entity
                | Rule::scalar_property
                | Rule::unreified_relation
                | Rule::annotation_property
        ) {
            self.specializes_itself(&p);
        }
    }

    fn relation_entity(&mut self, p: &Pair<Rule>) {
        let forward = has(p, Rule::forward_relation);
        if is_ref(p) {
            if forward {
                self.error(
                    p,
                    format!("Cannot name a forward relation on a ref to {}", name(p)),
                );
            }
            if has(p, Rule::reverse_relation) {
                self.error(p, no_reverse(p));
            }
        } else if !forward {
            self.error(p, format!("Must name a forward relation on {}", name(p)));
        }
    }

    fn rule(&mut self, p: &Pair<Rule>) {
        let antecedent = has(p, Rule::antecedent);
        let consequent = has(p, Rule::consequent);
        if is_ref(p) {
            if antecedent {
                self.error(
                    p,
                    format!(
                        "Rule {} cannot respecify a predicate as antecedent",
                        name(p)
                    ),
                );
            }
            if consequent {
                self.error(
                    p,
                    format!(
                        "Rule {} cannot respecify a predicate as consequent",
                        name(p)
                    ),
                );
            }
        } else if !antecedent {
            self.error(
                p,
                format!(
                    "Rule {} needs to specify some predicates as antecedent",
                    name(p)
                ),
            );
        } else if !consequent {
            self.error(
                p,
                format!(
                    "Rule {} needs to specify some predicates as consequent",
                    name(p)
                ),
            );
        }
    }

    fn relation_instance(&mut self, p: &Pair<Rule>) {
        if is_ref(p) {
            return;
        }
        if !has(p, Rule::from_clause) {
            self.error(
                p,
                format!(
                    "Relation instance {} needs to specify at least one source",
                    name(p)
                ),
            );
        } else if !has(p, Rule::to_clause) {
            self.error(
                p,
                format!(
                    "Relation instance {} needs to specify at least one target",
                    name(p)
                ),
            );
        }
    }

    fn relation_entity_predicate(&mut self, p: &Pair<Rule>) {
        let ordinal = ["first", "second", "third"];
        let arguments = p
            .clone()
            .into_inner()
            .filter(|a| a.as_rule() == Rule::argument);
        for (i, a) in arguments.enumerate() {
            if is_literal(&a) {
                self.error(
                    &a,
                    format!(
                        "Relation entity predicate cannot have a literal as a {} argument",
                        ordinal[i.min(2)]
                    ),
                );
            }
        }
    }

    fn property_predicate(&mut self, p: &Pair<Rule>) {
        let first = p
            .clone()
            .into_inner()
            .find(|a| a.as_rule() == Rule::argument);
        if let Some(a) = first.filter(is_literal) {
            self.error(
                &a,
                "Property predicate cannot have a literal as a first argument".into(),
            );
        }
    }

    fn specializes_itself(&mut self, term: &Pair<Rule>) {
        let Some(this) = self.iri_of(
            term.clone()
                .into_inner()
                .find(|p| matches!(p.as_rule(), Rule::id | Rule::reference)),
        ) else {
            return;
        };
        for p in term.clone().into_inner() {
            let refs: Vec<Pair<Rule>> = match p.as_rule() {
                Rule::entity_specialization | Rule::entity_equivalence => p
                    .into_inner()
                    .flat_map(|x| std::iter::once(x.clone()).chain(x.into_inner()))
                    .filter(|x| x.as_rule() == Rule::reference)
                    .collect(),
                Rule::property_specialization | Rule::property_equivalence => p
                    .into_inner()
                    .filter(|x| x.as_rule() == Rule::reference)
                    .collect(),
                _ => continue,
            };
            for r in refs {
                if self.iri_of(Some(r.clone())).as_deref() == Some(&this) {
                    self.warning(&r, format!("Term {} specializes itself", name(term)));
                }
            }
        }
    }

    fn iri_of(&self, p: Option<Pair<Rule>>) -> Option<String> {
        let p = p?;
        if p.as_rule() == Rule::id {
            return Some(format!("{}{}", self.ns, unescape_id(p.as_str())));
        }
        resolve_ref(&self.prefixes, &self.ns, p)
    }
}

fn is_literal(argument: &Pair<Rule>) -> bool {
    argument
        .clone()
        .into_inner()
        .next()
        .is_some_and(|p| p.as_rule() == Rule::literal)
}

fn no_reverse(p: &Pair<Rule>) -> String {
    format!("Cannot name a reverse relation on a ref to {}", name(p))
}

/// The namespace of an import and its prefix.
fn import_of(import: &Pair<Rule>) -> Option<(String, Option<String>)> {
    let mut ns = None;
    let mut prefix = None;
    for x in import.clone().into_inner() {
        match x.as_rule() {
            Rule::namespace => ns = Some(namespace(&x)),
            Rule::id => prefix = Some(unescape_id(x.as_str()).to_string()),
            _ => {}
        }
    }
    Some((ns?, prefix))
}
