//! Read OML into horned-owl components.
//!
//! The mapping is that of the reference `oml2owl` tool, which defines OML's
//! semantics by translation to OWL.

use std::collections::HashMap;

use horned_owl::model::*;
use horned_owl::vocab::Facet;
use pest::Parser;
use pest::iterators::Pair;

use crate::parser::{OmlParser, Rule};

const OML: &str = "http://opencaesar.io/oml#";
const XSD: &str = "http://www.w3.org/2001/XMLSchema#";

/// Imports of these are not carried into the OWL.
const BUILT_IN: [&str; 5] = [
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

/// What a local term is, which decides how it is used elsewhere.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Kind {
    Aspect,
    Concept,
    RelationEntity,
    Scalar,
    ScalarProperty,
    AnnotationProperty,
    Relation,
}

/// Read an OML document into an ontology.
pub fn read<A, O>(text: &str, build: &Build<A>) -> Result<O, OmlError>
where
    A: ForIRI,
    O: MutableOntology<A> + Default,
{
    let oml = OmlParser::parse(Rule::oml, text)?.next().unwrap();
    let ontology = oml.into_inner().next().unwrap();
    let mut reader = Reader {
        build,
        ns: String::new(),
        prefixes: HashMap::new(),
        kinds: HashMap::new(),
        out: O::default(),
    };
    reader.ontology(ontology)?;
    Ok(reader.out)
}

struct Reader<'a, A: ForIRI, O> {
    build: &'a Build<A>,
    /// The namespace, with its trailing `#` or `/`.
    ns: String,
    /// Prefix to namespace, for this ontology and its imports.
    prefixes: HashMap<String, String>,
    /// The kind of each term declared (not `ref`d) in this ontology.
    kinds: HashMap<String, Kind>,
    out: O,
}

fn inner(pair: Pair<Rule>) -> Vec<Pair<Rule>> {
    pair.into_inner().collect()
}

fn unescape_id(text: &str) -> &str {
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
        let mut annotations = vec![];
        let mut body = None;
        for p in pair.into_inner() {
            match p.as_rule() {
                Rule::annotation => annotations.push(p),
                _ => body = Some(p),
            }
        }
        let body = body.unwrap();
        let kind = if body.as_rule() == Rule::vocabulary_box {
            "Vocabulary"
        } else {
            "Description"
        };

        let mut bundle = false;
        let mut statements = vec![];
        let mut imports = vec![];
        let mut prefix = String::new();
        for p in body.into_inner() {
            match p.as_rule() {
                Rule::kw_vocabulary | Rule::kw_description | Rule::kw_as => {}
                Rule::kw_bundle => bundle = true,
                Rule::namespace => {
                    let t = p.as_str();
                    self.ns = t[1..t.len() - 1].to_string();
                }
                Rule::id => prefix = unescape_id(p.as_str()).to_string(),
                Rule::import => imports.push(p),
                _ => statements.push(p),
            }
        }
        self.prefixes.insert(prefix.clone(), self.ns.clone());
        let mut import_iris = vec![];
        for i in imports {
            let mut ns = String::new();
            let mut p = None;
            for x in i.into_inner() {
                match x.as_rule() {
                    Rule::namespace => ns = x.as_str()[1..x.as_str().len() - 1].to_string(),
                    Rule::id => p = Some(unescape_id(x.as_str()).to_string()),
                    _ => {}
                }
            }
            if let Some(p) = p {
                self.prefixes.insert(p, ns.clone());
            }
            import_iris.push(ns);
        }

        let kind = match (kind, bundle) {
            ("Vocabulary", false) => "Vocabulary",
            ("Vocabulary", true) => "VocabularyBundle",
            (_, false) => "Description",
            (_, true) => "DescriptionBundle",
        };

        let id = self.ns[..self.ns.len() - 1].to_string();
        self.add(OntologyID {
            iri: Some(self.iri(&id)),
            viri: None,
        });
        self.ontology_annotation(
            "type",
            AnnotationValue::IRI(self.iri(&format!("{OML}{kind}"))),
        );
        self.ontology_annotation(
            "namespace",
            AnnotationValue::IRI(self.iri(&self.ns.clone())),
        );
        self.ontology_annotation(
            "prefix",
            AnnotationValue::Literal(Literal::Simple { literal: prefix }),
        );
        for ns in import_iris {
            let iri = &ns[..ns.len() - 1];
            if !BUILT_IN.contains(&iri) {
                self.add(Import(self.iri(iri)));
            }
        }
        for a in annotations {
            for ann in self.annotation(a)? {
                self.add(OntologyAnnotation(ann));
            }
        }

        // Kinds first, as restrictions refer to properties by kind.
        for s in &statements {
            self.register(s)?;
        }
        for s in statements {
            self.statement(s)?;
        }
        Ok(())
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

    fn kind_of(&self, iri: &str) -> Result<Kind, OmlError> {
        match self.kinds.get(iri) {
            Some(k) => Ok(*k),
            None => invalid(format!(
                "cannot tell what {iri} is, as it is not declared here"
            )),
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

    /// The kind of a statement and whether it is a `ref`.
    fn shape(pair: &Pair<Rule>) -> Option<(Kind, bool)> {
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

    fn term<'i>(pair: &Pair<'i, Rule>) -> Option<Pair<'i, Rule>> {
        pair.clone()
            .into_inner()
            .find(|p| matches!(p.as_rule(), Rule::id | Rule::reference))
    }

    fn register(&mut self, statement: &Pair<Rule>) -> Result<(), OmlError> {
        let (_, term) = split(statement.clone());
        let Some((kind, is_ref)) = Self::shape(&term) else {
            return Ok(());
        };
        let name = Self::term(&term).unwrap();
        let iri = if name.as_rule() == Rule::id {
            format!("{}{}", self.ns, unescape_id(name.as_str()))
        } else {
            self.reference(name)?
        };
        if !is_ref {
            self.kinds.insert(iri, kind);
        }
        Ok(())
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

fn unquote(t: &str) -> String {
    if let Some(s) = t
        .strip_prefix("\"\"\"")
        .and_then(|s| s.strip_suffix("\"\"\""))
    {
        return s.to_string();
    }
    if let Some(s) = t.strip_prefix("'''").and_then(|s| s.strip_suffix("'''")) {
        return s.to_string();
    }
    let body = &t[1..t.len() - 1];
    let mut out = String::new();
    let mut chars = body.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        match chars.next() {
            Some('n') => out.push('\n'),
            Some('t') => out.push('\t'),
            Some('r') => out.push('\r'),
            Some('b') => out.push('\u{8}'),
            Some('f') => out.push('\u{c}'),
            Some(c) => out.push(c),
            None => out.push('\\'),
        }
    }
    out
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

    fn statement(&mut self, pair: Pair<Rule>) -> Result<(), OmlError> {
        let (annotations, term) = split(pair);
        let Some((kind, is_ref)) = Self::shape(&term) else {
            return invalid(format!("not supported yet: {:?}", term.as_rule()));
        };
        let mut parts = inner(term).into_iter().filter(|p| !p.as_str().is_empty());
        let mut name = None;
        let mut rest = vec![];
        for p in parts.by_ref() {
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
        self.annotate(&iri, annotations)?;
        match kind {
            Kind::Aspect | Kind::Concept => self.entity(kind, &iri, is_ref, rest),
            Kind::RelationEntity => invalid("not supported yet: relation entity"),
            Kind::Scalar => self.scalar(&iri, is_ref, rest),
            Kind::ScalarProperty => self.scalar_property(&iri, is_ref, rest),
            Kind::AnnotationProperty => {
                if !is_ref {
                    self.add(DeclareAnnotationProperty(
                        self.build.annotation_property(iri.as_str()),
                    ));
                }
                Ok(())
            }
            Kind::Relation => self.unreified_relation(&iri, is_ref, rest),
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
        let this = self.class_ce(iri);
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
        }
        Ok(())
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

    fn restrictions(&self, block: Pair<Rule>) -> Result<Vec<ClassExpression<A>>, OmlError> {
        block.into_inner().map(|r| self.restriction(r)).collect()
    }

    fn restriction(&self, pair: Pair<Rule>) -> Result<ClassExpression<A>, OmlError> {
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
                    _ => invalid("not supported yet: anonymous instance"),
                }
            }
        }
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
            self.reverse_relation(iri, &sources, &targets, &flags, r)?;
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
        forward: &str,
        sources: &[String],
        targets: &[String],
        flags: &[String],
        pair: Pair<Rule>,
    ) -> Result<(), OmlError> {
        let (annotations, name) = {
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
        };
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
                av: AnnotationValue::IRI(self.iri(forward)),
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
