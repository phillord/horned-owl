//! Turn the syntax tree into model values.
//!
//! A port of `io/omn/reader/from_pair.rs`: the same decisions in the same
//! order, including the declaration-aware choice between object and data
//! restrictions, so both readers build the same components.
use std::collections::{BTreeSet, HashSet};

use curie::{Curie, PrefixMapping};

use crate::io::ofn::reader::error::PErr;
use crate::model::*;
use crate::vocab::{Facet as VocabFacet, OWL};

use super::syntax::*;

pub type CR<'a, T> = Result<T, PErr<'a>>;

/// `DataProperty:` and `Datatype:` frame subjects seen so far.
pub struct Declarations<A: ForIRI> {
    pub data_props: HashSet<IRI<A>>,
    pub datatypes: HashSet<IRI<A>>,
}

impl<A: ForIRI> Default for Declarations<A> {
    fn default() -> Self {
        Declarations {
            data_props: HashSet::new(),
            datatypes: HashSet::new(),
        }
    }
}

/// The components a unit produced, in order.
pub struct Out<A: ForIRI>(pub Vec<AnnotatedComponent<A>>);

impl<A: ForIRI> Out<A> {
    pub fn insert<C: Into<AnnotatedComponent<A>>>(&mut self, c: C) {
        self.0.push(c.into());
    }
}

pub struct Conv<'c, 'a, A: ForIRI> {
    pub build: &'c Build<A>,
    pub prefixes: &'c PrefixMapping,
    pub decls: &'c Declarations<A>,
    /// The text of the unit being converted, which locates errors that have
    /// no better position.
    pub unit: &'a str,
}

type AnnItem<A, T> = (BTreeSet<Annotation<A>>, T);

/// The namespace of a prefix that the OWL 2 Structural Specification
/// (section 2.4) predefines, so that a document may use it undeclared.
fn predefined(prefix: &str) -> Option<&'static str> {
    match prefix {
        "owl" => Some("http://www.w3.org/2002/07/owl#"),
        "rdf" => Some("http://www.w3.org/1999/02/22-rdf-syntax-ns#"),
        "rdfs" => Some("http://www.w3.org/2000/01/rdf-schema#"),
        "xsd" => Some("http://www.w3.org/2001/XMLSchema#"),
        _ => None,
    }
}

fn invalid<'a, T>(at: &'a str, msg: impl Into<String>) -> CR<'a, T> {
    Err(PErr::semantic(at, at.len().min(1), msg))
}

fn facet_from_symbol(s: &str) -> Option<VocabFacet> {
    match s {
        ">=" => Some(VocabFacet::MinInclusive),
        "<=" => Some(VocabFacet::MaxInclusive),
        ">" => Some(VocabFacet::MinExclusive),
        "<" => Some(VocabFacet::MaxExclusive),
        _ => match s.to_ascii_lowercase().as_str() {
            "length" => Some(VocabFacet::Length),
            "minlength" => Some(VocabFacet::MinLength),
            "maxlength" => Some(VocabFacet::MaxLength),
            "pattern" => Some(VocabFacet::Pattern),
            "langrange" => Some(VocabFacet::LangRange),
            "totaldigits" => Some(VocabFacet::TotalDigits),
            "fractiondigits" => Some(VocabFacet::FractionDigits),
            _ => None,
        },
    }
}

const BUILTIN_NAMESPACES: [&str; 2] = [
    "http://www.w3.org/2003/11/swrlb#",
    "http://sqwrl.stanford.edu/ontologies/built-ins/3.4/sqwrl.owl#",
];

enum SwrlArgKind<A: ForIRI> {
    Var(Variable<A>),
    Lit(Literal<A>),
    Ind(Individual<A>),
}

impl<'c, 'a, A: ForIRI> Conv<'c, 'a, A> {
    fn is_data_prop(&self, iri: &IRI<A>) -> bool {
        self.decls.data_props.contains(iri)
    }

    fn is_datatype(&self, iri: &IRI<A>) -> bool {
        self.decls.datatypes.contains(iri)
    }

    // -- terminals ---------------------------------------------------------

    pub fn iri(&self, x: &Iri<'a>) -> CR<'a, IRI<A>> {
        let (curie, bare) = match x.kind {
            IriKind::Full(s) => return Ok(self.build.iri(s)),
            IriKind::Abbrev(prefix, local) => (Curie::new(Some(prefix), local), false),
            IriKind::Simple(local) => (Curie::new(Some(""), local), true),
        };
        match self.prefixes.expand_curie(&curie) {
            Ok(s) => Ok(self.build.iri(s)),
            Err(curie::ExpansionError::Invalid) => match x.kind {
                IriKind::Abbrev(prefix, local) if predefined(prefix).is_some() => Ok(self
                    .build
                    .iri(format!("{}{local}", predefined(prefix).unwrap_or_default()))),
                _ => Err(PErr::semantic(x.text, x.text.len(), "undefined prefix")),
            },
            Err(curie::ExpansionError::MissingDefault) => Err(PErr::semantic(
                x.text,
                x.text.len(),
                if bare {
                    "bare local name but no default prefix is declared"
                } else {
                    "missing default prefix"
                },
            )),
        }
    }

    fn individual(&self, x: &Ind<'a>) -> CR<'a, Individual<A>> {
        Ok(match x {
            Ind::Anon(label) => {
                let id = label.strip_prefix("_:").unwrap_or(label);
                Individual::Anonymous(self.build.anon(id))
            }
            Ind::Named(iri) => Individual::Named(NamedIndividual(self.iri(iri)?)),
        })
    }

    fn literal(&self, x: &Lit<'a>) -> CR<'a, Literal<A>> {
        const XSD: &str = "http://www.w3.org/2001/XMLSchema#";
        let typed = |text: &str, dt: &str| Literal::Datatype {
            literal: text.to_string(),
            datatype_iri: self.build.iri(format!("{XSD}{dt}")),
        };
        Ok(match x {
            Lit::Typed(s, dt) => Literal::Datatype {
                literal: s.clone(),
                datatype_iri: self.iri(dt)?,
            },
            Lit::Lang(s, tag) => Literal::Language {
                literal: s.clone(),
                lang: tag.trim().to_string(),
            },
            Lit::Simple(s) => Literal::Simple { literal: s.clone() },
            Lit::Integer(t) => typed(t, "integer"),
            Lit::Decimal(t) => typed(t, "decimal"),
            Lit::Float(t) => typed(t, "float"),
            Lit::Boolean(t) => typed(t, "boolean"),
        })
    }

    fn ope(&self, x: &Ope<'a>) -> CR<'a, ObjectPropertyExpression<A>> {
        let op = ObjectProperty(self.iri(&x.iri)?);
        Ok(if x.inverse {
            ObjectPropertyExpression::InverseObjectProperty(op)
        } else {
            ObjectPropertyExpression::ObjectProperty(op)
        })
    }

    fn cardinality(&self, text: &'a str) -> CR<'a, u32> {
        text.parse()
            .or_else(|_| invalid(text, "invalid cardinality"))
    }

    // -- data ranges -------------------------------------------------------

    pub fn data_range(&self, x: &SynDataRange<'a>) -> CR<'a, DataRange<A>> {
        let mut conjs =
            x.0.iter()
                .map(|c| self.data_conjunction(c))
                .collect::<CR<'a, Vec<_>>>()?;
        Ok(if conjs.len() == 1 {
            conjs.remove(0)
        } else {
            DataRange::DataUnionOf(conjs)
        })
    }

    fn data_conjunction(&self, x: &DataConj<'a>) -> CR<'a, DataRange<A>> {
        let mut prims =
            x.0.iter()
                .map(|p| self.data_primary(p))
                .collect::<CR<'a, Vec<_>>>()?;
        Ok(if prims.len() == 1 {
            prims.remove(0)
        } else {
            DataRange::DataIntersectionOf(prims)
        })
    }

    fn data_primary(&self, x: &DataPrim<'a>) -> CR<'a, DataRange<A>> {
        let atomic = self.data_atomic(&x.atom)?;
        Ok(if x.not {
            DataRange::DataComplementOf(Box::new(atomic))
        } else {
            atomic
        })
    }

    fn data_atomic(&self, x: &DataAtom<'a>) -> CR<'a, DataRange<A>> {
        Ok(match x {
            DataAtom::OneOf(lits) => DataRange::DataOneOf(
                lits.iter()
                    .map(|l| self.literal(l))
                    .collect::<CR<'a, _>>()?,
            ),
            DataAtom::Restriction(dt, facets) => DataRange::DatatypeRestriction(
                Datatype(self.iri(dt)?),
                facets
                    .iter()
                    .map(|f| self.facet(f))
                    .collect::<CR<'a, _>>()?,
            ),
            DataAtom::Paren(d) => self.data_range(d)?,
            DataAtom::Datatype(iri) => DataRange::Datatype(Datatype(self.iri(iri)?)),
        })
    }

    fn facet(&self, x: &Facet<'a>) -> CR<'a, FacetRestriction<A>> {
        let f = match facet_from_symbol(x.symbol) {
            Some(f) => f,
            None => {
                return invalid(x.symbol, format!("unknown facet symbol: {}", x.symbol));
            }
        };
        Ok(FacetRestriction {
            f,
            l: self.literal(&x.lit)?,
        })
    }

    // -- class expressions -------------------------------------------------

    pub fn description(&self, x: &Desc<'a>) -> CR<'a, ClassExpression<A>> {
        let mut ces =
            x.0.iter()
                .map(|c| self.conjunction(c))
                .collect::<CR<'a, Vec<_>>>()?;
        Ok(if ces.len() == 1 {
            ces.pop().unwrap()
        } else {
            ClassExpression::ObjectUnionOf(ces)
        })
    }

    fn conjunction(&self, x: &Conj<'a>) -> CR<'a, ClassExpression<A>> {
        match x {
            Conj::That(cls, ops) => {
                let mut ces = vec![ClassExpression::Class(Class(self.iri(cls)?))];
                for (not, r) in ops {
                    let ce = self.restriction(r)?;
                    ces.push(if *not {
                        ClassExpression::ObjectComplementOf(Box::new(ce))
                    } else {
                        ce
                    });
                }
                Ok(ClassExpression::ObjectIntersectionOf(ces))
            }
            Conj::And(prims) => {
                let mut ces = prims
                    .iter()
                    .map(|p| self.primary(p))
                    .collect::<CR<'a, Vec<_>>>()?;
                Ok(if ces.len() == 1 {
                    ces.pop().unwrap()
                } else {
                    ClassExpression::ObjectIntersectionOf(ces)
                })
            }
        }
    }

    fn primary(&self, x: &Prim<'a>) -> CR<'a, ClassExpression<A>> {
        let ce = match &x.body {
            PrimBody::Restr(r) => self.restriction(r)?,
            PrimBody::OneOf(inds) => ClassExpression::ObjectOneOf(
                inds.iter()
                    .map(|i| self.individual(i))
                    .collect::<CR<'a, _>>()?,
            ),
            PrimBody::Paren(d) => self.description(d)?,
            PrimBody::Class(iri) => ClassExpression::Class(Class(self.iri(iri)?)),
        };
        Ok(if x.not {
            ClassExpression::ObjectComplementOf(Box::new(ce))
        } else {
            ce
        })
    }

    fn restriction(&self, r: &Restr<'a>) -> CR<'a, ClassExpression<A>> {
        match &r.prop {
            Prop::Data(dp) => self.data_restriction(r, dp),
            Prop::Obj(ope) => self.object_restriction(r, ope),
        }
    }

    fn data_restriction(&self, r: &Restr<'a>, dp: &Iri<'a>) -> CR<'a, ClassExpression<A>> {
        let dp = DataProperty(self.iri(dp)?);
        if r.kw == RKw::Value {
            let Filler::Lit(l) = &r.filler else {
                unreachable!("a data value restriction has a literal")
            };
            return Ok(ClassExpression::DataHasValue {
                dp,
                l: self.literal(l)?,
            });
        }
        let n = match r.card {
            Some(c) => Some(self.cardinality(c)?),
            None => None,
        };
        let dr = match &r.filler {
            Filler::Dr(d) => self.data_range(d)?,
            _ => DataRange::Datatype(Datatype(
                self.build
                    .iri("http://www.w3.org/2000/01/rdf-schema#Literal"),
            )),
        };
        Ok(match r.kw {
            RKw::Some => ClassExpression::DataSomeValuesFrom { dp, dr },
            RKw::Only => ClassExpression::DataAllValuesFrom { dp, dr },
            RKw::Min => ClassExpression::DataMinCardinality {
                n: n.unwrap(),
                dp,
                dr,
            },
            RKw::Max => ClassExpression::DataMaxCardinality {
                n: n.unwrap(),
                dp,
                dr,
            },
            RKw::Exactly => ClassExpression::DataExactCardinality {
                n: n.unwrap(),
                dp,
                dr,
            },
            RKw::Value | RKw::SelfKw => unreachable!("not a data arm"),
        })
    }

    fn object_restriction(
        &self,
        r: &Restr<'a>,
        ope_syntax: &Ope<'a>,
    ) -> CR<'a, ClassExpression<A>> {
        let ope = self.ope(ope_syntax)?;
        // A plain property that was declared as a data property.
        let prop_is_data = matches!(&ope,
            ObjectPropertyExpression::ObjectProperty(ObjectProperty(iri))
                if self.is_data_prop(iri));
        let plain = |ope: &ObjectPropertyExpression<A>| -> Option<IRI<A>> {
            match ope {
                ObjectPropertyExpression::ObjectProperty(ObjectProperty(iri)) => Some(iri.clone()),
                _ => None,
            }
        };
        let bare_datatype_iri = |bce: &ClassExpression<A>| -> Option<IRI<A>> {
            if let ClassExpression::Class(Class(iri)) = bce
                && self.is_datatype(iri)
            {
                return Some(iri.clone());
            }
            None
        };
        let negated_datatype = |bce: &ClassExpression<A>| -> Option<IRI<A>> {
            if let ClassExpression::ObjectComplementOf(inner) = bce
                && let ClassExpression::Class(Class(iri)) = &**inner
                && (prop_is_data || self.is_datatype(iri))
            {
                return Some(iri.clone());
            }
            None
        };
        let data_range_filler = |bce: &ClassExpression<A>| -> Option<DataRange<A>> {
            if let ClassExpression::Class(Class(iri)) = bce
                && (prop_is_data || self.is_datatype(iri))
            {
                return Some(DataRange::Datatype(Datatype(iri.clone())));
            }
            negated_datatype(bce).map(|iri| {
                DataRange::DataComplementOf(Box::new(DataRange::Datatype(Datatype(iri))))
            })
        };
        let filler = |this: &Self| -> CR<'a, Option<Box<ClassExpression<A>>>> {
            match &r.filler {
                Filler::Prim(p) => Ok(Some(Box::new(this.primary(p)?))),
                _ => Ok(None),
            }
        };

        match r.kw {
            RKw::Some | RKw::Only => {
                let is_some = r.kw == RKw::Some;
                let bce = filler(self)?.expect("some and only have a filler");
                let object = |ope, bce| {
                    if is_some {
                        ClassExpression::ObjectSomeValuesFrom { ope, bce }
                    } else {
                        ClassExpression::ObjectAllValuesFrom { ope, bce }
                    }
                };
                let data = |dp, dr| {
                    if is_some {
                        ClassExpression::DataSomeValuesFrom { dp, dr }
                    } else {
                        ClassExpression::DataAllValuesFrom { dp, dr }
                    }
                };
                if let Some(dt_iri) = bare_datatype_iri(&bce) {
                    let Some(iri) = plain(&ope) else {
                        return invalid(r.text, "an inverse property has no datatype filler");
                    };
                    Ok(data(
                        DataProperty(iri),
                        DataRange::Datatype(Datatype(dt_iri)),
                    ))
                } else if let Some(dt_iri) = negated_datatype(&bce) {
                    match plain(&ope) {
                        Some(iri) => Ok(data(
                            DataProperty(iri),
                            DataRange::DataComplementOf(Box::new(DataRange::Datatype(Datatype(
                                dt_iri,
                            )))),
                        )),
                        None => Ok(object(ope, bce)),
                    }
                } else if prop_is_data {
                    if let ClassExpression::Class(Class(filler_iri)) = *bce {
                        let iri = plain(&ope).expect("declared data property is plain");
                        Ok(data(
                            DataProperty(iri),
                            DataRange::Datatype(Datatype(filler_iri)),
                        ))
                    } else {
                        Ok(object(ope, bce))
                    }
                } else {
                    Ok(object(ope, bce))
                }
            }
            RKw::Value => {
                let Filler::Ind(i) = &r.filler else {
                    unreachable!("a value restriction has an individual")
                };
                Ok(ClassExpression::ObjectHasValue {
                    ope,
                    i: self.individual(i)?,
                })
            }
            RKw::SelfKw => Ok(ClassExpression::ObjectHasSelf(ope)),
            RKw::Min | RKw::Max | RKw::Exactly => {
                let n = self.cardinality(r.card.expect("a cardinality"))?;
                let object = |ope, bce: Box<ClassExpression<A>>| match r.kw {
                    RKw::Min => ClassExpression::ObjectMinCardinality { n, ope, bce },
                    RKw::Max => ClassExpression::ObjectMaxCardinality { n, ope, bce },
                    _ => ClassExpression::ObjectExactCardinality { n, ope, bce },
                };
                match filler(self)? {
                    Some(bce) => match data_range_filler(&bce) {
                        Some(dr) => match plain(&ope) {
                            Some(iri) => {
                                let dp = DataProperty(iri);
                                Ok(match r.kw {
                                    RKw::Min => ClassExpression::DataMinCardinality { n, dp, dr },
                                    RKw::Max => ClassExpression::DataMaxCardinality { n, dp, dr },
                                    _ => ClassExpression::DataExactCardinality { n, dp, dr },
                                })
                            }
                            None => invalid(r.text, "data property cannot be inverse"),
                        },
                        None => Ok(object(ope, bce)),
                    },
                    None => {
                        let bce =
                            Box::new(ClassExpression::Class(Class(self.build.iri(OWL::Thing))));
                        Ok(object(ope, bce))
                    }
                }
            }
        }
    }

    // -- annotations -------------------------------------------------------

    fn annotation_value(&self, t: &Target<'a>) -> CR<'a, AnnotationValue<A>> {
        Ok(match t {
            Target::Lit(l) => AnnotationValue::Literal(self.literal(l)?),
            Target::Iri(i) => AnnotationValue::IRI(self.iri(i)?),
            Target::Anon(label) => {
                let id = label.strip_prefix("_:").unwrap_or(label);
                AnnotationValue::AnonymousIndividual(self.build.anon(id))
            }
        })
    }

    fn annotation(&self, e: &AnnEntry<'a>) -> CR<'a, Annotation<A>> {
        let ann = match &e.nested {
            Some(n) => self.annotations(n)?.into_iter().collect(),
            None => BTreeSet::new(),
        };
        let ap = AnnotationProperty(self.iri(&e.property)?);
        let av = self.annotation_value(&e.target)?;
        Ok(Annotation { ap, av, ann })
    }

    pub fn annotations(&self, x: &Annotations<'a>) -> CR<'a, Vec<Annotation<A>>> {
        x.0.iter().map(|e| self.annotation(e)).collect()
    }

    fn ann_set(&self, x: &Option<Annotations<'a>>) -> CR<'a, BTreeSet<Annotation<A>>> {
        match x {
            Some(a) => Ok(self.annotations(a)?.into_iter().collect()),
            None => Ok(BTreeSet::new()),
        }
    }

    /// Fold a list into per-item annotations and items.
    fn items<T, U>(
        &self,
        list: &AnnList<'a, T>,
        mut item: impl FnMut(&T) -> CR<'a, U>,
    ) -> CR<'a, Vec<AnnItem<A, U>>> {
        let mut out = Vec::new();
        for (ann, t) in &list.0 {
            let set = self.ann_set(ann)?;
            out.push((set, item(t)?));
        }
        Ok(out)
    }

    fn bind_leading_to_first<T>(ann: BTreeSet<Annotation<A>>, list: &mut [AnnItem<A, T>]) {
        if let Some(first) = list.first_mut() {
            first.0.extend(ann);
        }
    }

    fn merge_list_ann<T>(
        ann: &mut BTreeSet<Annotation<A>>,
        list: Vec<AnnItem<A, T>>,
        items: &mut Vec<T>,
    ) {
        for (item_ann, item) in list {
            ann.extend(item_ann);
            items.push(item);
        }
    }

    fn entity_annotation_assertion(
        subject: AnnotationSubject<A>,
        mut entry: Annotation<A>,
    ) -> AnnotatedComponent<A> {
        let axiom_ann = std::mem::take(&mut entry.ann);
        AnnotatedComponent {
            component: Component::AnnotationAssertion(AnnotationAssertion {
                subject,
                ann: entry,
            }),
            ann: axiom_ann,
        }
    }

    fn desc_list(&self, b: &Body<'a>) -> CR<'a, Vec<AnnItem<A, ClassExpression<A>>>> {
        match b {
            Body::Descs(l) => self.items(l, |d| self.description(d)),
            _ => unreachable!("clause body"),
        }
    }

    fn ope_list(&self, b: &Body<'a>) -> CR<'a, Vec<AnnItem<A, ObjectPropertyExpression<A>>>> {
        match b {
            Body::Opes(l) => self.items(l, |o| self.ope(o)),
            _ => unreachable!("clause body"),
        }
    }

    fn iri_list(&self, b: &Body<'a>) -> CR<'a, Vec<AnnItem<A, IRI<A>>>> {
        match b {
            Body::Iris(l) => self.items(l, |i| self.iri(i)),
            _ => unreachable!("clause body"),
        }
    }

    fn ind_list(&self, b: &Body<'a>) -> CR<'a, Vec<AnnItem<A, Individual<A>>>> {
        match b {
            Body::Inds(l) => self.items(l, |i| self.individual(i)),
            _ => unreachable!("clause body"),
        }
    }

    // -- units -------------------------------------------------------------

    pub fn frame(&self, f: &Frame<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        match f.kind {
            FrameKind::Class => self.class_frame(f, out),
            FrameKind::ObjectProperty => self.object_property_frame(f, out),
            FrameKind::DataProperty => self.data_property_frame(f, out),
            FrameKind::AnnotationProperty => self.annotation_property_frame(f, out),
            FrameKind::Individual => self.individual_frame(f, out),
            FrameKind::Datatype => self.datatype_frame(f, out),
        }
    }

    /// The `Annotations:` of a clause that has a keyword, and its body.
    fn clause_ann(&self, c: &Clause<'a>) -> CR<'a, BTreeSet<Annotation<A>>> {
        if c.kw == Ck::Annotations {
            Ok(BTreeSet::new())
        } else {
            self.ann_set(&c.ann)
        }
    }

    fn entity_annotations(
        &self,
        c: &Clause<'a>,
        subject: &AnnotationSubject<A>,
        out: &mut Out<A>,
    ) -> CR<'a, ()> {
        let Body::Annotations(a) = &c.body else {
            unreachable!("annotations clause")
        };
        for item in self.annotations(a)? {
            out.0
                .push(Self::entity_annotation_assertion(subject.clone(), item));
        }
        Ok(())
    }

    fn class_frame(&self, f: &Frame<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        let decl_ann = self.ann_set(&f.subject_ann)?;
        let Subject::Desc(d) = &f.subject else {
            unreachable!("class subject")
        };
        let subject_ce = self.description(d)?;
        let atomic_iri: Option<IRI<A>> = if let ClassExpression::Class(Class(ref iri)) = subject_ce
        {
            Some(iri.clone())
        } else {
            None
        };
        if let Some(ref iri) = atomic_iri {
            out.insert(AnnotatedComponent {
                component: Component::DeclareClass(DeclareClass(Class(iri.clone()))),
                ann: decl_ann,
            });
        }
        for clause in &f.clauses {
            let mut ann = self.clause_ann(clause)?;
            match clause.kw {
                Ck::Annotations => {
                    if let Some(ref iri) = atomic_iri {
                        self.entity_annotations(clause, &AnnotationSubject::IRI(iri.clone()), out)?;
                    }
                }
                Ck::SubClassOf => {
                    let mut list = self.desc_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, sup) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::SubClassOf(SubClassOf {
                                sub: subject_ce.clone(),
                                sup,
                            }),
                            ann: item_ann,
                        });
                    }
                }
                Ck::EquivalentTo => {
                    let mut list = self.desc_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, ce) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::EquivalentClasses(EquivalentClasses(vec![
                                subject_ce.clone(),
                                ce,
                            ])),
                            ann: item_ann,
                        });
                    }
                }
                Ck::DisjointWith => {
                    let mut list = self.desc_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, ce) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::DisjointClasses(DisjointClasses(vec![
                                subject_ce.clone(),
                                ce,
                            ])),
                            ann: item_ann,
                        });
                    }
                }
                Ck::DisjointUnionOf => {
                    if let Some(ref iri) = atomic_iri {
                        let mut items = Vec::new();
                        Self::merge_list_ann(&mut ann, self.desc_list(&clause.body)?, &mut items);
                        out.insert(AnnotatedComponent {
                            component: Component::DisjointUnion(DisjointUnion(
                                Class(iri.clone()),
                                items,
                            )),
                            ann,
                        });
                    }
                }
                Ck::HasKey => {
                    if let Some(ref iri) = atomic_iri {
                        let Body::Keys(keys) = &clause.body else {
                            unreachable!("keys")
                        };
                        let mut vpe = Vec::new();
                        for k in keys {
                            let ope = self.ope(k)?;
                            let pe = match &ope {
                                ObjectPropertyExpression::ObjectProperty(ObjectProperty(
                                    key_iri,
                                )) if self.is_data_prop(key_iri) => {
                                    PropertyExpression::DataProperty(DataProperty(key_iri.clone()))
                                }
                                _ => PropertyExpression::ObjectPropertyExpression(ope),
                            };
                            vpe.push(pe);
                        }
                        out.insert(AnnotatedComponent {
                            component: Component::HasKey(HasKey {
                                ce: ClassExpression::Class(Class(iri.clone())),
                                vpe,
                            }),
                            ann,
                        });
                    }
                }
                _ => unreachable!("class clause"),
            }
        }
        Ok(())
    }

    fn object_property_frame(&self, f: &Frame<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        let Subject::Ope(o) = &f.subject else {
            unreachable!("object property subject")
        };
        let subject_ope = self.ope(o)?;
        let subject: Option<IRI<A>> = match &subject_ope {
            ObjectPropertyExpression::ObjectProperty(ObjectProperty(iri)) => Some(iri.clone()),
            _ => None,
        };
        if let Some(iri) = &subject {
            out.insert(DeclareObjectProperty(ObjectProperty(iri.clone())));
        }
        for clause in &f.clauses {
            let ann = self.clause_ann(clause)?;
            match clause.kw {
                Ck::Annotations => {
                    if let Some(iri) = &subject {
                        self.entity_annotations(clause, &AnnotationSubject::IRI(iri.clone()), out)?;
                    }
                }
                Ck::SubPropertyChain => {
                    let Body::Chain(c) = &clause.body else {
                        unreachable!("chain")
                    };
                    let chain = c.iter().map(|o| self.ope(o)).collect::<CR<'a, Vec<_>>>()?;
                    out.insert(AnnotatedComponent {
                        component: Component::SubObjectPropertyOf(SubObjectPropertyOf {
                            sub: SubObjectPropertyExpression::ObjectPropertyChain(chain),
                            sup: subject_ope.clone(),
                        }),
                        ann,
                    });
                }
                Ck::SubPropertyOf => {
                    let mut list = self.ope_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, sup) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::SubObjectPropertyOf(SubObjectPropertyOf {
                                sub: SubObjectPropertyExpression::ObjectPropertyExpression(
                                    subject_ope.clone(),
                                ),
                                sup,
                            }),
                            ann: item_ann,
                        });
                    }
                }
                Ck::EquivalentTo => {
                    let mut list = self.ope_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, sup) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::EquivalentObjectProperties(
                                EquivalentObjectProperties(vec![subject_ope.clone(), sup]),
                            ),
                            ann: item_ann,
                        });
                    }
                }
                Ck::DisjointWith => {
                    let mut list = self.ope_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, sup) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::DisjointObjectProperties(
                                DisjointObjectProperties(vec![subject_ope.clone(), sup]),
                            ),
                            ann: item_ann,
                        });
                    }
                }
                Ck::InverseOf => {
                    let mut list = self.ope_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, inv) in list {
                        match (&subject, inv) {
                            (Some(subj_iri), ObjectPropertyExpression::ObjectProperty(p)) => {
                                out.insert(AnnotatedComponent {
                                    component: Component::InverseObjectProperties(
                                        InverseObjectProperties(
                                            ObjectProperty(subj_iri.clone()),
                                            p,
                                        ),
                                    ),
                                    ann: item_ann,
                                });
                            }
                            _ => {
                                return invalid(
                                    self.unit,
                                    "InverseOf: expected named object properties on both sides",
                                );
                            }
                        }
                    }
                }
                Ck::Domain => {
                    let mut list = self.desc_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, ce) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::ObjectPropertyDomain(ObjectPropertyDomain {
                                ope: subject_ope.clone(),
                                ce,
                            }),
                            ann: item_ann,
                        });
                    }
                }
                Ck::Range => {
                    let mut list = self.desc_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, ce) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::ObjectPropertyRange(ObjectPropertyRange {
                                ope: subject_ope.clone(),
                                ce,
                            }),
                            ann: item_ann,
                        });
                    }
                }
                Ck::Characteristics => {
                    let Body::Chars(chars) = &clause.body else {
                        unreachable!("characteristics")
                    };
                    let empty = BTreeSet::new();
                    for (i, ch) in chars.iter().enumerate() {
                        let item_ann = if i == 0 { &ann } else { &empty };
                        self.object_characteristic(ch, &subject_ope, item_ann, out)?;
                    }
                }
                _ => unreachable!("object property clause"),
            }
        }
        Ok(())
    }

    fn object_characteristic(
        &self,
        kw: &str,
        ope: &ObjectPropertyExpression<A>,
        ann: &BTreeSet<Annotation<A>>,
        out: &mut Out<A>,
    ) -> CR<'a, ()> {
        let ope = ope.clone();
        let ann = ann.clone();
        let component = match kw.to_ascii_lowercase().as_str() {
            "functional" => Component::FunctionalObjectProperty(FunctionalObjectProperty(ope)),
            "inversefunctional" => {
                Component::InverseFunctionalObjectProperty(InverseFunctionalObjectProperty(ope))
            }
            "reflexive" => Component::ReflexiveObjectProperty(ReflexiveObjectProperty(ope)),
            "irreflexive" => Component::IrreflexiveObjectProperty(IrreflexiveObjectProperty(ope)),
            "symmetric" => Component::SymmetricObjectProperty(SymmetricObjectProperty(ope)),
            "asymmetric" => Component::AsymmetricObjectProperty(AsymmetricObjectProperty(ope)),
            "transitive" => Component::TransitiveObjectProperty(TransitiveObjectProperty(ope)),
            other => {
                return invalid(self.unit, format!("unknown object characteristic: {other}"));
            }
        };
        out.insert(AnnotatedComponent { component, ann });
        Ok(())
    }

    fn data_property_frame(&self, f: &Frame<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        let Subject::Iri(s) = &f.subject else {
            unreachable!("data property subject")
        };
        let subject = self.iri(s)?;
        out.insert(DeclareDataProperty(DataProperty(subject.clone())));
        for clause in &f.clauses {
            let ann = self.clause_ann(clause)?;
            match clause.kw {
                Ck::Annotations => {
                    self.entity_annotations(clause, &AnnotationSubject::IRI(subject.clone()), out)?
                }
                Ck::SubPropertyOf => {
                    let mut list = self.iri_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, iri) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::SubDataPropertyOf(SubDataPropertyOf {
                                sub: DataProperty(subject.clone()),
                                sup: DataProperty(iri),
                            }),
                            ann: item_ann,
                        });
                    }
                }
                Ck::EquivalentTo => {
                    let mut list = self.iri_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, iri) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::EquivalentDataProperties(
                                EquivalentDataProperties(vec![
                                    DataProperty(subject.clone()),
                                    DataProperty(iri),
                                ]),
                            ),
                            ann: item_ann,
                        });
                    }
                }
                Ck::DisjointWith => {
                    let mut list = self.iri_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, iri) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::DisjointDataProperties(DisjointDataProperties(
                                vec![DataProperty(subject.clone()), DataProperty(iri)],
                            )),
                            ann: item_ann,
                        });
                    }
                }
                Ck::Domain => {
                    let mut list = self.desc_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, ce) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::DataPropertyDomain(DataPropertyDomain {
                                dp: DataProperty(subject.clone()),
                                ce,
                            }),
                            ann: item_ann,
                        });
                    }
                }
                Ck::Range => {
                    let Body::DataRanges(drs) = &clause.body else {
                        unreachable!("data ranges")
                    };
                    for d in drs {
                        out.insert(AnnotatedComponent {
                            component: Component::DataPropertyRange(DataPropertyRange {
                                dp: DataProperty(subject.clone()),
                                dr: self.data_range(d)?,
                            }),
                            ann: ann.clone(),
                        });
                    }
                }
                Ck::Characteristics => {
                    let Body::Chars(chars) = &clause.body else {
                        unreachable!("characteristics")
                    };
                    let empty = BTreeSet::new();
                    for (i, ch) in chars.iter().enumerate() {
                        let item_ann = if i == 0 { &ann } else { &empty };
                        if ch.eq_ignore_ascii_case("functional") {
                            out.insert(AnnotatedComponent {
                                component: Component::FunctionalDataProperty(
                                    FunctionalDataProperty(DataProperty(subject.clone())),
                                ),
                                ann: item_ann.clone(),
                            });
                        } else {
                            return invalid(
                                ch,
                                "data properties only support the Functional characteristic",
                            );
                        }
                    }
                }
                _ => unreachable!("data property clause"),
            }
        }
        Ok(())
    }

    fn annotation_property_frame(&self, f: &Frame<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        let Subject::Iri(s) = &f.subject else {
            unreachable!("annotation property subject")
        };
        let subject = self.iri(s)?;
        out.insert(DeclareAnnotationProperty(AnnotationProperty(
            subject.clone(),
        )));
        for clause in &f.clauses {
            let ann = self.clause_ann(clause)?;
            match clause.kw {
                Ck::Annotations => {
                    self.entity_annotations(clause, &AnnotationSubject::IRI(subject.clone()), out)?
                }
                Ck::SubPropertyOf => {
                    let mut list = self.iri_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, iri) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::SubAnnotationPropertyOf(
                                SubAnnotationPropertyOf {
                                    sub: AnnotationProperty(subject.clone()),
                                    sup: AnnotationProperty(iri),
                                },
                            ),
                            ann: item_ann,
                        });
                    }
                }
                Ck::Domain => {
                    let mut list = self.iri_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, iri) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::AnnotationPropertyDomain(
                                AnnotationPropertyDomain {
                                    ap: AnnotationProperty(subject.clone()),
                                    iri,
                                },
                            ),
                            ann: item_ann,
                        });
                    }
                }
                Ck::Range => {
                    let mut list = self.iri_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, iri) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::AnnotationPropertyRange(
                                AnnotationPropertyRange {
                                    ap: AnnotationProperty(subject.clone()),
                                    iri,
                                },
                            ),
                            ann: item_ann,
                        });
                    }
                }
                _ => unreachable!("annotation property clause"),
            }
        }
        Ok(())
    }

    fn individual_frame(&self, f: &Frame<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        let Subject::Ind(s) = &f.subject else {
            unreachable!("individual subject")
        };
        let subject_ind = self.individual(s)?;
        let anno_subject = match &subject_ind {
            Individual::Named(ni) => {
                out.insert(DeclareNamedIndividual(ni.clone()));
                AnnotationSubject::IRI(ni.0.clone())
            }
            Individual::Anonymous(ai) => AnnotationSubject::AnonymousIndividual(ai.clone()),
        };
        for clause in &f.clauses {
            let ann = self.clause_ann(clause)?;
            match clause.kw {
                Ck::Annotations => self.entity_annotations(clause, &anno_subject, out)?,
                Ck::Types => {
                    let mut list = self.desc_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, ce) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::ClassAssertion(ClassAssertion {
                                i: subject_ind.clone(),
                                ce,
                            }),
                            ann: item_ann,
                        });
                    }
                }
                Ck::Facts => {
                    let Body::Facts(facts) = &clause.body else {
                        unreachable!("facts")
                    };
                    let empty = BTreeSet::new();
                    for (i, fact) in facts.iter().enumerate() {
                        let item_ann = if i == 0 { &ann } else { &empty };
                        self.fact(fact, &subject_ind, item_ann, out)?;
                    }
                }
                Ck::SameAs => {
                    let mut list = self.ind_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, ind) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::SameIndividual(SameIndividual(vec![
                                subject_ind.clone(),
                                ind,
                            ])),
                            ann: item_ann,
                        });
                    }
                }
                Ck::DifferentFrom => {
                    let mut list = self.ind_list(&clause.body)?;
                    Self::bind_leading_to_first(ann, &mut list);
                    for (item_ann, ind) in list {
                        out.insert(AnnotatedComponent {
                            component: Component::DifferentIndividuals(DifferentIndividuals(vec![
                                subject_ind.clone(),
                                ind,
                            ])),
                            ann: item_ann,
                        });
                    }
                }
                _ => unreachable!("individual clause"),
            }
        }
        Ok(())
    }

    fn fact(
        &self,
        fact: &Fact<'a>,
        from: &Individual<A>,
        ann: &BTreeSet<Annotation<A>>,
        out: &mut Out<A>,
    ) -> CR<'a, ()> {
        let ope = self.ope(&fact.ope)?;
        let component = match &fact.target {
            FactTarget::Lit(l) => {
                let lit = self.literal(l)?;
                let dp = match &ope {
                    ObjectPropertyExpression::ObjectProperty(p) => DataProperty(p.0.clone()),
                    ObjectPropertyExpression::InverseObjectProperty(_) => {
                        return invalid(fact.ope.iri.text, "inverse property in a data fact");
                    }
                };
                if fact.not {
                    Component::NegativeDataPropertyAssertion(NegativeDataPropertyAssertion {
                        dp,
                        from: from.clone(),
                        to: lit,
                    })
                } else {
                    Component::DataPropertyAssertion(DataPropertyAssertion {
                        dp,
                        from: from.clone(),
                        to: lit,
                    })
                }
            }
            FactTarget::Ind(i) => {
                let to = self.individual(i)?;
                if fact.not {
                    Component::NegativeObjectPropertyAssertion(NegativeObjectPropertyAssertion {
                        ope,
                        from: from.clone(),
                        to,
                    })
                } else {
                    Component::ObjectPropertyAssertion(ObjectPropertyAssertion {
                        ope,
                        from: from.clone(),
                        to,
                    })
                }
            }
        };
        out.insert(AnnotatedComponent {
            component,
            ann: ann.clone(),
        });
        Ok(())
    }

    fn datatype_frame(&self, f: &Frame<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        let Subject::Iri(s) = &f.subject else {
            unreachable!("datatype subject")
        };
        let subject = self.iri(s)?;
        out.insert(DeclareDatatype(Datatype(subject.clone())));
        for clause in &f.clauses {
            let ann = self.clause_ann(clause)?;
            match clause.kw {
                Ck::Annotations => {
                    self.entity_annotations(clause, &AnnotationSubject::IRI(subject.clone()), out)?
                }
                Ck::EquivalentTo => {
                    let Body::DataRange(d) = &clause.body else {
                        unreachable!("data range")
                    };
                    out.insert(AnnotatedComponent {
                        component: Component::DatatypeDefinition(DatatypeDefinition {
                            kind: Datatype(subject.clone()),
                            range: self.data_range(d)?,
                        }),
                        ann,
                    });
                }
                _ => unreachable!("datatype clause"),
            }
        }
        Ok(())
    }

    pub fn misc(&self, m: &Misc<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        let mut ann = self.ann_set(&m.ann)?;
        let component = match m.kind {
            MiscKind::EquivalentClasses => {
                let mut v = Vec::new();
                Self::merge_list_ann(&mut ann, self.desc_list(&m.body)?, &mut v);
                Component::EquivalentClasses(EquivalentClasses(v))
            }
            MiscKind::DisjointClasses => {
                let mut v = Vec::new();
                Self::merge_list_ann(&mut ann, self.desc_list(&m.body)?, &mut v);
                Component::DisjointClasses(DisjointClasses(v))
            }
            MiscKind::EquivalentProperties => {
                let mut opes = Vec::new();
                Self::merge_list_ann(&mut ann, self.ope_list(&m.body)?, &mut opes);
                if let Some(dps) = self.all_as_data_props(&opes) {
                    Component::EquivalentDataProperties(EquivalentDataProperties(dps))
                } else {
                    Component::EquivalentObjectProperties(EquivalentObjectProperties(opes))
                }
            }
            MiscKind::DisjointProperties => {
                let mut opes = Vec::new();
                Self::merge_list_ann(&mut ann, self.ope_list(&m.body)?, &mut opes);
                if let Some(dps) = self.all_as_data_props(&opes) {
                    Component::DisjointDataProperties(DisjointDataProperties(dps))
                } else {
                    Component::DisjointObjectProperties(DisjointObjectProperties(opes))
                }
            }
            MiscKind::SameIndividual => {
                let mut v = Vec::new();
                Self::merge_list_ann(&mut ann, self.ind_list(&m.body)?, &mut v);
                Component::SameIndividual(SameIndividual(v))
            }
            MiscKind::DifferentIndividuals => {
                let mut v = Vec::new();
                Self::merge_list_ann(&mut ann, self.ind_list(&m.body)?, &mut v);
                Component::DifferentIndividuals(DifferentIndividuals(v))
            }
        };
        out.insert(AnnotatedComponent { component, ann });
        Ok(())
    }

    fn all_as_data_props(
        &self,
        opes: &[ObjectPropertyExpression<A>],
    ) -> Option<Vec<DataProperty<A>>> {
        if opes.is_empty() {
            return None;
        }
        opes.iter()
            .map(|ope| match ope {
                ObjectPropertyExpression::ObjectProperty(ObjectProperty(iri))
                    if self.is_data_prop(iri) =>
                {
                    Some(DataProperty(iri.clone()))
                }
                _ => None,
            })
            .collect()
    }

    // -- SWRL --------------------------------------------------------------

    fn swrl_arg_kind(&self, a: &Arg<'a>) -> CR<'a, SwrlArgKind<A>> {
        Ok(match a {
            Arg::Var(v) => SwrlArgKind::Var(Variable(self.iri(v)?)),
            Arg::Lit(l) => SwrlArgKind::Lit(self.literal(l)?),
            Arg::Ind(i) => SwrlArgKind::Ind(self.individual(i)?),
        })
    }

    fn swrl_iarg(&self, k: &SwrlArgKind<A>) -> CR<'a, IArgument<A>> {
        match k {
            SwrlArgKind::Var(v) => Ok(IArgument::Variable(v.clone())),
            SwrlArgKind::Ind(i) => Ok(IArgument::Individual(i.clone())),
            SwrlArgKind::Lit(_) => invalid(
                self.unit,
                "SWRL: expected an individual or variable argument, found a literal",
            ),
        }
    }

    fn swrl_darg(&self, k: &SwrlArgKind<A>) -> CR<'a, DArgument<A>> {
        match k {
            SwrlArgKind::Var(v) => Ok(DArgument::Variable(v.clone())),
            SwrlArgKind::Lit(l) => Ok(DArgument::Literal(l.clone())),
            SwrlArgKind::Ind(_) => invalid(
                self.unit,
                "SWRL: expected a data value or variable argument, found an individual",
            ),
        }
    }

    fn atom(&self, atom: &SynAtom<'a>) -> CR<'a, Atom<A>> {
        match atom {
            SynAtom::SameAs(a, b) | SynAtom::DifferentFrom(a, b) => {
                let i1 = self.swrl_iarg(&self.swrl_arg_kind(a)?)?;
                let i2 = self.swrl_iarg(&self.swrl_arg_kind(b)?)?;
                Ok(if matches!(atom, SynAtom::SameAs(..)) {
                    Atom::SameIndividualAtom(i1, i2)
                } else {
                    Atom::DifferentIndividualsAtom(i1, i2)
                })
            }
            SynAtom::Pred(pred, args) => {
                let pred_ce = self.description(pred)?;
                let pred_iri: Option<IRI<A>> = match &pred_ce {
                    ClassExpression::Class(Class(iri)) => Some(iri.clone()),
                    _ => None,
                };
                let kinds: Vec<SwrlArgKind<A>> = args
                    .iter()
                    .map(|a| self.swrl_arg_kind(a))
                    .collect::<CR<'a, _>>()?;
                let bare = || -> CR<'a, IRI<A>> {
                    match &pred_iri {
                        Some(i) => Ok(i.clone()),
                        None => {
                            invalid(self.unit, "SWRL: this atom requires a named-IRI predicate")
                        }
                    }
                };
                let is_builtin_ns = pred_iri.as_ref().is_some_and(|i| {
                    BUILTIN_NAMESPACES
                        .iter()
                        .any(|ns| i.as_ref().starts_with(ns))
                });
                if is_builtin_ns {
                    Ok(Atom::BuiltInAtom {
                        pred: bare()?,
                        args: kinds
                            .iter()
                            .map(|k| self.swrl_darg(k))
                            .collect::<CR<'a, _>>()?,
                    })
                } else if kinds.len() == 1 {
                    match &kinds[0] {
                        SwrlArgKind::Lit(l) => Ok(Atom::DataRangeAtom {
                            pred: DataRange::Datatype(Datatype(bare()?)),
                            arg: DArgument::Literal(l.clone()),
                        }),
                        SwrlArgKind::Var(v)
                            if pred_iri.as_ref().is_some_and(|i| self.is_datatype(i)) =>
                        {
                            Ok(Atom::DataRangeAtom {
                                pred: DataRange::Datatype(Datatype(bare()?)),
                                arg: DArgument::Variable(v.clone()),
                            })
                        }
                        other => Ok(Atom::ClassAtom {
                            pred: pred_ce,
                            arg: self.swrl_iarg(other)?,
                        }),
                    }
                } else {
                    let first_is_lit = matches!(kinds[0], SwrlArgKind::Lit(_));
                    let second_is_lit = matches!(kinds[1], SwrlArgKind::Lit(_));
                    if first_is_lit || kinds.len() > 2 {
                        Ok(Atom::BuiltInAtom {
                            pred: bare()?,
                            args: kinds
                                .iter()
                                .map(|k| self.swrl_darg(k))
                                .collect::<CR<'a, _>>()?,
                        })
                    } else if second_is_lit
                        || pred_iri.as_ref().is_some_and(|i| self.is_data_prop(i))
                    {
                        Ok(Atom::DataPropertyAtom {
                            pred: DataProperty(bare()?),
                            args: (self.swrl_darg(&kinds[0])?, self.swrl_darg(&kinds[1])?),
                        })
                    } else {
                        Ok(Atom::ObjectPropertyAtom {
                            pred: ObjectPropertyExpression::ObjectProperty(ObjectProperty(bare()?)),
                            args: (self.swrl_iarg(&kinds[0])?, self.swrl_iarg(&kinds[1])?),
                        })
                    }
                }
            }
        }
    }

    pub fn rule(&self, r: &RuleFrame<'a>, out: &mut Out<A>) -> CR<'a, ()> {
        let ann = self.ann_set(&r.ann)?;
        let body = r
            .body
            .iter()
            .map(|a| self.atom(a))
            .collect::<CR<'a, Vec<_>>>()?;
        let head = r
            .head
            .iter()
            .map(|a| self.atom(a))
            .collect::<CR<'a, Vec<_>>>()?;
        out.insert(AnnotatedComponent {
            component: Component::Rule(crate::model::Rule { head, body }),
            ann,
        });
        Ok(())
    }
}
