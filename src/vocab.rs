//! Standard Vocabularies for OWL
use self::Namespace::*;
use enum_meta::*;

use crate::model::Build;
use crate::model::Facet;
use crate::model::NamedEntity;
use crate::model::NamedEntityKind;
use crate::model::IRI;

use failure::Error;

pub trait WithIRI<'a>: Meta<&'a IRIString> {
    /// Return a string representation of the IRI associated with this
    /// entity.
    fn iri_s(&self) -> &'a String {
        &self.meta().0
    }

    fn iri_b(&self) -> &'a [u8] {
        &self.meta().0.as_bytes()
    }

    fn iri_str(&self) -> &'a str {
        &self.meta().0[..]
    }

    fn var_s(tag: &'a str) -> Option<Self> {
        Self::var_b(tag.as_bytes())
    }

    fn var_b(tag: &'a [u8]) -> Option<Self> {
        for v in Self::all() {
            if tag == v.iri_b() {
                return Some(v);
            }
        }
        None
    }
}

pub struct IRIString(String);

impl<'a, T> WithIRI<'a> for T where T: Meta<&'a IRIString> {}

fn to_meta(s: &str) -> IRIString {
    IRIString(s.to_string())
}

fn extend<'a, I>(i: I, s: &'a str) -> IRIString
where
    I: WithIRI<'a>,
{
    to_meta(&format!("{}{}", i.iri_s(), s))
}

#[derive(Debug, Eq, PartialEq)]
pub enum Namespace {
    OWL,
    RDF,
    RDFS,
    XSD,
}

lazy_meta! {
    Namespace, IRIString, METANS;
    OWL, to_meta("http://www.w3.org/2002/07/owl#");
    RDF, to_meta("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
    RDFS, to_meta("http://www.w3.org/2000/01/rdf-schema#");
    XSD, to_meta("http://www.w3.org/2001/XMLSchema#");
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RDF {
    First,
    List,
    Nil,
    Rest,
    Type,
}

lazy_meta! {
    RDF, IRIString, METARDF;
    First, extend(RDF, "first");
    List, extend(RDF, "List");
    Nil, extend(RDF, "nil");
    Rest, extend(RDF, "rest");
    Type, extend(RDF, "type");
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RDFS {
    Comment,
    Datatype,
    Domain,
    IsDefinedBy,
    Label,
    Range,
    SeeAlso,
    SubClassOf,
    SubPropertyOf,
}

impl RDFS {
    pub fn is_builtin(&self) -> bool {
        match self {
            RDFS::Label | RDFS::Comment | RDFS::SeeAlso | RDFS::IsDefinedBy => true,
            _ => false,
        }
    }
}

lazy_meta! {
    RDFS, IRIString, METARDFS;
    Comment, extend(RDFS, "comment");
    Datatype, extend(RDFS, "Datatype");
    Domain, extend(RDFS, "domain");
    IsDefinedBy, extend(RDFS, "isDefinedBy");
    Label, extend(RDFS, "label");
    Range, extend(RDFS, "range");
    SeeAlso, extend(RDFS, "seeAlso");
    SubClassOf, extend(RDFS, "subClassOf");
    SubPropertyOf, extend(RDFS, "subPropertyOf");
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum OWL {
    AllDifferent,
    AllDisjointProperties,
    AllValuesFrom,
    AnnotatedProperty,
    AnnotatedSource,
    AnnotatedTarget,
    AnnotationProperty,
    AssertionProperty,
    AsymmetricProperty,
    Axiom,
    Cardinality,
    Class,
    ComplementOf,
    DatatypeComplementOf,
    DatatypeProperty,
    DifferentFrom,
    DisjointUnionOf,
    DisjointWith,
    DistinctMembers,
    EquivalentClass,
    EquivalentProperty,
    FunctionalProperty,
    HasKey,
    HasValue,
    Imports,
    IntersectionOf,
    InverseFunctionalProperty,
    InverseOf,
    IrreflexiveProperty,
    MaxCardinality,
    MaxQualifiedCardinality,
    Members,
    MinCardinality,
    MinQualifiedCardinality,
    NamedIndividual,
    NegativePropertyAssertion,
    Nothing,
    ObjectProperty,
    OnClass,
    OnDataRange,
    OnDatatype,
    OneOf,
    OnProperty,
    Ontology,
    QualifiedCardinality,
    PropertyChainAxiom,
    PropertyDisjointWith,
    ReflexiveProperty,
    Restriction,
    SameAs,
    SourceIndividual,
    SomeValuesFrom,
    SymmetricProperty,
    TargetIndividual,
    TargetValue,
    TopDataProperty,
    Thing,
    TransitiveProperty,
    UnionOf,
    VersionIRI,
    VersionInfo,
    WithRestrictions,
}

lazy_meta! {
    OWL, IRIString, METAOWL;

    AllDifferent, extend(OWL, "AllDifferent");
    AllDisjointProperties, extend(OWL, "AllDisjointProperties");
    AllValuesFrom, extend(OWL, "allValuesFrom");
    AnnotatedProperty, extend(OWL, "annotatedProperty");
    AnnotatedSource, extend(OWL, "annotatedSource");
    AnnotatedTarget, extend(OWL, "annotatedTarget");
    AnnotationProperty, extend(OWL, "AnnotationProperty");
    AssertionProperty, extend(OWL, "assertionProperty");
    AsymmetricProperty, extend(OWL, "AsymmetricProperty");
    Axiom, extend(OWL, "Axiom");
    Class, extend(OWL, "Class");
    ComplementOf, extend(OWL, "complementOf");
    DatatypeComplementOf, extend(OWL, "datatypeComplementOf");
    DatatypeProperty, extend(OWL, "DatatypeProperty");
    DifferentFrom, extend(OWL, "differentFrom");
    DisjointUnionOf, extend(OWL, "disjointUnionOf");
    DisjointWith, extend(OWL, "disjointWith");
    DistinctMembers, extend(OWL, "distinctMembers");
    Cardinality, extend(OWL, "cardinality");
    EquivalentClass, extend(OWL, "equivalentClass");
    EquivalentProperty, extend(OWL, "equivalentProperty");
    FunctionalProperty, extend(OWL, "FunctionalProperty");
    Imports, extend(OWL, "imports");
    IntersectionOf, extend(OWL, "intersectionOf");
    InverseFunctionalProperty, extend(OWL, "InverseFunctionalProperty");
    InverseOf, extend(OWL, "inverseOf");
    IrreflexiveProperty, extend(OWL, "IrreflexiveProperty");
    HasKey, extend(OWL, "hasKey");
    HasValue, extend(OWL, "hasValue");
    MaxCardinality, extend(OWL, "maxCardinality");
    MaxQualifiedCardinality, extend(OWL, "maxQualifiedCardinality");
    Members, extend(OWL, "members");
    MinCardinality, extend(OWL, "minCardinality");
    MinQualifiedCardinality, extend(OWL, "minQualifiedCardinality");
    NamedIndividual, extend(OWL, "NamedIndividual");
    NegativePropertyAssertion, extend(OWL, "NegativePropertyAssertion");
    Nothing, extend(OWL, "Nothing");
    ObjectProperty, extend(OWL, "ObjectProperty");
    OnClass, extend(OWL, "onClass");
    OnDataRange, extend(OWL, "onDataRange");
    OnDatatype, extend(OWL, "onDatatype");
    OneOf, extend(OWL, "oneOf");
    OnProperty, extend(OWL, "onProperty");
    Ontology, extend(OWL, "Ontology");
    PropertyChainAxiom, extend(OWL, "propertyChainAxiom");
    PropertyDisjointWith, extend(OWL, "propertyDisjointWith");
    QualifiedCardinality, extend(OWL, "qualifiedCardinality");
    ReflexiveProperty, extend(OWL, "ReflexiveProperty");
    Restriction, extend(OWL, "Restriction");
    SameAs, extend(OWL, "sameAs");
    SourceIndividual, extend(OWL, "sourceIndividual");
    SomeValuesFrom, extend(OWL, "someValuesFrom");
    SymmetricProperty, extend(OWL, "SymmetricProperty");
    TargetIndividual, extend(OWL, "targetIndividual");
    TargetValue, extend(OWL, "targetValue");
    Thing, extend(OWL, "Thing");
    TopDataProperty, extend(OWL, "topDataProperty");
    TransitiveProperty, extend(OWL, "TransitiveProperty");
    UnionOf, extend(OWL, "unionOf");
    VersionIRI, extend(OWL, "versionIRI");
    VersionInfo, extend(OWL, "versionInfo");
    WithRestrictions, extend(OWL, "withRestrictions");
}

pub fn is_thing(iri: &IRI) -> bool {
    iri.as_ref() == OWL::Thing.iri_s()
}

pub fn is_nothing(iri: &IRI) -> bool {
    iri.as_ref() == OWL::Nothing.iri_s()
}

pub fn to_built_in_entity(iri: &IRI) -> Option<NamedEntityKind> {
    let ir = iri.as_ref();
    match ir {
        _ if ir == OWL::TopDataProperty.iri_s() => Some(NamedEntityKind::DataProperty),
        _ => None,
    }
}

#[test]
fn meta_testing() {
    assert_eq!("http://www.w3.org/2002/07/owl#", OWL.iri_s());
    assert_eq!(b"http://www.w3.org/2002/07/owl#", OWL.iri_b());

    assert_eq!(
        Namespace::var_s("http://www.w3.org/2002/07/owl#").unwrap(),
        OWL
    );

    assert_eq!(
        Namespace::var_b(b"http://www.w3.org/2002/07/owl#").unwrap(),
        OWL
    );
}

pub fn entity_for_iri(type_iri: &str, entity_iri: &str, b: &Build) -> Result<NamedEntity, Error> {
    // Datatypes are handled here because they are not a
    // "type" but an "RDF schema" element.
    if type_iri == "http://www.w3.org/2000/01/rdf-schema#Datatype" {
        return Ok(b.datatype(entity_iri).into());
    }

    if type_iri.len() < 30 {
        bail!("IRI is not for a type of entity:{}", type_iri);
    }

    Ok(match &type_iri[30..] {
        "Class" => b.class(entity_iri).into(),
        "ObjectProperty" => b.object_property(entity_iri).into(),
        "DatatypeProperty" => b.data_property(entity_iri).into(),
        "AnnotationProperty" => b.annotation_property(entity_iri).into(),
        "NamedIndividual" => b.named_individual(entity_iri).into(),
        _ => bail!("IRI is not a type of entity:{}", type_iri),
    })
}

#[test]
pub fn test_entity_for_iri() {
    let b = Build::new();

    assert!(entity_for_iri(
        "http://www.w3.org/2002/07/owl#Class",
        "http://www.example.com",
        &b
    )
    .is_ok());
    assert!(entity_for_iri(
        "http://www.w3.org/2002/07/owl#Fred",
        "http://www.example.com",
        &b
    )
    .is_err());
}

pub enum OWL2Datatype {
    RDFSLiteral,
}

lazy_meta! {
    OWL2Datatype, IRIString, METAOWL2DATATYPE;
    RDFSLiteral, extend(RDFS, "Literal")
}

pub enum AnnotationBuiltIn {
    LABEL,
    COMMENT,
    SEEALSO,
    ISDEFINEDBY,
    DEPRECATED,
    VERSIOINFO,
    PRIORVERSION,
    BACKWARDCOMPATIBLEWITH,
    INCOMPATIBLEWITH,
}

lazy_meta! {
    AnnotationBuiltIn, IRIString, METAANNOTATIONBUILTIN;
    LABEL, extend(RDFS, "label");
    COMMENT, extend(RDFS, "comment");
    SEEALSO, extend(RDFS, "seeAlso");
    ISDEFINEDBY, extend(RDFS, "isDefinedBy");
    DEPRECATED, extend(OWL, "deprecated");
    VERSIOINFO, extend(OWL, "versionInfo");
    PRIORVERSION, extend(OWL, "priorVersion");
    BACKWARDCOMPATIBLEWITH, extend(OWL, "backwardCompatibleWith");
    INCOMPATIBLEWITH, extend(OWL, "incompatibleWith");
}

pub fn is_annotation_builtin(iri: &String) -> bool {
    for meta in AnnotationBuiltIn::all() {
        if meta.iri_s() == iri {
            return true;
        }
    }
    return false;
}

#[test]
fn annotation_builtin() {
    assert!(is_annotation_builtin(
        &"http://www.w3.org/2002/07/owl#deprecated".to_string()
    ));
    assert!(is_annotation_builtin(
        &"http://www.w3.org/2000/01/rdf-schema#comment".to_string()
    ));
    assert!(!is_annotation_builtin(
        &"http://www.w3.org/2002/07/owl#fred".to_string()
    ));
}

lazy_meta! {
    Facet, IRIString, METAFACET;
    Length, extend(XSD, "length");
    MinLength, extend(XSD, "minLength");
    MaxLength, extend(XSD, "maxLength");
    Pattern, extend(XSD, "pattern");
    MinInclusive, extend(XSD, "minInclusive");
    MinExclusive, extend(XSD, "minExclusive");
    MaxInclusive, extend(XSD, "maxInclusive");
    MaxExclusive, extend(XSD, "maxExclusive");
    TotalDigits, extend(XSD, "totalDigits");
    FractionDigits, extend(XSD, "fractionDigits");
    LangRange, extend(RDF, "langRange");
}


#[test]
fn facet_meta() {
    assert_eq!(
        Facet::MinLength.iri_s(),
        "http://www.w3.org/2001/XMLSchema#minLength"
    );

    assert_eq!(
        Facet::Pattern.iri_s(),
        "http://www.w3.org/2001/XMLSchema#pattern"
    );

    assert_eq!(
        Facet::var_s(&"http://www.w3.org/2001/XMLSchema#pattern".to_string()).unwrap(),
        Facet::Pattern
    );

    assert_eq!(
        Facet::var_b(b"http://www.w3.org/2001/XMLSchema#minExclusive").unwrap(),
        Facet::MinExclusive
    );
}

pub enum XSD {
    NonNegativeInteger
}

lazy_meta! {
    XSD, IRIString, METAXSD;
    NonNegativeInteger, extend(XSD, "nonNegativeInteger")
}


pub enum Vocab {
    Facet(Facet),
    RDF(RDF),
    RDFS(RDFS),
    OWL(OWL),
    XSD(XSD),
    Namespace(Namespace)
}

impl<'a> Meta<&'a IRIString> for Vocab {
    fn meta(&self) -> &'a IRIString {
        match self {
            Self::Facet(facet) => facet.meta(),
            Self::RDF(rdf) => rdf.meta(),
            Self::RDFS(rdfs) => rdfs.meta(),
            Self::OWL(owl) => owl.meta(),
            Self::XSD(xsd) => xsd.meta(),
            Self::Namespace(ns) => ns.meta(),
        }
    }

    fn all() -> Vec<Self> {
        todo!()
    }
}

impl From<Facet> for Vocab {
    fn from(facet: Facet) -> Self {
        Self::Facet(facet)
    }
}

impl From<&Facet> for Vocab {
    fn from(facet: &Facet) -> Self {
        Self::Facet(facet.clone())
    }
}

impl From<RDF> for Vocab {
    fn from(rdf: RDF) -> Self {
        Self::RDF(rdf)
    }
}

impl From<RDFS> for Vocab {
    fn from(rdfs: RDFS) -> Self {
        Self::RDFS(rdfs)
    }
}

impl From<OWL> for Vocab {
    fn from(owl: OWL) -> Self {
        Self::OWL(owl)
    }
}

impl From<XSD> for Vocab {
    fn from(xsd: XSD) -> Self {
        Self::XSD(xsd)
    }
}
