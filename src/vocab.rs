//! Core RDF vocabularies used in the OWL2 RDF format.
use self::Namespace::*;
use enum_meta::*;

use crate::error::invalid;
use crate::error::HornedError;
use crate::model::Build;
use crate::model::Facet;
use crate::model::ForIRI;
use crate::model::NamedEntity;
use crate::model::NamedEntityKind;
use crate::model::IRI;

use std::borrow::Borrow;

// TODO: refactor to be included in model, or refactor into new `iri` module 
// together with IRI and ForIRI.
/// Provides methods to access the [IRI]s associated to a meta-enum.
pub trait WithIRI<'a>: Meta<&'a IRI<String>> {

    /// Returns a byte slice containing the IRI associated with this entity.
    fn as_iri_bytes(&self) -> &'a [u8] {
        self.meta().as_bytes()
    }

    /// Returns a string representation of the IRI associated with this
    /// entity.
    fn as_iri_str(&self) -> &'a str {
        self.meta().as_ref()
    }

    /// The string is checked against all the variants of the meta-enum.
    /// Returns [Some(variant)] if a match is found with the metadata of a
    /// variant, otherwise [None] is returned.
    fn variant_from_str(tag: &'a str) -> Option<Self> {
        Self::variant_from_bytes(tag.as_bytes())
    }

    /// The byte slice is checked against all the variants of the meta-enum.
    /// Returns [Some(variant)] if a match is found with the metadata of a
    /// variant, otherwise [None] is returned.
    fn variant_from_bytes(tag: &'a [u8]) -> Option<Self> {
        Self::all()
            .into_iter()
            .find(|item| item.as_iri_bytes() == tag)
    }
}

#[deprecated(since = "0.15.0", note = "Use `IRI<String>` instead.")]
pub type IRIString = IRI<String>;

impl<'a, T> WithIRI<'a> for T where T: Meta<&'a IRI<String>> {}

fn to_meta(s: &str) -> IRI<String> {
    let builder = Build::new_string();
    builder.iri(s)
}

fn extend<'a, I>(i: I, suffix: &'a str) -> IRI<String>
where
    I: WithIRI<'a>,
{
    to_meta(&format!("{}{}", i.as_iri_str(), suffix))
}

/// [Namespaces](https://www.w3.org/TR/2004/REC-owl-guide-20040210/#Namespaces)
/// that are typically used within an OWL document.
#[derive(Debug, Eq, PartialEq)]
pub enum Namespace {
    /// Ontology Web Language
    OWL,
    /// Resource Description Framework
    RDF,
    /// RDF Schema
    RDFS,
    /// XML Schema datatype
    XSD,
}

lazy_meta! {
    Namespace, IRI<String>, METANS;
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
    RDF, IRI<String>, METARDF;
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
        matches!{
            self,
            RDFS::Label | RDFS::Comment | RDFS::SeeAlso | RDFS::IsDefinedBy
        }
    }
}

lazy_meta! {
    RDFS, IRI<String>, METARDFS;
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
    TopObjectProperty,
    Thing,
    TransitiveProperty,
    UnionOf,
    VersionIRI,
    WithRestrictions,
}

lazy_meta! {
    OWL, IRI<String>, METAOWL;

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
    TopObjectProperty, extend(OWL, "topObjectProperty");
    TransitiveProperty, extend(OWL, "TransitiveProperty");
    UnionOf, extend(OWL, "unionOf");
    VersionIRI, extend(OWL, "versionIRI");
    WithRestrictions, extend(OWL, "withRestrictions");
}

// TODO: change parameter from `IRI<A>` to `Class<A>`.
//       Only Classes can be owl:Thing?
pub fn is_thing<A: ForIRI>(iri: &IRI<A>) -> bool {
    iri.as_ref() == OWL::Thing.as_iri_str()
}

// TODO: change parameter from `IRI<A>` to `Class<A>`.
//       Only Classes can be owl:Nothing?
pub fn is_nothing<A: ForIRI>(iri: &IRI<A>) -> bool {
    iri.as_ref() == OWL::Nothing.as_iri_str()
}

/// Returns a [NamedEntityKind] if the IRI points to a built-in entity, otherwise [None].
pub fn to_built_in_entity<A: ForIRI>(iri: &IRI<A>) -> Option<NamedEntityKind> {
    let ir = iri.as_ref();
    match ir {
        _ if ir == OWL::TopDataProperty.as_iri_str() => Some(NamedEntityKind::DataProperty),
        _ if ir == OWL::TopObjectProperty.as_iri_str() => Some(NamedEntityKind::ObjectProperty),
        _ if ir == OWL::Thing.as_iri_str() => Some(NamedEntityKind::Class),
        _ if ir == OWL::Nothing.as_iri_str() => Some(NamedEntityKind::Class),
        _ => None,
    }
}

#[test]
fn meta_testing() {
    assert_eq!("http://www.w3.org/2002/07/owl#", OWL.as_iri_str());
    assert_eq!(b"http://www.w3.org/2002/07/owl#", OWL.as_iri_bytes());

    assert_eq!(
        Namespace::variant_from_str("http://www.w3.org/2002/07/owl#").unwrap(),
        OWL
    );

    assert_eq!(
        Namespace::variant_from_bytes(b"http://www.w3.org/2002/07/owl#").unwrap(),
        OWL
    );
}

#[test]
fn test_to_built_in_entity() {
    let builder = Build::new_rc();
    let iri_top_dp = builder.iri(OWL::TopDataProperty.as_iri_str());
    let iri_top_op = builder.iri(OWL::TopObjectProperty.as_iri_str());
    let iri_thing = builder.iri(OWL::Thing.as_iri_str());
    let iri_nothing = builder.iri(OWL::Nothing.as_iri_str());
    assert_eq!(to_built_in_entity(&iri_top_dp), Some(NamedEntityKind::DataProperty));
    assert_eq!(to_built_in_entity(&iri_top_op), Some(NamedEntityKind::ObjectProperty));
    assert_eq!(to_built_in_entity(&iri_thing), Some(NamedEntityKind::Class));
    assert_eq!(to_built_in_entity(&iri_nothing), Some(NamedEntityKind::Class));
    // todo!("Correct to_built_in_entity method to correctly identify owl:Thing and owl:Nothing as Class entities.");
}


pub fn entity_for_iri<A: ForIRI, S: Borrow<str>>(
    type_iri: S,
    entity_iri: S,
    b: &Build<A>,
) -> Result<NamedEntity<A>, HornedError> {
    // Datatypes are handled here because they are not a
    // "type" but an "RDF schema" element.
    if type_iri.borrow() == "http://www.w3.org/2000/01/rdf-schema#Datatype" {
        return Ok(b.datatype(entity_iri).into());
    }

    if type_iri.borrow().len() < 30 {
        return Err(invalid!(
            "IRI is not for a type of entity:{:?}",
            type_iri.borrow()
        ));
    }

    Ok(match &type_iri.borrow()[30..] {
        "Class" => b.class(entity_iri).into(),
        "ObjectProperty" => b.object_property(entity_iri).into(),
        "DatatypeProperty" => b.data_property(entity_iri).into(),
        "AnnotationProperty" => b.annotation_property(entity_iri).into(),
        "NamedIndividual" => b.named_individual(entity_iri).into(),
        _ => {
            return Err(invalid!(
                "IRI is not a type of entity:{:?}",
                type_iri.borrow()
            ));
        }
    })
}

#[test]
pub fn test_entity_for_iri() {
    let b = Build::new_rc();

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

#[test]
pub fn test_namespace_in_entity_for_iri() {
    let b = Build::new_rc();

    assert!(entity_for_iri(
        "http://www.w3.org/2002/07/low#Class",
        "http://www.example.com",
        &b
    )
    .is_err());
    assert!(entity_for_iri(
        "abcdefghijklmnopqrstuvwxyz123#Class",
        "http://www.example.com",
        &b
    )
    .is_err());
    todo!("Correct entity_for_iri method to incorporate proper namespace checking and make this test pass.");
}

pub enum OWL2Datatype {
    RDFSLiteral,
}

lazy_meta! {
    OWL2Datatype, IRI<String>, METAOWL2DATATYPE;
    RDFSLiteral, extend(RDFS, "Literal")
}

pub enum AnnotationBuiltIn {
    LABEL,
    COMMENT,
    SEEALSO,
    ISDEFINEDBY,
    DEPRECATED,
    VERSIONINFO,
    PRIORVERSION,
    BACKWARDCOMPATIBLEWITH,
    INCOMPATIBLEWITH,
}

lazy_meta! {
    AnnotationBuiltIn, IRI<String>, METAANNOTATIONBUILTIN;
    LABEL, extend(RDFS, "label");
    COMMENT, extend(RDFS, "comment");
    SEEALSO, extend(RDFS, "seeAlso");
    ISDEFINEDBY, extend(RDFS, "isDefinedBy");
    DEPRECATED, extend(OWL, "deprecated");
    VERSIONINFO, extend(OWL, "versionInfo");
    PRIORVERSION, extend(OWL, "priorVersion");
    BACKWARDCOMPATIBLEWITH, extend(OWL, "backwardCompatibleWith");
    INCOMPATIBLEWITH, extend(OWL, "incompatibleWith");
}

#[inline]
pub fn is_annotation_builtin<A:AsRef<str>>(iri: A) -> bool {
    AnnotationBuiltIn::all()
        .iter()
        .any(|meta| meta.as_iri_str() == iri.as_ref())
}

#[test]
fn annotation_builtin() {
    // Method can accept `str`ings...
    assert!(is_annotation_builtin("http://www.w3.org/2002/07/owl#deprecated"));
    // ...and `String`s.
    assert!(is_annotation_builtin(
        &"http://www.w3.org/2000/01/rdf-schema#comment".to_string()
    ));
    assert!(!is_annotation_builtin(
        &"http://www.w3.org/2002/07/owl#fred".to_string()
    ));
}

lazy_meta! {
    Facet, IRI<String>, METAFACET;
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
        Facet::MinLength.as_iri_str(),
        "http://www.w3.org/2001/XMLSchema#minLength"
    );

    assert_eq!(
        Facet::Pattern.as_iri_str(),
        "http://www.w3.org/2001/XMLSchema#pattern"
    );

    assert_eq!(
        Facet::variant_from_str("http://www.w3.org/2001/XMLSchema#pattern").unwrap(),
        Facet::Pattern
    );

    assert_eq!(
        Facet::variant_from_bytes(b"http://www.w3.org/2001/XMLSchema#minExclusive").unwrap(),
        Facet::MinExclusive
    );
}

pub enum XSD {
    NonNegativeInteger,
}

pub fn is_xsd_datatype<A:AsRef<str>>(iri:A) -> bool {
    //TODO. This is over-simplistic
    iri.as_ref().starts_with("http://www.w3.org/2001/XMLSchema")
}


lazy_meta! {
    XSD, IRI<String>, METAXSD;
    NonNegativeInteger, extend(XSD, "nonNegativeInteger")
}

#[test]
fn test_is_xsd_datatype() {
    assert!(is_xsd_datatype("http://www.w3.org/2001/XMLSchema#nonNegativeInteger"));
    assert!(!is_xsd_datatype("http://www.w3.org/2001/XMLSchemaaa#nonNegativeInteger"));
    assert!(!is_xsd_datatype("http://www.w3.org/2001/XMLSchema.pdf"));
    todo!("Correct is_xsd_datatype method to incorporate IRI validation and make this test pass.");
}

pub enum Vocab {
    Facet(Facet),
    RDF(RDF),
    RDFS(RDFS),
    OWL(OWL),
    XSD(XSD),
    Namespace(Namespace),
}

impl<'a> Meta<&'a IRI<String>> for Vocab {
    fn meta(&self) -> &'a IRI<String> {
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
        let facet_all = Facet::all().into_iter().map(|variant| Self::Facet(variant));
        let rdf_all = RDF::all().into_iter().map(|variant| Self::RDF(variant));
        let rdfs_all = RDFS::all().into_iter().map(|variant| Self::RDFS(variant));
        let owl_all = OWL::all().into_iter().map(|variant| Self::OWL(variant));
        let xsd_all = XSD::all().into_iter().map(|variant| Self::XSD(variant));
        let ns_all = Namespace::all().into_iter().map(|variant| Self::Namespace(variant));

        facet_all
            .chain(rdf_all)
            .chain(rdfs_all)
            .chain(owl_all)
            .chain(xsd_all)
            .chain(ns_all)
            .collect()
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
