//! Core RDF vocabularies used in the OWL2 RDF format.
use self::Namespace::*;
use enum_meta::*;

use crate::error::invalid;
use crate::error::HornedError;
use crate::model::Build;
use crate::model::ForIRI;
use crate::model::NamedEntity;
use crate::model::NamedEntityKind;
use crate::model::IRI;

use std::borrow::Borrow;
use std::convert::TryFrom;

// TODO: refactor to be included in model, or refactor into new `iri` module
// together with IRI and ForIRI.
/// Provides methods to access the [IRI]s associated to a meta-enum.
#[deprecated(since = "0.15.0", note = "Deprecated trait, consider using [std::ops::Deref] and [std::convert::TryFrom] instead.")]
pub trait WithIRI<'a>: Meta<&'a IRI<String>> {

    /// Returns a byte slice containing the IRI associated with this entity.
    #[deprecated(since = "0.15.0", note = "Enumerations associated to vocabulary terms now implement [std::ops::Deref<Target = IRI<String>>].")]
    fn as_iri_bytes(&self) -> &'a [u8] {
        self.meta().as_bytes()
    }

    /// Returns a string representation of the IRI associated with this
    /// entity.
    #[deprecated(since = "0.15.0", note = "Enumerations associated to vocabulary terms now implement [std::ops::Deref<Target = IRI<String>>].")]
    fn as_iri_str(&self) -> &'a str {
        self.meta().as_ref()
    }

    /// The string is checked against all the variants of the meta-enum.
    /// Returns [Some(variant)] if a match is found with the metadata of a
    /// variant, otherwise [None] is returned.
    #[deprecated(since = "0.15.0", note = "Enumerations associated to vocabulary terms now implement [std::convert::TryFrom<str>].")]
    fn variant_from_str(tag: &'a str) -> Option<Self> {
        Self::all()
            .into_iter()
            .find(|item| item.meta().as_ref() == tag)
    }

    /// The byte slice is checked against all the variants of the meta-enum.
    /// Returns [Some(variant)] if a match is found with the metadata of a
    /// variant, otherwise [None] is returned.
    #[deprecated(since = "0.15.0", note = "Enumerations associated to vocabulary terms now implement [std::convert::TryFrom<&[u8]>].")]
    fn variant_from_bytes(tag: &'a [u8]) -> Option<Self> {
        Self::all()
            .into_iter()
            .find(|item| item.meta().as_bytes() == tag)
    }
}

#[deprecated(since = "0.15.0", note = "Use `IRI<String>` instead.")]
pub type IRIString = IRI<String>;

fn set_iri(s: &str) -> IRI<String> {
    let builder = Build::new_string();
    builder.iri(s)
}

fn append_to_ns<'a>(ns: Namespace, suffix: &'a str) -> IRI<String>
{
    set_iri(&format!("{}{}", ns.as_ref(), suffix))
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

impl TryFrom<&str> for Namespace {
    type Error = HornedError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Namespace::all()
        .into_iter()
        .find_map(|ns| {
            if ns.as_ref() == value {
                Some(ns)
            } else {
                None
            }
        }).ok_or_else(|| invalid!("Unknown namespace: {}", value))
    }
}

impl TryFrom<&[u8]> for Namespace {
    type Error = HornedError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Namespace::all()
            .into_iter()
            .find_map(|ns| {
                if ns.as_bytes() == value {
                    Some(ns)
                } else {
                    None
                }
        }).ok_or_else(|| invalid!("Unknown namespace: {:?}", value))
    }
}

impl std::ops::Deref for Namespace {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
    }
}

lazy_meta! {
    Namespace, IRI<String>, METANS;
    OWL, set_iri("http://www.w3.org/2002/07/owl#");
    RDF, set_iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
    RDFS, set_iri("http://www.w3.org/2000/01/rdf-schema#");
    XSD, set_iri("http://www.w3.org/2001/XMLSchema#");
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum RDF {
    First,
    List,
    Nil,
    Rest,
    Type,
}

impl TryFrom<&str> for RDF {
    type Error = HornedError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        RDF::all()
        .into_iter()
        .find_map(|ns| {
            if ns.as_ref() == value {
                Some(ns)
            } else {
                None
            }
        }).ok_or_else(|| invalid!("Unknown RDF attribute: {}", value))
    }
}

impl TryFrom<&[u8]> for RDF {
    type Error = HornedError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        RDF::all()
            .into_iter()
            .find_map(|ns| {
                if ns.as_bytes() == value {
                    Some(ns)
                } else {
                    None
                }
        }).ok_or_else(|| invalid!("Unknown RDF attribute: {:?}", value))
    }
}

impl std::ops::Deref for RDF {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
    }
}

lazy_meta! {
    RDF, IRI<String>, METARDF;
    First, append_to_ns(RDF,"first");
    List, append_to_ns(RDF,"List");
    Nil, append_to_ns(RDF,"nil");
    Rest, append_to_ns(RDF,"rest");
    Type, append_to_ns(RDF,"type");
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
        match &self {
            RDFS::Label | RDFS::Comment | RDFS::SeeAlso | RDFS::IsDefinedBy => true,
            _ => false,
        }
    }
}

impl TryFrom<&str> for RDFS {
    type Error = HornedError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        RDFS::all()
        .into_iter()
        .find_map(|ns| {
            if ns.as_ref() == value {
                Some(ns)
            } else {
                None
            }
        }).ok_or_else(|| invalid!("Unknown RDFS attribute: {}", value))
    }
}

impl TryFrom<&[u8]> for RDFS {
    type Error = HornedError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        RDFS::all()
            .into_iter()
            .find_map(|ns| {
                if ns.as_bytes() == value {
                    Some(ns)
                } else {
                    None
                }
        }).ok_or_else(|| invalid!("Unknown RDFS attribute: {:?}", value))
    }
}

impl std::ops::Deref for RDFS {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
    }
}


lazy_meta! {
    RDFS, IRI<String>, METARDFS;
    Comment, append_to_ns(RDFS,"comment");
    Datatype, append_to_ns(RDFS,"Datatype");
    Domain, append_to_ns(RDFS,"domain");
    IsDefinedBy, append_to_ns(RDFS,"isDefinedBy");
    Label, append_to_ns(RDFS,"label");
    Range, append_to_ns(RDFS,"range");
    SeeAlso, append_to_ns(RDFS,"seeAlso");
    SubClassOf, append_to_ns(RDFS,"subClassOf");
    SubPropertyOf, append_to_ns(RDFS,"subPropertyOf");
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

impl TryFrom<&str> for OWL {
    type Error = HornedError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        OWL::all()
        .into_iter()
        .find_map(|ns| {
            if ns.as_ref() == value {
                Some(ns)
            } else {
                None
            }
        }).ok_or_else(|| invalid!("Unknown OWL entity: {}", value))
    }
}

impl TryFrom<&[u8]> for OWL {
    type Error = HornedError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        OWL::all()
            .into_iter()
            .find_map(|ns| {
                if ns.as_bytes() == value {
                    Some(ns)
                } else {
                    None
                }
        }).ok_or_else(|| invalid!("Unknown OWL entity: {:?}", value))
    }
}

impl std::ops::Deref for OWL {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
    }
}

impl Borrow<str> for OWL {
    fn borrow(&self) -> &str {
        self.meta().as_ref()
    }
}

lazy_meta! {
    OWL, IRI<String>, METAOWL;

    AllDifferent, append_to_ns(OWL,"AllDifferent");
    AllDisjointProperties, append_to_ns(OWL,"AllDisjointProperties");
    AllValuesFrom, append_to_ns(OWL,"allValuesFrom");
    AnnotatedProperty, append_to_ns(OWL,"annotatedProperty");
    AnnotatedSource, append_to_ns(OWL,"annotatedSource");
    AnnotatedTarget, append_to_ns(OWL,"annotatedTarget");
    AnnotationProperty, append_to_ns(OWL,"AnnotationProperty");
    AssertionProperty, append_to_ns(OWL,"assertionProperty");
    AsymmetricProperty, append_to_ns(OWL,"AsymmetricProperty");
    Axiom, append_to_ns(OWL,"Axiom");
    Class, append_to_ns(OWL,"Class");
    ComplementOf, append_to_ns(OWL,"complementOf");
    DatatypeComplementOf, append_to_ns(OWL,"datatypeComplementOf");
    DatatypeProperty, append_to_ns(OWL,"DatatypeProperty");
    DifferentFrom, append_to_ns(OWL,"differentFrom");
    DisjointUnionOf, append_to_ns(OWL,"disjointUnionOf");
    DisjointWith, append_to_ns(OWL,"disjointWith");
    DistinctMembers, append_to_ns(OWL,"distinctMembers");
    Cardinality, append_to_ns(OWL,"cardinality");
    EquivalentClass, append_to_ns(OWL,"equivalentClass");
    EquivalentProperty, append_to_ns(OWL,"equivalentProperty");
    FunctionalProperty, append_to_ns(OWL,"FunctionalProperty");
    Imports, append_to_ns(OWL,"imports");
    IntersectionOf, append_to_ns(OWL,"intersectionOf");
    InverseFunctionalProperty, append_to_ns(OWL,"InverseFunctionalProperty");
    InverseOf, append_to_ns(OWL,"inverseOf");
    IrreflexiveProperty, append_to_ns(OWL,"IrreflexiveProperty");
    HasKey, append_to_ns(OWL,"hasKey");
    HasValue, append_to_ns(OWL,"hasValue");
    MaxCardinality, append_to_ns(OWL,"maxCardinality");
    MaxQualifiedCardinality, append_to_ns(OWL,"maxQualifiedCardinality");
    Members, append_to_ns(OWL,"members");
    MinCardinality, append_to_ns(OWL,"minCardinality");
    MinQualifiedCardinality, append_to_ns(OWL,"minQualifiedCardinality");
    NamedIndividual, append_to_ns(OWL,"NamedIndividual");
    NegativePropertyAssertion, append_to_ns(OWL,"NegativePropertyAssertion");
    Nothing, append_to_ns(OWL,"Nothing");
    ObjectProperty, append_to_ns(OWL,"ObjectProperty");
    OnClass, append_to_ns(OWL,"onClass");
    OnDataRange, append_to_ns(OWL,"onDataRange");
    OnDatatype, append_to_ns(OWL,"onDatatype");
    OneOf, append_to_ns(OWL,"oneOf");
    OnProperty, append_to_ns(OWL,"onProperty");
    Ontology, append_to_ns(OWL,"Ontology");
    PropertyChainAxiom, append_to_ns(OWL,"propertyChainAxiom");
    PropertyDisjointWith, append_to_ns(OWL,"propertyDisjointWith");
    QualifiedCardinality, append_to_ns(OWL,"qualifiedCardinality");
    ReflexiveProperty, append_to_ns(OWL,"ReflexiveProperty");
    Restriction, append_to_ns(OWL,"Restriction");
    SameAs, append_to_ns(OWL,"sameAs");
    SourceIndividual, append_to_ns(OWL,"sourceIndividual");
    SomeValuesFrom, append_to_ns(OWL,"someValuesFrom");
    SymmetricProperty, append_to_ns(OWL,"SymmetricProperty");
    TargetIndividual, append_to_ns(OWL,"targetIndividual");
    TargetValue, append_to_ns(OWL,"targetValue");
    Thing, append_to_ns(OWL,"Thing");
    TopDataProperty, append_to_ns(OWL,"topDataProperty");
    TopObjectProperty, append_to_ns(OWL,"topObjectProperty");
    TransitiveProperty, append_to_ns(OWL,"TransitiveProperty");
    UnionOf, append_to_ns(OWL,"unionOf");
    VersionIRI, append_to_ns(OWL,"versionIRI");
    WithRestrictions, append_to_ns(OWL,"withRestrictions");
}

// TODO: change parameter from `IRI<A>` to `Class<A>`.
//       Only Classes can be owl:Thing?
pub fn is_thing<A: ForIRI>(iri: &IRI<A>) -> bool {
    iri.as_ref() == OWL::Thing.as_ref()
}

// TODO: change parameter from `IRI<A>` to `Class<A>`.
//       Only Classes can be owl:Nothing?
pub fn is_nothing<A: ForIRI>(iri: &IRI<A>) -> bool {
    iri.as_ref() == OWL::Nothing.as_ref()
}

/// Returns a [NamedEntityKind] if the IRI points to a built-in entity, otherwise [None].
pub fn to_built_in_entity<A: ForIRI>(iri: &IRI<A>) -> Option<NamedEntityKind> {
    let ir = iri.as_ref();
    match ir {
        _ if ir == OWL::TopDataProperty.as_ref() => Some(NamedEntityKind::DataProperty),
        _ if ir == OWL::TopObjectProperty.as_ref() => Some(NamedEntityKind::ObjectProperty),
        _ if ir == OWL::Thing.as_ref() => Some(NamedEntityKind::Class),
        _ if ir == OWL::Nothing.as_ref() => Some(NamedEntityKind::Class),
        _ => None,
    }
}

#[test]
fn test_namespace_try_from() {
    assert_eq!("http://www.w3.org/2002/07/owl#", OWL.as_ref());
    assert_eq!(b"http://www.w3.org/2002/07/owl#", OWL.as_bytes());

    assert!(Namespace::try_from("http://www.w3.org/2002/07/owl#").is_ok());
    assert!(Namespace::try_from("http://www.example.org/2002/07/owl#").is_err());

    assert!(Namespace::try_from(b"http://www.w3.org/2002/07/owl#".as_ref()).is_ok());
    assert!(Namespace::try_from(b"http://www.example.org/2002/07/owl#".as_ref()).is_err());
}

#[test]
fn test_to_built_in_entity() {
    let builder = Build::new_rc();
    let iri_top_dp = builder.iri(OWL::TopDataProperty.as_ref());
    let iri_top_op = builder.iri(OWL::TopObjectProperty.as_ref());
    let iri_thing = builder.iri(OWL::Thing.as_ref());
    let iri_nothing = builder.iri(OWL::Nothing.as_ref());
    assert_eq!(
        to_built_in_entity(&iri_top_dp),
        Some(NamedEntityKind::DataProperty)
    );
    assert_eq!(
        to_built_in_entity(&iri_top_op),
        Some(NamedEntityKind::ObjectProperty)
    );
    assert_eq!(to_built_in_entity(&iri_thing), Some(NamedEntityKind::Class));
    assert_eq!(
        to_built_in_entity(&iri_nothing),
        Some(NamedEntityKind::Class)
    );
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

    match &type_iri.borrow().strip_prefix(Namespace::OWL.as_ref()) {
        Some("Class") => Ok(b.class(entity_iri).into()),
        Some("ObjectProperty") => Ok(b.object_property(entity_iri).into()),
        Some("DatatypeProperty") => Ok(b.data_property(entity_iri).into()),
        Some("AnnotationProperty") => Ok(b.annotation_property(entity_iri).into()),
        Some("NamedIndividual") => Ok(b.named_individual(entity_iri).into()),
        _ => Err(invalid!(
            "IRI is not a type of entity:{:?}",
            type_iri.borrow()
        )),
    }
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
}

pub enum OWL2Datatype {
    RDFSLiteral,
}

impl TryFrom<&str> for OWL2Datatype {
    type Error = HornedError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        OWL2Datatype::all()
        .into_iter()
        .find_map(|ns| {
            if ns.as_ref() == value {
                Some(ns)
            } else {
                None
            }
        }).ok_or_else(|| invalid!("Unknown OWL2 datatype: {}", value))
    }
}

impl TryFrom<&[u8]> for OWL2Datatype {
    type Error = HornedError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        OWL2Datatype::all()
            .into_iter()
            .find_map(|ns| {
                if ns.as_bytes() == value {
                    Some(ns)
                } else {
                    None
                }
        }).ok_or_else(|| invalid!("Unknown OWL2 datatype: {:?}", value))
    }
}

impl std::ops::Deref for OWL2Datatype {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
    }
}

lazy_meta! {
    OWL2Datatype, IRI<String>, METAOWL2DATATYPE;
    RDFSLiteral, append_to_ns(RDFS, "Literal")
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

impl TryFrom<&str> for AnnotationBuiltIn {
    type Error = HornedError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        AnnotationBuiltIn::all()
        .into_iter()
        .find_map(|ns| {
            if ns.as_ref() == value {
                Some(ns)
            } else {
                None
            }
        }).ok_or_else(|| invalid!("Unknown annotation keyword: {}", value))
    }
}

impl TryFrom<&[u8]> for AnnotationBuiltIn {
    type Error = HornedError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        AnnotationBuiltIn::all()
            .into_iter()
            .find_map(|ns| {
                if ns.as_bytes() == value {
                    Some(ns)
                } else {
                    None
                }
        }).ok_or_else(|| invalid!("Unknown annotation keyword: {:?}", value))
    }
}

impl std::ops::Deref for AnnotationBuiltIn {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
    }
}

lazy_meta! {
    AnnotationBuiltIn, IRI<String>, METAANNOTATIONBUILTIN;
    LABEL, append_to_ns(RDFS, "label");
    COMMENT, append_to_ns(RDFS, "comment");
    SEEALSO, append_to_ns(RDFS, "seeAlso");
    ISDEFINEDBY, append_to_ns(RDFS, "isDefinedBy");
    DEPRECATED, append_to_ns(OWL, "deprecated");
    VERSIONINFO, append_to_ns(OWL, "versionInfo");
    PRIORVERSION, append_to_ns(OWL, "priorVersion");
    BACKWARDCOMPATIBLEWITH, append_to_ns(OWL, "backwardCompatibleWith");
    INCOMPATIBLEWITH, append_to_ns(OWL, "incompatibleWith");
}

#[inline]
pub fn is_annotation_builtin<A: AsRef<str>>(iri: A) -> bool {
    AnnotationBuiltIn::try_from(iri.as_ref()).is_ok()
    // AnnotationBuiltIn::all()
    //     .iter()
    //     .any(|item| item.as_ref() == iri.as_ref())
}

#[test]
fn test_is_annotation_builtin() {
    // Method can accept `str`ings...
    assert!(is_annotation_builtin(
        "http://www.w3.org/2002/07/owl#deprecated"
    ));
    // ...and `String`s.
    assert!(is_annotation_builtin(
        &"http://www.w3.org/2000/01/rdf-schema#comment".to_string()
    ));
    assert!(!is_annotation_builtin(
        &"http://www.w3.org/2002/07/owl#fred".to_string()
    ));
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Facet {
    Length,
    MinLength,
    MaxLength,
    Pattern,
    MinInclusive,
    MinExclusive,
    MaxInclusive,
    MaxExclusive,
    TotalDigits,
    FractionDigits,
    LangRange,
}

impl TryFrom<&str> for Facet {
    type Error = HornedError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Facet::all()
        .into_iter()
        .find_map(|ns| {
            if ns.as_ref() == value {
                Some(ns)
            } else {
                None
            }
        }).ok_or_else(|| invalid!("Unknown facet: {}", value))
    }
}

impl TryFrom<&[u8]> for Facet {
    type Error = HornedError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        Facet::all()
            .into_iter()
            .find_map(|ns| {
                if ns.as_bytes() == value {
                    Some(ns)
                } else {
                    None
                }
        }).ok_or_else(|| invalid!("Unknown facet: {:?}", value))
    }
}

impl std::ops::Deref for Facet {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
    }
}

lazy_meta! {
    Facet, IRI<String>, METAFACET;
    Length, append_to_ns(XSD, "length");
    MinLength, append_to_ns(XSD, "minLength");
    MaxLength, append_to_ns(XSD, "maxLength");
    Pattern, append_to_ns(XSD, "pattern");
    MinInclusive, append_to_ns(XSD, "minInclusive");
    MinExclusive, append_to_ns(XSD, "minExclusive");
    MaxInclusive, append_to_ns(XSD, "maxInclusive");
    MaxExclusive, append_to_ns(XSD, "maxExclusive");
    TotalDigits, append_to_ns(XSD, "totalDigits");
    FractionDigits, append_to_ns(XSD, "fractionDigits");
    LangRange, append_to_ns(RDF, "langRange");
}

#[test]
fn test_facet_meta() {
    assert_eq!(
        Facet::MinLength.as_ref(),
        "http://www.w3.org/2001/XMLSchema#minLength"
    );

    assert_eq!(
        Facet::Pattern.as_ref(),
        "http://www.w3.org/2001/XMLSchema#pattern"
    );

    assert_eq!(
        Facet::try_from("http://www.w3.org/2001/XMLSchema#pattern").unwrap(),
        Facet::Pattern
    );

    assert_eq!(
        Facet::try_from(b"http://www.w3.org/2001/XMLSchema#minExclusive".as_ref()).unwrap(),
        Facet::MinExclusive
    );
}

pub enum XSD {
    NonNegativeInteger,
}

pub fn is_xsd_datatype<A: AsRef<str>>(iri: A) -> bool {
    // This only checks that the IRI starts with the XSD namespace.
    iri.as_ref().starts_with(Namespace::XSD.as_ref())
}

impl TryFrom<&str> for XSD {
    type Error = HornedError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        XSD::all()
        .into_iter()
        .find_map(|ns| {
            if ns.as_ref() == value {
                Some(ns)
            } else {
                None
            }
        }).ok_or_else(|| invalid!("Unknown XSD datatype: {}", value))
    }
}

impl TryFrom<&[u8]> for XSD {
    type Error = HornedError;

    fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
        XSD::all()
            .into_iter()
            .find_map(|ns| {
                if ns.as_bytes() == value {
                    Some(ns)
                } else {
                    None
                }
        }).ok_or_else(|| invalid!("Unknown XSD datatype: {:?}", value))
    }
}

impl std::ops::Deref for XSD {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
    }
}

lazy_meta! {
    XSD, IRI<String>, METAXSD;
    NonNegativeInteger, append_to_ns(XSD, "nonNegativeInteger")
}

#[test]
fn test_is_xsd_datatype() {
    assert!(is_xsd_datatype(
        "http://www.w3.org/2001/XMLSchema#nonNegativeInteger"
    ));
    assert!(!is_xsd_datatype(
        "http://www.w3.org/2001/XMLSchemaaa#nonNegativeInteger"
    ));
    assert!(!is_xsd_datatype("http://www.w3.org/2001/XMLSchema.pdf"));
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
        let ns_all = Namespace::all()
            .into_iter()
            .map(|variant| Self::Namespace(variant));

        facet_all
            .chain(rdf_all)
            .chain(rdfs_all)
            .chain(owl_all)
            .chain(xsd_all)
            .chain(ns_all)
            .collect()
    }
}

impl std::ops::Deref for Vocab {
    type Target = IRI<String>;

    fn deref(&self) -> &Self::Target {
        self.meta()
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
