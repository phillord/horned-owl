//! Core RDF vocabularies used in the OWL2 RDF format.
use enum_meta::*;

use crate::error::invalid;
use crate::error::HornedError;
use crate::model::Build;
use crate::model::IRI;
use crate::model::{NamedOWLEntity, NamedOWLEntityKind};

use std::borrow::Borrow;
use std::convert::TryFrom;
use std::str::FromStr;

macro_rules! vocabulary_traits {
    ($($enum_type:ident),+; $return_type:ty
    ) => {

        $(
            impl TryFrom<&[u8]> for $enum_type {
            type Error = HornedError;

            fn try_from(value: &[u8]) -> Result<Self, Self::Error> {
                $enum_type::all()
                    .into_iter()
                    .find_map(|variant| {
                        if variant.as_bytes() == value {
                            Some(variant)
                        } else {
                            None
                        }
                }).ok_or_else(|| invalid!("Unknown {} variant: {:?}", stringify!{$enum_type}, value))
            }
            }

            impl std::str::FromStr for $enum_type {
                type Err = HornedError;

                fn from_str(s: &str) -> Result<Self, Self::Err> {
                    $enum_type::try_from(s.as_bytes())
                }
            }

            impl TryFrom<&str> for $enum_type {
                type Error = HornedError;

                fn try_from(value: &str) -> Result<Self, Self::Error> {
                    $enum_type::from_str(value)
                }
            }

            impl std::ops::Deref for $enum_type {
                type Target = $return_type;

                fn deref(&self) -> &Self::Target {
                    self.meta()
                }
            }

            impl Borrow<str> for $enum_type {
                fn borrow(&self) -> &str {
                    self.meta().borrow()
                }
            }
        )+
    };
}

macro_rules! vocabulary_type {
    ($(#[$attr:meta])* $enum_type:ident, $return_type:ty, $storage:ident, [$(($ns:ident, $variant:ident, $first_lowercase:expr)),*]) => {

        $(#[$attr]) *
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum $enum_type {
            $(
                $variant,
            )*
        }

        impl $enum_type {
            fn get_iri(self) -> IRI<String> {

                match self {
                    $(
                        $enum_type::$variant => {
                            let mut iri_str = String::from(Borrow::<str>::borrow(&Namespace::$ns));
                            let mut variant_str = String::from(stringify!($variant));
                            if $first_lowercase {
                                let tail = variant_str.split_off(1);
                                variant_str.make_ascii_lowercase();
                                variant_str.push_str(&tail);
                            }
                            iri_str.push_str(&variant_str);
                            IRI(iri_str)
                        }
                    )*,
                }
            }
        }

        lazy_meta! {
            $enum_type, IRI<String>, $storage;
            $(
                $variant, $enum_type::get_iri($enum_type::$variant);
            )*

        }

        vocabulary_traits! { $enum_type; $return_type }
    }
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
    /// SWRL
    SWRL,
    /// XML Schema datatype
    XSD,
}

vocabulary_traits! {
    Namespace;
    IRI<String>
}

lazy_meta! {
    Namespace, IRI<String>, METANS;
    OWL, IRI(String::from("http://www.w3.org/2002/07/owl#"));
    RDF, IRI(String::from("http://www.w3.org/1999/02/22-rdf-syntax-ns#"));
    RDFS, IRI(String::from("http://www.w3.org/2000/01/rdf-schema#"));
    SWRL, IRI(String::from("http://www.w3.org/2003/11/swrl#"));
    XSD, IRI(String::from("http://www.w3.org/2001/XMLSchema#"));
}

vocabulary_type! {
    /// RDF Collections vocabulary.
    RDF, IRI<String>, METARDF, [
        (RDF, List, false),
        (RDF, First, true),
        (RDF, Nil, true),
        (RDF, Rest, true),
        (RDF, Type, true)
    ]
}

vocabulary_type! {
    RDFS, IRI<String>, METARDFS, [
        (RDFS, Comment, true),
        (RDFS, Datatype, false),
        (RDFS, Domain, true),
        (RDFS, IsDefinedBy, true),
        (RDFS, Label, true),
        (RDFS, Range, true),
        (RDFS, SeeAlso, true),
        (RDFS, SubClassOf, true),
        (RDFS, SubPropertyOf, true)
    ]
}

impl RDFS {
    pub fn is_builtin(&self) -> bool {
        matches! {&self,
            RDFS::Label | RDFS::Comment | RDFS::SeeAlso | RDFS::IsDefinedBy
        }
    }
}

vocabulary_type! {
    OWL, IRI<String>, METAOWL, [
        (OWL, AllDifferent, false),
        (OWL, AllDisjointProperties, false),
        (OWL, AllValuesFrom, true),
        (OWL, AnnotatedProperty, true),
        (OWL, AnnotatedSource, true),
        (OWL, AnnotatedTarget, true),
        (OWL, AnnotationProperty, false),
        (OWL, AssertionProperty, true),
        (OWL, AsymmetricProperty, false),
        (OWL, Axiom, false),
        (OWL, Cardinality, true),
        (OWL, Class, false),
        (OWL, ComplementOf, true),
        (OWL, DatatypeComplementOf, true),
        (OWL, DatatypeProperty, false),
        (OWL, DifferentFrom, true),
        (OWL, DisjointUnionOf, true),
        (OWL, DisjointWith, true),
        (OWL, DistinctMembers, true),
        (OWL, EquivalentClass, true),
        (OWL, EquivalentProperty, true),
        (OWL, FunctionalProperty, false),
        (OWL, HasKey, true),
        (OWL, HasSelf, true),
        (OWL, HasValue, true),
        (OWL, Imports, true),
        (OWL, IntersectionOf, true),
        (OWL, InverseFunctionalProperty, false),
        (OWL, InverseOf, true),
        (OWL, IrreflexiveProperty, false),
        (OWL, MaxCardinality, true),
        (OWL, MaxQualifiedCardinality, true),
        (OWL, Members, true),
        (OWL, MinCardinality, true),
        (OWL, MinQualifiedCardinality, true),
        (OWL, NamedIndividual, false),
        (OWL, NegativePropertyAssertion, false),
        (OWL, Nothing, false),
        (OWL, ObjectProperty, false),
        (OWL, OnClass, true),
        (OWL, OnDataRange, true),
        (OWL, OnDatatype, true),
        (OWL, OneOf, true),
        (OWL, OnProperty, true),
        (OWL, Ontology, false),
        (OWL, QualifiedCardinality, true),
        (OWL, PropertyChainAxiom, true),
        (OWL, PropertyDisjointWith, true),
        (OWL, ReflexiveProperty, false),
        (OWL, Restriction, false),
        (OWL, SameAs, true),
        (OWL, SourceIndividual, true),
        (OWL, SomeValuesFrom, true),
        (OWL, SymmetricProperty, false),
        (OWL, TargetIndividual, true),
        (OWL, TargetValue, true),
        (OWL, TopDataProperty, true),
        (OWL, TopObjectProperty, true),
        (OWL, Thing, false),
        (OWL, TransitiveProperty, false),
        (OWL, UnionOf, true),
        (OWL, VersionIRI, true),
        (OWL, WithRestrictions, true)
    ]
}

/// Returns a [NamedOWLEntityKind] if the IRI points to a built-in entity, otherwise [None].
pub fn to_built_in_entity<A: Borrow<str>>(iri: &IRI<A>) -> Option<NamedOWLEntityKind> {
    let ir = Borrow::<str>::borrow(iri);
    match ir {
        _ if ir == Borrow::<str>::borrow(&OWL::TopDataProperty) => {
            Some(NamedOWLEntityKind::DataProperty)
        }
        _ if ir == Borrow::<str>::borrow(&OWL::TopObjectProperty) => {
            Some(NamedOWLEntityKind::ObjectProperty)
        }
        _ if ir == Borrow::<str>::borrow(&OWL::Thing) => Some(NamedOWLEntityKind::Class),
        _ if ir == Borrow::<str>::borrow(&OWL::Nothing) => Some(NamedOWLEntityKind::Class),
        _ => None,
    }
}

pub fn entity_for_iri<A, S: Borrow<str>>(
    type_iri: S,
    entity_iri: S,
    b: &Build<A>,
) -> Result<NamedOWLEntity<A>, HornedError>
where
    A: Borrow<str> + Clone + From<String> + Ord,
{
    // Datatypes are handled here because they are not a
    // "type" but an "RDF schema" element.
    if type_iri.borrow() == "http://www.w3.org/2000/01/rdf-schema#Datatype" {
        return Ok(b.datatype(entity_iri).into());
    }

    match &type_iri
        .borrow()
        .strip_prefix(Borrow::<str>::borrow(&Namespace::OWL))
    {
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

vocabulary_type! {
    OWL2Datatype,
    IRI<String>,
    METAOWL2DATATYPE,
    [(RDFS, Literal, false)]
}

vocabulary_type! {
    AnnotationBuiltIn, IRI<String>, METAANNOTATIONBUILTIN, [
        (RDFS, Label, true),
        (RDFS, Comment, true),
        (RDFS, SeeAlso, true),
        (RDFS, IsDefinedBy, true),
        (OWL, Deprecated, true),
        (OWL, VersionInfo, true),
        (OWL, PriorVersion, true),
        (OWL, BackwardCompatibleWith, true),
        (OWL, IncompatibleWith, true)
    ]
}

#[inline]
pub fn is_annotation_builtin<A: Borrow<str> + ?Sized>(iri: &A) -> bool {
    AnnotationBuiltIn::from_str(iri.borrow()).is_ok()
}

vocabulary_type! {
    Facet, IRI<String>, METAFACET, [
        (XSD, Length, true),
        (XSD, MinLength, true),
        (XSD, MaxLength, true),
        (XSD, Pattern, true),
        (XSD, MinInclusive, true),
        (XSD, MinExclusive, true),
        (XSD, MaxInclusive, true),
        (XSD, MaxExclusive, true),
        (XSD, TotalDigits, true),
        (XSD, FractionDigits, true),
        (RDF, LangRange, true)
    ]
}

vocabulary_type! {
    XSD, IRI<String>, METAXSD, [
        (XSD, Boolean, true),
        (XSD, NonNegativeInteger, true)
    ]
}

pub fn is_xsd_datatype<A: Borrow<str>>(iri: &A) -> bool {
    // This only checks that the IRI starts with the XSD namespace.
    Borrow::<str>::borrow(iri).starts_with(Borrow::<str>::borrow(&Namespace::XSD))
}

vocabulary_type! {
    SWRL, IRI<String>, METASWRL, [
        (SWRL, Argument1, true),
        (SWRL, Argument2, true),
        (SWRL, Arguments, true),
        (SWRL, AtomList, false),
        (SWRL, Builtin, true),
        (SWRL, BuiltinAtom, false),
        (SWRL, Body, true),
        (SWRL, ClassAtom, false),
        (SWRL, ClassPredicate, true),
        (SWRL, DataRange, true),
        (SWRL, DataRangeAtom, false),
        (SWRL, DatavaluedPropertyAtom, false),
        (SWRL, DifferentIndividualsAtom, false),
        (SWRL, Head, true),
        (SWRL, Imp, false),
        (SWRL, IndividualPropertyAtom, false),
        (SWRL, PropertyPredicate, true),
        (SWRL, SameIndividualAtom, false),
        (SWRL, Variable, false)
    ]
}

pub enum Vocab {
    Facet(Facet),
    RDF(RDF),
    RDFS(RDFS),
    OWL(OWL),
    SWRL(SWRL),
    XSD(XSD),
    Namespace(Namespace),
}

vocabulary_traits! {
    Vocab;
    IRI<String>
}

impl<'a> Meta<&'a IRI<String>> for Vocab {
    fn meta(&self) -> &'a IRI<String> {
        match self {
            Self::Facet(facet) => facet.meta(),
            Self::RDF(rdf) => rdf.meta(),
            Self::RDFS(rdfs) => rdfs.meta(),
            Self::OWL(owl) => owl.meta(),
            Self::SWRL(swrl) => swrl.meta(),
            Self::XSD(xsd) => xsd.meta(),
            Self::Namespace(ns) => ns.meta(),
        }
    }

    fn all() -> Vec<Self> {
        let facet_all = Facet::all().into_iter().map(Self::Facet);
        let rdf_all = RDF::all().into_iter().map(Self::RDF);
        let rdfs_all = RDFS::all().into_iter().map(Self::RDFS);
        let owl_all = OWL::all().into_iter().map(Self::OWL);
        let swrl_all = SWRL::all().into_iter().map(Self::SWRL);
        let xsd_all = XSD::all().into_iter().map(Self::XSD);
        let ns_all = Namespace::all().into_iter().map(Self::Namespace);

        facet_all
            .chain(rdf_all)
            .chain(rdfs_all)
            .chain(owl_all)
            .chain(swrl_all)
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

impl From<SWRL> for Vocab {
    fn from(owl: SWRL) -> Self {
        Self::SWRL(owl)
    }
}

impl From<XSD> for Vocab {
    fn from(xsd: XSD) -> Self {
        Self::XSD(xsd)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_meta_rdf() {
        assert_eq!(
            Borrow::<str>::borrow(&RDF::First),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDF::List),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDF::Nil),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDF::Rest),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDF::Type),
            "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
        );
    }

    #[test]
    fn test_meta_rdfs() {
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::Comment),
            "http://www.w3.org/2000/01/rdf-schema#comment"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::Datatype),
            "http://www.w3.org/2000/01/rdf-schema#Datatype"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::Domain),
            "http://www.w3.org/2000/01/rdf-schema#domain"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::IsDefinedBy),
            "http://www.w3.org/2000/01/rdf-schema#isDefinedBy"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::Label),
            "http://www.w3.org/2000/01/rdf-schema#label"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::Range),
            "http://www.w3.org/2000/01/rdf-schema#range"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::SeeAlso),
            "http://www.w3.org/2000/01/rdf-schema#seeAlso"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::SubClassOf),
            "http://www.w3.org/2000/01/rdf-schema#subClassOf"
        );
        assert_eq!(
            Borrow::<str>::borrow(&RDFS::SubPropertyOf),
            "http://www.w3.org/2000/01/rdf-schema#subPropertyOf"
        );
    }

    #[test]
    fn test_meta_owl() {
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AllDifferent),
            "http://www.w3.org/2002/07/owl#AllDifferent"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AllDisjointProperties),
            "http://www.w3.org/2002/07/owl#AllDisjointProperties"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AllValuesFrom),
            "http://www.w3.org/2002/07/owl#allValuesFrom"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AnnotatedProperty),
            "http://www.w3.org/2002/07/owl#annotatedProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AnnotatedSource),
            "http://www.w3.org/2002/07/owl#annotatedSource"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AnnotatedTarget),
            "http://www.w3.org/2002/07/owl#annotatedTarget"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AnnotationProperty),
            "http://www.w3.org/2002/07/owl#AnnotationProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AssertionProperty),
            "http://www.w3.org/2002/07/owl#assertionProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::AsymmetricProperty),
            "http://www.w3.org/2002/07/owl#AsymmetricProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Axiom),
            "http://www.w3.org/2002/07/owl#Axiom"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Cardinality),
            "http://www.w3.org/2002/07/owl#cardinality"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Class),
            "http://www.w3.org/2002/07/owl#Class"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::ComplementOf),
            "http://www.w3.org/2002/07/owl#complementOf"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::DatatypeComplementOf),
            "http://www.w3.org/2002/07/owl#datatypeComplementOf"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::DatatypeProperty),
            "http://www.w3.org/2002/07/owl#DatatypeProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::DifferentFrom),
            "http://www.w3.org/2002/07/owl#differentFrom"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::DisjointUnionOf),
            "http://www.w3.org/2002/07/owl#disjointUnionOf"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::DisjointWith),
            "http://www.w3.org/2002/07/owl#disjointWith"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::DistinctMembers),
            "http://www.w3.org/2002/07/owl#distinctMembers"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::EquivalentClass),
            "http://www.w3.org/2002/07/owl#equivalentClass"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::EquivalentProperty),
            "http://www.w3.org/2002/07/owl#equivalentProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::FunctionalProperty),
            "http://www.w3.org/2002/07/owl#FunctionalProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::HasKey),
            "http://www.w3.org/2002/07/owl#hasKey"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::HasValue),
            "http://www.w3.org/2002/07/owl#hasValue"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Imports),
            "http://www.w3.org/2002/07/owl#imports"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::IntersectionOf),
            "http://www.w3.org/2002/07/owl#intersectionOf"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::InverseFunctionalProperty),
            "http://www.w3.org/2002/07/owl#InverseFunctionalProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::InverseOf),
            "http://www.w3.org/2002/07/owl#inverseOf"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::IrreflexiveProperty),
            "http://www.w3.org/2002/07/owl#IrreflexiveProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::MaxCardinality),
            "http://www.w3.org/2002/07/owl#maxCardinality"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::MaxQualifiedCardinality),
            "http://www.w3.org/2002/07/owl#maxQualifiedCardinality"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Members),
            "http://www.w3.org/2002/07/owl#members"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::MinCardinality),
            "http://www.w3.org/2002/07/owl#minCardinality"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::MinQualifiedCardinality),
            "http://www.w3.org/2002/07/owl#minQualifiedCardinality"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::NamedIndividual),
            "http://www.w3.org/2002/07/owl#NamedIndividual"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::NegativePropertyAssertion),
            "http://www.w3.org/2002/07/owl#NegativePropertyAssertion"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Nothing),
            "http://www.w3.org/2002/07/owl#Nothing"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::ObjectProperty),
            "http://www.w3.org/2002/07/owl#ObjectProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::OnClass),
            "http://www.w3.org/2002/07/owl#onClass"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::OnDataRange),
            "http://www.w3.org/2002/07/owl#onDataRange"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::OnDatatype),
            "http://www.w3.org/2002/07/owl#onDatatype"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::OneOf),
            "http://www.w3.org/2002/07/owl#oneOf"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::OnProperty),
            "http://www.w3.org/2002/07/owl#onProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Ontology),
            "http://www.w3.org/2002/07/owl#Ontology"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::QualifiedCardinality),
            "http://www.w3.org/2002/07/owl#qualifiedCardinality"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::PropertyChainAxiom),
            "http://www.w3.org/2002/07/owl#propertyChainAxiom"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::PropertyDisjointWith),
            "http://www.w3.org/2002/07/owl#propertyDisjointWith"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::ReflexiveProperty),
            "http://www.w3.org/2002/07/owl#ReflexiveProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Restriction),
            "http://www.w3.org/2002/07/owl#Restriction"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::SameAs),
            "http://www.w3.org/2002/07/owl#sameAs"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::SourceIndividual),
            "http://www.w3.org/2002/07/owl#sourceIndividual"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::SomeValuesFrom),
            "http://www.w3.org/2002/07/owl#someValuesFrom"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::SymmetricProperty),
            "http://www.w3.org/2002/07/owl#SymmetricProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::TargetIndividual),
            "http://www.w3.org/2002/07/owl#targetIndividual"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::TargetValue),
            "http://www.w3.org/2002/07/owl#targetValue"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::TopDataProperty),
            "http://www.w3.org/2002/07/owl#topDataProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::TopObjectProperty),
            "http://www.w3.org/2002/07/owl#topObjectProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::Thing),
            "http://www.w3.org/2002/07/owl#Thing"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::TransitiveProperty),
            "http://www.w3.org/2002/07/owl#TransitiveProperty"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::UnionOf),
            "http://www.w3.org/2002/07/owl#unionOf"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::VersionIRI),
            "http://www.w3.org/2002/07/owl#versionIRI"
        );
        assert_eq!(
            Borrow::<str>::borrow(&OWL::WithRestrictions),
            "http://www.w3.org/2002/07/owl#withRestrictions"
        );
    }

    #[test]
    fn test_namespace_try_from() {
        assert_eq!(
            "http://www.w3.org/2002/07/owl#",
            Borrow::<str>::borrow(&Namespace::OWL)
        );
        assert_eq!(b"http://www.w3.org/2002/07/owl#", Namespace::OWL.as_bytes());

        assert!(Namespace::from_str("http://www.w3.org/2002/07/owl#").is_ok());
        assert!(Namespace::from_str("http://www.example.org/2002/07/owl#").is_err());

        assert!(Namespace::try_from(b"http://www.w3.org/2002/07/owl#".as_ref()).is_ok());
        assert!(Namespace::try_from(b"http://www.example.org/2002/07/owl#".as_ref()).is_err());
    }

    #[test]
    fn test_to_built_in_entity() {
        let builder = Build::new_rc();
        let iri_top_dp = builder.iri(Borrow::<str>::borrow(&OWL::TopDataProperty));
        let iri_top_op = builder.iri(Borrow::<str>::borrow(&OWL::TopObjectProperty));
        let iri_thing = builder.iri(Borrow::<str>::borrow(&OWL::Thing));
        let iri_nothing = builder.iri(Borrow::<str>::borrow(&OWL::Nothing));
        assert_eq!(
            to_built_in_entity(&iri_top_dp),
            Some(NamedOWLEntityKind::DataProperty)
        );
        assert_eq!(
            to_built_in_entity(&iri_top_op),
            Some(NamedOWLEntityKind::ObjectProperty)
        );
        assert_eq!(
            to_built_in_entity(&iri_thing),
            Some(NamedOWLEntityKind::Class)
        );
        assert_eq!(
            to_built_in_entity(&iri_nothing),
            Some(NamedOWLEntityKind::Class)
        );
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

    #[test]
    fn test_meta_annotation_builtin() {
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

    #[test]
    fn test_meta_facet() {
        assert_eq!(
            Borrow::<str>::borrow(&Facet::MinLength),
            "http://www.w3.org/2001/XMLSchema#minLength"
        );

        assert_eq!(
            Borrow::<str>::borrow(&Facet::Pattern),
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

    #[test]
    fn test_is_xsd_datatype() {
        assert!(is_xsd_datatype(&IRI(
            "http://www.w3.org/2001/XMLSchema#nonNegativeInteger"
        )));
        assert!(!is_xsd_datatype(&IRI(
            "http://www.w3.org/2001/XMLSchemaaa#nonNegativeInteger"
        )));
        assert!(!is_xsd_datatype(&IRI(
            "http://www.w3.org/2001/XMLSchema.pdf"
        )));
    }
}
