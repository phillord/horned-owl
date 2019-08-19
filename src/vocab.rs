use self::Namespace::*;
use enum_meta::*;
use crate::model::Facet;

pub trait WithIRI<'a>: Meta<&'a IRIString> {
    /// Return a string representation of the IRI associated with this
    /// entity.
    fn iri_s(&self) -> &'a String {
        &self.meta().0
    }

    fn iri_b(&self) -> &'a [u8] {
        &self.meta().0.as_bytes()
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

pub enum OWL {
    // TODO add the others
    Thing,
    Nothing,
}

lazy_meta! {
    OWL, IRIString, METAOWL;
    Thing, extend(OWL, "Thing");
    Nothing, extend(OWL, "Nothing");
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
fn annotation_builtin(){
    assert!(is_annotation_builtin(&"http://www.w3.org/2002/07/owl#deprecated".to_string()));
    assert!(is_annotation_builtin(&"http://www.w3.org/2000/01/rdf-schema#comment".to_string()));
    assert!(!is_annotation_builtin(&"http://www.w3.org/2002/07/owl#fred".to_string()));
}

lazy_meta!{
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
