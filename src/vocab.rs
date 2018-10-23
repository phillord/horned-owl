use enum_meta::*;
use model::Facet;
use self::Namespace::*;

pub trait WithIRI<'a>: Meta<&'a IRIString> {
    /// Return a string representation of the IRI associated with this
    /// entity.
    fn iri_s(&self) -> &'a String {
        &self.meta().0
    }

    fn iri_b(&self) ->&'a [u8] {
        &self.meta().0.as_bytes()
    }

    fn var_s(tag: &'a str) -> Option<Self>
    {
        Self::var_b(tag.as_bytes())
    }

    fn var_b(tag: &'a [u8]) -> Option<Self>{
        for v in Self::all() {
            if tag == v.iri_b() {
                return Some(v);
            }
        }
        None
    }
}

pub struct IRIString (String);

impl <'a, T> WithIRI<'a> for T
    where T:Meta<&'a IRIString>{
}

fn to_meta(s:&str) -> IRIString
{
    IRIString(s.to_string())
}

fn extend<'a, I>(i:I, s:&'a str) -> IRIString
    where I: WithIRI<'a>
{
    to_meta(&format!("{}{}", i.iri_s(), s))
}

#[derive(Debug, Eq, PartialEq)]
pub enum Namespace {
    OWL, RDF, RDFS, XSD
}

lazy_meta! {
    Namespace, IRIString, META_NS;
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
    OWL, IRIString, META_OWL;
    Thing, extend(OWL, "Thing");
    Nothing, extend(OWL, "Nothing");
}

pub enum OWL2Datatype{
    RDFSLiteral
}

lazy_meta! {
    OWL2Datatype, IRIString, META_OWL2Datatype;
    RDFSLiteral, extend(RDFS, "Literal")
}

#[test]
fn meta_testing() {
    assert_eq!("http://www.w3.org/2002/07/owl#", OWL.iri_s());
    assert_eq!(b"http://www.w3.org/2002/07/owl#", OWL.iri_b());

    assert_eq!(Namespace::var_s("http://www.w3.org/2002/07/owl#")
               .unwrap(),
               OWL);

    assert_eq!(Namespace::var_b(b"http://www.w3.org/2002/07/owl#")
               .unwrap(),
               OWL);
}

lazy_meta!{
    Facet, IRIString, META_FACET;
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

    assert_eq!(Facet::MinLength.iri_s(),
               "http://www.w3.org/2001/XMLSchema#minLength");

    assert_eq!(Facet::Pattern.iri_s(),
               "http://www.w3.org/2001/XMLSchema#pattern");

    assert_eq!(
        Facet::var_s(&"http://www.w3.org/2001/XMLSchema#pattern"
                     .to_string())
            .unwrap(),
        Facet::Pattern);

    assert_eq!(
        Facet::var_b(b"http://www.w3.org/2001/XMLSchema#minExclusive")
            .unwrap(),
        Facet::MinExclusive);
}
