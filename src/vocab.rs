use self::Namespace::*;
use enum_meta::*;

pub trait WithIRI<'a>: Meta<&'a IRIString> {
    /// Return a string representation of the IRI associated with this
    /// entity.
    fn iri_s(&self) -> &'a String {
        &self.meta().0
    }

    fn iri_b(&self) ->&'a [u8] {
        &self.meta().0.as_bytes()
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

pub fn var<'a, T>(all:Vec<T>, tag: &[u8]) -> Option<T>
    where T:WithIRI<'a>{
    for v in all {
        if tag == v.iri_b() {
            return Some(v);
        }
    }
    None
}

pub fn var_s<'a, T>(all:Vec<T>, tag: &'a str) -> Option<T>
    where T:WithIRI<'a>
{
    var(all, tag.as_bytes())
}

#[derive(Debug, Eq, PartialEq)]
pub enum Namespace {
    OWL, RDF, XSD
}

lazy_meta! {
    Namespace, IRIString, META_NS;
    OWL, to_meta("http://www.w3.org/2002/07/owl#");
    RDF, to_meta("http://www.w3.org/1999/02/22-rdf-syntax-ns#");
    XSD, to_meta("http://www.w3.org/2001/XMLSchema#");
}

#[test]
fn meta_testing() {
    assert_eq!("http://www.w3.org/2002/07/owl#", OWL.iri_s());
    assert_eq!(b"http://www.w3.org/2002/07/owl#", OWL.iri_b());

    assert_eq!(var_s(Namespace::all(),
                     "http://www.w3.org/2002/07/owl#")
               .unwrap(),
               OWL);

    assert_eq!(var(Namespace::all(),
                   b"http://www.w3.org/2002/07/owl#")
               .unwrap(),
               OWL);
}

pub mod facet {
    use ::model::Facet;
    use super::*;

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
            var_s(Facet::all(),
                  &"http://www.w3.org/2001/XMLSchema#pattern".to_string())
                .unwrap(),
            Facet::Pattern);

        assert_eq!(
            var(Facet::all(),
                b"http://www.w3.org/2001/XMLSchema#minExclusive"
            ).unwrap(),
            Facet::MinExclusive
        );
    }
}
