//! Support conversion of HornedOWL data structures

//! # Overview
//!
//! This module supports the conversion of `IRI`s to `oxiri` which
//! provides parsing and validation support.
use crate::model::ForIRI;

use std::convert::TryInto;

impl<A: ForIRI> crate::model::IRI<A> {
    pub fn as_oxiri(&self) -> Result<oxiri::Iri<&str>, oxiri::IriParseError> {
        oxiri::Iri::parse(&self.0.borrow())
    }
}

impl<'a, A: ForIRI> TryInto<oxiri::Iri<&'a str>> for &'a crate::model::IRI<A> {
    type Error=oxiri::IriParseError;

    fn try_into(self) -> Result<oxiri::Iri<&'a str>, oxiri::IriParseError>{
        oxiri::Iri::parse(&self.0.borrow())
    }
}


#[cfg(test)]
mod test{
    use crate::model::Build;

    use std::convert::TryInto;

    #[test]
    fn test_oxiri() {
        let b = Build::new_rc();

        let iri = b.iri("http://www.example.com");

        let oxiri = iri.as_oxiri().unwrap();
        assert_eq!(oxiri.authority(), Some("www.example.com"));
    }

    #[test]
    fn test_try_into() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com");

        let oxiri:oxiri::Iri<&str> = (&iri).try_into().unwrap();
        assert_eq!(oxiri.authority(), Some("www.example.com"))
    }
}
