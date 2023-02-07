use crate::model::ForIRI;
use oxiri::{Iri, IriParseError};
use std::{fmt::Debug,
          hash::Hash,
          ops::Deref,
          borrow::Borrow};

pub struct IriMaybe<T: ForIRI> {
    raw: T,
    iri_maybe: Option<Result<Iri<T>, IriParseError>>,
}

impl<T: Deref<Target=str> + ForIRI> IriMaybe<T> {
    pub fn new(t:T) -> IriMaybe<T> {
        IriMaybe{raw:t, iri_maybe:None}
    }

    pub fn as_raw(&self) -> &T {
        &self.raw
    }

    pub fn to_raw(self) -> T {
        self.raw
    }

    // This shouldn't be mutable probably
    pub fn as_parsed(&mut self) -> &Result<Iri<T>, IriParseError>{
        if self.iri_maybe.is_none() {
            self.iri_maybe = Some(Iri::parse(self.raw.clone()));
        }

        match &self.iri_maybe {
            Some(res) => res,
            None => panic!("IriMaybe in an inconsistent state"),
        }
    }

    pub fn to_parsed(mut self) -> Result<Iri<T>, IriParseError> {
        let _ = self.as_parsed();
        self.iri_maybe.unwrap()
    }
}

impl<T: ForIRI> AsRef<str> for IriMaybe<T> {
    fn as_ref(&self) -> &str {
        &self.raw.as_ref()
    }
}

impl<T: ForIRI> Borrow<str> for IriMaybe<T> {
    fn borrow(&self) -> &str {
        &self.raw.borrow()
    }
}

impl<T: ForIRI> Clone for IriMaybe<T> {
    fn clone(&self) -> Self {
        Self { raw: self.raw.clone(), iri_maybe: None }
    }
}

impl<T: ForIRI> Debug for IriMaybe<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("IriMaybe{")?;
        self.raw.fmt(f)?;
        f.write_str("}")
    }
}

impl<T: ForIRI> Eq for IriMaybe<T> {
}

impl<T: ForIRI> Hash for IriMaybe<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

impl<T: ForIRI> Ord for IriMaybe<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.raw.cmp(&other.raw)
    }
}

impl<T: ForIRI> PartialEq for IriMaybe<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<T: ForIRI> PartialOrd for IriMaybe<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.raw.partial_cmp(&other.raw)
    }
}


impl<T: Deref<Target=str> + ForIRI> From<String> for IriMaybe<T> {
    fn from(s:String) -> IriMaybe<T> {
        IriMaybe::new(s.into())
    }
}


#[cfg(test)]
mod test {
    use std::rc::Rc;

    use super::*;
    use crate::model::*;

    #[test]
    fn build_iri_from_iri_maybe() {
        let b:Build<IriMaybe<Rc<str>>> = Build::new();

        let _iri = b.iri("http://www.example.com");
        assert!(true)

    }

    #[test]
    fn iri_maybe_to_oxiri() {
        let b:Build<IriMaybe<Rc<str>>> = Build::new();

        let iri = b.iri("http://www.example.com");
        let iri_maybe = iri.underlying();
        let oxiri:oxiri::Iri<_> = iri_maybe.to_parsed().ok().unwrap();

        assert_eq!(oxiri.authority(), Some("www.example.com"));
    }
}
