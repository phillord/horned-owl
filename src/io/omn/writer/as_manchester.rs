use std::fmt::{Display, Error, Formatter};
use std::marker::PhantomData;

use curie::PrefixMapping;

use crate::model::*;

/// OWL elements renderable in Manchester syntax.
pub trait AsManchester<A: ForIRI> {
    fn as_manchester(&self) -> Manchester<'_, Self, A> {
        Manchester(self, None, PhantomData)
    }
    fn as_manchester_with_prefixes<'t>(
        &'t self,
        prefix: &'t PrefixMapping,
    ) -> Manchester<'t, Self, A> {
        Manchester(self, Some(prefix), PhantomData)
    }
}

/// Lazy `Display` wrapper for a Manchester-rendered element.
#[derive(Debug)]
pub struct Manchester<'t, T: ?Sized, A: ForIRI>(&'t T, Option<&'t PrefixMapping>, PhantomData<A>);

impl<'t, T, A> Display for Manchester<'t, &'t T, A>
where
    Manchester<'t, T, A>: Display,
    A: ForIRI,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        Manchester(*self.0, self.1, PhantomData).fmt(f)
    }
}

/// Render an IRI: abbreviated `prefix:local` if a prefix matches, else `<iri>`.
fn write_iri(
    iri: &str,
    prefix: Option<&PrefixMapping>,
    f: &mut Formatter<'_>,
) -> Result<(), Error> {
    if let Some(pm) = prefix
        && let Ok(curie) = pm.shrink_iri(iri)
    {
        return write!(f, "{curie}");
    }
    write!(f, "<{iri}>")
}

impl<A: ForIRI> Display for Manchester<'_, IRI<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write_iri(self.0.as_ref(), self.1, f)
    }
}
impl<A: ForIRI> AsManchester<A> for IRI<A> {}

impl<A: ForIRI> Display for Manchester<'_, Class<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for Class<A> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Build;

    #[test]
    fn renders_named_class() {
        let b = Build::new_rc();
        let c = b.class("http://example.org/Dog");
        assert_eq!(c.as_manchester().to_string(), "<http://example.org/Dog>");
    }

    #[test]
    fn renders_class_with_prefix() {
        let b = Build::new_rc();
        let c = b.class("http://example.org/Dog");
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("ex", "http://example.org/").unwrap();
        assert_eq!(c.as_manchester_with_prefixes(&pm).to_string(), "ex:Dog");
    }
}
