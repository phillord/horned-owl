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

// ---------------------------------------------------------------------------

/// Write a string literal while escaping `"` and `\` characters.
fn quote(mut s: &str, f: &mut Formatter<'_>) -> Result<(), Error> {
    f.write_str("\"")?;
    while let Some((i, c)) = s.chars().enumerate().find(|(_, c)| *c == '\\' || *c == '"') {
        f.write_str(&s[..i])?;
        match c {
            '\\' => f.write_str("\\\\")?,
            '"' => f.write_str("\\\"")?,
            _ => unreachable!(),
        }
        s = &s[i + 1..];
    }
    f.write_str(s)?;
    f.write_str("\"")
}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, ObjectProperty<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for ObjectProperty<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, ObjectPropertyExpression<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use ObjectPropertyExpression::*;
        match self.0 {
            ObjectProperty(op) => Manchester(op, self.1, PhantomData::<A>).fmt(f),
            InverseObjectProperty(op) => {
                write!(f, "inverse ({})", Manchester(op, self.1, PhantomData::<A>))
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for ObjectPropertyExpression<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, DataProperty<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for DataProperty<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, NamedIndividual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for NamedIndividual<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, AnonymousIndividual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "_:{}", self.0.0.borrow())
    }
}
impl<A: ForIRI> AsManchester<A> for AnonymousIndividual<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, Individual<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use Individual::*;
        match self.0 {
            Named(i) => Manchester(i, self.1, PhantomData::<A>).fmt(f),
            Anonymous(i) => Manchester(i, self.1, PhantomData::<A>).fmt(f),
        }
    }
}
impl<A: ForIRI> AsManchester<A> for Individual<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, Datatype<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}", Manchester(&self.0.0, self.1, PhantomData::<A>))
    }
}
impl<A: ForIRI> AsManchester<A> for Datatype<A> {}

// ---------------------------------------------------------------------------

impl<A: ForIRI> Display for Manchester<'_, Literal<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self.0 {
            Literal::Simple { literal } => quote(literal, f),
            Literal::Language { literal, lang } => {
                quote(literal, f)?;
                write!(f, "@{lang}")
            }
            Literal::Datatype {
                literal,
                datatype_iri,
            } => {
                quote(literal, f)?;
                write!(
                    f,
                    "^^{}",
                    Manchester(datatype_iri, self.1, PhantomData::<A>)
                )
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for Literal<A> {}

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

    #[test]
    fn renders_object_property_and_inverse() {
        let b = Build::new_rc();
        let p = b.object_property("http://example.org/hasParent");
        assert_eq!(
            ObjectPropertyExpression::ObjectProperty(p.clone())
                .as_manchester()
                .to_string(),
            "<http://example.org/hasParent>"
        );
        assert_eq!(
            ObjectPropertyExpression::InverseObjectProperty(p)
                .as_manchester()
                .to_string(),
            "inverse (<http://example.org/hasParent>)"
        );
    }

    #[test]
    fn renders_individual_and_literals() {
        let b = Build::new_rc();
        let i = Individual::Named(b.named_individual("http://example.org/fido"));
        assert_eq!(i.as_manchester().to_string(), "<http://example.org/fido>");

        let typed = Literal::Datatype {
            literal: "5".to_string(),
            datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
        };
        assert_eq!(
            typed.as_manchester().to_string(),
            "\"5\"^^<http://www.w3.org/2001/XMLSchema#integer>"
        );

        let lang = Literal::<RcStr>::Language {
            literal: "hello".to_string(),
            lang: "en".to_string(),
        };
        assert_eq!(lang.as_manchester().to_string(), "\"hello\"@en");

        let simple = Literal::<RcStr>::Simple {
            literal: "plain".to_string(),
        };
        assert_eq!(simple.as_manchester().to_string(), "\"plain\"");
    }
}
