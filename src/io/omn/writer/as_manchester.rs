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
/// When the prefix is empty (default namespace) the CURIE Display produces `:local`;
/// we strip the leading colon so Manchester gets bare local names.
fn write_iri(
    iri: &str,
    prefix: Option<&PrefixMapping>,
    f: &mut Formatter<'_>,
) -> Result<(), Error> {
    if let Some(pm) = prefix
        && let Ok(curie) = pm.shrink_iri(iri)
    {
        let s = curie.to_string();
        // `add_prefix("", ns)` stores "" in the prefix *mapping* (not the
        // default slot), so `shrink_iri` returns a `Curie { prefix: Some(""), .. }`
        // which `Display` formats as ":local". curie 0.1.4's `reference` field is
        // private (no clean accessor), so strip the leading ':' to get the bare
        // local name.
        return if let Some(local) = s.strip_prefix(':') {
            write!(f, "{local}")
        } else {
            write!(f, "{s}")
        };
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

// ---------------------------------------------------------------------------
// DataRange — facets + operator precedence

/// Map each `Facet` variant to its W3C OWL 2 Manchester Syntax symbol.
fn facet_symbol(f: &crate::vocab::Facet) -> &'static str {
    use crate::vocab::Facet::*;
    match f {
        MinInclusive => ">=",
        MaxInclusive => "<=",
        MinExclusive => ">",
        MaxExclusive => "<",
        Length => "length",
        MinLength => "minLength",
        MaxLength => "maxLength",
        Pattern => "pattern",
        LangRange => "langRange",
        TotalDigits => "totalDigits",
        FractionDigits => "fractionDigits",
    }
}

/// Precedence for `DataRange` operators.
/// Tightest → loosest: atoms/restrictions (3) > `and` (2) > `or` (1).
fn dr_prec<A: ForIRI>(dr: &DataRange<A>) -> u8 {
    match dr {
        DataRange::DataUnionOf(_) => 1,
        DataRange::DataIntersectionOf(_) => 2,
        _ => 3,
    }
}

/// Render `inner` as an operand requiring at least `need` precedence.
/// Parenthesizes when `inner` binds looser than `need`.
fn dr_operand<A: ForIRI>(
    inner: &DataRange<A>,
    need: u8,
    pm: Option<&PrefixMapping>,
    f: &mut Formatter<'_>,
) -> Result<(), Error> {
    if dr_prec(inner) < need {
        write!(f, "({})", Manchester(inner, pm, PhantomData::<A>))
    } else {
        write!(f, "{}", Manchester(inner, pm, PhantomData::<A>))
    }
}

impl<A: ForIRI> Display for Manchester<'_, DataRange<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use DataRange::*;
        let pm = self.1;
        match self.0 {
            Datatype(dt) => Manchester(dt, pm, PhantomData::<A>).fmt(f),
            DataIntersectionOf(drs) => {
                let mut first = true;
                for dr in drs {
                    if !first {
                        write!(f, " and ")?;
                    }
                    first = false;
                    dr_operand(dr, 2, pm, f)?;
                }
                Ok(())
            }
            DataUnionOf(drs) => {
                let mut first = true;
                for dr in drs {
                    if !first {
                        write!(f, " or ")?;
                    }
                    first = false;
                    dr_operand(dr, 1, pm, f)?;
                }
                Ok(())
            }
            DataComplementOf(dr) => {
                write!(f, "not ")?;
                dr_operand(dr.as_ref(), 3, pm, f)
            }
            DataOneOf(lits) => {
                write!(f, "{{ ")?;
                let mut first = true;
                for l in lits {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    Manchester(l, pm, PhantomData::<A>).fmt(f)?;
                }
                write!(f, " }}")
            }
            DatatypeRestriction(dt, frs) => {
                Manchester(dt, pm, PhantomData::<A>).fmt(f)?;
                write!(f, "[")?;
                let mut first = true;
                for fr in frs {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    write!(f, "{} ", facet_symbol(&fr.f))?;
                    Manchester(&fr.l, pm, PhantomData::<A>).fmt(f)?;
                }
                write!(f, "]")
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for DataRange<A> {}

// ---------------------------------------------------------------------------
// ClassExpression — Manchester operator precedence
//
// Tightest → loosest: atoms / `not` / restrictions (prec 3) > `and` (2) > `or` (1).
// A sub-expression is parenthesized when its precedence is STRICTLY LOWER than
// the minimum precedence required by the context.

fn ce_prec<A: ForIRI>(ce: &ClassExpression<A>) -> u8 {
    match ce {
        ClassExpression::ObjectUnionOf(_) => 1,
        ClassExpression::ObjectIntersectionOf(_) => 2,
        _ => 3,
    }
}

/// Render `inner` as an operand under a context requiring at least `need` precedence.
/// Parenthesizes when `inner` binds looser than `need`.
fn ce_operand<A: ForIRI>(
    inner: &ClassExpression<A>,
    need: u8,
    pm: Option<&PrefixMapping>,
    f: &mut Formatter<'_>,
) -> Result<(), Error> {
    if ce_prec(inner) < need {
        write!(f, "({})", Manchester(inner, pm, PhantomData::<A>))
    } else {
        write!(f, "{}", Manchester(inner, pm, PhantomData::<A>))
    }
}

impl<A: ForIRI> Display for Manchester<'_, ClassExpression<A>, A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        use ClassExpression::*;
        let pm = self.1;
        match self.0 {
            Class(c) => Manchester(c, pm, PhantomData::<A>).fmt(f),

            ObjectIntersectionOf(operands) => {
                let mut first = true;
                for ce in operands {
                    if !first {
                        write!(f, " and ")?;
                    }
                    first = false;
                    ce_operand(ce, 2, pm, f)?;
                }
                Ok(())
            }

            ObjectUnionOf(operands) => {
                let mut first = true;
                for ce in operands {
                    if !first {
                        write!(f, " or ")?;
                    }
                    first = false;
                    ce_operand(ce, 1, pm, f)?;
                }
                Ok(())
            }

            ObjectComplementOf(bce) => {
                write!(f, "not ")?;
                ce_operand(bce.as_ref(), 3, pm, f)
            }

            ObjectOneOf(individuals) => {
                write!(f, "{{")?;
                let mut first = true;
                for i in individuals {
                    if !first {
                        write!(f, ", ")?;
                    }
                    first = false;
                    Manchester(i, pm, PhantomData::<A>).fmt(f)?;
                }
                write!(f, "}}")
            }

            ObjectSomeValuesFrom { ope, bce } => {
                write!(f, "{} some ", Manchester(ope, pm, PhantomData::<A>))?;
                ce_operand(bce.as_ref(), 3, pm, f)
            }

            ObjectAllValuesFrom { ope, bce } => {
                write!(f, "{} only ", Manchester(ope, pm, PhantomData::<A>))?;
                ce_operand(bce.as_ref(), 3, pm, f)
            }

            ObjectHasValue { ope, i } => {
                write!(
                    f,
                    "{} value {}",
                    Manchester(ope, pm, PhantomData::<A>),
                    Manchester(i, pm, PhantomData::<A>)
                )
            }

            ObjectHasSelf(ope) => {
                write!(f, "{} Self", Manchester(ope, pm, PhantomData::<A>))
            }

            ObjectMinCardinality { n, ope, bce } => {
                write!(f, "{} min {} ", Manchester(ope, pm, PhantomData::<A>), n)?;
                ce_operand(bce.as_ref(), 3, pm, f)
            }

            ObjectMaxCardinality { n, ope, bce } => {
                write!(f, "{} max {} ", Manchester(ope, pm, PhantomData::<A>), n)?;
                ce_operand(bce.as_ref(), 3, pm, f)
            }

            ObjectExactCardinality { n, ope, bce } => {
                write!(
                    f,
                    "{} exactly {} ",
                    Manchester(ope, pm, PhantomData::<A>),
                    n
                )?;
                ce_operand(bce.as_ref(), 3, pm, f)
            }

            DataSomeValuesFrom { dp, dr } => {
                write!(
                    f,
                    "{} some {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }

            DataAllValuesFrom { dp, dr } => {
                write!(
                    f,
                    "{} only {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }

            DataHasValue { dp, l } => {
                write!(
                    f,
                    "{} value {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    Manchester(l, pm, PhantomData::<A>)
                )
            }

            DataMinCardinality { n, dp, dr } => {
                write!(
                    f,
                    "{} min {} {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    n,
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }

            DataMaxCardinality { n, dp, dr } => {
                write!(
                    f,
                    "{} max {} {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    n,
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }

            DataExactCardinality { n, dp, dr } => {
                write!(
                    f,
                    "{} exactly {} {}",
                    Manchester(dp, pm, PhantomData::<A>),
                    n,
                    Manchester(dr, pm, PhantomData::<A>)
                )
            }
        }
    }
}
impl<A: ForIRI> AsManchester<A> for ClassExpression<A> {}

// ---------------------------------------------------------------------------

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

    #[test]
    fn renders_data_ranges_and_facets() {
        use crate::vocab::Facet;
        let b = Build::new_rc();
        let int = b.datatype("http://www.w3.org/2001/XMLSchema#integer");
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("xsd", "http://www.w3.org/2001/XMLSchema#")
            .unwrap();
        let m = |dr: &DataRange<_>| dr.as_manchester_with_prefixes(&pm).to_string();

        // bare datatype
        assert_eq!(m(&DataRange::Datatype(int.clone())), "xsd:integer");

        // xsd:integer[>= "0"^^xsd:integer]
        let fr = FacetRestriction {
            f: Facet::MinInclusive,
            l: Literal::Datatype {
                literal: "0".to_string(),
                datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
            },
        };
        assert_eq!(
            m(&DataRange::DatatypeRestriction(int.clone(), vec![fr])),
            "xsd:integer[>= \"0\"^^xsd:integer]"
        );

        // {1, 2} enumeration
        let one = Literal::Datatype {
            literal: "1".into(),
            datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
        };
        let two = Literal::Datatype {
            literal: "2".into(),
            datatype_iri: b.iri("http://www.w3.org/2001/XMLSchema#integer"),
        };
        assert_eq!(
            m(&DataRange::DataOneOf(vec![one, two])),
            "{ \"1\"^^xsd:integer, \"2\"^^xsd:integer }"
        );

        // DataIntersectionOf precedence: union-inside-intersection → parens
        let int_dr = DataRange::Datatype(int.clone());
        let string_dt = b.datatype("http://www.w3.org/2001/XMLSchema#string");
        let str_dr = DataRange::Datatype(string_dt);
        let union = DataRange::DataUnionOf(vec![int_dr.clone(), str_dr.clone()]);
        // union inside intersection must be parenthesized
        assert_eq!(
            m(&DataRange::DataIntersectionOf(vec![union, int_dr.clone()])),
            "(xsd:integer or xsd:string) and xsd:integer"
        );

        // DataComplementOf a union → parens
        let union2 = DataRange::DataUnionOf(vec![int_dr.clone(), str_dr.clone()]);
        assert_eq!(
            m(&DataRange::DataComplementOf(Box::new(union2))),
            "not (xsd:integer or xsd:string)"
        );
    }

    #[test]
    fn renders_class_expressions_with_precedence() {
        let b = Build::new_rc();
        let a = ClassExpression::Class(b.class("http://t/A"));
        let c = ClassExpression::Class(b.class("http://t/C"));
        let d = ClassExpression::Class(b.class("http://t/D"));
        let mut pm = curie::PrefixMapping::default();
        pm.add_prefix("", "http://t/").unwrap(); // default prefix → bare local names
        let m = |ce: &ClassExpression<_>| ce.as_manchester_with_prefixes(&pm).to_string();

        assert_eq!(
            m(&ClassExpression::ObjectIntersectionOf(vec![
                a.clone(),
                c.clone()
            ])),
            "A and C"
        );
        assert_eq!(
            m(&ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()])),
            "A or C"
        );
        assert_eq!(
            m(&ClassExpression::ObjectComplementOf(Box::new(a.clone()))),
            "not A"
        );

        // and binds tighter than or → no parens on the and-operand inside or
        let cd = ClassExpression::ObjectIntersectionOf(vec![c.clone(), d.clone()]);
        assert_eq!(
            m(&ClassExpression::ObjectUnionOf(vec![a.clone(), cd])),
            "A or C and D"
        );
        // or under and → MUST parenthesize
        let ac = ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]);
        assert_eq!(
            m(&ClassExpression::ObjectIntersectionOf(vec![ac, d.clone()])),
            "(A or C) and D"
        );
        // not over an `or` → parenthesized
        let aorc = ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]);
        assert_eq!(
            m(&ClassExpression::ObjectComplementOf(Box::new(aorc))),
            "not (A or C)"
        );

        let r = ObjectPropertyExpression::ObjectProperty(b.object_property("http://t/r"));
        assert_eq!(
            m(&ClassExpression::ObjectSomeValuesFrom {
                ope: r.clone(),
                bce: Box::new(a.clone())
            }),
            "r some A"
        );
        assert_eq!(
            m(&ClassExpression::ObjectAllValuesFrom {
                ope: r.clone(),
                bce: Box::new(a.clone())
            }),
            "r only A"
        );
        assert_eq!(
            m(&ClassExpression::ObjectMinCardinality {
                n: 2,
                ope: r.clone(),
                bce: Box::new(a.clone())
            }),
            "r min 2 A"
        );
        // filler that's an `or` under a restriction → parens
        let aorc2 = ClassExpression::ObjectUnionOf(vec![a.clone(), c.clone()]);
        assert_eq!(
            m(&ClassExpression::ObjectSomeValuesFrom {
                ope: r,
                bce: Box::new(aorc2)
            }),
            "r some (A or C)"
        );
    }
}
