use std::io::Write;

use curie::PrefixMapping;

use crate::error::HornedError;
use crate::model::Component;
use crate::model::ComponentKind;
use crate::model::ForIRI;
use crate::ontology::component_mapped::ComponentMappedOntology;
use crate::ontology::indexed::ForIndex;

mod as_functional;

pub use self::as_functional::AsFunctional;
pub use self::as_functional::Functional;

/// Write an Ontology to `write`, using the given `PrefixMapping`.
///
/// The ontology is written in OWL
/// [Functional-Style](https://www.w3.org/TR/2012/REC-owl2-syntax-20121211/)
/// syntax.
pub fn write<A: ForIRI, AA: ForIndex<A>, W: Write>(
    mut write: W,
    ont: &ComponentMappedOntology<A, AA>,
    mapping: Option<&PrefixMapping>,
) -> Result<(), HornedError> {
    // Ensure we have a prefix mapping; the default is a no-op and
    // it's easier than checking every time.
    let default_mapper = PrefixMapping::default();
    let mapping = match mapping {
        Some(m) => m,
        None => &default_mapper,
    };

    // Ensure we have a single OntologyID in the ontology.
    let optional_id = {
        let mut components = ont.i().component_for_kind(ComponentKind::OntologyID);
        let component = components.next();
        if components.next().is_some() {
            return Err(HornedError::invalid("multiple ontology IDs found"));
        }
        component.map(|c| {
            if let Component::OntologyID(ontology_id) = &c.component {
                ontology_id
            } else {
                unreachable!()
            }
        })
    };

    // Write prefixes
    write!(
        &mut write,
        "{}",
        <PrefixMapping as AsFunctional<A>>::as_functional(mapping)
    )?;

    // Start the ontology element
    write!(write, "Ontology(")?;

    // Write the IRI and Version IRI if any
    if let Some(ontology_id) = optional_id {
        if let Some(iri) = &ontology_id.iri {
            write!(write, "{}", iri.as_functional_with_prefixes(mapping))?;
            if let Some(viri) = &ontology_id.viri {
                writeln!(write, " {}", viri.as_functional_with_prefixes(mapping))?;
            } else {
                writeln!(write)?;
            }
        }
    }

    // Write axioms in order
    for kind in ComponentKind::all_kinds() {
        if kind != ComponentKind::OntologyID && kind != ComponentKind::DocIRI {
            let mut components = ont.i().component_for_kind(kind).collect::<Vec<_>>();
            components.sort();
            for component in components {
                writeln!(
                    &mut write,
                    "    {}",
                    component.as_functional_with_prefixes(mapping)
                )?;
            }
        }
    }

    // Close the ontology
    writeln!(write, ")").map_err(From::from)
}

#[cfg(test)]
mod test {
    use super::*;

    use crate::model::AnnotatedComponent;
    use crate::model::RcStr;

    use pretty_assertions::assert_eq;
    use test_generator::test_resources;

    #[test_resources("src/ont/owl-functional/*.ofn")]
    fn roundtrip_resource(resource: &str) {
        let reader = std::fs::File::open(&resource)
            .map(std::io::BufReader::new)
            .unwrap();
        let (ont, prefixes): (ComponentMappedOntology<RcStr, AnnotatedComponent<RcStr>>, _) =
            crate::io::ofn::reader::read(reader, Default::default()).unwrap();

        let mut writer = Vec::new();
        crate::io::ofn::writer::write(&mut writer, &ont, Some(&prefixes)).unwrap();

        let (ont2, prefixes2) =
            crate::io::ofn::reader::read(std::io::Cursor::new(&writer), Default::default())
                .unwrap();

        assert_eq!(prefixes, prefixes2, "prefix mapping differ");
        assert_eq!(ont, ont2, "ontologies differ");
    }
}
