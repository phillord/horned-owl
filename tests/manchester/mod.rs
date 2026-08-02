//! Shared helpers for the Manchester conformance harness.
use std::io::BufReader;
use std::rc::Rc;

use curie::PrefixMapping;
use horned_owl::io::ParserConfiguration;
use horned_owl::io::omn::{read as read_omn, write as write_omn};
use horned_owl::model::AnnotatedComponent;
use horned_owl::ontology::component_mapped::ComponentMappedOntology;
use horned_owl::ontology::set::SetOntology;

pub mod adversarial;
pub mod canonical;
pub mod constructs;
pub mod corpus;
pub mod report;

pub type O = SetOntology<Rc<str>>;

/// Parse a Manchester document string into a SetOntology + prefixes.
pub fn read_str(s: &str) -> Result<(O, PrefixMapping), String> {
    read_omn::<Rc<str>, O, _>(BufReader::new(s.as_bytes()), ParserConfiguration::default())
        .map_err(|e| format!("{e}"))
}

/// Render a SetOntology back to Manchester text.
pub fn write_str(ont: &O, pm: &PrefixMapping) -> String {
    let amo: ComponentMappedOntology<Rc<str>, Rc<AnnotatedComponent<Rc<str>>>> = ont.clone().into();
    let buf = write_omn(Vec::<u8>::new(), &amo, Some(pm)).expect("omn write");
    String::from_utf8(buf).expect("utf8")
}

/// Sorted multiset of components, for order-insensitive structural comparison.
pub fn components_sorted(ont: &O) -> Vec<String> {
    let mut v: Vec<String> = ont.iter().map(|ac| format!("{:?}", ac.component)).collect();
    v.sort();
    v
}
