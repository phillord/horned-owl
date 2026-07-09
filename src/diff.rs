use horned_owl::model::{AnnotatedComponent, Component, RcStr};
use horned_owl::ontology::set::SetOntology;
use std::collections::BTreeSet;

pub struct RawDiff {
    pub only_in_source: Vec<AnnotatedComponent<RcStr>>,
    pub only_in_roundtrip: Vec<AnnotatedComponent<RcStr>>,
}

pub fn diff(src: &SetOntology<RcStr>, rt: &SetOntology<RcStr>) -> RawDiff {
    let s: BTreeSet<AnnotatedComponent<RcStr>> = src.iter().cloned().collect();
    let r: BTreeSet<AnnotatedComponent<RcStr>> = rt.iter().cloned().collect();
    RawDiff {
        only_in_source: s.difference(&r).cloned().collect(),
        only_in_roundtrip: r.difference(&s).cloned().collect(),
    }
}

pub fn kind_of(c: &AnnotatedComponent<RcStr>) -> String {
    // Component variant name, e.g. "DeclareClass", "SubClassOf".
    match &c.component {
        Component::DeclareClass(_) => "DeclareClass",
        Component::DeclareObjectProperty(_) => "DeclareObjectProperty",
        Component::DeclareDataProperty(_) => "DeclareDataProperty",
        Component::DeclareAnnotationProperty(_) => "DeclareAnnotationProperty",
        Component::DeclareNamedIndividual(_) => "DeclareNamedIndividual",
        Component::DeclareDatatype(_) => "DeclareDatatype",
        _ => "Other",
    }
    .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Format;
    use crate::ontology::read_source;
    #[test]
    fn reports_lost_and_gained() {
        let a = read_source(Format::Ofn, b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\nDeclaration(Class(<http://ex/B>))\n)").unwrap().model;
        let b = read_source(
            Format::Ofn,
            b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>\nDeclaration(Class(<http://ex/A>))\n)",
        )
        .unwrap()
        .model;
        let d = diff(&a, &b);
        assert_eq!(d.only_in_source.len(), 1); // B lost
        assert_eq!(d.only_in_roundtrip.len(), 0);
    }
}
