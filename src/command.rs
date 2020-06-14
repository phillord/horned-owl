use crate::model::Ontology;
use curie::PrefixMapping;

use failure::Error;

use std::{path::Path, fs::File, io::BufReader};

pub fn parse_path(path: &Path) -> Result<(Ontology, PrefixMapping), Error> {
    let file = File::open(&path)?;
    let mut bufreader = BufReader::new(file);


    Ok(
        match path.extension().map(|s| s.to_str()).flatten() {
            Some("oxl") => super::io::owx::reader::read(&mut bufreader)?,
            Some("owl") => super::io::rdf::reader::read(&mut bufreader)?,
            _ => todo!()
        }
    )
}

pub mod naming {
    use crate::model::AxiomKind;
    use crate::model::AxiomKind::*;

    pub fn name(axk: &AxiomKind) -> &'static str {
        match axk {
            OntologyAnnotation => "Ontology Annotation",
            Import => "Import",
            DeclareClass => "Declare Class",
            DeclareObjectProperty => "Declare Object Property",
            DeclareAnnotationProperty => "Declare Annotation Property",
            DeclareDataProperty => "Declare Data Property",
            DeclareNamedIndividual => "Declare Named Individual",
            DeclareDatatype => "Declare Datatype",
            SubClassOf => "Sub-Class Of",
            EquivalentClasses => "Equivalent Classes",
            DisjointClasses => "Disjoint Classes",
            DisjointUnion => "Disjoint Union",
            SubObjectPropertyOf => "Sub Object Property Of",
            EquivalentObjectProperties => "Equivalent Object Properties",
            DisjointObjectProperties => "Disjoint Object Properties",
            InverseObjectProperties => "Inverse Object Properties",
            ObjectPropertyDomain => "Object Property Domain",
            ObjectPropertyRange => "Object Property Range",
            FunctionalObjectProperty => "Functional Object Property",
            InverseFunctionalObjectProperty => "Inverse Functional Object Property",
            ReflexiveObjectProperty => "Reflexive Object Property",
            IrreflexiveObjectProperty => "Irreflexive Object Property",
            SymmetricObjectProperty => "Symmetric Object Property",
            AsymmetricObjectProperty => "Assymmetric Object Property",
            TransitiveObjectProperty => "Transitive Object Property",
            SubDataPropertyOf => "Sub Data Property Of",
            EquivalentDataProperties => "Equivalent Data Properties",
            DisjointDataProperties => "Disjoint Data Properties",
            DataPropertyDomain => "Data Property Domain",
            DataPropertyRange => "Data Property Range",
            FunctionalDataProperty => "Functional Data Property",
            DatatypeDefinition => "Datatype Definition",
            HasKey => "Has Key",
            SameIndividual => "Same Individual",
            DifferentIndividuals => "Different Individuals",
            ClassAssertion => "Class Assertion",
            ObjectPropertyAssertion => "Object Property Assertion",
            NegativeObjectPropertyAssertion => "Negative Object Property Assertion",
            DataPropertyAssertion => "Data Property Assertion",
            NegativeDataPropertyAssertion => "Negative Data Property Assertion",
            AnnotationAssertion => "Annotation Assertion",
            SubAnnotationPropertyOf => "Sub Annotation Property Of",
            AnnotationPropertyDomain => "Annotation Property Domain",
            AnnotationPropertyRange => "Annotation Property Range",
        }
    }
}

pub mod summary {

    use indexmap::map::IndexMap;
    use crate::model::AxiomKind;
    use crate::model::Ontology;

    #[derive(Debug)]
    pub struct SummaryStatistics {
        pub logical_axiom: usize,
        pub annotation_axiom: usize,
        pub axiom_type: IndexMap<AxiomKind, usize>,
    }

    impl SummaryStatistics {
        pub fn with_axiom_types(&self) -> impl Iterator<Item=(&AxiomKind,&usize)> + '_ {
            self.axiom_type.iter().filter(|&(_, v)| v > &0 )
        }
    }

    pub fn summarize(ont: &Ontology) -> SummaryStatistics {
        SummaryStatistics {
            logical_axiom: ont.iter().count(),
            annotation_axiom: ont.iter().map(|aa| aa.ann.iter().count()).sum::<usize>(),
            axiom_type: axiom_types(ont),
        }
    }

    fn axiom_types(ont: &Ontology) -> IndexMap<AxiomKind, usize> {
        let mut im = IndexMap::new();
        for ax in AxiomKind::all_kinds() {
            im.insert(ax, ont.axiom(ax).count());
        }

        im
    }

}
