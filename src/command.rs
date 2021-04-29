//! Support for Horned command line programmes

use crate::{io::{ParserOutput, ResourceType}, model::{Build, IRI}, ontology::{axiom_mapped::AxiomMappedOntology}, resolve::{localize_iri, strict_resolve_iri}};

use failure::Error;

use std::{fs::File, io::{BufReader, Write}, path::Path};

pub fn path_type(path: &Path) -> Option<ResourceType> {
   match path.extension().map(|s| s.to_str()).flatten() {
       Some("owx") => Some(ResourceType::OWX),
       Some("owl") => Some(ResourceType::RDF),
       _ => None,
   }
}

pub fn parse_path(path: &Path) -> Result<ParserOutput, Error>
{
    let file = File::open(&path)?;
    let mut bufreader = BufReader::new(file);

    Ok(match path_type(path) {
        Some(ResourceType::OWX) => super::io::owx::reader::read(&mut bufreader)?.into(),
        Some(ResourceType::RDF) => super::io::rdf::reader::read(&mut bufreader)?.into(),
        _ => {
            eprintln!("Do not know how to parse file with path: {:?}", path);
            todo!()
        },
    })
}

/// Parse but only as far as the imports, if that makes sense.
pub fn parse_imports(path: &Path) -> Result<ParserOutput, Error> {
    let file = File::open(&path)?;
    let mut bufreader = BufReader::new(file);
    Ok(match path_type(path) {
        Some(ResourceType::OWX) => super::io::owx::reader::read(&mut bufreader)?.into(),
        Some(ResourceType::RDF) => {
            let b = Build::new();
            let mut p = crate::io::rdf::reader::parser_with_build(&mut bufreader,
                                                              &b);
            p.parse_imports()?;
            p.as_ontology_and_incomplete()?.into()
        }
        _ => {
            eprintln!("Do not know how to parse file with path: {:?}", path);
            todo!()
        }
    })
}

pub fn materialize(input: &str) -> Result<Vec<IRI>,Error> {
    let mut v = vec![];
    materialize_1(input, &mut v, true)?;
    Ok(v)
}

pub fn materialize_1<'a>(input: &str, done: &'a mut Vec<IRI>, recurse: bool)
                         -> Result<&'a mut Vec<IRI>,Error> {
    println!("Parsing: {}", input);
    let amont:AxiomMappedOntology = parse_imports(Path::new(input))?.into();
    let import = amont.i().import();

    let b = Build::new();

    // Get all the imports
    for i in import {
        if !done.contains(&i.0) {

            let local:String = localize_iri(&i.0, &b.iri(input)).into();
            let local_path = Path::new(&local);
            if !local_path.exists() {
                println!("Retrieving Ontology: {}", &i.0);
                let imported_data = strict_resolve_iri(&i.0);
                done.push(i.0.clone());
                println!("Saving to {}", local);
                let mut file = File::create(&local)?;
                file.write_all(&imported_data.as_bytes())?;
            }
            else {
                println!("Already Present: {}", local);
            }
            if recurse {
                materialize_1(&local, done, true)?;
            }
        }
        else {
            println!("Already materialized: {}", &i.0);
        }
    }

    Ok(done)
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

    use crate::{model::AxiomKind, ontology::axiom_mapped::AxiomMappedOntology};
    use indexmap::map::IndexMap;

    #[derive(Debug)]
    pub struct SummaryStatistics {
        pub logical_axiom: usize,
        pub annotation_axiom: usize,
        pub axiom_type: IndexMap<AxiomKind, usize>,
    }

    impl SummaryStatistics {
        pub fn with_axiom_types(&self) -> impl Iterator<Item = (&AxiomKind, &usize)> + '_ {
            self.axiom_type.iter().filter(|&(_, v)| v > &0)
        }
    }

    pub fn summarize<O: Into<AxiomMappedOntology>>(ont: O) -> SummaryStatistics
    where
        O: ,
    {
        let ont: AxiomMappedOntology = ont.into();
        SummaryStatistics {
            logical_axiom: ont.i().iter().count(),
            annotation_axiom: ont
                .i()
                .iter()
                .map(|aa| aa.ann.iter().count())
                .sum::<usize>(),
            axiom_type: axiom_types(ont),
        }
    }

    fn axiom_types<O: Into<AxiomMappedOntology>>(ont: O) -> IndexMap<AxiomKind, usize> {
        let ont: AxiomMappedOntology = ont.into();
        let mut im = IndexMap::new();
        for ax in AxiomKind::all_kinds() {
            im.insert(ax, ont.i().axiom(ax).count());
        }

        im
    }
}


