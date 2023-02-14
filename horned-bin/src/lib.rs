//! Support for Horned command line programmes

use horned_owl::{
    error::HornedError,
    io::{ParserOutput, ResourceType, ParserConfiguration},
    model::{Build, IRI, RcAnnotatedComponent, RcStr},
    ontology::axiom_mapped::RcComponentMappedOntology,
    resolve::{localize_iri, strict_resolve_iri},
};

use std::{
    fs::File,
    io::{BufReader, Write},
    path::Path,
};

pub fn path_type(path: &Path) -> Option<ResourceType> {
    match path.extension().and_then(|s| s.to_str()) {
        Some("owx") => Some(ResourceType::OWX),
        Some("owl") => Some(ResourceType::RDF),
        _ => None,
    }
}

pub fn parse_path(
    path: &Path,
    config: ParserConfiguration
) -> Result<ParserOutput<RcStr, RcAnnotatedComponent>, HornedError> {
    Ok(match path_type(path) {
        Some(ResourceType::OWX) => {
            let file = File::open(&path)?;
            let mut bufreader = BufReader::new(file);
            horned_owl::io::owx::reader::read(&mut bufreader, config)?.into()
        }
        Some(ResourceType::RDF) => {
            let b = Build::new();
            let iri = horned_owl::resolve::path_to_file_iri(&b, path);
            horned_owl::io::rdf::closure_reader::read(&iri, config)?.into()
        }
        None => {
            return Err(HornedError::CommandError(format!(
                "Cannot parse a file of this format: {:?}",
                path
            )));
        }
    })
}

/// Parse but only as far as the imports, if that makes sense.
pub fn parse_imports(
    path: &Path,
    config: ParserConfiguration
) -> Result<ParserOutput<RcStr, RcAnnotatedComponent>, HornedError> {
    let file = File::open(&path)?;
    let mut bufreader = BufReader::new(file);
    Ok(match path_type(path) {
        Some(ResourceType::OWX) => horned_owl::io::owx::reader::read(&mut bufreader, config)?.into(),
        Some(ResourceType::RDF) => {
            let b = Build::new();
            let mut p = horned_owl::io::rdf::reader::parser_with_build(&mut bufreader, &b, config);
            p.parse_imports()?;
            p.as_ontology_and_incomplete()?.into()
        }
        None => {
            return Err(HornedError::CommandError(format!(
                "Cannot parse a file of this format: {:?}",
                path
            )))
        }
    })
}

pub fn materialize(input: &str, config: ParserConfiguration) -> Result<Vec<IRI<RcStr>>, HornedError> {
    let mut v = vec![];
    materialize_1(input, config, &mut v, true)?;
    Ok(v)
}

pub fn materialize_1<'a>(
    input: &str,
    config: ParserConfiguration,
    done: &'a mut Vec<IRI<RcStr>>,
    recurse: bool,
) -> Result<&'a mut Vec<IRI<RcStr>>, HornedError> {
    println!("Parsing: {}", input);
    let amont: RcComponentMappedOntology = parse_imports(Path::new(input), config)?.into();
    let import = amont.i().import();

    let b = Build::new_rc();

    // Get all the imports
    for i in import {
        if !done.contains(&i.0) {
            let local: String = localize_iri(&i.0, &b.iri(input)).into();
            let local_path = Path::new(&local);
            if !local_path.exists() {
                println!("Retrieving Ontology: {}", &i.0);
                let imported_data = strict_resolve_iri(&i.0);
                done.push(i.0.clone());
                println!("Saving to {}", local);
                let mut file = File::create(&local)?;
                file.write_all(imported_data.as_bytes())?;
            } else {
                println!("Already Present: {}", local);
            }
            if recurse {
                materialize_1(&local, config, done, true)?;
            }
        } else {
            println!("Already materialized: {}", &i.0);
        }
    }

    Ok(done)
}

pub mod naming {
    use horned_owl::model::ComponentKind;
    use horned_owl::model::ComponentKind::*;

    pub fn name(axk: &ComponentKind) -> &'static str {
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

    use horned_owl::{model::ComponentKind, ontology::axiom_mapped::RcComponentMappedOntology};
    use indexmap::map::IndexMap;

    #[derive(Debug)]
    pub struct SummaryStatistics {
        pub logical_axiom: usize,
        pub annotation_axiom: usize,
        pub axiom_type: IndexMap<ComponentKind, usize>,
    }

    impl SummaryStatistics {
        pub fn with_axiom_types(&self) -> impl Iterator<Item = (&ComponentKind, &usize)> + '_ {
            self.axiom_type.iter().filter(|&(_, v)| v > &0)
        }
    }

    pub fn summarize<O: Into<RcComponentMappedOntology>>(ont: O) -> SummaryStatistics
    where
        O:,
    {
        let ont: RcComponentMappedOntology = ont.into();
        SummaryStatistics {
            logical_axiom: ont.i().iter().count(),
            annotation_axiom: ont
                .i()
                .iter()
                .map(|aa| aa.ann.len())
                .sum::<usize>(),
            axiom_type: axiom_types(ont),
        }
    }

    fn axiom_types<O: Into<RcComponentMappedOntology>>(ont: O) -> IndexMap<ComponentKind, usize> {
        let ont: RcComponentMappedOntology = ont.into();
        let mut im = IndexMap::new();
        for ax in ComponentKind::all_kinds() {
            im.insert(ax, ont.i().axiom(ax).count());
        }

        im
    }
}

pub mod config {
    use clap::App;
    use clap::ArgAction;
    use clap::ArgMatches;
    use horned_owl::io::ParserConfiguration;
    use horned_owl::io::RDFParserConfiguration;

    pub fn parser_app(app: App<'static>) -> App<'static> {
        app.arg(
            clap::arg!(--"strict")
                .required(false)
                .action(ArgAction::SetTrue)
                .help("Parse RDF strictly")
        )
    }

    pub fn parser_config(matches: &ArgMatches) -> ParserConfiguration {
        ParserConfiguration{
            rdf: RDFParserConfiguration {
                lax: !matches.get_one::<bool>("strict").unwrap_or(&false)
            },
            ..Default::default()
        }
    }
}
