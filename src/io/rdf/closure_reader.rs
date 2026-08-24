use crate::error::HornedError;
use crate::io::IncompleteParse;
#[cfg(test)]
use crate::io::ParserConfiguration;
use crate::io::RDFParserConfiguration;
use crate::io::rdf::reader::OntologyParser;
use crate::io::rdf::reader::RDFOntology;
use crate::io::rdf::reader::parser_with_build;
use crate::model::Build;
use crate::model::DocIRI;
use crate::model::ForIRI;
use crate::model::IRI;
use crate::ontology::indexed::ForIndex;
use crate::ontology::set::SetIndex;
use crate::resolve::path_to_file_iri;
use crate::resolve::resolve_iri;

use std::collections::HashMap;
use std::path::PathBuf;

pub struct ClosureOntologyParser<
    A: ForIRI,
    AA: ForIndex<A>,
    O: RDFOntology<A, AA>,
    B: AsRef<Build<A>> = Build<A>,
> {
    // A map between the resolvable IRI of an Ontology and an OntologyParser
    op: HashMap<IRI<A>, OntologyParser<A, AA, O, B>>,
    // A map between the resolvable IRI of an Ontology and the
    // resolvable IRIs of any Ontology that it imports.
    import_map: HashMap<IRI<A>, Vec<IRI<A>>>,
    // A map between an Ontology's plain IRI and the key it is
    // actually stored under in `op`/`import_map` (its version IRI),
    // for Ontologies that have both. An `owl:imports` statement may
    // legally reference either the plain IRI or the version IRI of
    // the Ontology it imports, so we need to be able to resolve
    // either back to the same entry.
    alias: HashMap<IRI<A>, IRI<A>>,
    config: RDFParserConfiguration<A, B>,
}

impl<A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>, B: AsRef<Build<A>> + Clone>
    ClosureOntologyParser<A, AA, O, B>
{
    pub fn new(config: RDFParserConfiguration<A, B>) -> Self {
        ClosureOntologyParser {
            import_map: HashMap::new(),
            op: HashMap::new(),
            alias: HashMap::new(),
            config,
        }
    }

    pub fn parse_path(&mut self, pb: &PathBuf) -> Result<Vec<IRI<A>>, HornedError> {
        let file_iri = path_to_file_iri(self.config.common.build.as_ref(), pb);
        let s = ::std::fs::read_to_string(pb)?;

        // We use the IRI that we try to parse, but we don't know that
        // this is the same as file says at this point.
        self.parse_content_from_iri(s, None, file_iri)
    }

    /// Parse content from some IRI.
    ///
    /// Content will be taken by using
    /// [resolve_iri], meaning that it
    /// will be loaded from a local resource if possible.
    ///
    /// # Arguments
    ///
    /// * `source_iri` -- the source IRI from which we should
    ///   parse. This may be the declared IRI of the ontology, or a
    ///   local document IRI. This IRI may not be used directly as the
    ///   source depending on the `relative_doc_iri`.
    /// * `relative_doc_iri` -- an IRI that `source_iri` should be
    ///   interpreted as relative to, if any.
    ///
    /// # Returns
    ///
    /// A `Vec` of the declared IRIs from the parsed Ontology and its
    /// import closure, or an error.
    pub fn parse_iri(
        &mut self,
        source_iri: &IRI<A>,
        relative_doc_iri: Option<&IRI<A>>,
    ) -> Result<Vec<IRI<A>>, HornedError> {
        let (new_doc_iri, s) = resolve_iri(
            source_iri,
            relative_doc_iri,
            self.config.common.remote_body_limit,
            self.config.common.local_only,
            self.config.common.catalog.as_deref(),
        )?;
        self.parse_content_from_iri(s, relative_doc_iri, new_doc_iri)
    }

    /// Parse content from some IRI
    ///
    /// This assumes that we already know the full content (that is we
    /// do not have to resolve the IRI to content). In addition to the
    /// content, we also need to supply the IRIs from which the
    /// ontology claims to be, and from where it actually was.
    ///
    /// # Arguments
    ///
    /// * `s` -- A string of the ontology to be parsed
    /// * `relative_doc_iri` -- The document IRI which was used to
    ///   determine the relative location of `s` if any.
    /// * `new_doc_iri` -- the IRI that `s` was actually read from
    ///
    /// # Returns
    ///
    /// A `Vec` of the declared IRIs from the parsed Ontology and its
    /// import closure, or an error.
    fn parse_content_from_iri(
        &mut self,
        s: String,
        relative_doc_iri: Option<&IRI<A>>,
        new_doc_iri: IRI<A>,
    ) -> Result<Vec<IRI<A>>, HornedError> {
        // Parse the contents of the string
        let mut p = parser_with_build(&mut s.as_bytes(), self.config.clone())?;
        let imports = p.parse_imports().unwrap();
        p.parse_declarations()?;

        // push the DocIRI onto the partially parsed ontology
        let o: &mut O = p.mut_ontology_ref();
        o.insert(DocIRI(new_doc_iri.clone()));

        // Find the viri_or_iri
        let si: &SetIndex<A, AA> = o.as_ref();
        let id = si.the_ontology_id_or_default();

        // Use the declared ontology IRI (or version IRI) as the storage key;
        // fall back to the document IRI for anonymous ontologies so they are
        // still stored and can be returned by `as_ontology_vec_and_incomplete`.
        let storage_iri = id
            .clone()
            .viri_or_iri()
            .unwrap_or_else(|| new_doc_iri.clone());

        // Stuff the iri of this ontology, if we have one into a vec
        let mut res = match id.clone().viri_or_iri() {
            Some(resolved_iri) => vec![resolved_iri],
            _ => vec![],
        };

        // Add the ontology that we have parsed into import_map. An
        // `owl:imports` statement may reference either the plain IRI
        // or the version IRI of an Ontology, so if both are present
        // and differ, record the plain IRI as an alias of the
        // version IRI so that either can be used to find this entry.
        if let (Some(iri), Some(viri)) = (id.iri, id.viri)
            && iri != viri
        {
            self.alias.insert(iri, viri);
        }
        self.import_map.insert(storage_iri.clone(), imports.clone());
        self.op.insert(storage_iri, p);

        // Now parse all of the imported ontologies as well
        for import in imports {
            let recursive_imports =
                self.parse_iri(&import, relative_doc_iri.or(Some(&new_doc_iri)))?;
            res.extend(recursive_imports);
        }

        Ok(res)
    }

    // Finish the parse for the Ontology with the declared IRI.
    //
    // Returns a Result with HornedError in case of failure to parse,
    // or a boolean indicating whether the `IRI` is in the import closure.
    pub fn finish_parse(&mut self, iri: &IRI<A>) -> Result<bool, HornedError> {
        let op_pointer: *mut HashMap<_, _> = &mut self.op;

        // From the import map, we can extract the IRIs for the import
        // closure for the Ontology that we wish to complete the parse
        // of.
        let import_iris = self.import_map.get(iri).unwrap();

        // Now we can get references to the actual ontologies. An
        // import may reference an Ontology by its plain IRI even
        // though it is stored under its version IRI, so fall back to
        // the alias map if a direct lookup fails.
        let import_closure: Result<Vec<_>, HornedError> = import_iris
            .iter()
            .map(|i| {
                self.op
                    .get(i)
                    .or_else(|| {
                        self.alias
                            .get(i)
                            .and_then(|canonical| self.op.get(canonical))
                    })
                    .ok_or_else(|| HornedError::ImportError(i.to_string()))
                    .map(|i| i.ontology_ref())
            })
            .collect();

        // If we cannot find one of the ontologies in the import
        // closure, fail here
        let import_closure = import_closure?;

        // The import closure references ontologies in the op
        // HashMap. We need to modify one of the ontologies in the map
        // while retaining a reference to the others. Hence the unsafe.
        unsafe {
            if let Some(o) = (*op_pointer).get_mut(iri) {
                o.finish_parse(&import_closure)?;
            } else {
                return Ok(false);
            }
        }

        Ok(true)
    }

    // Return ontology in potentially incompletely parsed state
    pub fn as_ontology_vec(self) -> Vec<O> {
        self.as_ontology_vec_and_incomplete()
            .into_iter()
            .map(|(o, _)| o)
            .collect()
    }

    // Return ontology in potentially incompletely parsed state
    pub fn as_ontology_vec_and_incomplete(self) -> Vec<(O, IncompleteParse<A>)> {
        self.op
            .into_values()
            .map(|op| op.as_ontology_and_incomplete())
            .collect()
    }
}

// Returns the an Ontology and an IncompleteParse report found at a
// given IRI or an Error
#[allow(clippy::type_complexity)]
pub fn read<A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>, B: AsRef<Build<A>> + Clone>(
    iri: &IRI<A>,
    config: RDFParserConfiguration<A, B>,
) -> Result<(O, IncompleteParse<A>), HornedError> {
    // Do parse, then full parse of first, drop the rest
    let mut c = ClosureOntologyParser::new(config);
    c.parse_iri(iri, None)?;

    let keys: Vec<_> = c.op.keys().cloned().collect();
    for i in keys {
        c.finish_parse(&i)?;
    }

    let res = c.as_ontology_vec_and_incomplete();
    res.into_iter().next().ok_or_else(|| {
        HornedError::ValidityError(
            "RDF document contains no named owl:Ontology".to_string(),
            crate::error::Location::Unknown,
        )
    })
}

// Returns the import closure of an Ontology and IncompleteParse
// report found at a given IRI or an error
#[allow(clippy::type_complexity)]
pub fn read_to_closure<
    A: ForIRI,
    AA: ForIndex<A>,
    O: RDFOntology<A, AA>,
    B: AsRef<Build<A>> + Clone,
>(
    iri: &IRI<A>,
    config: RDFParserConfiguration<A, B>,
) -> Result<Vec<(O, IncompleteParse<A>)>, HornedError> {
    // Do parse, then full parse, then result the results
    let mut c = ClosureOntologyParser::new(config);
    c.parse_iri(iri, None)?;
    let keys: Vec<_> = c.op.keys().cloned().collect();
    for i in keys {
        c.finish_parse(&i)?;
    }

    Ok(c.as_ontology_vec_and_incomplete())
}

#[cfg(test)]
mod test {
    use crate::io::rdf::closure_reader::*;
    use crate::io::rdf::reader::ConcreteRcRDFOntology;
    use crate::ontology::set::SetOntology;
    use std::path::Path;

    #[test]
    fn test_read_single() {
        let path = Path::new("src/ont/owl-rdf/withimport/import-property.owl");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, path);

        let (_, ic): (ConcreteRcRDFOntology, _) =
            read(&iri, ParserConfiguration::new(&b).into()).unwrap();
        assert!(ic.is_complete());
    }

    // import-property.owl should parse completely with full parse so
    // is a good test.
    #[test]
    fn test_read_to_closure() {
        let path = Path::new("src/ont/owl-rdf/withimport/import-property.owl");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, path);

        let v: Vec<(ConcreteRcRDFOntology, _)> =
            read_to_closure(&iri, ParserConfiguration::new(&b).into()).unwrap();
        let v: Vec<SetOntology<_>> = v
            .into_iter()
            .map(|(rdfo, ic)| {
                assert!(ic.is_complete());
                rdfo.into()
            })
            .collect();

        assert_eq!(v.len(), 2);
    }

    // `withcatalog/` reuses the exact same bubo/OWL-API-generated content
    // as `withimport/import-property.owl` + `other-property.owl` (bubo is
    // just a Clojure front end to the OWL API -- these files are real OWL
    // API output, not hand-written), but with the imported file moved into
    // an `imports/` subdirectory. `localize_iri`'s heuristic never looks
    // in subdirectories, so this setup is a genuine, real-shape case
    // (an import IRI whose physical file has moved relative to where a
    // naive same-directory guess would look) -- not a synthetic one.
    //
    // `withcatalog/catalog-v001.xml` is itself real OWL API output too,
    // not hand-typed: generated by calling the actual
    // `OWLZipSaver.catalogIndex()` method (the same code Protege's own
    // catalog files are generated by, or something sharing its template
    // -- the `<group id="Folder Repository, ...">` wrapper matches real
    // Protege-generated catalogs found on disk in ~/src/knowledge/
    // ontology-clj byte for byte) via Java interop from a bubo script --
    // see `src/ont/bubo/withcatalog/generate-catalog.clj`. Only the
    // entry's *path* (`imports/other-property.owl`) is supplied by that
    // script, via `OWLZipSaver`'s own `setEntryPath` customisation hook
    // (its default just returns the ontology IRI verbatim, confirmed by
    // reading `OWLZipSaver.java` -- not meant for filesystem redirects
    // out of the box); the XML structure/escaping is 100% real
    // `catalogIndex()` output.
    #[test]
    fn test_read_to_closure_relocated_import_fails_without_catalog() {
        let path = Path::new("src/ont/owl-rdf/withcatalog/import-property.owl");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, path);

        // local_only means no network fallback can silently paper over
        // the heuristic's failure to find the relocated file.
        let config = RDFParserConfiguration {
            common: ParserConfiguration {
                local_only: true,
                ..ParserConfiguration::new(&b)
            },
            format: None,
        };
        let result: Result<Vec<(ConcreteRcRDFOntology, _)>, _> = read_to_closure(&iri, config);
        assert!(
            result.is_err(),
            "expected resolution to fail without a catalog, since the import was moved out of \
             reach of the same-directory heuristic"
        );
    }

    #[test]
    fn test_read_to_closure_relocated_import_succeeds_with_catalog() {
        let path = Path::new("src/ont/owl-rdf/withcatalog/import-property.owl");
        let catalog_path = Path::new("src/ont/owl-rdf/withcatalog/catalog-v001.xml");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, path);

        let catalog = horned_catalog::Catalog::from_path(catalog_path).unwrap();
        let config = RDFParserConfiguration {
            common: ParserConfiguration {
                local_only: true,
                catalog: Some(std::rc::Rc::new(catalog)),
                ..ParserConfiguration::new(&b)
            },
            format: None,
        };

        let v: Vec<(ConcreteRcRDFOntology, _)> = read_to_closure(&iri, config).unwrap();
        let v: Vec<SetOntology<_>> = v
            .into_iter()
            .map(|(rdfo, ic)| {
                assert!(ic.is_complete());
                rdfo.into()
            })
            .collect();

        // Same shape as the un-relocated withimport/ case: the importing
        // ontology plus the one it imports.
        assert_eq!(v.len(), 2);
    }

    #[test]
    fn test_read_to_closure_with_viri() {
        let path = Path::new("src/ont/owl-rdf/withimport/import-property-by-viri.owl");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, path);

        let v: Vec<(ConcreteRcRDFOntology, _)> =
            read_to_closure(&iri, ParserConfiguration::new(&b).into()).unwrap();
        let v: Vec<SetOntology<_>> = v
            .into_iter()
            .map(|(rdfo, ic)| {
                assert!(ic.is_complete());
                rdfo.into()
            })
            .collect();

        assert_eq!(v.len(), 2);
    }

    // #[test]
    // fn test_import_with_version() {
    //     let path = Path::new("src/ont/owl-rdf/manual/annodc.owl");
    //     let b = Build::new_rc();
    //     let iri = path_to_file_iri(&b, path);

    //     let (_, ic): (ConcreteRcRDFOntology, _) = read(&iri, Default::default()).unwrap();
    //     assert!(ic.is_complete());
    // }
}
