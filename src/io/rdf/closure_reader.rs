use crate::error::HornedError;
use crate::io::IncompleteParse;
use crate::io::ParserConfiguration;
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

pub struct ClosureOntologyParser<'a, A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>> {
    // A map between the declared IRI of an Ontology and an OntologyParser
    op: HashMap<IRI<A>, OntologyParser<'a, A, AA, O>>,
    // A map between the declared IRI of an Ontology and the declared
    // IRIs of any Ontology that it imports.
    import_map: HashMap<IRI<A>, Vec<IRI<A>>>,
    b: &'a Build<A>,
    config: ParserConfiguration,
}

impl<'a, A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>> ClosureOntologyParser<'a, A, AA, O> {
    pub fn new(b: &'a Build<A>, config: ParserConfiguration) -> Self {
        ClosureOntologyParser {
            b,
            import_map: HashMap::new(),
            op: HashMap::new(),
            config,
        }
    }

    pub fn parse_path(&mut self, pb: &PathBuf) -> Result<Vec<IRI<A>>, HornedError> {
        let file_iri = path_to_file_iri(self.b, pb);
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
        let (new_doc_iri, s) = resolve_iri(source_iri, relative_doc_iri)?;
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
        let mut p = parser_with_build(&mut s.as_bytes(), self.b, self.config);
        let imports = p.parse_imports().unwrap();
        p.parse_declarations()?;
        let o: &mut O = p.mut_ontology_ref();

        o.insert(DocIRI(new_doc_iri.clone()));

        let si: &SetIndex<A, AA> = o.as_ref();

        let mut res = match si.the_ontology_id_or_default().iri {
            Some(declared_iri) => {
                vec![declared_iri]
            }
            _ => {
                vec![]
            }
        };

        if let Some(declared_iri) = si.the_ontology_id_or_default().iri {
            self.import_map
                .insert(declared_iri.clone(), imports.clone());
            self.op.insert(declared_iri, p);
        }

        res.extend(
            imports
                .iter()
                .flat_map(|iri| self.parse_iri(iri, relative_doc_iri.or(Some(&new_doc_iri))))
                .flatten(),
        );
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

        // Now we can get references to the actual ontologies.
        let import_closure: Result<Vec<_>, HornedError> = import_iris
            .iter()
            .map(|i| {
                self.op
                    .get(i)
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
            .map(|op| op.as_ontology_and_incomplete().unwrap())
            .collect()
    }
}

// Returns the an Ontology and an IncompleteParse report found at a
// given IRI or an Error
#[allow(clippy::type_complexity)]
pub fn read<A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>>(
    iri: &IRI<A>,
    config: ParserConfiguration,
) -> Result<(O, IncompleteParse<A>), HornedError> {
    // Do parse, then full parse of first, drop the rest
    let b = Build::new();
    let mut c = ClosureOntologyParser::new(&b, config);
    c.parse_iri(iri, None)?;

    let keys: Vec<_> = c.op.keys().cloned().collect();
    for i in keys {
        c.finish_parse(&i)?;
    }

    let res = c.as_ontology_vec_and_incomplete();
    Ok(res.into_iter().next().unwrap())
}

// Returns the import closure of an Ontology and IncompleteParse
// report found at a given IRI or an error
#[allow(clippy::type_complexity)]
pub fn read_closure<A: ForIRI, AA: ForIndex<A>, O: RDFOntology<A, AA>>(
    b: &Build<A>,
    iri: &IRI<A>,
    config: ParserConfiguration,
) -> Result<Vec<(O, IncompleteParse<A>)>, HornedError> {
    // Do parse, then full parse, then result the results
    let mut c = ClosureOntologyParser::new(b, config);
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
    fn test_read() {
        let path = Path::new("src/ont/owl-rdf/withimport/import-property.owl");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, path);

        let (_, ic): (ConcreteRcRDFOntology, _) = read(&iri, Default::default()).unwrap();
        assert!(ic.is_complete());
    }

    // import-property.owl should parse completely with full parse so
    // is a good test.
    #[test]
    fn test_read_closure() {
        let path = Path::new("src/ont/owl-rdf/withimport/import-property.owl");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, path);

        let v: Vec<(ConcreteRcRDFOntology, _)> =
            read_closure(&b, &iri, Default::default()).unwrap();
        let v: Vec<SetOntology<_>> = v
            .into_iter()
            .map(|(rdfo, ic)| {
                assert!(ic.is_complete());
                rdfo.into()
            })
            .collect();

        assert_eq!(v.len(), 2);
    }
}
