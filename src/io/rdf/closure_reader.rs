use crate::error::HornedError;
use crate::io::rdf::reader::parser_with_build;
use crate::io::rdf::reader::OntologyParser;
use crate::io::ParserConfiguration;
use crate::io::IncompleteParse;
use crate::io::RDFOntology;
use crate::model::Build;
use crate::model::ForIRI;
use crate::model::Ontology;
use crate::model::IRI;
use crate::ontology::indexed::ForIndex;
use crate::resolve::path_to_file_iri;
use crate::resolve::resolve_iri;

use std::collections::HashMap;
use std::path::PathBuf;

pub struct ClosureOntologyParser<'a, A: ForIRI, AA: ForIndex<A>> {
    op: HashMap<IRI<A>, OntologyParser<'a, A, AA>>,
    import_map: HashMap<IRI<A>, Vec<IRI<A>>>,
    b: &'a Build<A>,
    config: ParserConfiguration,
}

impl<'a, A: ForIRI, AA: ForIndex<A>> ClosureOntologyParser<'a, A, AA> {
    pub fn new(b: &'a Build<A>, config: ParserConfiguration) -> Self {
        ClosureOntologyParser {
            b,
            import_map: HashMap::new(),
            op: HashMap::new(),
            config
        }
    }

    pub fn parse_path(&mut self, pb: &PathBuf) -> Result<Vec<IRI<A>>, HornedError> {
        let file_iri = path_to_file_iri(self.b, pb);
        let s = ::std::fs::read_to_string(&pb)?;
        let mut v = vec![];

        // We use the IRI that we try to parse, but we don't know that
        // this is the same as file says at this point.
        self.parse_content_from_iri(s, None, file_iri, &mut v)?;

        Ok(v)
    }

    /// Parse content from some IRI.
    ///
    /// Content will be taken by using
    /// [resolve_iri](crate::resolve::resolve_iri), meaning that it
    /// will be loaded from a local resource if possible.
    ///
    /// # Arguments
    ///
    /// * `source_iri` -- the source IRI from which we should
    ///    parse. This may be the declared IRI of the ontology, or a
    ///    local document IRI. This IRI may not be used directly as
    ///    the source depending on the `relative_doc_iri`.
    /// * `relative_doc_iri` -- an IRI that `source_iri` should be
    ///    interpreted as relative to, if any.
    pub fn parse_iri(
        &mut self,
        source_iri: &IRI<A>,
        relative_doc_iri: Option<&IRI<A>>,
    ) -> Result<Vec<IRI<A>>, HornedError> {
        let mut v = vec![];
        self.parse_iri_1(source_iri, relative_doc_iri, &mut v)?;

        Ok(v)
    }

    fn parse_iri_1(
        &mut self,
        source_iri: &IRI<A>,
        relative_doc_iri: Option<&IRI<A>>,
        v: &mut Vec<IRI<A>>,
    ) -> Result<(), HornedError> {
        let (new_doc_iri, s) = resolve_iri(source_iri, relative_doc_iri);
        self.parse_content_from_iri(s, relative_doc_iri, new_doc_iri, v)
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
    ///    determine the relative location of `s` if any.
    /// * `new_doc_iri` -- the IRI that `s` was actually read from
    /// * `v` -- Vec containing all IRIs in the import closure.
    fn parse_content_from_iri(
        &mut self,
        s: String,
        relative_doc_iri: Option<&IRI<A>>,
        new_doc_iri: IRI<A>,
        v: &mut Vec<IRI<A>>,
    ) -> Result<(), HornedError> {
        let mut p = parser_with_build(&mut s.as_bytes(), self.b, self.config);
        let imports = p.parse_imports().unwrap();
        p.parse_declarations()?;
        let o = p.mut_ontology_ref();

        ::std::mem::swap(o.mut_doc_iri(), &mut Some(new_doc_iri.clone()));

        if let Some(declared_iri) = o.i().the_ontology_id_or_default().iri.clone() {
            v.push(declared_iri.clone());

            self.import_map
                .insert(declared_iri.clone(), imports.clone());
            self.op.insert(declared_iri, p);
        }

        for iri in imports {
            // check we haven't already
            self.parse_iri_1(&iri, relative_doc_iri.or(Some(&new_doc_iri)), v)?;
        }
        Ok(())
    }

    // Finish the parse for the ontology at index `i`
    pub fn finish_parse(&mut self, iri: &IRI<A>) -> Result<(), HornedError> {
        let op_pointer: *mut HashMap<_, _> = &mut self.op;

        let import_iris = self.import_map.get(iri).unwrap();
        let import_closure: Vec<_> = import_iris
            .iter()
            .map(|i| self.op.get(i).unwrap().ontology_ref())
            .collect();

        // The import closure references ontologies in the op
        // HashMap. We need to modify one of the ontologies in the map
        // while retaining a reference to the others. Hence the unsafe.
        unsafe {
            (*op_pointer)
                .get_mut(iri)
                .unwrap()
                .finish_parse(&import_closure)?;
        }

        Ok(())
    }

    // Return ontology in potentially incompletely parsed state
    pub fn as_ontology_vec(self) -> Vec<RDFOntology<A, AA>> {
        todo!()
    }

    // Return ontology in potentially incompletely parsed state
    pub fn as_ontology_vec_and_incomplete(self) -> Vec<(RDFOntology<A, AA>, IncompleteParse<A>)> {
        self.op
            .into_values()
            .map(|op| op.as_ontology_and_incomplete().unwrap())
            .collect()
    }
}

// Parse the ontology at IRI, resolving any knowledge from imports necessary
#[allow(clippy::type_complexity)]
pub fn read<A: ForIRI, AA: ForIndex<A>>(
    iri: &IRI<A>,
    config: ParserConfiguration
) -> Result<(RDFOntology<A, AA>, IncompleteParse<A>), HornedError> {
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

#[allow(clippy::type_complexity)]
pub fn read_closure<A: ForIRI, AA: ForIndex<A>>(
    b: &Build<A>,
    iri: &IRI<A>,
    config: ParserConfiguration
) -> Result<Vec<(RDFOntology<A, AA>, IncompleteParse<A>)>, HornedError> {
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
    use crate::io::rdf::reader::RcRDFOntology;
    use crate::ontology::set::SetOntology;
    use std::path::Path;

    #[test]
    fn test_read() {
        let path = Path::new("src/ont/owl-rdf/import-property.owl");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, &path);

        let (_, ic): (RcRDFOntology, _) = read(&iri, Default::default()).unwrap();
        assert!(ic.is_complete());
    }

    // import-property.owl should parse completely with full parse so
    // is a good test.
    #[test]
    fn test_read_closure() {
        let path = Path::new("src/ont/owl-rdf/import-property.owl");
        let b = Build::new_rc();
        let iri = path_to_file_iri(&b, &path);

        let v: Vec<(RcRDFOntology, _)> = read_closure(&b, &iri, Default::default()).unwrap();
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
