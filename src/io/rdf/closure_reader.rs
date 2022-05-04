use crate::model::Build;
use crate::model::ForIRI;
use crate::model::IRI;
use crate::model::Ontology;
use crate::io::rdf::reader::OntologyParser;
use crate::io::rdf::reader::ReadError;
use crate::io::RDFOntology;
use crate::io::IncompleteParse;
use crate::ontology::indexed::ForIndex;
use crate::resolve::resolve_iri;
use crate::io::rdf::reader::parser_with_build;

use std::collections::HashMap;
use std::path::PathBuf;

pub struct ClosureOntologyParser<'a, A:ForIRI, AA:ForIndex<A>> {
    op: HashMap<IRI<A>, OntologyParser<'a, A, AA>>,
    import_map: HashMap<IRI<A>, Vec<IRI<A>>>,
    b: &'a Build<A>,
}

impl<'a, A:ForIRI, AA:ForIndex<A>> ClosureOntologyParser<'a, A, AA> {

    pub fn new(b: &'a Build<A>) -> Self {
        ClosureOntologyParser{b, import_map: HashMap::new(), op: HashMap::new().into()}
    }

    pub fn parse_path(pb: &PathBuf) {
        //let file = File::open(&pb)?;brea
        //let reader = io::BufReader::new(file);
        todo!()
    }

    pub fn parse_iri(&mut self, iri: &IRI<A>, doc_iri: Option<&IRI<A>>) -> Vec<IRI<A>> {
        let mut v = vec![];
        self.parse_iri_1(iri, doc_iri, &mut v);

        v
    }

    fn parse_iri_1(&mut self, iri: &IRI<A>, doc_iri: Option<&IRI<A>>, v:&mut Vec<IRI<A>>) {
        let (new_doc_iri, s) = resolve_iri(iri, doc_iri);
        v.push(iri.clone());
        let mut p = parser_with_build(&mut s.as_bytes(), self.b);
        let imports = p.parse_imports().unwrap();
        p.parse_declarations();
        self.import_map.insert(iri.clone(), imports.clone());

        let o = p.mut_ontology_ref();
        ::std::mem::swap(o.mut_doc_iri(), &mut Some(new_doc_iri.clone()));
        self.op.insert(iri.clone(), p);

        for iri in imports {
            // check we haven't already
            self.parse_iri_1(&iri, doc_iri.or(Some(&new_doc_iri)), v);
        }
    }

    // Finish the parse for the ontology at index `i`
    pub fn finish_parse(&mut self, iri: &IRI<A>) {

        let op_pointer: *mut HashMap<_, _> = &mut self.op;

        let import_iris = self.import_map.get(iri).unwrap();
        let import_closure:Vec<_> = import_iris.iter()
            .map(|i| self.op.get(i).unwrap().ontology_ref()).collect();

        dbg!(&iri);
        dbg!(&import_closure);
        // The import closure references ontologies in the op
        // HashMap. We need to modify one of the ontologies in the map
        // while retaining a reference to the others. Hence the unsafe.
        unsafe{
            (*op_pointer).get_mut(iri).unwrap().finish_parse(&import_closure);
        }
    }

    fn parse_iri_if_needed(&self, iri: &IRI<A>) {
        // Parse an IRI that has been imported unless it's already in Vec

        //
    }

    // Return ontology in potentially incompletely parsed state
    pub fn as_ontology_vec(self) -> Vec<RDFOntology<A, AA>> {
        todo!()
    }

    // Return ontology in potentially incompletely parsed state
    pub fn as_ontology_vec_and_incomplete(self) -> Vec<(RDFOntology<A, AA>, IncompleteParse<A>)> {
        self.op.into_values().map(|op| op.as_ontology_and_incomplete().unwrap()).collect()
    }
}


// Parse the ontology at IRI, resolving any knowledge from imports necessary
pub fn read<A:ForIRI, AA:ForIndex<A>>(iri: &IRI<A>) -> Result<(RDFOntology<A, AA>, IncompleteParse<A>), ReadError> {
    // Do parse, then full parse of first, drop the rest
    let b = Build::new();
    let mut c = ClosureOntologyParser::new(&b);
    c.parse_iri(iri, None);

    let keys:Vec<_> = c.op.keys().map(|k|k.clone()).collect();
    for i in keys.clone() {
        c.finish_parse(&i);
    }

    let res = c.as_ontology_vec_and_incomplete();
    Ok(res.into_iter().next().unwrap())
}


pub fn read_closure<A:ForIRI, AA:ForIndex<A>>(b: &Build<A>, iri: &IRI<A>)
                                              -> Result<Vec<(RDFOntology<A, AA>, IncompleteParse<A>)>, ReadError> {
    // Do parse, then full parse, then result the results
    let mut c = ClosureOntologyParser::new(b);
    c.parse_iri(iri, None);
    let keys:Vec<_> = c.op.keys().map(|k|k.clone()).collect();
    for i in keys.clone() {
        c.finish_parse(&i);
    }

    Ok(c.as_ontology_vec_and_incomplete())
}


#[cfg(test)]
mod test {
    use crate::ontology::set::SetOntology;
    use crate::io::rdf::reader::RcRDFOntology;
    use crate::io::rdf::closure_reader::*;
    use crate::resolve::*;
    use std::path::Path;

    #[test]
    fn test_read() {
        let path_buf = Path::new("src/ont/owl-rdf/import-property.owl").to_path_buf();
        let b = Build::new_rc();
        let iri = pathbuf_to_file_iri(&b, &path_buf);

        let (rdfo, ic):(RcRDFOntology, _) = read(&iri).unwrap();
        assert!(ic.is_complete());
    }


    // import-property.owl should parse completely with full parse so
    // is a good test.
    #[test]
    fn test_read_closure() {
        let path_buf = Path::new("src/ont/owl-rdf/import-property.owl").to_path_buf();
        let b = Build::new_rc();
        let iri = pathbuf_to_file_iri(&b, &path_buf);

        let v:Vec<(RcRDFOntology, _)> = read_closure(&b, &iri).unwrap();
        let v:Vec<SetOntology<_>> = v.into_iter().map(|(rdfo, ic)| {
            assert!(ic.is_complete());
            rdfo.into()
        }).collect();

        assert_eq!(v.len(), 2);
    }
}
