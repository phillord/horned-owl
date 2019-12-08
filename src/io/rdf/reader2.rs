#![allow(dead_code, unused_variables)]

use crate::model::*;
use crate::vocab::*;

use crate::index::update_logically_equal_axiom;

use curie::PrefixMapping;

use failure::Error;

use sophia::term::BNodeId;
use sophia::term::IriData;
use sophia::term::LiteralKind;
use sophia::term::Term;


use std::io::BufRead;
use std::rc::Rc;

// Two type aliases for "SoPhia" entities.
type SpTerm = Term<Rc<str>>;
type SpIri = IriData<Rc<str>>;

#[derive(Debug)]
enum AcceptState {
    // Accept and consume the triple
    Accept,

    // Do not accept the triple and return it
    Return([Term<Rc<str>>; 3]),

    // Return triples that were previously accepted
    BackTrack(Vec<[Term<Rc<str>>; 3]>),
}

enum CompleteState {
    // The acceptor does not have all of the information that it needs
    // because it has not recieved enough triples.
    NotComplete,

    // The acceptor has all the information that it needs to complete,
    // but can still accept more. The acceptor may require further
    // information from the ontology, for instance the type of IRIs in
    // accepted triples, to actually complete.
    CanComplete,

    // The acceptor has all the information that it needs to complete,
    // and will not accept further triples. The acceptor may require
    // further information from the ontology, for instance the type of
    // IRIs in accepted triples, to actually complete.
    Complete,
}

trait Acceptor<O>: std::fmt::Debug {
    // Accept a triple.
    fn accept(&mut self, b: &Build, triple: [Term<Rc<str>>; 3]) -> AcceptState;

    // Indicate the completion state of the acceptor.
    fn complete_state(&self) -> CompleteState;

    fn is_complete(&self) -> bool {
        match self.complete_state() {
            CompleteState::Complete => true,
            _ => false,
        }
    }
    fn can_complete(&self) -> bool {
        match self.complete_state() {
            CompleteState::CanComplete | CompleteState::Complete => true,
            _ => false,
        }
    }

    // Return an ontology entity based on the triples that have been
    // consumed and the data in the ontology. Return an Error if the
    // acceptor cannot return yet because there is information missing
    // from the ontology (such as a declaration).

    // This method should not be called till can_complete would return
    // either Complete or CanComplete if called.

    // TODO: Do I need more than one Error here? I know for sure that
    // I will need to return "not got enough information from the
    // ontology"; I guess I should be able to return a "not in the
    // right state" error also. Anything else?
    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<O, Error>;
}

#[derive(Debug, Default)]
struct OntologyAcceptor {
    // Acceptors which are NotComplete or CanComplete
    incomplete_acceptors: Vec<Box<dyn Acceptor<(AnnotatedAxiom, Merge)>>>,

    // Acceptors which are Complete
    complete_acceptors: Vec<Box<dyn Acceptor<(AnnotatedAxiom, Merge)>>>,

    // Does this make any sense -- we are replicating the Ontology
    // data structure here? And our data structures are
    // complicated. Why do we not build these up as we go?

    // Because now we have to convert backwards and forwards between
    // the sophia data structures -- think it was better before. It
    // will probably help anyway when we come to structures were we
    // cannot work it out early
    iri: Option<SpIri>,
    viri: Option<SpIri>,
}

impl Acceptor<Ontology> for OntologyAcceptor {
    fn accept(&mut self, b: &Build, mut triple: [Term<Rc<str>>; 3]) -> AcceptState {
        match &triple {
            [Term::Iri(s), Term::Iri(p), Term::Iri(ob)]
                if p == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                    && ob == &OWL::Ontology.iri_str() =>
            {
                self.iri = Some(s.clone());
                AcceptState::Accept
            }
            [Term::Iri(s), Term::Iri(p), Term::Iri(ob)]
                if self.iri.as_ref() == Some(s) && p == &OWL::VersionIRI.iri_str() =>
            {
                self.viri = Some(ob.clone());
                AcceptState::Accept
            }
            _ => {
                // Pass on to incomplete acceptors, till one of them
                // accepts, then pass onto new acceptors and see if
                // one of them accepts. Collect and collate any "backtracks",
                // return one of these.

                // iterate through the incomplete acceptors and try
                // them first
                for i in 0..self.incomplete_acceptors.len() {
                    let ac = &mut self.incomplete_acceptors[i];
                    let acr = ac.accept(b, triple);
                    match acr {
                        AcceptState::Accept => {
                            if ac.is_complete() {
                                self.complete_acceptors.push(
                                    self.incomplete_acceptors.remove(i)
                                );
                            }
                            return acr;
                        }
                        AcceptState::Return(t) => {
                            triple = t;
                        }
                        AcceptState::BackTrack(ts) => unimplemented!(),
                    }
                }

                // Iterate through new acceptor
                let acceptors: Vec<Box<dyn Acceptor<(AnnotatedAxiom, Merge)>>> = vec![
                    Box::new(SimpleAnnotatedAxiomAcceptor::default()),
                    Box::new(AnnotatedAxiomAcceptor::default()),
                ];

                for mut ac in acceptors {
                    let acr = ac.accept(b, triple);
                    match acr {
                        AcceptState::Accept => {
                            if ac.is_complete() {
                                self.complete_acceptors.push(ac)
                            } else {
                                self.incomplete_acceptors.push(ac)
                            };
                            return acr;
                        }
                        AcceptState::Return(t) => {
                            triple = t;
                        }
                        AcceptState::BackTrack(ts) => unimplemented!(),
                    }
                }

                AcceptState::Return(triple)
            }
        }
    }

    fn complete_state(&self) -> CompleteState {
        unimplemented!()
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<Ontology, Error> {
        // Iterate over all the complete Acceptor, run complete on
        // them, and insert this
        let mut o = Ontology::default();
        o.id.iri = self.iri.as_ref().map(|i| b.iri(i.to_string()));
        o.id.viri = self.viri.as_ref().map(|i| b.iri(i.to_string()));

        //let mut ch = self.complete_acceptors.iter().chain(self.incomplete_acceptors.iter());
        for ac in self.complete_acceptors.iter_mut()
            .chain(self.incomplete_acceptors.iter_mut()) {
            let c = ac.complete(b, &o)?;
            if let Merge::NeedsMerge = c.1 {
                update_logically_equal_axiom(&mut o, c.0)
            }
            else {
                o.insert(c.0);
            }
        }

        return Ok(o);
    }
}

#[derive(Debug)]
enum Merge {
    NeedsMerge,
    DoesNotNeedMerge,
}

// Accept axioms with no annotations then as an AnnotatedAxiom (with
// no annotations)
#[derive(Debug, Default)]
struct SimpleAnnotatedAxiomAcceptor {
    // Currently we just support a single axiom type
    ac: Option<Box<dyn Acceptor<AnnotatedAxiom>>>,
}

impl Acceptor<(AnnotatedAxiom, Merge)> for SimpleAnnotatedAxiomAcceptor {
    fn accept(&mut self, b: &Build, mut triple: [Term<Rc<str>>; 3]) -> AcceptState {
        match &mut self.ac {
            None => {
                // Try all the possibilities till we find the first which
                // accepts. Or currently, just fake it
                let acceptors: Vec<Box<dyn Acceptor<AnnotatedAxiom>>> = vec![
                    Box::new(DeclarationAcceptor::default()),
                    Box::new(SubClassOfAcceptor::default())
                ];

                for mut ac in acceptors {
                    let acr = ac.accept(b, triple);
                    match acr {
                        AcceptState::Accept => {
                            self.ac = Some(ac);
                            return AcceptState::Accept;
                        }
                        AcceptState::Return(t) => {
                            triple = t
                        }
                        _ => {
                            panic!("Acceptor in unexpected state");
                        }
                    }
                }
                AcceptState::Return(triple)
            }
            Some(acceptor) => acceptor.accept(b, triple),
        }
    }

    fn complete_state(&self) -> CompleteState {
        match &self.ac {
            Some(a) => a.complete_state(),
            None => CompleteState::NotComplete,
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<(AnnotatedAxiom, Merge), Error> {
        match &mut self.ac {
            Some(boxacceptor) => Ok((boxacceptor.complete(b, o)?, Merge::DoesNotNeedMerge)),
            None => unimplemented!(),
        }
    }
}

// Accept reified annotations of an axiom
#[derive(Debug, Default)]
struct AnnotatedAxiomAcceptor {
    bnodeid: Option<BNodeId<Rc<str>>>,
    // Currently we just support a single axiom type
    annotated_source: Option<SpTerm>,
    annotated_property: Option<SpTerm>,
    annotated_target: Option<SpTerm>,
    annotations: Vec<AnnotationAcceptor>,
    complete: bool
}

impl Acceptor<(AnnotatedAxiom, Merge)> for AnnotatedAxiomAcceptor {
    fn accept(&mut self, b: &Build, triple: [Term<Rc<str>>; 3]) -> AcceptState {
        match &triple {
            // This should only happen when bnodeid is None
            [Term::BNode(s), Term::Iri(p), Term::Iri(ob)]
                if p == &RDF::Type.iri_str() && ob == &OWL::Axiom.iri_str() =>
            {
                self.bnodeid = Some(s.clone());
                AcceptState::Accept
            }
            [Term::BNode(s), Term::Iri(p), Term::Iri(ob)]
                if Some(s) == self.bnodeid.as_ref() =>
            {
                match p {
                    _ if p == &OWL::AnnotatedSource.iri_str() => {
                        self.annotated_source = Some(triple[2].clone());
                        AcceptState::Accept
                    },
                    _ if p == &OWL::AnnotatedProperty.iri_str() => {
                        self.annotated_property = Some(triple[2].clone());
                        AcceptState::Accept
                    }
                    _ if p == &OWL::AnnotatedTarget.iri_str() => {
                        self.annotated_target = Some(triple[2].clone());
                        AcceptState::Accept
                    }
                    _ => {
                        dbg!("On bnode, but unrecognised", &triple);
                        // This needs to be passed on to an annotation
                        // acceptor
                        AcceptState::Return(triple)
                    }
                }
            }
            [Term::BNode(s), _, _]
                if Some(s) == self.bnodeid.as_ref() =>
            {
                let mut annac = AnnotationAcceptor::default();
                let rtn = annac.accept(b, triple);

                if annac.is_complete() {
                    self.annotations.push(annac);
                }
                rtn
            }
            _ if self.bnodeid.is_some() && !self.complete => {
                self.complete = true;
                AcceptState::Return(triple)
            }
            _ => {
                AcceptState::Return(triple)
            }
        }
    }

    fn complete_state(&self) -> CompleteState {
        if self.complete {
            CompleteState::Complete
        }
        else {
            if self.bnodeid.is_some()
                && self.annotated_source.is_some()
                && self.annotated_property.is_some()
                && self.annotated_target.is_some()
            {
                CompleteState::CanComplete
            } else {
                CompleteState::NotComplete
            }
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<(AnnotatedAxiom, Merge), Error> {
        // Convert the reified triple into a normal one, and then pass
        // it to SimpleAnnotatedAxiomAcceptor to get the normal
        // axiom.
        let mut simple_acceptor = SimpleAnnotatedAxiomAcceptor::default();

        let acs = simple_acceptor.accept(b,
                                         [self.annotated_source.take().unwrap(),
                                          self.annotated_property.take().unwrap(),
                                          self.annotated_target.take().unwrap()]);

        let mut ann_axiom = simple_acceptor.complete(b, o)?.0;
        for i in &mut self.annotations {
            ann_axiom.ann.insert(i.complete(b, o)?);
        }

        Ok((ann_axiom, Merge::NeedsMerge))
    }
}

// Accept declarations of type
#[derive(Debug, Default)]
struct DeclarationAcceptor {
    iri: Option<IriData<Rc<str>>>,
}

impl Acceptor<AnnotatedAxiom> for DeclarationAcceptor {
    fn accept(&mut self, b: &Build, triple: [Term<Rc<str>>; 3]) -> AcceptState {
        match &triple {
            [Term::Iri(s), Term::Iri(p), Term::Iri(ob)]
                if p == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#type" =>
            {
                self.iri = Some(s.clone());
                AcceptState::Accept
            }
            _ => AcceptState::Return(triple),
        }
    }

    fn complete_state(&self) -> CompleteState {
        if self.iri.is_some() {
            CompleteState::Complete
        } else {
            CompleteState::NotComplete
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<AnnotatedAxiom, Error> {
        // Iterate over all the complete Acceptor, run complete on
        // them, and insert this
        let n: NamedEntity = b.class(self.iri.as_ref().unwrap().to_string()).into();
        Ok(declaration(n).into())
    }
}

#[derive(Debug, Default)]
struct SubClassOfAcceptor {
    superclass: Option<SpIri>,
    subclass: Option<SpIri>,
}

impl Acceptor<AnnotatedAxiom> for SubClassOfAcceptor {
    fn accept(&mut self, b: &Build, triple: [Term<Rc<str>>; 3]) -> AcceptState {
        match &triple {
            [Term::Iri(s), Term::Iri(p), Term::Iri(ob)]
                if p == &RDFS::SubClassOf.iri_str() =>
            {
                self.superclass=Some(ob.clone());
                self.subclass=Some(s.clone());
                AcceptState::Accept
            }
            _ => AcceptState::Return(triple),
        }
    }

    fn complete_state(&self) -> CompleteState {
        if self.superclass.is_some() {
            CompleteState::Complete
        } else {
            CompleteState::NotComplete
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<AnnotatedAxiom, Error> {
        // Iterate over all the complete Acceptor, run complete on
        // them, and insert this
        Ok(
            SubClassOf {
                sub: b.class(self.subclass.as_ref().unwrap().to_string()).into(),
                sup: b.class(self.superclass.as_ref().unwrap().to_string()).into(),
            }.into()
        )
    }
}

// Accept annotations
#[derive(Debug, Default)]
struct AnnotationAcceptor {
    p: Option<SpIri>,
    iri_val: Option<SpIri>,
    literal_val: Option<Rc<str>>,
    literal_lang: Option<Rc<str>>,
}

impl Acceptor<Annotation> for AnnotationAcceptor {
    fn accept(&mut self, b: &Build, triple: [Term<Rc<str>>; 3]) -> AcceptState {
        match &triple {
            [Term::BNode(s), Term::Iri(p), Term::Literal(ob, kind)] =>
            {
                // Literal value
                self.p = Some(p.clone());
                self.literal_val = Some(ob.clone());
                if let LiteralKind::Lang(lang) = kind {
                    self.literal_lang = Some(lang.clone());
                }
                AcceptState::Return(triple)
            }
            [Term::BNode(s), Term::Iri(p), Term::Iri(ob)] => {
                // IRI annotation value
                self.p = Some(p.clone());
                self.iri_val = Some(ob.clone());
                AcceptState::Return(triple)
            }
            _ => {
                unimplemented!()
            }
        }
    }

    fn complete_state(&self) -> CompleteState {
        if self.p.is_some() &&
            self.iri_val.is_some() ||
            self.literal_val.is_some() {
                CompleteState::Complete
            }
        else {
            CompleteState::NotComplete
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<Annotation, Error> {
        Ok(
            Annotation {
                ap: b.annotation_property(self.p.as_ref().unwrap().to_string()),
                av: if self.iri_val.is_some() {
                    b.iri(self.iri_val.as_ref().unwrap().to_string()).into()
                }
                else {
                    Literal::Language {
                        literal:self.literal_val.as_ref().unwrap().to_string(),
                        lang: self.literal_lang.as_ref().unwrap().to_string(),
                    }.into()
                }
            }
        )
    }
}

fn read_then_complete(
    triple_iter: impl Iterator<Item = Result<[Term<Rc<str>>; 3], sophia::error::Error>>,
    b: &Build,
    mut acceptor: OntologyAcceptor,
) -> Result<Ontology, Error> {
    for t in triple_iter {
        let t = t.unwrap();
        acceptor.accept(b, t);
    }

    acceptor.complete(b, &Ontology::default())
}

pub fn read_with_build<R: BufRead>(
    bufread: &mut R,
    build: &Build,
) -> Result<(Ontology, PrefixMapping), Error> {
    let parser = sophia::parser::xml::Config::default();
    let triple_iter = parser.parse_bufread(bufread);

    return read_then_complete(triple_iter, build, OntologyAcceptor::default())
        .map(|o| return (o, PrefixMapping::default()));
}

pub fn read<R: BufRead>(bufread: &mut R) -> Result<(Ontology, PrefixMapping), Error> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::path::PathBuf;

    use pretty_assertions::assert_eq;

    fn read_ok<R: BufRead>(bufread: &mut R) -> (Ontology, PrefixMapping) {
        let r = read(bufread);

        assert!(r.is_ok(), "Expected ontology, get failure: {:?}", r.err());
        r.unwrap()
    }

    fn compare(test: &str) {
        let dir_path_buf = PathBuf::from(file!());
        let dir = dir_path_buf.parent().unwrap().to_string_lossy();

        compare_str(
            &slurp::read_all_to_string(format!("{}/../../ont/owl-rdf/{}.owl", dir, test)).unwrap(),
            &slurp::read_all_to_string(format!("{}/../../ont/owl-xml/{}.owx", dir, test)).unwrap(),
        );
    }

    fn compare_str(rdfread: &str, xmlread: &str) {
        let (rdfont, _rdfmapping) = read_ok(&mut rdfread.as_bytes());
        let (xmlont, _xmlmapping) = crate::io::reader::test::read_ok(&mut xmlread.as_bytes());

        assert_eq!(rdfont, xmlont);

        //let rdfmapping: &HashMap<&String, &String> = &rdfmapping.mappings().collect();
        //let xmlmapping: &HashMap<&String, &String> = &xmlmapping.mappings().collect();

        //assert_eq!(rdfmapping, xmlmapping);
    }

    #[test]
    fn one_class() {
        compare("one-class");
    }

    #[test]
    fn declaration_with_annotation() {
        compare("declaration-with-annotation");
    }

    // #[test]
    // fn class_with_two_annotations() {
    //     compare("class_with_two_annotations");
    // }

    #[test]
    fn one_ont() {
        compare("one-ont");
    }

    #[test]
    fn one_subclass() {
        compare("one-subclass");
    }

    #[test]
    fn subclass_with_annotation() {
        compare("annotation-on-subclass");
    }

    // #[test]
    // fn one_oproperty() {
    //     compare("one-oproperty");
    // }

    // #[test]
    // fn one_some() {
    //     compare("one-some");
    // }

    // #[test]
    // fn one_only() {
    //     compare("one-only");
    // }

    // #[test]
    // fn one_and() {
    //     compare("one-and");
    // }

    // #[test]
    // fn one_or() {
    //     compare("one-or");
    // }

    // #[test]
    // fn one_not() {
    //     compare("one-not");
    // }

    // #[test]
    // fn one_annotation_property() {
    //     compare("one-annotation-property");
    // }

    // #[test]
    // fn one_annotation() {
    //     compare("one-annotation");
    // }

    // #[test]
    // fn annotation_domain() {
    //     compare("annotation-domain");
    // }

    // #[test]
    // fn annotation_range() {
    //     compare("annotation-range");
    // }

    // #[test]
    // fn one_label() {
    //     compare("one-label");
    // }

    // #[test]
    // fn one_comment() {
    //     // This is currently failing because the XML parser gives the
    //     // comment a language and a datatype ("PlainLiteral") while
    //     // the RDF one gives it just the language, as literals can't
    //     // be both. Which is correct?
    //     compare("one-comment");
    // }

    // #[test]
    // fn one_ontology_annotation() {
    //     compare("one-ontology-annotation");
    // }

    // #[test]
    // fn one_equivalent_class() {
    //     compare("one-equivalent");
    // }

    // #[test]
    // fn one_disjoint_class() {
    //     compare("one-disjoint");
    // }

    // #[test]
    // fn disjoint_union() {
    //     compare("disjoint-union");
    // }

    // #[test]
    // fn one_sub_property() {
    //     compare("one-suboproperty");
    // }

    // #[test]
    // fn one_inverse() {
    //     compare("inverse-properties");
    // }

    // #[test]
    // fn one_transitive() {
    //     compare("transitive-properties");
    // }

    // #[test]
    // fn one_annotated_transitive() {
    //     compare("annotation-on-transitive");
    // }

    // #[test]
    // fn one_subproperty_chain() {
    //     compare("subproperty-chain");
    // }

    // #[test]
    // fn one_subproperty_chain_with_inverse() {
    //     compare("subproperty-chain-with-inverse");
    // }

    // #[test]
    // fn annotation_on_annotation() {
    //     compare("annotation-with-annotation");
    // }

    // #[test]
    // fn sub_annotation() {
    //     compare("sub-annotation");
    // }

    // #[test]
    // fn data_property() {
    //     compare("data-property");
    // }

    // #[test]
    // fn literal_escaped() {
    //     compare("literal-escaped");
    // }

    // #[test]
    // fn named_individual() {
    //     compare("named-individual");
    // }

    // #[test]
    // fn import() {
    //     compare("import");
    // }

    // #[test]
    // fn datatype() {
    //    compare("datatype");
    //}

    // #[test]
    // fn object_has_value() {
    //     compare("object-has-value");
    // }

    // #[test]
    // fn object_one_of() {
    //     compare("object-one-of");
    // }

    // #[test]
    // fn inverse() {
    //     compare("some-inverse");
    // }

    // #[test]
    // fn object_unqualified_cardinality() {
    //     compare("object-unqualified-max-cardinality");
    // }

    // #[test]
    // fn object_min_cardinality() {
    //     compare("object-min-cardinality");
    // }

    // #[test]
    // fn object_max_cardinality() {
    //     compare("object-max-cardinality");
    // }

    // #[test]
    // fn object_exact_cardinality() {
    //     compare("object-exact-cardinality");
    // }

    // #[test]
    // fn datatype_alias() {
    //     compare("datatype-alias");
    // }

    // #[test]
    // fn datatype_intersection() {
    //     compare("datatype-intersection");
    // }

    // #[test]
    // fn datatype_union() {
    //     compare("datatype-union");
    // }

    // #[test]
    // fn datatype_complement() {
    //     compare("datatype-complement");
    // }

    // #[test]
    // fn datatype_oneof() {
    //     compare("datatype-oneof");
    // }

    // #[test]
    // fn datatype_some() {
    //    compare("data-some");
    // }

    // #[test]
    // fn facet_restriction() {
    //     compare("facet-restriction");
    // }

    // #[test]
    // fn data_only() {
    //     compare("data-only");
    // }

    // #[test]
    // fn data_exact_cardinality() {
    //     compare("data-exact-cardinality");
    // }

    // #[test]
    // fn data_has_value() {
    //     compare("data-has-value");
    // }

    // #[test]
    // fn data_max_cardinality() {
    //     compare("data-max-cardinality");
    // }

    // #[test]
    // fn data_min_cardinality() {
    //     compare("data-min-cardinality");
    // }

    // #[test]
    // fn class_assertion() {
    //     compare("class-assertion");
    // }

    // #[test]
    // fn data_property_assertion() {
    //     compare("data-property-assertion");
    // }

    // #[test]
    // fn same_individual() {
    //     compare("same-individual");
    // }

    // #[test]
    // fn different_individuals() {
    //     compare("different-individual");
    // }

    // #[test]
    // fn negative_data_property_assertion() {
    //     compare("negative-data-property-assertion");
    // }

    // #[test]
    // fn negative_object_property_assertion() {
    //     compare("negative-object-property-assertion");
    // }

    // #[test]
    // fn object_property_assertion() {
    //     compare("object-property-assertion");
    // }

    // #[test]
    // fn data_has_key() {
    //     compare("data-has-key");
    // }

    // #[test]
    // fn data_property_disjoint() {
    //     compare("data-property-disjoint");
    // }

    // #[test]
    // fn data_property_domain() {
    //     compare("data-property-domain");
    // }

    // #[test]
    // fn data_property_equivalent() {
    //     compare("data-property-equivalent");
    // }

    // #[test]
    // fn data_property_functional() {
    //     compare("data-property-functional");
    // }

    // #[test]
    // fn data_property_range() {
    //     compare("data-property-range");
    // }

    // #[test]
    // fn data_property_sub() {
    //     compare("data-property-sub");
    // }

    // #[test]
    // fn disjoint_object_properties() {
    //     compare("disjoint-object-properties");
    // }

    // #[test]
    // fn equivalent_object_properties() {
    //     compare("equivalent_object_properties");
    // }

    // #[test]
    // fn object_has_key() {
    //     compare("object-has-key");
    // }

    // #[test]
    // fn object_property_asymmetric() {
    //     compare("object-property-asymmetric");
    // }

    // #[test]
    // fn object_property_domain() {
    //     compare("object-property-domain");
    // }

    // #[test]
    // fn object_property_functional() {
    //     compare("object-property-functional");
    // }

    // #[test]
    // fn object_property_inverse_functional() {
    //     compare("object-property-inverse-functional");
    // }

    // #[test]
    // fn object_property_irreflexive() {
    //     compare("object-property-irreflexive");
    // }

    // #[test]
    // fn object_property_range() {
    //     compare("object-property-range");
    // }

    // #[test]
    // fn object_property_reflexive() {
    //     compare("object-property-reflexive");
    // }

    // #[test]
    // fn object_property_symmetric() {
    //     compare("object-property-symmetric");
    // }

    // #[test]
    // fn family() {
    //     compare("family");
    // }
}
