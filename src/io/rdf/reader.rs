use crate::model::*;

use curie::PrefixMapping;

use std::collections::VecDeque;
use std::io::BufRead;
use std::rc::Rc;

use failure::Error;

use sophia::term::BNodeId;
use sophia::term::IriData;
use sophia::term::LiteralKind;
use sophia::term::Term;

/// https://www.w3.org/TR/owl2-mapping-to-rdf/

pub fn read<R: BufRead>(bufread: &mut R) -> Result<(Ontology, PrefixMapping), Error> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

pub fn read_with_build<R: BufRead>(
    bufread: &mut R,
    build: &Build,
) -> Result<(Ontology, PrefixMapping), Error> {
    let parser = sophia::parser::xml::Config::default();
    let triple_iter = parser.parse_bufread(bufread);
    let mut o = Ontology::new();

    let mut incomplete_acceptors: VecDeque<Box<dyn Acceptor>> = VecDeque::new();

    'outer: for triple in triple_iter {
        dbg!(&triple);
        let triple = triple.unwrap();

        let mut offset = 0;
        for index in 0..incomplete_acceptors.len() {
            match incomplete_acceptors[index - offset].accept(&mut o, build, &triple)? {
                AcceptorState::Accepted => {
                    continue 'outer;
                }
                AcceptorState::AcceptedComplete => {
                    incomplete_acceptors.remove(index);
                    continue 'outer;
                }
                AcceptorState::NotAcceptedComplete => {
                    incomplete_acceptors.remove(index - offset);
                    offset = offset + 1;
                }
                AcceptorState::NotAccepted => {}
            }
        }

        let mut args = (&mut o, build, &mut incomplete_acceptors, &triple);
        let _rtn = accept_maybe(SingleAcceptor::default(), &mut args)?
            || accept_maybe(AnnotationMainTriple::default(), &mut args)?;
    }

    Ok((o, PrefixMapping::default()))
}

fn accept_maybe<A: Acceptor + 'static>(
    mut acceptor: A,
    args: &mut (
        &mut Ontology,
        &Build,
        &mut VecDeque<Box<dyn Acceptor>>,
        &[Term<Rc<str>>; 3],
    ),
) -> Result<bool, Error> {
    let (o, b, incomplete_acceptors, triple) = args;
    match acceptor.accept(o, b, triple)? {
        AcceptorState::Accepted => {
            incomplete_acceptors.push_front(Box::new(acceptor));
            Ok(true)
        }
        AcceptorState::AcceptedComplete => Ok(true),
        AcceptorState::NotAcceptedComplete => (Ok(false)),
        AcceptorState::NotAccepted => Ok(false),
    }
}

// Helper Functions
fn iri_from_term(triple: &Term<Rc<str>>, b: &Build) -> Result<IRI, Error> {
    match triple {
        Term::Iri(iri_data) => Ok(b.iri(iri_data.to_string())),
        _ => bail!("Expected IRI term found: {:?}", triple),
    }
}

fn iri_from_iri_data(iri_data: &IriData<Rc<str>>, b: &Build) -> IRI {
    b.iri(iri_data.to_string())
}

fn annotation_value_from_term(term: &Term<Rc<str>>, b: &Build) -> Result<AnnotationValue, Error> {
    match term {
        Term::Iri(iri_data) => Ok(iri_from_iri_data(iri_data, b).into()),
        Term::Literal(v, LiteralKind::Lang(lang)) => Ok(Literal::Language {
            lang: lang.to_string(),
            literal: v.to_string(),
        }
        .into()),
        Term::Literal(v, LiteralKind::Datatype(iri_data)) => Ok(Literal::Datatype {
            datatype_iri: iri_from_iri_data(iri_data, b),
            literal: v.to_string(),
        }
        .into()),
        _ => bail!("Expected annotation value"),
    }
}

// Acceptor trait and implementations
enum AcceptorState {
    Accepted,
    AcceptedComplete,
    NotAcceptedComplete,
    NotAccepted,
}

trait Acceptor {
    fn accept(
        &mut self,
        o: &mut Ontology,
        b: &Build,
        triple: &[Term<Rc<str>>; 3],
    ) -> Result<AcceptorState, Error>;
}

#[derive(Default)]
struct SingleAcceptor;

impl Acceptor for SingleAcceptor {
    fn accept(
        &mut self,
        o: &mut Ontology,
        b: &Build,
        triple: &[Term<Rc<str>>; 3],
    ) -> Result<AcceptorState, Error> {
        match &triple[1] {
            Term::Iri(d) => {
                if *d == "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" {
                    return self.accept_type(o, b, triple);
                }
                if *d == "http://www.w3.org/2000/01/rdf-schema#subClassOf" {
                    o.insert(SubClassOf {
                        sup: ClassExpression::Class(b.class(iri_from_term(&triple[2], b)?)),
                        sub: ClassExpression::Class(b.class(iri_from_term(&triple[0], b)?)),
                    });
                    return Ok(AcceptorState::AcceptedComplete);
                }
                if *d == "http://www.w3.org/2002/07/owl#versionIRI" {
                    o.id.viri = Some(iri_from_term(&triple[2], b)?);
                    return Ok(AcceptorState::AcceptedComplete);
                }
                if crate::vocab::is_annotation_builtin(&d.to_string()) {
                    o.insert(AnnotationAssertion {
                        subject: iri_from_term(&triple[0], b)?,
                        ann: Annotation {
                            ap: b.annotation_property(iri_from_term(&triple[1], b)?),
                            av: annotation_value_from_term(&triple[2], b)?,
                        },
                    });
                    return Ok(AcceptorState::AcceptedComplete);
                }

            }
            _ => {}
        }
        return Ok(AcceptorState::NotAccepted);
    }
}

impl SingleAcceptor {
    fn accept_type(
        &self,
        o: &mut Ontology,
        b: &Build,
        triple: &[Term<Rc<str>>; 3],
    ) -> Result<AcceptorState, Error> {
        // let subject = match &triple[0] {
        //     Term::Iri(d) => d.to_string(),
        //     _ => panic!("whoops"),
        // };
        let subject = &triple[0].value();

        match &triple[2] {
            Term::Iri(s) if s == &"http://www.w3.org/2002/07/owl#Ontology" => {
                o.id.iri = Some(b.iri(subject));
            }
            Term::Iri(s) if s == &"http://www.w3.org/2002/07/owl#Class" => {
                o.declare(b.class(subject));
            }
            _ => {
                return Ok(AcceptorState::NotAccepted);
            }
        }
        return Ok(AcceptorState::AcceptedComplete);
    }
}

// s p xlt .
// _:x rdf:type owl:Axiom .
// _:x owl:annotatedSource s .
// _:x owl:annotatedProperty p .
// _:x owl:annotatedTarget xlt .

#[derive(Debug, Default)]
struct AnnotationMainTriple {
    bnode: Option<BNodeId<Rc<str>>>,
    source: Option<IRI>,
    property: Option<IRI>,
    target: Option<IRI>,
}

impl Acceptor for AnnotationMainTriple {
    fn accept(
        &mut self,
        _o: &mut Ontology,
        b: &Build,
        triple: &[Term<Rc<str>>; 3],
    ) -> Result<AcceptorState, Error> {
        match (&self, &triple) {
            (&Self { bnode: None, .. }, [Term::BNode(s), Term::Iri(p), Term::Iri(o)])
                if p == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                    && o == &"http://www.w3.org/2002/07/owl#Axiom" =>
            {
                self.bnode = Some(s.to_owned());
                return Ok(AcceptorState::Accepted);
            }
            (
                &Self {
                    bnode: Some(ref bnode),
                    ..
                },
                [Term::BNode(s), Term::Iri(p), Term::Iri(o)],
            ) if bnode == s => match p {
                _ if p == &"http://www.w3.org/2002/07/owl#annotatedSource" => {
                    self.source = Some(iri_from_iri_data(o, b));
                    return Ok(AcceptorState::Accepted);
                }
                _ if p == &"http://www.w3.org/2002/07/owl#annotatedProperty" => {
                    self.property = Some(iri_from_iri_data(o, b));
                    return Ok(AcceptorState::Accepted);
                }
                _ if p == &"http://www.w3.org/2002/07/owl#annotatedTarget" => {
                    self.target = Some(iri_from_iri_data(o, b));
                    return Ok(AcceptorState::Accepted);
                }
                _ => {
                    return Ok(AcceptorState::NotAccepted);
                }
            },
            (
                &Self {
                    bnode: Some(_),
                    source: Some(_),
                    property: Some(_),
                    target: Some(_),
                },
                [_, _, _],
            ) => {
                // We now have all the data that we need to identify
                // the axiom, so all the triples that follow are annotations

                return Ok(AcceptorState::NotAcceptedComplete);
            }
            _ => {
                return Ok(AcceptorState::NotAccepted);
            }
        }
    }
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

    // #[test]
    // fn declaration_with_annotation() {
    //     compare("declaration-with-annotation");
    // }

    #[test]
    fn class_with_two_annotations() {
        compare("class_with_two_annotations");
    }

    #[test]
    fn one_ont() {
        compare("one-ont");
    }

    // #[test]
    // fn round_one_ont_prefix() {
    //     let (_ont_orig, prefix_orig, _ont_round, prefix_round) =
    //         roundtrip(include_str!("../ont/owl-xml/one-ont.owx"));

    //     let prefix_orig_map: HashMap<&String, &String> = prefix_orig.mappings().collect();

    //     let prefix_round_map: HashMap<&String, &String> = prefix_round.mappings().collect();

    //     assert_eq!(prefix_orig_map, prefix_round_map);
    //}

    #[test]
    fn one_subclass() {
        compare("one-subclass");
    }

    //#[test]
    //fn one_some() {
    //    compare("one-some");
    //}

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

    #[test]
    fn one_comment() {
        // This is currently failing because the XML parser gives the
        // comment a language and a datatype ("PlainLiteral") while
        // the RDF one gives it just the language, as literals can't
        // be both. Which is correct?
        compare("one-comment");
    }

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
    //     compare("datatype");
    // }

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
    //     compare("data-some");
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
