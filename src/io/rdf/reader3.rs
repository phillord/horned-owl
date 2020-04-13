#![allow(unused_imports)]
use AcceptState::*;
use Term::*;

use curie::PrefixMapping;

use crate::index::update_logically_equal_axiom;
use crate::model::Literal;
use crate::model::*;
use crate::vocab::WithIRI;
use crate::vocab::OWL as VOWL;
use crate::vocab::RDF as VRDF;
use crate::vocab::RDFS as VRDFS;

use enum_meta::Meta;
use failure::Error;
use failure::SyncFailure;

use log::{debug, trace};

use sophia::term::BNodeId;
use sophia::term::IriData;
use sophia::term::LiteralKind;

use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::io::BufRead;
use std::rc::Rc;

// Two type aliases for "SoPhia" entities.
type SpTerm = sophia::term::Term<Rc<str>>;
type SpIri = IriData<Rc<str>>;
type SpBNode = BNodeId<Rc<str>>;

#[derive(Clone, Debug, Eq, PartialEq)]
enum Term {
    Iri(SpIri),
    BNode(SpBNode),
    Literal(Rc<str>, LiteralKind<Rc<str>>),
    Variable(Rc<str>),
    OWL(VOWL),
    RDF(VRDF),
    RDFS(VRDFS),
}

impl Term {
    fn ord(&self) -> isize {
        match self {
            OWL(_) => 1,
            RDF(_) => 2,
            RDFS(_) => 3,
            Iri(_) => 4,
            BNode(_) => 5,
            Literal(_, _) => 6,
            Variable(_) => 7,
        }
    }
}

// impl PartialEq for Term {
//     fn eq(&self, other: &Self) -> bool {
//         match
//     }
// }

impl Ord for Term {
    fn cmp(&self, other: &Term) -> Ordering {
        match (self, other) {
            (OWL(s), OWL(o)) => s.cmp(o),
            (RDF(s), RDF(o)) => s.cmp(o),
            (RDFS(s), RDFS(o)) => s.cmp(o),
            (Iri(s), Iri(o)) => s.to_string().cmp(&o.to_string()),
            (BNode(s), BNode(o)) => (*s).cmp(&(*o)),
            (Literal(s, _), Literal(o, _)) => s.cmp(o),
            (Variable(s), Variable(o)) => s.cmp(o),
            _ => self.ord().cmp(&other.ord()),
        }
    }
}

impl PartialOrd for Term {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Term {
    pub fn n3_maybe(&self) -> String {
        match self {
            Iri(_) | BNode(_) | Literal(_, _) | Variable(_) => self.n3(),
            OWL(v) => format!("{:?}", v),
            RDFS(v) => format!("{:?}", v),
            RDF(v) => format!("{:?}", v),
        }
    }

    pub fn n3(&self) -> String {
        match self {
            Iri(i) => sophia::term::Term::Iri(i.clone()).n3(),
            BNode(id) => sophia::term::Term::BNode(id.clone()).n3(),
            Literal(l, k) => sophia::term::Term::Literal(l.clone(), k.clone()).n3(),
            Variable(v) => sophia::term::Term::Variable(v.clone()).n3(),
            OWL(v) => vocab_to_term(v).n3(),
            RDFS(v) => vocab_to_term(v).n3(),
            RDF(v) => vocab_to_term(v).n3(),
        }
    }

    pub fn value(&self) -> String {
        match self {
            Iri(i) => sophia::term::Term::Iri(i.clone()).value(),
            BNode(id) => sophia::term::Term::BNode(id.clone()).value(),
            Literal(l, k) => sophia::term::Term::Literal(l.clone(), k.clone()).value(),
            Variable(v) => sophia::term::Term::Variable(v.clone()).value(),
            OWL(v) => vocab_to_term(v).value(),
            RDFS(v) => vocab_to_term(v).value(),
            RDF(v) => vocab_to_term(v).value(),
        }
    }
}

trait Convert {
    fn to_iri(&self, b: &Build) -> IRI;
}

impl Convert for SpIri {
    fn to_iri(&self, b: &Build) -> IRI {
        b.iri(self.to_string())
    }
}

trait TryBuild<N: From<IRI>> {
    fn to_some_iri(&self, b: &Build) -> Option<IRI>;

    fn to_iri_maybe(&self, b: &Build) -> Result<IRI, Error> {
        match self.to_some_iri(b) {
            Some(iri) => Ok(iri),
            None => todo!("Fix this"),
        }
    }

    fn try_build(&self, b: &Build) -> Result<N, Error> {
        Ok(self.to_iri_maybe(b)?.into())
    }
}

impl<N: From<IRI>> TryBuild<N> for Option<SpIri> {
    fn to_some_iri(&self, b: &Build) -> Option<IRI> {
        self.as_ref().map(|i| i.to_iri(b))
    }
}

impl<N: From<IRI>> TryBuild<N> for Term {
    fn to_some_iri(&self, b: &Build) -> Option<IRI> {
        match self {
            Term::Iri(spiri) => Some(spiri.to_iri(b)),
            _ => None,
        }
    }
}

fn vocab_to_term<'a, V: WithIRI<'a>>(v: &V) -> SpTerm {
    // unwrap should be safe for all known WithIRIs
    sophia::term::Term::new_iri(Rc::from(v.iri_str())).unwrap()
}

fn vocab_lookup() -> HashMap<SpTerm, Term> {
    let mut m = HashMap::default();

    for v in VOWL::all() {
        m.insert(vocab_to_term(&v), Term::OWL(v));
    }

    for v in VRDFS::all() {
        m.insert(vocab_to_term(&v), Term::RDFS(v));
    }

    for v in VRDF::all() {
        m.insert(vocab_to_term(&v), Term::RDF(v));
    }

    m
}

fn to_term(t: &SpTerm, m: &HashMap<SpTerm, Term>) -> Term {
    if let Some(t) = m.get(t) {
        t.clone()
    } else {
        match t {
            sophia::term::Term::Iri(i) => Iri(i.clone()),
            sophia::term::Term::BNode(id) => BNode(id.clone()),
            sophia::term::Term::Literal(l, k) => Literal(l.clone(), k.clone()),
            sophia::term::Term::Variable(v) => Variable(v.clone()),
        }
    }
}

#[derive(Debug)]
enum AcceptState<A> {
    // Accept and consume the triple
    Accept,

    // Accept and consume the triple and return the SpBNode
    AcceptOn(SpBNode),
    // Do not accept the triple and return it
    Return([Term; 3]),

    // Complete acceptance and return a result
    Complete(A),
}

trait Acceptor<A>: std::fmt::Debug {
    fn new(b: &Build, o: &Ontology) -> Self;

    #[must_use]
    fn accept(&mut self, triple: [Term; 3]) -> Result<AcceptState<A>, Error>;
}

struct OntologyParser<'a> {
    o: Ontology,
    b: &'a Build,
    bnode_triples: HashMap<SpBNode, Vec<[Term; 3]>>,
    simple_triples: Vec<[Term; 3]>,
}

impl<'a> OntologyParser<'a> {
    fn new(b: &'a Build) -> OntologyParser {
        OntologyParser {
            o: Ontology::default(),
            b,
            bnode_triples: HashMap::new(),
            simple_triples: vec![],
        }
    }

    fn group_triples(&mut self, triple: Vec<[Term; 3]>) {
        // Next group together triples on a BNode, so we have
        // HashMap<BNodeID, Vec<[SpTerm; 3]> All of which should be
        // triples should begin with the BNodeId. We should be able to
        // gather these in a single pass.
        for t in &triple {
            match t {
                [BNode(id), _, _] => {
                    let v = self
                        .bnode_triples
                        .entry(id.clone())
                        .or_insert_with(Vec::new);
                    v.push(t.clone())
                }
                _ => {
                    self.simple_triples.push(t.clone());
                }
            }
        }
    }

    fn stitch_seqs(&self) {
        // As described in Table 3 of RDF Graphs
        // Work backward BNode with RDF::rest RDF::Nil

        // There should be only one of these per list, although RDF
        // does not guarantee it strictly. It's easier to work
        // backward than forward. This way we should be able to pull
        // out the whole thing.
        //
        // Find all BNode with RDF::rest
        // Put those with RDF::Nil into vec
        // Put those with BNode into HashMap keyed on object "restmap"
        //
        // Find all BNode with RDF::first
        // Put those into HashMap keyed on subject: "firstmap"
        //
        // Create Used BNode vec

        // Now iterate through RDF::Nil, gives BNode subject
        // Create value vec
        // Lookup Subject of rest in firstmap find RDF::First node, this is our
        // list element add to value vec, add BNode to used BNode vec
        // Lookup Subject of rest in restmap, gives us BNode::Subject
        // and recurse if we find it

        // reverse vec, add to HashMap keyed on last value of BNode

        // First find all BNode with RDF::First as a term
        // Create vec and find Rest node.
        // Add to data structure like so
        //
        // SpBNode (first) -> SPNode (rest), Vec[Term] (or nil)
        //
        // Vec will be length one long
        //
        // Take first entry, lookup SPNode(rest), add it's vec to our
        // vec, and add back to the Map,
    }

    fn resolve_imports(&mut self) {
        // Section 3.1.2/table 4 of RDF Graphs
    }

    fn headers(&mut self) -> Result<(), Error> {
        // Section 3.1.2/table 4
        // *:x rdf:type owl:Ontology .
        // [ *:x owl:versionIRI *:y .]
        let mut iri: Option<SpIri> = None;
        let mut viri: Option<SpIri> = None;

        let simple_triples = ::std::mem::take(&mut self.simple_triples);
        let (_, remain): (Vec<[Term; 3]>, Vec<[Term; 3]>) =
            simple_triples.into_iter().partition(|n| match n {
                [Term::Iri(s), Term::RDF(VRDF::Type), Term::OWL(VOWL::Ontology)] => {
                    iri = Some(s.clone());
                    true
                }
                _ => false,
            });

        let (_, remain) = remain.into_iter().partition(|n| match n {
            [Term::Iri(s), Term::OWL(VOWL::VersionIRI), Term::Iri(ob)]
                if iri.as_ref() == Some(s) =>
            {
                viri = Some(ob.clone());
                true
            }
            _ => false,
        });

        self.o.id.iri = TryBuild::<IRI>::to_some_iri(&iri, &self.b);
        self.o.id.viri = TryBuild::<IRI>::to_some_iri(&viri, &self.b);

        self.simple_triples = remain;

        Ok(())
    }

    fn backward_compat(&mut self) {
        // Table 5, Table 6
    }

    fn annotations(&self, triples: &[[Term; 3]]) -> BTreeSet<Annotation> {
        let mut ann = BTreeSet::default();
        for a in triples {
            ann.insert(self.annotation(a));
        }
        ann
    }

    fn annotation(&self, t: &[Term; 3]) -> Annotation {
        match t {
            [_, Iri(p), Term::Literal(ob, kind)] => Annotation {
                ap: AnnotationProperty(p.to_iri(self.b)),
                av: match kind {
                    LiteralKind::Lang(lang) => Literal::Language {
                        lang: lang.clone().to_string(),
                        literal: ob.clone().to_string(),
                    }
                    .into(),
                    LiteralKind::Datatype(iri) => Literal::Datatype {
                        datatype_iri: iri.to_iri(self.b),
                        literal: ob.clone().to_string(),
                    }
                    .into(),
                },
            },
            [_, Iri(p), Iri(ob)] => {
                // IRI annotation value
                Annotation {
                    ap: AnnotationProperty(p.to_iri(self.b)),
                    av: ob.to_iri(self.b).into(),
                }
            }
            _ => todo!(),
        }
    }

    fn declarations(&mut self) {
        // Table 7
        let mut annotated_triples: Vec<([Term; 3], BTreeSet<Annotation>)> =
            ::std::mem::take(&mut self.simple_triples)
                .into_iter()
                .map(|t| (t, BTreeSet::new()))
                .collect();

        let bnode_triples = ::std::mem::take(&mut self.bnode_triples);
        let _: HashMap<SpBNode, Vec<[Term; 3]>> = bnode_triples
            .into_iter()
            .filter(|(_, v)| match v.as_slice() {
                [
                    [_, Term::OWL(VOWL::AnnotatedSource), sb @ Term::Iri(_)],
                    [_, Term::OWL(VOWL::AnnotatedProperty), p @ Term::RDF(VRDF::Type)],
                    [_, Term::OWL(VOWL::AnnotatedTarget), ob @ Term::OWL(_)],
                    [_, Term::RDF(VRDF::Type), Term::OWL(VOWL::Axiom)],
                    ann @ ..
                ] => {
                    annotated_triples.push(([sb.clone(), p.clone(), ob.clone()],
                                            self.annotations(ann)));
                    false
                }
                _ =>
                {
                    true
                }
            })
            .collect();

        let remain: Vec<[Term; 3]> = annotated_triples
            .into_iter()
            .filter(|n| match n {
                // TODO Change this into a single outer match
                ([Term::Iri(s), Term::RDF(VRDF::Type), entity], ann) => {
                    // TODO Move match into function
                    let entity = match entity {
                        Term::OWL(VOWL::Class) => Class(s.to_iri(self.b)).into(),
                        Term::OWL(VOWL::ObjectProperty) => ObjectProperty(s.to_iri(self.b)).into(),
                        Term::OWL(VOWL::AnnotationProperty) => {
                            AnnotationProperty(s.to_iri(self.b)).into()
                        }
                        Term::OWL(VOWL::DatatypeProperty) => DataProperty(s.to_iri(self.b)).into(),
                        Term::RDFS(VRDFS::Datatype) => Datatype(s.to_iri(self.b)).into(),
                        _ => {
                            return true;
                        }
                    };

                    let is_empty = ann.is_empty();
                    let ax = AnnotatedAxiom {
                        axiom: declaration(entity),
                        ann: ann.clone(),
                    };

                    if is_empty {
                        self.o.insert(ax);
                    } else {
                        update_logically_equal_axiom(&mut self.o, ax);
                    }

                    false
                }
                _ => true,
            })
            .map(|(t, _a)| t)
            .collect();

        self.simple_triples = remain;
    }

    fn axioms(&mut self) {
        // TODO match subclass axiom for the moment on a BNode.
        // We will need to have altered the BNode tiples at this
        // point, so taht they have ClassExpression at the ned

        let simple_triples = ::std::mem::take(&mut self.simple_triples);

        let remain = simple_triples
            .into_iter()
            .filter(|n| match n {
                [Term::Iri(sub), Term::RDFS(VRDFS::SubClassOf), Term::Iri(sup)] => {
                    self.o.insert(AnnotatedAxiom {
                        axiom: SubClassOf {
                            sub: Class(sub.to_iri(self.b)).into(),
                            sup: Class(sup.to_iri(self.b)).into(),
                        }
                        .into(),
                        ann: BTreeSet::new(),
                    })
                }
                _ => todo!(),
            })
            .collect();

        self.simple_triples = remain
    }

    fn read(mut self, triple: Vec<[SpTerm; 3]>) -> Result<Ontology, Error> {
        // move to our own Terms, with IRIs swapped
        let m = vocab_lookup();
        let triple: Vec<[Term; 3]> = triple
            .into_iter()
            .map(|t| [to_term(&t[0], &m), to_term(&t[1], &m), to_term(&t[2], &m)])
            .collect();

        self.group_triples(triple);

        // sort the triples, so that I can get a dependable order
        for (_, vec) in self.bnode_triples.iter_mut() {
            vec.sort();
        }

        self.stitch_seqs();
        self.resolve_imports();
        self.headers();
        self.backward_compat();

        self.declarations();

        // for t in bnode_triples.values() {
        //     match t.as_slice()[0] {
        //         [BNode(s), RDF(VRDF::First), ob] => {
        //             //let v = vec![];
        //             // So, we have captured first (value of which is ob)
        //             // Rest of the sequence could be either in
        //             // bnode_seq or in bnode_triples -- confusing
        //             //bnode_seq.insert(s.clone(), self.seq())
        //         }
        //     }
        // }

        // Then handle SEQ this should give HashMap<BNodeID,
        // Vec<[SpTerm]> where the BNodeID is the first node of the
        // seq, and the SpTerms are the next in order. This will
        // require multiple passes through the triples (This is Table
        // 3 in the structural Specification)

        // At this point we should have everything we need to be able
        // to make all the entities that we need, already grouped into
        // a place we can access it.

        // Now we work through the tables in the RDF serialization

        // Table 4: headers. To do this fully requires imports also,
        // but we need to fudge this a little. We need to be to able
        // to read an ontology just for declarations. At the moment, I
        // don't know how to get to another set of triples for these
        // -- we will need some kind of factory.

        // Can we pull out annotations at this point and handle them
        // as we do in reader2? Tranform them into a triple which we
        // handle normally, then bung the annotation on later?

        // Table 5: Backward compability -- skip this for now (maybe
        // for ever)

        // Table 6: Don't understand this

        // Table 7: Declarations (this should be simple, if we have a
        // generic solution for handling annotations, there is no
        // handling of bnodes).

        // Table 8:
        // We need two traits, one for dealing with iri, iri, *
        // triples, and another for dealing with IRIs.

        // Table 16: Axioms without annotations
        self.axioms();

        if self.simple_triples.len() > 0 {
            dbg!("simple remaining", self.simple_triples);
        }

        if self.bnode_triples.len() > 0 {
            dbg!("bnodes remaining", self.bnode_triples);
        }
        Ok(self.o)
    }
}

pub fn read_with_build<R: BufRead>(
    bufread: &mut R,
    build: &Build,
) -> Result<(Ontology, PrefixMapping), Error> {
    let parser = sophia::parser::xml::Config::default();
    let triple_iter = parser.parse_bufread(bufread);

    let triple_result: Result<Vec<_>, _> = triple_iter.collect();
    let triple_v: Vec<[SpTerm; 3]> = triple_result.map_err(SyncFailure::new)?;

    return OntologyParser::new(build)
        .read(triple_v)
        .map(|o| return (o, PrefixMapping::default()));
}

pub fn read<R: BufRead>(bufread: &mut R) -> Result<(Ontology, PrefixMapping), Error> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

#[cfg(test)]
mod test {
    use super::*;

    use std::io::Write;
    use std::path::PathBuf;

    use pretty_assertions::assert_eq;

    fn init_log() {
        let _ = env_logger::builder()
            .format(|buf, record| writeln!(buf, "{}", record.args()))
            .is_test(true)
            .try_init();
    }

    fn read_ok<R: BufRead>(bufread: &mut R) -> (Ontology, PrefixMapping) {
        init_log();

        let r = read(bufread);

        assert!(r.is_ok(), "Expected ontology, get failure: {:?}", r.err());
        r.unwrap()
    }

    fn compare(test: &str) {
        compare_two(test, test);
    }

    fn compare_two(testrdf: &str, testowl: &str) {
        let dir_path_buf = PathBuf::from(file!());
        let dir = dir_path_buf.parent().unwrap().to_string_lossy();

        compare_str(
            &slurp::read_all_to_string(format!("{}/../../ont/owl-rdf/{}.owl", dir, testrdf))
                .unwrap(),
            &slurp::read_all_to_string(format!("{}/../../ont/owl-xml/{}.owx", dir, testowl))
                .unwrap(),
        );
    }

    fn compare_str(rdfread: &str, xmlread: &str) {
        let (rdfont, _rdfmapping) = read_ok(&mut rdfread.as_bytes());
        let (xmlont, _xmlmapping) = crate::io::reader::test::read_ok(&mut xmlread.as_bytes());

        //dbg!(&rdfont); if true {panic!()};

        assert_eq!(rdfont, xmlont);

        //let rdfmapping: &HashMap<&String, &String> = &rdfmapping.mappings().collect();
        //let xmlmapping: &HashMap<&String, &String> = &xmlmapping.mappings().collect();

        //assert_eq!(rdfmapping, xmlmapping);
    }

    #[test]
    fn class() {
        compare("class");
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
    fn ont() {
        compare("ont");
    }

    #[test]
    fn one_subclass() {
        compare("one-subclass");
    }

    // #[test]
    // fn subclass_with_annotation() {
    //     compare("annotation-on-subclass");
    // }

    #[test]
    fn oproperty() {
        compare("oproperty");
    }

    // #[test]
    // fn some() {
    //     compare("some");
    // }

    // #[test]
    // fn one_some_reversed() {
    //     compare_two("manual/one-some-reversed-triples", "some");
    // }

    // #[test]
    // fn one_some_property_filler_reversed() {
    //     compare_two("manual/one-some-property-filler-reversed", "some");
    // }

    // #[test]
    // fn only() {
    //     compare("only");
    //}

    // #[test]
    // fn and() {
    //     compare("and");
    // }

    // #[test]
    // fn or() {
    //     compare("or");
    // }

    // #[test]
    // fn not() {
    //     compare("not");
    // }

    #[test]
    fn annotation_property() {
        compare("annotation-property");
    }

    // #[test]
    // fn annotation() {
    //     compare("annotation");
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
    // fn label() {
    //     compare("label");
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
    //    compare("disjoint-union");
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
    // fn inverse_transitive() {
    //     compare("inverse-transitive")
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

    //#[test]
    //fn annotation_on_annotation() {
    //    compare("annotation-with-annotation");
    //}

    // #[test]
    // fn sub_annotation() {
    //     compare("sub-annotation");
    // }

    #[test]
    fn data_property() {
        compare("data-property");
    }

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

    #[test]
    fn datatype() {
        compare("datatype");
    }

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
    //   compare("data-some");
    // }

    // #[test]
    // fn facet_restriction() {
    //     compare("facet-restriction");
    // }

    // #[test]
    // fn data_only() {
    //    compare("data-only");
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
    //    compare("object-property-asymmetric");
    // }

    // #[test]
    // fn object_property_domain() {
    //    compare("object-property-domain");
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
    //    compare("object-property-range");
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
