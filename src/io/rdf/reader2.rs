use AcceptState::*;

// Required because of name clash with sophia::term::Term
use crate::model::Literal;
use crate::model::*;
use crate::vocab::*;

use crate::index::find_declaration_kind;
use crate::index::update_logically_equal_axiom;

use curie::PrefixMapping;

use failure::Error;

use log::{debug, trace};

use sophia::term::BNodeId;
use sophia::term::IriData;
use sophia::term::LiteralKind;
use sophia::term::Term;
use sophia::term::Term::*;

use std::fmt::Debug;
use std::io::BufRead;
use std::rc::Rc;

// Two type aliases for "SoPhia" entities.
type SpTerm = Term<Rc<str>>;
type SpIri = IriData<Rc<str>>;
type SpBNode = BNodeId<Rc<str>>;

#[derive(Debug)]
enum AcceptState {
    // Accept and consume the triple
    Accept,

    // Do not accept the triple and return it
    Return([SpTerm; 3]),
}

trait Acceptor<O>: std::fmt::Debug {
    // Accept a triple.
    //
    // The ontology which is accepting the triple and its import
    // closure is also passed. This is in the currently known state;
    // both the ontologies and the number of ontologies may change
    // over time.
    #[must_use]
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error>;

    // Indicate the completion state of the acceptor.

    // If true, the acceptor has all the information that it needs to
    // complete, and will not accept further triples. If false, the
    // acceptor may still accept triples.
    fn is_complete(&self) -> bool;

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

impl<N: From<IRI>> TryBuild<N> for SpTerm {
    fn to_some_iri(&self, b: &Build) -> Option<IRI> {
        match self {
            Term::Iri(spiri) => Some(spiri.to_iri(b)),
            _ => None,
        }
    }
}

impl<A, B> Acceptor<B> for Option<A>
where
    A: Acceptor<B>,
{
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match self.as_mut() {
            Some(ac) => ac.accept(b, triple, o),
            None => retn(triple, "Option"),
        }
    }

    fn is_complete(&self) -> bool {
        self.as_ref().map_or(false, |ac| ac.is_complete())
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<B, Error> {
        match self.as_mut() {
            Some(ac) => ac.complete(b, o),
            None => todo!(),
        }
    }
}

impl<A, B> Acceptor<B> for Box<A>
where
    A: Acceptor<B>,
{
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        (**self).accept(b, triple, o)
    }

    fn is_complete(&self) -> bool {
        (**self).is_complete()
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<B, Error> {
        (**self).complete(b, o)
    }
}

macro_rules! all_some {
    ($name:expr) => {
        $name.is_some()
    };
    ($name:expr, $($names:expr),*) => {
        all_some!($name) && all_some!($($names),*)
    };
}

fn accept(log: &str) -> Result<AcceptState, Error> {
    debug!("Accept: {}", log);
    Ok(Accept)
}

fn retn(triple: [SpTerm; 3], log: &str) -> Result<AcceptState, Error> {
    trace!("Return: {}", log);
    Ok(Return(triple))
}

#[allow(dead_code)]
fn p_tup(triple: &[SpTerm; 3]) {
    debug!(
        "{}\n\t{}\n\t{}",
        &triple[0].n3(),
        &triple[1].n3(),
        &triple[2].n3()
    );
}

#[derive(Debug)]
struct OntologyParser {
    // Acceptors which are NotComplete or CanComplete
    incomplete_acceptors: Vec<Box<dyn Acceptor<(AnnotatedAxiom, Merge)>>>,

    // Acceptors which are Complete
    complete_acceptors: Vec<Box<dyn Acceptor<(AnnotatedAxiom, Merge)>>>,

    iri: Option<SpIri>,
    viri: Option<SpIri>,

    // The ontology we are currently parsing. We need an Option here
    // because we are going to take the Ontology later.
    so: Option<Ontology>,
}

impl Default for OntologyParser {
    fn default() -> OntologyParser {
        OntologyParser {
            so: Some(Ontology::default()),
            incomplete_acceptors: vec![],
            complete_acceptors: vec![],
            iri: None,
            viri: None,
        }
    }
}

impl OntologyParser {
    fn accept(&mut self, b: &Build, mut triple: [SpTerm; 3]) -> Result<AcceptState, Error> {
        match self.so.as_mut() {
            None => retn(triple, "Ontology Acceptor"),
            Some(o) => {
                match &triple {
                    [Term::Iri(s), Term::Iri(p), Term::Iri(ob)]
                        if p == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
                            && ob == &OWL::Ontology.iri_str() =>
                    {
                        self.iri = Some(s.clone());
                        o.id.iri = TryBuild::<IRI>::to_some_iri(&self.iri, b);
                        accept("Ontology Acceptor")
                    }
                    [Term::Iri(s), Term::Iri(p), Term::Iri(ob)]
                        if self.iri.as_ref() == Some(s) && p == &OWL::VersionIRI.iri_str() =>
                    {
                        self.viri = Some(ob.clone());
                        accept("Ontology Acceptor")
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
                            let acr = ac.accept(b, triple, &o)?;
                            match acr {
                                Accept => {
                                    if ac.is_complete() {
                                        let act = &mut self.incomplete_acceptors.remove(i);
                                        self.complete_acceptor(b, act)?;
                                    }
                                    return Ok(acr);
                                }
                                Return(t) => {
                                    triple = t;
                                }
                            }
                        }

                        // Iterate through new acceptor
                        let acceptors: Vec<Box<dyn Acceptor<(AnnotatedAxiom, Merge)>>> = vec![
                            Box::new(SimpleAnnotatedAxiomAcceptor::default()),
                            Box::new(AnnotatedAxiomAcceptor::default()),
                        ];

                        for mut ac in acceptors {
                            let acr = ac.accept(b, triple, &o)?;
                            match acr {
                                Accept => {
                                    if ac.is_complete() {
                                        self.complete_acceptor(b, &mut ac)?;
                                    } else {
                                        self.incomplete_acceptors.push(ac);
                                    }

                                    return Ok(acr);
                                }
                                Return(t) => {
                                    triple = t;
                                }
                            }
                        }

                        retn(triple, "Ontology Acceptor")
                    }
                }
            }
        }
    }

    fn complete_acceptor(
        &mut self,
        b: &Build,
        ac: &mut Box<dyn Acceptor<(AnnotatedAxiom, Merge)>>,
    ) -> Result<(), Error> {
        match self.so.as_mut() {
            None => todo!(),
            Some(mut o) => {
                let mut c = ac.complete(b, &o)?;

                // Ontology Annotations are a pain to find, so we just
                // check them at the end.
                if let Axiom::AnnotationAssertion(AnnotationAssertion { subject, ann }) = &c.0.axiom
                {
                    if o.id.iri == Some(subject.clone()) {
                        c = (OntologyAnnotation(ann.clone()).into(), c.1)
                    }
                }

                if let Merge::NeedsMerge = c.1 {
                    update_logically_equal_axiom(&mut o, c.0);
                } else {
                    o.insert(c.0);
                }
                Ok(())
            }
        }
    }

    fn complete(&mut self, b: &Build) -> Result<Ontology, Error> {
        match self.so.as_mut() {
            None => todo!(),
            Some(mut o) => {
                // Iterate over all the complete Acceptor, run complete on
                // them, and insert this
                o.id.viri = TryBuild::<IRI>::to_some_iri(&self.viri, b);

                let mut v = std::mem::take(&mut self.incomplete_acceptors);
                for mut ac in v.iter_mut() {
                    self.complete_acceptor(b, &mut ac)?;
                }

                // dbg!(&o); if true {panic!()}
                return Ok(self.so.take().unwrap());
            }
        }
    }

    fn read_then_complete_1(
        &mut self,
        triple_iter: impl Iterator<Item = Result<[SpTerm; 3], sophia::error::Error>>,
        b: &Build,
    ) -> Vec<Result<[SpTerm; 3], sophia::error::Error>> {
        let mut not_accepted_triples = vec![];
        let iter = triple_iter;

        for t in iter {
            let t = t.unwrap();
            debug!(
                "\nChecking: {}\n\t\t{}\n\t\t{}",
                &t[0].n3(),
                &t[1].n3(),
                &t[2].n3()
            );

            // Check return type of this and do something sensible
            match self.accept(b, t) {
                Ok(Accept) => {}
                Ok(Return(triple)) => {
                    let x = triple;
                    not_accepted_triples.push(Ok(x));
                }
                _ => todo!(),
            }
        }

        not_accepted_triples
    }

    fn read_then_complete(
        &mut self,
        triple_iter: impl Iterator<Item = Result<[Term<Rc<str>>; 3], sophia::error::Error>>,
        b: &Build,
    ) -> Result<Ontology, Error> {
        debug!("First read_then_complete");
        let mut not_accepted = self.read_then_complete_1(triple_iter, b);
        let mut last_len = std::usize::MAX;
        //dbg!("not accepted", &not_accepted);

        let mut i = 0;
        while not_accepted.len() < last_len && not_accepted.len() > 0 {
            // I thought this would have the same type as triple_iter, but
            // it doesn't. Perhaps do a collect on triple_iter to vec,
            // then it should be easier.
            debug!("read_then_complete: {}", i);
            i = i + 1;
            last_len = not_accepted.len();
            not_accepted = self.read_then_complete_1(not_accepted.into_iter(), b);
            debug!("not accepted:{}: last_len:{}", not_accepted.len(), last_len);
        }

        if not_accepted.len() > 0 {
            debug!("Read Complete with unaccepted triples");
        } else {
            debug!("Read Complete");
        }

        self.complete(b)
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
    ac: Option<Box<dyn Acceptor<AnnotatedAxiom>>>,
}

impl Acceptor<(AnnotatedAxiom, Merge)> for SimpleAnnotatedAxiomAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        mut triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &mut self.ac {
            None => {
                // Try all the possibilities till we find the first which
                // accepts. Or currently, just fake it
                let acceptors: Vec<Box<dyn Acceptor<AnnotatedAxiom>>> = vec![
                    Box::new(DeclarationAcceptor::default()),
                    Box::new(TwoClassAcceptor::default()),
                    Box::new(PropertyAxiomAcceptor::default()),
                    // Needs to go last, because it slurps up
                    // triples at top-level
                    Box::new(AnnotationAssertionAcceptor::default()),
                ];

                for mut ac in acceptors {
                    let acr = ac.accept(b, triple, o)?;
                    match acr {
                        Accept => {
                            self.ac = Some(ac);
                            return accept("SimpleAnnotatedAxiom");
                        }
                        Return(t) => triple = t,
                    }
                }
                retn(triple, "SimpleAnnotatedAxiomAcceptor")
            }
            Some(acceptor) => acceptor.accept(b, triple, o),
        }
    }

    fn is_complete(&self) -> bool {
        match &self.ac {
            Some(a) => a.is_complete(),
            None => false,
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<(AnnotatedAxiom, Merge), Error> {
        match &mut self.ac {
            Some(boxacceptor) => Ok((boxacceptor.complete(b, o)?, Merge::DoesNotNeedMerge)),
            None => todo!(),
        }
    }
}

// Accept reified annotations of an axiom
#[derive(Debug, Default)]
struct AnnotatedAxiomAcceptor {
    bnodeid: Option<SpBNode>,
    // Currently we just support a single axiom type
    annotated_source: Option<SpTerm>,
    annotated_property: Option<SpTerm>,
    annotated_target: Option<SpTerm>,
    annotations: Vec<AnnotationAcceptor>,
    complete: bool,
}

impl Acceptor<(AnnotatedAxiom, Merge)> for AnnotatedAxiomAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &triple {
            // This should only happen when bnodeid is None
            [BNode(s), Iri(p), Iri(ob)]
                if p == &RDF::Type.iri_str() && ob == &OWL::Axiom.iri_str() =>
            {
                self.bnodeid = Some(s.clone());
                accept("AnnotatedAxiom")
            }
            [BNode(s), Iri(p), Iri(_ob)] if Some(s) == self.bnodeid.as_ref() => {
                match p {
                    _ if p == &OWL::AnnotatedSource.iri_str() => {
                        self.annotated_source = Some(triple[2].clone());
                        accept("AnnotatedAxiom")
                    }
                    _ if p == &OWL::AnnotatedProperty.iri_str() => {
                        self.annotated_property = Some(triple[2].clone());
                        accept("AnnotatedAxiom")
                    }
                    _ if p == &OWL::AnnotatedTarget.iri_str() => {
                        self.annotated_target = Some(triple[2].clone());
                        accept("AnnotatedAxiom")
                    }
                    _ => {
                        // This needs to be passed on to an annotation
                        // acceptor
                        retn(triple, "AnnotatedAxiomAcceptor")
                    }
                }
            }
            [BNode(s), _, _] if Some(s) == self.bnodeid.as_ref() => {
                let mut annac = AnnotationAcceptor::default();
                let rtn = annac.accept(b, triple, o);

                if annac.is_complete() {
                    self.annotations.push(annac);
                }
                rtn
            }
            _ if self.bnodeid.is_some() && !self.complete => {
                self.complete = true;
                retn(triple, "AnnotatedAxiomAcceptor")
            }
            _ => retn(triple, "AnnotatedAxiomAcceptor"),
        }
    }

    fn is_complete(&self) -> bool {
        self.complete
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<(AnnotatedAxiom, Merge), Error> {
        // Convert the reified triple into a normal one, and then pass
        // it to SimpleAnnotatedAxiomAcceptor to get the normal
        // axiom.
        let mut simple_acceptor = SimpleAnnotatedAxiomAcceptor::default();

        let _ = simple_acceptor.accept(
            b,
            [
                self.annotated_source.take().unwrap(),
                self.annotated_property.take().unwrap(),
                self.annotated_target.take().unwrap(),
            ],
            o,
        );

        let mut ann_axiom = simple_acceptor.complete(b, o)?.0;
        for i in &mut self.annotations {
            ann_axiom.ann.insert(i.complete(b, o)?);
        }

        Ok((ann_axiom, Merge::NeedsMerge))
    }
}

// Accept axioms that declare the type of an entity
#[derive(Debug, Default)]
struct DeclarationAcceptor {
    iri: Option<SpIri>,
    kind: Option<SpIri>,
}

impl Acceptor<AnnotatedAxiom> for DeclarationAcceptor {
    fn accept(
        &mut self,
        _b: &Build,
        triple: [SpTerm; 3],
        _o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &triple {
            [Iri(s), Iri(p), Iri(ob)]
                if p == &"http://www.w3.org/1999/02/22-rdf-syntax-ns#type" =>
            {
                self.iri = Some(s.clone());
                self.kind = Some(ob.clone());
                accept("DeclarationAcceptor")
            }
            _ => retn(triple, "DeclarationAcceptor"),
        }
    }

    fn is_complete(&self) -> bool {
        self.iri.is_some()
    }

    fn complete(&mut self, b: &Build, _o: &Ontology) -> Result<AnnotatedAxiom, Error> {
        // Iterate over all the complete Acceptor, run complete on
        // them, and insert this
        let n: NamedEntity = match &self.kind {
            Some(i) if i == &OWL::Class.iri_str() => {
                TryBuild::<Class>::try_build(&self.iri, b)?.into()
            }
            Some(i) if i == &OWL::ObjectProperty.iri_str() => {
                TryBuild::<ObjectProperty>::try_build(&self.iri, b)?.into()
            }
            Some(i) if i == &OWL::DatatypeProperty.iri_str() => {
                TryBuild::<DataProperty>::try_build(&self.iri, b)?.into()
            }
            Some(i) if i == &OWL::Datatype.iri_str() => {
                TryBuild::<Datatype>::try_build(&self.iri, b)?.into()
            }
            Some(i) if i == &OWL::AnnotationProperty.iri_str() => {
                TryBuild::<AnnotationProperty>::try_build(&self.iri, b)?.into()
            }
            Some(_) => todo!(),
            None => {
                todo!();
            }
        };

        Ok(declaration(n).into())
    }
}

#[derive(Debug)]
enum TypedPropertyAcceptor {
    Immediate(Axiom),
}

impl Acceptor<AnnotatedAxiom> for TypedPropertyAcceptor {
    fn accept(
        &mut self,
        _b: &Build,
        triple: [SpTerm; 3],
        _o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &self {
            Self::Immediate(_) => retn(triple, "TypedProperty: Immediate"),
        }
    }
    fn is_complete(&self) -> bool {
        match &self {
            Self::Immediate(_) => true,
        }
    }
    fn complete(&mut self, _b: &Build, _o: &Ontology) -> Result<AnnotatedAxiom, Error> {
        match self {
            Self::Immediate(ax) => Ok(ax.clone().into()),
        }
    }
}

#[derive(Debug, Default)]
struct PropertyAxiomAcceptor {
    tpa: Option<TypedPropertyAcceptor>,
}

impl Acceptor<AnnotatedAxiom> for PropertyAxiomAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        if self.tpa.is_some() {
            return self.tpa.accept(b, triple, o);
        }

        match &triple {
            [Iri(s), Iri(p), Iri(ob)] => {
                let fdk = |p: &SpIri| -> Result<Option<NamedEntityKind>, Error> {
                    let iri = p.to_iri(b);
                    Ok(find_declaration_kind(o, iri))
                };

                match p {
                    i if i == &RDFS::Domain.iri_str() => match fdk(s)? {
                        Some(NamedEntityKind::AnnotationProperty) => {
                            self.tpa = Some(TypedPropertyAcceptor::Immediate(
                                AnnotationPropertyDomain {
                                    ap: s.to_iri(b).into(),
                                    iri: ob.to_iri(b),
                                }
                                .into(),
                            ));

                            accept("PropertyAxiom")
                        }
                        _ => retn(triple, "PropertyAxiom: 3.1"),
                    },

                    i if i == &RDFS::Range.iri_str() => match fdk(s)? {
                        Some(NamedEntityKind::AnnotationProperty) => {
                            self.tpa = Some(TypedPropertyAcceptor::Immediate(
                                AnnotationPropertyRange {
                                    ap: s.to_iri(b).into(),
                                    iri: ob.to_iri(b),
                                }
                                .into(),
                            ));

                            accept("PropertyAxiom")
                        }
                        _ => retn(triple, "PropertyAxiom: 3.2"),
                    },
                    _ => retn(triple, "PropertyAxiom: 2"),
                }
            }
            _ => retn(triple, "PropertyAxiom: 1"),
        }
    }

    fn is_complete(&self) -> bool {
        self.tpa.is_complete()
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<AnnotatedAxiom, Error> {
        self.tpa.complete(b, o)
    }
}

#[derive(Debug)]
struct SeqAcceptor<A> {
    end_of_seq: SpBNode,
    seq: Vec<A>,
    complete: bool,
}

impl<A> SeqAcceptor<A> {
    fn from_bnode(bnode: SpBNode) -> SeqAcceptor<A> {
        SeqAcceptor {
            end_of_seq: bnode,
            seq: vec![],
            complete: false,
        }
    }
}

impl<A, E> Acceptor<Vec<E>> for SeqAcceptor<A>
where
    A: Debug + From<SpIri> + From<SpBNode> + Acceptor<E>,
    E: Debug,
{
    fn accept(
        &mut self,
        _b: &Build,
        triple: [SpTerm; 3],
        _o: &Ontology,
    ) -> Result<AcceptState, Error> {
        if self.complete {
            return retn(triple, "Seq");
        }

        match &triple {
            // Next Item
            [BNode(s), Iri(p), ob] if s == &self.end_of_seq && p == &RDF::First.iri_str() => {
                self.seq.push(match ob {
                    Iri(iri) => iri.clone().into(),
                    BNode(id) => id.clone().into(),
                    _ => todo!(),
                });

                accept("Seq")
            }
            // Pointer to Rest
            [BNode(s), Iri(p), BNode(ob)] if s == &self.end_of_seq && p == &RDF::Rest.iri_str() => {
                // We make the assumption here that we get first, then
                // rest, then first, then rest. Otherwize, we have
                // over-written the value of this bnode and we may not
                // have got it's equivalent rest. If we cannot rely
                // on that, we need a list of "rest" bnodes, and
                // collect the "firsts" as we go.
                self.end_of_seq = ob.clone();
                accept("Seq")
            }
            // End of Seq
            [BNode(s), Iri(p), Iri(ob)]
                if s == &self.end_of_seq
                    && p == &RDF::Rest.iri_str()
                    && ob == &RDF::Nil.iri_str() =>
            {
                self.complete = true;
                accept("Seq")
            }
            _ => todo!(),
        }
    }

    fn is_complete(&self) -> bool {
        self.complete
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<Vec<E>, Error> {
        let op: Result<Vec<_>, _> = std::mem::take(&mut self.seq)
            .into_iter()
            .map(|mut op| op.complete(b, o))
            .collect();

        op
    }
}

#[derive(Debug)]
struct PropositionAcceptor {
    bnode: SpBNode,
    typed_acceptor: Option<Box<TypedPropositionAcceptor>>,
}

impl PropositionAcceptor {
    fn from_bnode(bnode: SpBNode) -> PropositionAcceptor {
        PropositionAcceptor {
            bnode,
            typed_acceptor: None,
        }
    }
}

impl Acceptor<ClassExpression> for PropositionAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &triple {
            [BNode(id), Iri(p), BNode(ob)]
                if id == &self.bnode && p == &OWL::IntersectionOf.iri_str()
                    || p == &OWL::UnionOf.iri_str() =>
            {
                self.typed_acceptor = Some(Box::new(TypedPropositionAcceptor::Nary(
                    NaryPropositionAcceptor::new(ob.clone(), p.clone()),
                )));
                accept("Proposition")
            }
            [BNode(id), Iri(p), ob] if id == &self.bnode && p == &OWL::ComplementOf.iri_str() => {
                self.typed_acceptor = Some(Box::new(TypedPropositionAcceptor::Unary(
                    UnaryPropositionAcceptor::new(ob.clone()),
                )));
                accept("Proposition")
            }
            _ => self.typed_acceptor.as_mut().unwrap().accept(b, triple, o),
        }
    }

    fn is_complete(&self) -> bool {
        self.typed_acceptor.is_complete()
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        self.typed_acceptor.complete(b, o)
    }
}

#[derive(Debug)]
enum TypedPropositionAcceptor {
    Nary(NaryPropositionAcceptor),
    Unary(UnaryPropositionAcceptor),
}

impl Acceptor<ClassExpression> for TypedPropositionAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match self {
            Self::Nary(ac) => ac.accept(b, triple, o),
            Self::Unary(ac) => ac.accept(b, triple, o),
        }
    }

    // Indicate the completion state of the acceptor.
    fn is_complete(&self) -> bool {
        match self {
            Self::Nary(ac) => ac.is_complete(),
            Self::Unary(ac) => ac.is_complete(),
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        match self {
            Self::Nary(ac) => ac.complete(b, o),
            Self::Unary(ac) => ac.complete(b, o),
        }
    }
}

#[derive(Debug)]
struct UnaryPropositionAcceptor {
    ac: ClassAcceptor,
}

impl UnaryPropositionAcceptor {
    fn new(term: SpTerm) -> UnaryPropositionAcceptor {
        UnaryPropositionAcceptor {
            ac: match term {
                Iri(iri) => ClassAcceptor::from_iri(iri),
                BNode(id) => ClassAcceptor::from_bnode(id),
                _ => todo!(),
            },
        }
    }
}

impl Acceptor<ClassExpression> for UnaryPropositionAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        self.ac.accept(b, triple, o)
    }

    fn is_complete(&self) -> bool {
        self.ac.is_complete()
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        Ok(ClassExpression::ObjectComplementOf(Box::new(
            self.ac.complete(b, o)?,
        )))
    }
}

#[derive(Debug)]
struct NaryPropositionAcceptor {
    kind: SpIri,
    operands: SeqAcceptor<ClassAcceptor>,
}

impl NaryPropositionAcceptor {
    fn new(bnode: SpBNode, kind: SpIri) -> NaryPropositionAcceptor {
        NaryPropositionAcceptor {
            operands: SeqAcceptor::from_bnode(bnode),
            kind,
        }
    }
}

impl Acceptor<ClassExpression> for NaryPropositionAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        if self.operands.is_complete() {
            return retn(triple, "NaryProposition");
        }

        self.operands.accept(b, triple, o)
    }

    fn is_complete(&self) -> bool {
        self.operands.is_complete()
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        match &self.kind {
            k if k == &OWL::IntersectionOf.iri_str() => Ok(ClassExpression::ObjectIntersectionOf(
                self.operands.complete(b, o)?,
            )),
            k if k == &OWL::UnionOf.iri_str() => Ok(ClassExpression::ObjectUnionOf(
                self.operands.complete(b, o)?,
            )),
            _ => {
                dbg!(&self);
                todo!()
            }
        }
    }
}

#[derive(Debug)]
struct ObjectRestriction {
    // The kind of restriction, typed as an SpIri
    kind: Option<SpIri>,
    // The object property which we must know at construction
    // time. This is actually an OPE, so this will get more complex
    ope: IRI,
    // Some, all, and the cardinalities have a class associated
    // with them
    ce: Option<Box<ClassAcceptor>>,
    // Cardinality has a number
    n: Option<u32>,
    // HasValue has an individual
    i: Option<SpIri>,
}

impl Acceptor<ClassExpression> for ObjectRestriction {
    fn accept(
        &mut self,
        _b: &Build,
        triple: [SpTerm; 3],
        _o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match triple {
            [BNode(_), Iri(p), ob]
                if p == OWL::SomeValuesFrom.iri_str() || p == OWL::AllValuesFrom.iri_str() =>
            {
                self.kind = Some(p);
                self.ce = Some(Box::new(ClassAcceptor::from_term(ob)));
                accept("TypedRestrictionAcceptor")
            }
            _ => retn(triple, "TypedRestrictionAcceptor"),
        }
    }

    fn is_complete(&self) -> bool {
        // Working out whether we are complete requires us to do
        // different things for all the restriction types. So fudge
        // it.
        self.ce.is_complete()
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        let ope = ObjectProperty(self.ope.clone());

        match &self.kind {
            Some(t) if t == &OWL::SomeValuesFrom.iri_str() => {
                Ok(ClassExpression::ObjectSomeValuesFrom {
                    ope: ope.into(),
                    bce: self.ce.complete(b, o)?.into(),
                })
            }
            Some(t) if t == &OWL::AllValuesFrom.iri_str() => {
                Ok(ClassExpression::ObjectAllValuesFrom {
                    ope: ope.into(),
                    bce: self.ce.complete(b, o)?.into(),
                })
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
struct DataRestriction {
    kind: Option<SpIri>,
    dp: IRI,
    dr: Option<SpTerm>,
}

impl Acceptor<ClassExpression> for DataRestriction {
    fn accept(
        &mut self,
        _b: &Build,
        triple: [SpTerm; 3],
        _o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match triple {
            [BNode(_), Iri(p), ob]
                if p == OWL::SomeValuesFrom.iri_str() || p == OWL::AllValuesFrom.iri_str() =>
            {
                self.kind = Some(p);
                self.dr = Some(ob);
                accept("DataRestriction")
            }
            _ => retn(triple, "DataRestriction"),
        }
    }

    fn is_complete(&self) -> bool {
        all_some!(self.kind, self.dr)
    }

    fn complete(&mut self, b: &Build, _o: &Ontology) -> Result<ClassExpression, Error> {
        let dp = DataProperty(self.dp.clone());
        //
        match &self.kind {
            _ => Ok(ClassExpression::DataSomeValuesFrom {
                dp: dp,
                dr: TryBuild::<Datatype>::try_build(&self.dr.take().unwrap(), b)?.into(),
            }),
        }
    }
}

#[derive(Debug)]
enum TypedRestrictionAcceptor {
    Object(ObjectRestriction),
    Data(DataRestriction),
}

impl Acceptor<ClassExpression> for TypedRestrictionAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match self {
            Self::Object(ref mut obj) => obj.accept(b, triple, o),
            Self::Data(ref mut data) => data.accept(b, triple, o),
        }
    }

    fn is_complete(&self) -> bool {
        match self {
            Self::Object(ref obj) => obj.is_complete(),
            Self::Data(ref data) => data.is_complete(),
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        match self {
            Self::Object(ref mut obj) => obj.complete(b, o),
            Self::Data(ref mut data) => data.complete(b, o),
        }
    }
}

#[derive(Debug)]
struct RestrictionAcceptor {
    bnode: Option<SpBNode>,
    typed_acceptor: Option<TypedRestrictionAcceptor>,
}

impl RestrictionAcceptor {
    fn from_bnode(bnode: SpBNode) -> RestrictionAcceptor {
        RestrictionAcceptor {
            bnode: Some(bnode),
            typed_acceptor: None,
        }
    }
}

impl Acceptor<ClassExpression> for RestrictionAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &triple {
            [BNode(id), _, _] if Some(id) == self.bnode.as_ref() => match &triple {
                [_, Iri(p), ob] if p == &OWL::OnProperty.iri_str() => {
                    let on_prop_iri = b.iri(ob.value());
                    match find_declaration_kind(o, on_prop_iri.clone()) {
                        Some(NamedEntityKind::ObjectProperty) => {
                            self.typed_acceptor =
                                Some(TypedRestrictionAcceptor::Object(ObjectRestriction {
                                    ope: on_prop_iri.clone(),
                                    ce: None,
                                    n: None,
                                    i: None,
                                    kind: None,
                                }));
                            accept("Restriction")
                        }
                        Some(NamedEntityKind::DataProperty) => {
                            self.typed_acceptor =
                                Some(TypedRestrictionAcceptor::Data(DataRestriction {
                                    dp: on_prop_iri.clone(),
                                    dr: None,
                                    kind: None,
                                }));
                            accept("Restriction")
                        }
                        _ => retn(triple, "Restriction"),
                    }
                }
                _ => self.typed_acceptor.accept(b, triple, o),
            },
            _ => retn(triple, "Restriction"),
        }
    }

    // Indicate the completion state of the acceptor.
    fn is_complete(&self) -> bool {
        self.typed_acceptor.is_complete()
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        self.typed_acceptor.complete(b, o)
    }
}

#[derive(Debug)]
enum ClassExpressionSubAcceptor {
    Restriction(RestrictionAcceptor),
    Proposition(PropositionAcceptor),
}

#[derive(Debug)]
struct ClassExpressionAcceptor {
    bnode: Option<SpBNode>,
    sub: Option<ClassExpressionSubAcceptor>,
}

impl ClassExpressionAcceptor {
    fn from_bnode(bnode: SpBNode) -> ClassExpressionAcceptor {
        ClassExpressionAcceptor {
            bnode: Some(bnode),
            sub: None,
        }
    }
}

impl Acceptor<ClassExpression> for ClassExpressionAcceptor {
    // Accept a triple.
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &triple {
            [BNode(id), Iri(p), Iri(ob)]
                if Some(id) == self.bnode.as_ref() && p == &RDF::Type.iri_str() =>
            {
                if ob == &OWL::Restriction.iri_str() {
                    self.sub = Some(ClassExpressionSubAcceptor::Restriction(
                        RestrictionAcceptor::from_bnode(id.clone()),
                    ));
                    return accept("ClassExpression");
                }

                if ob == &OWL::Class.iri_str() {
                    self.sub = Some(ClassExpressionSubAcceptor::Proposition(
                        PropositionAcceptor::from_bnode(id.clone()),
                    ));
                    return accept("ClassExpression");
                }

                todo!()
            }
            _ if self.sub.is_some() => match self.sub.as_mut().unwrap() {
                ClassExpressionSubAcceptor::Restriction(ac) => ac.accept(b, triple, o),
                ClassExpressionSubAcceptor::Proposition(ac) => ac.accept(b, triple, o),
            },
            _ => retn(triple, "ClassExpression"),
        }
    }

    // Indicate the completion state of the acceptor.
    fn is_complete(&self) -> bool {
        match self.sub.as_ref() {
            Some(ClassExpressionSubAcceptor::Restriction(ac)) => ac.is_complete(),
            Some(ClassExpressionSubAcceptor::Proposition(ac)) => ac.is_complete(),
            None => false,
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        match self.sub.as_mut() {
            Some(ClassExpressionSubAcceptor::Restriction(ac)) => ac.complete(b, o),
            Some(ClassExpressionSubAcceptor::Proposition(ac)) => ac.complete(b, o),
            None => todo!(),
        }
    }
}

#[derive(Debug)]
enum ClassAcceptor {
    Named(SpIri),
    Expression(ClassExpressionAcceptor),
}

impl ClassAcceptor {
    fn from_iri(class: SpIri) -> ClassAcceptor {
        ClassAcceptor::Named(class)
    }

    fn from_bnode(bnode: SpBNode) -> ClassAcceptor {
        ClassAcceptor::Expression(ClassExpressionAcceptor::from_bnode(bnode))
    }

    fn from_term(term: SpTerm) -> ClassAcceptor {
        match term {
            Iri(i) => ClassAcceptor::from_iri(i),
            BNode(id) => ClassAcceptor::from_bnode(id),
            _ => todo!(),
        }
    }
}

impl From<SpBNode> for ClassAcceptor {
    fn from(id: SpBNode) -> ClassAcceptor {
        ClassAcceptor::from_bnode(id)
    }
}

impl From<SpIri> for ClassAcceptor {
    fn from(class: SpIri) -> ClassAcceptor {
        ClassAcceptor::from_iri(class)
    }
}

impl Acceptor<ClassExpression> for ClassAcceptor {
    // Accept a triple.
    fn accept(
        &mut self,
        b: &Build,
        triple: [SpTerm; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match self {
            ClassAcceptor::Named(_) => retn(triple, "Class"),
            ClassAcceptor::Expression(ce) => ce.accept(b, triple, o),
        }
    }

    // Indicate the completion state of the acceptor.
    fn is_complete(&self) -> bool {
        match self {
            ClassAcceptor::Named(_) => false,
            ClassAcceptor::Expression(ce) => ce.is_complete(),
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<ClassExpression, Error> {
        match self {
            ClassAcceptor::Named(nc) => {
                let c: Class = nc.to_iri(b).into();
                Ok(c.into())
            }
            ClassAcceptor::Expression(ce) => ce.complete(b, o),
        }
    }
}

#[derive(Debug, Default)]
struct TwoClassAcceptor {
    kind: Option<AxiomKind>,
    from: Option<ClassAcceptor>,
    to: Option<ClassAcceptor>,
}

impl Acceptor<AnnotatedAxiom> for TwoClassAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [Term<Rc<str>>; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        // If we already have found the main triple, then check sub acceptors
        if self.from.is_some() && self.to.is_some() {
            match self.from.accept(b, triple, o)? {
                Accept => return Ok(Accept),
                Return(triple) => match self.to.accept(b, triple, o)? {
                    Accept => return Ok(Accept),
                    Return(triple) => return retn(triple, "TwoClass"),
                },
            };
        }

        if let [_, Iri(p), _] = &triple {
            // Check the kind of the triple and return if we cannot
            // handle it
            self.kind = Some(match 10 {
                _ if p == &RDFS::SubClassOf.iri_str() => AxiomKind::SubClassOf,
                _ if p == &OWL::EquivalentClass.iri_str() => AxiomKind::EquivalentClasses,
                _ if p == &OWL::DisjointWith.iri_str() => AxiomKind::DisjointClasses,
                _ => return retn(triple, "TwoClass"),
            });

            if let [Iri(s), _, _] = &triple {
                self.to = Some(s.clone().into());
            }

            if let [BNode(s), _, _] = &triple {
                self.to = Some(s.clone().into());
            }

            if let [_, _, Iri(ob)] = &triple {
                self.from = Some(ob.clone().into());
            }

            if let [_, _, BNode(ob)] = &triple {
                self.from = Some(ob.clone().into());
            }

            // We must have discovered the kind earlier, so return now
            return accept("TwoClass");
        }

        // We have some random triple that is nothing to do with us
        retn(triple, "TwoClass")
    }

    fn is_complete(&self) -> bool {
        if self.from.is_complete() && self.to.is_complete() {
            true
        } else {
            false
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<AnnotatedAxiom, Error> {
        match self.kind {
            Some(AxiomKind::SubClassOf) => Ok(SubClassOf {
                sub: self.to.complete(b, o)?,
                sup: self.from.complete(b, o)?,
            }
            .into()),
            Some(AxiomKind::EquivalentClasses) => {
                Ok(
                    EquivalentClasses(vec![self.from.complete(b, o)?, self.to.complete(b, o)?])
                        .into(),
                )
            }
            Some(AxiomKind::DisjointClasses) => {
                Ok(
                    DisjointClasses(vec![self.from.complete(b, o)?, self.to.complete(b, o)?])
                        .into(),
                )
            }
            _ => panic!(),
        }
    }
}

// #[derive(Debug, Default)]
// struct OneToManyAcceptor {}

// impl Acceptor<AnnotatedAxiom> for OneToManyAcceptor {
//     fn accept(
//         &mut self,
//         b: &Build,
//         triple: [Term<Rc<str>>; 3],
//         o: &Ontology,
//     ) -> Result<AcceptState, Error> {
//         todo!()
//         //match &triple {}
//     }

//     fn is_complete(&self) -> CompleteState {
//         todo!()
//     }

//     fn complete(&mut self, b: &Build, o: &Ontology) -> Result<AnnotatedAxiom, Error> {
//         todo!()
//     }
// }

#[derive(Debug, Default)]
struct AnnotationAssertionAcceptor {
    ac: AnnotationAcceptor,
    subject: Option<SpIri>,
}

impl Acceptor<AnnotatedAxiom> for AnnotationAssertionAcceptor {
    fn accept(
        &mut self,
        b: &Build,
        triple: [Term<Rc<str>>; 3],
        o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &triple {
            [Iri(s), _, _] => {
                self.subject = Some(s.clone());
                self.ac.accept(b, triple, o)
            }
            _ => Ok(Return(triple)),
        }
    }

    fn is_complete(&self) -> bool {
        if self.subject.is_some() {
            true
        } else {
            false
        }
    }

    fn complete(&mut self, b: &Build, o: &Ontology) -> Result<AnnotatedAxiom, Error> {
        Ok(AnnotationAssertion {
            subject: TryBuild::<IRI>::to_iri_maybe(&self.subject, b)?,
            //b.iri(self.subject.as_ref().unwrap().to_string()),
            ann: self.ac.complete(b, o)?,
        }
        .into())
    }
}

// Accept annotations
#[derive(Debug, Default)]
struct AnnotationAcceptor {
    p: Option<SpIri>,
    iri_val: Option<SpIri>,
    literal_val: Option<Rc<str>>,
    literal_lang: Option<Rc<str>>,
    literal_datatype: Option<SpIri>,
}

impl Acceptor<Annotation> for AnnotationAcceptor {
    fn accept(
        &mut self,
        _b: &Build,
        triple: [Term<Rc<str>>; 3],
        _o: &Ontology,
    ) -> Result<AcceptState, Error> {
        match &triple {
            [_, Iri(p), Term::Literal(ob, kind)] => {
                // Literal value
                self.p = Some(p.clone());
                self.literal_val = Some(ob.clone());
                match kind {
                    LiteralKind::Lang(lang) => {
                        self.literal_lang = Some(lang.clone());
                    }
                    LiteralKind::Datatype(iri) => self.literal_datatype = Some(iri.clone()),
                }
                accept("AnnotationAcceptor")
            }
            [_, Iri(p), Iri(ob)] => {
                // IRI annotation value
                self.p = Some(p.clone());
                self.iri_val = Some(ob.clone());
                accept("AnnotationAcceptor")
            }
            _ => todo!(),
        }
    }

    fn is_complete(&self) -> bool {
        if self.p.is_some() && self.iri_val.is_some() || self.literal_val.is_some() {
            true
        } else {
            false
        }
    }

    fn complete(&mut self, b: &Build, _o: &Ontology) -> Result<Annotation, Error> {
        Ok(Annotation {
            ap: TryBuild::<AnnotationProperty>::try_build(&self.p, b)?,
            av: if self.iri_val.is_some() {
                TryBuild::<IRI>::try_build(&self.iri_val, b)?.into()
            } else {
                match (&self.literal_lang, &self.literal_datatype) {
                    (Some(lang), None) => Literal::Language {
                        literal: self.literal_val.as_ref().unwrap().to_string(),
                        lang: lang.to_string(),
                    }
                    .into(),
                    (None, Some(data)) => Literal::Datatype {
                        literal: self.literal_val.as_ref().unwrap().to_string(),
                        datatype_iri: data.to_iri(b),
                    }
                    .into(),
                    _ => todo!("This shouldn't happen"),
                }
            },
        })
    }
}

pub fn read_with_build<R: BufRead>(
    bufread: &mut R,
    build: &Build,
) -> Result<(Ontology, PrefixMapping), Error> {
    let parser = sophia::parser::xml::Config::default();
    let triple_iter = parser.parse_bufread(bufread);

    return OntologyParser::default()
        .read_then_complete(triple_iter, build)
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
    fn one_class() {
        compare("one-class");
    }

    #[test]
    fn declaration_with_annotation() {
        compare("declaration-with-annotation");
    }

    #[test]
    fn class_with_two_annotations() {
        compare("class_with_two_annotations");
    }

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

    #[test]
    fn one_oproperty() {
        compare("one-oproperty");
    }

    #[test]
    fn one_some() {
        compare("one-some");
    }

    #[test]
    fn one_some_reversed() {
        compare_two("one-some-reversed-triples", "one-some");
    }

    #[test]
    fn one_some_property_filler_reversed() {
        compare_two("one-some-property-filler-reversed", "one-some");
    }

    #[test]
    fn one_only() {
        compare("one-only");
    }

    #[test]
    fn one_and() {
        compare("one-and");
    }

    #[test]
    fn one_or() {
        compare("one-or");
    }

    #[test]
    fn one_not() {
        compare("one-not");
    }

    #[test]
    fn one_annotation_property() {
        compare("one-annotation-property");
    }

    #[test]
    fn one_annotation() {
        compare("one-annotation");
    }

    #[test]
    fn annotation_domain() {
        compare("annotation-domain");
    }

    #[test]
    fn annotation_range() {
        compare("annotation-range");
    }

    #[test]
    fn one_label() {
        compare("one-label");
    }

    #[test]
    fn one_comment() {
        // This is currently failing because the XML parser gives the
        // comment a language and a datatype ("PlainLiteral") while
        // the RDF one gives it just the language, as literals can't
        // be both. Which is correct?
        compare("one-comment");
    }

    #[test]
    fn one_ontology_annotation() {
        compare("one-ontology-annotation");
    }

    #[test]
    fn one_equivalent_class() {
        compare("one-equivalent");
    }

    #[test]
    fn one_disjoint_class() {
        compare("one-disjoint");
    }

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

    #[test]
    fn datatype_some() {
        compare("data-some");
    }

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
