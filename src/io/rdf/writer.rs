use std::{collections::HashSet, io::Write, rc::Rc};

use failure::Error;
use rio_xml::chunked_formatter::{AsRefBlankNode, AsRefNamedNode, AsRefNamedOrBlankNode, AsRefTerm, AsRefTriple, ChunkedRdfXmlFormatterConfig, PrettyRdfXmlFormatter};
use crate::{model::*, ontology::axiom_mapped::AxiomMappedOntology, vocab::{OWL, RDF, RDFS, WithIRI, Vocab}};

pub fn write<W:Write>(
    write: &mut W,
    ont: &AxiomMappedOntology,
) -> Result<(), Error> {

    // Entirely unsatisfying to set this randomly here, but we can't
    // access ns our parser yet
    let mut p = indexmap::IndexMap::new();
    p.insert("http://www.w3.org/2002/07/owl#".to_string(),"owl".to_string());
    p.insert("http://www.w3.org/XML/1998/namespace".to_string(),"xml".to_string());
    p.insert("http://www.w3.org/2001/XMLSchema#".to_string(),"xsd".to_string());
    p.insert("http://www.w3.org/2000/01/rdf-schema#".to_string(), "rdfs".to_string());

    let mut f:PrettyRdfXmlFormatter<Rc<str>,_> = PrettyRdfXmlFormatter::new(
        write,
        ChunkedRdfXmlFormatterConfig::all().prefix(p)
    )?;
    let mut bng = NodeGenerator::default();
    ont.render(&mut f, &mut bng)?;
    f.finish()?;
    Ok(())
}

#[derive(Default)]
struct NodeGenerator {
    i: u64,
    b: HashSet<Rc<str>>
}


impl NodeGenerator {
    pub fn nn<V:Into<Vocab>>(&mut self, v:V) -> AsRefNamedNode<Rc<str>> {
        AsRefNamedNode::new(self.cache_rc(v)).into()
    }

    fn cache_rc<V:Into<Vocab>>(&mut self, v:V) -> Rc<str> {
        let voc:&str = v.into().iri_s();
        if let Some(rc) = self.b.get(voc) {
            return rc.clone();
        }

        let rc:Rc<str> = (*voc).into();
        self.b.insert(rc.clone());
        rc
    }

    pub fn bn(&mut self) -> AsRefNamedOrBlankNode<Rc<str>> {
        self.i += 1;
        AsRefNamedOrBlankNode::BlankNode(
            AsRefBlankNode{
                id: format!{"bn{}", self.i}.into()
            }
        )
    }
}

impl From<&IRI> for AsRefTerm<Rc<str>> {
    fn from(iri: &IRI) -> Self {
        AsRefNamedNode::new(iri.into()).into()
    }
}

impl From<&IRI> for AsRefNamedNode<Rc<str>> {
    fn from(iri: &IRI) -> Self {
        AsRefNamedNode::new(iri.into())
    }
}

impl From<&IRI> for AsRefNamedOrBlankNode<Rc<str>> {
    fn from(iri: &IRI) -> Self {
        let nn:AsRefNamedNode<Rc<str>> = iri.into();
        nn.into()
    }
}

trait Render {
    fn render<W:Write>(&self, f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                       ng: &mut NodeGenerator) ->
        Result<Option<AsRefNamedOrBlankNode<Rc<str>>>, Error>;

    fn render_to<W:Write>(&self, f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                          ng: &mut NodeGenerator) ->
        Result<AsRefNamedOrBlankNode<Rc<str>>, Error> {
            self.render(f, ng)?.ok_or_else
                (|| format_err!("Attempt to unpack an empty node"))
        }
}

/// The types in `Render` are too long to type.
macro_rules! render {
    ($type:ty, $self:ident, $f:ident, $ng:ident,
     $body:tt) => {
        impl Render for $type {
            fn render<W:Write>(& $self, $f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                               $ng: &mut NodeGenerator)
                               -> Result<Option<AsRefNamedOrBlankNode<Rc<str>>>, Error>
                $body
        }
    }
}

macro_rules! triples {
    ($f:ident) => {};
    ($f:ident, $sub:expr, $pred:expr, $ob:expr) => {
        $f.format(to_triple(
                 $sub, $pred, $ob
                ))?;
     };
    ($f:ident, $sub:expr, $pred:expr, $ob:expr, $($rest:expr),+) => {
        triples!($f, $sub, $pred, $ob);
        triples!($f, $($rest),*);
    }
}

macro_rules! render_triple {
    ($type:ty, $self:ident, $ng:ident, $sub:expr, $pred:expr, $ob:expr) => {
        render! {
            $type, $self, f, $ng,
            {
                triples!(f, $sub, $pred, $ob);
                Ok(None)
            }
        }
    }
}

fn to_triple<'a, NB, NN, T>(subject:NB, predicate:NN, object:T) -> AsRefTriple<Rc<str>>
where NB: Into<AsRefNamedOrBlankNode<Rc<str>>>,
      NN: Into<AsRefNamedNode<Rc<str>>>,
      T: Into<AsRefTerm<Rc<str>>>
{
    AsRefTriple{
        subject: subject.into(),
        predicate: predicate.into(),
        object: object.into(),
    }
}


impl<'a, T: Render> Render for Box<T> {
    fn render<W:Write>(&self, f:&mut PrettyRdfXmlFormatter<Rc<str>, W>,
                       bng: &mut NodeGenerator) ->
        Result<Option<AsRefNamedOrBlankNode<Rc<str>>>, Error> {
            (**self).render(f, bng)
        }
}

render! {
    &AxiomMappedOntology, self, f, ng,
    {
        if let Some(iri) = &self.id().iri {
            triples!(
                f,
                iri,
                ng.nn(RDF::Type),
                ng.nn(OWL::Ontology)
            );

            if let Some(viri) = &self.id().viri {
                triples!(
                    f,
                    iri,
                    ng.nn(OWL::VersionIRI),
                    viri
                );
            }

            let imp = self.i().import();
            for i in imp {
                triples!(
                    f,
                    iri,
                    ng.nn(OWL::Imports),
                    &i.0
                );
            }

            for ax in self.i().iter() {
                ax.render(f, ng)?;
            }
        }
        Ok(None)
    }
}

render! {
    AnnotatedAxiom, self, f, ng,
    {
        self.axiom.render(f, ng)
    }
}

render! {
    Axiom, self, f, ng,
    {
        match self {
            // We render imports earlier
            Axiom::Import(_ax) => Ok(None),
            //Axiom::OntologyAnnotation(ax) => ax.render(f, ng),
            Axiom::DeclareClass(ax) => ax.render(f, ng),
            Axiom::DeclareObjectProperty(ax) => ax.render(f, ng),
            Axiom::DeclareAnnotationProperty(ax) => ax.render(f, ng),
            Axiom::DeclareDataProperty(ax) => ax.render(f, ng),
            Axiom::DeclareNamedIndividual(ax) => ax.render(f, ng),
            Axiom::DeclareDatatype(ax) => ax.render(f, ng),
            Axiom::SubClassOf(ax) => ax.render(f, ng),
            // Axiom::EquivalentClasses(ax) => ax.render(f, ng),
            // Axiom::DisjointClasses(ax) => ax.render(f, ng),
            // Axiom::DisjointUnion(ax) => ax.render(f, ng),
            // Axiom::SubObjectPropertyOf(ax) => ax.render(f, ng),
            // Axiom::EquivalentObjectProperties(ax) => ax.render(f, ng),
            // Axiom::DisjointObjectProperties(ax) => ax.render(f, ng),
            // Axiom::InverseObjectProperties(ax) => ax.render(f, ng),
            // Axiom::ObjectPropertyDomain(ax) => ax.render(f, ng),
            // Axiom::ObjectPropertyRange(ax) => ax.render(f, ng),
            // Axiom::FunctionalObjectProperty(ax) => ax.render(f, ng),
            // Axiom::InverseFunctionalObjectProperty(ax) => ax.render(f, ng),
            // Axiom::ReflexiveObjectProperty(ax) => ax.render(f, ng),
            // Axiom::IrreflexiveObjectProperty(ax) => ax.render(f, ng),
            // Axiom::SymmetricObjectProperty(ax) => ax.render(f, ng),
            // Axiom::AsymmetricObjectProperty(ax) => ax.render(f, ng),
            // Axiom::TransitiveObjectProperty(ax) => ax.render(f, ng),
            // Axiom::SubDataPropertyOf(ax) => ax.render(f, ng),
            // Axiom::EquivalentDataProperties(ax) => ax.render(f, ng),
            // Axiom::DisjointDataProperties(ax) => ax.render(f, ng),
            // Axiom::DataPropertyDomain(ax) => ax.render(f, ng),
            // Axiom::DataPropertyRange(ax) => ax.render(f, ng),
            // Axiom::FunctionalDataProperty(ax) => ax.render(f, ng),
            // Axiom::DatatypeDefinition(ax) => ax.render(f, ng),
            // Axiom::HasKey(ax) => ax.render(f, ng),
            // Axiom::SameIndividual(ax) => ax.render(f, ng),
            // Axiom::DifferentIndividuals(ax) => ax.render(f, ng),
            // Axiom::ClassAssertion(ax) => ax.render(f, ng),
            // Axiom::ObjectPropertyAssertion(ax) => ax.render(f, ng),
            // Axiom::NegativeObjectPropertyAssertion(ax) => ax.render(f, ng),
            // Axiom::DataPropertyAssertion(ax) => ax.render(f, ng),
            // Axiom::NegativeDataPropertyAssertion(ax) => ax.render(f, ng),
            // Axiom::AnnotationAssertion(ax) => ax.render(f, ng),
            // Axiom::SubAnnotationPropertyOf(ax) => ax.render(f, ng),
            // Axiom::AnnotationPropertyDomain(ax) => ax.render(f, ng),
            // Axiom::AnnotationPropertyRange(ax) => ax.render(f,
            // ng)
            _ => todo!("TODO: {:?}", self)
        }
    }
}

render! {
    ClassExpression, self, f, ng,
    {
        Ok(
            match self {
                ClassExpression::Class(cl) => Some((&cl.0).into()),
                ClassExpression::ObjectSomeValuesFrom{ref bce, ref ope} => {
                    let bn = ng.bn();
                    let node_ce = bce.render_to(f, ng)?;
                    let node_ope = ope.render_to(f, ng)?;

                    triples!(
                        f,
                        bn.clone(), ng.nn(RDF::Type), ng.nn(OWL::Restriction),
                        bn.clone(), ng.nn(OWL::OnProperty), node_ope,
                        bn.clone(), ng.nn(OWL::SomeValuesFrom), node_ce
                    );

                    Some(bn)
                }
                _=> todo!("TODO: {:?}", self)
            }
        )
    }
}

render! {
    ObjectPropertyExpression, self, _f, _ng,
    {
        Ok(
            match self {
                ObjectPropertyExpression::ObjectProperty(op)
                    => Some((&op.0).into()),
                ObjectPropertyExpression::InverseObjectProperty(_op)
                    => todo!()
            }
        )
    }
}
render_triple! {
    DeclareClass, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::Class)
}

render_triple! {
    DeclareDatatype, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(RDFS::Datatype)
}

render_triple! {
    DeclareObjectProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::ObjectProperty)
}

render_triple! {
    DeclareDataProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::DatatypeProperty)
}

render_triple! {
    DeclareAnnotationProperty, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::AnnotationProperty)
}

render_triple! {
    DeclareNamedIndividual, self, ng,
    &self.0.0, ng.nn(RDF::Type), ng.nn(OWL::NamedIndividual)
}

render! {
    SubClassOf, self, f, ng,
    {
        let sub = self.sub.render_to(f, ng)?;
        let obj = self.sup.render_to(f, ng)?;

        triples!(f,
                 sub,
                 ng.nn(RDFS::SubClassOf),
                 obj
        );
        Ok(None)
    }
}


#[cfg(test)]
mod test {

    extern crate mktemp;

    use self::mktemp::Temp;
    use super::*;
    use crate::{model::Build, ontology::set::SetOntology};

    // use std::collections::HashMap;

    // use std::fs::File;
    use std::{fs::File, io::{BufRead, BufReader, BufWriter}};
    // use std::io::BufReader;
    // use std::io::BufWriter;

    fn read_ok<R: BufRead>(bufread: &mut R) -> SetOntology {
        let r = crate::io::rdf::reader::read(bufread);
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        let (o, incomplete) = r.ok().unwrap();

        assert!(incomplete.is_complete(), "Read Not Complete: {:#?}", incomplete);
        o.into()
    }

    #[test]
    fn test_ont_rt() {
        let mut ont = AxiomMappedOntology::default();
        let build = Build::new();

        let iri = build.iri("http://www.example.com/a".to_string());
        ont.mut_id().iri = Some(iri);
        let temp_file = Temp::new_file().unwrap();
        let file = File::create(&temp_file).ok().unwrap();
        write(&mut BufWriter::new(file), &ont).ok().unwrap();

        let file = File::open(&temp_file).ok().unwrap();
        let ont2 = read_ok(&mut BufReader::new(file));

        assert_eq!(ont.id().iri, ont2.id().iri);
    }

    fn roundtrip(
        ont: &str,
    ) -> (
        SetOntology,
        SetOntology,
    ) {
        let ont_orig = read_ok(&mut ont.as_bytes());
        let mut temp_file = Temp::new_file().unwrap();

        let file = File::create(&temp_file).ok().unwrap();
        let mut buf_writer = BufWriter::new(&file);

        let amo: AxiomMappedOntology = ont_orig.clone().into();
        write(&mut buf_writer, &amo)
            .ok()
            .unwrap();
        buf_writer.flush().ok();

        let file = File::open(&temp_file).ok().unwrap();
        let ont_round = read_ok(&mut BufReader::new(&file));

        temp_file.release();

        return (ont_orig, ont_round);
    }

    fn assert_round(
        ont: &str,
    ) -> (
        SetOntology,
        SetOntology,
    ) {
        let (ont_orig, ont_round) = roundtrip(ont);

        assert_eq!(ont_orig, ont_round);

        return (ont_orig, ont_round);
    }

    #[test]
    fn round_ont() {
        assert_round(include_str!("../../ont/owl-rdf/ont.owl"));
    }

    #[test]
    fn round_class() {
        assert_round(include_str!("../../ont/owl-rdf/class.owl"));
    }

    // #[test]
    // fn round_class_with_annotation() {
    //     let (ont_orig, _prefix_orig, ont_round, _prefix_round) = roundtrip(include_str!(
    //         "../../ont/owl-rdf/declaration-with-annotation.owl"
    //     ));

    //     assert_eq!(ont_orig, ont_round);
    // }

    #[test]
    fn round_subclass() {
        assert_round(include_str!("../../ont/owl-rdf/subclass.owl"));
    }

    #[test]
    fn round_oproperty() {
        assert_round(include_str!("../../ont/owl-rdf/oproperty.owl"));
    }

    #[test]
    fn round_some() {
        assert_round(include_str!("../../ont/owl-rdf/some.owl"));
    }

    // #[test]
    // fn round_only() {
    //     assert_round(include_str!("../../ont/owl-rdf/only.owl"));
    // }

    // #[test]
    // fn round_and() {
    //     assert_round(include_str!("../../ont/owl-rdf/and.owl"));
    // }

    // #[test]
    // fn round_or() {
    //     assert_round(include_str!("../../ont/owl-rdf/or.owl"));
    // }

    // #[test]
    // fn round_not() {
    //     assert_round(include_str!("../../ont/owl-rdf/not.owl"));
    // }

    #[test]
    fn round_annotation_property() {
        assert_round(include_str!("../../ont/owl-rdf/annotation-property.owl"));
    }

    // #[test]
    // fn round_annotation() {
    //     assert_round(include_str!("../../ont/owl-rdf/annotation.owl"));
    // }

    // #[test]
    // fn round_annotation_domain() {
    //     assert_round(include_str!("../../ont/owl-rdf/annotation-domain.owl"));
    // }

    // #[test]
    // fn round_annotation_range() {
    //     assert_round(include_str!("../../ont/owl-rdf/annotation-range.owl"));
    // }

    // #[test]
    // fn round_label() {
    //     assert_round(include_str!("../../ont/owl-rdf/label.owl"));
    // }

    // #[test]
    // fn round_one_comment() {
    //     assert_round(include_str!("../../ont/owl-rdf/one-comment.owl"));
    // }

    // #[test]
    // fn round_one_ontology_annotation() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/one-ontology-annotation.owl"
    //     ));
    // }

    // #[test]
    // fn round_one_equivalent_class() {
    //     assert_round(include_str!("../../ont/owl-rdf/one-equivalent.owl"));
    // }

    // #[test]
    // fn round_one_disjoint_class() {
    //     assert_round(include_str!("../../ont/owl-rdf/one-disjoint.owl"));
    // }

    // #[test]
    // fn round_disjoint_union() {
    //     assert_round(include_str!("../../ont/owl-rdf/disjoint-union.owl"));
    // }

    // #[test]
    // fn round_one_sub_property() {
    //     assert_round(include_str!("../../ont/owl-rdf/one-suboproperty.owl"));
    // }

    // #[test]
    // fn round_one_inverse() {
    //     assert_round(include_str!("../../ont/owl-rdf/inverse-properties.owl"));
    // }

    // #[test]
    // fn round_one_transitive() {
    //     assert_round(include_str!("../../ont/owl-rdf/transitive-properties.owl"));
    // }

    // #[test]
    // fn round_one_annotated_transitive() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/annotation-on-transitive.owl"
    //     ));
    // }

    // #[test]
    // fn round_one_subproperty_chain() {
    //     assert_round(include_str!("../../ont/owl-rdf/subproperty-chain.owl"));
    // }

    // #[test]
    // fn round_one_subproperty_chain_with_inverse() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/subproperty-chain-with-inverse.owl"
    //     ));
    // }

    // #[test]
    // fn round_annotation_on_annotation() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/annotation-with-annotation.owl"
    //     ));
    // }

    // #[test]
    // fn round_sub_annotation() {
    //     assert_round(include_str!("../../ont/owl-rdf/sub-annotation.owl"));
    // }

    #[test]
    fn round_data_property() {
        assert_round(include_str!("../../ont/owl-rdf/data-property.owl"));
    }

    // #[test]
    // fn round_literal_escaped() {
    //     assert_round(include_str!("../../ont/owl-rdf/literal-escaped.owl"));
    // }

    #[test]
    fn round_named_individual() {
        assert_round(include_str!("../../ont/owl-rdf/named-individual.owl"));
    }

    #[test]
    fn round_import() {
        assert_round(include_str!("../../ont/owl-rdf/import.owl"));
    }

    #[test]
    fn datatype() {
        assert_round(include_str!("../../ont/owl-rdf/datatype.owl"));
    }

    // #[test]
    // fn object_has_value() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-has-value.owl"));
    // }

    // #[test]
    // fn object_one_of() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-one-of.owl"));
    // }

    // #[test]
    // fn inverse() {
    //     assert_round(include_str!("../../ont/owl-rdf/some-inverse.owl"));
    // }

    // #[test]
    // fn object_unqualified_cardinality() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-unqualified-max-cardinality.owl"
    //     ));
    // }

    // #[test]
    // fn object_min_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-min-cardinality.owl"));
    // }

    // #[test]
    // fn object_max_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-max-cardinality.owl"));
    // }

    // #[test]
    // fn object_exact_cardinality() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-exact-cardinality.owl"
    //     ));
    // }

    // #[test]
    // fn datatype_alias() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-alias.owl"));
    // }

    // #[test]
    // fn datatype_intersection() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-intersection.owl"));
    // }

    // #[test]
    // fn datatype_union() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-union.owl"));
    // }

    // #[test]
    // fn datatype_complement() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-complement.owl"));
    // }

    // #[test]
    // fn datatype_oneof() {
    //     assert_round(include_str!("../../ont/owl-rdf/datatype-oneof.owl"));
    // }

    // #[test]
    // fn datatype_some() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-some.owl"));
    // }

    // #[test]
    // fn facet_restriction() {
    //     assert_round(include_str!("../../ont/owl-rdf/facet-restriction.owl"));
    // }

    // #[test]
    // fn data_only() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-only.owl"));
    // }
    // #[test]
    // fn data_exact_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-exact-cardinality.owl"));
    // }

    // #[test]
    // fn data_has_value() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-has-value.owl"));
    // }

    // #[test]
    // fn data_max_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-max-cardinality.owl"));
    // }

    // #[test]
    // fn data_min_cardinality() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-min-cardinality.owl"));
    // }

    // #[test]
    // fn class_assertion() {
    //     assert_round(include_str!("../../ont/owl-rdf/class-assertion.owl"));
    // }

    // #[test]
    // fn data_property_assertion() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/data-property-assertion.owl"
    //     ));
    // }

    // #[test]
    // fn same_individual() {
    //     assert_round(include_str!("../../ont/owl-rdf/same-individual.owl"));
    // }

    // #[test]
    // fn different_individuals() {
    //     assert_round(include_str!("../../ont/owl-rdf/different-individual.owl"));
    // }

    // #[test]
    // fn negative_data_property_assertion() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/negative-data-property-assertion.owl"
    //     ));
    // }

    // #[test]
    // fn negative_object_property_assertion() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/negative-object-property-assertion.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_assertion() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-assertion.owl"
    //     ));
    // }

    // #[test]
    // fn data_has_key() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-has-key.owl"));
    // }

    // #[test]
    // fn data_property_disjoint() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-property-disjoint.owl"));
    // }

    // #[test]
    // fn data_property_domain() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-property-domain.owl"));
    // }

    // #[test]
    // fn data_property_equivalent() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/data-property-equivalent.owl"
    //     ));
    // }

    // #[test]
    // fn data_property_functional() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/data-property-functional.owl"
    //     ));
    // }

    // #[test]
    // fn data_property_range() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-property-range.owl"));
    // }

    // #[test]
    // fn data_property_sub() {
    //     assert_round(include_str!("../../ont/owl-rdf/data-property-sub.owl"));
    // }

    // #[test]
    // fn disjoint_object_properties() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/disjoint-object-properties.owl"
    //     ));
    // }

    // #[test]
    // fn equivalent_object_properties() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/equivalent-object-properties.owl"
    //     ));
    // }

    // #[test]
    // fn object_has_key() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-has-key.owl"));
    // }

    // #[test]
    // fn object_property_asymmetric() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-asymmetric.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_domain() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-property-domain.owl"));
    // }

    // #[test]
    // fn object_property_functional() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-functional.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_inverse_functional() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-inverse-functional.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_irreflexive() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-irreflexive.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_range() {
    //     assert_round(include_str!("../../ont/owl-rdf/object-property-range.owl"));
    // }

    // #[test]
    // fn object_property_reflexive() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-reflexive.owl"
    //     ));
    // }

    // #[test]
    // fn object_property_symmetric() {
    //     assert_round(include_str!(
    //         "../../ont/owl-rdf/object-property-symmetric.owl"
    //     ));
    // }

    // #[test]
    // fn family() {
    //     assert_round(include_str!("../../ont/owl-rdf/family.owl"));
    // }

}
