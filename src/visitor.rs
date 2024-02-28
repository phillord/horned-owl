use crate::model::*;
use crate::ontology::set::SetOntology;
use std::collections::BTreeSet;
use std::marker::PhantomData;

pub trait Visit<A: ForIRI> {
    fn visit_string(&mut self, _: &String) {}
    fn visit_u32(&mut self, _: &u32) {}
    fn visit_iri(&mut self, _: &IRI<A>) {}
    fn visit_anonymous_individual(&mut self, _: &AnonymousIndividual<A>) {}
    fn visit_individual(&mut self, _: &Individual<A>) {}
    fn visit_annotation_subject(&mut self, _: &AnnotationSubject<A>) {}
    fn visit_dociri(&mut self, _: &DocIRI<A>) {}
    fn visit_class(&mut self, _: &Class<A>) {}
    fn visit_datatype(&mut self, _: &Datatype<A>) {}
    fn visit_object_property(&mut self, _: &ObjectProperty<A>) {}
    fn visit_data_property(&mut self, _: &DataProperty<A>) {}
    fn visit_annotation_property(&mut self, _: &AnnotationProperty<A>) {}
    fn visit_named_individual(&mut self, _: &NamedIndividual<A>) {}
    fn visit_annotated_component(&mut self, _: &AnnotatedComponent<A>) {}
    fn visit_component(&mut self, _: &Component<A>) {}
    fn visit_import(&mut self, _: &Import<A>) {}
    fn visit_ontology_annotation(&mut self, _: &OntologyAnnotation<A>) {}
    fn visit_declare_class(&mut self, _: &DeclareClass<A>) {}
    fn visit_declare_object_property(&mut self, _: &DeclareObjectProperty<A>) {}
    fn visit_declare_annotation_property(&mut self, _: &DeclareAnnotationProperty<A>) {}
    fn visit_declare_data_property(&mut self, _: &DeclareDataProperty<A>) {}
    fn visit_declare_named_individual(&mut self, _: &DeclareNamedIndividual<A>) {}
    fn visit_declare_datatype(&mut self, _: &DeclareDatatype<A>) {}
    fn visit_sub_class_of(&mut self, _: &SubClassOf<A>) {}
    fn visit_equivalent_classes(&mut self, _: &EquivalentClasses<A>) {}
    fn visit_disjoint_classes(&mut self, _: &DisjointClasses<A>) {}
    fn visit_disjoint_union(&mut self, _: &DisjointUnion<A>) {}
    fn visit_sub_object_property_of(&mut self, _: &SubObjectPropertyOf<A>) {}
    fn visit_equivalent_object_properties(&mut self, _: &EquivalentObjectProperties<A>) {}
    fn visit_disjoint_object_properties(&mut self, _: &DisjointObjectProperties<A>) {}
    fn visit_inverse_object_properties(&mut self, _: &InverseObjectProperties<A>) {}
    fn visit_object_property_domain(&mut self, _: &ObjectPropertyDomain<A>) {}
    fn visit_object_property_range(&mut self, _: &ObjectPropertyRange<A>) {}
    fn visit_functional_object_property(&mut self, _: &FunctionalObjectProperty<A>) {}
    fn visit_inverse_functional_object_property(&mut self, _: &InverseFunctionalObjectProperty<A>) {
    }
    fn visit_reflexive_object_property(&mut self, _: &ReflexiveObjectProperty<A>) {}
    fn visit_irreflexive_object_property(&mut self, _: &IrreflexiveObjectProperty<A>) {}
    fn visit_symmetric_object_property(&mut self, _: &SymmetricObjectProperty<A>) {}
    fn visit_asymmetric_object_property(&mut self, _: &AsymmetricObjectProperty<A>) {}
    fn visit_transitive_object_property(&mut self, _: &TransitiveObjectProperty<A>) {}
    fn visit_sub_data_property_of(&mut self, _: &SubDataPropertyOf<A>) {}
    fn visit_equivalent_data_properties(&mut self, _: &EquivalentDataProperties<A>) {}
    fn visit_disjoint_data_properties(&mut self, _: &DisjointDataProperties<A>) {}
    fn visit_data_property_domain(&mut self, _: &DataPropertyDomain<A>) {}
    fn visit_data_property_range(&mut self, _: &DataPropertyRange<A>) {}
    fn visit_functional_data_property(&mut self, _: &FunctionalDataProperty<A>) {}
    fn visit_datatype_definition(&mut self, _: &DatatypeDefinition<A>) {}
    fn visit_has_key(&mut self, _: &HasKey<A>) {}
    fn visit_same_individual(&mut self, _: &SameIndividual<A>) {}
    fn visit_different_individuals(&mut self, _: &DifferentIndividuals<A>) {}
    fn visit_class_assertion(&mut self, _: &ClassAssertion<A>) {}
    fn visit_object_property_assertion(&mut self, _: &ObjectPropertyAssertion<A>) {}
    fn visit_negative_object_property_assertion(&mut self, _: &NegativeObjectPropertyAssertion<A>) {
    }
    fn visit_data_property_assertion(&mut self, _: &DataPropertyAssertion<A>) {}
    fn visit_negative_data_property_assertion(&mut self, _: &NegativeDataPropertyAssertion<A>) {}
    fn visit_annotation_assertion(&mut self, _: &AnnotationAssertion<A>) {}
    fn visit_sub_annotation_property_of(&mut self, _: &SubAnnotationPropertyOf<A>) {}
    fn visit_annotation_property_domain(&mut self, _: &AnnotationPropertyDomain<A>) {}
    fn visit_annotation_property_range(&mut self, _: &AnnotationPropertyRange<A>) {}
    fn visit_literal(&mut self, _: &Literal<A>) {}
    fn visit_annotation(&mut self, _: &Annotation<A>) {}
    fn visit_annotation_value(&mut self, _: &AnnotationValue<A>) {}
    fn visit_object_property_expression(&mut self, _: &ObjectPropertyExpression<A>) {}
    fn visit_sub_object_property_expression(&mut self, _: &SubObjectPropertyExpression<A>) {}
    fn visit_property_expression(&mut self, _: &PropertyExpression<A>) {}
    fn visit_facet_restriction(&mut self, _: &FacetRestriction<A>) {}
    fn visit_facet(&mut self, _: &Facet) {}
    fn visit_data_range(&mut self, _: &DataRange<A>) {}
    fn visit_class_expression(&mut self, _: &ClassExpression<A>) {}
    fn visit_ontology_id(&mut self, _: &OntologyID<A>) {}
    fn visit_set_ontology(&mut self, _: &SetOntology<A>) {}
    fn visit_option_iri(&mut self, _: &Option<IRI<A>>) {}
    fn visit_annotation_set(&mut self, _: &BTreeSet<Annotation<A>>) {}
    fn visit_class_expression_vec(&mut self, _: &Vec<ClassExpression<A>>) {}
    fn visit_object_property_expression_vec(&mut self, _: &Vec<ObjectPropertyExpression<A>>) {}
    fn visit_data_property_vec(&mut self, _: &Vec<DataProperty<A>>) {}
    fn visit_data_range_vec(&mut self, _: &Vec<DataRange<A>>) {}
    fn visit_individual_vec(&mut self, _: &Vec<Individual<A>>) {}
    fn visit_literal_vec(&mut self, _: &Vec<Literal<A>>) {}
    fn visit_facet_restriction_vec(&mut self, _: &Vec<FacetRestriction<A>>) {}
}

pub struct Walk<A, V>(V, PhantomData<A>);

impl<A: ForIRI, V: Visit<A>> Walk<A, V> {
    pub fn new(v: V) -> Self {
        Walk(v, PhantomData::default())
    }

    pub fn as_mut_visit(&mut self) -> &mut V {
        &mut self.0
    }

    pub fn into_visit(self) -> V {
        self.0
    }

    pub fn iri(&mut self, e: &IRI<A>) {
        self.0.visit_iri(e);
    }

    pub fn anonymous_individual(&mut self, e: &AnonymousIndividual<A>) {
        self.0.visit_anonymous_individual(e);
    }

    pub fn individual(&mut self, e: &Individual<A>) {
        self.0.visit_individual(e);
        match e {
            Individual::Anonymous(e) => self.anonymous_individual(e),
            Individual::Named(e) => self.named_individual(e),
        }
    }

    pub fn annotation_subject(&mut self, e: &AnnotationSubject<A>) {
        self.0.visit_annotation_subject(e);
        match e {
            AnnotationSubject::IRI(e) => self.iri(e),
            AnnotationSubject::AnonymousIndividual(e) => self.anonymous_individual(e),
        }
    }

    pub fn dociri(&mut self, e:&DocIRI<A>) {
        self.0.visit_dociri(e);
        self.iri(&e.0);
    }
    
    pub fn class(&mut self, e: &Class<A>) {
        self.0.visit_class(e);
        self.iri(&e.0);
    }

    pub fn datatype(&mut self, e: &Datatype<A>) {
        self.0.visit_datatype(e);
        self.iri(&e.0);
    }

    pub fn object_property(&mut self, e: &ObjectProperty<A>) {
        self.0.visit_object_property(e);
        self.iri(&e.0);
    }

    pub fn data_property(&mut self, e: &DataProperty<A>) {
        self.0.visit_data_property(e);
        self.iri(&e.0);
    }

    pub fn annotation_property(&mut self, e: &AnnotationProperty<A>) {
        self.0.visit_annotation_property(e);
        self.iri(&e.0);
    }

    pub fn named_individual(&mut self, e: &NamedIndividual<A>) {
        self.0.visit_named_individual(e);
        self.iri(&e.0);
    }

    pub fn annotated_component(&mut self, e: &AnnotatedComponent<A>) {
        self.0.visit_annotated_component(e);
        self.component(&e.component);
        self.annotation_set(&e.ann);
    }

    pub fn component(&mut self, e: &Component<A>) {
        self.0.visit_component(e);
        match e {
            Component::OntologyID(ax) => self.ontology_id(ax),
            Component::DocIRI(ax) => self.dociri(ax),
            Component::Import(ax) => self.import(ax),
            Component::OntologyAnnotation(ax) => self.ontology_annotation(ax),
            Component::DeclareClass(ax) => self.declare_class(ax),
            Component::DeclareObjectProperty(ax) => self.declare_object_property(ax),
            Component::DeclareAnnotationProperty(ax) => self.declare_annotation_property(ax),
            Component::DeclareDataProperty(ax) => self.declare_data_property(ax),
            Component::DeclareNamedIndividual(ax) => self.declare_named_individual(ax),
            Component::DeclareDatatype(ax) => self.declare_datatype(ax),
            Component::SubClassOf(ax) => self.sub_class_of(ax),
            Component::EquivalentClasses(ax) => self.equivalent_classes(ax),
            Component::DisjointClasses(ax) => self.disjoint_classes(ax),
            Component::DisjointUnion(ax) => self.disjoint_union(ax),
            Component::SubObjectPropertyOf(ax) => self.sub_object_property_of(ax),
            Component::EquivalentObjectProperties(ax) => self.equivalent_object_properties(ax),
            Component::DisjointObjectProperties(ax) => self.disjoint_object_properties(ax),
            Component::InverseObjectProperties(ax) => self.inverse_object_properties(ax),
            Component::ObjectPropertyDomain(ax) => self.object_property_domain(ax),
            Component::ObjectPropertyRange(ax) => self.object_property_range(ax),
            Component::FunctionalObjectProperty(ax) => self.functional_object_property(ax),
            Component::InverseFunctionalObjectProperty(ax) => {
                self.inverse_functional_object_property(ax)
            }
            Component::ReflexiveObjectProperty(ax) => self.reflexive_object_property(ax),
            Component::IrreflexiveObjectProperty(ax) => self.irreflexive_object_property(ax),
            Component::SymmetricObjectProperty(ax) => self.symmetric_object_property(ax),
            Component::AsymmetricObjectProperty(ax) => self.asymmetric_object_property(ax),
            Component::TransitiveObjectProperty(ax) => self.transitive_object_property(ax),
            Component::SubDataPropertyOf(ax) => self.sub_data_property_of(ax),
            Component::EquivalentDataProperties(ax) => self.equivalent_data_properties(ax),
            Component::DisjointDataProperties(ax) => self.disjoint_data_properties(ax),
            Component::DataPropertyDomain(ax) => self.data_property_domain(ax),
            Component::DataPropertyRange(ax) => self.data_property_range(ax),
            Component::FunctionalDataProperty(ax) => self.functional_data_property(ax),
            Component::DatatypeDefinition(ax) => self.datatype_definition(ax),
            Component::HasKey(ax) => self.has_key(ax),
            Component::SameIndividual(ax) => self.same_individual(ax),
            Component::DifferentIndividuals(ax) => self.different_individuals(ax),
            Component::ClassAssertion(ax) => self.class_assertion(ax),
            Component::ObjectPropertyAssertion(ax) => self.object_property_assertion(ax),
            Component::NegativeObjectPropertyAssertion(ax) => {
                self.negative_object_property_assertion(ax)
            }
            Component::DataPropertyAssertion(ax) => self.data_property_assertion(ax),
            Component::NegativeDataPropertyAssertion(ax) => self.negative_data_property_assertion(ax),
            Component::AnnotationAssertion(ax) => self.annotation_assertion(ax),
            Component::SubAnnotationPropertyOf(ax) => self.sub_annotation_property_of(ax),
            Component::AnnotationPropertyDomain(ax) => self.annotation_property_domain(ax),
            Component::AnnotationPropertyRange(ax) => self.annotation_property_range(ax),
        }
    }

    pub fn import(&mut self, e: &Import<A>) {
        self.0.visit_import(e);
        self.iri(&e.0);
    }

    pub fn ontology_annotation(&mut self, e: &OntologyAnnotation<A>) {
        self.0.visit_ontology_annotation(e);
        self.annotation(&e.0);
    }

    pub fn declare_class(&mut self, e: &DeclareClass<A>) {
        self.0.visit_declare_class(e);
        self.class(&e.0);
    }

    pub fn declare_object_property(&mut self, e: &DeclareObjectProperty<A>) {
        self.0.visit_declare_object_property(e);
        self.object_property(&e.0);
    }

    pub fn declare_annotation_property(&mut self, e: &DeclareAnnotationProperty<A>) {
        self.0.visit_declare_annotation_property(e);
        self.annotation_property(&e.0);
    }

    pub fn declare_data_property(&mut self, e: &DeclareDataProperty<A>) {
        self.0.visit_declare_data_property(e);
        self.data_property(&e.0);
    }

    pub fn declare_named_individual(&mut self, e: &DeclareNamedIndividual<A>) {
        self.0.visit_declare_named_individual(e);
        self.named_individual(&e.0);
    }

    pub fn declare_datatype(&mut self, e: &DeclareDatatype<A>) {
        self.0.visit_declare_datatype(e);
        self.datatype(&e.0);
    }

    pub fn sub_class_of(&mut self, e: &SubClassOf<A>) {
        self.0.visit_sub_class_of(e);
        self.class_expression(&e.sup);
        self.class_expression(&e.sub);
    }

    pub fn equivalent_classes(&mut self, e: &EquivalentClasses<A>) {
        self.0.visit_equivalent_classes(e);
        self.class_expression_vec(&e.0);
    }

    pub fn disjoint_classes(&mut self, e: &DisjointClasses<A>) {
        self.0.visit_disjoint_classes(e);
        self.class_expression_vec(&e.0);
    }

    pub fn disjoint_union(&mut self, e: &DisjointUnion<A>) {
        self.0.visit_disjoint_union(e);
        self.class(&e.0);
        self.class_expression_vec(&e.1);
    }

    pub fn sub_object_property_of(&mut self, e: &SubObjectPropertyOf<A>) {
        self.0.visit_sub_object_property_of(e);
        self.object_property_expression(&e.sup);
        self.sub_object_property_expression(&e.sub);
    }

    pub fn equivalent_object_properties(&mut self, e: &EquivalentObjectProperties<A>) {
        self.0.visit_equivalent_object_properties(e);
        self.object_property_expression_vec(&e.0);
    }

    pub fn disjoint_object_properties(&mut self, e: &DisjointObjectProperties<A>) {
        self.0.visit_disjoint_object_properties(e);
        self.object_property_expression_vec(&e.0);
    }

    pub fn inverse_object_properties(&mut self, e: &InverseObjectProperties<A>) {
        self.0.visit_inverse_object_properties(e);
        self.object_property(&e.0);
        self.object_property(&e.1);
    }

    pub fn object_property_domain(&mut self, e: &ObjectPropertyDomain<A>) {
        self.0.visit_object_property_domain(e);
        self.object_property_expression(&e.ope);
        self.class_expression(&e.ce);
    }

    pub fn object_property_range(&mut self, e: &ObjectPropertyRange<A>) {
        self.0.visit_object_property_range(e);
        self.object_property_expression(&e.ope);
        self.class_expression(&e.ce);
    }

    pub fn functional_object_property(&mut self, e: &FunctionalObjectProperty<A>) {
        self.0.visit_functional_object_property(e);
        self.object_property_expression(&e.0);
    }

    pub fn inverse_functional_object_property(&mut self, e: &InverseFunctionalObjectProperty<A>) {
        self.0.visit_inverse_functional_object_property(e);
        self.object_property_expression(&e.0);
    }

    pub fn reflexive_object_property(&mut self, e: &ReflexiveObjectProperty<A>) {
        self.0.visit_reflexive_object_property(e);
        self.object_property_expression(&e.0);
    }

    pub fn irreflexive_object_property(&mut self, e: &IrreflexiveObjectProperty<A>) {
        self.0.visit_irreflexive_object_property(e);
        self.object_property_expression(&e.0);
    }

    pub fn symmetric_object_property(&mut self, e: &SymmetricObjectProperty<A>) {
        self.0.visit_symmetric_object_property(e);
        self.object_property_expression(&e.0);
    }

    pub fn asymmetric_object_property(&mut self, e: &AsymmetricObjectProperty<A>) {
        self.0.visit_asymmetric_object_property(e);
        self.object_property_expression(&e.0);
    }

    pub fn transitive_object_property(&mut self, e: &TransitiveObjectProperty<A>) {
        self.0.visit_transitive_object_property(e);
        self.object_property_expression(&e.0);
    }

    pub fn sub_data_property_of(&mut self, e: &SubDataPropertyOf<A>) {
        self.0.visit_sub_data_property_of(e);
        self.data_property(&e.sup);
        self.data_property(&e.sub);
    }

    pub fn equivalent_data_properties(&mut self, e: &EquivalentDataProperties<A>) {
        self.0.visit_equivalent_data_properties(e);
        self.data_property_vec(&e.0);
    }

    pub fn disjoint_data_properties(&mut self, e: &DisjointDataProperties<A>) {
        self.0.visit_disjoint_data_properties(e);
        self.data_property_vec(&e.0);
    }

    pub fn data_property_domain(&mut self, e: &DataPropertyDomain<A>) {
        self.0.visit_data_property_domain(e);
        self.data_property(&e.dp);
        self.class_expression(&e.ce);
    }

    pub fn data_property_range(&mut self, e: &DataPropertyRange<A>) {
        self.0.visit_data_property_range(e);
        self.data_property(&e.dp);
        self.data_range(&e.dr);
    }

    pub fn functional_data_property(&mut self, e: &FunctionalDataProperty<A>) {
        self.0.visit_functional_data_property(e);
        self.data_property(&e.0);
    }

    pub fn datatype_definition(&mut self, e: &DatatypeDefinition<A>) {
        self.0.visit_datatype_definition(e);
        self.datatype(&e.kind);
        self.data_range(&e.range);
    }

    pub fn has_key(&mut self, e: &HasKey<A>) {
        self.0.visit_has_key(e);
        self.class_expression(&e.ce);
        for i in e.vpe.iter() {
            self.property_expression(i);
        }
    }

    pub fn same_individual(&mut self, e: &SameIndividual<A>) {
        self.0.visit_same_individual(e);
        self.individual_vec(&e.0);
    }

    pub fn different_individuals(&mut self, e: &DifferentIndividuals<A>) {
        self.0.visit_different_individuals(e);
        self.individual_vec(&e.0);
    }

    pub fn class_assertion(&mut self, e: &ClassAssertion<A>) {
        self.0.visit_class_assertion(e);
        self.class_expression(&e.ce);
        self.individual(&e.i);
    }

    pub fn object_property_assertion(&mut self, e: &ObjectPropertyAssertion<A>) {
        self.0.visit_object_property_assertion(e);
        self.object_property_expression(&e.ope);
        self.individual(&e.from);
        self.individual(&e.to);
    }

    pub fn negative_object_property_assertion(&mut self, e: &NegativeObjectPropertyAssertion<A>) {
        self.0.visit_negative_object_property_assertion(e);
        self.object_property_expression(&e.ope);
        self.individual(&e.from);
        self.individual(&e.to);
    }

    pub fn data_property_assertion(&mut self, e: &DataPropertyAssertion<A>) {
        self.0.visit_data_property_assertion(e);
        self.data_property(&e.dp);
        self.individual(&e.from);
        self.literal(&e.to);
    }

    pub fn negative_data_property_assertion(&mut self, e: &NegativeDataPropertyAssertion<A>) {
        self.0.visit_negative_data_property_assertion(e);
        self.data_property(&e.dp);
        self.individual(&e.from);
        self.literal(&e.to);
    }

    pub fn annotation_assertion(&mut self, e: &AnnotationAssertion<A>) {
        self.0.visit_annotation_assertion(e);
        self.annotation_subject(&e.subject);
        self.annotation(&e.ann);
    }

    pub fn sub_annotation_property_of(&mut self, e: &SubAnnotationPropertyOf<A>) {
        self.0.visit_sub_annotation_property_of(e);
        self.annotation_property(&e.sup);
        self.annotation_property(&e.sub);
    }

    pub fn annotation_property_domain(&mut self, e: &AnnotationPropertyDomain<A>) {
        self.0.visit_annotation_property_domain(e);
        self.annotation_property(&e.ap);
        self.iri(&e.iri);
    }

    pub fn annotation_property_range(&mut self, e: &AnnotationPropertyRange<A>) {
        self.0.visit_annotation_property_range(e);
        self.annotation_property(&e.ap);
        self.iri(&e.iri);
    }

    pub fn literal(&mut self, e: &Literal<A>) {
        self.0.visit_literal(e);
        match e {
            Literal::Simple { literal } => self.0.visit_string(literal),
            Literal::Language { literal, lang } => {
                self.0.visit_string(literal);
                self.0.visit_string(lang);
            }
            Literal::Datatype {
                literal: _,
                datatype_iri,
            } => self.iri(datatype_iri),
        }
    }

    pub fn annotation(&mut self, e: &Annotation<A>) {
        self.0.visit_annotation(e);
        self.annotation_property(&e.ap);
        self.annotation_value(&e.av);
    }

    pub fn annotation_value(&mut self, e: &AnnotationValue<A>) {
        self.0.visit_annotation_value(e);
        match e {
            AnnotationValue::Literal(e) => self.literal(e),
            AnnotationValue::IRI(e) => self.iri(e),
            AnnotationValue::AnonymousIndividual(a) => self.anonymous_individual(a),
        }
    }

    pub fn object_property_expression(&mut self, e: &ObjectPropertyExpression<A>) {
        self.0.visit_object_property_expression(e);
        match e {
            ObjectPropertyExpression::ObjectProperty(e) => self.object_property(e),
            ObjectPropertyExpression::InverseObjectProperty(e) => self.object_property(e),
        }
    }

    pub fn sub_object_property_expression(&mut self, e: &SubObjectPropertyExpression<A>) {
        self.0.visit_sub_object_property_expression(e);
        match e {
            SubObjectPropertyExpression::ObjectPropertyChain(e) => {
                self.object_property_expression_vec(e)
            }
            SubObjectPropertyExpression::ObjectPropertyExpression(e) => {
                self.object_property_expression(e)
            }
        }
    }

    pub fn property_expression(&mut self, e: &PropertyExpression<A>) {
        self.0.visit_property_expression(e);
        match e {
            PropertyExpression::ObjectPropertyExpression(e) => self.object_property_expression(e),
            PropertyExpression::DataProperty(e) => self.data_property(e),
            PropertyExpression::AnnotationProperty(e) => self.annotation_property(e),
        }
    }

    pub fn facet_restriction(&mut self, e: &FacetRestriction<A>) {
        self.0.visit_facet_restriction(e);
        self.facet(&e.f);
        self.literal(&e.l);
    }

    pub fn facet(&mut self, e: &Facet) {
        self.0.visit_facet(e);
    }

    pub fn data_range(&mut self, e: &DataRange<A>) {
        self.0.visit_data_range(e);
        match e {
            DataRange::Datatype(e) => self.datatype(e),
            DataRange::DataIntersectionOf(e) => self.data_range_vec(e),
            DataRange::DataUnionOf(e) => self.data_range_vec(e),
            DataRange::DataComplementOf(e) => self.data_range(e),
            DataRange::DataOneOf(e) => self.literal_vec(e),
            DataRange::DatatypeRestriction(dt, v) => {
                self.datatype(dt);
                self.facet_restriction_vec(v);
            }
        }
    }

    pub fn class_expression(&mut self, e: &ClassExpression<A>) {
        self.0.visit_class_expression(e);
        match e {
            ClassExpression::Class(e) => self.class(e),
            ClassExpression::ObjectIntersectionOf(e) => self.class_expression_vec(e),
            ClassExpression::ObjectUnionOf(e) => self.class_expression_vec(e),
            ClassExpression::ObjectComplementOf(e) => self.class_expression(e),
            ClassExpression::ObjectOneOf(e) => self.individual_vec(e),
            ClassExpression::ObjectSomeValuesFrom { ope, bce } => {
                self.object_property_expression(ope);
                self.class_expression(bce);
            }
            ClassExpression::ObjectAllValuesFrom { ope, bce } => {
                self.object_property_expression(ope);
                self.class_expression(bce);
            }
            ClassExpression::ObjectHasValue { ope, i } => {
                self.object_property_expression(ope);
                self.individual(i);
            }
            ClassExpression::ObjectHasSelf(e) => self.object_property_expression(e),
            ClassExpression::ObjectMinCardinality { n, ope, bce } => {
                self.0.visit_u32(n);
                self.object_property_expression(ope);
                self.class_expression(bce);
            }
            ClassExpression::ObjectMaxCardinality { n, ope, bce } => {
                self.0.visit_u32(n);
                self.object_property_expression(ope);
                self.class_expression(bce);
            }
            ClassExpression::ObjectExactCardinality { n, ope, bce } => {
                self.0.visit_u32(n);
                self.object_property_expression(ope);
                self.class_expression(bce);
            }
            ClassExpression::DataSomeValuesFrom { dp, dr } => {
                self.data_property(dp);
                self.data_range(dr);
            }
            ClassExpression::DataAllValuesFrom { dp, dr } => {
                self.data_property(dp);
                self.data_range(dr);
            }
            ClassExpression::DataHasValue { dp, l } => {
                self.data_property(dp);
                self.literal(l);
            }
            ClassExpression::DataMinCardinality { n, dp, dr } => {
                self.0.visit_u32(n);
                self.data_property(dp);
                self.data_range(dr);
            }
            ClassExpression::DataMaxCardinality { n, dp, dr } => {
                self.0.visit_u32(n);
                self.data_property(dp);
                self.data_range(dr);
            }
            ClassExpression::DataExactCardinality { n, dp, dr } => {
                self.0.visit_u32(n);
                self.data_property(dp);
                self.data_range(dr);
            }
        }
    }

    pub fn ontology_id(&mut self, e:&OntologyID<A>) {
        self.0.visit_ontology_id(e);
        self.option_iri(&e.iri);
        self.option_iri(&e.viri);
    }

    pub fn set_ontology(&mut self, e: &SetOntology<A>) {
        self.0.visit_set_ontology(e);
        for i in e.iter() {
            self.annotated_component(i);
        }
    }

    pub fn option_iri(&mut self, e: &Option<IRI<A>>) {
        self.0.visit_option_iri(e);
        match e {
            Some(e) => self.iri(e),
            None => {}
        }
    }

    // Collections
    pub fn annotation_set(&mut self, e: &BTreeSet<Annotation<A>>) {
        self.0.visit_annotation_set(e);
        for i in e.iter() {
            self.annotation(i);
        }
    }

    pub fn class_expression_vec(&mut self, e: &Vec<ClassExpression<A>>) {
        self.0.visit_class_expression_vec(e);
        for i in e.iter() {
            self.class_expression(i);
        }
    }

    pub fn object_property_expression_vec(&mut self, e: &Vec<ObjectPropertyExpression<A>>) {
        self.0.visit_object_property_expression_vec(e);
        for i in e.iter() {
            self.object_property_expression(i);
        }
    }

    pub fn data_property_vec(&mut self, e: &Vec<DataProperty<A>>) {
        self.0.visit_data_property_vec(e);
        for i in e.iter() {
            self.data_property(i);
        }
    }

    pub fn individual_vec(&mut self, e: &Vec<Individual<A>>) {
        self.0.visit_individual_vec(e);
        for i in e.iter() {
            self.individual(i);
        }
    }

    pub fn literal_vec(&mut self, e: &Vec<Literal<A>>) {
        self.0.visit_literal_vec(e);
        for i in e.iter() {
            self.literal(i);
        }
    }

    pub fn facet_restriction_vec(&mut self, e: &Vec<FacetRestriction<A>>) {
        self.0.visit_facet_restriction_vec(e);
        for i in e.iter() {
            self.facet_restriction(i);
        }
    }

    pub fn data_range_vec(&mut self, e: &Vec<DataRange<A>>) {
        self.0.visit_data_range_vec(e);
        for i in e.iter() {
            self.data_range(i);
        }
    }
}

pub mod entity {
    use super::Visit;
    use crate::model::ForIRI;
    use crate::model::IRI;
    use crate::visitor::AnonymousIndividual;

    pub struct IRIExtract<A>(Vec<IRI<A>>);

    impl<A> Default for IRIExtract<A> {
        fn default() -> Self {
            IRIExtract(vec![])
        }
    }

    impl<A> IRIExtract<A> {
        pub fn as_mut_vec(&mut self) -> &mut Vec<IRI<A>> {
            &mut self.0
        }

        pub fn into_vec(self) -> Vec<IRI<A>> {
            self.0
        }
    }

    impl<A: ForIRI> Visit<A> for IRIExtract<A> {
        fn visit_iri(&mut self, iri: &IRI<A>) {
            self.0.push(iri.clone())
        }
    }

    pub struct EntityExtract<A>(Vec<A>);

    impl<A> Default for EntityExtract<A> {
        fn default() -> Self {
            EntityExtract(vec![])
        }
    }

    impl<A> EntityExtract<A> {
        pub fn as_mut_vec(&mut self) -> &mut Vec<A> {
            &mut self.0
        }

        pub fn into_vec(self) -> Vec<A> {
            self.0
        }
    }

    impl<A: ForIRI> Visit<A> for EntityExtract<A> {
        fn visit_iri(&mut self, iri: &IRI<A>) {
            self.0.push(iri.underlying())
        }

        fn visit_anonymous_individual(&mut self, anon: &AnonymousIndividual<A>) {
            self.0.push(anon.underlying())
        }
    }
}

#[cfg(test)]
mod test {
    use crate::io::owx::reader::read_with_build;
    use crate::ontology::set::SetOntology;

    use super::*;

    use std::io::BufRead;

    #[test]
    fn it_works() {
        assert!(true)
    }

    pub fn read_ok<R: BufRead>(bufread: &mut R) -> SetOntology<String> {
        let r = read_with_build(bufread, &Build::new_string());
        assert!(r.is_ok(), "Expected ontology, got failure:{:?}", r.err());
        let (o, _) = r.ok().unwrap();

        o
    }

    #[test]
    fn single_class() {
        let ont_s = include_str!("./ont/owl-xml/class.owx");
        let ont = read_ok(&mut ont_s.as_bytes());

        let mut walk = Walk::new(super::entity::EntityExtract::default());
        walk.set_ontology(&ont);
        let mut v = walk.into_visit().into_vec();
        v.sort();
        assert_eq!(
            v,
            [
                "http://www.example.com/iri",
                "http://www.example.com/iri#C",
                "http://www.example.com/viri"
            ]
        );
    }
}
