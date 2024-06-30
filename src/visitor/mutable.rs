//! A mutable Visitor for Horned-OWL
//!

//! The interface is similar to that of the immutable visitor; however,
//! parts of the Horned-OWL data model use set implementations which
//! are not readily mutable in place. These are, therefore, unwound
//! into `Vec` before passing to the `VisitMut` instance.

use crate::model::*;
use crate::vocab::Facet;
use std::marker::PhantomData;

pub trait VisitMut<A: ForIRI> {
    fn visit_string(&mut self, _: &mut String) {}
    fn visit_u32(&mut self, _: &mut u32) {}
    fn visit_iri(&mut self, _: &mut IRI<A>) {}
    fn visit_anonymous_individual(&mut self, _: &mut AnonymousIndividual<A>) {}
    fn visit_individual(&mut self, _: &mut Individual<A>) {}
    fn visit_annotation_subject(&mut self, _: &AnnotationSubject<A>) {}
    fn visit_dociri(&mut self, _: &mut DocIRI<A>) {}
    fn visit_class(&mut self, _: &mut Class<A>) {}
    fn visit_datatype(&mut self, _: &mut Datatype<A>) {}
    fn visit_object_property(&mut self, _: &mut ObjectProperty<A>) {}
    fn visit_data_property(&mut self, _: &mut DataProperty<A>) {}
    fn visit_annotation_property(&mut self, _: &mut AnnotationProperty<A>) {}
    fn visit_named_individual(&mut self, _: &mut NamedIndividual<A>) {}
    fn visit_annotated_component(&mut self, _: &mut AnnotatedComponent<A>) {}
    fn visit_component(&mut self, _: &mut Component<A>) {}
    fn visit_import(&mut self, _: &mut Import<A>) {}
    fn visit_ontology_annotation(&mut self, _: &mut OntologyAnnotation<A>) {}
    fn visit_declare_class(&mut self, _: &mut DeclareClass<A>) {}
    fn visit_declare_object_property(&mut self, _: &mut DeclareObjectProperty<A>) {}
    fn visit_declare_annotation_property(&mut self, _: &mut DeclareAnnotationProperty<A>) {}
    fn visit_declare_data_property(&mut self, _: &mut DeclareDataProperty<A>) {}
    fn visit_declare_named_individual(&mut self, _: &mut DeclareNamedIndividual<A>) {}
    fn visit_declare_datatype(&mut self, _: &mut DeclareDatatype<A>) {}
    fn visit_sub_class_of(&mut self, _: &mut SubClassOf<A>) {}
    fn visit_equivalent_classes(&mut self, _: &mut EquivalentClasses<A>) {}
    fn visit_disjoint_classes(&mut self, _: &mut DisjointClasses<A>) {}
    fn visit_disjoint_union(&mut self, _: &mut DisjointUnion<A>) {}
    fn visit_sub_object_property_of(&mut self, _: &mut SubObjectPropertyOf<A>) {}
    fn visit_equivalent_object_properties(&mut self, _: &mut EquivalentObjectProperties<A>) {}
    fn visit_disjoint_object_properties(&mut self, _: &mut DisjointObjectProperties<A>) {}
    fn visit_inverse_object_properties(&mut self, _: &mut InverseObjectProperties<A>) {}
    fn visit_object_property_domain(&mut self, _: &mut ObjectPropertyDomain<A>) {}
    fn visit_object_property_range(&mut self, _: &mut ObjectPropertyRange<A>) {}
    fn visit_functional_object_property(&mut self, _: &mut FunctionalObjectProperty<A>) {}
    fn visit_inverse_functional_object_property(
        &mut self,
        _: &mut InverseFunctionalObjectProperty<A>,
    ) {
    }
    fn visit_reflexive_object_property(&mut self, _: &mut ReflexiveObjectProperty<A>) {}
    fn visit_irreflexive_object_property(&mut self, _: &mut IrreflexiveObjectProperty<A>) {}
    fn visit_symmetric_object_property(&mut self, _: &mut SymmetricObjectProperty<A>) {}
    fn visit_asymmetric_object_property(&mut self, _: &mut AsymmetricObjectProperty<A>) {}
    fn visit_transitive_object_property(&mut self, _: &mut TransitiveObjectProperty<A>) {}
    fn visit_sub_data_property_of(&mut self, _: &mut SubDataPropertyOf<A>) {}
    fn visit_equivalent_data_properties(&mut self, _: &mut EquivalentDataProperties<A>) {}
    fn visit_disjoint_data_properties(&mut self, _: &mut DisjointDataProperties<A>) {}
    fn visit_data_property_domain(&mut self, _: &mut DataPropertyDomain<A>) {}
    fn visit_data_property_range(&mut self, _: &mut DataPropertyRange<A>) {}
    fn visit_functional_data_property(&mut self, _: &mut FunctionalDataProperty<A>) {}
    fn visit_datatype_definition(&mut self, _: &mut DatatypeDefinition<A>) {}
    fn visit_has_key(&mut self, _: &mut HasKey<A>) {}
    fn visit_same_individual(&mut self, _: &mut SameIndividual<A>) {}
    fn visit_different_individuals(&mut self, _: &mut DifferentIndividuals<A>) {}
    fn visit_class_assertion(&mut self, _: &mut ClassAssertion<A>) {}
    fn visit_object_property_assertion(&mut self, _: &mut ObjectPropertyAssertion<A>) {}
    fn visit_negative_object_property_assertion(
        &mut self,
        _: &mut NegativeObjectPropertyAssertion<A>,
    ) {
    }
    fn visit_data_property_assertion(&mut self, _: &mut DataPropertyAssertion<A>) {}
    fn visit_negative_data_property_assertion(&mut self, _: &mut NegativeDataPropertyAssertion<A>) {
    }
    fn visit_annotation_assertion(&mut self, _: &mut AnnotationAssertion<A>) {}
    fn visit_sub_annotation_property_of(&mut self, _: &mut SubAnnotationPropertyOf<A>) {}
    fn visit_annotation_property_domain(&mut self, _: &mut AnnotationPropertyDomain<A>) {}
    fn visit_annotation_property_range(&mut self, _: &mut AnnotationPropertyRange<A>) {}
    fn visit_rule(&mut self, _: &mut Rule<A>) {}
    fn visit_atom(&mut self, _: &mut Atom<A>) {}
    fn visit_variable(&mut self, _: &mut Variable<A>) {}
    fn visit_iarg(&mut self, _: &mut IArgument<A>) {}
    fn visit_darg(&mut self, _: &mut DArgument<A>) {}
    fn visit_literal(&mut self, _: &mut Literal<A>) {}
    fn visit_annotation(&mut self, _: &mut Annotation<A>) {}
    fn visit_annotation_value(&mut self, _: &mut AnnotationValue<A>) {}
    fn visit_object_property_expression(&mut self, _: &mut ObjectPropertyExpression<A>) {}
    fn visit_sub_object_property_expression(&mut self, _: &mut SubObjectPropertyExpression<A>) {}
    fn visit_property_expression(&mut self, _: &mut PropertyExpression<A>) {}
    fn visit_facet_restriction(&mut self, _: &mut FacetRestriction<A>) {}
    fn visit_facet(&mut self, _: &mut Facet) {}
    fn visit_data_range(&mut self, _: &mut DataRange<A>) {}
    fn visit_class_expression(&mut self, _: &mut ClassExpression<A>) {}
    fn visit_ontology_id(&mut self, _: &mut OntologyID<A>) {}
    fn visit_ontology_vec(&mut self, _: &mut Vec<AnnotatedComponent<A>>) {}
    fn visit_option_iri(&mut self, _: &mut Option<IRI<A>>) {}
    fn visit_annotation_vec(&mut self, _: &mut Vec<Annotation<A>>) {}
    fn visit_class_expression_vec(&mut self, _: &mut Vec<ClassExpression<A>>) {}
    fn visit_object_property_expression_vec(&mut self, _: &mut Vec<ObjectPropertyExpression<A>>) {}
    fn visit_data_property_vec(&mut self, _: &mut Vec<DataProperty<A>>) {}
    fn visit_data_range_vec(&mut self, _: &mut Vec<DataRange<A>>) {}
    fn visit_individual_vec(&mut self, _: &mut Vec<Individual<A>>) {}
    fn visit_literal_vec(&mut self, _: &mut Vec<Literal<A>>) {}
    fn visit_facet_restriction_vec(&mut self, _: &Vec<FacetRestriction<A>>) {}
    fn visit_atom_vec(&mut self, _: &mut Vec<Atom<A>>) {}
    fn visit_darg_vec(&mut self, _: &mut Vec<DArgument<A>>) {}
}

pub struct WalkMut<A, V>(V, PhantomData<A>);

impl<A: ForIRI, V: VisitMut<A>> WalkMut<A, V> {
    pub fn new(v: V) -> Self {
        WalkMut(v, PhantomData)
    }

    pub fn as_mut_visit(&mut self) -> &mut V {
        &mut self.0
    }

    pub fn into_visit(self) -> V {
        self.0
    }

    pub fn iri(&mut self, e: &mut IRI<A>) {
        self.0.visit_iri(e);
    }

    pub fn anonymous_individual(&mut self, e: &mut AnonymousIndividual<A>) {
        self.0.visit_anonymous_individual(e);
    }

    pub fn individual(&mut self, e: &mut Individual<A>) {
        self.0.visit_individual(e);
        match e {
            Individual::Anonymous(e) => self.anonymous_individual(e),
            Individual::Named(e) => self.named_individual(e),
        }
    }

    pub fn annotation_subject(&mut self, e: &mut AnnotationSubject<A>) {
        self.0.visit_annotation_subject(e);
        match e {
            AnnotationSubject::IRI(e) => self.iri(e),
            AnnotationSubject::AnonymousIndividual(e) => self.anonymous_individual(e),
        }
    }

    pub fn dociri(&mut self, e: &mut DocIRI<A>) {
        self.0.visit_dociri(e);
        self.iri(&mut e.0);
    }

    pub fn class(&mut self, e: &mut Class<A>) {
        self.0.visit_class(e);
        self.iri(&mut e.0);
    }

    pub fn datatype(&mut self, e: &mut Datatype<A>) {
        self.0.visit_datatype(e);
        self.iri(&mut e.0);
    }

    pub fn object_property(&mut self, e: &mut ObjectProperty<A>) {
        self.0.visit_object_property(e);
        self.iri(&mut e.0);
    }

    pub fn data_property(&mut self, e: &mut DataProperty<A>) {
        self.0.visit_data_property(e);
        self.iri(&mut e.0);
    }

    pub fn annotation_property(&mut self, e: &mut AnnotationProperty<A>) {
        self.0.visit_annotation_property(e);
        self.iri(&mut e.0);
    }

    pub fn named_individual(&mut self, e: &mut NamedIndividual<A>) {
        self.0.visit_named_individual(e);
        self.iri(&mut e.0);
    }

    pub fn annotated_component(&mut self, e: &mut AnnotatedComponent<A>) {
        self.0.visit_annotated_component(e);
        self.component(&mut e.component);
        let mut v = e.clone().ann.into_iter().collect();
        self.annotation_vec(&mut v);

        let mut annset = v.into_iter().collect();
        std::mem::swap(&mut annset, &mut e.ann)
    }

    pub fn component(&mut self, e: &mut Component<A>) {
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
            Component::NegativeDataPropertyAssertion(ax) => {
                self.negative_data_property_assertion(ax)
            }
            Component::AnnotationAssertion(ax) => self.annotation_assertion(ax),
            Component::SubAnnotationPropertyOf(ax) => self.sub_annotation_property_of(ax),
            Component::AnnotationPropertyDomain(ax) => self.annotation_property_domain(ax),
            Component::AnnotationPropertyRange(ax) => self.annotation_property_range(ax),
            Component::Rule(sr) => self.rule(sr),
        }
    }

    pub fn import(&mut self, e: &mut Import<A>) {
        self.0.visit_import(e);
        self.iri(&mut e.0);
    }

    pub fn ontology_annotation(&mut self, e: &mut OntologyAnnotation<A>) {
        self.0.visit_ontology_annotation(e);
        self.annotation(&mut e.0);
    }

    pub fn declare_class(&mut self, e: &mut DeclareClass<A>) {
        self.0.visit_declare_class(e);
        self.class(&mut e.0);
    }

    pub fn declare_object_property(&mut self, e: &mut DeclareObjectProperty<A>) {
        self.0.visit_declare_object_property(e);
        self.object_property(&mut e.0);
    }

    pub fn declare_annotation_property(&mut self, e: &mut DeclareAnnotationProperty<A>) {
        self.0.visit_declare_annotation_property(e);
        self.annotation_property(&mut e.0);
    }

    pub fn declare_data_property(&mut self, e: &mut DeclareDataProperty<A>) {
        self.0.visit_declare_data_property(e);
        self.data_property(&mut e.0);
    }

    pub fn declare_named_individual(&mut self, e: &mut DeclareNamedIndividual<A>) {
        self.0.visit_declare_named_individual(e);
        self.named_individual(&mut e.0);
    }

    pub fn declare_datatype(&mut self, e: &mut DeclareDatatype<A>) {
        self.0.visit_declare_datatype(e);
        self.datatype(&mut e.0);
    }

    pub fn sub_class_of(&mut self, e: &mut SubClassOf<A>) {
        self.0.visit_sub_class_of(e);
        self.class_expression(&mut e.sup);
        self.class_expression(&mut e.sub);
    }

    pub fn equivalent_classes(&mut self, e: &mut EquivalentClasses<A>) {
        self.0.visit_equivalent_classes(e);
        self.class_expression_vec(&mut e.0);
    }

    pub fn disjoint_classes(&mut self, e: &mut DisjointClasses<A>) {
        self.0.visit_disjoint_classes(e);
        self.class_expression_vec(&mut e.0);
    }

    pub fn disjoint_union(&mut self, e: &mut DisjointUnion<A>) {
        self.0.visit_disjoint_union(e);
        self.class(&mut e.0);
        self.class_expression_vec(&mut e.1);
    }

    pub fn sub_object_property_of(&mut self, e: &mut SubObjectPropertyOf<A>) {
        self.0.visit_sub_object_property_of(e);
        self.object_property_expression(&mut e.sup);
        self.sub_object_property_expression(&mut e.sub);
    }

    pub fn equivalent_object_properties(&mut self, e: &mut EquivalentObjectProperties<A>) {
        self.0.visit_equivalent_object_properties(e);
        self.object_property_expression_vec(&mut e.0);
    }

    pub fn disjoint_object_properties(&mut self, e: &mut DisjointObjectProperties<A>) {
        self.0.visit_disjoint_object_properties(e);
        self.object_property_expression_vec(&mut e.0);
    }

    pub fn inverse_object_properties(&mut self, e: &mut InverseObjectProperties<A>) {
        self.0.visit_inverse_object_properties(e);
        self.object_property(&mut e.0);
        self.object_property(&mut e.1);
    }

    pub fn object_property_domain(&mut self, e: &mut ObjectPropertyDomain<A>) {
        self.0.visit_object_property_domain(e);
        self.object_property_expression(&mut e.ope);
        self.class_expression(&mut e.ce);
    }

    pub fn object_property_range(&mut self, e: &mut ObjectPropertyRange<A>) {
        self.0.visit_object_property_range(e);
        self.object_property_expression(&mut e.ope);
        self.class_expression(&mut e.ce);
    }

    pub fn functional_object_property(&mut self, e: &mut FunctionalObjectProperty<A>) {
        self.0.visit_functional_object_property(e);
        self.object_property_expression(&mut e.0);
    }

    pub fn inverse_functional_object_property(
        &mut self,
        e: &mut InverseFunctionalObjectProperty<A>,
    ) {
        self.0.visit_inverse_functional_object_property(e);
        self.object_property_expression(&mut e.0);
    }

    pub fn reflexive_object_property(&mut self, e: &mut ReflexiveObjectProperty<A>) {
        self.0.visit_reflexive_object_property(e);
        self.object_property_expression(&mut e.0);
    }

    pub fn irreflexive_object_property(&mut self, e: &mut IrreflexiveObjectProperty<A>) {
        self.0.visit_irreflexive_object_property(e);
        self.object_property_expression(&mut e.0);
    }

    pub fn symmetric_object_property(&mut self, e: &mut SymmetricObjectProperty<A>) {
        self.0.visit_symmetric_object_property(e);
        self.object_property_expression(&mut e.0);
    }

    pub fn asymmetric_object_property(&mut self, e: &mut AsymmetricObjectProperty<A>) {
        self.0.visit_asymmetric_object_property(e);
        self.object_property_expression(&mut e.0);
    }

    pub fn transitive_object_property(&mut self, e: &mut TransitiveObjectProperty<A>) {
        self.0.visit_transitive_object_property(e);
        self.object_property_expression(&mut e.0);
    }

    pub fn sub_data_property_of(&mut self, e: &mut SubDataPropertyOf<A>) {
        self.0.visit_sub_data_property_of(e);
        self.data_property(&mut e.sup);
        self.data_property(&mut e.sub);
    }

    pub fn equivalent_data_properties(&mut self, e: &mut EquivalentDataProperties<A>) {
        self.0.visit_equivalent_data_properties(e);
        self.data_property_vec(&mut e.0);
    }

    pub fn disjoint_data_properties(&mut self, e: &mut DisjointDataProperties<A>) {
        self.0.visit_disjoint_data_properties(e);
        self.data_property_vec(&mut e.0);
    }

    pub fn data_property_domain(&mut self, e: &mut DataPropertyDomain<A>) {
        self.0.visit_data_property_domain(e);
        self.data_property(&mut e.dp);
        self.class_expression(&mut e.ce);
    }

    pub fn data_property_range(&mut self, e: &mut DataPropertyRange<A>) {
        self.0.visit_data_property_range(e);
        self.data_property(&mut e.dp);
        self.data_range(&mut e.dr);
    }

    pub fn functional_data_property(&mut self, e: &mut FunctionalDataProperty<A>) {
        self.0.visit_functional_data_property(e);
        self.data_property(&mut e.0);
    }

    pub fn datatype_definition(&mut self, e: &mut DatatypeDefinition<A>) {
        self.0.visit_datatype_definition(e);
        self.datatype(&mut e.kind);
        self.data_range(&mut e.range);
    }

    pub fn has_key(&mut self, e: &mut HasKey<A>) {
        self.0.visit_has_key(e);
        self.class_expression(&mut e.ce);
        for i in e.vpe.iter_mut() {
            self.property_expression(i);
        }
    }

    pub fn same_individual(&mut self, e: &mut SameIndividual<A>) {
        self.0.visit_same_individual(e);
        self.individual_vec(&mut e.0);
    }

    pub fn different_individuals(&mut self, e: &mut DifferentIndividuals<A>) {
        self.0.visit_different_individuals(e);
        self.individual_vec(&mut e.0);
    }

    pub fn class_assertion(&mut self, e: &mut ClassAssertion<A>) {
        self.0.visit_class_assertion(e);
        self.class_expression(&mut e.ce);
        self.individual(&mut e.i);
    }

    pub fn object_property_assertion(&mut self, e: &mut ObjectPropertyAssertion<A>) {
        self.0.visit_object_property_assertion(e);
        self.object_property_expression(&mut e.ope);
        self.individual(&mut e.from);
        self.individual(&mut e.to);
    }

    pub fn negative_object_property_assertion(
        &mut self,
        e: &mut NegativeObjectPropertyAssertion<A>,
    ) {
        self.0.visit_negative_object_property_assertion(e);
        self.object_property_expression(&mut e.ope);
        self.individual(&mut e.from);
        self.individual(&mut e.to);
    }

    pub fn data_property_assertion(&mut self, e: &mut DataPropertyAssertion<A>) {
        self.0.visit_data_property_assertion(e);
        self.data_property(&mut e.dp);
        self.individual(&mut e.from);
        self.literal(&mut e.to);
    }

    pub fn negative_data_property_assertion(&mut self, e: &mut NegativeDataPropertyAssertion<A>) {
        self.0.visit_negative_data_property_assertion(e);
        self.data_property(&mut e.dp);
        self.individual(&mut e.from);
        self.literal(&mut e.to);
    }

    pub fn annotation_assertion(&mut self, e: &mut AnnotationAssertion<A>) {
        self.0.visit_annotation_assertion(e);
        self.annotation_subject(&mut e.subject);
        self.annotation(&mut e.ann);
    }

    pub fn sub_annotation_property_of(&mut self, e: &mut SubAnnotationPropertyOf<A>) {
        self.0.visit_sub_annotation_property_of(e);
        self.annotation_property(&mut e.sup);
        self.annotation_property(&mut e.sub);
    }

    pub fn annotation_property_domain(&mut self, e: &mut AnnotationPropertyDomain<A>) {
        self.0.visit_annotation_property_domain(e);
        self.annotation_property(&mut e.ap);
        self.iri(&mut e.iri);
    }

    pub fn annotation_property_range(&mut self, e: &mut AnnotationPropertyRange<A>) {
        self.0.visit_annotation_property_range(e);
        self.annotation_property(&mut e.ap);
        self.iri(&mut e.iri);
    }

    pub fn rule(&mut self, r: &mut Rule<A>) {
        self.0.visit_rule(r);
        self.atom_vec(&mut r.head);
        self.atom_vec(&mut r.body);
    }

    pub fn atom(&mut self, a: &mut Atom<A>) {
        self.0.visit_atom(a);
        match a {
            Atom::BuiltInAtom { pred, args } => {
                self.iri(pred);
                self.darg_vec(args);
            }
            Atom::ClassAtom { pred, arg } => {
                self.class_expression(pred);
                self.iarg(arg);
            }
            Atom::DataPropertyAtom { pred, args } => {
                self.data_property(pred);
                self.darg(&mut args.0);
                self.darg(&mut args.1);
            }
            Atom::DataRangeAtom { pred, arg } => {
                self.data_range(pred);
                self.darg(arg);
            }
            Atom::DifferentIndividualsAtom(arg1, arg2) => {
                self.iarg(arg1);
                self.iarg(arg2);
            }
            Atom::ObjectPropertyAtom { pred, args } => {
                self.object_property_expression(pred);
                self.iarg(&mut args.0);
                self.iarg(&mut args.1);
            }
            Atom::SameIndividualAtom(arg1, arg2) => {
                self.iarg(arg1);
                self.iarg(arg2);
            }
        }
    }

    pub fn variable(&mut self, v: &mut Variable<A>) {
        self.0.visit_variable(v);
    }

    pub fn darg(&mut self, d: &mut DArgument<A>) {
        self.0.visit_darg(d);
        match d {
            DArgument::Literal(l) => self.literal(l),
            DArgument::Variable(v) => self.variable(v),
        }
    }

    pub fn iarg(&mut self, i: &mut IArgument<A>) {
        self.0.visit_iarg(i);
        match i {
            IArgument::Individual(i) => self.individual(i),
            IArgument::Variable(v) => self.variable(v),
        }
    }

    pub fn literal(&mut self, e: &mut Literal<A>) {
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

    pub fn annotation(&mut self, e: &mut Annotation<A>) {
        self.0.visit_annotation(e);
        self.annotation_property(&mut e.ap);
        self.annotation_value(&mut e.av);
    }

    pub fn annotation_value(&mut self, e: &mut AnnotationValue<A>) {
        self.0.visit_annotation_value(e);
        match e {
            AnnotationValue::Literal(e) => self.literal(e),
            AnnotationValue::IRI(e) => self.iri(e),
            AnnotationValue::AnonymousIndividual(a) => self.anonymous_individual(a),
        }
    }

    pub fn object_property_expression(&mut self, e: &mut ObjectPropertyExpression<A>) {
        self.0.visit_object_property_expression(e);
        match e {
            ObjectPropertyExpression::ObjectProperty(e) => self.object_property(e),
            ObjectPropertyExpression::InverseObjectProperty(e) => self.object_property(e),
        }
    }

    pub fn sub_object_property_expression(&mut self, e: &mut SubObjectPropertyExpression<A>) {
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

    pub fn property_expression(&mut self, e: &mut PropertyExpression<A>) {
        self.0.visit_property_expression(e);
        match e {
            PropertyExpression::ObjectPropertyExpression(e) => self.object_property_expression(e),
            PropertyExpression::DataProperty(e) => self.data_property(e),
            PropertyExpression::AnnotationProperty(e) => self.annotation_property(e),
        }
    }

    pub fn facet_restriction(&mut self, e: &mut FacetRestriction<A>) {
        self.0.visit_facet_restriction(e);
        self.facet(&mut e.f);
        self.literal(&mut e.l);
    }

    pub fn facet(&mut self, e: &mut Facet) {
        self.0.visit_facet(e);
    }

    pub fn data_range(&mut self, e: &mut DataRange<A>) {
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

    pub fn class_expression(&mut self, e: &mut ClassExpression<A>) {
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

    pub fn ontology_id(&mut self, e: &mut OntologyID<A>) {
        self.0.visit_ontology_id(e);
        self.option_iri(&mut e.iri);
        self.option_iri(&mut e.viri);
    }

    pub fn ontology_vec(&mut self, e: &mut Vec<AnnotatedComponent<A>>) {
        self.0.visit_ontology_vec(e);
        for i in e.iter_mut() {
            self.annotated_component(i);
        }
    }

    pub fn option_iri(&mut self, e: &mut Option<IRI<A>>) {
        self.0.visit_option_iri(e);
        match e {
            Some(e) => self.iri(e),
            None => {}
        }
    }

    // Collections
    pub fn annotation_vec(&mut self, e: &mut Vec<Annotation<A>>) {
        self.0.visit_annotation_vec(e);
        for i in e.iter_mut() {
            self.annotation(i);
        }
    }

    pub fn class_expression_vec(&mut self, e: &mut Vec<ClassExpression<A>>) {
        self.0.visit_class_expression_vec(e);
        for i in e.iter_mut() {
            self.class_expression(i);
        }
    }

    pub fn object_property_expression_vec(&mut self, e: &mut Vec<ObjectPropertyExpression<A>>) {
        self.0.visit_object_property_expression_vec(e);
        for i in e.iter_mut() {
            self.object_property_expression(i);
        }
    }

    pub fn data_property_vec(&mut self, e: &mut Vec<DataProperty<A>>) {
        self.0.visit_data_property_vec(e);
        for i in e.iter_mut() {
            self.data_property(i);
        }
    }

    pub fn individual_vec(&mut self, e: &mut Vec<Individual<A>>) {
        self.0.visit_individual_vec(e);
        for i in e.iter_mut() {
            self.individual(i);
        }
    }

    pub fn literal_vec(&mut self, e: &mut Vec<Literal<A>>) {
        self.0.visit_literal_vec(e);
        for i in e.iter_mut() {
            self.literal(i);
        }
    }

    pub fn facet_restriction_vec(&mut self, e: &mut Vec<FacetRestriction<A>>) {
        self.0.visit_facet_restriction_vec(e);
        for i in e.iter_mut() {
            self.facet_restriction(i);
        }
    }

    pub fn data_range_vec(&mut self, e: &mut Vec<DataRange<A>>) {
        self.0.visit_data_range_vec(e);
        for i in e.iter_mut() {
            self.data_range(i);
        }
    }

    pub fn atom_vec(&mut self, v: &mut Vec<Atom<A>>) {
        self.0.visit_atom_vec(v);
        for i in v.iter_mut() {
            self.atom(i);
        }
    }

    pub fn darg_vec(&mut self, v: &mut Vec<DArgument<A>>) {
        self.0.visit_darg_vec(v);
        for i in v.iter_mut() {
            self.darg(i);
        }
    }
}

#[cfg(test)]

mod test {
    use super::*;
    use crate::io::owx::reader::test::read_ok;
    use crate::model::Build;

    struct LabeltoFred;
    impl<A: ForIRI> VisitMut<A> for LabeltoFred {
        fn visit_annotation(&mut self, a: &mut Annotation<A>) {
            if a.ap
                .0
                 .0
                .as_ref()
                .eq("http://www.w3.org/2000/01/rdf-schema#label")
            {
                a.av = Literal::Simple {
                    literal: "fred".into(),
                }
                .into()
            }
        }
    }

    #[test]
    fn test_label() {
        let ont_s = include_str!("../ont/owl-xml/label.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        assert_eq!(ont.i().annotation_assertion().count(), 1);

        let mut walk = super::WalkMut::new(LabeltoFred);
        let mut vec = ont.into_iter().collect();
        walk.ontology_vec(&mut vec);

        match &vec[2] {
            AnnotatedComponent {
                component:
                    Component::AnnotationAssertion(AnnotationAssertion {
                        ann:
                            Annotation {
                                av: AnnotationValue::Literal(Literal::Simple { literal }),
                                ..
                            },
                        ..
                    }),
                ..
            } => {
                assert_eq!(literal, &"fred".to_string());
            }
            _ => {
                assert!(false);
            }
        }
    }

    struct AddAnnotation<A: ForIRI>(Build<A>);
    impl<A: ForIRI> VisitMut<A> for AddAnnotation<A> {
        fn visit_annotation_vec(&mut self, a: &mut Vec<Annotation<A>>) {
            a.push(Annotation {
                ap: self.0.annotation_property("http://example.com"),
                av: Literal::Simple {
                    literal: "hello".to_string(),
                }
                .into(),
            })
        }
    }

    #[test]
    fn test_annotation() {
        let ont_s = include_str!("../ont/owl-xml/class.owx");
        let (ont, _) = read_ok(&mut ont_s.as_bytes());

        dbg!(&ont.i().declare_class().next());

        let mut walk = super::WalkMut::new(AddAnnotation(Build::new()));
        let mut vec: Vec<_> = ont.into_iter().collect();
        assert_eq!(vec[0].ann.len(), 0);

        walk.ontology_vec(&mut vec);

        assert_eq!(vec[0].ann.len(), 1);
    }
}
