use std::collections::HashMap;

use crate::{
    model::{
        Class, ClassExpression, DataRange, ForIRI, IRI, NamedEntityKind, ObjectProperty,
        ObjectPropertyExpression,
    },
    visitor::mutable::VisitMut,
};

/// Convert a `DataRange` to a `ClassExpression` by reinterpreting datatype IRIs as class IRIs.
///
/// Returns `None` for data ranges that have no class expression equivalent (e.g. facet
/// restrictions, data oneOf).
pub(super) fn data_range_to_class_expression<A: ForIRI>(
    dr: DataRange<A>,
) -> Option<ClassExpression<A>> {
    match dr {
        DataRange::Datatype(dt) => Some(ClassExpression::Class(Class(dt.0))),
        DataRange::DataComplementOf(dr) => data_range_to_class_expression(*dr)
            .map(|ce| ClassExpression::ObjectComplementOf(Box::new(ce))),
        DataRange::DataIntersectionOf(drs) => drs
            .into_iter()
            .map(data_range_to_class_expression)
            .collect::<Option<Vec<_>>>()
            .map(ClassExpression::ObjectIntersectionOf),
        DataRange::DataUnionOf(drs) => drs
            .into_iter()
            .map(data_range_to_class_expression)
            .collect::<Option<Vec<_>>>()
            .map(ClassExpression::ObjectUnionOf),
        DataRange::DataOneOf(_) | DataRange::DatatypeRestriction(_, _) => None,
    }
}

pub(crate) struct ComponentVisitor<A: ForIRI> {
    pub(crate) entity_kinds: HashMap<IRI<A>, NamedEntityKind>,
}

impl<A: ForIRI> VisitMut<A> for ComponentVisitor<A> {
    fn visit_class_expression(&mut self, e: &mut ClassExpression<A>) {
        let ope_for = |dp: &crate::model::DataProperty<A>| {
            ObjectPropertyExpression::ObjectProperty(ObjectProperty(dp.0.clone()))
        };

        let is_object = |dp: &crate::model::DataProperty<A>| {
            matches!(
                self.entity_kinds.get(&dp.0),
                Some(&NamedEntityKind::ObjectProperty) | None
            )
        };

        let converted = match e {
            ClassExpression::DataSomeValuesFrom { dp, dr } if is_object(dp) => {
                data_range_to_class_expression(dr.clone()).map(|bce| {
                    ClassExpression::ObjectSomeValuesFrom {
                        ope: ope_for(dp),
                        bce: Box::new(bce),
                    }
                })
            }
            ClassExpression::DataAllValuesFrom { dp, dr } if is_object(dp) => {
                data_range_to_class_expression(dr.clone()).map(|bce| {
                    ClassExpression::ObjectAllValuesFrom {
                        ope: ope_for(dp),
                        bce: Box::new(bce),
                    }
                })
            }
            ClassExpression::DataMinCardinality { n, dp, dr } if is_object(dp) => {
                let n = *n;
                data_range_to_class_expression(dr.clone()).map(|bce| {
                    ClassExpression::ObjectMinCardinality {
                        n,
                        ope: ope_for(dp),
                        bce: Box::new(bce),
                    }
                })
            }
            ClassExpression::DataMaxCardinality { n, dp, dr } if is_object(dp) => {
                let n = *n;
                data_range_to_class_expression(dr.clone()).map(|bce| {
                    ClassExpression::ObjectMaxCardinality {
                        n,
                        ope: ope_for(dp),
                        bce: Box::new(bce),
                    }
                })
            }
            ClassExpression::DataExactCardinality { n, dp, dr } if is_object(dp) => {
                let n = *n;
                data_range_to_class_expression(dr.clone()).map(|bce| {
                    ClassExpression::ObjectExactCardinality {
                        n,
                        ope: ope_for(dp),
                        bce: Box::new(bce),
                    }
                })
            }
            _ => None,
        };

        if let Some(new_ce) = converted {
            *e = new_ce;
        }
    }
}
