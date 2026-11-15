//! An index that provides rapid look up via declaration kind

use crate::model::{
    AnnotatedComponent, Component, ComponentKind, ForIRI, IRI, Kinded, NamedEntityKind,
    NamedOWLEntityKind, RcAnnotatedComponent, RcStr,
};

use super::indexed::ForIndex;
use super::indexed::OntologyIndex;

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::marker::PhantomData;

/// What the index knows about one IRI.
#[derive(Clone, Copy, Debug)]
struct Declared {
    /// The single kind that `kind` reports.
    collapsed: NamedEntityKind,
    /// Every kind the IRI is declared as, one bit per kind.
    all: u8,
}

const DECLARATION_KINDS: [NamedEntityKind; 6] = [
    NamedEntityKind::Class,
    NamedEntityKind::Datatype,
    NamedEntityKind::ObjectProperty,
    NamedEntityKind::DataProperty,
    NamedEntityKind::AnnotationProperty,
    NamedEntityKind::NamedIndividual,
];

/// The order in which to choose a collapsed kind when we have to.
const PREFERENCE: [NamedEntityKind; 6] = [
    NamedEntityKind::DataProperty,
    NamedEntityKind::ObjectProperty,
    NamedEntityKind::AnnotationProperty,
    NamedEntityKind::Class,
    NamedEntityKind::Datatype,
    NamedEntityKind::NamedIndividual,
];

fn bit(kind: NamedEntityKind) -> u8 {
    DECLARATION_KINDS
        .iter()
        .position(|k| *k == kind)
        .map_or(0, |i| 1 << i)
}

#[derive(Debug)]
pub struct DeclarationMappedIndex<A, AA> {
    // Map between IRIs and their kinds
    kinds: HashMap<IRI<A>, Declared>,
    // Set of punned IRIs
    puns: HashSet<IRI<A>>,
    pd: PhantomData<AA>,
}

impl<A: ForIRI, AA: ForIndex<A>> DeclarationMappedIndex<A, AA> {
    pub fn new() -> DeclarationMappedIndex<A, AA> {
        DeclarationMappedIndex::default()
    }

    pub fn is_annotation_property(&self, iri: &IRI<A>) -> bool {
        matches! {
            self.declaration_kind(iri),
            Some(NamedOWLEntityKind::AnnotationProperty)
        }
    }

    /// Is `iri` declared as `kind`?
    ///
    /// True if `kind` is any of the kinds `iri` is declared as, so it
    /// is correct for a punned IRI whatever order its declarations
    /// arrived in. See [`DeclarationMappedIndex::declaration_kind`].
    pub fn is_declaration_kind(&self, iri: &IRI<A>, kind: NamedOWLEntityKind) -> bool {
        self.is_kind(iri, kind.into())
    }

    /// The one kind `iri` is declared as.
    ///
    /// An IRI declared as several kinds (a pun) is reported as only one
    /// of them, and which one can depend on the order of the
    /// declarations. Use [`DeclarationMappedIndex::is_declaration_kind`]
    /// to ask about a particular kind, and
    /// [`DeclarationMappedIndex::all_kinds`] for all of them.
    pub fn declaration_kind(&self, iri: &IRI<A>) -> Option<NamedOWLEntityKind> {
        self.kind(iri).and_then(|e| e.as_owl())
    }

    /// The property kind of `iri`, if it is declared as a property.
    ///
    /// Ignores any other kind `iri` is declared as, so a class that is
    /// also an object property is an object property. Illegal punning
    /// between property kinds is resolved as for
    /// [`DeclarationMappedIndex::declaration_kind`]: data beats object
    /// beats annotation.
    pub fn property_kind(&self, iri: &IRI<A>) -> Option<NamedOWLEntityKind> {
        use NamedOWLEntityKind::*;
        [DataProperty, ObjectProperty, AnnotationProperty]
            .into_iter()
            .find(|k| self.is_declaration_kind(iri, *k))
    }

    /// Every declared IRI with every kind it is declared as, in no
    /// particular order.
    pub fn iter_all_kinds(&self) -> impl Iterator<Item = (&IRI<A>, BTreeSet<NamedEntityKind>)> {
        self.kinds.iter().map(|(iri, d)| {
            let kinds = DECLARATION_KINDS
                .into_iter()
                .filter(|k| d.all & bit(*k) != 0)
                .collect();
            (iri, kinds)
        })
    }

    /// Every kind `iri` is declared as.
    ///
    /// Empty if `iri` is not declared. Built in vocabulary has no
    /// declaration, so it is also empty; see
    /// [`DeclarationMappedIndex::kind`].
    pub fn all_kinds(&self, iri: &IRI<A>) -> BTreeSet<NamedEntityKind> {
        self.kinds
            .get(iri)
            .map(|d| {
                DECLARATION_KINDS
                    .into_iter()
                    .filter(|k| d.all & bit(*k) != 0)
                    .collect()
            })
            .unwrap_or_default()
    }

    pub fn declaration_kind_prefer_punned_individual(
        &self,
        iri: &IRI<A>,
    ) -> Option<NamedOWLEntityKind> {
        self.kind_prefer_punned_individual(iri)
            .and_then(|e| e.as_owl())
    }

    /// Is `iri` declared as `kind`?
    ///
    /// True if `kind` is any of the kinds `iri` is declared as. An
    /// undeclared built in such as `owl:Thing` has its own kind.
    pub fn is_kind(&self, iri: &IRI<A>, kind: NamedEntityKind) -> bool {
        match self.kinds.get(iri) {
            Some(d) => d.all & bit(kind) != 0,
            None => self.kind(iri) == Some(kind),
        }
    }

    pub fn kind(&self, iri: &IRI<A>) -> Option<NamedEntityKind> {
        self.kinds
            .get(iri)
            .map(|d| d.collapsed)
            // If the IRI is to a built in property (such as Thing) it
            // won't have been declared but should return the right
            // thing.
            .or_else(|| crate::vocab::to_built_in_entity(iri).map(|e| e.into()))
    }

    pub fn kind_prefer_punned_individual(&self, iri: &IRI<A>) -> Option<NamedEntityKind> {
        if self.puns.contains(iri) {
            Some(NamedEntityKind::NamedIndividual)
        } else {
            self.kind(iri)
        }
    }

    pub fn puns(&self) -> &HashSet<IRI<A>> {
        &self.puns
    }

    fn aa_to_ne(&self, ax: &AnnotatedComponent<A>) -> Option<NamedEntityKind> {
        match ax.kind() {
            ComponentKind::DeclareClass
            | ComponentKind::DeclareObjectProperty
            | ComponentKind::DeclareAnnotationProperty
            | ComponentKind::DeclareDataProperty
            | ComponentKind::DeclareDatatype
            | ComponentKind::DeclareNamedIndividual => match &ax.component {
                Component::DeclareClass(_) => Some(NamedEntityKind::Class),
                Component::DeclareObjectProperty(_) => Some(NamedEntityKind::ObjectProperty),
                Component::DeclareAnnotationProperty(_) => {
                    Some(NamedEntityKind::AnnotationProperty)
                }
                Component::DeclareDataProperty(_) => Some(NamedEntityKind::DataProperty),
                Component::DeclareDatatype(_) => Some(NamedEntityKind::Datatype),
                Component::DeclareNamedIndividual(_) => Some(NamedEntityKind::NamedIndividual),
                _ => None,
            },
            _ => None,
        }
    }

    fn aa_to_iri(&self, ax: &AnnotatedComponent<A>) -> Option<IRI<A>> {
        match ax.kind() {
            ComponentKind::DeclareClass
            | ComponentKind::DeclareObjectProperty
            | ComponentKind::DeclareAnnotationProperty
            | ComponentKind::DeclareDataProperty
            | ComponentKind::DeclareDatatype
            | ComponentKind::DeclareNamedIndividual => match &ax.component {
                Component::DeclareClass(dc) => Some(dc.0.0.clone()),
                Component::DeclareObjectProperty(op) => Some(op.0.0.clone()),
                Component::DeclareAnnotationProperty(ap) => Some(ap.0.0.clone()),
                Component::DeclareDataProperty(dp) => Some(dp.0.0.clone()),
                Component::DeclareDatatype(dt) => Some(dt.0.0.clone()),
                Component::DeclareNamedIndividual(ni) => Some(ni.0.0.clone()),
                _ => None,
            },
            _ => None,
        }
    }
}

macro_rules! some {
    ($body:expr) => {
        (|| Some($body))()
    };
}

impl<A, AA> Default for DeclarationMappedIndex<A, AA> {
    fn default() -> Self {
        DeclarationMappedIndex {
            kinds: Default::default(),
            puns: Default::default(),
            pd: Default::default(),
        }
    }
}

impl<A: ForIRI, AA: ForIndex<A>> OntologyIndex<A, AA> for DeclarationMappedIndex<A, AA> {
    fn index_insert(&mut self, ax: AA) -> bool {
        some! {
            {
                let ne = self.aa_to_ne(ax.borrow())?;
                let iri = self.aa_to_iri(ax.borrow())?;

                // Every declaration counts for `all_kinds`, whether or
                // not it decides the collapsed kind.
                let current = self.kinds.get(&iri).map(|d| d.collapsed);
                if let Some(d) = self.kinds.get_mut(&iri) {
                    d.all |= bit(ne);
                }

                // If this is a individual and we already have a
                // class, this is a pun, and we ignore the NI
                if ne == NamedEntityKind::NamedIndividual &&
                    current == Some(NamedEntityKind::Class)
                {
                    self.puns.insert(iri.clone());
                    return None;
                }

                // AnnotationProperty/ObjectProperty/DataProperty
                // punning is not legal in OWL 2 DL, but happens. We
                // resolve it with a fixed rule, whatever the insertion
                // order: DataProperty beats ObjectProperty beats
                // AnnotationProperty.
                //
                // The OWL specification gives no such rule, so this is
                // our own. It began as an ad-hoc accident (insertion
                // order decided, and the round-trip writers happened to
                // emit Data declarations last) and is now implemented
                // specifically, so that a reread agrees with its source
                // however the declarations are ordered.
                match (ne, current) {
                    (
                        NamedEntityKind::AnnotationProperty,
                        Some(NamedEntityKind::ObjectProperty | NamedEntityKind::DataProperty),
                    )
                    | (NamedEntityKind::ObjectProperty, Some(NamedEntityKind::DataProperty)) => {
                        return None;
                    }
                    _ => {}
                }

                // Save the kind
                let all = self.kinds.get(&iri).map_or(0, |d| d.all) | bit(ne);
                self.kinds.insert(iri.clone(), Declared { collapsed: ne, all });

                // If we have replaced an NI with a class, we have a pun
                if ne == NamedEntityKind::Class &&
                    current == Some(NamedEntityKind::NamedIndividual)
                {
                    self.puns.insert(iri);
                }

                // The collapsed kind is now ne
                ne
            }
        }
        .is_some()
    }

    fn index_remove(&mut self, ax: &AnnotatedComponent<A>) -> bool {
        let s = some! {
            {
                let iri = self.aa_to_iri(ax)?;
                let ne = self.aa_to_ne(ax)?;

                if let Some(d) = self.kinds.get_mut(&iri) {
                    d.all &= !bit(ne);
                    if d.all == 0 {
                        self.kinds.remove(&iri);
                    } else if d.collapsed == ne {
                        // Another declaration remains; use the kind
                        // that would win whatever the order.
                        d.collapsed = PREFERENCE
                            .into_iter()
                            .find(|k| d.all & bit(*k) != 0)
                            .unwrap_or(d.collapsed);
                    }
                }

                let both = bit(NamedEntityKind::Class) | bit(NamedEntityKind::NamedIndividual);
                if self.kinds.get(&iri).is_none_or(|d| d.all & both != both) {
                    self.puns.remove(&iri);
                }
            }
        };

        s.is_some()
    }
}

impl DeclarationMappedIndex<RcStr, RcAnnotatedComponent> {
    pub fn new_rc() -> Self {
        Self::default()
    }
}

#[cfg(test)]
mod test {
    use super::DeclarationMappedIndex;
    use crate::model::{
        AnnotatedComponent, Build, NamedEntityKind, NamedOWLEntity, NamedOWLEntityKind, RcStr,
    };
    use crate::ontology::indexed::OntologyIndex;
    use crate::vocab::OWL;
    use std::collections::BTreeSet;
    fn stuff() -> (
        AnnotatedComponent<RcStr>,
        AnnotatedComponent<RcStr>,
        AnnotatedComponent<RcStr>,
    ) {
        let b = Build::new_rc();
        let c: NamedOWLEntity<_> = b.class("http://www.example.com/c").into();
        let o: NamedOWLEntity<_> = b.object_property("http://www.example.com/p").into();
        let b: NamedOWLEntity<_> = b.data_property("http://www.example.com/d").into();

        (c.into(), o.into(), b.into())
    }

    #[test]
    fn test_cons() {
        let _d = DeclarationMappedIndex::new_rc();
    }

    #[test]
    fn test_insert() {
        let mut d = DeclarationMappedIndex::new_rc();
        let s = stuff();
        assert!(d.index_insert(s.0.into()));
        assert!(d.index_insert(s.1.into()));
        assert!(d.index_insert(s.2.into()));
    }

    #[test]
    fn test_data_property_wins_over_object_property() {
        // Data wins deliberately, whichever declaration is inserted first
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        let data: AnnotatedComponent<RcStr> =
            NamedOWLEntity::from(b.data_property(iri.clone())).into();
        let object: AnnotatedComponent<RcStr> =
            NamedOWLEntity::from(b.object_property(iri.clone())).into();

        for order in [[&data, &object], [&object, &data]] {
            let mut d = DeclarationMappedIndex::new_rc();
            for ax in order {
                d.index_insert(ax.clone().into());
            }
            assert_eq!(
                d.declaration_kind(&iri),
                Some(NamedOWLEntityKind::DataProperty)
            );
        }
    }

    #[test]
    fn test_data_property_wins_over_annotation_property() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        let data: AnnotatedComponent<RcStr> =
            NamedOWLEntity::from(b.data_property(iri.clone())).into();
        let annotation: AnnotatedComponent<RcStr> =
            NamedOWLEntity::from(b.annotation_property(iri.clone())).into();

        for order in [[&data, &annotation], [&annotation, &data]] {
            let mut d = DeclarationMappedIndex::new_rc();
            for ax in order {
                d.index_insert(ax.clone().into());
            }
            assert_eq!(
                d.declaration_kind(&iri),
                Some(NamedOWLEntityKind::DataProperty)
            );
        }
    }

    #[test]
    fn test_declaration() {
        let mut d = DeclarationMappedIndex::new_rc();
        let s = stuff();
        assert!(d.index_insert(s.0.into()));
        assert!(d.index_insert(s.1.into()));
        assert!(d.index_insert(s.2.into()));

        let b = Build::new();
        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/c")),
            Some(NamedOWLEntityKind::Class)
        );

        assert!(d.is_declaration_kind(
            &b.iri("http://www.example.com/c"),
            NamedOWLEntityKind::Class
        ));

        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/p")),
            Some(NamedOWLEntityKind::ObjectProperty)
        );

        assert!(d.is_declaration_kind(
            &b.iri("http://www.example.com/p"),
            NamedOWLEntityKind::ObjectProperty
        ));

        assert_eq!(
            d.declaration_kind(&b.iri("http://www.example.com/d")),
            Some(NamedOWLEntityKind::DataProperty)
        );

        assert!(d.is_declaration_kind(
            &b.iri("http://www.example.com/d"),
            NamedOWLEntityKind::DataProperty
        ));
    }

    #[test]
    fn test_declaration_builtin() {
        let d = DeclarationMappedIndex::new_rc();
        let b = Build::new_rc();
        assert_eq!(
            d.declaration_kind(&b.iri(OWL::TopDataProperty.as_ref())),
            Some(NamedOWLEntityKind::DataProperty)
        );
    }

    #[test]
    fn test_pun_support() {
        let mut d = DeclarationMappedIndex::new_rc();
        let b = Build::new_rc();

        assert_eq!(d.puns().len(), 0);

        let iri = b.iri("http://www.example.com/p");
        let c: NamedOWLEntity<_> = b.class("http://www.example.com/p").into();
        let c: AnnotatedComponent<_> = c.into();
        let ni: NamedOWLEntity<_> = b.named_individual("http://www.example.com/p").into();
        let ni: AnnotatedComponent<_> = ni.into();

        d.index_insert(c.clone().into());
        d.index_insert(ni.clone().into());

        assert_eq!(d.puns().len(), 1);
        assert_eq!(d.puns().iter().next(), Some(&iri));
        assert_eq!(d.declaration_kind(&iri), Some(NamedOWLEntityKind::Class));
        assert_eq!(
            d.declaration_kind_prefer_punned_individual(&iri),
            Some(NamedOWLEntityKind::NamedIndividual)
        );

        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(ni.clone().into());
        d.index_insert(c.clone().into());

        assert_eq!(d.puns().len(), 1);
        assert_eq!(d.puns().iter().next(), Some(&iri));
        assert_eq!(d.declaration_kind(&iri), Some(NamedOWLEntityKind::Class));
        assert_eq!(
            d.declaration_kind_prefer_punned_individual(&iri),
            Some(NamedOWLEntityKind::NamedIndividual)
        );
    }

    // Regression test for https://github.com/phillord/horned-owl/issues/228
    //
    // An IRI declared as both AnnotationProperty and ObjectProperty (illegal
    // punning under OWL 2 DL, but seen in real-world ontologies) must
    // resolve to the same declaration_kind() regardless of which
    // declaration was inserted first -- otherwise round-tripping through a
    // writer that reorders the declarations changes the resolved kind and
    // breaks reread.
    #[test]
    fn test_annotation_object_property_pun_order_independent() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        let ap: AnnotatedComponent<_> = {
            let ap: NamedOWLEntity<_> = b.annotation_property("http://www.example.com/p").into();
            ap.into()
        };
        let op: AnnotatedComponent<_> = {
            let op: NamedOWLEntity<_> = b.object_property("http://www.example.com/p").into();
            op.into()
        };

        // AnnotationProperty declared first, ObjectProperty second.
        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(ap.clone().into());
        d.index_insert(op.clone().into());
        assert_eq!(
            d.declaration_kind(&iri),
            Some(NamedOWLEntityKind::ObjectProperty)
        );

        // ObjectProperty declared first, AnnotationProperty second.
        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(op.into());
        d.index_insert(ap.into());
        assert_eq!(
            d.declaration_kind(&iri),
            Some(NamedOWLEntityKind::ObjectProperty)
        );
    }

    // Same as above but for DataProperty rather than ObjectProperty.
    #[test]
    fn test_annotation_data_property_pun_order_independent() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        let ap: AnnotatedComponent<_> = {
            let ap: NamedOWLEntity<_> = b.annotation_property("http://www.example.com/p").into();
            ap.into()
        };
        let dp: AnnotatedComponent<_> = {
            let dp: NamedOWLEntity<_> = b.data_property("http://www.example.com/p").into();
            dp.into()
        };

        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(ap.clone().into());
        d.index_insert(dp.clone().into());
        assert_eq!(
            d.declaration_kind(&iri),
            Some(NamedOWLEntityKind::DataProperty)
        );

        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(dp.into());
        d.index_insert(ap.into());
        assert_eq!(
            d.declaration_kind(&iri),
            Some(NamedOWLEntityKind::DataProperty)
        );
    }

    fn decl(b: &Build<RcStr>, kind: NamedEntityKind, iri: &str) -> AnnotatedComponent<RcStr> {
        match kind {
            NamedEntityKind::Class => NamedOWLEntity::from(b.class(iri)).into(),
            NamedEntityKind::Datatype => NamedOWLEntity::from(b.datatype(iri)).into(),
            NamedEntityKind::ObjectProperty => NamedOWLEntity::from(b.object_property(iri)).into(),
            NamedEntityKind::DataProperty => NamedOWLEntity::from(b.data_property(iri)).into(),
            NamedEntityKind::AnnotationProperty => {
                NamedOWLEntity::from(b.annotation_property(iri)).into()
            }
            NamedEntityKind::NamedIndividual => {
                NamedOWLEntity::from(b.named_individual(iri)).into()
            }
            _ => unreachable!(),
        }
    }

    const KINDS: [NamedEntityKind; 6] = [
        NamedEntityKind::Class,
        NamedEntityKind::Datatype,
        NamedEntityKind::ObjectProperty,
        NamedEntityKind::DataProperty,
        NamedEntityKind::AnnotationProperty,
        NamedEntityKind::NamedIndividual,
    ];

    // Every pair of kinds, in both orders: each is a kind of the IRI
    // and all_kinds has exactly the two.
    #[test]
    fn test_every_pun_reports_both_kinds_in_either_order() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        for k1 in KINDS {
            for k2 in KINDS {
                let mut d = DeclarationMappedIndex::new_rc();
                d.index_insert(decl(&b, k1, "http://www.example.com/p").into());
                d.index_insert(decl(&b, k2, "http://www.example.com/p").into());

                assert!(d.is_kind(&iri, k1), "{k1:?} then {k2:?}");
                assert!(d.is_kind(&iri, k2), "{k1:?} then {k2:?}");
                assert_eq!(
                    d.all_kinds(&iri),
                    BTreeSet::from([k1, k2]),
                    "{k1:?} then {k2:?}"
                );
                for other in KINDS.into_iter().filter(|k| *k != k1 && *k != k2) {
                    assert!(!d.is_kind(&iri, other), "{k1:?} then {k2:?}: {other:?}");
                }
            }
        }
    }

    #[test]
    fn test_class_object_property_pun_is_both() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        for order in [
            [NamedEntityKind::Class, NamedEntityKind::ObjectProperty],
            [NamedEntityKind::ObjectProperty, NamedEntityKind::Class],
        ] {
            let mut d = DeclarationMappedIndex::new_rc();
            for k in order {
                d.index_insert(decl(&b, k, "http://www.example.com/p").into());
            }
            assert!(d.is_declaration_kind(&iri, NamedOWLEntityKind::Class));
            assert!(d.is_declaration_kind(&iri, NamedOWLEntityKind::ObjectProperty));
            assert_eq!(
                d.property_kind(&iri),
                Some(NamedOWLEntityKind::ObjectProperty)
            );
        }
    }

    #[test]
    fn test_property_kind_ignores_non_properties() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        let mut d = DeclarationMappedIndex::new_rc();
        assert_eq!(d.property_kind(&iri), None);
        d.index_insert(decl(&b, NamedEntityKind::Class, "http://www.example.com/p").into());
        d.index_insert(
            decl(
                &b,
                NamedEntityKind::NamedIndividual,
                "http://www.example.com/p",
            )
            .into(),
        );
        assert_eq!(d.property_kind(&iri), None);
    }

    #[test]
    fn test_property_kind_precedence_order_independent() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        let props = [
            NamedEntityKind::AnnotationProperty,
            NamedEntityKind::ObjectProperty,
            NamedEntityKind::DataProperty,
        ];
        for k1 in props {
            for k2 in props {
                let mut d = DeclarationMappedIndex::new_rc();
                d.index_insert(decl(&b, k1, "http://www.example.com/p").into());
                d.index_insert(decl(&b, k2, "http://www.example.com/p").into());
                let want = [k1, k2].into_iter().max_by_key(|k| match k {
                    NamedEntityKind::DataProperty => 2,
                    NamedEntityKind::ObjectProperty => 1,
                    _ => 0,
                });
                assert_eq!(
                    d.property_kind(&iri),
                    want.and_then(|k| k.as_owl()),
                    "{k1:?} then {k2:?}"
                );
            }
        }
    }

    #[test]
    fn test_remove_drops_one_kind() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        let class = decl(&b, NamedEntityKind::Class, "http://www.example.com/p");
        let op = decl(
            &b,
            NamedEntityKind::ObjectProperty,
            "http://www.example.com/p",
        );

        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(class.clone().into());
        d.index_insert(op.clone().into());

        d.index_remove(&op);
        assert_eq!(d.all_kinds(&iri), BTreeSet::from([NamedEntityKind::Class]));
        assert_eq!(d.kind(&iri), Some(NamedEntityKind::Class));

        d.index_remove(&class);
        assert!(d.all_kinds(&iri).is_empty());
        assert_eq!(d.kind(&iri), None);
    }

    #[test]
    fn test_remove_clears_pun() {
        let b = Build::new_rc();
        let iri = b.iri("http://www.example.com/p");
        let class = decl(&b, NamedEntityKind::Class, "http://www.example.com/p");
        let ni = decl(
            &b,
            NamedEntityKind::NamedIndividual,
            "http://www.example.com/p",
        );

        let mut d = DeclarationMappedIndex::new_rc();
        d.index_insert(class.clone().into());
        d.index_insert(ni.clone().into());
        assert!(d.puns().contains(&iri));

        d.index_remove(&ni);
        assert!(!d.puns().contains(&iri));
        assert!(d.is_kind(&iri, NamedEntityKind::Class));
    }

    #[test]
    fn test_builtin_is_kind_without_declaration() {
        let d = DeclarationMappedIndex::new_rc();
        let b = Build::new_rc();
        let iri = b.iri(OWL::TopDataProperty.as_ref());
        assert!(d.is_kind(&iri, NamedEntityKind::DataProperty));
        assert_eq!(
            d.property_kind(&iri),
            Some(NamedOWLEntityKind::DataProperty)
        );
        assert!(d.all_kinds(&iri).is_empty());
    }
}
