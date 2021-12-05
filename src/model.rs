//! This module implements the basic data data structure for OWL2.

//! # Overview
//!
//! This module consists of a core data model, most of which are
//! concrete implementations.
//!
//! An `Ontology` is represented with an interface similar to a
//! `HashSet` but using a trait. No methods are provided to search or
//! access the contents of an `Ontology` which are instead provided by
//! [implementations](../ontology/index.html). Each `Ontology` is a set of
//! `AnnotatedAxiom` instances, which consists of an `Axiom` and a set
//! of `Annotation`. The `Axiom` itself is a large enum representing
//! the different axioms that OWL2 supports.

//! Efficiency is gained from the use of an IRI which is a newtype
//! over an `Rc<String>`.

//! # Naming
//!
//! The OWL specification is large and complicated. This library,
//! therefore, takes efforts to be as regular and predictable as
//! possible. Names have been chosen to reflect the OWL structural
//! specification, (see <https://www.w3.org/TR/owl2-syntax/>).
//!
//! The core data structures use both C-style structs and tuples
//! structs. The aim is for this to maximize usability.

//! The rules are as follows:
//! 1. Use tuples where all the entities are of the same type and
//! semantically equivalent
//! 2. Use structs where entities are not equivalent.
//! 3. Where values are equivalent and bounded in number to two, use a
//! tuple with two values.
//! 4. Where values are equivalent, but unbounded, use a Vec, either as
//! part of a tuple or struct.
//! 5. Where structs are used, variables names should be short,
//! identify the type is unique, or be descriptive if not.

//! # Example
//! - Rule 1:
//! ```
//! # use horned_owl::model::*;
//! // TransitiveObjectProperty(ObjectProperty)
//! let b = Build::new();
//! let top = TransitiveObjectProperty(ObjectPropertyExpression::ObjectProperty
//!                          (b.object_property("http://www.example.com/op")));
//! ```
//! - Rule 2:
//! ```
//! # use horned_owl::model::*;
//! // ObjectSomeValuesFrom{ope:PropertyExpression, ce:ClassExpression}
//! let b = Build::new();
//! let some = ClassExpression::ObjectSomeValuesFrom{
//!                 ope: b.object_property("http://www.example.com/p").into(),
//!                 bce: b.class("http://www.example.com/c").into()
//! };
//! ```
//! - Rule 3:
//! ```
//! # use horned_owl::model::*;
//! // InverseObjectProperty(ObjectProperty, ObjectProperty)
//! let b = Build::new();
//! let iop = InverseObjectProperties
//!             (b.object_property("http://www.example.com/op1"),
//!              b.object_property("http://www.example.com/op2"));
//! ```
//! - Rule 4:
//! ```
//! # use horned_owl::model::*;
//! // EquivalentClasses(Vec<ClassExpression>)
//! let b = Build::new();
//! let ec = EquivalentClasses
//!           (vec!(b.class("http://www.example.com/op1").into(),
//!                 b.class("http://www.example.com/op2").into()));
//! ```
//! - Rule 5:
//! ```
//! //ObjectPropertyAssertion {
//! //ope: ObjectPropertyExpression,
//! //from: Individual,
//! //to: Individual,
//! //}
//! # use horned_owl::model::*;
//! let b = Build::new();
//! let opa = ObjectPropertyAssertion {
//!     ope: b.object_property("http://www.example.com/op").into(),
//!     from: b.named_individual("http://www.example.com/i1").into(),
//!     to: b.named_individual("http://www.example.com/i2").into(),
//! };
//! ```

use std::{cell::RefCell, fmt::Formatter};
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::rc::Rc;

/// An
/// [IRI](https://en.wikipedia.org/wiki/Internationalized_Resource_Identifier)
/// is an internationalized version of an URI/URL.
///
/// Here, we represent it as a simple string. In Horned-OWL IRIs are
/// created through `Build`; this caches the underlying String meaning
/// that IRIs are light-weight to `clone`.
#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct IRI(Rc<str>);

impl Deref for IRI {
    type Target=str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl AsRef<str> for IRI {
    fn as_ref(&self) -> &str {
        &self.0.as_ref()
    }
}

impl From<&IRI> for Rc<str> {
    fn from(i:&IRI) -> Rc<str> {
        i.0.clone().into()
    }
}

impl From<&IRI> for String {
    fn from(i:&IRI) -> String {
        i.0.as_ref().to_string()
    }
}

impl From<IRI> for String {
    fn from(i:IRI) -> String {
        i.0.as_ref().to_string()
    }
}

impl Display for IRI {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(&self.0)
    }
}


/// `Build` creates new `IRI` and `NamedEntity` instances.
///
/// There is caching for performance. An `IRI` or `NamedEntity` with a
/// given IRI will use the same string in memory, if they have been
/// created with the same builder. Equality, ordering and hashing is
/// conserved across different `Build` instances, so entities from
/// different instances can be combined within a single ontology
/// without consequences except for increased memory use.

// Currently `Build` uses Rc/RefCell, as does IRI which limits this
// library to a single thread, as does the use of Rc in IRI. One or
// both could be replaced by traits or enums straight-forwardly
// enough, to enable threading.
#[derive(Debug, Default)]
pub struct Build(Rc<RefCell<BTreeSet<IRI>>>);

impl Build {
    pub fn new() -> Build {
        Build(Rc::new(RefCell::new(BTreeSet::new())))
    }

    /// Constructs a new `IRI`
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new();
    /// let iri = b.iri("http://www.example.com");
    /// assert_eq!("http://www.example.com", String::from(iri));
    /// ```
    pub fn iri<S>(&self, s: S) -> IRI
    where
        S: Into<String>,
    {
        let s:String = s.into();
        let iri = IRI(s.into());

        let mut cache = self.0.borrow_mut();
        if cache.contains(&iri) {
            return cache.get(&iri).unwrap().clone();
        }

        cache.insert(iri.clone());
        iri
    }

    /// Constructs a new `Class`.

    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new();
    /// let c1 = b.class("http://www.example.com".to_string());
    /// let c2 = b.class("http://www.example.com");
    ///
    /// assert_eq!(c1, c2);
    /// ```
    ///
    pub fn class<S>(&self, s: S) -> Class
    where
        S: Into<String>,
    {
        Class(self.iri(s))
    }

    /// Constructs a new `ObjectProperty`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new();
    /// let obp1 = b.object_property("http://www.example.com".to_string());
    /// let obp2 = b.object_property("http://www.example.com");
    ///
    /// assert_eq!(obp1, obp2);
    /// ```
    pub fn object_property<S>(&self, s: S) -> ObjectProperty
    where
        S: Into<String>,
    {
        ObjectProperty(self.iri(s))
    }

    /// Constructs a new `AnnotationProperty`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new();
    /// let anp1 = b.annotation_property("http://www.example.com".to_string());
    /// let anp2 = b.annotation_property("http://www.example.com");
    ///
    /// assert_eq!(anp1, anp2);
    /// ```
    pub fn annotation_property<S>(&self, s: S) -> AnnotationProperty
    where
        S: Into<String>,
    {
        AnnotationProperty(self.iri(s))
    }

    /// Constructs a new `DataProperty`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new();
    /// let dp1 = b.data_property("http://www.example.com".to_string());
    /// let dp2 = b.data_property("http://www.example.com");
    ///
    /// assert_eq!(dp1, dp2);
    /// ```
    pub fn data_property<S>(&self, s: S) -> DataProperty
    where
        S: Into<String>,
    {
        DataProperty(self.iri(s))
    }

    /// Constructs a new `NamedIndividual`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new();
    /// let ni1 = b.named_individual("http://www.example.com".to_string());
    /// let ni2 = b.named_individual("http://www.example.com");
    ///
    /// assert_eq!(ni1, ni2);
    /// ```
    pub fn named_individual<S>(&self, s: S) -> NamedIndividual
    where
        S: Into<String>,
    {
        NamedIndividual(self.iri(s))
    }

    /// Constructs a new `Datatype`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new();
    /// let ni1 = b.datatype("http://www.example.com".to_string());
    /// let ni2 = b.datatype("http://www.example.com");
    ///
    /// assert_eq!(ni1, ni2);
    /// ```
    pub fn datatype<S>(&self, s: S) -> Datatype
    where
        S: Into<String>,
    {
        Datatype(self.iri(s))
    }
}

macro_rules! named {
    ($($(#[$attr:meta])* $name:ident),*)  => {

        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
        pub enum NamedEntityKind {
            $($name),*
        }

        /// An OWL entity that is directly resolvable to an IRI
        ///
        /// All variants in this enum are named after the struct
        /// equivalent form. The individual structs for each variant
        /// provide us types for use elsewhere in the library.
        #[derive(Debug, Eq, PartialEq, Hash)]
        pub enum NamedEntity{
            $($name($name)),*
        }

        $(
            $(#[$attr]) *
            #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub struct $name(pub IRI);

            impl From<IRI> for $name {
                fn from(iri: IRI) -> $name {
                    Self::from(&iri)
                }
            }

            impl <'a> From<&'a IRI> for $name {
                fn from(iri: &IRI) -> $name {
                    $name(iri.clone())
                }
            }

            impl From<$name> for String {
                fn from(n: $name) -> String {
                    String::from(n.0)
                }
            }

            impl <'a> From<&'a $name> for String {
                fn from(n: &$name) -> String {
                    String::from(&n.0)
                }
            }

            impl From<$name> for IRI {
                fn from(n: $name) -> IRI {
                    Self::from(&n)
                }
            }

            impl <'a> From<&'a $name> for IRI {
                fn from(n: &$name) -> IRI {
                    (n.0).clone()
                }
            }

            impl From<$name> for NamedEntity {
                fn from(n:$name) -> NamedEntity {
                    NamedEntity::$name(n)
                }
            }

            impl From<$name> for NamedEntityKind {
                fn from(_n:$name) -> NamedEntityKind {
                    NamedEntityKind::$name
                }
            }

            impl $name {
                pub fn is<I>(&self, iri: I) -> bool
                    where I:Into<IRI>
                {
                    self.0 == iri.into()
                }

                pub fn is_s<S>(&self, iri:S) -> bool
                    where S:Into<String>
                {
                    *(self.0).0 == iri.into()
                }

            }
        ) *
    }
}



named! {
    /// An OWL
    /// [Class](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Classes_and_Instances)
    /// is a group of individuals.
    ///
    /// Usually these individuals have something in common with
    /// each other.
    Class,

    /// An OWL
    /// [Datatype](https://www.w3.org/TR/owl2-primer/#Datatypes) is a
    /// specific kind of data, such as an integer, string or so forth.
    Datatype,

    /// An OWL
    /// [ObjectProperty](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Object_Properties)
    /// is a relationship between two individuals.
    ///
    /// Although the relationship is between individuals, it is most
    /// often expressed as a relationship between two classes. See
    /// `ClassExpression` for more information.
    ObjectProperty,

    /// An OWL
    /// [DataProperty](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Datatypes)
    /// is a relationship between part of an ontology and some
    /// concrete information.
    DataProperty,

    /// An OWL
    /// [AnnotationProperty](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Document_Information_and_Annotations)
    /// is a relationship between a part of an ontology and an
    /// `Annotation`.
    ///
    /// The `Annotation` describes that part of the ontology. It is
    /// not part of the description of the world, but a description of
    /// the ontology itself.
    AnnotationProperty,

    /// An OWL
    /// [NamedIndividual](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Classes_and_Instances)
    /// is an individual in the ontology which is specifically known
    /// about and can be identified by name.
    NamedIndividual
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct AnonymousIndividual(pub Rc<str>);

impl Deref for AnonymousIndividual {
    type Target=str;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Individual{
    Anonymous(AnonymousIndividual),
    Named(NamedIndividual),
}

impl Deref for Individual {
    type Target=str;

    fn deref(&self) -> &Self::Target {
        match self {
            Individual::Named(ni) => &*ni.0,
            Individual::Anonymous(ai) => &*ai,
        }
    }
}


impl From<NamedIndividual> for Individual {
    fn from(ni:NamedIndividual) -> Individual {
        Self::Named(ni)
    }
}

impl From<AnonymousIndividual> for Individual {
    fn from(ai:AnonymousIndividual) -> Individual {
        Self::Anonymous(ai)
    }
}

impl From<Rc<str>> for AnonymousIndividual {
    fn from(rc:Rc<str>) -> AnonymousIndividual {
        AnonymousIndividual(rc)
    }
}

impl From<String> for AnonymousIndividual {
    fn from(s:String) -> AnonymousIndividual {
        AnonymousIndividual(s.into())
    }

}
impl From<&IRI> for Individual {
    fn from(iri:&IRI) -> Individual {
        let ni:NamedIndividual = iri.into();
        ni.into()
    }
}

impl From<IRI> for Individual {
    fn from(iri:IRI) -> Individual {
        Individual::from(&iri)
    }
}


impl From<NamedEntity> for Axiom {
    fn from(ne: NamedEntity) -> Axiom {
        match ne {
            NamedEntity::Class(c) => Axiom::DeclareClass(DeclareClass(c)),
            NamedEntity::ObjectProperty(obp) => {
                Axiom::DeclareObjectProperty(DeclareObjectProperty(obp))
            }
            NamedEntity::AnnotationProperty(anp) => {
                Axiom::DeclareAnnotationProperty(DeclareAnnotationProperty(anp))
            }
            NamedEntity::DataProperty(dp) => Axiom::DeclareDataProperty(DeclareDataProperty(dp)),
            NamedEntity::NamedIndividual(ni) => {
                Axiom::DeclareNamedIndividual(DeclareNamedIndividual(ni))
            }
            NamedEntity::Datatype(dt) => Axiom::DeclareDatatype(DeclareDatatype(dt)),
        }
    }
}

impl From<NamedEntity> for AnnotatedAxiom {
    fn from(ne: NamedEntity) -> AnnotatedAxiom {
        let ax: Axiom = ne.into();
        ax.into()
    }
}

/// An interface providing access to any `Annotation` attached to an
/// entity.
trait Annotated {
    /// Return the annotation
    ///
    /// The returned `BTreeSet` may be empty.
    fn annotation(&self) -> &BTreeSet<Annotation>;
}

/// An interface providing access to the `AxiomKind`
///
/// An OWL ontology consists of a set of axioms of one of many
/// different kinds. These axioms all return an variant instance of
/// the `AxiomKind` enum. This is used in the API mostly to retrieve
/// instances of a certain kind.
pub trait Kinded {
    fn kind(&self) -> AxiomKind;
}

/// An `AnnotatedAxiom` is an `Axiom` with one orpmore `Annotation`.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct AnnotatedAxiom {
    pub axiom: Axiom,
    pub ann: BTreeSet<Annotation>,
}

impl AnnotatedAxiom {
    pub fn new<I>(axiom: I, ann: BTreeSet<Annotation>) -> AnnotatedAxiom
    where
        I: Into<Axiom>,
    {
        AnnotatedAxiom {
            axiom: axiom.into(),
            ann,
        }
    }

    pub fn logical_cmp(&self, other: &AnnotatedAxiom) -> Ordering {
        self.axiom.cmp(&other.axiom)
    }

    pub fn logical_partial_cmp(&self, other: &AnnotatedAxiom) -> Option<Ordering> {
        Some(self.cmp(other))
    }

    pub fn logical_eq(&self, other: &AnnotatedAxiom) -> bool {
        self.axiom == other.axiom
    }

    pub fn logical_hash<H: Hasher>(&self, state: &mut H) -> () {
        self.axiom.hash(state)
    }
}

impl From<Axiom> for AnnotatedAxiom {
    fn from(axiom: Axiom) -> AnnotatedAxiom {
        AnnotatedAxiom {
            axiom,
            ann: BTreeSet::new(),
        }
    }
}

impl Kinded for AnnotatedAxiom {
    fn kind(&self) -> AxiomKind {
        self.axiom.kind()
    }
}

/// Add `Kinded` and `From` for each axiom.
macro_rules! axiomimpl {
    ($name:ident) => {
        impl From<$name> for Axiom {
            fn from(ax: $name) -> Axiom {
                Axiom::$name(ax)
            }
        }

        impl From<$name> for AnnotatedAxiom {
            fn from(ax: $name) -> AnnotatedAxiom {
                AnnotatedAxiom::from(Axiom::from(ax))
            }
        }

        impl Kinded for $name {
            fn kind(&self) -> AxiomKind {
                AxiomKind::$name
            }
        }
    };
}

/// Define a new axiom
///
/// Axioms can be either a tuple-like or normal struct. Documentation
/// is attached as a doc attribute after.
//
// I tried extensively to pass the attribute in the more normal
// location in front of the entity, but couldn't get it too match. I
// noticed that the quick_error crate passes afterwards and it's easy
// to get to work this way. As it's an internal macro, I think this is fine.
macro_rules! axiom {
    ($name:ident ($($tt:ty),*) $(#[$attr:meta])*) =>
    {
        $(#[$attr]) *
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name($(pub $tt),*);
        axiomimpl!($name);
    };
    ($name:ident {
        $($field_name:ident: $field_type:ty),*
    }
    $(#[$attr:meta])*
    ) => {
        $(#[$attr]) *
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name
        {
            $(pub $field_name: $field_type),*,
        }

        impl $name {
            pub fn new($($field_name: $field_type),*)
                -> $name
            {
                $name {
                    $($field_name),*
                }
            }

        }
        axiomimpl!($name);
    }
}

/// Generate all the axiom data structures
//
// This macro generates all of the axioms at once (delegated to the
// axiom macro). We have to do this in one go, although it makes
// the pattern matching a pain, because we need to know all the axiom
// names at once so we can generate the AxiomKind and Axiom
// enums.
macro_rules! axioms {
    ($($(#[$attr:meta])* $name:ident $tt:tt),*)
        =>
    {
        /// Contains all different kinds of axiom
        ///
        /// Variants of this C-style enum represent all of the
        /// different axioms that can exist in the ontology. Instances
        /// of this enum are returned by all `Axiom` and other
        /// entities as part of the `Kinded` trait.
        /// See also `Axiom` which is a Enum whose variants take
        /// instances of the `Axiom`
        #[derive(Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord)]
        pub enum AxiomKind {
            $($name),*
        }

        impl AxiomKind {
            pub fn all_kinds() -> Vec<AxiomKind> {
                vec![$(AxiomKind::$name),*]
            }
        }

        impl std::fmt::Debug for AxiomKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "AxiomKind::{}",
                       match self {
                           $(
                               AxiomKind::$name => stringify!($name)
                           ),*

                       })
            }
        }

        /// An axiom
        ///
        /// This enum has variants representing the various kinds of
        /// Axiom that can be found in an OWL Ontology. An OWL axiom
        /// maps to three different entities in Horned-OWL. First is a
        /// struct (for example, `SubClassOf`) which contains the data
        /// which defines the axiom (i.e. super and sub class for
        /// `SubClassOf`). Second, is a variant of the `AxiomKind`,
        /// which is used to identify all instances of a particular
        /// kind of axiom (i.e. any `SubClassOf` axiom will return an
        /// instance of AxiomKind::SubClassOf). Finally, we have a
        /// variant of this enum, which contains one of the structs
        /// (i.e. Axiom::SubClassOf(SubClassOf)), which is used as a union
        /// type for all structs. The struct and enum variants all
        /// share identical names.
        #[derive(Clone, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Axiom{
            $($name($name)),*
        }

        impl std::fmt::Debug for Axiom {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "Axiom::{}({})",
                       match self {
                           $(
                               Axiom::$name(_) =>
                                   stringify!($name)
                           ),*
                       },
                       match self {
                           $(
                               Axiom::$name(ax) =>
                                   format!("{:?}", ax)
                           ),*
                       },
                )
            }
        }

        impl Kinded for Axiom
        {
            fn kind(&self) -> AxiomKind
            {
                match self
                {
                    $(
                        Axiom::$name(n) => n.kind()
                    ),*

                }
            }
        }

        $(
            axiom!(
                $name $tt $(#[$attr]) *
            );
        ) *
    }
}

axioms! {
    /// An annotation associated with this Ontology
    OntologyAnnotation (Annotation),

    /// Declares that an IRI is an import of this ontology
    Import(IRI),

    // Declaration Axioms

    /// Declares that an IRI represents a Class in the Ontology
    ///
    /// In OWL, entities must be declared to be of a particular
    /// type. While, OWL (and Horned-OWL) allows the use of Class in
    /// an ontology where there is no declaration, the end ontology
    /// will change profile to OWL Full.  See also the [OWL
    /// Primer](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Entity_Declarations)
    DeclareClass(Class),

    /// Declares that an IRI represents an ObjectProperty in the
    /// Ontology.
    ///
    /// See also [`DeclareClass`](struct.DeclareClass.html)
    DeclareObjectProperty(ObjectProperty),

    /// Declares that an IRI represents an AnnotationProperty in the
    /// Ontology.
    ///
    /// See also [`DeclareClass`](struct.DeclareClass.html)
    DeclareAnnotationProperty (AnnotationProperty),
    /// Declares that an IRI represents a DataProperty in the
    /// ontology.
    ///
    /// See also [`DeclareClass`](struct.DeclareClass.html)
    DeclareDataProperty (DataProperty),

    /// Declare that an IRI represents a NamedIndividual in the
    /// ontology.
    ///
    /// See also [`DeclareClass`](struct.DeclareClass.html)
    DeclareNamedIndividual (NamedIndividual),

    /// Declare that an IRI represents a Datatype in the ontology.
    ///
    DeclareDatatype(Datatype),

    // Class Axioms

    /// A subclass relationship between two `ClassExpression`.
    ///
    /// All instances of `sub_class` are also instances of
    /// `super_class`.
    SubClassOf{
        sup: ClassExpression,
        sub: ClassExpression
    },

    /// An equivalance relationship between two `ClassExpression`.
    ///
    /// All instances of `ClassExpression` are also instances
    /// of other other.
    EquivalentClasses(Vec<ClassExpression>),

    /// A disjoint relationship between two `ClassExpression`
    ///
    /// No instance of one `ClassExpression` can also be an instance
    /// of any of the others.
    DisjointClasses(Vec<ClassExpression>),

    /// A disjoint union expression between one `ClassExpression` and
    /// a set of others.
    ///
    /// See also: https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions
    DisjointUnion(Class, Vec<ClassExpression>),

    // ObjectProperty axioms

    /// A sub property relationship between two object properties.
    ///
    /// The existance of the sub property relationship between two
    /// individuals also implies the super property relationship
    /// also. The super property can also be a property chain.
    /// So, if `s` is a super property of `r` then `a r b` implies `a
    /// s b`.
    ///
    /// See also: [Property Hierarchies](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Property_Hierarchies)
    /// See also: [Property Chains](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Property_Chains)
    SubObjectPropertyOf{
        sup: ObjectPropertyExpression,
        sub: SubObjectPropertyExpression
    },

    /// An equivalent object properties relationship.
    ///
    /// States that two object properties are semantically identical
    /// to each other.
    EquivalentObjectProperties(Vec<ObjectPropertyExpression>),

    /// A disjoint object property relationship.
    ///
    /// This states that is an individual is connected by one of these
    /// object properties, it cannot be connected by any of the others.
    DisjointObjectProperties(Vec<ObjectPropertyExpression>),

    /// An inverse relationship between two object properties.
    ///
    /// If two individuals are related by one relationship, they are
    /// related by the other in the opposite direction. So, if `r` and
    /// `s` are transitive, then `a r b` implies `b r a`.
    ///
    /// See also: [Property Characteristics](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Property_Characteristics)
    InverseObjectProperties(ObjectProperty,ObjectProperty),

    /// The domain of the object property.
    ///
    /// This states that if an individual `i` has an relationship,
    /// `ope` to any other individual, then the individual `i` is an
    /// instances of `ce`
    ///
    /// See also: [Domain](https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain)
    ObjectPropertyDomain{ope:ObjectPropertyExpression, ce:ClassExpression},

    /// The range of the object property.
    ///
    /// This states that if an individual `i` is connected to be the
    /// relationship `ope`, then it is an individual of `ce`.1
    ///
    /// See also: [Domain](https://www.w3.org/TR/owl2-syntax/#Object_Property_Range)
    ObjectPropertyRange{ope:ObjectPropertyExpression, ce:ClassExpression},

    /// The functional characteristic.
    ///
    /// This states that if for a given individual `i`, there can be
    /// only one individual `j` connected to `i` by this object
    /// property expression.
    ///
    /// See also: [Functional](https://www.w3.org/TR/owl2-syntax/#Functional_Object_Properties)
    FunctionalObjectProperty(ObjectPropertyExpression),

    /// The inverse functional characteristic
    ///
    /// This states that for each individual `i`, there can be at most
    /// one individual `j` connected to `i` via this object property
    /// expression.
    ///
    /// See also: [Inverse Functional](https://www.w3.org/TR/owl2-syntax/#Inverse-Functional_Object_Properties)
    InverseFunctionalObjectProperty(ObjectPropertyExpression),

    /// The reflexive characteristic
    ///
    /// Every individual that is connected via the
    /// ObjectPropertyExpression is connected to itself.
    ///
    /// See also: [Reflexive](https://www.w3.org/TR/owl2-syntax/#Reflexive_Object_Properties)
    ReflexiveObjectProperty(ObjectPropertyExpression),

    /// The irreflexive characteristic
    ///
    /// No individual can be connected to itself by this property.
    ///
    /// See also: [Irreflexive](https://www.w3.org/TR/owl2-syntax/#Irreflexive_Object_Properties)
    IrreflexiveObjectProperty(ObjectPropertyExpression),

    /// The symmetric characteristic
    ///
    /// If an individual `i` is connected to `j` by this
    /// ObjectPropertyExpression, then `j` is also connected by `i`
    /// See also: [Symmetric](https://www.w3.org/TR/owl2-syntax/#Symmetric_Object_Properties)
    SymmetricObjectProperty(ObjectPropertyExpression),

    /// The asymmetric characteristic.
    ///
    /// if an individual `i` is connected to `j` by this
    /// ObjectPropertyExpression, then `j` cannot be connected to `i`
    /// by the ObjectPropertyExpression.
    ///
    /// See also: [Asymmetric](https://www.w3.org/TR/owl2-syntax/#Asymmetric_Object_Properties)
    AsymmetricObjectProperty(ObjectPropertyExpression),

    /// A transitive relationship between two object properties.
    ///
    /// When `r` is transitive, then `a r b`, and `b r c` implies `a r
    /// c` also.
    ///
    /// See also: [TransitiveObjectProperty](https://www.w3.org/TR/owl2-syntax/#Transitive_Object_Properties)
    TransitiveObjectProperty(ObjectPropertyExpression),

    /// A sub data property relationship.
    ///
    /// The existence of the `sub_property` relationship also implies
    /// the existence of the `super_property`.
    ///
    /// See also: [Data Subproperties](https://www.w3.org/TR/owl2-syntax/#Data_Subproperties)
    SubDataPropertyOf {
        sup:DataProperty,
        sub:DataProperty
    },

    /// An equivalent data property relationship.
    ///
    /// All these DataProperties are semantically identical.
    ///
    /// See also: [EquivalentDataproperties](https://www.w3.org/TR/owl2-syntax/#Equivalent_Data_Properties)
    EquivalentDataProperties(Vec<DataProperty>),

    /// A disjoint data property relationship.
    ///
    /// No individual can be connected to a data property expression
    /// by more than one of these DataProperty relations.
    ///
    /// See also: [DisjointDataProperties](https://www.w3.org/TR/owl2-syntax/#Disjoint_Data_Properties)
    DisjointDataProperties(Vec<DataProperty>),

    /// The domain of a DataProperty.
    ///
    /// If an individual `i` has a relationship `dp` then `i` must be
    /// of type `ce`.
    ///
    /// See also: [Data Property Domain](https://www.w3.org/TR/owl2-syntax/#Disjoint_Data_Properties)
    DataPropertyDomain{dp:DataProperty,ce:ClassExpression},

    /// The range of a DataProperty.
    ///
    /// If in individual `i` has a relationship `dp` with some literal
    /// `l`, then `l` must by in `dr`.
    ///
    /// See also: [Data Property Range](https://www.w3.org/TR/owl2-syntax/#Data_Property_Range)
    DataPropertyRange{dp:DataProperty,dr:DataRange},

    /// The functional DataProperty characteristic.
    ///
    /// Any individual `i` can only be connected to a single literal
    /// by this DataProperty.
    ///
    /// See also: [Functional Data Property]:(https://www.w3.org/TR/owl2-syntax/#Functional_Data_Properties)
    FunctionalDataProperty(DataProperty),

    /// Defintion of a datatype.
    ///
    /// See also: [Datatype Definitions](https://www.w3.org/TR/owl2-syntax/#Datatype_Definitions)
    DatatypeDefinition {
        kind: Datatype,
        range: DataRange
    },

    /// A key
    ///
    /// An individual `i` which is of type `ce` can be uniquely
    /// identified by `pe`. Keys can only be applied to individuals
    /// which are explicitly named in the ontology, not those that are
    /// inferred.
    ///
    /// See also: [Keys](https://www.w3.org/TR/owl2-syntax/#Keys)
    HasKey{ce:ClassExpression, vpe:Vec<PropertyExpression>},

    // Assertions
    /// A same individual expression.
    ///
    /// See also: [Individual Equality](https://www.w3.org/TR/owl2-syntax/#Individual_Equality)
    SameIndividual (
        Vec<Individual>
    ),

    /// A different individuals expression.
    ///
    /// See also: [Individual Inequality](https://www.w3.org/TR/owl2-syntax/#Individual_Inequality)
    DifferentIndividuals (
        Vec<Individual>
    ),

    /// A class assertion expression.
    ///
    /// States that `i` is in class `ce`.
    ///
    /// See also: [Class Assertions](https://www.w3.org/TR/owl2-syntax/#Class_Assertions)
    ClassAssertion {
        ce: ClassExpression,
        i: Individual
    },

    /// An object property assertion.
    ///
    /// Individual `from` is connected `to` by `ope`.
    ///
    /// See also: [Positive Object Property Assertions](https://www.w3.org/TR/owl2-syntax/#Positive_Object_Property_Assertions)
    ObjectPropertyAssertion {
        ope: ObjectPropertyExpression,
        from: Individual,
        to: Individual
    },

    /// A negative object property assertion.
    ///
    /// Individual `from` is not connected `to` by `ope`
    ///
    /// See also: [Negative Object Property Assertions](https://www.w3.org/TR/owl2-syntax/#Negative_Object_Property_Assertions)
    NegativeObjectPropertyAssertion {
        ope: ObjectPropertyExpression,
        from: Individual,
        to: Individual
    },

    /// A data property assertion.
    ///
    /// Individual `from` is connected `to`` literal by `dp`.
    ///
    /// See also: [Data Property Assertion](https://www.w3.org/TR/owl2-syntax/#Positive_Data_Property_Assertions)
    DataPropertyAssertion {
        dp: DataProperty,
        from: Individual,
        to: Literal
    },

    /// A negative data property assertion.
    ///
    /// Individual `from` is not connected `to` literal by `dp`.
    ///
    /// See also [Negative Data Property Assertions](https://www.w3.org/TR/owl2-syntax/#Negative_Data_Property_Assertions)
    NegativeDataPropertyAssertion {
        dp: DataProperty,
        from: Individual,
        to: Literal
    },

    // Annotation Axioms
    /// An annotation assertion axiom
    ///
    /// States that `annotation` applies to the
    /// `annotation_subject`. Annotations refer to an `IRI` rather
    /// than the `NamedEntity` identified by that `IRI`.
    AnnotationAssertion {
        subject: Individual,
        ann: Annotation
    },

    /// An sub-property assertion for annotation properties.
    ///
    /// Implies that any annotation of the type `sub_property` is also
    /// an annotation of the type `super_property`.
    SubAnnotationPropertyOf {
        sup: AnnotationProperty,
        sub: AnnotationProperty
    },

    /// Assert the domain of an `AnnotationProperty`
    AnnotationPropertyDomain {
        ap: AnnotationProperty,
        iri: IRI
    },

    /// Assert the range of an `AnnotationProperty`
    AnnotationPropertyRange {
        ap: AnnotationProperty,
        iri: IRI
    }
}

// Non-axiom data structures associated with OWL
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Literal {
    // Simple Literals are syntactic sugar for a Datatype with type:
    // http://www.w3.org/2001/XMLSchema#string
    Simple { literal: String },
    // Language-tagged literals have a lang tag and must be (or have
    // an implicit) of datatype
    // http://www.w3.org/1999/02/22-rdf-syntax-ns#langString
    Language { literal: String, lang: String },
    Datatype { literal: String, datatype_iri: IRI },
}

impl Literal {
    pub fn literal(&self) -> &String {
        match self {
            Literal::Simple { literal } => literal,
            Literal::Language { literal, .. } => literal,
            Literal::Datatype { literal, .. } => literal,
        }
    }
}

// #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
// pub struct Literal {
//     pub datatype_iri: Option<IRI>,
//     pub lang: Option<String>,
//     pub literal: Option<String>,
// }

/// Data associated with a part of the ontology.
///
/// Annotations are associated an IRI and describe that IRI in a
/// particular way, defined by the property.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Annotation {
    pub ap: AnnotationProperty,
    pub av: AnnotationValue,
}

/// The value of an annotation
///
/// This Enum is currently not complete.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AnnotationValue {
    Literal(Literal),
    IRI(IRI),
}

impl From<Literal> for AnnotationValue {
    fn from(literal: Literal) -> AnnotationValue {
        AnnotationValue::Literal(literal)
    }
}

impl From<IRI> for AnnotationValue {
    fn from(iri: IRI) -> AnnotationValue {
        AnnotationValue::IRI(iri)
    }
}

/// A object property expression
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ObjectPropertyExpression {
    ObjectProperty(ObjectProperty),
    InverseObjectProperty(ObjectProperty),
}

impl From<ObjectProperty> for ObjectPropertyExpression {
    fn from(op: ObjectProperty) -> ObjectPropertyExpression {
        ObjectPropertyExpression::ObjectProperty(op)
    }
}

impl From<IRI> for ObjectPropertyExpression {
    fn from(iri: IRI) -> ObjectPropertyExpression {
        let op: ObjectProperty = iri.into();
        op.into()
    }
}

impl From<&IRI> for ObjectPropertyExpression {
    fn from(iri: &IRI) -> ObjectPropertyExpression {
        iri.clone().into()
    }
}

impl ObjectPropertyExpression {
    pub fn as_property(&self) -> Option<&ObjectProperty> {
        match self {
            ObjectPropertyExpression::ObjectProperty(op) => Some(op),
            ObjectPropertyExpression::InverseObjectProperty(_) => None,
        }
    }
}

/// A sub-object property expression
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SubObjectPropertyExpression {
    // We use Vec here rather than BTreeSet because, perhaps
    // surprisingly, BTreeSet is not itself hashable.
    ObjectPropertyChain(Vec<ObjectPropertyExpression>),
    ObjectPropertyExpression(ObjectPropertyExpression),
}

impl From<ObjectPropertyExpression> for SubObjectPropertyExpression {
    fn from(ope: ObjectPropertyExpression) -> SubObjectPropertyExpression {
        SubObjectPropertyExpression::ObjectPropertyExpression(ope)
    }
}

/// A property expression
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum PropertyExpression {
    ObjectPropertyExpression(ObjectPropertyExpression),
    DataProperty(DataProperty),
    AnnotationProperty(AnnotationProperty),
}

impl From<ObjectPropertyExpression> for PropertyExpression {
    fn from(ope: ObjectPropertyExpression) -> PropertyExpression {
        PropertyExpression::ObjectPropertyExpression(ope)
    }
}
impl From<DataProperty> for PropertyExpression {
    fn from(dp: DataProperty) -> PropertyExpression {
        PropertyExpression::DataProperty(dp)
    }
}

// Data!!!
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FacetRestriction {
    pub f: Facet,
    pub l: Literal,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Facet {
    Length,
    MinLength,
    MaxLength,
    Pattern,
    MinInclusive,
    MinExclusive,
    MaxInclusive,
    MaxExclusive,
    TotalDigits,
    FractionDigits,
    LangRange,
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum DataRange {
    Datatype(Datatype),
    DataIntersectionOf(Vec<DataRange>),
    DataUnionOf(Vec<DataRange>),
    DataComplementOf(Box<DataRange>),
    DataOneOf(Vec<Literal>),
    DatatypeRestriction(Datatype, Vec<FacetRestriction>),
}

impl From<Datatype> for DataRange {
    fn from(dr: Datatype) -> DataRange {
        DataRange::Datatype(dr)
    }
}

/// A class expression
///
/// As well as a named class, it is possible to define classes of
/// individuals based on these class constructors.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ClassExpression {
    /// A named class
    Class(Class),

    /// The boolean and
    ///
    /// The class of individuals which are individuals of all these
    /// classes.
    ObjectIntersectionOf(Vec<ClassExpression>),

    /// The boolean or
    ///
    /// The class of individuals which are individuals of any of these
    /// classes.
    ObjectUnionOf(Vec<ClassExpression>),

    /// The boolean not
    ///
    /// The class of individuals which are not individuals of any of
    /// these classes.
    ObjectComplementOf(Box<ClassExpression>),

    /// An enumeration of individuals
    ///
    /// This is the class containing exactly the given set of
    /// individuals.
    ObjectOneOf(Vec<Individual>),

    /// An existential relationship
    ///
    /// This is the anonymous class of individuals `i`, which have the
    /// relationship `o` to a class expression `ce`. Every individual
    /// in `i` must have this relationship to one individual in `ce`.
    ObjectSomeValuesFrom {
        ope: ObjectPropertyExpression,
        bce: Box<ClassExpression>,
    },

    /// A universal relationship
    ///
    /// This is the anonymous class of individuals `i` where all
    /// individuals which are related by `o` are instances of
    /// `ce`. This does not imply that the `i` necessarily has any
    /// relation `r`.
    ObjectAllValuesFrom {
        ope: ObjectPropertyExpression,
        bce: Box<ClassExpression>,
    },

    /// An existential relationship to an individual
    ///
    /// This is the class of individuals `c` which have the
    /// relationship `o` to another individual `i`. Every individual
    /// in `c` must have this relationship to the individual `i`
    ObjectHasValue {
        ope: ObjectPropertyExpression,
        i: Individual,
    },

    /// The class of individuals which have a relation to themselves
    ///
    /// Given a object property `r`, this class defines all the
    /// individuals where `i r i`.
    ObjectHasSelf(ObjectPropertyExpression),

    /// A min cardinality relationship between individuals
    ///
    /// Given an object property `o` and a class `ce`, this describes
    /// the class of individuals which have the `o` relationship to at
    /// least `n` other individuals.
    ObjectMinCardinality {
        n: u32,
        ope: ObjectPropertyExpression,
        bce: Box<ClassExpression>,
    },

    /// A max cardinality relationship between individuals
    ///
    /// Given an object property `o` and a class `ce`, this describes
    /// the class of individuals which have the `o` relationship to at
    /// most `n` other individuals.
    ObjectMaxCardinality {
        n: u32,
        ope: ObjectPropertyExpression,
        bce: Box<ClassExpression>,
    },

    /// An exact cardinality relationship between individuals
    ///
    /// Given an object property `o` and a class `ce`, this describes
    /// the class of individuals which have the `o` relationship to exactly
    /// `n` other individuals.
    ObjectExactCardinality {
        n: u32,
        ope: ObjectPropertyExpression,
        bce: Box<ClassExpression>,
    },

    /// An exististential relationship.
    ///
    /// This is the anonymous class of individuals `i` which have the
    /// relationship `dp` to the data range, `dr`. Every individual
    /// `i` must have this relationship to data constrainted by `dr`.
    ///
    /// See also: [Existential Quantification](https://www.w3.org/TR/owl2-syntax/#Existential_Quantification_2)
    DataSomeValuesFrom { dp: DataProperty, dr: DataRange },

    /// A universal relationship.
    ///
    /// This is the anonymous class of individuals `i` which if they
    /// have a relationship `dp` to some data, then that must be of
    /// type `dr`.
    ///
    /// See also [Universal Quantification](https://www.w3.org/TR/owl2-syntax/#Universal_Quantification_2)
    DataAllValuesFrom { dp: DataProperty, dr: DataRange },

    /// A has-value relationship.

    /// This is the class of individuals, `i`, which have the
    /// relationship `dp` to exactly the literal `l`.

    /// See also [Value Restriction](https://www.w3.org/TR/owl2-syntax/#Literal_Value_Restriction)
    DataHasValue { dp: DataProperty, l: Literal },

    /// A minimum cardinality restriction

    /// The class of individuals have at least `n` relationships of
    /// the kind `dp` to a given data range `dr`.

    /// See also [Min Cardinality](https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality_2)
    DataMinCardinality {
        n: u32,
        dp: DataProperty,
        dr: DataRange,
    },

    /// A max cardinality restriction

    /// The class of individuals have at most `n` relationships of
    /// the kind `dp` to a given data range `dr`.

    /// See also [Max Cardinality](https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality_2)
    DataMaxCardinality {
        n: u32,
        dp: DataProperty,
        dr: DataRange,
    },

    /// An exact cardinality restriction

    /// The class of individuals have exactly `n` relationships of
    /// the kind `dp` to a given data range `dr`.

    /// See also [Exactly Cardinality](https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality_2)
    DataExactCardinality {
        n: u32,
        dp: DataProperty,
        dr: DataRange,
    },
}

impl From<Class> for ClassExpression {
    fn from(c: Class) -> ClassExpression {
        ClassExpression::Class(c)
    }
}

impl<'a> From<&'a Class> for ClassExpression {
    fn from(c: &'a Class) -> ClassExpression {
        ClassExpression::Class(c.clone())
    }
}

impl From<Class> for Box<ClassExpression> {
    fn from(c: Class) -> Box<ClassExpression> {
        Box::new(c.into())
    }
}

/// An ontology identifier
///
/// An ontology is identified by an IRI which is expected to remain
/// stable over the lifetime of the ontology, and a version IRI which
/// is expected to change between versions.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct OntologyID {
    pub iri: Option<IRI>,
    pub viri: Option<IRI>,
}

/// Access or change the `OntologyID` of an `Ontology`
pub trait Ontology {
    fn id(&self) -> &OntologyID;
    fn mut_id(&mut self) -> &mut OntologyID;
    fn doc_iri(&self) -> &Option<IRI>;
    fn mut_doc_iri(&mut self) -> &Option<IRI>;
}

/// Add or remove axioms to an `MutableOntology`
pub trait MutableOntology {
    /// Insert an axiom into the ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// # use horned_owl::ontology::set::SetOntology;
    /// let mut o = SetOntology::new();
    /// let b = Build::new();
    /// o.insert(DeclareClass(b.class("http://www.example.com/a")));
    /// o.insert(DeclareObjectProperty(b.object_property("http://www.example.com/r")));
    /// ```
    ///
    /// See `declare` for an easier way to declare named entities.
    fn insert<A>(&mut self, ax: A) -> bool
    where
        A: Into<AnnotatedAxiom>;

    fn remove(&mut self, ax: &AnnotatedAxiom) -> bool {
        self.take(ax).is_some()
    }

    fn take(&mut self, ax: &AnnotatedAxiom) -> Option<AnnotatedAxiom>;

    /// Declare an NamedEntity for the ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// # use horned_owl::ontology::set::SetOntology;
    /// let mut o = SetOntology::new();
    /// let b = Build::new();
    /// o.declare(b.class("http://www.example.com/a"));
    /// o.declare(b.object_property("http://www.example.com/r"));
    /// ```
    fn declare<N>(&mut self, ne: N) -> bool
    where
        N: Into<NamedEntity>,
    {
        let ne: NamedEntity = ne.into();
        let ax: Axiom = ne.into();
        self.insert(ax)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ontology::axiom_mapped::AxiomMappedOntology;

    #[test]
    fn test_iri_from_string() {
        let build = Build::new();
        let iri = build.iri("http://www.example.com");

        assert_eq!(String::from(iri), "http://www.example.com");
    }

    #[test]
    fn test_iri_creation() {
        let build = Build::new();

        let iri1 = build.iri("http://example.com".to_string());

        let iri2 = build.iri("http://example.com".to_string());

        // these are equal to each other
        assert_eq!(iri1, iri2);

        // these are the same object in memory
        assert!(Rc::ptr_eq(&iri1.0, &iri2.0));

        // iri1, iri2 and one in the cache == 3
        assert_eq!(Rc::strong_count(&iri1.0), 3);
    }

    #[test]
    fn test_iri_string_creation() {
        let build = Build::new();

        let iri_string = build.iri("http://www.example.com".to_string());
        let iri_static = build.iri("http://www.example.com");
        let iri_from_iri = build.iri(iri_static.clone());

        let s = "http://www.example.com";
        let iri_str = build.iri(&s[..]);

        assert_eq!(iri_string, iri_static);
        assert_eq!(iri_string, iri_str);
        assert_eq!(iri_static, iri_str);
        assert_eq!(iri_from_iri, iri_str);
    }

    #[test]
    fn test_class() {
        let mut o = AxiomMappedOntology::default();
        let c = Build::new().class("http://www.example.com");
        o.insert(DeclareClass(c));

        assert_eq!(o.i().declare_class().count(), 1);
    }

    #[test]
    fn test_class_declare() {
        let c = Build::new().class("http://www.example.com");

        let mut o = AxiomMappedOntology::default();
        o.declare(c);

        assert_eq!(o.i().declare_class().count(), 1);
    }

    #[test]
    fn test_class_convertors() {
        let c = Build::new().class("http://www.example.com");
        let i = Build::new().iri("http://www.example.com");

        let i1: IRI = c.clone().into();
        assert_eq!(i, i1);

        let c1: Class = Class::from(i);
        assert_eq!(c, c1);

        let ne: NamedEntity = c.clone().into();
        assert_eq!(ne, NamedEntity::Class(c));
    }

    #[test]
    fn test_class_string_ref() {
        let s = String::from("http://www.example.com");
        let c = Build::new().class(s.clone());

        assert!(c.is_s(s));
    }

    #[test]
    fn test_is() {
        let c = Build::new().class("http://www.example.com");
        let i = Build::new().named_individual("http://www.example.com");
        let iri = Build::new().iri("http://www.example.com");

        assert!(c.is(iri));
        assert!(c.is(i.clone()));
        assert!(i.is(c));
    }

    #[test]
    fn test_axiom_convertors() {
        let c = Build::new().class("http://www.example.com");

        let dc = DisjointClasses(vec![c.clone().into(), c.clone().into()]);
        let _aa: Axiom = dc.into();
    }

    #[test]
    fn test_object_property_expression() {
        let b = Build::new();
        let obp = b.object_property("http://www.example.com");

        let obpe = ObjectPropertyExpression::ObjectProperty(obp.clone());

        assert_eq!(obpe.as_property().unwrap(), &obp);
    }

    #[test]
    fn test_axiom_equality() {
        let b = Build::new();

        let ann = Annotation {
            ap: b.annotation_property("http://www.example.com/ap"),
            av: b.iri("http://www.example.com/av").into()
        };

        let mut decl1:AnnotatedAxiom =
            DeclareClass(b.class("http://www.example.com#a")).into();
        let decl2 = decl1.clone();

        assert_eq!(decl1, decl2);


        decl1.ann.insert(ann);
        assert!(!(decl1 == decl2));
    }
}
