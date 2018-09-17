#![allow(dead_code)]

//! # Horned-OWL
//!
//! Horned-OWL is a library for the reading, manipulation and
//! generation of
//! [OWL](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/)
//! ontologies. As well as a library, it offers a number of
//! command-line tools for performing the same.
//!
//! The focus of this library is on performance, compared to the [OWL
//! API](https://github.com/owlcs/owlapi). Currently, on IO tasks, it
//! is between 1 and 2 orders of magnitude faster.
//!
//! # Author
//!
//! This library is written by Phillip Lord <phillip.lord@newcastle.ac.uk>
//!
//! # Status
//!
//! At the moment, the library is in early stages and it is not a
//! complete implementation of the OWL specification.
//!
//! # Roadmap
//!
//! I am working incrementally toward a full implementation. I plan to
//! add semantics in a demand-led way (i.e. as I need it!). Currently,
//! serialisation is to/from OWL Presentation format only, but RDF
//! serialisation will be supported.
//!
//! - 0.3 Add vocab.rs
//! - 0.4 Infrastructure for parsable command line tools
//! - 0.5 More command line tools
//! - 0.6 OWL Primer ontology read/write
//! - 0.7 RDF IO

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::cmp::Ordering;
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
pub struct IRI(Rc<String>);

impl Deref for IRI{
    type Target = String;

    fn deref(&self) -> &String{
        &self.0
    }
}

impl From<IRI> for String{
    fn from(i:IRI) -> String {
        // Clone Rc'd value
        (*i.0).clone()
    }
}

impl <'a> From<&'a IRI> for String {
    fn from(i:&'a IRI) -> String {
        (*i.0).clone()
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

impl Build{
    pub fn new() -> Build{
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
        where S: Into<String>
    {
        let iri = IRI(Rc::new(s.into()));

        let mut cache = self.0.borrow_mut();
        if cache.contains(&iri){
            return cache.get(&iri).unwrap().clone()
        }

        cache.insert(iri.clone());
        return iri;
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
    pub fn class<S>(&self, s:S) -> Class
        where S: Into<String>
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
    pub fn object_property<S>(&self, s:S) -> ObjectProperty
        where S: Into<String>
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
    pub fn annotation_property<S>(&self, s:S)-> AnnotationProperty
        where S: Into<String>
    {
        AnnotationProperty(self.iri(s))
    }
}

macro_rules! named {
    ($($(#[$attr:meta])* $name:ident),*)  => {

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

        ) *
    }
}

named!{
    /// An OWL
    /// [Class](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Classes_and_Instances)
    /// is a group of individuals.
    ///
    /// Usually these individuals have something in common with
    /// each other.
    Class,

    /// An OWL
    /// [ObjectProperty](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Object_Properties)
    /// is a relationship between two individuals.
    ///
    /// Although the relationship is between individuals, it is most
    /// often expressed as a relationship between two classes. See
    /// `ClassExpression` for more information.
    ObjectProperty,

    /// An OWL
    /// [AnnotationProperty](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Document_Information_and_Annotations)
    /// is a relationship between a part of an ontology and an
    /// `Annotation`.
    ///
    /// The `Annotation` describes that part of the ontology. It is
    /// not part of the description of the world, but a description of
    /// the ontology itself.
    AnnotationProperty
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
trait Kinded {
    fn kind(&self) -> AxiomKind;
}

/// An `AnnotatedAxiom` is an `Axiom` with one or more `Annotation`.
///
/// An `AnnotatedAxiom` takes it ordering, hash and equality semantics
/// only from the underlying `Axiom` and not from the `Annotation`.
#[derive(Debug)]
pub struct AnnotatedAxiom{
    pub axiom: Axiom,
    pub annotation: BTreeSet<Annotation>
}

impl AnnotatedAxiom {
    pub fn new(axiom: Axiom, annotation: BTreeSet<Annotation>)
        -> AnnotatedAxiom {
        AnnotatedAxiom{axiom, annotation}
    }
}

impl Ord for AnnotatedAxiom {
    fn cmp(&self, other: &AnnotatedAxiom) -> Ordering {
        self.axiom.cmp(&other.axiom)
    }
}

impl PartialOrd for AnnotatedAxiom {
    fn partial_cmp(&self, other: &AnnotatedAxiom) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for AnnotatedAxiom{}

impl PartialEq for AnnotatedAxiom {
    fn eq(&self, other: &AnnotatedAxiom) -> bool {
        self.axiom == other.axiom
    }
}

impl Hash for AnnotatedAxiom {
    fn hash<H: Hasher>(&self, state: &mut H) -> () {
        self.axiom.hash(state)
    }
}

impl From<Axiom> for AnnotatedAxiom {
    fn from(axiom: Axiom) -> AnnotatedAxiom {
        AnnotatedAxiom {
            axiom: axiom,
            annotation: BTreeSet::new()
        }
    }
}

impl Kinded for AnnotatedAxiom {
    fn kind(&self) -> AxiomKind {
        self.axiom.kind()
    }
}

// Macro implementations. The core data model of horned is fairly
// duplicative -- an axiom, for instance, has three different Rust
// entities: a struct representing the data, an enum variant which can
// contain the struct, and a empty enum variant that can be used to
// describe one or the other of these types and operate as a key for
// the kind.

/// Return all axioms of a specific `AxiomKind`
#[allow(unused_macros)]
macro_rules! on {
    ($ont:ident, $kind:ident)
        => {
            $ont.axiom(AxiomKind::$kind)
                .map(|ax|
                     {
                         match ax {
                             Axiom::$kind(n) => n,
                             _ => panic!()
                         }
                     })
        }
}

/// Add a method to `Ontology` which returns axioms of a specific
/// `AxiomKind`.
#[allow(unused_macros)]
macro_rules! onimpl {
    ($kind:ident, $method:ident)
        =>
    {
        onimpl!($kind, $method, stringify!($kind));
    };
    ($kind:ident, $method:ident, $skind:expr)
        =>
    {
        impl Ontology {

            #[doc = "Return all instances of"]
            #[doc = $skind]
            #[doc = "in the ontology."]
            pub fn $method(&self)
                       -> impl Iterator<Item=&$kind> {
                on!(self, $kind)
            }
        }
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
                AnnotatedAxiom::from(
                    Axiom::from(ax))
            }
        }

        impl Kinded for $name {
            fn kind(&self) -> AxiomKind {
                AxiomKind::$name
            }
        }
    }
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
    ($name:ident ($($tt:tt),*) $(#[$attr:meta])*) =>
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
        #[derive(Clone, Copy, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
        pub enum AxiomKind {
            $($name),*
        }

        /// An axiom
        ///
        /// This enum has variants representing the various kinds of
        /// Axiom that can be found in an OWL Ontology. An OWL axiom
        /// maps to three different entities in Horned-OWL. First is a
        /// struct (for example, `SubClass`) which contains the data
        /// which defines the axiom (i.e. super and sub class for
        /// `SubClass`). Second, is a variant of the `AxiomKind`,
        /// which is used to identify all instances of a particular
        /// kind of axiom (i.e. any `SubClass` axiom will return an
        /// instance of AxiomKind::SubClass). Finally, we have a
        /// variant of this enum, which contains one of the structs
        /// (i.e. Axiom::SubClass(SubClass)), which is used as a union
        /// type for all structs. The struct and enum variants all
        /// share identical names.
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Axiom{
            $($name($name)),*
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

axioms!{
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

    // Class Axioms

    /// A subclass relationship between two `ClassExpression`.
    ///
    /// All instances of `sub_class` are also instances of
    /// `super_class`.
    SubClass{
        super_class: ClassExpression,
        sub_class: ClassExpression
    },

    /// An equivalance relationship between two `ClassExpression`.
    ///
    /// All instances of the two `ClassExpression` are also instances
    /// of other other.
    EquivalentClass(ClassExpression,ClassExpression),

    /// A disjoint relationship between two `ClassExpression`
    ///
    /// No instance of one `ClassExpression` can also be an instance
    /// of the other.
    DisjointClass(ClassExpression,ClassExpression),

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
    SubObjectProperty{
        super_property:ObjectPropertyExpression,
        sub_property:ObjectProperty
    },

    /// An inverse relationship between two object properties.
    ///
    /// If two individuals are related by one relationship, they are
    /// related by the other in the opposite direction. So, if `r` and
    /// `s` are transitive, then `a r b` implies `b r a`.
    ///
    /// See also: [Property Characteristics](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Property_Characteristics)
    InverseObjectProperty(ObjectProperty,ObjectProperty),

    /// A transitive relationship between two object properties.
    ///
    /// When `r` is transitive, then `a r b`, and `b r c` implies `a r
    /// c` also.
    TransitiveObjectProperty(ObjectProperty),

    // Annotation Axioms
    /// An annotation assertion axiom
    ///
    /// States that `annotation` applies to the
    /// `annotation_subject`. Annotations refer to an `IRI` rather
    /// than the `NamedEntity` identified by that `IRI`.
    AssertAnnotation {
        annotation_subject:IRI,
        annotation: Annotation
    },

    /// An sub-property assertion for annotation properties.
    ///
    /// Implies that any annotation of the type `sub_property` is also
    /// an annotation of the type `super_property`.
    SubAnnotationProperty {
        super_property:AnnotationProperty,
        sub_property: AnnotationProperty
    },

    /// An annotation associated with this Ontology
    OntologyAnnotation (Annotation)
}

// In the ideal world, we would have generated these onimpl! calls as
// part of the axiom macro. This should be possible, as their is a
// fixed relationship between the struct name and the method name.
// But rust won't let us generate new identifiers or make string like
// manipulations on the them. So we can't.
//
// "Whoever does not understand LISP is doomed to reinvent it" (badly)
onimpl!{DeclareClass, declare_class}
onimpl!{DeclareObjectProperty, declare_object_property}
onimpl!{DeclareAnnotationProperty, declare_annotation_property}
onimpl!{SubClass, sub_class}
onimpl!{EquivalentClass, equivalent_class}
onimpl!{DisjointClass, disjoint_class}
onimpl!{SubObjectProperty, sub_object_property}
onimpl!{InverseObjectProperty, inverse_object_property}
onimpl!{TransitiveObjectProperty, transitive_object_property}
onimpl!{AssertAnnotation, assert_annotation}
onimpl!{SubAnnotationProperty, sub_annotation_property}
onimpl!{OntologyAnnotation, ontology_annotation}


// Non-axiom data structures associated with OWL

/// Data associated with a part of the ontology.
///
/// Annotations are associated an IRI and describe that IRI in a
/// particular way, defined by the property.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Annotation {
    pub annotation_property: AnnotationProperty,
    pub annotation_value: AnnotationValue
}

/// The value of an annotation
///
/// This Enum is currently not complete.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AnnotationValue {
    PlainLiteral{
        datatype_iri: Option<IRI>,
        lang: Option<String>,
        literal: Option<String>,
    },
    IRI(IRI)
}

/// A object property expression
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ObjectPropertyExpression {
    // We use Vec here rather than BTreeSet because, perhaps
    // surprisingly, BTreeSet is not itself hashable.
    ObjectPropertyChain(Vec<ObjectProperty>),
    ObjectProperty(ObjectProperty)
}


/// A class expression
///
/// As well as a named class, it is possible to define classes of
/// individuals based on these class constructors.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ClassExpression
{
    /// A named class
    Class(Class),

    /// An existential relationship
    ///
    /// This is the anonymous class of individuals `i`, which have the
    /// relationship `o` to a class expression `ce`. Every individual
    /// in `i` must have this relationship to one individual in `ce`.
    Some{o:ObjectProperty, ce:Box<ClassExpression>},

    /// A universal relationship
    ///
    /// This is the anonymous class of individuals `i` where all
    /// individuals which are related by `o` are instances of
    /// `ce`. This does not imply that the `i` necessarily has any
    /// relation `r`.
    Only{o:ObjectProperty, ce:Box<ClassExpression>},

    /// The boolean and
    ///
    /// The class of individuals which are individuals of all these
    /// classes.
    And{o:Vec<ClassExpression>},

    /// The boolean or
    ///
    /// The class of individuals which are individuals of any of these
    /// classes.
    Or{o:Vec<ClassExpression>},

    /// The boolean not
    ///
    /// The class of individuals which are not individuals of any of
    /// these classes.
    Not{ce:Box<ClassExpression>}
}

impl From<Class> for ClassExpression {
    fn from(c:Class) -> ClassExpression {
        ClassExpression::Class(c)
    }
}

impl <'a> From<&'a Class> for ClassExpression {
    fn from(c:&'a Class) -> ClassExpression {
        ClassExpression::Class(c.clone())
    }
}

/// An ontology identifier
///
/// An ontology is identified by an IRI which is expected to remain
/// stable over the lifetime of the ontology, and a version IRI which
/// is expected to change between versions.
#[derive(Debug, Default, Eq, PartialEq)]
pub struct OntologyID{
    pub iri: Option<IRI>,
    pub viri: Option<IRI>,
}

/// An ontology
///
/// An ontology consists of a identifier and set of axiom
#[derive(Debug, Default, Eq, PartialEq)]
pub struct Ontology
{
    pub id: OntologyID,
    // The use an BTreeMap keyed on AxiomKind allows efficient
    // retrieval of axioms. Otherwise, we'd have to iterate through
    // the lot every time.
    axiom: RefCell<BTreeMap<AxiomKind,BTreeSet<AnnotatedAxiom>>>,
}

impl Ontology {

    /// Fetch the axioms hashmap as a raw pointer.
    ///
    /// This method also ensures that the BTreeSet for `axk` is
    /// instantiated, which means that it effects equality of the
    /// ontology. It should only be used where the intention is to
    /// update the ontology.
    fn axioms_as_ptr(&self, axk: AxiomKind)
        -> *mut BTreeMap<AxiomKind,BTreeSet<AnnotatedAxiom>>
    {
        self.axiom.borrow_mut().entry(axk)
            .or_insert(BTreeSet::new());
        self.axiom.as_ptr()
    }

    /// Fetch the axioms for the given kind.
    fn set_for_kind(&self, axk: AxiomKind)
                     -> Option<&BTreeSet<AnnotatedAxiom>>
    {
        unsafe{
            (*self.axiom.as_ptr())
                .get(&axk)
        }
    }

    /// Fetch the axioms for given kind as a mutable ref.
    fn mut_set_for_kind(&mut self, axk: AxiomKind)
                        -> &mut BTreeSet<AnnotatedAxiom>
    {
        unsafe {
            (*self.axioms_as_ptr(axk))
                .get_mut(&axk).unwrap()
        }
    }

    /// Create a new ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// let o = Ontology::new();
    /// let o2 = Ontology::new();
    ///
    /// assert_eq!(o, o2);
    /// ```
    pub fn new() -> Ontology{
        Ontology::default()
    }

    /// Insert an axiom into the ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// let mut o = Ontology::new();
    /// let b = Build::new();
    /// o.insert(DeclareClass(b.class("http://www.example.com/a")));
    /// o.insert(DeclareObjectProperty(b.object_property("http://www.example.com/r")));
    /// ```
    ///
    /// See `declare` for an easier way to declare named entities.
    pub fn insert<A>(&mut self, ax:A) -> bool
        where A: Into<AnnotatedAxiom>
    {
        let ax:AnnotatedAxiom = ax.into();

        self.mut_set_for_kind(ax.kind()).insert(ax)
    }

    /// Declare an NamedEntity for the ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// let mut o = Ontology::new();
    /// let b = Build::new();
    /// o.declare(b.class("http://www.example.com/a"));
    /// o.declare(b.object_property("http://www.example.com/r"));
    /// ```
    pub fn declare<N>(&mut self, ne: N) -> bool
        where N: Into<NamedEntity>
    {
        self.insert(
            match ne.into() {
                NamedEntity::Class(c) =>
                    Axiom::DeclareClass(DeclareClass(c)),
                NamedEntity::ObjectProperty(obp) =>
                    Axiom::DeclareObjectProperty(DeclareObjectProperty(obp)),
                NamedEntity::AnnotationProperty(anp) =>
                    Axiom::DeclareAnnotationProperty
                    (DeclareAnnotationProperty(anp))
            }
        )
    }

    /// Fetch the AnnotatedAxiom for a given kind
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// let mut o = Ontology::new();
    /// let b = Build::new();
    /// o.declare(b.class("http://www.example.com/a"));
    /// o.declare(b.object_property("http://www.example.com/r"));
    ///
    /// assert_eq!(o.annotated_axiom(AxiomKind::DeclareClass).count(), 1);
    /// ```
    ///
    /// See also `axiom` for access to the `Axiom` without annotations.
    pub fn annotated_axiom(&self, axk: AxiomKind)
        -> impl Iterator<Item=&AnnotatedAxiom>
    {
        self.set_for_kind(axk).
            into_iter().flat_map(|hs| hs.iter())
    }

    /// Fetch the Axiom for a given kind
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// let mut o = Ontology::new();
    /// let b = Build::new();
    /// o.declare(b.class("http://www.example.com/a"));
    /// o.declare(b.object_property("http://www.example.com/r"));
    ///
    /// assert_eq!(o.axiom(AxiomKind::DeclareClass).count(), 1);
    /// ```
    ///
    /// See methods such as `declare_class` for access to the Axiom
    /// struct directly.
    pub fn axiom(&self, axk: AxiomKind)
             -> impl Iterator<Item=&Axiom>
    {
        self.annotated_axiom(axk)
            .map(|ann| &ann.axiom)
    }
}



impl Ontology {

    /// Returns all direct subclasses
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let mut o = Ontology::new();
    /// let b = Build::new();
    ///
    /// let sup = b.class("http://www.example.com/super");
    /// let sub = b.class("http://www.example.com/sub");
    /// let subsub = b.class("http://www.example.com/subsub");
    ///
    /// o.insert(SubClass::new((&sup).into(), (&sub).into()));
    /// o.insert(SubClass::new((&sub).into(), (&subsub).into()));
    ///
    /// let subs:Vec<&ClassExpression> = o.direct_subclass(&sup).collect();
    ///
    /// assert_eq!(vec![&ClassExpression::Class(sub)],subs);
    /// ```
    pub fn direct_subclass<C>(&self, c: C)
                              ->impl Iterator<Item=&ClassExpression>
        where C:Into<ClassExpression>
    {
        let c = c.into();
        self.sub_class()
            .filter(move |sc| &sc.super_class == &c )
            .map(|sc| &sc.sub_class )
    }

    /// Returns true is `subclass` is a subclass of `superclass`
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let mut o = Ontology::new();
    /// let b = Build::new();
    ///
    /// let sup = b.class("http://www.example.com/super");
    /// let sub = b.class("http://www.example.com/sub");
    /// let subsub = b.class("http://www.example.com/subsub");
    ///
    /// o.insert(SubClass::new((&sup).into(), (&sub).into()));
    /// o.insert(SubClass::new((&sub).into(), (&subsub).into()));
    ///
    /// assert!(o.is_subclass(&sup, &sub));
    /// assert!(!o.is_subclass(&sub, &sup));
    /// assert!(!o.is_subclass(&sup, &subsub));
    /// ```
    pub fn is_subclass<C>(&self, super_class:C,
                       sub_class:C) -> bool
        where C: Into<ClassExpression>
    {
        let super_class = super_class.into();
        let sub_class = sub_class.into();
        self.sub_class()
            .any(|sc|
                 sc.super_class == super_class &&
                 sc.sub_class == sub_class)
    }
}

#[cfg(test)]
mod test{
    use super::*;

    #[test]
    fn test_iri_from_string() {
        let build = Build::new();
        let iri = build.iri("http://www.example.com");

        assert_eq!(String::from(iri), "http://www.example.com");
    }

    #[test]
    fn test_iri_creation(){
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
    fn test_iri_string_creation(){
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
    fn test_ontology_cons(){
        let _ = Ontology::new();
        assert!(true);
    }

    #[test]
    fn test_class(){
        let mut o = Ontology::new();
        let c = Build::new().class("http://www.example.com");
        o.insert(DeclareClass(c));

        assert_eq!(o.declare_class().count(), 1);
    }

    #[test]
    fn test_class_declare() {
        let c = Build::new().class("http://www.example.com");

        let mut o = Ontology::new();
        o.declare(c);

        assert_eq!(o.declare_class().count(), 1);
    }

    #[test]
    fn test_class_convertors() {
        let c = Build::new().class("http://www.example.com");
        let i = Build::new().iri("http://www.example.com");

        let i1:IRI = c.clone().into();
        assert_eq!(i, i1);

        let c1:Class = Class::from(i);
        assert_eq!(c, c1);

        let ne:NamedEntity = c.clone().into();
        assert_eq!(ne, NamedEntity::Class(c));
    }
}
