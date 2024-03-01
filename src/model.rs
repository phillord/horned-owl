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
//! `AnnotatedComponent` instances, which consists of an `Component` and a set
//! of `Annotation`. The `Component` itself is a large enum representing
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
//! let b = Build::new_rc();
//! let top = TransitiveObjectProperty(ObjectPropertyExpression::ObjectProperty
//!                          (b.object_property("http://www.example.com/op")));
//! ```
//! - Rule 2:
//! ```
//! # use horned_owl::model::*;
//! // ObjectSomeValuesFrom{ope:PropertyExpression, ce:ClassExpression}
//! let b = Build::new_rc();
//! let some = ClassExpression::ObjectSomeValuesFrom{
//!                 ope: b.object_property("http://www.example.com/p").into(),
//!                 bce: b.class("http://www.example.com/c").into()
//! };
//! ```
//! - Rule 3:
//! ```
//! # use horned_owl::model::*;
//! // InverseObjectProperty(ObjectProperty, ObjectProperty)
//! let b = Build::new_rc();
//! let iop = InverseObjectProperties
//!             (b.object_property("http://www.example.com/op1"),
//!              b.object_property("http://www.example.com/op2"));
//! ```
//! - Rule 4:
//! ```
//! # use horned_owl::model::*;
//! // EquivalentClasses(Vec<ClassExpression>)
//! let b = Build::new_rc();
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
//! let b = Build::new_rc();
//! let opa = ObjectPropertyAssertion {
//!     ope: b.object_property("http://www.example.com/op").into(),
//!     from: b.named_individual("http://www.example.com/i1").into(),
//!     to: b.named_individual("http://www.example.com/i2").into(),
//! };
//! ```
use std::borrow::Borrow;
use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::BTreeSet;
use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::rc::Rc;
use std::sync::Arc;

/// An
/// [IRI](https://en.wikipedia.org/wiki/Internationalized_Resource_Identifier)
/// is an internationalized version of an URI/URL.
///
/// Here, we represent it as a simple `Borrow<str>`. IRIs are produced
/// using a `Build` instance, which allows for caching of the
/// underlying data.
///
/// No attempt is made to ensure that the IRI is valid with respect to
/// the specification. This can be achieved through
/// [`as_oxiri`](IRI::as_oxiri) method which both validates and also
/// provides access to the constituent parts.
#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct IRI<A>(pub (crate) A);

pub trait ForIRI:
    AsRef<str> + Borrow<str> + Clone + Debug + Eq + From<String> + Hash + PartialEq + Ord + PartialOrd
{
}

impl<T: ?Sized> ForIRI for T where
    T: AsRef<str>
        + Borrow<str>
        + Clone
        + Debug
        + Eq
        + From<String>
        + Hash
        + PartialEq
        + Ord
        + PartialOrd
{
}

pub type RcStr = Rc<str>;
pub type ArcStr = Arc<str>;

impl<A: ForIRI> IRI<A> {
    pub fn underlying(&self) -> A {
        self.0.clone()
    }
}

impl<A: ForIRI> Deref for IRI<A> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.borrow()
    }
}

impl<A: ForIRI> AsRef<str> for IRI<A> {
    fn as_ref(&self) -> &str {
        self.0.borrow()
    }
}

impl<A: ForIRI> Borrow<str> for IRI<A> {
    fn borrow(&self) -> &str {
        self.as_ref()
    }
}

impl From<&IRI<RcStr>> for RcStr {
    fn from(i: &IRI<RcStr>) -> RcStr {
        i.0.clone()
    }
}

impl From<IRI<RcStr>> for RcStr {
    fn from(i: IRI<RcStr>) -> RcStr {
        i.0
    }
}

impl<A: ForIRI> From<&IRI<A>> for String {
    fn from(i: &IRI<A>) -> String {
        i.0.borrow().to_string()
    }
}

impl<A: ForIRI> From<IRI<A>> for String {
    fn from(i: IRI<A>) -> String {
        i.0.borrow().to_string()
    }
}

impl<A: ForIRI> Display for IRI<A> {
    fn fmt(&self, f: &mut Formatter<'_>) -> ::std::fmt::Result {
        f.write_str(self.0.borrow())
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
pub struct Build<A: ForIRI>(
    RefCell<BTreeSet<IRI<A>>>,
    RefCell<BTreeSet<AnonymousIndividual<A>>>,
);

impl<A: ForIRI> Build<A> {
    pub fn new() -> Build<A> {
        Build(RefCell::new(BTreeSet::new()), RefCell::new(BTreeSet::new()))
    }

    /// Constructs a new `AnonymousIndividual`
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new_rc();
    /// let anon = b.anon("anon00001");
    /// assert_eq!("anon00001", String::from(anon));
    /// ```
    pub fn anon<S: Borrow<str>>(&self, s: S) -> AnonymousIndividual<A> {
        let mut cache = self.1.borrow_mut();
        if let Some(anon) = cache.get(s.borrow()) {
            anon.clone()
        } else {
            let anon = AnonymousIndividual(s.borrow().to_string().into());
            cache.insert(anon.clone());
            anon
        }
    }

    /// Constructs a new `IRI`
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new_rc();
    /// let iri = b.iri("http://www.example.com");
    /// assert_eq!("http://www.example.com", String::from(iri));
    /// ```
    pub fn iri<S: Borrow<str>>(&self, s: S) -> IRI<A> {
        let mut cache = self.0.borrow_mut();
        if let Some(iri) = cache.get(s.borrow()) {
            iri.clone()
        } else {
            let iri = IRI(s.borrow().to_string().into());
            cache.insert(iri.clone());
            iri
        }
    }

    /// Constructs a new `Class`.

    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new_rc();
    /// let c1 = b.class("http://www.example.com".to_string());
    /// let c2 = b.class("http://www.example.com");
    ///
    /// assert_eq!(c1, c2);
    /// ```
    ///
    pub fn class<S>(&self, s: S) -> Class<A>
    where
        S: Borrow<str>,
    {
        Class(self.iri(s))
    }

    /// Constructs a new `ObjectProperty`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new_rc();
    /// let obp1 = b.object_property("http://www.example.com".to_string());
    /// let obp2 = b.object_property("http://www.example.com");
    ///
    /// assert_eq!(obp1, obp2);
    /// ```
    pub fn object_property<S>(&self, s: S) -> ObjectProperty<A>
    where
        S: Borrow<str>,
    {
        ObjectProperty(self.iri(s))
    }

    /// Constructs a new `AnnotationProperty`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new_rc();
    /// let anp1 = b.annotation_property("http://www.example.com".to_string());
    /// let anp2 = b.annotation_property("http://www.example.com");
    ///
    /// assert_eq!(anp1, anp2);
    /// ```
    pub fn annotation_property<S>(&self, s: S) -> AnnotationProperty<A>
    where
        S: Borrow<str>,
    {
        AnnotationProperty(self.iri(s))
    }

    /// Constructs a new `DataProperty`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new_rc();
    /// let dp1 = b.data_property("http://www.example.com".to_string());
    /// let dp2 = b.data_property("http://www.example.com");
    ///
    /// assert_eq!(dp1, dp2);
    /// ```
    pub fn data_property<S>(&self, s: S) -> DataProperty<A>
    where
        S: Borrow<str>,
    {
        DataProperty(self.iri(s))
    }

    /// Constructs a new `NamedIndividual`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new_rc();
    /// let ni1 = b.named_individual("http://www.example.com".to_string());
    /// let ni2 = b.named_individual("http://www.example.com");
    ///
    /// assert_eq!(ni1, ni2);
    /// ```
    pub fn named_individual<S>(&self, s: S) -> NamedIndividual<A>
    where
        S: Borrow<str>,
    {
        NamedIndividual(self.iri(s))
    }

    /// Constructs a new `Datatype`.
    ///
    /// # Examples
    ///
    /// ```
    /// # use horned_owl::model::*;
    /// let b = Build::new_rc();
    /// let ni1 = b.datatype("http://www.example.com".to_string());
    /// let ni2 = b.datatype("http://www.example.com");
    ///
    /// assert_eq!(ni1, ni2);
    /// ```
    pub fn datatype<S>(&self, s: S) -> Datatype<A>
    where
        S: Borrow<str>,
    {
        Datatype(self.iri(s))
    }
}

impl Build<RcStr> {
    pub fn new_rc() -> Build<RcStr> {
        Build::new()
    }
}

impl Build<ArcStr> {
    pub fn new_arc() -> Build<ArcStr> {
        Build::new()
    }
}

impl Build<String> {
    pub fn new_string() -> Build<String> {
        Build::new()
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
        #[derive(Clone, Debug, Eq, PartialEq, Hash)]
        pub enum NamedEntity<A>{
            $($name($name<A>)),*
        }

        $(
            $(#[$attr]) *
            #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
            pub struct $name<A>(pub IRI<A>);

            impl<A: ForIRI> From<IRI<A>> for $name<A> {
                fn from(iri: IRI<A>) -> $name<A> {
                    $name(iri)
                }
            }

            impl<'a, A: ForIRI> From<&'a IRI<A>> for $name<A> {
                 fn from(iri: &IRI<A>) -> $name<A> {
                     $name(iri.clone())
                 }
            }

            impl<A: ForIRI> From<$name<A>> for String {
                fn from(n: $name<A>) -> String {
                    n.0.0.borrow().to_string()
                }
            }

            impl<'a, A: ForIRI> From<&'a $name<A>> for String {
                fn from(n: &$name<A>) -> String {
                    n.0.0.borrow().to_string()
                }
            }

            impl<A: ForIRI> From<$name<A>> for IRI<A> {
                fn from(n: $name<A>) -> IRI<A> {
                    n.0
                }
            }

            impl<'a, A: ForIRI> From<&'a $name<A>> for IRI<A> {
                fn from(n: &$name<A>) -> IRI<A> {
                    (n.0).clone()
                }
            }

            impl<A: ForIRI> From<$name<A>> for NamedEntity<A> {
                fn from(n:$name<A>) -> NamedEntity<A> {
                    NamedEntity::$name(n)
                }
            }

            impl<A: ForIRI> From<$name<A>> for NamedEntityKind {
                fn from(_n:$name<A>) -> NamedEntityKind {
                    NamedEntityKind::$name
                }
            }

            impl<A:ForIRI> $name<A> {
                pub fn is<I>(&self, iri: I) -> bool
                    where I:Into<IRI<A>>
                {
                    self.0 == iri.into()
                }

                pub fn is_s<S>(&self, iri:S) -> bool
                    where S:Into<String>
                {
                    self.0.0.borrow() == iri.into()
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
pub struct AnonymousIndividual<A>(pub A);

impl<A: ForIRI> Deref for AnonymousIndividual<A> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.0.borrow()
    }
}

impl<A: ForIRI> AnonymousIndividual<A> {
    pub fn underlying(&self) -> A {
        self.0.clone()
    }
}

impl<A: ForIRI> AsRef<str> for AnonymousIndividual<A> {
    fn as_ref(&self) -> &str {
        self.0.borrow()
    }
}

impl<A: ForIRI> Borrow<str> for AnonymousIndividual<A> {
    fn borrow(&self) -> &str {
        self.as_ref()
    }
}

impl<A: ForIRI> From<AnonymousIndividual<A>> for String {
    fn from(i: AnonymousIndividual<A>) -> String {
        i.0.borrow().to_string()
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum Individual<A> {
    Anonymous(AnonymousIndividual<A>),
    Named(NamedIndividual<A>),
}

impl<A: ForIRI> Deref for Individual<A> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Individual::Named(ni) => &*ni.0,
            Individual::Anonymous(ai) => &*ai,
        }
    }
}

impl<A: ForIRI> From<NamedIndividual<A>> for Individual<A> {
    fn from(ni: NamedIndividual<A>) -> Individual<A> {
        Self::Named(ni)
    }
}

impl<A: ForIRI> From<AnonymousIndividual<A>> for Individual<A> {
    fn from(ai: AnonymousIndividual<A>) -> Individual<A> {
        Self::Anonymous(ai)
    }
}

impl From<RcStr> for AnonymousIndividual<RcStr> {
    fn from(rc: RcStr) -> AnonymousIndividual<RcStr> {
        AnonymousIndividual(rc)
    }
}

impl From<String> for AnonymousIndividual<RcStr> {
    fn from(s: String) -> AnonymousIndividual<RcStr> {
        AnonymousIndividual(s.into())
    }
}
impl<A: ForIRI> From<&IRI<A>> for Individual<A> {
    fn from(iri: &IRI<A>) -> Individual<A> {
        let ni: NamedIndividual<_> = iri.into();
        ni.into()
    }
}

impl<A: ForIRI> From<IRI<A>> for Individual<A> {
    fn from(iri: IRI<A>) -> Individual<A> {
        Individual::from(&iri)
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum AnnotationSubject<A> {
    IRI(IRI<A>),
    AnonymousIndividual(AnonymousIndividual<A>),
}

impl<A: ForIRI> Deref for AnnotationSubject<A> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::IRI(iri) => &*iri,
            Self::AnonymousIndividual(ai) => &*ai,
        }
    }
}

impl<A: ForIRI> From<IRI<A>> for AnnotationSubject<A> {
    fn from(iri: IRI<A>) -> AnnotationSubject<A> {
        AnnotationSubject::IRI(iri)
    }
}

impl<A: ForIRI> From<AnonymousIndividual<A>> for AnnotationSubject<A> {
    fn from(anon: AnonymousIndividual<A>) -> AnnotationSubject<A> {
        AnnotationSubject::AnonymousIndividual(anon)
    }
}

impl<A: ForIRI> From<&IRI<A>> for AnnotationSubject<A> {
    fn from(iri: &IRI<A>) -> AnnotationSubject<A> {
        AnnotationSubject::IRI(iri.clone())
    }
}

impl<A: ForIRI> From<&AnonymousIndividual<A>> for AnnotationSubject<A> {
    fn from(anon: &AnonymousIndividual<A>) -> AnnotationSubject<A> {
        AnnotationSubject::AnonymousIndividual(anon.clone())
    }
}

impl<A: ForIRI> From<NamedEntity<A>> for Component<A> {
    fn from(ne: NamedEntity<A>) -> Component<A> {
        match ne {
            NamedEntity::Class(c) => Component::DeclareClass(DeclareClass(c)),
            NamedEntity::ObjectProperty(obp) => {
                Component::DeclareObjectProperty(DeclareObjectProperty(obp))
            }
            NamedEntity::AnnotationProperty(anp) => {
                Component::DeclareAnnotationProperty(DeclareAnnotationProperty(anp))
            }
            NamedEntity::DataProperty(dp) => Component::DeclareDataProperty(DeclareDataProperty(dp)),
            NamedEntity::NamedIndividual(ni) => {
                Component::DeclareNamedIndividual(DeclareNamedIndividual(ni))
            }
            NamedEntity::Datatype(dt) => Component::DeclareDatatype(DeclareDatatype(dt)),
        }
    }
}

impl<A: ForIRI> From<NamedEntity<A>> for AnnotatedComponent<A> {
    fn from(ne: NamedEntity<A>) -> AnnotatedComponent<A> {
        let ax: Component<_> = ne.into();
        ax.into()
    }
}

/// An interface providing access to any `Annotation` attached to an
/// entity.
trait Annotated<A> {
    /// Return the annotation
    ///
    /// The returned `BTreeSet` may be empty.
    fn annotation(&self) -> &BTreeSet<Annotation<A>>;
}

/// An interface providing access to the `ComponentKind`
///
/// An OWL ontology consists of a set of axioms of one of many
/// different kinds. These axioms all return an variant instance of
/// the `ComponentKind` enum. This is used in the API mostly to retrieve
/// instances of a certain kind.
pub trait Kinded {
    fn kind(&self) -> ComponentKind;

    fn is_axiom(&self) -> bool {
        match self.kind() {
            _ => true
        }
    }

    fn is_id(&self) -> bool {
        match self.kind() {
            _ => false
        }
    }
}

/// An `AnnotatedComponent` is an `Component` with one orpmore `Annotation`.
#[derive(Clone, Debug, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub struct AnnotatedComponent<A> {
    pub axiom: Component<A>,
    pub ann: BTreeSet<Annotation<A>>,
}

pub type RcAnnotatedComponent=Rc<AnnotatedComponent<RcStr>>;
pub type ArcAnnotatedComponent=Arc<AnnotatedComponent<ArcStr>>;


impl<A: ForIRI> AnnotatedComponent<A> {
    pub fn new<I>(axiom: I, ann: BTreeSet<Annotation<A>>) -> AnnotatedComponent<A>
    where
        I: Into<Component<A>>,
    {
        AnnotatedComponent {
            axiom: axiom.into(),
            ann,
        }
    }

    pub fn logical_cmp(&self, other: &AnnotatedComponent<A>) -> Ordering {
        self.axiom.cmp(&other.axiom)
    }

    pub fn logical_partial_cmp(&self, other: &AnnotatedComponent<A>) -> Option<Ordering> {
        Some(self.cmp(other))
    }

    pub fn logical_eq(&self, other: &AnnotatedComponent<A>) -> bool {
        self.axiom == other.axiom
    }

    pub fn logical_hash<H: Hasher>(&self, state: &mut H) {
        self.axiom.hash(state)
    }
}

impl<A: ForIRI> From<Component<A>> for AnnotatedComponent<A> {
    fn from(axiom: Component<A>) -> AnnotatedComponent<A> {
        AnnotatedComponent {
            axiom,
            ann: BTreeSet::new(),
        }
    }
}

impl<A: ForIRI> Kinded for AnnotatedComponent<A> {
    fn kind(&self) -> ComponentKind {
        self.axiom.kind()
    }
}

/// Add `Kinded` and `From` for each axiom.
macro_rules! componentimpl {
    ($A:ident, $name:ident) => {
        impl<$A: ForIRI> From<$name<$A>> for Component<$A> {
            fn from(ax: $name<$A>) -> Component<$A> {
                Component::$name(ax)
            }
        }

        impl<$A: ForIRI> From<$name<$A>> for AnnotatedComponent<$A> {
            fn from(ax: $name<$A>) -> AnnotatedComponent<$A> {
                AnnotatedComponent::from(Component::from(ax))
            }
        }

        impl<$A: ForIRI> Kinded for $name<$A> {
            fn kind(&self) -> ComponentKind {
                ComponentKind::$name
            }
        }
    };
}

/// Define a new axiom
///
/// Components can be either a tuple-like or normal struct. Documentation
/// is attached as a doc attribute after.
//
// I tried extensively to pass the attribute in the more normal
// location in front of the entity, but couldn't get it too match. I
// noticed that the quick_error crate passes afterwards and it's easy
// to get to work this way. As it's an internal macro, I think this is fine.
macro_rules! component {
    ($A:ident $name:ident ($($tt:ty),*) $(#[$attr:meta])*) =>
    {
        $(#[$attr]) *
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name<$A>($(pub $tt),*);
        componentimpl!($A, $name);
    };
    ($A:ident $name:ident {
        $($field_name:ident: $field_type:ty),*
     }
     $(#[$attr:meta])*
    ) => {
        $(#[$attr]) *
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name<$A>
        {
            $(pub $field_name: $field_type),*,
        }

        impl<$A:ForIRI> $name<$A> {
            pub fn new($($field_name: $field_type),*)
                -> $name<$A>
            {
                $name {
                    $($field_name),*
                }
            }

        }
        componentimpl!($A, $name);
    }
}

/// Generate all the axiom data structures
//
// This macro generates all of the axioms at once (delegated to the
// axiom macro). We have to do this in one go, although it makes
// the pattern matching a pain, because we need to know all the axiom
// names at once so we can generate the ComponentKind and Component
// enums.
macro_rules! components {
    ($A:ident,
     $($(#[$attr:meta])* $name:ident $tt:tt),*)
        =>
    {
        /// Contains all different kinds of axiom
        ///
        /// Variants of this C-style enum represent all of the
        /// different axioms that can exist in the ontology. Instances
        /// of this enum are returned by all `Component` and other
        /// entities as part of the `Kinded` trait.
        /// See also `Component` which is a Enum whose variants take
        /// instances of the `Component`
        #[derive(Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord)]
        pub enum ComponentKind {
            $($name),*
        }

        impl ComponentKind {
            pub fn all_kinds() -> Vec<ComponentKind> {
                vec![$(ComponentKind::$name),*]
            }
        }

        impl std::fmt::Debug for ComponentKind {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "ComponentKind::{}",
                       match self {
                           $(
                               ComponentKind::$name => stringify!($name)
                           ),*

                       })
            }
        }

        /// An axiom
        ///
        /// This enum has variants representing the various kinds of
        /// Component that can be found in an OWL Ontology. An OWL axiom
        /// maps to three different entities in Horned-OWL. First is a
        /// struct (for example, `SubClassOf`) which contains the data
        /// which defines the axiom (i.e. super and sub class for
        /// `SubClassOf`). Second, is a variant of the `ComponentKind`,
        /// which is used to identify all instances of a particular
        /// kind of axiom (i.e. any `SubClassOf` axiom will return an
        /// instance of ComponentKind::SubClassOf). Finally, we have a
        /// variant of this enum, which contains one of the structs
        /// (i.e. Component::SubClassOf(SubClassOf)), which is used as a union
        /// type for all structs. The struct and enum variants all
        /// share identical names.
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub enum Component<$A>{
            $($name($name<$A>)),*
        }

        // impl<$A:ForIRI> std::fmt::Debug for Component<$A> {
        //     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        //         write!(f, "Component::{}({})",
        //                match self {
        //                    $(
        //                        Component::$name(_) =>
        //                            stringify!($name)
        //                    ),*
        //                },
        //                match self {
        //                    $(
        //                        Component::$name(ax) =>
        //                            format!("{:?}", ax)
        //                    ),*
        //                },
        //         )
        //     }
        // }

        impl<$A:ForIRI> Kinded for Component<$A>
        {
            fn kind(&self) -> ComponentKind
            {
                match self
                {
                    $(
                        Component::$name(n) => n.kind()
                    ),*

                }
            }
        }

        $(
            component!(
                $A $name $tt $(#[$attr]) *
            );
        ) *
    }
}

components! {
    A,

    // Temporary to avoid nameclash with existing OntologyID
    OntologyIDComponent{iri: Option<IRI<A>>, viri: Option<IRI<A>>},

    /// An annotation associated with this Ontology
    OntologyAnnotation (Annotation<A>),

    /// Declares that an IRI is an import of this ontology
    Import(IRI<A>),

    // Declaration Components

    /// Declares that an IRI represents a Class in the Ontology
    ///
    /// In OWL, entities must be declared to be of a particular
    /// type. While, OWL (and Horned-OWL) allows the use of Class in
    /// an ontology where there is no declaration, the end ontology
    /// will change profile to OWL Full.  See also the [OWL
    /// Primer](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Entity_Declarations)
    DeclareClass(Class<A>),

    /// Declares that an IRI represents an ObjectProperty in the
    /// Ontology.
    ///
    /// See also [`DeclareClass`](struct.DeclareClass.html)
    DeclareObjectProperty(ObjectProperty<A>),

    /// Declares that an IRI represents an AnnotationProperty in the
    /// Ontology.
    ///
    /// See also [`DeclareClass`](struct.DeclareClass.html)
    DeclareAnnotationProperty (AnnotationProperty<A>),
    /// Declares that an IRI represents a DataProperty in the
    /// ontology.
    ///
    /// See also [`DeclareClass`](struct.DeclareClass.html)
    DeclareDataProperty (DataProperty<A>),

    /// Declare that an IRI represents a NamedIndividual in the
    /// ontology.
    ///
    /// See also [`DeclareClass`](struct.DeclareClass.html)
    DeclareNamedIndividual (NamedIndividual<A>),

    /// Declare that an IRI represents a Datatype in the ontology.
    ///
    DeclareDatatype(Datatype<A>),

    // Class Components

    /// A subclass relationship between two `ClassExpression`.
    ///
    /// All instances of `sub_class` are also instances of
    /// `super_class`.
    SubClassOf{
        sup: ClassExpression<A>,
        sub: ClassExpression<A>
    },

    /// An equivalence relationship between two `ClassExpression`.
    ///
    /// All instances of `ClassExpression` are also instances
    /// of other other.
    EquivalentClasses(Vec<ClassExpression<A>>),

    /// A disjoint relationship between two `ClassExpression`
    ///
    /// No instance of one `ClassExpression` can also be an instance
    /// of any of the others.
    DisjointClasses(Vec<ClassExpression<A>>),

    /// A disjoint union expression between one `ClassExpression` and
    /// a set of others.
    ///
    /// See also: https://www.w3.org/TR/owl2-syntax/#Disjoint_Union_of_Class_Expressions
    DisjointUnion(Class<A>, Vec<ClassExpression<A>>),

    // ObjectProperty axioms

    /// A sub property relationship between two object properties.
    ///
    /// The existence of the sub property relationship between two
    /// individuals also implies the super property relationship
    /// also. The super property can also be a property chain.
    /// So, if `s` is a super property of `r` then `a r b` implies `a
    /// s b`.
    ///
    /// See also: [Property Hierarchies](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Property_Hierarchies)
    /// See also: [Property Chains](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Property_Chains)
    SubObjectPropertyOf{
        sup: ObjectPropertyExpression<A>,
        sub: SubObjectPropertyExpression<A>
    },

    /// An equivalent object properties relationship.
    ///
    /// States that two object properties are semantically identical
    /// to each other.
    EquivalentObjectProperties(Vec<ObjectPropertyExpression<A>>),

    /// A disjoint object property relationship.
    ///
    /// This states that is an individual is connected by one of these
    /// object properties, it cannot be connected by any of the others.
    DisjointObjectProperties(Vec<ObjectPropertyExpression<A>>),

    /// An inverse relationship between two object properties.
    ///
    /// If two individuals are related by one relationship, they are
    /// related by the other in the opposite direction. So, if `r` and
    /// `s` are transitive, then `a r b` implies `b r a`.
    ///
    /// See also: [Property Characteristics](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/#Property_Characteristics)
    InverseObjectProperties(ObjectProperty<A>,ObjectProperty<A>),

    /// The domain of the object property.
    ///
    /// This states that if an individual `i` has an relationship,
    /// `ope` to any other individual, then the individual `i` is an
    /// instances of `ce`
    ///
    /// See also: [Domain](https://www.w3.org/TR/owl2-syntax/#Object_Property_Domain)
    ObjectPropertyDomain{ope:ObjectPropertyExpression<A>, ce:ClassExpression<A>},

    /// The range of the object property.
    ///
    /// This states that if an individual `i` is connected to be the
    /// relationship `ope`, then it is an individual of `ce`.1
    ///
    /// See also: [Domain](https://www.w3.org/TR/owl2-syntax/#Object_Property_Range)
    ObjectPropertyRange{ope:ObjectPropertyExpression<A>, ce:ClassExpression<A>},

    /// The functional characteristic.
    ///
    /// This states that if for a given individual `i`, there can be
    /// only one individual `j` connected to `i` by this object
    /// property expression.
    ///
    /// See also: [Functional](https://www.w3.org/TR/owl2-syntax/#Functional_Object_Properties)
    FunctionalObjectProperty(ObjectPropertyExpression<A>),

    /// The inverse functional characteristic
    ///
    /// This states that for each individual `i`, there can be at most
    /// one individual `j` connected to `i` via this object property
    /// expression.
    ///
    /// See also: [Inverse Functional](https://www.w3.org/TR/owl2-syntax/#Inverse-Functional_Object_Properties)
    InverseFunctionalObjectProperty(ObjectPropertyExpression<A>),

    /// The reflexive characteristic
    ///
    /// Every individual that is connected via the
    /// ObjectPropertyExpression is connected to itself.
    ///
    /// See also: [Reflexive](https://www.w3.org/TR/owl2-syntax/#Reflexive_Object_Properties)
    ReflexiveObjectProperty(ObjectPropertyExpression<A>),

    /// The irreflexive characteristic
    ///
    /// No individual can be connected to itself by this property.
    ///
    /// See also: [Irreflexive](https://www.w3.org/TR/owl2-syntax/#Irreflexive_Object_Properties)
    IrreflexiveObjectProperty(ObjectPropertyExpression<A>),

    /// The symmetric characteristic
    ///
    /// If an individual `i` is connected to `j` by this
    /// ObjectPropertyExpression, then `j` is also connected by `i`
    /// See also: [Symmetric](https://www.w3.org/TR/owl2-syntax/#Symmetric_Object_Properties)
    SymmetricObjectProperty(ObjectPropertyExpression<A>),

    /// The asymmetric characteristic.
    ///
    /// if an individual `i` is connected to `j` by this
    /// ObjectPropertyExpression, then `j` cannot be connected to `i`
    /// by the ObjectPropertyExpression.
    ///
    /// See also: [Asymmetric](https://www.w3.org/TR/owl2-syntax/#Asymmetric_Object_Properties)
    AsymmetricObjectProperty(ObjectPropertyExpression<A>),

    /// A transitive relationship between two object properties.
    ///
    /// When `r` is transitive, then `a r b`, and `b r c` implies `a r
    /// c` also.
    ///
    /// See also: [TransitiveObjectProperty](https://www.w3.org/TR/owl2-syntax/#Transitive_Object_Properties)
    TransitiveObjectProperty(ObjectPropertyExpression<A>),

    /// A sub data property relationship.
    ///
    /// The existence of the `sub_property` relationship also implies
    /// the existence of the `super_property`.
    ///
    /// See also: [Data Subproperties](https://www.w3.org/TR/owl2-syntax/#Data_Subproperties)
    SubDataPropertyOf {
        sup:DataProperty<A>,
        sub:DataProperty<A>
    },

    /// An equivalent data property relationship.
    ///
    /// All these DataProperties are semantically identical.
    ///
    /// See also: [EquivalentDataproperties](https://www.w3.org/TR/owl2-syntax/#Equivalent_Data_Properties)
    EquivalentDataProperties(Vec<DataProperty<A>>),

    /// A disjoint data property relationship.
    ///
    /// No individual can be connected to a data property expression
    /// by more than one of these DataProperty relations.
    ///
    /// See also: [DisjointDataProperties](https://www.w3.org/TR/owl2-syntax/#Disjoint_Data_Properties)
    DisjointDataProperties(Vec<DataProperty<A>>),

    /// The domain of a DataProperty.
    ///
    /// If an individual `i` has a relationship `dp` then `i` must be
    /// of type `ce`.
    ///
    /// See also: [Data Property Domain](https://www.w3.org/TR/owl2-syntax/#Disjoint_Data_Properties)
    DataPropertyDomain{dp:DataProperty<A>,ce:ClassExpression<A>},

    /// The range of a DataProperty.
    ///
    /// If in individual `i` has a relationship `dp` with some literal
    /// `l`, then `l` must by in `dr`.
    ///
    /// See also: [Data Property Range](https://www.w3.org/TR/owl2-syntax/#Data_Property_Range)
    DataPropertyRange{dp:DataProperty<A>,dr:DataRange<A>},

    /// The functional DataProperty characteristic.
    ///
    /// Any individual `i` can only be connected to a single literal
    /// by this DataProperty.
    ///
    /// See also: [Functional Data Property]:(https://www.w3.org/TR/owl2-syntax/#Functional_Data_Properties)
    FunctionalDataProperty(DataProperty<A>),

    /// Definition of a datatype.
    ///
    /// See also: [Datatype Definitions](https://www.w3.org/TR/owl2-syntax/#Datatype_Definitions)
    DatatypeDefinition {
        kind: Datatype<A>,
        range: DataRange<A>
    },

    /// A key
    ///
    /// An individual `i` which is of type `ce` can be uniquely
    /// identified by `pe`. Keys can only be applied to individuals
    /// which are explicitly named in the ontology, not those that are
    /// inferred.
    ///
    /// See also: [Keys](https://www.w3.org/TR/owl2-syntax/#Keys)
    HasKey{ce:ClassExpression<A>, vpe:Vec<PropertyExpression<A>>},

    // Assertions
    /// A same individual expression.
    ///
    /// See also: [Individual Equality](https://www.w3.org/TR/owl2-syntax/#Individual_Equality)
    SameIndividual (
        Vec<Individual<A>>
    ),

    /// A different individuals expression.
    ///
    /// See also: [Individual Inequality](https://www.w3.org/TR/owl2-syntax/#Individual_Inequality)
    DifferentIndividuals (
        Vec<Individual<A>>
    ),

    /// A class assertion expression.
    ///
    /// States that `i` is in class `ce`.
    ///
    /// See also: [Class Assertions](https://www.w3.org/TR/owl2-syntax/#Class_Assertions)
    ClassAssertion {
        ce: ClassExpression<A>,
        i: Individual<A>
    },

    /// An object property assertion.
    ///
    /// Individual `from` is connected `to` by `ope`.
    ///
    /// See also: [Positive Object Property Assertions](https://www.w3.org/TR/owl2-syntax/#Positive_Object_Property_Assertions)
    ObjectPropertyAssertion {
        ope: ObjectPropertyExpression<A>,
        from: Individual<A>,
        to: Individual<A>
    },

    /// A negative object property assertion.
    ///
    /// Individual `from` is not connected `to` by `ope`
    ///
    /// See also: [Negative Object Property Assertions](https://www.w3.org/TR/owl2-syntax/#Negative_Object_Property_Assertions)
    NegativeObjectPropertyAssertion {
        ope: ObjectPropertyExpression<A>,
        from: Individual<A>,
        to: Individual<A>
    },

    /// A data property assertion.
    ///
    /// Individual `from` is connected `to`` literal by `dp`.
    ///
    /// See also: [Data Property Assertion](https://www.w3.org/TR/owl2-syntax/#Positive_Data_Property_Assertions)
    DataPropertyAssertion {
        dp: DataProperty<A>,
        from: Individual<A>,
        to: Literal<A>
    },

    /// A negative data property assertion.
    ///
    /// Individual `from` is not connected `to` literal by `dp`.
    ///
    /// See also [Negative Data Property Assertions](https://www.w3.org/TR/owl2-syntax/#Negative_Data_Property_Assertions)
    NegativeDataPropertyAssertion {
        dp: DataProperty<A>,
        from: Individual<A>,
        to: Literal<A>
    },

    // Annotation Components
    /// An annotation assertion axiom
    ///
    /// States that `annotation` applies to the
    /// `annotation_subject`. Annotations refer to an `IRI` rather
    /// than the `NamedEntity` identified by that `IRI`.
    AnnotationAssertion {
        subject: AnnotationSubject<A>,
        ann: Annotation<A>
    },

    /// An sub-property assertion for annotation properties.
    ///
    /// Implies that any annotation of the type `sub_property` is also
    /// an annotation of the type `super_property`.
    SubAnnotationPropertyOf {
        sup: AnnotationProperty<A>,
        sub: AnnotationProperty<A>
    },

    /// Assert the domain of an `AnnotationProperty`
    AnnotationPropertyDomain {
        ap: AnnotationProperty<A>,
        iri: IRI<A>
    },

    /// Assert the range of an `AnnotationProperty`
    AnnotationPropertyRange {
        ap: AnnotationProperty<A>,
        iri: IRI<A>
    }
}

// Non-axiom data structures associated with OWL
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Literal<A> {
    // Simple Literals are syntactic sugar for a Datatype with type:
    // http://www.w3.org/2001/XMLSchema#string
    Simple {
        literal: String,
    },
    // Language-tagged literals have a lang tag and must be (or have
    // an implicit) of datatype
    // http://www.w3.org/1999/02/22-rdf-syntax-ns#langString
    Language {
        literal: String,
        lang: String,
    },
    Datatype {
        literal: String,
        datatype_iri: IRI<A>,
    },
}

impl<A: ForIRI> Literal<A> {
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
pub struct Annotation<A> {
    pub ap: AnnotationProperty<A>,
    pub av: AnnotationValue<A>,
}

/// The value of an annotation
///
/// This Enum is currently not complete.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AnnotationValue<A> {
    Literal(Literal<A>),
    IRI(IRI<A>),
}

impl<A: ForIRI> From<Literal<A>> for AnnotationValue<A> {
    fn from(literal: Literal<A>) -> AnnotationValue<A> {
        AnnotationValue::Literal(literal)
    }
}

impl<A: ForIRI> From<IRI<A>> for AnnotationValue<A> {
    fn from(iri: IRI<A>) -> AnnotationValue<A> {
        AnnotationValue::IRI(iri)
    }
}

/// A object property expression
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ObjectPropertyExpression<A> {
    ObjectProperty(ObjectProperty<A>),
    InverseObjectProperty(ObjectProperty<A>),
}

impl<A: ForIRI> From<ObjectProperty<A>> for ObjectPropertyExpression<A> {
    fn from(op: ObjectProperty<A>) -> ObjectPropertyExpression<A> {
        ObjectPropertyExpression::ObjectProperty(op)
    }
}

impl<A: ForIRI> From<IRI<A>> for ObjectPropertyExpression<A> {
    fn from(iri: IRI<A>) -> ObjectPropertyExpression<A> {
        let op: ObjectProperty<_> = iri.into();
        op.into()
    }
}

impl<A: ForIRI> From<&IRI<A>> for ObjectPropertyExpression<A> {
    fn from(iri: &IRI<A>) -> ObjectPropertyExpression<A> {
        iri.clone().into()
    }
}

impl<A: ForIRI> ObjectPropertyExpression<A> {
    pub fn as_property(&self) -> Option<&ObjectProperty<A>> {
        match self {
            ObjectPropertyExpression::ObjectProperty(op) => Some(op),
            ObjectPropertyExpression::InverseObjectProperty(_) => None,
        }
    }
}

/// A sub-object property expression
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum SubObjectPropertyExpression<A> {
    // We use Vec here rather than BTreeSet because, perhaps
    // surprisingly, BTreeSet is not itself hashable.
    ObjectPropertyChain(Vec<ObjectPropertyExpression<A>>),
    ObjectPropertyExpression(ObjectPropertyExpression<A>),
}

impl<A: ForIRI> From<ObjectPropertyExpression<A>> for SubObjectPropertyExpression<A> {
    fn from(ope: ObjectPropertyExpression<A>) -> SubObjectPropertyExpression<A> {
        SubObjectPropertyExpression::ObjectPropertyExpression(ope)
    }
}

/// A property expression
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum PropertyExpression<A> {
    ObjectPropertyExpression(ObjectPropertyExpression<A>),
    DataProperty(DataProperty<A>),
    AnnotationProperty(AnnotationProperty<A>),
}

impl<A: ForIRI> From<ObjectPropertyExpression<A>> for PropertyExpression<A> {
    fn from(ope: ObjectPropertyExpression<A>) -> PropertyExpression<A> {
        PropertyExpression::ObjectPropertyExpression(ope)
    }
}
impl<A: ForIRI> From<DataProperty<A>> for PropertyExpression<A> {
    fn from(dp: DataProperty<A>) -> PropertyExpression<A> {
        PropertyExpression::DataProperty(dp)
    }
}

// Data!!!
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FacetRestriction<A> {
    pub f: Facet,
    pub l: Literal<A>,
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
pub enum DataRange<A> {
    Datatype(Datatype<A>),
    DataIntersectionOf(Vec<DataRange<A>>),
    DataUnionOf(Vec<DataRange<A>>),
    DataComplementOf(Box<DataRange<A>>),
    DataOneOf(Vec<Literal<A>>),
    DatatypeRestriction(Datatype<A>, Vec<FacetRestriction<A>>),
}

impl<A: ForIRI> From<Datatype<A>> for DataRange<A> {
    fn from(dr: Datatype<A>) -> DataRange<A> {
        DataRange::Datatype(dr)
    }
}

/// A class expression
///
/// As well as a named class, it is possible to define classes of
/// individuals based on these class constructors.
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ClassExpression<A> {
    /// A named class
    Class(Class<A>),

    /// The boolean and
    ///
    /// The class of individuals which are individuals of all these
    /// classes.
    ObjectIntersectionOf(Vec<ClassExpression<A>>),

    /// The boolean or
    ///
    /// The class of individuals which are individuals of any of these
    /// classes.
    ObjectUnionOf(Vec<ClassExpression<A>>),

    /// The boolean not
    ///
    /// The class of individuals which are not individuals of any of
    /// these classes.
    ObjectComplementOf(Box<ClassExpression<A>>),

    /// An enumeration of individuals
    ///
    /// This is the class containing exactly the given set of
    /// individuals.
    ObjectOneOf(Vec<Individual<A>>),

    /// An existential relationship
    ///
    /// This is the anonymous class of individuals `i`, which have the
    /// relationship `o` to a class expression `ce`. Every individual
    /// in `i` must have this relationship to one individual in `ce`.
    ObjectSomeValuesFrom {
        ope: ObjectPropertyExpression<A>,
        bce: Box<ClassExpression<A>>,
    },

    /// A universal relationship
    ///
    /// This is the anonymous class of individuals `i` where all
    /// individuals which are related by `o` are instances of
    /// `ce`. This does not imply that the `i` necessarily has any
    /// relation `r`.
    ObjectAllValuesFrom {
        ope: ObjectPropertyExpression<A>,
        bce: Box<ClassExpression<A>>,
    },

    /// An existential relationship to an individual
    ///
    /// This is the class of individuals `c` which have the
    /// relationship `o` to another individual `i`. Every individual
    /// in `c` must have this relationship to the individual `i`
    ObjectHasValue {
        ope: ObjectPropertyExpression<A>,
        i: Individual<A>,
    },

    /// The class of individuals which have a relation to themselves
    ///
    /// Given a object property `r`, this class defines all the
    /// individuals where `i r i`.
    ObjectHasSelf(ObjectPropertyExpression<A>),

    /// A min cardinality relationship between individuals
    ///
    /// Given an object property `o` and a class `ce`, this describes
    /// the class of individuals which have the `o` relationship to at
    /// least `n` other individuals.
    ObjectMinCardinality {
        n: u32,
        ope: ObjectPropertyExpression<A>,
        bce: Box<ClassExpression<A>>,
    },

    /// A max cardinality relationship between individuals
    ///
    /// Given an object property `o` and a class `ce`, this describes
    /// the class of individuals which have the `o` relationship to at
    /// most `n` other individuals.
    ObjectMaxCardinality {
        n: u32,
        ope: ObjectPropertyExpression<A>,
        bce: Box<ClassExpression<A>>,
    },

    /// An exact cardinality relationship between individuals
    ///
    /// Given an object property `o` and a class `ce`, this describes
    /// the class of individuals which have the `o` relationship to exactly
    /// `n` other individuals.
    ObjectExactCardinality {
        n: u32,
        ope: ObjectPropertyExpression<A>,
        bce: Box<ClassExpression<A>>,
    },

    /// An existential relationship.
    ///
    /// This is the anonymous class of individuals `i` which have the
    /// relationship `dp` to the data range, `dr`. Every individual
    /// `i` must have this relationship to data constrained by `dr`.
    ///
    /// See also: [Existential Quantification](https://www.w3.org/TR/owl2-syntax/#Existential_Quantification_2)
    DataSomeValuesFrom {
        dp: DataProperty<A>,
        dr: DataRange<A>,
    },

    /// A universal relationship.
    ///
    /// This is the anonymous class of individuals `i` which if they
    /// have a relationship `dp` to some data, then that must be of
    /// type `dr`.
    ///
    /// See also [Universal Quantification](https://www.w3.org/TR/owl2-syntax/#Universal_Quantification_2)
    DataAllValuesFrom {
        dp: DataProperty<A>,
        dr: DataRange<A>,
    },

    /// A has-value relationship.

    /// This is the class of individuals, `i`, which have the
    /// relationship `dp` to exactly the literal `l`.

    /// See also [Value Restriction](https://www.w3.org/TR/owl2-syntax/#Literal_Value_Restriction)
    DataHasValue { dp: DataProperty<A>, l: Literal<A> },

    /// A minimum cardinality restriction

    /// The class of individuals have at least `n` relationships of
    /// the kind `dp` to a given data range `dr`.

    /// See also [Min Cardinality](https://www.w3.org/TR/owl2-syntax/#Minimum_Cardinality_2)
    DataMinCardinality {
        n: u32,
        dp: DataProperty<A>,
        dr: DataRange<A>,
    },

    /// A max cardinality restriction

    /// The class of individuals have at most `n` relationships of
    /// the kind `dp` to a given data range `dr`.

    /// See also [Max Cardinality](https://www.w3.org/TR/owl2-syntax/#Maximum_Cardinality_2)
    DataMaxCardinality {
        n: u32,
        dp: DataProperty<A>,
        dr: DataRange<A>,
    },

    /// An exact cardinality restriction

    /// The class of individuals have exactly `n` relationships of
    /// the kind `dp` to a given data range `dr`.

    /// See also [Exactly Cardinality](https://www.w3.org/TR/owl2-syntax/#Exact_Cardinality_2)
    DataExactCardinality {
        n: u32,
        dp: DataProperty<A>,
        dr: DataRange<A>,
    },
}

impl<A: ForIRI> From<Class<A>> for ClassExpression<A> {
    fn from(c: Class<A>) -> ClassExpression<A> {
        ClassExpression::Class(c)
    }
}

impl<'a, A: ForIRI> From<&'a Class<A>> for ClassExpression<A> {
    fn from(c: &'a Class<A>) -> ClassExpression<A> {
        ClassExpression::Class(c.clone())
    }
}

impl<A: ForIRI> From<Class<A>> for Box<ClassExpression<A>> {
    fn from(c: Class<A>) -> Box<ClassExpression<A>> {
        Box::new(c.into())
    }
}

/// An ontology identifier
///
/// An ontology is identified by an IRI which is expected to remain
/// stable over the lifetime of the ontology, and a version IRI which
/// is expected to change between versions.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct OntologyID<A> {
    pub iri: Option<IRI<A>>,
    pub viri: Option<IRI<A>>,
}

impl<A> Default for OntologyID<A> {
    fn default() -> OntologyID<A> {
        OntologyID {
            iri: None,
            viri: None,
        }
    }
}

/// Access or change the `OntologyID` of an `Ontology`
pub trait Ontology<A> {
    fn id(&self) -> &OntologyID<A>;
    fn mut_id(&mut self) -> &mut OntologyID<A>;
    fn doc_iri(&self) -> &Option<IRI<A>>;
    fn mut_doc_iri(&mut self) -> &mut Option<IRI<A>>;
}

/// Add or remove axioms to an `MutableOntology`
pub trait MutableOntology<A> {
    /// Insert an axiom into the ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// # use horned_owl::ontology::set::SetOntology;
    /// let mut o = SetOntology::new_rc();
    /// let b = Build::new();
    /// o.insert(DeclareClass(b.class("http://www.example.com/a")));
    /// o.insert(DeclareObjectProperty(b.object_property("http://www.example.com/r")));
    /// ```
    ///
    /// See `declare` for an easier way to declare named entities.
    fn insert<AA>(&mut self, ax: AA) -> bool
    where
        AA: Into<AnnotatedComponent<A>>;

    fn remove(&mut self, ax: &AnnotatedComponent<A>) -> bool {
        self.take(ax).is_some()
    }

    fn take(&mut self, ax: &AnnotatedComponent<A>) -> Option<AnnotatedComponent<A>>;

    /// Declare an NamedEntity for the ontology.
    ///
    /// # Examples
    /// ```
    /// # use horned_owl::model::*;
    /// # use horned_owl::ontology::set::SetOntology;
    /// let mut o = SetOntology::new_rc();
    /// let b = Build::new();
    /// o.declare(b.class("http://www.example.com/a"));
    /// o.declare(b.object_property("http://www.example.com/r"));
    /// ```
    fn declare<N>(&mut self, ne: N) -> bool
    where
        A: ForIRI,
        N: Into<NamedEntity<A>>,
    {
        let ne: NamedEntity<_> = ne.into();
        let ax: Component<_> = ne.into();
        self.insert(ax)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ontology::component_mapped::ComponentMappedOntology;

    #[test]
    fn test_iri_from_string() {
        let build = Build::new_rc();
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
        let build = Build::new_rc();

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
        let mut o = ComponentMappedOntology::new_rc();
        let c = Build::new().class("http://www.example.com");
        o.insert(DeclareClass(c));

        assert_eq!(o.i().declare_class().count(), 1);
    }

    #[test]
    fn test_class_declare() {
        let c = Build::new_rc().class("http://www.example.com");

        let mut o = ComponentMappedOntology::new_rc();
        o.declare(c);

        assert_eq!(o.i().declare_class().count(), 1);
    }

    #[test]
    fn test_class_convertors() {
        let c = Build::new_rc().class("http://www.example.com");
        let i = Build::new_rc().iri("http://www.example.com");

        let i1: IRI<_> = c.clone().into();
        assert_eq!(i, i1);

        let c1: Class<_> = Class::from(i);
        assert_eq!(c, c1);

        let ne: NamedEntity<_> = c.clone().into();
        assert_eq!(ne, NamedEntity::Class(c));
    }

    #[test]
    fn test_class_string_ref() {
        let s = String::from("http://www.example.com");
        let c = Build::new_rc().class(s.clone());

        assert!(c.is_s(s));
    }

    #[test]
    fn test_is() {
        let c = Build::new_rc().class("http://www.example.com");
        let i = Build::new().named_individual("http://www.example.com");
        let iri = Build::new().iri("http://www.example.com");

        assert!(c.is(iri));
        assert!(c.is(i.clone()));
        assert!(i.is(c));
    }

    #[test]
    fn test_axiom_convertors() {
        let c = Build::new_rc().class("http://www.example.com");

        let dc = DisjointClasses(vec![c.clone().into(), c.clone().into()]);
        let _aa: Component<_> = dc.into();
    }

    #[test]
    fn test_object_property_expression() {
        let b = Build::new_rc();
        let obp = b.object_property("http://www.example.com");

        let obpe = ObjectPropertyExpression::ObjectProperty(obp.clone());

        assert_eq!(obpe.as_property().unwrap(), &obp);
    }

    #[test]
    fn test_axiom_equality() {
        let b = Build::new_rc();

        let ann = Annotation {
            ap: b.annotation_property("http://www.example.com/ap"),
            av: b.iri("http://www.example.com/av").into(),
        };

        let mut decl1: AnnotatedComponent<_> = DeclareClass(b.class("http://www.example.com#a")).into();
        let decl2 = decl1.clone();

        assert_eq!(decl1, decl2);

        decl1.ann.insert(ann);
        assert!(!(decl1 == decl2));
    }

    #[test]
    fn test_oxiri() {
        let b = Build::new_rc();

        let iri = b.iri("http://www.example.com");

        let oxiri = iri.as_oxiri().unwrap();
        assert_eq!(oxiri.authority(), Some("www.example.com"));
    }
}
