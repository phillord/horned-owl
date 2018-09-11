#![allow(dead_code)]

use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::cmp::Ordering;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::rc::Rc;

// IRIs
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

// The builder class returns named entities and caches
// IRIs. Currently, this uses Rc internally which means that we will
// be limited to single threading, but this can be re-written as a
// trait and made generic if we need.
#[derive(Debug, Default)]
pub struct Build(Rc<RefCell<HashSet<IRI>>>);

impl Build{
    pub fn new() -> Build{
        Build(Rc::new(RefCell::new(HashSet::new())))
    }

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
    ($($name:ident),*)  => {

        #[derive(Debug, Eq, PartialEq, Hash)]
        pub enum NamedEntity{
            $($name($name)),*
        }

        $(
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
    Class,
    ObjectProperty,
    AnnotationProperty
}

// Axiom
trait Annotated {
    fn annotation(&self) -> &HashSet<Annotation>;
}

trait Kinded {
    fn kind(&self) -> AxiomKind;
}

#[derive(Debug)]
pub struct AnnotatedAxiom{
    pub axiom: Axiom,
    pub annotation: HashSet<Annotation>
}

impl AnnotatedAxiom {
    pub fn new(axiom: Axiom, annotation: HashSet<Annotation>)
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
            annotation: HashSet::new()
        }
    }
}

impl Kinded for AnnotatedAxiom {
    fn kind(&self) -> AxiomKind {
        self.axiom.kind()
    }
}

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

#[allow(unused_macros)]
macro_rules! onimpl {
    ($kind:ident, $method:ident)
        =>
    {
        impl Ontology {
            pub fn $method(&self)
                       -> impl Iterator<Item=&$kind> {
                on!(self, $kind)
            }
        }
    }
}

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


macro_rules! axiom {
    ($name:ident ($($tt:tt),*)) => {
        #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
        pub struct $name($(pub $tt),*);
        axiomimpl!($name);
    };
    ($name:ident {
        $($field_name:ident: $field_type:ty),*
    }) => {
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


macro_rules! axioms {
    ($($name:ident $tt:tt),*)
        => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
        pub enum AxiomKind {
            $($name),*
        }

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
                axiom!($name $tt);
            ) *
        }
}

axioms!{
    // Declaration Axioms
    DeclareClass(Class),
    DeclareObjectProperty(ObjectProperty),
    DeclareAnnotationProperty (AnnotationProperty),

    // Class Axioms
    SubClass{
        super_class: ClassExpression,
        sub_class: ClassExpression
    },

    EquivalentClass(ClassExpression,ClassExpression),
    DisjointClass(ClassExpression,ClassExpression),

    // ObjectProperty axioms
    SubObjectProperty{
        super_property:ObjectPropertyExpression,
        sub_property:ObjectProperty
    },
    InverseObjectProperty(ObjectProperty,ObjectProperty),
    TransitiveObjectProperty(ObjectProperty),

    // Annotation Axioms
    AssertAnnotation {
        annotation_subject:IRI,
        annotation: Annotation
    },

    SubAnnotationProperty {
        super_property:AnnotationProperty,
        sub_property: AnnotationProperty
    },

    OntologyAnnotation (Annotation)
}

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


impl Ontology {

    fn axioms_as_ptr(&self, axk: AxiomKind)
        -> *mut HashMap<AxiomKind,HashSet<AnnotatedAxiom>>
    {
        self.axiom.borrow_mut().entry(axk)
            .or_insert(HashSet::new());
        self.axiom.as_ptr()
    }

    fn set_for_kind(&self, axk: AxiomKind)
                     -> Option<&HashSet<AnnotatedAxiom>>
    {
        unsafe{
            (*self.axiom.as_ptr())
                .get(&axk)
        }
    }

    fn mut_set_for_kind(&mut self, axk: AxiomKind)
                        -> &mut HashSet<AnnotatedAxiom>
    {
        unsafe {
            (*self.axioms_as_ptr(axk))
                .get_mut(&axk).unwrap()
        }
    }

    pub fn insert<A>(&mut self, ax:A) -> bool
        where A: Into<AnnotatedAxiom>
    {
        let ax:AnnotatedAxiom = ax.into();

        self.mut_set_for_kind(ax.kind()).insert(ax)
    }

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

    pub fn annotated_axiom(&self, axk: AxiomKind)
        -> impl Iterator<Item=&AnnotatedAxiom>
    {
        self.set_for_kind(axk).
            into_iter().flat_map(|hs| hs.iter())
    }

    pub fn axiom(&self, axk: AxiomKind)
             -> impl Iterator<Item=&Axiom>
    {
        self.annotated_axiom(axk)
            .map(|ann| &ann.axiom)
    }
}

// Old Axioms
#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Annotation {
    pub annotation_property: AnnotationProperty,
    pub annotation_value: AnnotationValue
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum AnnotationValue {
    PlainLiteral{
        datatype_iri: Option<IRI>,
        lang: Option<String>,
        literal: Option<String>,
    },
    IRI(IRI)
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ObjectPropertyExpression {
    ObjectPropertyChain(Vec<ObjectProperty>),
    ObjectProperty(ObjectProperty)
}


#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ClassExpression
{
    Class(Class),
    Some{o:ObjectProperty, ce:Box<ClassExpression>},
    Only{o:ObjectProperty, ce:Box<ClassExpression>},
    And{o:Vec<ClassExpression>},
    Or{o:Vec<ClassExpression>},
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


// Ontology
#[derive(Debug, Default, Eq, PartialEq)]
pub struct OntologyID{
    pub iri: Option<IRI>,
    pub viri: Option<IRI>,
}

#[derive(Debug, Default, Eq, PartialEq)]
pub struct Ontology
{
    pub id: OntologyID,
    axiom: RefCell<HashMap<AxiomKind,HashSet<AnnotatedAxiom>>>,
}

impl Ontology {
    pub fn new() -> Ontology{
        Ontology::default()
    }

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
