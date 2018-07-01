#![allow(dead_code)]

use std::collections::HashSet;
use std::rc::Rc;
use std::cell::RefCell;
use std::ops::Deref;

pub trait Checkable{
    fn check(&self, ont: &Ontology)-> ();
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct IRI(Rc<String>);

impl Deref for IRI{
    type Target = String;

    fn deref(&self) -> &String{
        &self.0
    }
}

#[derive(Debug)]
pub struct IRIBuild(Rc<RefCell<HashSet<IRI>>>);

impl IRIBuild{
    pub fn new() -> IRIBuild{
        IRIBuild(Rc::new(RefCell::new(HashSet::new())))
    }

    pub fn iri(&self, s: String) -> IRI{
        let iri = IRI(Rc::new(s));

        let mut cache = self.0.borrow_mut();
        if cache.contains(&iri){
            return cache.get(&iri).unwrap().clone()
        }

        cache.insert(iri.clone());
        return iri;
    }
}

#[test]
fn test_iri_creation(){
    let iri_build = IRIBuild::new();

    let iri1 = iri_build.iri("http://example.com".to_string());

    let iri2 = iri_build.iri("http://example.com".to_string());

    // these are equal to each other
    assert_eq!(iri1, iri2);

    // these are the same object in memory
    assert!(Rc::ptr_eq(&iri1.0, &iri2.0));

    // iri1, iri2 and one in the cache == 3
    assert_eq!(Rc::strong_count(&iri1.0), 3);
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct Class(pub IRI);

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct ObjectProperty(IRI);

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct SubClass{
    pub superclass: ClassExpression,
    pub subclass: ClassExpression,
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct Some{
    pub object_property: ObjectProperty,
    pub filler: Box<ClassExpression>
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct And{
    pub operands: Vec<ClassExpression>
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct Or{
    pub operands: Vec<ClassExpression>
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct Not{
    pub operand: ClassExpression
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub enum ClassExpression
{
    Class(Class),
    Some(Some),
    And(And),
    Or(And),
}

#[derive(Debug)]
pub struct OntologyID{
    pub iri: Option<IRI>,
    pub viri: Option<IRI>,
}

#[derive(Debug)]
pub struct Ontology
{
    pub iri_build:IRIBuild,
    pub id: OntologyID,
    pub class: HashSet<Class>,
    pub subclass: HashSet<SubClass>,
    pub object_property: HashSet<ObjectProperty>,
    pub some: HashSet<ClassExpression>,
    pub and: HashSet<And>
}

impl Ontology {
    pub fn new() -> Ontology{
        Ontology::new_with_build(IRIBuild::new())
    }

    pub fn new_with_build(iri_build:IRIBuild) -> Ontology{
        Ontology{
            iri_build: iri_build,
            id: OntologyID{iri:None,viri:None},
            class: HashSet::new(),
            subclass: HashSet::new(),
            object_property: HashSet::new(),
            some: HashSet::new(),
            and: HashSet::new(),
        }
    }

    pub fn iri(&self, s: String)-> IRI{
        self.iri_build.iri(s)
    }

    pub fn class(&mut self, i: IRI) -> Class {
        let c = Class(i);

        if let Some(_) = self.class.get(&c)
        {return c;}

        self.class.insert(c.clone());
        c
    }

    pub fn object_property(&mut self, i: IRI) -> ObjectProperty{
        let o = ObjectProperty(i);

        if let Some(_) = self.object_property.get(&o)
        {return o;};

        self.object_property.insert(o.clone());
        o
    }

    pub fn subclass(&mut self, superclass:Class, subclass: Class)
                    -> SubClass
    {
        self.subclass_exp(ClassExpression::Class(superclass),
                          ClassExpression::Class(subclass))
    }

    pub fn subclass_exp(&mut self, superclass:ClassExpression,
                        subclass: ClassExpression) -> SubClass
    {
        let sc = SubClass{superclass:superclass,subclass:subclass};

        if let Some(_) = self.subclass.get(&sc)
        {return sc;}

        self.subclass.insert(sc.clone());
        sc
    }

    pub fn some(&mut self, object_property:ObjectProperty,
                class:Class)
                -> ClassExpression{
        self.some_exp(object_property,ClassExpression::Class(class))
    }

    pub fn some_exp(&mut self, object_property:ObjectProperty,
                    filler:ClassExpression) -> ClassExpression{
        let some =
            ClassExpression::Some(
                Some{object_property:object_property,
                     filler:Box::new(filler)});

        if let Some(_) = self.some.get(&some)
        {return some;}

        self.some.insert(some.clone());
        some
    }

    // Query Methods
    pub fn direct_subclass(&self, c: Class)
                           ->Vec<ClassExpression>{
        self.direct_subclass_exp(ClassExpression::Class(c))
    }

    pub fn direct_subclass_exp(&self, c: ClassExpression)
                           -> Vec<ClassExpression>{
        self.subclass
            .iter()
            .filter(|sc| sc.superclass == c )
            .map(|sc| sc.subclass.clone())
            .collect::<Vec<ClassExpression>>()
    }

    pub fn is_subclass(&self, superclass:&Class, subclass:&Class)
        -> bool{
        self.is_subclass_exp(&ClassExpression::Class(superclass.clone()),
                             &ClassExpression::Class(subclass.clone()))
    }

    pub fn is_subclass_exp(&self, superclass:&ClassExpression,
                           subclass:&ClassExpression)
                       ->bool{

        let first:Option<&SubClass> =
            self.subclass.iter()
            .filter(|sc|
                    sc.superclass == *superclass &&
                    sc.subclass == *subclass)
            .next();

        match first
        {
            Some(_) => true,
            None => false
        }
    }
}
