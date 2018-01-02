#![allow(dead_code)]

use std::collections::HashMap;
use std::collections::HashSet;

pub mod io;

static mut COUNTER: usize = 0;

pub trait Checkable{
    fn check(&self, ont: &Ontology)-> ();
}

#[derive(Eq,PartialEq,Hash,Copy,Clone,Debug)]
pub struct IRI(usize);

impl Checkable for IRI{
    fn check(&self, ont: &Ontology){
        if !ont.contains_id(self.0){
            panic!("Attempt to add IRI to wrong ontology")
        }
    }
}

#[derive(Eq,PartialEq,Hash,Copy,Clone,Debug)]
pub struct Class(IRI);

impl Checkable for Class{
    fn check(&self, ont: &Ontology){
        if !ont.contains_id((self.0).0){
            panic!("Attempt to add class to wrong ontology");
        }
    }
}

#[derive(Eq,PartialEq,Hash,Copy,Clone,Debug)]
pub struct ObjectProperty(IRI);

impl Checkable for ObjectProperty{
    fn check(&self, ont: &Ontology){
        if !ont.contains_id((self.0).0){
            panic!("Attempt to add object property to wrong ontology");
        }
    }
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct SubClass{
    superclass: ClassExpression,
    subclass: ClassExpression,
}

impl Checkable for SubClass{
    fn check(&self, ont: &Ontology){
        self.superclass.check(ont);
        self.subclass.check(ont);
    }
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct Some{
    object_property: ObjectProperty,
    filler: Box<ClassExpression>
}

impl Checkable for Some{
    fn check(&self, ont:&Ontology) -> ()
    {
        self.object_property.check(ont);
        self.filler.check(ont);
    }
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct And{
    operands: Vec<ClassExpression>
}

impl Checkable for And
{
    fn check(&self, ont: &Ontology) -> (){
        for i in &self.operands{
            i.check(ont);
        }
    }
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct Or{
    operands: Vec<ClassExpression>
}

impl Checkable for Or
{
    fn check(&self, ont: &Ontology) -> (){
        for i in &self.operands{
            i.check(ont);
        }
    }
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct Not{
    operand: ClassExpression
}

impl Checkable for Not
{
    fn check(&self, ont:&Ontology) -> (){
        self.operand.check(ont)
    }
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub enum ClassExpression
{
    Class(Class),
    Some(Some),
    And(And),
    Or(And),
}

impl Checkable for ClassExpression{
    fn check(&self, ont:&Ontology) -> ()
    {
        match self{
            &ClassExpression::Class(ref i) => i.check(ont),
            &ClassExpression::Some(ref i)  => i.check(ont),
            &ClassExpression::And(ref i) => i.check(ont),
            &ClassExpression::Or(ref i) => i.check(ont)
        }
    }
}

#[derive(Debug)]
pub struct OntologyID{
    iri: Option<IRI>,
    viri: Option<IRI>,
}

#[derive(Debug)]
pub struct Ontology
{
    str_iri: HashMap<String,IRI>,
    id_str: HashMap<usize,String>,
    id: OntologyID,
    class: HashSet<Class>,
    subclass: HashSet<SubClass>,
    object_property: HashSet<ObjectProperty>,
    some: HashSet<ClassExpression>,
    and: HashSet<And>
}

impl Ontology {
    pub fn new() -> Ontology{
        Ontology{
            str_iri: HashMap::new(),
            id_str: HashMap::new(),
            id: OntologyID{iri:None,viri:None},
            class: HashSet::new(),
            subclass: HashSet::new(),
            object_property: HashSet::new(),
            some: HashSet::new(),
            and: HashSet::new(),
        }
    }

    fn next_id(&mut self) -> usize{
        unsafe{
            COUNTER = COUNTER + 1;
            COUNTER
        }
    }

    pub fn contains_id(&self, id:usize)-> bool {
        self.id_str.contains_key(&id)
    }

    pub fn contains_iri(&self, iri:String) -> bool {
        self.str_iri.contains_key(&iri)
    }

    pub fn iri(&mut self, s: String) -> IRI {
        {
            let iri = self.str_iri.get(&s);
            if let Some(res) = iri {return res.clone();}
        }
        let id = self.next_id();
        let iri = IRI(id);
        self.str_iri.insert(s.clone(),iri);
        self.id_str.insert(id,s);
        iri
    }

    pub fn iri_to_str(&self, i: IRI ) -> Option<&String>{
        self.id_str.get(&i.0)
    }

    pub fn class(&mut self, i: IRI) -> Class {
        let c = Class(i);
        c.check(self);

        if let Some(_) = self.class.get(&c)
        {return c;}

        self.class.insert(c);
        c
    }

    pub fn object_property(&mut self, i: IRI) -> ObjectProperty{
        let o = ObjectProperty(i);
        o.check(self);

        if let Some(_) = self.object_property.get(&o)
        {return o;};

        self.object_property.insert(o);
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
        sc.check(self);

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

        some.check(self);

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

    pub fn is_subclass(&self, superclass:Class, subclass:Class)
        -> bool{
        self.is_subclass_exp(ClassExpression::Class(superclass),
                             ClassExpression::Class(subclass))
    }

    pub fn is_subclass_exp(&self, superclass:ClassExpression,
                           subclass:ClassExpression)
                       ->bool{

        let first:Option<&SubClass> =
            self.subclass.iter()
            .filter(|&sc|
                    sc.superclass == superclass &&
                    sc.subclass == subclass)
            .next();

        match first
        {
            Some(_) => true,
            None => false
        }
    }
}


#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
