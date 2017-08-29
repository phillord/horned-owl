#![allow(dead_code)]

use std::collections::HashMap;
use std::collections::HashSet;

static mut COUNTER: usize = 0;

pub trait Indexable{
    fn to_index(&self) -> Box<Iterator<Item=usize>>;
}

#[derive(Eq,PartialEq,Hash,Copy,Clone,Debug)]
pub struct IRI(usize);

#[derive(Eq,PartialEq,Hash,Copy,Clone,Debug)]
pub struct Class(usize);

impl Indexable for Class{
    fn to_index(&self) -> Box<Iterator<Item=usize>>{
        Box::new(vec![self.0].into_iter())
    }
}

#[derive(Eq,PartialEq,Hash,Copy,Clone,Debug)]
pub struct ObjectProperty(usize);

impl Indexable for ObjectProperty{
    fn to_index(&self) -> Box<Iterator<Item=usize>>{
        Box::new(vec![self.0].into_iter())
    }
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct SubClass{
    superclass: ClassExpression,
    subclass: ClassExpression,
}

impl Indexable for SubClass{
    fn to_index(&self) -> Box<Iterator<Item=usize>>{
        Box::new(self
                 .superclass.to_index()
                 .chain(self.subclass.to_index()))
    }
}

#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub struct Some{
    object_property: ObjectProperty,
    filler: Box<ClassExpression>
}

impl Indexable for Some{
    fn to_index(&self) -> Box<Iterator<Item=usize>>{
        Box::new(self.object_property.to_index()
                 .chain(self.filler.to_index()))
    }
}
#[derive(Eq,PartialEq,Hash,Clone,Debug)]
pub enum ClassExpression
{
    Class(Class),
    Some(Some)
}

impl Indexable for ClassExpression{
    fn to_index(&self) -> Box<Iterator<Item=usize>>{
        match self{
            &ClassExpression::Class(ref i) => i.to_index(),
            &ClassExpression::Some(ref i)  => i.to_index(),
        }
    }
}
pub struct MutableOntology
{
    str_iri: HashMap<String,IRI>,
    id_str: HashMap<usize,String>,
    class: HashSet<Class>,
    subclass: HashSet<SubClass>,
    object_property: HashSet<ObjectProperty>,
    some: HashSet<ClassExpression>,
}

impl MutableOntology {
    pub fn new() -> MutableOntology{
        MutableOntology{
            str_iri: HashMap::new(),
            id_str: HashMap::new(),
            class: HashSet::new(),
            subclass: HashSet::new(),
            object_property: HashSet::new(),
            some: HashSet::new(),
        }
    }

    fn next_id(&mut self) -> usize{
        unsafe{
            COUNTER = COUNTER + 1;
            COUNTER
        }
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

    pub fn class(&mut self, i: IRI) -> Class {
        let c = Class(i.0);

        if let Some(_) = self.class.get(&c)
        {return c;}

        self.class.insert(c);
        c
    }

    pub fn object_property(&mut self, i: IRI) -> ObjectProperty{
        let o = ObjectProperty(i.0);

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
