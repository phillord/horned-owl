extern crate horned_owl_index;

use horned_owl_index::*;


pub fn main(){
    let mut o = MutableOntology::new();

    let i = o.iri("http://example.com/c".to_string());

    println!("i is {:?}", i);

    let c = o.class(i);

    println!("c is {:?}", c);

    let i = o.iri("http://example.com/d".to_string());
    let d = o.class(i);

    let sc = o.subclass(c,d);
    println!("sc is {:?}", sc);
    let dsc = o.direct_subclass(c);

    println!("dsc is {:?}", dsc);

    println!("is_subclass {:?}", o.is_subclass(c, d));

    let iri = o.iri("http://example.com/r".to_string());
    let op = o.object_property(iri);
    let some = o.some(op,d);
    let sc2 = o.subclass_exp(ClassExpression::Class(c), some.clone());
    println!("some is {:?}", some);
    let dsc = o.direct_subclass(c);
    println!("dsc is {:?}", dsc);
}
