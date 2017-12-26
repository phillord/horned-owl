extern crate horned_owl;


use horned_owl::*;

pub fn main(){
    let mut o = MutableOntology::new();

    let i = o.iri("http://example.com/c".to_string());
    let c = o.class(i);
    let i = o.iri("http://example.com/d".to_string());
    let d = o.class(i);

    let i = o.iri("http://example.com/r".to_string());
    let ob = o.object_property(i);

    let some = o.some(ob, d);
    let se = o.subclass_exp(ClassExpression::Class(c),some);
    println!("{:?}",se);


}
