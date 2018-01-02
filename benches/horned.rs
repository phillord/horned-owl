#[macro_use]
extern crate bencher;
extern crate horned_owl;

use horned_owl::*;
use bencher::Bencher;

fn a_thousand_classes(bench: &mut Bencher) {
    bench.iter(|| {
        let mut o = Ontology::new();
        for m in 1..1000 {
            let _i = o.iri(format!("http://example.com/b{}", m));
        }
    })
}


#[test]
fn big_tree(){
    let mut o = Ontology::new();
    let mut i = 1_000_000;
    create_tree(&mut o, &mut i);
}

fn create_tree(o:&mut Ontology, n:&mut i32){
    let i = o.iri(format!("http://example.com/a{}", n));
    let c = o.class(i);
    create_tree_0(o, vec![c], n );
}

fn create_tree_0(o:&mut Ontology,
                 current:Vec<Class>, remaining:&mut i32){
    let mut next = vec![];

    for curr in current.into_iter() {
        let i = o.iri(format!("http://example.com/a{}", remaining));
        let c = o.class(i);
        *remaining = *remaining - 1;
        let i = o.iri(format!("http://example.com/a{}",
                              remaining));
        let d = o.class(i);
        *remaining = *remaining - 1;

        next.push(c);
        next.push(d);

        o.subclass(curr, c);
        o.subclass(curr, d);

        if *remaining < 0 {
            return
        }
    }
    create_tree_0(o, next, remaining);
}

#[test]
fn is_subclass_with_many_direct_subclasses(){
    let mut o = Ontology::new();
    let i = o.iri("http://example.com/a".to_string());
    let c = o.class(i);

    let n = 1_000;
    for m in 1..n {
        let i =
            o.iri(format!("http://example.com/b{}", m));
        let d = o.class(i);
        o.subclass(c,d);
    }

    let i = o.iri(format!("http://example.com/b{}", n - 1));
    let d = o.class(i);

    assert!(!o.is_subclass(d,c));
    assert!(o.is_subclass(c,d));
}



benchmark_group!(benches, a_thousand_classes, big_tree
                 is_subclass_with_many_direct_subclasses
);
benchmark_main!(benches);
