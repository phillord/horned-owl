#[macro_use]
extern crate bencher;
extern crate horned_owl;

use horned_owl::model::*;

use bencher::Bencher;

fn a_thousand_classes(bench: &mut Bencher) {
    bench.iter(|| {
        let mut o = Ontology::new();
        for m in 1..1000 {
            let i = o.iri(format!("http://example.com/b{}", m));
            let _c = o.class(i);
        }
    })
}


fn big_tree(bench: &mut Bencher){
    bench.iter(|| {
        let mut o = Ontology::new();
        let mut i = 10_000;
        create_tree(&mut o, &mut i);
    })
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

        next.push(c.clone());
        next.push(d.clone());

        o.subclass(curr.clone(), c);
        o.subclass(curr, d);

        if *remaining < 0 {
            return
        }
    }
    create_tree_0(o, next, remaining);
}

fn is_subclass_with_many_direct_subclasses(bench: &mut Bencher){
    bench.iter(|| {
        let mut o = Ontology::new();
        let i = o.iri("http://example.com/a".to_string());
        let c = o.class(i);

        let n = 1_000;
        for m in 1..n {
            let i =
                o.iri(format!("http://example.com/b{}", m));
            let d = o.class(i);
            o.subclass(c.clone(),d.clone());
        }

        let i = o.iri(format!("http://example.com/b{}", n - 1));
        let d = o.class(i);

        assert!(!o.is_subclass(&d,&c));
        assert!(o.is_subclass(&c,&d));
    })
}



benchmark_group!(benches, a_thousand_classes, big_tree, is_subclass_with_many_direct_subclasses);


use std::fs::File;
use std::io::BufReader;


fn io_read(bench: &mut Bencher){
    bench.iter(||{
        let f = File::open("benches/ont/o100.owl").ok().unwrap();
        let mut f = BufReader::new(f);

        horned_owl::io::reader::read(&mut f);
    })
}

benchmark_group!(iobenches, io_read);

benchmark_main!(benches,iobenches);
