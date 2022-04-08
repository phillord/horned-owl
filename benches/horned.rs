#[macro_use]
extern crate bencher;
extern crate horned_owl;

use horned_owl::model::*;
use horned_owl::ontology::set::*;

use bencher::Bencher;

fn a_thousand_classes(bench: &mut Bencher) {
    bench.iter(|| {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        for m in 1..1000 {
            let i = b.iri(format!("http://example.com/b{}", m));
            let _c = o.declare(b.class(i));
        }
    })
}

fn big_tree(bench: &mut Bencher) {
    bench.iter(|| {
        let b = Build::new_rc();
        let mut o = SetOntology::new();
        let mut i = 10_000;
        create_tree(&b, &mut o, &mut i);
    })
}

fn create_tree<A:ForIRI, O:MutableOntology<A>>(b: &Build<A>, o: &mut O, n: &mut i32) {

    let i = b.iri(format!("http://example.com/a{}", n));
    let c = b.class(i);
    create_tree_0(b, o, vec![c], n);
}

fn create_tree_0<A:ForIRI, O:MutableOntology<A>>(b:&Build<A>, o: &mut O, current: Vec<Class<A>>, remaining: &mut i32) {
    let mut next = vec![];

    for curr in current.into_iter() {
        let i = b.iri(format!("http://example.com/a{}", remaining));
        let c = b.class(i);
        *remaining = *remaining - 1;
        let i = b.iri(format!("http://example.com/a{}", remaining));
        let d = b.class(i);
        *remaining = *remaining - 1;

        next.push(c.clone());
        next.push(d.clone());

        o.insert(SubClassOf::new(
            ClassExpression::Class(curr.clone()),
            ClassExpression::Class(c),
        ));
        o.insert(SubClassOf::new(
            ClassExpression::Class(curr),
            ClassExpression::Class(d),
        ));

        if *remaining < 0 {
            return;
        }
    }
    create_tree_0(b, o, next, remaining);
}

// fn is_subclass_with_many_direct_subclasses(bench: &mut Bencher) {
//     bench.iter(|| {
//         let b = Build::default();
//         let mut o = SetOntology::new();
//         let i = b.iri("http://example.com/a".to_string());
//         let c = b.class(i);

//         let n = 1_000;
//         for m in 1..n {
//             let i = b.iri(format!("http://example.com/b{}", m));
//             let d = b.class(i);
//             o.insert(SubClassOf::new(
//                 ClassExpression::Class(c.clone()),
//                 ClassExpression::Class(d.clone()),
//             ));
//         }

//         let i = b.iri(format!("http://example.com/b{}", n - 1));
//         let d = b.class(i);
//         o.declare(d.clone());

//         assert!(!o.is_subclass(&d, &c));
//         assert!(o.is_subclass(&c, &d));
//     })
// }

benchmark_group!(
    benches,
    a_thousand_classes,
    big_tree,
//    is_subclass_with_many_direct_subclasses
);


use curie::PrefixMapping;
use std::fs::File;
use std::io::BufReader;
use std::rc::Rc;

fn io_read(bench: &mut Bencher) {
    bench.iter(|| {
        let f = File::open("benches/ont/o100.owl").ok().unwrap();
        let mut f = BufReader::new(f);

        let _: Option<(SetOntology<Rc<str>>, PrefixMapping)> = horned_owl::io::owx::reader::read(&mut f).ok();
    })
}


fn bigger_tree<A:ForIRI>(b: Build<A>){
    let o = SetOntology::new();
    bigger_tree_in_ontology(b, o)
}

fn bigger_tree_in_ontology<A:ForIRI, O: MutableOntology<A>>(b:Build<A>, mut o: O){
    let mut i = 100_000;
    create_tree(&b, &mut o, &mut i);
}

fn bigger_tree_rc(bench: &mut Bencher) {
    bench.iter(|| {
        bigger_tree(Build::new_rc())
    })
}

fn bigger_tree_arc(bench: &mut Bencher) {
    bench.iter(|| {
        bigger_tree(Build::new_arc())
    })
}

fn bigger_tree_string(bench: &mut Bencher) {
    bench.iter(|| {
        let b:Build<String> = Build::default();
        bigger_tree(b)
    })
}

use horned_owl::ontology::indexed::OneIndexedOntology;
use std::sync::Arc;

fn bigger_tree_set_index_rc(bench: &mut Bencher) {
    bench.iter(|| {
        let b:Build<Rc<str>> = Build::new_rc();
        let o = OneIndexedOntology::new_rc(SetIndex::new());
        bigger_tree_in_ontology(b, o)
    }
    )
}

/*fn big_tree_set_index_mixed(bench: &mut Bencher) {
    bench.iter(|| {
        let b:Build<Rc<str>> = Build::new_rc();
        let o = OneIndexedOntology::new_arc(SetIndex::new());
        bigger_tree_in_ontology(b, o)
    }
    )
}*/

fn bigger_tree_set_index_arc(bench: &mut Bencher) {
    bench.iter(|| {
        let b:Build<Arc<str>> = Build::new_arc();
        let o = OneIndexedOntology::new_arc(SetIndex::new());
        bigger_tree_in_ontology(b, o)
    }
    )
}



benchmark_group!(iribench, bigger_tree_rc, bigger_tree_arc, bigger_tree_string);
benchmark_group!(indexbench, bigger_tree_set_index_rc,bigger_tree_set_index_arc);
benchmark_group!(iobenches, io_read);

benchmark_main!(benches, iobenches, iribench, indexbench);
