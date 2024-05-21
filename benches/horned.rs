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

fn create_tree<A: ForIRI, O: MutableOntology<A>>(b: &Build<A>, o: &mut O, n: &mut i32) {
    let i = b.iri(format!("http://example.com/a{}", n));
    let c = b.class(i);
    create_tree_0(b, o, vec![c], n);
}

fn create_tree_0<A: ForIRI, O: MutableOntology<A>>(
    b: &Build<A>,
    o: &mut O,
    current: Vec<Class<A>>,
    remaining: &mut i32,
) {
    let mut next = vec![];

    for curr in current.into_iter() {
        let i = b.iri(format!("http://example.com/a{}", remaining));
        let c = b.class(i);
        *remaining -= 1;
        let i = b.iri(format!("http://example.com/a{}", remaining));
        let d = b.class(i);
        *remaining -= 1;

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

fn io_read(bench: &mut Bencher) {
    bench.iter(|| {
        let f = File::open("benches/ont/o100.owl").ok().unwrap();
        let mut f = BufReader::new(f);

        let _: Option<(SetOntology<RcStr>, PrefixMapping)> =
            horned_owl::io::owx::reader::read(&mut f, Default::default()).ok();
    })
}

benchmark_group!(iobenches, io_read);

fn bigger_tree<A: ForIRI>(b: Build<A>) {
    let o = SetOntology::new();
    bigger_tree_in_ontology(b, o)
}

fn bigger_tree_in_ontology<A: ForIRI, O: MutableOntology<A>>(b: Build<A>, mut o: O) {
    let mut i = 100_000;
    create_tree(&b, &mut o, &mut i);
}

fn bigger_tree_rc(bench: &mut Bencher) {
    bench.iter(|| bigger_tree(Build::new_rc()))
}

fn bigger_tree_arc(bench: &mut Bencher) {
    bench.iter(|| bigger_tree(Build::new_arc()))
}

fn bigger_tree_string(bench: &mut Bencher) {
    bench.iter(|| {
        let b: Build<String> = Build::default();
        bigger_tree(b)
    })
}

benchmark_group!(
    iribench,
    bigger_tree_rc,
    bigger_tree_arc,
    bigger_tree_string
);

use horned_owl::ontology::indexed::OneIndexedOntology;
use std::sync::Arc;

fn bigger_tree_set_index_rc(bench: &mut Bencher) {
    bench.iter(|| {
        let b: Build<RcStr> = Build::new_rc();
        let o = OneIndexedOntology::new_rc(SetIndex::new());
        bigger_tree_in_ontology(b, o)
    })
}

fn bigger_tree_set_index_annotated_component_rc_iri(bench: &mut Bencher) {
    bench.iter(|| {
        let b = Build::new_rc();
        let o: OneIndexedOntology<RcStr, AnnotatedComponent<RcStr>, _> =
            OneIndexedOntology::new(SetIndex::new());
        bigger_tree_in_ontology(b, o)
    })
}

fn bigger_tree_set_index_arc(bench: &mut Bencher) {
    bench.iter(|| {
        let b: Build<ArcStr> = Build::new_arc();
        let o = OneIndexedOntology::new_arc(SetIndex::new());
        bigger_tree_in_ontology(b, o)
    })
}
benchmark_group!(
    indexbench,
    bigger_tree_set_index_rc,
    bigger_tree_set_index_arc,
    bigger_tree_set_index_annotated_component_rc_iri
);

use horned_owl::io::rdf::reader::RDFOntology;
use horned_owl::ontology::indexed::ForIndex;
use std::io::Cursor;

fn food_to_vec() -> Vec<u8> {
    std::fs::read("./benches/ont/food.owl").unwrap()
}

fn read_vec<A: ForIRI, AA: ForIndex<A>>(v: &Vec<u8>, b: Build<A>) -> RDFOntology<A, AA> {
    let mut c = Cursor::new(v.clone());
    horned_owl::io::rdf::reader::read_with_build(&mut c, &b, Default::default())
        .unwrap()
        .0
}

fn food_rc_index_rc_iri(bench: &mut Bencher) {
    let v = food_to_vec();
    bench.iter(|| {
        let b: Build<RcStr> = Build::new();
        let _o: RDFOntology<RcStr, RcAnnotatedComponent> = read_vec(&v, b);
    })
}

fn food_direct_index_rc_iri(bench: &mut Bencher) {
    let v = food_to_vec();
    bench.iter(|| {
        let b: Build<RcStr> = Build::new();
        let _o: RDFOntology<RcStr, AnnotatedComponent<RcStr>> = read_vec(&v, b);
    })
}

fn food_arc_index_arc_iri(bench: &mut Bencher) {
    let v = food_to_vec();
    bench.iter(|| {
        let b: Build<ArcStr> = Build::new();
        let _o: RDFOntology<ArcStr, Arc<AnnotatedComponent<ArcStr>>> = read_vec(&v, b);
    })
}
fn food_direct_index_arc_iri(bench: &mut Bencher) {
    let v = food_to_vec();
    bench.iter(|| {
        let b: Build<ArcStr> = Build::new();
        let _o: RDFOntology<ArcStr, AnnotatedComponent<ArcStr>> = read_vec(&v, b);
    })
}

benchmark_group!(
    foodbench,
    food_rc_index_rc_iri,
    food_direct_index_rc_iri,
    food_arc_index_arc_iri,
    food_direct_index_arc_iri
);

/*
fn pizza_to_vec() -> Vec<u8> {
    std::fs::read("./benches/ont/pizza.owl").unwrap()
}

fn pizza_rc_index_rc_iri(bench: &mut Bencher) {
    let v = pizza_to_vec();
    bench.iter(|| {
        let b:Build<RcStr> = Build::new();
        let _o:RDFOntology<RcStr, RcAnnotatedComponent> = read_vec(&v, b);
    }
    )
}

fn pizza_direct_index_rc_iri(bench: &mut Bencher) {
    let v = pizza_to_vec();
    bench.iter(|| {
        let b:Build<RcStr> = Build::new();
        let _o:RDFOntology<RcStr, AnnotatedComponent<RcStr>> = read_vec(&v, b);
    }
    )
}

fn pizza_arc_index_arc_iri(bench: &mut Bencher) {
    let v = pizza_to_vec();
    bench.iter(|| {
        let b:Build<ArcStr> = Build::new();
        let _o:RDFOntology<ArcStr, Arc<AnnotatedComponent<ArcStr>>> = read_vec(&v, b);
    }
    )
}
fn pizza_direct_index_arc_iri(bench: &mut Bencher) {
    let v = pizza_to_vec();
    bench.iter(|| {
        let b:Build<ArcStr> = Build::new();
        let _o:RDFOntology<ArcStr, AnnotatedComponent<ArcStr>> = read_vec(&v, b);
    }
    )
}

benchmark_group!(pizzabench, pizza_rc_index_rc_iri, pizza_direct_index_rc_iri, pizza_arc_index_arc_iri, pizza_direct_index_arc_iri);
*/

benchmark_main!(benches, iobenches, iribench, indexbench, foodbench);
//benchmark_main!(pizza bench);
