use std::io::Cursor;
use std::rc::Rc;

use criterion::{criterion_group, AxisScale, BenchmarkId, Criterion, PlotConfiguration};

use horned_owl::io::rdf::reader::ConcreteRDFOntology;
use horned_owl::model::*;
use horned_owl::ontology::component_mapped::ComponentMappedOntology;
use horned_owl::ontology::declaration_mapped::DeclarationMappedIndex;
use horned_owl::ontology::indexed::{
    ForIndex, FourIndexedOntology, OneIndexedOntology, TwoIndexedOntology,
};
use horned_owl::ontology::iri_mapped::IRIMappedIndex;
use horned_owl::ontology::logically_equal::LogicallyEqualIndex;
use horned_owl::ontology::set::*;

fn create_many_classes(i: isize) {
    let b = Build::new_rc();
    let mut o = SetOntology::new();
    for m in 1..i {
        let i = b.iri(format!("http://example.com/b{}", m));
        let _c = o.declare(b.class(i));
    }
}

// We start by testing the basic ontology model
fn classes(c: &mut Criterion) {
    let mut group = c.benchmark_group("classes");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    for n in [100, 1_000, 10_000, 100_000].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(n), n, |b, &n| {
            b.iter(|| create_many_classes(n));
        });
    }
}

fn create_tree<A: ForIRI, O: MutableOntology<A>>(b: &Build<A>, o: &mut O, n: isize) {
    let i = b.iri(format!("http://example.com/a{}", n));
    let c = b.class(i);
    create_tree_0(b, o, vec![c], n);
}

fn create_tree_0<A: ForIRI, O: MutableOntology<A>>(
    b: &Build<A>,
    o: &mut O,
    current: Vec<Class<A>>,
    mut remaining: isize,
) {
    let mut next = vec![];

    for curr in current.into_iter() {
        let i = b.iri(format!("http://example.com/a{}", remaining));
        let c = b.class(i);
        remaining -= 1;
        let i = b.iri(format!("http://example.com/a{}", remaining));
        let d = b.class(i);
        remaining -= 1;

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

        if remaining < 0 {
            return;
        }
    }
    create_tree_0(b, o, next, remaining);
}

// Now test to see what impact the pointer and caching of strings has
fn tree(c: &mut Criterion) {
    let mut group = c.benchmark_group("tree");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    for n in [100, 1_000, 10_000, 100_000].iter() {
        group.bench_with_input(BenchmarkId::new("Rc", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_rc();
                let mut o = SetOntology::new();
                create_tree(&b, &mut o, n);
            })
        });

        group.bench_with_input(BenchmarkId::new("Arc", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_arc();
                let mut o = SetOntology::new();
                create_tree(&b, &mut o, n);
            })
        });

        group.bench_with_input(BenchmarkId::new("str", n), n, |b, &n| {
            b.iter(|| {
                let b: Build<String> = Build::default();
                let mut o = SetOntology::new();
                create_tree(&b, &mut o, n);
            })
        });
    }
}

// Now test to see the impact of the pointers in indexes have
fn set_index_tree(c: &mut Criterion) {
    let mut group = c.benchmark_group("set_index_tree");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    for n in [100, 1_000, 10_000, 100_000].iter() {
        group.bench_with_input(BenchmarkId::new("SetOntology", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_rc();
                let mut o = SetOntology::new();
                create_tree(&b, &mut o, n);
            })
        });

        group.bench_with_input(BenchmarkId::new("SetOntologyRcIndex", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_rc();
                let mut o = OneIndexedOntology::new(SetIndex::new_rc());
                create_tree(&b, &mut o, n);
            })
        });

        group.bench_with_input(BenchmarkId::new("SetOntologyArcIndex", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_arc();
                let mut o = OneIndexedOntology::new_arc(SetIndex::new());
                create_tree(&b, &mut o, n);
            })
        });
    }
}

// Test all of the different indexes
fn multi_index_tree(c: &mut Criterion) {
    let mut group = c.benchmark_group("multi_index_tree");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    for n in [100, 1_000, 10_000, 50_000, 100_000].iter() {
        group.bench_with_input(BenchmarkId::new("SetOntology", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_rc();
                let mut o = SetOntology::new();
                create_tree(&b, &mut o, n);
            })
        });

        group.bench_with_input(
            BenchmarkId::new("ComponentMappedOntology", n),
            n,
            |b, &n| {
                b.iter(|| {
                    let b = Build::new_rc();
                    let mut o = ComponentMappedOntology::new_rc();
                    create_tree(&b, &mut o, n);
                })
            },
        );

        group.bench_with_input(
            BenchmarkId::new("DeclarationMappedOntology", n),
            n,
            |b, &n| {
                b.iter(|| {
                    let b = Build::new_rc();
                    // This is not normally the right way to use
                    // DeclarationMappedIndex as it does not guarantee to
                    // store all axioms
                    let mut o: OneIndexedOntology<_, Rc<AnnotatedComponent<_>>, _> =
                        OneIndexedOntology::new(DeclarationMappedIndex::default());
                    create_tree(&b, &mut o, n);
                })
            },
        );

        group.bench_with_input(
            BenchmarkId::new("SetAndDeclarationMappedOntology", n),
            n,
            |b, &n| {
                b.iter(|| {
                    let b = Build::new_rc();
                    // This is a more realistic use of TwoIndexOntology
                    let mut o: TwoIndexedOntology<_, Rc<AnnotatedComponent<_>>, _, _> =
                        TwoIndexedOntology::new(
                            SetIndex::default(),
                            DeclarationMappedIndex::default(),
                        );
                    create_tree(&b, &mut o, n);
                })
            },
        );

        group.bench_with_input(BenchmarkId::new("LogicallyEqualOntology", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_rc();
                let mut o: OneIndexedOntology<_, Rc<AnnotatedComponent<_>>, _> =
                    OneIndexedOntology::new(LogicallyEqualIndex::new());
                create_tree(&b, &mut o, n);
            })
        });

        group.bench_with_input(BenchmarkId::new("IRIMappedOntology", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_rc();
                let mut o: OneIndexedOntology<_, Rc<AnnotatedComponent<_>>, _> =
                    OneIndexedOntology::new(IRIMappedIndex::new());
                create_tree(&b, &mut o, n);
            })
        });

        group.bench_with_input(BenchmarkId::new("FourIndexedOntology", n), n, |b, &n| {
            b.iter(|| {
                let b = Build::new_rc();
                let mut o: FourIndexedOntology<_, Rc<AnnotatedComponent<_>>, _, _, _, _> =
                    FourIndexedOntology::new(
                        SetIndex::new(),
                        SetIndex::new(),
                        SetIndex::new(),
                        SetIndex::new(),
                    );
                create_tree(&b, &mut o, n);
            })
        });
    }
}

fn food_to_vec() -> Vec<u8> {
    std::fs::read("./benches/ont/food.owl").unwrap()
}

fn read_vec<A: ForIRI, AA: ForIndex<A>>(v: &Vec<u8>, b: Build<A>) -> ConcreteRDFOntology<A, AA> {
    let mut c = Cursor::new(v.clone());
    horned_owl::io::rdf::reader::read_with_build(&mut c, &b, Default::default())
        .unwrap()
        .0
}

// Test the different pointers on a more realistic workload
fn food(c: &mut Criterion) {
    let food = food_to_vec();

    let mut group = c.benchmark_group("food");

    group.bench_function("food_rc_str_rc_comp", |b| {
        b.iter(|| {
            let b: Build<RcStr> = Build::new();
            let _: ConcreteRDFOntology<RcStr, RcAnnotatedComponent> = read_vec(&food, b);
        })
    });

    group.bench_function("food_arc_str_arc_comp", |b| {
        b.iter(|| {
            let b: Build<ArcStr> = Build::new();
            let _: ConcreteRDFOntology<ArcStr, ArcAnnotatedComponent> = read_vec(&food, b);
        })
    });

    group.bench_function("food_string_direct_comp", |b| {
        b.iter(|| {
            let b: Build<String> = Build::new();
            let _: ConcreteRDFOntology<String, AnnotatedComponent<String>> = read_vec(&food, b);
        })
    });
}

criterion_group!(model, classes, tree, set_index_tree, multi_index_tree, food);
