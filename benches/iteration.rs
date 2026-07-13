use std::io::Cursor;

use criterion::{AxisScale, BatchSize, BenchmarkId, Criterion, PlotConfiguration, criterion_group};

use horned_owl::io::rdf::reader::{ConcreteRDFOntology, ConcreteRcRDFOntology};
use horned_owl::model::*;
use horned_owl::ontology::component_mapped::{ComponentMappedOntology, RcComponentMappedOntology};
use horned_owl::ontology::indexed::ForIndex;
use horned_owl::ontology::set::SetOntology;

fn build_set_ontology(n: isize) -> SetOntology<RcStr> {
    let b = Build::new_rc();
    let mut o = SetOntology::new();
    for m in 0..n {
        o.declare(b.class(format!("http://example.com/a{m}")));
    }
    o
}

fn build_component_mapped_ontology(n: isize) -> RcComponentMappedOntology {
    let b = Build::new_rc();
    let mut o = ComponentMappedOntology::new_rc();
    for m in 0..n {
        o.declare(b.class(format!("http://example.com/a{m}")));
    }
    o
}

// Time Ontology::iter (borrowing) and IntoIterator::into_iter (owning,
// consuming) separately, since the whole point of `into_component`'s
// Rc::try_unwrap fast path is that owning iteration should be cheaper
// than borrowing-then-cloning for Rc-backed ontologies.
fn synthetic(c: &mut Criterion) {
    let mut group = c.benchmark_group("iteration");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    for n in [10, 100, 1_000].iter() {
        let so = build_set_ontology(*n);
        group.bench_with_input(BenchmarkId::new("SetOntology_iter", n), n, |b, _| {
            // A naive `.count()` over an unchanging, pure iterator is loop-invariant,
            // and LLVM will happily hoist and cache it across criterion's whole
            // sampling loop; black-boxing each yielded item, not just the input,
            // forces genuine per-item work every call.
            b.iter(|| {
                Ontology::iter(std::hint::black_box(&so))
                    .map(std::hint::black_box)
                    .count()
            })
        });
        group.bench_with_input(BenchmarkId::new("SetOntology_into_iter", n), n, |b, &n| {
            b.iter_batched(
                || build_set_ontology(n),
                |so| so.into_iter().count(),
                BatchSize::SmallInput,
            )
        });

        let cmo = build_component_mapped_ontology(*n);
        group.bench_with_input(
            BenchmarkId::new("ComponentMappedOntology_iter", n),
            n,
            |b, _| {
                b.iter(|| {
                    Ontology::iter(std::hint::black_box(&cmo))
                        .map(std::hint::black_box)
                        .count()
                })
            },
        );
        group.bench_with_input(
            BenchmarkId::new("ComponentMappedOntology_into_iter", n),
            n,
            |b, &n| {
                b.iter_batched(
                    || build_component_mapped_ontology(n),
                    |cmo| cmo.into_iter().count(),
                    BatchSize::SmallInput,
                )
            },
        );
    }
}

fn family_to_vec() -> Vec<u8> {
    std::fs::read("./dev/family.owl").unwrap()
}

fn read_vec<A: ForIRI, AA: ForIndex<A>>(v: &[u8], b: Build<A>) -> ConcreteRDFOntology<A, AA> {
    let mut c = Cursor::new(v.to_owned());
    horned_owl::io::rdf::reader::read_with_build(&mut c, &b, Default::default())
        .unwrap()
        .0
}

// A real ontology with a realistic mix of constructs (not just
// DeclareClass, unlike `synthetic` above), comparing borrowing iteration
// cost across the three main ontology representations.
fn real_file(c: &mut Criterion) {
    let family = family_to_vec();
    let mut group = c.benchmark_group("iteration_family");

    let rdf_o: ConcreteRcRDFOntology = read_vec(&family, Build::new());
    group.bench_function("ConcreteRDFOntology_iter", |b| {
        b.iter(|| {
            Ontology::iter(std::hint::black_box(&rdf_o))
                .map(std::hint::black_box)
                .count()
        })
    });

    let set_o: SetOntology<RcStr> =
        read_vec::<RcStr, RcAnnotatedComponent>(&family, Build::new()).into();
    group.bench_function("SetOntology_iter", |b| {
        b.iter(|| {
            Ontology::iter(std::hint::black_box(&set_o))
                .map(std::hint::black_box)
                .count()
        })
    });

    let cmo: RcComponentMappedOntology = {
        let set_o: SetOntology<RcStr> =
            read_vec::<RcStr, RcAnnotatedComponent>(&family, Build::new()).into();
        set_o.into()
    };
    group.bench_function("ComponentMappedOntology_iter", |b| {
        b.iter(|| {
            Ontology::iter(std::hint::black_box(&cmo))
                .map(std::hint::black_box)
                .count()
        })
    });
}

// Validates that `into_component`'s identity fast-path (a pure move for
// SetOntology, since AA = AnnotatedComponent<A> there, never Rc-wrapped)
// makes owning iteration cost independent of the IRI backing type. Before
// it, cloning an already-owned-but-discarded AnnotatedComponent<String>
// had to deep-clone every embedded IRI's String, while
// AnnotatedComponent<RcStr> only bumped Rc refcounts — so String and RcStr
// diverged. Now both are just a move, so they should track each other.
fn build_set_ontology_generic<A: ForIRI>(b: &Build<A>, n: isize) -> SetOntology<A> {
    let mut o = SetOntology::new();
    for m in 0..n {
        o.declare(b.class(format!("http://example.com/a{m}")));
    }
    o
}

fn iri_backing(c: &mut Criterion) {
    let mut group = c.benchmark_group("iteration_iri_backing");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    let b_rc: Build<RcStr> = Build::new_rc();
    let b_string: Build<String> = Build::default();

    for n in [10, 100, 1_000].iter() {
        let so_rc = build_set_ontology_generic(&b_rc, *n);
        group.bench_with_input(BenchmarkId::new("RcStr_iter", n), n, |b, _| {
            b.iter(|| {
                Ontology::iter(std::hint::black_box(&so_rc))
                    .map(std::hint::black_box)
                    .count()
            })
        });
        group.bench_with_input(BenchmarkId::new("RcStr_into_iter", n), n, |b, &n| {
            b.iter_batched(
                || build_set_ontology_generic(&b_rc, n),
                |so| so.into_iter().map(std::hint::black_box).count(),
                BatchSize::SmallInput,
            )
        });

        let so_string = build_set_ontology_generic(&b_string, *n);
        group.bench_with_input(BenchmarkId::new("String_iter", n), n, |b, _| {
            b.iter(|| {
                Ontology::iter(std::hint::black_box(&so_string))
                    .map(std::hint::black_box)
                    .count()
            })
        });
        group.bench_with_input(BenchmarkId::new("String_into_iter", n), n, |b, &n| {
            b.iter_batched(
                || build_set_ontology_generic(&b_string, n),
                |so| so.into_iter().map(std::hint::black_box).count(),
                BatchSize::SmallInput,
            )
        });
    }
}

criterion_group!(iteration, synthetic, real_file, iri_backing);
