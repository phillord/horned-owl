use criterion::{BenchmarkId, Criterion, Throughput, criterion_group, criterion_main};
use horned_owl::model::{Build, RcStr};
use horned_owl::ontology::set::SetOntology;

mod generate;

fn resolver(vocabulary: String) -> impl Fn(&str) -> Option<String> {
    move |iri| {
        (iri.trim_end_matches('#') == generate::VOCABULARY_IRI.trim_end_matches('#'))
            .then(|| vocabulary.clone())
    }
}

fn bench_read(c: &mut Criterion) {
    let mut group = c.benchmark_group("oml_read");
    for n in [100, 1_000, 10_000] {
        let vocabulary = generate::vocabulary(n);
        group.throughput(Throughput::Bytes(vocabulary.len() as u64));
        group.bench_function(BenchmarkId::new("vocabulary", n), |b| {
            b.iter(|| {
                let build: Build<RcStr> = Build::new();
                let _: SetOntology<RcStr> = horned_oml::reader::read(&vocabulary, &build).unwrap();
            })
        });

        let description = generate::description(n, n * 10);
        let resolve = resolver(vocabulary);
        group.throughput(Throughput::Bytes(description.len() as u64));
        group.bench_function(BenchmarkId::new("description", n), |b| {
            b.iter(|| {
                let build: Build<RcStr> = Build::new();
                let _: SetOntology<RcStr> =
                    horned_oml::reader::read_with(&description, &build, &resolve).unwrap();
            })
        });
    }
    group.finish();
}

criterion_group!(oml_read, bench_read);
criterion_main!(oml_read);
