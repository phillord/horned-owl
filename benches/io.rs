use criterion::{criterion_group, BenchmarkId, Criterion, Throughput};
use std::fs::File;
use std::io::BufReader;

fn io_read(c: &mut Criterion) {
    let mut group = c.benchmark_group("io_read");

    for n in [10, 100, 1_000, 2500, 5000, 10_000].iter() {
        group.throughput(Throughput::Elements(*n as u64));

        group.bench_with_input(BenchmarkId::new("owl_io_read", n), n, |b, &n| {
            b.iter(|| {
                let f = File::open(format!("benches/ont/o{}.owl", n)).ok().unwrap();
                let mut f = BufReader::new(f);
                let _ = horned_owl::io::rdf::reader::read(&mut f, Default::default()).ok();
            })
        });

        group.bench_with_input(BenchmarkId::new("owx_io_read", n), n, |b, &n| {
            b.iter(|| {
                let f = File::open(format!("benches/ont/o{}.owx", n)).ok().unwrap();
                let mut f = BufReader::new(f);
                let _ = horned_owl::io::owx::reader::read(&mut f, Default::default()).ok();
            })
        });
    }
}

criterion_group!(io, io_read);
