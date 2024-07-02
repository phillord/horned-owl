use criterion::{criterion_group, AxisScale, BenchmarkId, Criterion, PlotConfiguration};
use horned_owl::model::RcStr;
use horned_owl::ontology::set::SetOntology;
use std::fs::File;
use std::io::BufReader;
use std::time::Duration;

fn io_read(c: &mut Criterion) {
    let mut group = c.benchmark_group("io_read");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    for n in [10, 100, 1_000, 2500, 5000, 10_000].iter() {
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
                let _: (SetOntology<RcStr>, _) =
                    horned_owl::io::owx::reader::read(&mut f, Default::default())
                        .ok()
                        .unwrap();
            })
        });
    }
}

criterion_group! {
    name = io;
    config = Criterion::default()
    .sample_size(50)
    .measurement_time(Duration::from_secs(20));
    targets = io_read
}
