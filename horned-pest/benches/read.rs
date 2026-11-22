use criterion::{Criterion, criterion_group, criterion_main};
use horned_bin::generate_big_owl;
use horned_owl::model::RcStr;
use horned_owl::ontology::set::SetOntology;
use std::fs::{File, create_dir_all};
use std::io::BufReader;
use std::time::Duration;

fn bench_pest_read(c: &mut Criterion) {
    let mut group = c.benchmark_group("io_read");
    create_dir_all("target/bench-data").unwrap();
    for n in [10, 100, 1_000, 2500, 5000, 10_000].iter() {
        let generate_if_needed = |filename, n: &isize, format| {
            if let Ok(mut f) = File::create_new(filename) {
                generate_big_owl(*n, format, &mut f).unwrap();
            }
        };

        let filename = format!("target/bench-data/o{}.ofn", n);
        generate_if_needed(filename.clone(), n, "ofn");
        group.bench_function(criterion::BenchmarkId::new("ofn_pest_io_read", n), |b| {
            b.iter(|| {
                let mut f = BufReader::new(File::open(filename.clone()).unwrap());
                let _: (SetOntology<RcStr>, _) =
                    horned_pest::ofn::read(&mut f, Default::default()).unwrap();
            })
        });

        let filename = format!("target/bench-data/o{}.omn", n);
        generate_if_needed(filename.clone(), n, "omn");
        group.bench_function(criterion::BenchmarkId::new("omn_pest_io_read", n), |b| {
            b.iter(|| {
                let f = BufReader::new(File::open(filename.clone()).unwrap());
                let _: (SetOntology<RcStr>, _) =
                    horned_pest::omn::read(f, Default::default()).unwrap();
            })
        });
    }
}

criterion_group! {
    name = read;
    config = Criterion::default()
    .sample_size(50)
    .measurement_time(Duration::from_secs(20));
    targets = bench_pest_read
}
criterion_main!(read);
