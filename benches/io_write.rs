use criterion::{AxisScale, BenchmarkId, Criterion, PlotConfiguration, criterion_group};
use horned_owl::model::{Build, MutableOntology, RcStr};
use horned_owl::ontology::component_mapped::RcComponentMappedOntology;
use horned_owl::ontology::set::SetOntology;
use horned_pretty_rdf::ox::WriterQuadSerializerAdaptor;
use oxrdfio::RdfSerializer;
use std::io::sink;
use std::time::Duration;

fn build_ontology(n: isize) -> RcComponentMappedOntology {
    let b = Build::new_rc();
    let mut o: SetOntology<RcStr> = SetOntology::new_rc();
    for i in 1..=n {
        o.declare(b.class(format!("https://www.example.com/o{}", i)));
    }
    o.into()
}

fn bench_io_write(c: &mut Criterion) {
    let mut group = c.benchmark_group("io_write");
    group.plot_config(PlotConfiguration::default().summary_scale(AxisScale::Logarithmic));

    for n in [10, 100, 1_000, 2500, 5000, 10_000].iter() {
        let ont = build_ontology(*n);

        // RDF/XML: pretty formatter (horned-pretty-rdf)
        group.bench_function(BenchmarkId::new("rdf_xml_pretty_io_write", n), |b| {
            b.iter(|| {
                horned_owl::io::rdf::writer::write_cmo(sink(), &ont, None).ok();
            })
        });

        // RDF/XML: plain oxrdfio serializer
        group.bench_function(BenchmarkId::new("rdf_xml_plain_io_write", n), |b| {
            b.iter(|| {
                let f = WriterQuadSerializerAdaptor::new(
                    RdfSerializer::from_format(oxrdfio::RdfFormat::RdfXml).for_writer(sink()),
                );
                horned_owl::io::rdf::writer::write_to_rdf_formatter(&ont, f).ok();
            })
        });

        group.bench_function(BenchmarkId::new("ttl_io_write", n), |b| {
            b.iter(|| {
                horned_owl::io::rdf::writer::write_to_rdf_format(sink(), &ont, "ttl").ok();
            })
        });

        group.bench_function(BenchmarkId::new("nt_io_write", n), |b| {
            b.iter(|| {
                horned_owl::io::rdf::writer::write_to_rdf_format(sink(), &ont, "nt").ok();
            })
        });

        group.bench_function(BenchmarkId::new("owx_io_write", n), |b| {
            b.iter(|| {
                horned_owl::io::owx::writer::write_cmo(sink(), &ont, None).ok();
            })
        });

        group.bench_function(BenchmarkId::new("ofn_io_write", n), |b| {
            b.iter(|| {
                horned_owl::io::ofn::writer::write_cmo(sink(), &ont, None).ok();
            })
        });

        group.bench_function(BenchmarkId::new("omn_io_write", n), |b| {
            b.iter(|| {
                horned_owl::io::omn::writer::write_cmo(sink(), &ont, None).ok();
            })
        });
    }
}

criterion_group! {
    name = io_write;
    config = Criterion::default()
    .sample_size(50)
    .measurement_time(Duration::from_secs(20));
    targets = bench_io_write
}
