use criterion::{black_box, criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion};
use oxrdfio::{RdfFormat, RdfParser};
use horned_pretty_rdf::{
    ChunkedRdfXmlFormatter, ChunkedRdfXmlFormatterConfig, PBlankNode, PChunk, PNamedNode,
    PNamedOrBlankNode, PTerm, PTriple,
};

fn triple(s: PNamedOrBlankNode<String>, p: &str, o: PTerm<String>) -> PTriple<String> {
    PTriple::new(s, PNamedNode::new(p.to_string()), o)
}

/// n triples each with a distinct named-node subject — no grouping possible
fn many_subjects(n: usize) -> Vec<PTriple<String>> {
    (0..n)
        .map(|i| {
            triple(
                PNamedNode::new(format!("http://example.com/s{i}")).into(),
                "http://example.com/p",
                PTerm::NamedNode(PNamedNode::new(format!("http://example.com/o{i}"))),
            )
        })
        .collect()
}

/// n triples all sharing one subject — maximum PMultiTriple grouping
fn single_subject(n: usize) -> Vec<PTriple<String>> {
    let subj: PNamedOrBlankNode<String> = PNamedNode::new("http://example.com/s".to_string()).into();
    (0..n)
        .map(|i| {
            triple(
                subj.clone(),
                &format!("http://example.com/p{i}"),
                PTerm::NamedNode(PNamedNode::new(format!("http://example.com/o{i}"))),
            )
        })
        .collect()
}

/// A chain of n blank nodes each pointing to the next — exercises bnode elision
fn bnode_chain(n: usize) -> Vec<PTriple<String>> {
    let mut triples = Vec::with_capacity(n * 2);
    triples.push(triple(
        PNamedNode::new("http://example.com/root".to_string()).into(),
        "http://example.com/p",
        PTerm::BlankNode(PBlankNode::new("bn0".to_string())),
    ));
    for i in 0..n - 1 {
        triples.push(triple(
            PNamedOrBlankNode::BlankNode(PBlankNode::new(format!("bn{i}"))),
            "http://example.com/p",
            PTerm::BlankNode(PBlankNode::new(format!("bn{}", i + 1))),
        ));
    }
    triples.push(triple(
        PNamedOrBlankNode::BlankNode(PBlankNode::new(format!("bn{}", n - 1))),
        "http://example.com/value",
        PTerm::NamedNode(PNamedNode::new("http://example.com/end".to_string())),
    ));
    triples
}

/// An RDF list (rdf:first/rdf:rest/rdf:nil) of n items — exercises PTripleSeq
fn rdf_list(n: usize) -> Vec<PTriple<String>> {
    let rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    let mut triples = Vec::with_capacity(n * 2 + 1);

    triples.push(triple(
        PNamedNode::new("http://example.com/basket".to_string()).into(),
        "http://example.com/items",
        PTerm::BlankNode(PBlankNode::new("node0".to_string())),
    ));

    for i in 0..n {
        let subj = PNamedOrBlankNode::BlankNode(PBlankNode::new(format!("node{i}")));
        triples.push(triple(
            subj.clone(),
            &format!("{rdf}first"),
            PTerm::NamedNode(PNamedNode::new(format!("http://example.com/item{i}"))),
        ));
        let rest = if i + 1 < n {
            PTerm::BlankNode(PBlankNode::new(format!("node{}", i + 1)))
        } else {
            PTerm::NamedNode(PNamedNode::new(format!("{rdf}nil")))
        };
        triples.push(triple(subj, &format!("{rdf}rest"), rest));
    }

    triples
}

fn format_triples(triples: Vec<PTriple<String>>) -> Vec<u8> {
    let config = ChunkedRdfXmlFormatterConfig::all();
    let mut f = ChunkedRdfXmlFormatter::new(Vec::new(), config).unwrap();
    let chk = PChunk::normalize(triples);
    f.format_chunk(chk).unwrap();
    f.finish().unwrap()
}

fn bench_hello_world(c: &mut Criterion) {
    c.bench_function("hello_world", |b| {
        b.iter(|| {
            format_triples(black_box(vec![triple(
                PNamedNode::new("http://example.com/s".to_string()).into(),
                "http://example.com/p",
                PTerm::NamedNode(PNamedNode::new("http://example.com/o".to_string())),
            )]))
        })
    });
}

fn bench_normalize(c: &mut Criterion) {
    let mut group = c.benchmark_group("normalize");
    for n in [100, 500, 1000] {
        group.bench_with_input(BenchmarkId::new("many_subjects", n), &n, |b, &n| {
            b.iter(|| PChunk::normalize(black_box(many_subjects(n))))
        });
        group.bench_with_input(BenchmarkId::new("single_subject", n), &n, |b, &n| {
            b.iter(|| PChunk::normalize(black_box(single_subject(n))))
        });
        group.bench_with_input(BenchmarkId::new("rdf_list", n), &n, |b, &n| {
            b.iter(|| PChunk::normalize(black_box(rdf_list(n))))
        });
    }
    group.finish();
}

fn bench_format(c: &mut Criterion) {
    let mut group = c.benchmark_group("format");
    for n in [100, 500, 1000] {
        group.bench_with_input(BenchmarkId::new("many_subjects", n), &n, |b, &n| {
            b.iter(|| format_triples(black_box(many_subjects(n))))
        });
        group.bench_with_input(BenchmarkId::new("single_subject", n), &n, |b, &n| {
            b.iter(|| format_triples(black_box(single_subject(n))))
        });
        group.bench_with_input(BenchmarkId::new("bnode_chain", n), &n, |b, &n| {
            b.iter(|| format_triples(black_box(bnode_chain(n))))
        });
        group.bench_with_input(BenchmarkId::new("rdf_list", n), &n, |b, &n| {
            b.iter(|| format_triples(black_box(rdf_list(n))))
        });
    }
    group.finish();
}

fn parse_owl(src: &[u8]) -> Vec<PTriple<String>> {
    RdfParser::from_format(RdfFormat::RdfXml)
        .for_reader(src)
        .map(|r| r.unwrap().into())
        .collect()
}

fn bench_owl_format(c: &mut Criterion) {
    let files: &[(&str, &[u8])] = &[
        ("go-short", include_bytes!("resources/go-short.owl")),
        ("ont", include_bytes!("resources/ont.owl")),
        ("family", include_bytes!("resources/family.owl")),
    ];

    let mut group = c.benchmark_group("owl_format");
    for (name, src) in files {
        let triples = parse_owl(src);
        group.bench_function(*name, |b| {
            b.iter_batched(
                || triples.clone(),
                |t| format_triples(t),
                BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

criterion_group!(benches, bench_hello_world, bench_normalize, bench_format, bench_owl_format);
criterion_main!(benches);
