#[macro_use]
extern crate bencher;
extern crate horned_owl;

use horned_owl::*;
use bencher::Bencher;

fn a_thousand_classes(bench: &mut Bencher) {
    bench.iter(|| {
        let mut o = MutableOntology::new();
        for m in 1..1000 {
            let _i = o.iri(format!("http://example.com/b{}", m));
        }
    })
}

benchmark_group!(benches, a_thousand_classes);
benchmark_main!(benches);
