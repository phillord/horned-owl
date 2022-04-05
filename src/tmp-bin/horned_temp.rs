use horned_owl::error::underlying;
use horned_owl::io::owx::writer::write;
use horned_owl::model::Build;
use horned_owl::model::MutableOntology;
use horned_owl::ontology::set::SetOntology;

use std::io::stdout;

fn main() {
    let b:Build<String> = Build::new();
    let mut o = SetOntology::new();

    for i in 1..10 + 1 {
        o.declare(b.class(format!("https://www.example.com/o{}", i)));
    }

    write(&mut stdout(), &o.into(), None).map_err(underlying);
}
