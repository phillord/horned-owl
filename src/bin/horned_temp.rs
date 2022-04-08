
use horned_owl::error::underlying;
use horned_owl::io::owx::writer::write;
use horned_owl::model::Build;
use horned_owl::model::MutableOntology;
use horned_owl::ontology::set::SetOntology;
use horned_owl::ontology::axiom_mapped::RcAxiomMappedOntology;

use std::io::stdout;

fn main() {
    let b = Build::new_rc();
    let mut o = SetOntology::new_rc();

    for i in 1..10 + 1 {
        o.declare(b.class(format!("https://www.example.com/o{}", i)));
    }

    let amo: RcAxiomMappedOntology = o.into();
    write(&mut stdout(), &amo, None).map_err(underlying).unwrap();
}
