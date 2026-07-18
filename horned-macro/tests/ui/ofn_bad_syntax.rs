use horned_macro::ofn;
use horned_owl::model::{Build, RcStr};
use horned_owl::ontology::set::SetOntology;

fn main() {
    let b: Build<RcStr> = Build::new_rc();
    let _onto: SetOntology<RcStr> = ofn!(&b, "Ontology(<http://example.org/> Declaration(");
}
