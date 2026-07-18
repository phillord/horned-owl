use horned_macro::omn;
use horned_owl::model::{Build, RcStr};
use horned_owl::ontology::set::SetOntology;

fn main() {
    let b: Build<RcStr> = Build::new_rc();
    let _onto: SetOntology<RcStr> = omn!(&b, "Class: Foo SubClassOf");
}
