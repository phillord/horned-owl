//! A small dogfooding smoke test for the `horned-macro` crate's `omn!`
//! macro, added as a dev-dependency (see docs/horned-macro-plan.md,
//! phase 6). Not a rewrite of existing fixtures -- just proof the
//! macro works from within `horned-owl`'s own test suite, the way a
//! downstream test fixture would use it.

use horned_macro::omn;
use horned_owl::model::{Build, RcStr};
use horned_owl::ontology::set::SetOntology;

#[test]
fn omn_macro_builds_an_ontology() {
    let b: Build<RcStr> = Build::new_rc();
    let onto: SetOntology<RcStr> = omn!(
        &b,
        "
        Prefix: : <http://example.org/>
        Class: Pizza
        Class: Margherita
            SubClassOf: Pizza
        "
    );

    assert_eq!(onto.iter().count(), 3); // 2 declarations + 1 SubClassOf
}
