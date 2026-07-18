use horned_macro::ofn;
use horned_owl::model::{Build, ClassExpression, Component, RcStr, SubClassOf};
use horned_owl::ontology::set::SetOntology;

#[test]
fn constructs_a_small_ontology() {
    let b: Build<RcStr> = Build::new_rc();
    let onto: SetOntology<RcStr> = ofn!(
        &b,
        "
        Prefix(:=<http://example.org/>)
        Ontology(<http://example.org/>
            Declaration(Class(:Foo))
            Declaration(Class(:Bar))
            SubClassOf(:Bar :Foo)
        )
        "
    );

    let foo = b.class("http://example.org/Foo");
    let bar = b.class("http://example.org/Bar");

    let expected_subclass = Component::SubClassOf(SubClassOf {
        sup: ClassExpression::Class(foo),
        sub: ClassExpression::Class(bar),
    });

    let found = onto.iter().any(|ac| ac.component == expected_subclass);
    assert!(
        found,
        "expected SubClassOf(Bar, Foo) in the parsed ontology"
    );
}

#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/ofn_*.rs");
}
