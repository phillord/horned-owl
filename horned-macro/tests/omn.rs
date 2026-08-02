use horned_macro::omn;
use horned_owl::model::{Build, ClassExpression, Component, RcStr, SubClassOf};
use horned_owl::ontology::set::SetOntology;

#[test]
fn constructs_a_small_ontology() {
    let b: Build<RcStr> = Build::new_rc();
    let onto: SetOntology<RcStr> = omn!(
        &b,
        "
        Prefix: : <http://example.org/>
        Class: Foo
        Class: Bar
            SubClassOf: Foo
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
fn works_with_a_bare_iri_and_no_prefix_declaration() {
    let b: Build<RcStr> = Build::new_rc();
    let onto: SetOntology<RcStr> = omn!(&b, "Class: <http://example.org/OnlyClass>");
    assert_eq!(onto.iter().count(), 1);
}

#[test]
fn ui() {
    // trybuild compiles tests/ui/*.rs and checks the output against the
    // matching *.stderr -- this is the "a syntax mistake is a genuine
    // compile error" guarantee that's the whole point of the macro.
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/bad_syntax.rs");
}

#[test]
fn works_with_a_raw_string() {
    let b: Build<RcStr> = Build::new_rc();
    let onto: SetOntology<RcStr> = omn!(
        &b,
        r#"
        Prefix: : <http://example.org/>
        Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        Class: Foo
            Annotations: rdfs:comment "contains a \backslash and \"escaped-looking\" text, untouched by a raw string"
        "#
    );
    assert_eq!(onto.iter().count(), 2); // the Class: declaration plus its annotation assertion
}
