use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufRead;

use curie::PrefixMapping;
use pest::Span;

use crate::error::HornedError;
use crate::io::ParserConfiguration;
use crate::model::AnnotatedComponent;
use crate::model::Build;
use crate::model::ForIRI;
use crate::model::IRI;
use crate::model::MutableOntology;
use crate::model::NamedEntityKind;
use crate::model::Ontology;
use crate::visitor::mutable::WalkMut;

mod ambiguity;
mod frames;
mod from_pair;
mod lexer;

use self::ambiguity::ComponentVisitor;
use self::from_pair::FromPair;
use self::from_pair::MutableOntologyWrapper;
use self::lexer::OwlManchesterLexer;
use self::lexer::Rule;

struct Context<'a, A: ForIRI> {
    build: &'a Build<A>,
    mapping: PrefixMapping,
    entity_kinds: HashMap<IRI<A>, crate::model::NamedEntityKind>,
    ambiguous_components: HashSet<(AnnotatedComponent<A>, Span<'a>)>,
}

impl<'a, A: ForIRI> Context<'a, A> {
    fn new(build: &'a Build<A>, mapping: PrefixMapping) -> Self {
        Self {
            build,
            mapping,
            entity_kinds: Default::default(),
            ambiguous_components: Default::default(),
        }
    }

    fn add_ambiguous_component(&mut self, component: AnnotatedComponent<A>, span: Span<'a>) {
        self.ambiguous_components.insert((component, span));
    }

    fn record_entity_kind(&mut self, iri: impl Into<IRI<A>>, kind: NamedEntityKind) {
        self.entity_kinds.insert(iri.into(), kind);
    }

    fn get_property_kind(&self, iri: &IRI<A>) -> Option<NamedEntityKind> {
        self.entity_kinds.get(iri).copied()
    }
}

pub fn read<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default, R: BufRead>(
    bufread: R,
    _config: ParserConfiguration,
) -> Result<(O, PrefixMapping), HornedError> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

pub fn read_with_build<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default, R: BufRead>(
    mut bufread: R,
    build: &Build<A>,
) -> Result<(O, PrefixMapping), HornedError> {
    let mut ctx = Context::new(build, PrefixMapping::default());

    // FIXME: implement iterative parser (this is possible in )
    let mut doc = String::new();
    bufread.read_to_string(&mut doc)?;

    let pair = OwlManchesterLexer::lex(Rule::OntologyDocument, doc.trim())?
        .next()
        .unwrap();

    let wrapper: Result<(MutableOntologyWrapper<A, O>, PrefixMapping), HornedError> =
        FromPair::from_pair(pair, &mut ctx);

    let (mut ontology, mapping) = wrapper.map(|r| (r.0.0, r.1))?;

    let mut walk = WalkMut::new(ComponentVisitor {
        entity_kinds: ctx.entity_kinds,
    });
    for (mut component, _) in ctx.ambiguous_components {
        walk.annotated_component(&mut component);
        ontology.insert(component);
    }

    Ok((ontology, mapping))
}

#[cfg(test)]
mod tests {
    use crate::model::*;
    use crate::ontology::set::SetOntology;

    #[test]
    fn undeclared_property_defaults_to_object() {
        let build = Build::<String>::new();

        let input = r#"
        Ontology:
            Class: <http://example.com/ontology/classB>

            SubClassOf:
                <http://example.com/ontology/propA> some <http://example.com/ontology/classA>
        "#;

        let (ont, _): (SetOntology<String>, _) =
            super::read_with_build(&mut input.as_bytes(), &build).unwrap();

        let expected = SetOntology::from_iter(vec![
            DeclareClass(build.class("http://example.com/ontology/classB")).into(),
            SubClassOf {
                sub: ClassExpression::Class(build.class("http://example.com/ontology/classB")),
                sup: ClassExpression::ObjectSomeValuesFrom {
                    ope: build
                        .object_property("http://example.com/ontology/propA")
                        .into(),
                    bce: build.class("http://example.com/ontology/classA").into(),
                },
            }
            .into(),
        ]);

        assert_eq!(ont, expected);
    }
}
