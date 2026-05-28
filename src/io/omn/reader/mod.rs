use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufRead;

use curie::PrefixMapping;
use pest::Span;

use crate::error::HornedError;
use crate::io::ParserConfiguration;
use crate::model::Build;
use crate::model::Component;
use crate::model::ForIRI;
use crate::model::IRI;
use crate::model::MutableOntology;
use crate::model::Ontology;

mod frames;
mod from_pair;
mod lexer;

use self::from_pair::FromPair;
use self::from_pair::MutableOntologyWrapper;
use self::lexer::OwlManchesterLexer;
use self::lexer::Rule;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum PropertyKind {
    Object,
    Data,
    Annotation,
}

struct Context<'a, A: ForIRI> {
    build: &'a Build<A>,
    mapping: &'a PrefixMapping,
    property_kinds: HashMap<IRI<A>, PropertyKind>,
    ambiguous_components: HashSet<(Component<A>, Span<'a>)>,
    allow_partial_parsing: bool,
}

impl<'a, A: ForIRI> Context<'a, A> {
    fn new(build: &'a Build<A>, mapping: &'a PrefixMapping) -> Self {
        Self {
            build,
            mapping,
            property_kinds: Default::default(),
            ambiguous_components: Default::default(),
            allow_partial_parsing: false,
        }
    }

    fn add_ambiguous_component(&mut self, component: Component<A>, span: Span<'a>) {
        self.ambiguous_components.insert((component, span));
    }

    fn mark_property_kind(&mut self, iri: impl Into<IRI<A>>, kind: PropertyKind) {
        self.property_kinds.insert(iri.into(), kind);
    }

    fn get_property_kind(&self, iri: impl Into<IRI<A>>) -> Option<PropertyKind> {
        self.property_kinds.get(&iri.into()).copied()
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
    let prefixes = PrefixMapping::default();
    let mut ctx = Context::new(build, &prefixes);

    // FIXME: implement iterative parser (this is possible in )
    let mut doc = String::new();
    bufread.read_to_string(&mut doc)?;

    let pair = OwlManchesterLexer::lex(Rule::OntologyDocument, doc.trim())?
        .next()
        .unwrap();

    let wrapper: Result<(MutableOntologyWrapper<A, O>, PrefixMapping), HornedError> =
        FromPair::from_pair(pair, &mut ctx);

    let (ontology, mapping) = wrapper.map(|r| (r.0.0, r.1))?;

    Ok((ontology, mapping))
}
