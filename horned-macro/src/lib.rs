//! Write Manchester Syntax ([`omn!`]) or OWL Functional Syntax ([`ofn!`])
//! directly in Rust source -- named after the file extension each
//! format already uses elsewhere in this repo (`.omn`, `.ofn`).
//!
//! See `docs/horned-macro-plan.md` at the repository root for the design
//! rationale. In short: each macro checks the embedded text against the
//! real grammar at compile time (a genuine Rust compile error on a
//! syntax mistake), then expands to a call into the corresponding
//! `horned_owl::io::{omn,ofn}::reader::read` -- the same, already-tested
//! runtime reader -- rather than re-implementing any of its semantics
//! inside the macro.
//!
//! The document is a quoted string, not bare tokens. An earlier version
//! of `omn!` took unquoted tokens instead (no string at all) -- see
//! `docs/horned-macro-plan.md`'s "Unquoted tokens: tried and reverted"
//! section for why that was abandoned: it could never accept a full
//! `<http://...>` IRI (Rust's lexer strips `//` as a comment before any
//! macro sees tokens), which meant it wasn't actually accepting the
//! real grammar, only a CURIE-only dialect of it. A quoted string has
//! no such restriction -- the full grammar `read` already supports is
//! available here, unrestricted.

use horned_owl::io::ofn::reader::{OwlFunctionalLexer, Rule as OfnRule};
use horned_owl::io::omn::reader::{ManchesterLexer, Rule as OmnRule};
use proc_macro::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};
use syn::{Expr, LitStr, Token, parse_macro_input};

struct MacroInput {
    build: Expr,
    text: LitStr,
}

impl Parse for MacroInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let build: Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let text: LitStr = input.parse()?;
        Ok(MacroInput { build, text })
    }
}

/// Shared expansion for `omn!`/`ofn!`: run the grammar's own
/// pure-syntax check against the text, and on success emit a call into
/// the real runtime reader.
fn expand(
    input: TokenStream,
    label: &str,
    check: impl Fn(&str) -> Result<(), String>,
    reader_call: proc_macro2::TokenStream,
) -> TokenStream {
    let MacroInput { build, text } = parse_macro_input!(input as MacroInput);
    let source_text = text.value();

    if let Err(e) = check(source_text.trim()) {
        let message = format!("{label}!: invalid syntax: {e}");
        return syn::Error::new(text.span(), message)
            .to_compile_error()
            .into();
    }

    let expanded = quote! {
        match #reader_call(
            &mut #text.as_bytes(),
            ::horned_owl::io::ParserConfiguration::new(#build),
        ) {
            ::std::result::Result::Ok((onto, _prefixes)) => onto,
            ::std::result::Result::Err(e) => panic!(
                "horned-macro: `{}!` passed its compile-time syntax check but failed at \
                 runtime construction ({{e}}); this means the text is syntactically valid \
                 but was semantically rejected -- see docs/horned-macro-plan.md in the \
                 horned-owl repository",
                #label,
            ),
        }
    };

    expanded.into()
}

/// Parse `$text` as a Manchester Syntax document at compile time, and
/// expand to code that constructs the ontology at runtime via `$build`
/// (a `&Build<A>` for whatever `A: ForIRI` the surrounding code uses).
///
/// ```
/// # use horned_owl::model::Build;
/// # use horned_owl::ontology::set::SetOntology;
/// # use horned_macro::omn;
/// let b = Build::new_rc();
/// let onto: SetOntology<_> = omn!(&b, "
///     Prefix: : <http://example.org/>
///     Class: Foo
///     Class: Bar
///         SubClassOf: Foo
/// ");
/// ```
///
/// A syntax mistake in the embedded text is a compile error at the
/// macro invocation site. See `docs/horned-macro-plan.md` for why
/// *semantic* errors (rare -- e.g. a `HasKey:` data/object key
/// ambiguity) are not caught until runtime, where they surface as a
/// panic from this macro's expansion rather than a compile error.
#[proc_macro]
pub fn omn(input: TokenStream) -> TokenStream {
    expand(
        input,
        "omn",
        |text| {
            ManchesterLexer::lex(OmnRule::ManchesterDocument, text)
                .map(|_| ())
                .map_err(|e| e.to_string())
        },
        quote! { ::horned_owl::io::omn::reader::read },
    )
}

/// Parse `$text` as an OWL Functional Syntax document at compile time,
/// and expand to code that constructs the ontology at runtime via
/// `$build` (a `&Build<A>` for whatever `A: ForIRI` the surrounding
/// code uses).
///
/// ```
/// # use horned_owl::model::Build;
/// # use horned_owl::ontology::set::SetOntology;
/// # use horned_macro::ofn;
/// let b = Build::new_rc();
/// let onto: SetOntology<_> = ofn!(&b, "
///     Prefix(:=<http://example.org/>)
///     Ontology(<http://example.org/>
///         Declaration(Class(:Foo))
///         Declaration(Class(:Bar))
///         SubClassOf(:Bar :Foo)
///     )
/// ");
/// ```
///
/// A syntax mistake in the embedded text is a compile error at the
/// macro invocation site. See `docs/horned-macro-plan.md` for why
/// *semantic* errors are not caught until runtime, where they surface
/// as a panic from this macro's expansion rather than a compile error.
#[proc_macro]
pub fn ofn(input: TokenStream) -> TokenStream {
    expand(
        input,
        "ofn",
        |text| {
            OwlFunctionalLexer::lex(OfnRule::OntologyDocument, text)
                .map(|_| ())
                .map_err(|e| e.to_string())
        },
        quote! { ::horned_owl::io::ofn::reader::read },
    )
}
