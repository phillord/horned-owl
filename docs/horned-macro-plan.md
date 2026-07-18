# `horned-macro`: write Manchester syntax directly in Rust

## Problem

Constructing an `Ontology`/`Component` tree by hand in Rust (test fixtures, inline example
ontologies, small embedded ontology snippets in application code) means writing out
`Build`/`SubClassOf { sup: ..., sub: ... }`/`b.class(...)` calls verbatim, which is verbose and
far removed from how anyone actually thinks about OWL. `horned-owl` already has two full,
well-tested textual syntaxes (Manchester Syntax and OWL Functional Syntax) — the goal here is to
let those be written directly in Rust source as a macro literal, e.g.:

```rust
let onto: SetOntology<RcStr> = omn!(&b, "
    Prefix: : <http://example.org/>
    Class: Foo
    Class: Bar
        SubClassOf: Foo
");
```

with syntax errors reported as *Rust compile errors*, at the macro invocation site, not as a
runtime panic three test-runs later.

## Design decision: compile-time syntax check, runtime construction — not a from-scratch engine

The tempting design is "parse Manchester into `Component`s entirely inside the proc-macro, at
compile time." This is a trap for this particular grammar: `horned-owl`'s Manchester reader
(`src/io/omn/reader/`) is not a pure grammar→AST transform. It does real semantic work — a
declaration pre-pass that disambiguates data vs. object properties, HasKey key typing, and more
(see the extensive module-doc "Supported §2.5 surface" / "Residual constructs" notes in
`src/io/omn/reader/mod.rs`). Re-implementing that inside a proc-macro, which runs in a separate
compilation context with no access to a real `Build<A>` (IRI interning is inherently a *runtime*
concern — `A` isn't even known at macro-expansion time; it's whatever the call site's `Build`
happens to be instantiated with), would mean either duplicating a large, actively-evolving piece
of semantic logic, or reinventing IRI interning at compile time for no reason. Both are a bad
trade for a first version.

Instead: `horned-macro` depends on `horned-owl` directly (proc-macro crates can depend on regular
crates freely — only the reverse is forbidden) and reuses two things that are *already* cleanly
separated in the existing reader:

1. **`horned_owl::io::omn::reader::{ManchesterLexer, Rule}`** — the pure `pest` grammar parse
   step (`ManchesterLexer::lex(Rule::ManchesterDocument, text)`), already public, already used
   internally exactly this way before the semantic passes run. This takes a plain `&str` and
   needs no `Build`, no IRI type, nothing runtime — perfect for compile time. Calling this from
   the proc-macro against the string literal's contents is the entire "compile-time check."
2. **`horned_owl::io::omn::reader::read_with_build`** — the existing, fully-tested runtime
   reader. The macro's expansion is just a call into this, with the caller's `Build` and the
   embedded string. All the semantic complexity above stays exactly where it already is, single
   source of truth, zero drift between "what the macro accepts" and "what the real reader
   accepts" (impossible for them to disagree, since it's the same code).

This is the same shape as `sqlx::query!`: real compile-time verification against the real grammar
(so typos are caught early, with a real Rust compile error), but the actual construction still
happens at runtime through the already-correct, already-tested machinery. It is a small amount of
new code — a proc-macro that extracts a string literal, calls an existing pure function on it,
and emits a call to another existing function — not a new parser.

**Trade-off, stated plainly:** this catches *syntax* errors at compile time but not *semantic*
ones (e.g. a HasKey data/object key ambiguity) — those still only surface at runtime, as a panic
from the macro's expansion (see API sketch). Given how rarely the semantic passes reject
something the grammar accepts, this is expected to be the overwhelming common case caught, for a
fraction of the engineering cost of a full compile-time semantic engine. Worth revisiting only if
it proves to be a real gap in practice.

## Detour: unquoted tokens, tried and reverted

An unquoted calling convention was tried and shipped briefly, then reverted back to the quoted
string shown above. Worth keeping this section rather than deleting it: the empirical findings
below are real and would resurface if unquoted tokens are ever tried again, and the reason for
reverting is a real design conclusion, not just a preference flip.

The idea: drop the surrounding string entirely, e.g.
`omn!(&b, Prefix: ex = "http://example.org/" Class: ex:Foo Class: ex:Bar SubClassOf: ex:Foo)`.
The design above (compile-time check + runtime construction via the real reader) survived intact
under this convention too -- only the input-parsing/reassembly step changed. Two hard constraints,
found empirically (not guessed), shaped what that version had to look like:

1. **A full `<http://...>` IRI can never appear as bare (unquoted) macro tokens.** Rust's own
   lexer strips `//` as a line comment *before any macro -- proc or `macro_rules!` -- ever sees a
   token stream*; this happens at the compiler's tokenizer stage, upstream of all macro expansion.
   Confirmed directly: `show!(Prefix: : <http://example.org/> Class: Foo)` through a trivial
   `macro_rules!` doesn't just mis-tokenize the IRI, it eats the rest of the line -- including the
   macro's own closing `)` and the following `;` -- producing a "mismatched closing delimiter"
   error. There is no proc-macro-side workaround; the only way content survives Rust's tokenizer
   with `//` intact is inside a string (or raw string) literal, because string contents are
   captured verbatim, never re-tokenized.

   Consequence: `omn!` cannot accept a bare `<http://...>` IRI anywhere. The one remaining quoted
   piece is a `Prefix: name = "iri"` declaration's IRI string; every entity reference after that
   is a bare CURIE (`ex:Foo`), which real Manchester documents mostly use anyway once prefixes are
   declared.

2. **`proc_macro2::TokenStream::to_string()` inserts a space around every token, including `:`,**
   turning `ex:Foo` into `ex : Foo` on reconstruction -- which the grammar rejects, since a CURIE
   (`prefix:LocalName`), a frame keyword (`Class:`, `SubClassOf:`, ...), and a blank node (`_:id`)
   are all lexed as a single unit with **no** whitespace around their `:`. (This is *not* the same
   spacing behaviour as the compiler-builtin `stringify!` macro, which happened to preserve
   `ex:Foo` with no space in an isolated test -- the two have different, non-interchangeable
   pretty-printing rules; don't assume one behaves like the other.) Fixed by writing a small
   custom re-stringifier (`stringify_tokens` in `horned-macro/src/lib.rs`) instead of using
   `TokenStream`'s own `Display`: it never inserts a space immediately before or after a `:`
   token, and otherwise respects Rust's own `Punct::spacing()` `Joint` hint (needed so a literal's
   `^^datatype` suffix -- two adjacent `^` tokens -- doesn't get split apart either). Verified
   against the real grammar via `ManchesterLexer::lex`, not assumed.

That token-parsing/reassembly version worked, was fully tested (including the `trybuild`
negative case, still passing), and was briefly the shipped design. **Reverted anyway.** The
reason: the CURIE-only restriction it required isn't a minor ergonomic wrinkle, it's a scope cut
that stops the macro from being a Manchester Syntax macro at all -- you categorically cannot
write a full `<http://...>` IRI in it, anywhere, ever, on stable Rust. That's a real subset of
the real grammar permanently out of reach, for the sake of dropping one pair of quote marks. The
quoted-string version has no such restriction: the *entire* §2.5 grammar `read_with_build`
supports is available, unrestricted, because the text never has to survive Rust's tokenizer at
all -- it's opaque string contents. "No quotes" reads nicer at the call site, but "not actually
Manchester syntax" is the wrong trade for what this macro is for.

## Scope

`omn!` (Manchester Syntax) and `ofn!` (OWL Functional Syntax) — named after the file extension
each format already uses elsewhere in this repo (`.omn`, `.ofn`), matching how the request to add
functional syntax support was framed ("we can use the file name extension as the macro name").
`ofn!` followed `omn!` in the same session once the design settled, since the OFN reader has
exactly the same shape as Manchester's: a `pest` grammar (`src/grammars/ofn.pest`) plus a
semantic pass, cleanly separated the same way. The only `horned-owl`-side change `ofn!` needed
was widening `src/io/ofn/reader/mod.rs`'s `mod lexer;`/private `use` to `pub mod lexer;`/`pub use`
-- `OwlFunctionalLexer`/`Rule` weren't previously exported the way Manchester's already were.

Both macros take a full document (prefixes/imports plus one or more
frames/axioms — the same thing each format's `io::*::reader::read` accepts), not a single bare
axiom or class expression. This is the broadest-leverage form: it reuses `read_with_build`
exactly as-is, and covers the main use case (test fixtures, small embedded ontologies) directly. A
finer-grained `omn_class!`/`omn_axiom!` for a single expression is plausible future work (noted
below), not v1.

**Considered and rejected: `include_str!("file.omn")` support.** Since `omn!`/`ofn!` are
function-like proc-macros, they always receive raw, unexpanded tokens -- a nested `include_str!`
call is never pre-resolved before they see it (confirmed directly: passing one to the `LitStr`
parser fails with "expected string literal"). It's possible to work around this: detect
`include_str!(...)` syntactically, read the file directly inside the proc-macro's own process
(ordinary, non-const code — heap allocation is completely fine there), resolving a relative path
against the calling file's own directory via the now-stable `Span::local_file()`. This was built
and confirmed working (a real bubo-generated `.omn` file, checked at compile time, embedded into
the binary) — then deliberately not kept. It was built purely to answer "is this possible," not
because the macro needed it; keeping bespoke path-resolution/file-reading logic in the macro for a
convenience nobody asked to keep isn't worth the added surface area, especially since one real gap
remains even in the working version: `proc_macro::tracked_path` (which would make Cargo rebuild
when the included file changes, like the compiler's own `include_str!` does) isn't stable, so
edits to the included file don't reliably trigger a rebuild. If this is wanted later, the
implementation is straightforward to redo -- `resolve_text` in an earlier revision of
`horned-macro/src/lib.rs` is the reference.

**Considered and rejected: compile-time construction via `const fn`.** Not viable for two
independent, both-fatal reasons, not just impractical: (1) `const` evaluation has no filesystem
access at all, on stable or nightly, by design — this is exactly why `include_str!`/`include_bytes!`
exist as special compiler builtins rather than being expressible in ordinary Rust; (2) even given
the text already in hand, the actual `Ontology`/`Component` value can't be a `const` regardless,
since `Build`'s IRI interning and `SetOntology`/`ComponentMappedOntology` are all
heap-allocated (`Rc`, `HashSet`, `Vec`, `IndexMap`), and const evaluation cannot allocate on the
heap on stable Rust -- confirmed directly: even a plain `Vec::push` inside a `const fn` fails to
compile on the toolchain used here (rustc 1.97.0) with "not yet stable as a const fn". This is
exactly why the compile-time half of `omn!`/`ofn!` only ever runs the pure syntax check (which
happens in the proc-macro's own ordinary process, free to heap-allocate) and always defers actual
construction to a generated runtime call -- a `const fn` could not do either half of what these
macros do.

## Public API sketch

```rust
// horned-macro/src/lib.rs

/// Parse `$manchester_text` as a Manchester Syntax document at compile time
/// (a real Rust compile error if it doesn't parse), and expand to code that
/// constructs the ontology at runtime via `$build`.
///
/// `$build` must be a `&Build<A>` for whatever `A: ForIRI` the surrounding
/// code is using. The expression's type is inferred the normal way from
/// context (e.g. a `let` binding's type annotation), exactly as if you'd
/// called `horned_owl::io::omn::reader::read_with_build` yourself.
///
/// # Panics
/// If the embedded text passes the compile-time syntax check but the
/// runtime semantic reader still rejects it (rare -- see
/// docs/horned-macro-plan.md's "Design decision" section) or errors for an
/// unrelated reason (e.g. an unresolvable prefix), the expansion panics
/// with the underlying `HornedError`'s message. This is a deliberate v1
/// simplification (see "Open questions" below) rather than forcing every
/// call site to unwrap a `Result` for what's meant to be an inline literal.
#[proc_macro]
pub fn omn(input: TokenStream) -> TokenStream { .. }
```

Usage:

```rust
use horned_owl::model::{Build, RcStr};
use horned_owl::ontology::set::SetOntology;
use horned_macro::omn;

let b: Build<RcStr> = Build::new_rc();
let onto: SetOntology<RcStr> = omn!(&b, "
    Prefix: : <http://example.org/>
    Class: Foo
    Class: Bar
        SubClassOf: Foo
");
```

Expansion (conceptually — exact hygiene/temporary-naming TBD during implementation):

```rust
{
    match ::horned_owl::io::omn::reader::read_with_build(
        "Prefix: : <http://example.org/>\nClass: Foo\n...".as_bytes(),
        &b,
    ) {
        ::std::result::Result::Ok((onto, _prefixes)) => onto,
        ::std::result::Result::Err(e) => panic!(
            "horned-macro: `omn!` passed its compile-time syntax check but failed at \
             runtime construction ({e}) -- this means the text is syntactically valid \
             Manchester but semantically rejected; see docs/horned-macro-plan.md"
        ),
    }
}
```

## Phased implementation plan

1. **Scaffold**: `horned-macro/Cargo.toml` (`[lib] proc-macro = true`), depending on `horned-owl`
   (path dependency, matching how `horned-catalog`/`horned-pretty-rdf` are wired into the
   workspace), `syn` (parsing the macro's own input: an expression, a comma, a string literal),
   `quote` (codegen), `proc-macro2`. Wired into the workspace `members`/`default-members`.
2. **Input parsing**: a small `syn::parse::Parse` impl for `(Expr, LitStr)` — the two
   comma-separated macro arguments.
3. **Compile-time check**: call `horned_owl::io::omn::reader::ManchesterLexer::lex(Rule::ManchesterDocument,
   &lit_str.value())`. On `Err`, turn the `HornedError`'s message (it carries a byte
   position/span via `Location` — see `src/error.rs`) into a `syn::Error` pointing at the string
   literal's span (span-level sub-highlighting of *where inside* the string is a stable-Rust
   limitation — see "Open questions"; v1 points at the whole literal and puts the byte/line
   position in the message text instead) and return `.to_compile_error()`.
4. **Codegen**: emit the expansion sketched above, using the original `Expr` for `$build` and the
   string literal's value embedded as a Rust string literal (re-quoted, not re-parsed).
5. **Tests**: proc-macro crates can't easily unit-test their own macro expansion in the same
   crate; the standard pattern is a `tests/` integration test in `horned-macro` itself that
   actually invokes `omn!` and asserts on the resulting `SetOntology`, plus a `trybuild`
   dev-dependency `tests/ui/bad_syntax.rs` + matching `.stderr` proving a genuine syntax mistake
   becomes a compile error, not a runtime panic — regenerated via `TRYBUILD=overwrite` and then
   verified stable on a normal run.
6. **`horned-owl` dev-dependency**: `horned-macro` added as a dev-dependency of `horned-owl`
   itself (a dev-dependency cycle — fully supported by Cargo, verified working here), with one
   illustrative smoke test (`tests/horned_macro_smoke.rs`) using `omn!` from within `horned-owl`'s
   own test suite. Rewriting existing fixtures to use it is out of scope for this session.

All six phases above are done and green (build/test/clippy/fmt clean across the whole workspace)
as of this session. The unquoted-tokens detour (see above) was built, fully tested, and then
reverted back to this design within the same session. `ofn!` was added afterwards, following
exactly the same shape (own `MacroInput`/`expand` reuse in `horned-macro/src/lib.rs`, own
`tests/ofn.rs` + `tests/ui/ofn_bad_syntax.rs`+`.stderr`), including its own `trybuild`
negative case proving OFN syntax mistakes are compile errors too.

## Open questions / deliberately deferred

- **Sub-span error highlighting.** Pointing a compile error at the *exact character* inside a
  multi-line string literal that has the syntax error (rather than underlining the whole literal)
  needs `proc_macro::Span::subspan`, which is nightly-only as of this writing. V1 underlines the
  whole string literal and puts `line N, column C within the text` in the message text instead.
  Worth revisiting if/when `subspan` stabilises.
- **A single-expression form** (`omn_class!`/`omn_axiom!` for one class expression or axiom
  rather than a whole document) — `io::omn::reader::parse_class_expression` already exists and
  is the right building block if this is wanted later; not v1.
- **Should semantic (not just syntax) errors also be caught at compile time?** Would need the
  proc-macro to either duplicate the declaration pre-pass or find a way to run the *real* semantic
  reader at compile time without a live `Build<A>` (e.g. a compile-time-only dummy `ForIRI`
  impl just for validation, discarding the interned IRIs afterward). Deferred pending evidence
  this is a real gap, not a hypothetical one — see the "Design decision" trade-off note above.
