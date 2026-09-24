# `horned-catalog`: OASIS XML Catalog support (issue #144)

## Problem

`horned-owl` currently resolves
`owl:imports` IRIs with a purely
heuristic scheme
(`src/resolve.rs::localize_iri`): guess
a handful of candidate local paths
relative to the importing document's own
path, and fall back to a network fetch
if none exist. There is no way to tell
it "this IRI maps to exactly this local
file," which is what every other OWL
tool (ROBOT, Protégé, the OWL API) uses
`catalog-v001.xml` (the OASIS XML
Catalog format) for. This is issue
[#144](https://github.com/phillord/horned-owl/issues/144),
also requested against `py-horned-owl`
([ontology-tools/py-horned-owl#43](https://github.com/ontology-tools/py-horned-owl/issues/43)).

phillord's own scoping on the issue:
*"it is just about handling imports...
Currently, Rust takes the IRI that is
imported and resolves it."* — i.e. this
is a new resolution path, plus (per
filippodebortoli's follow-up question,
agreed to) a validity check that can be
run before parsing to confirm every
catalog entry points at a file that
actually exists.

## Design decision: a standalone crate, not a module

`horned-catalog` will be a new workspace
member, not a module inside
`horned-owl`. It:

- Depends on nothing from `horned-owl`
  (no `horned-owl` dependency in its own
  `Cargo.toml`).
- Is generic over the *string type* it
  resolves, not
  `horned_owl::model::IRI<A>`. Every
  public entry point that takes an
  IRI-like value is bounded by
  `AsRef<str>` (occasionally `+
  Into<String>` where an owned copy
  needs to be stored), not by
  `horned_owl::model::ForIRI`.
  `horned_owl::model::IRI<A>` already
  satisfies `AsRef<str>` via its own
  `ForIRI` bound, so `horned-owl` can
  pass its `IRI<A>` values straight
  through with no adapter code — but so
  can a plain `&str`, `String`, or
  anyone else's IRI newtype. No custom
  trait needs to be invented or exported
  for this; `AsRef<str>` is the
  compatibility contract.

Rationale: catalog resolution is a
generically useful, small,
self-contained piece of functionality
(XML-format parsing + a lookup/rewrite
algorithm) that has nothing to do with
OWL axioms, ontology models, or
`ForIRI`'s heavier bound set (`Ord`,
`Hash`, `Deref<Target=str>`,
`From<String>`, ...). Keeping it
standalone means: it's independently
testable without pulling in the OWL
model at all, it's reusable outside
`horned-owl` (e.g. from `horned-bin`
directly, or by someone else's tool),
and `horned-owl`'s own dependency
surface for this feature stays a single
crate boundary, not a tangle of
`ForIRI`-generic code threaded through
`resolve.rs`.

`horned-owl` will depend on
`horned-catalog` (one direction only)
and do the small amount of glue work —
converting
`horned_catalog::CatalogError` into
`HornedError`, and plugging a
`horned_catalog::Resolver` into
`ParserConfiguration`/`ClosureOntologyParser`.

## Scope for v1 (per phillord's "just about handling imports" scoping)

The OASIS XML Catalog spec ([full
spec](https://www.oasis-open.org/committees/entity/spec-2001-08-06.html))
has entry types (`public`,
`delegatePublic`, `delegateSystem`, ...)
that exist for SGML/DTD-style
public-identifier resolution, which no
OWL tool catalog ever uses. Supporting
the full spec is not worth the surface
area. v1 supports the subset that
ROBOT/Protégé-generated catalogs
actually contain:

| Entry | Support in v1 |
|---|---|
| `<uri name="IRI" uri="local-path"/>` | Yes — the primary case; direct IRI → path mapping |
| `<system systemId="IRI" uri="local-path"/>` | Yes — treated identically to `uri` for our purposes (no DTD SYSTEM-identifier distinction applies to OWL) |
| `<rewriteURI uriStartString="prefix" rewritePrefix="local-prefix"/>` | Yes — longest-prefix-match rewriting, per spec |
| `<rewriteSystem systemIdStartString="..." rewritePrefix="..."/>` | Yes — same handling as `rewriteURI` |
| `<nextCatalog catalog="path"/>` | Yes — chase to another catalog file if the current one has no match |
| `<group prefer="..." xml:base="...">` | Partial — entries are flattened out of groups; `xml:base` is honoured for relative path resolution within the group; `prefer` (public vs. system precedence) is not meaningful here (no public IDs) and is ignored |
| `<public>`, `<delegatePublic>`, `<delegateSystem>`, `<delegateURI>` | **Not supported in v1** — no OWL catalog in the wild uses these; documented as a known gap, easy to add later behind the same `CatalogEntry` enum if ever needed |

Resolution order within a catalog,
matching the spec: `uri`/`system` exact
matches first, then
`rewriteURI`/`rewriteSystem`
longest-prefix match, then `nextCatalog`
delegation in document order. First
successful match wins.

## Public API sketch

```rust
// horned-catalog/src/lib.rs

/// A parsed OASIS XML Catalog (the subset described in docs/horned-catalog-plan.md).
pub struct Catalog {
    entries: Vec<CatalogEntry>,
    base: PathBuf, // directory the catalog file itself lives in; relative `uri`/rewritePrefix targets are resolved against this
}

enum CatalogEntry {
    Uri { name: String, uri: String },
    RewriteUri { start: String, prefix: String },
    NextCatalog { path: PathBuf },
}

pub enum CatalogError {
    Io(std::io::Error),
    Xml(quick_xml::Error),        // or a thin wrapper, TBD once implementation starts
    Malformed(String),            // e.g. missing required attribute
}

impl Catalog {
    /// Parse a catalog file from disk.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Catalog, CatalogError>;

    /// Parse catalog XML already in memory (for embedding / tests), with `base`
    /// as the directory relative `uri` targets should resolve against.
    pub fn from_str(xml: &str, base: impl AsRef<Path>) -> Result<Catalog, CatalogError>;

    /// Resolve `iri` to a local path, if this catalog (including any
    /// `nextCatalog` chain) has an entry for it. Returns `None`, not an
    /// error, on no match -- callers fall back to their own resolution
    /// (e.g. horned-owl's existing heuristic / remote fetch).
    pub fn resolve(&self, iri: impl AsRef<str>) -> Option<PathBuf>;

    /// Validate that every `uri`/`rewriteURI` target this catalog can
    /// produce points at a file that actually exists on disk. Returns
    /// every failing entry, not just the first -- see filippodebortoli's
    /// request on #144 for a pre-parse validity check.
    pub fn validate(&self) -> Vec<CatalogValidationError>;
}
```

No `Resolver`/state beyond `Catalog`
itself is needed for v1 —
`Catalog::resolve` is a pure function of
its own parsed entries, so there's no
separate "resolver" object to design.

## Phased implementation plan

1. **Scaffold**
   (`horned-catalog/Cargo.toml`,
   `src/lib.rs`), wired into the
   workspace `[workspace] members` /
   `default-members` and
   `[workspace.dependencies]`, matching
   the `horned-pretty-rdf` subcrate's
   pattern. Depends on `quick-xml`
   directly (already a workspace
   dependency of `horned-owl`, but
   `horned-catalog` pins its own version
   — no dependency on `horned-owl`
   itself).
2. **Catalog parsing**:
   `Catalog::from_str`/`from_path`,
   `CatalogEntry`, error type. Unit
   tests against representative fixture
   XML (a real ROBOT-style
   `catalog-v001.xml`, plus edge cases:
   missing file, malformed XML,
   unsupported entry types silently
   ignored per spec rather than erroring
   — an unsupported entry is not a
   malformed catalog).
3. **Resolution algorithm**:
   `Catalog::resolve` — exact
   `uri`/`system` match, then
   longest-prefix
   `rewriteURI`/`rewriteSystem`, then
   `nextCatalog` delegation. Unit tests
   per case in the scope table above.
4. **Validation**: `Catalog::validate`.
   Unit tests: valid catalog (empty
   error vec), catalog with a dangling
   target, catalog with a broken
   `nextCatalog` chain.
5. **`horned-owl` integration**:
   - Add `horned-catalog` to
     `horned-owl`'s own
     `[dependencies]`.
   - `ParserConfiguration` (or
     `RDFParserConfiguration`, TBD which
     layer is right once this is
     reached) gains an optional
     `catalog:
     Option<Rc<horned_catalog::Catalog>>`
     (needs to be `Clone`-cheap since
     `ParserConfiguration` is copied
     around recursively in
     `ClosureOntologyParser::parse_iri`).
   - `resolve.rs::resolve_iri` consults
     the catalog (if present) *before*
     the existing `localize_iri`
     heuristic and before remote
     fallback -- an explicit catalog
     mapping is a stronger signal than a
     path guess.
   - `impl
     From<horned_catalog::CatalogError>
     for HornedError`.
   - New `HornedError` variant if needed
     (`CatalogError` wrapping the source
     error), or reuse `ImportError` --
     decide during implementation once
     the error shapes are concrete.
6. **`horned-bin` CLI** (later phase,
   not this session unless time allows):
   a `--catalog <path>` global option
   alongside the existing
   `--lax`/`--remote-body-limit`/`--local-only`,
   and possibly a `horned
   validate-catalog <path>` subcommand
   for the standalone validity check.

This session's implementation work
covers phases 1-4 (the standalone crate,
fully tested) and starts phase 5
(`horned-owl` integration). Phase 6
(CLI) is left for a follow-up.

## Open questions / deliberately deferred

- Whether `Catalog::resolve` should also
  handle the "IRI used as both physical
  location and Ontology IRI" conflation
  issue raised in
#153 -- out of scope for #144 itself;
noted here so it isn't silently
forgotten if it resurfaces during
integration.
- Whether multiple catalogs (e.g. one
  per imported ontology's own directory,
  not just one global catalog) should be
  auto-discovered, the way `robot` walks
  up looking for `catalog-v001.xml`. v1
  takes a single explicit catalog path
  from the caller; auto-discovery is a
  plausible v2.
