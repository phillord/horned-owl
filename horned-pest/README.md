# horned-pest

The original [pest](https://pest.rs)-based readers for OWL Functional Syntax
(`horned_pest::ofn`), Manchester Syntax (`horned_pest::omn`) and OBO
(`horned_pest::obo`), split out of `horned-owl`.

**This crate is effectively deprecated.** The hand-written readers in
`horned_owl::io::{ofn, omn, obo}::reader` accept the same languages, are
faster, and can stream; use those. What remains here is the reference
implementation: the differential tests in `tests/` check that the main
readers accept and build exactly what these do, `horned-corpus` uses it as a
baseline, and `horned-macro` uses its grammar for compile-time syntax checks.

Each module has a `read` function with the same signature as the matching
`horned-owl` reader.

Licensed under the LGPL-3.0, as `horned-owl`.
