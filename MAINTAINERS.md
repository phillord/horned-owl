# Maintainers

This is a list of maintainers for crates, directories or files

## Workspace crates

| Crate | Maintainer(s) |
|---|---|
| `horned-owl` (root library) | Phillip Lord |
| `horned-bin` | Phillip Lord |
| `horned-catalog` | Phillip Lord |
| `horned-pretty-rdf` | Phillip Lord |
| `horned-macro` | Phillip Lord |

## Modules within `horned-owl`

| Module | Maintainer(s) | Notes |
|---|---|---|
| `src/model.rs` | Phillip Lord    | |
| `src/vocab.rs` | Phillip Lord    | |
| `src/ontology/`| Phillip Lord    | |
| `src/visitor/` | Phillip Lord    | |
| `src/error.rs` | Phillip Lord    | |
| `src/io/rdf/`  | Phillip Lord    | |
| `src/io/owx/`  | Phillip Lord    | |
| `src/io/ofn/`  | Martin Larralde | Martin wrote the original OFN reader and `ofn.pest` |
| `src/io/omn/`  | Michel Dumontier | Also wrote `omn.pest` |
| `src/grammars/ofn.pest` | Martin Larralde | |
| `src/grammars/omn.pest` | Michel Dumontier | |
| `src/grammars/{bcp47,rfc3987,sparql}.pest` | shared | Foundational IRI/literal grammars used by both OFN and OMN |

## Other regular contributors

Filippo De Bortoli and Konrad Höffner have made repeated contributions across most of the codebase
(model, vocab, ontology, `io/rdf`, `io/owx`, `io/ofn`)

---

Generated from `git log --format="%an"` commit counts per path, not asserted knowledge of who's
actively available to review right now — please correct names/areas that are wrong or stale.
