Version 2.1.0
=============

Features:
- New `--input-format` CLI option, with automatic content-sniffing as
  a fallback when a file's extension doesn't indicate its format.
- The `horned` CLI gained `--lax`, `--remote-body-limit`, and
  `--local-only` (guarantees no network access) as global options.
- `Ontology` now guarantees an `Iterator` which was previously by
  convention.
- pretty_rdf is now bundled directly as the `horned-pretty-rdf`
  workspace subcrate (previously a separate crate).

Enhancements:
- `horned-bin` binaries now report the horned-owl version they were
  compiled against in `--version` output.
- `ureq` updated to 3.3.0, with a configurable remote body size limit.
- OWL/XML reader errors now report byte positions.

Bugs:
- Several RDF/XML round-trip panics fixed (duplicate annotations,
  single-member DifferentIndividuals, malformed input now errors
  instead of panicking).
- `rdfs:Class` is now recognised in the RDFS vocabulary, fixing
  mis-parsing of RDFS-only ontologies (e.g. GEXO) as spurious class
  assertions.
- Fixed doubled-hash IRIs when an empty prefix ends with '#' in the
  OWL/XML reader.
- Unqualified `owl:minCardinality`/`owl:maxCardinality` now dispatch
  correctly on the property's declared kind.
- Stray text in OWL/XML input is now rejected by default (opt out with
  the new `lax` flag).

Contributors:
- Phillip Lord



Version 2.0.0
=============

Features:
- Manchester Syntax is now supported

Enhancements:
- The ForIRI interface has been updated to avoid an allocation which
  results in 5-10% performance gains.
- Other performance enchancements, including several in pretty_rdf.

Contributors:
- Michel Dumontier
- Phillip Lord
- Jim Balhoff



Version 1.4.0
=============

Last release was not fully complete

Version 1.3.0
=============

Features:
 - The RDF parser has been substantially re-written, and the
   mechanisms that it uses for distinguish properties changed, meaning
   it now has a more meaningful "lax" mode which will parse more ontologies.
 - The Rio dependency has been replaced by oxrdfio and all RDF output
   formats that it provides are now supported.
 - XML and RDF writer interface has been updated to return the Write instance.

Bugs:
 - The test suite is now more extensive and will reparse output from
   Horned-OWL with the OWL API to test for errors.
 - Lots of `todo!` statements have been removed.

Version 1.2.0
=============

Features:
 - The resolution mechanism for imported ontologies now supports the OWL2 versioning mechanism.
 - Rust edition has been upgraded to 2024

Bugs:
 - Punning now works in all syntaxes
 - Some error handling improvements

Version 1.1.0
=============

Features

  - Adds horned-validate
  - Many small interface extensions
  - IRI::is allows comparison between different generic IRIs

Bugs
  - Non deterministic parsing in RDF
  - Make more indexes usable cross thread

Version 1.0.0
=============

This release brings Horned-OWL to a first stable release.

Notable additions include:
  - Support for SWRL rules
  - Addition of Functional Syntax parser/renderer

There is now documentation for all modules. Code has been regularized.

Version 0.15.0
==============

This release adds nothing but breaks all the interfaces!

Ontologies are not an iterator of Components, rather than axioms. This
regularizes the way that OntologyID and DocIRIs are treated -- this
has the subsidary advantage that DocIRIs should work better now. The
main purpose, though, it to pave the way for support of SWRL rules.

Version 0.14.0
==============

All commands have now been moved to their own crate. This will all
library uses to have access to a slighly smaller binary with fewer
dependencies.

Version 0.13.0
==============

This release includes a lot of clean ups, including many version
bumps.

RDF parsing now has better location reporting for errors.

Version 0.12.0
==============

The core data model has been updated to make `IRI` generic. This
resolves a long standing problem that Horned-OWL was a single threaded
library. This has also meant that `SetOntology` has now been rewritten
to use `SetIndex`.

A general purpose visitor library has been added. At some point, we
should use them for rendering, but for now, an IRIMappedIndex has been
written.

A closure parser has been added for RDF, as it is often necessary to
parse the whole import closure to make a complete RDF parse.

Updated error handling to use a single unified hierarchy.

Many type aliases are now new types meaning that defaults work better.

Parsing of GCIs has now been fixed for RDF/XML.

The methods for retrieving axioms for axiom kinds and for IRIs in the
`AxiomMappedIndex` and the `IRIMappedIndex` have been renamed to disambiguate
them - as `axiom_for_kind` and `axiom_for_iri` respectively.


Version 0.11.0
==============

This release includes a new command `horned` which multiplexes all the
other commands. Thanks for Filippo De Bortoli for this addition.

There has been one model change with the introduction of
AnnotationSubject.

There has been a significant refactoring and updating of dependencies,
particularly the removal of the `failure` crate dependency.

There have been a number of performance updates thanks to Martin Larralde.

Version 0.10.0
==============

The major change is to move the RDF parser to RIO. RDF parsing now has
preliminary support for ontologies which require knowledge from
outside the current one to parse fully.

An RDF writer has been added.

Version 0.9.0
=============

A variety of advances and fixes, including an IRI resolver and
a horned-materialize command.

Version 0.8.0
=============

The RDF parser has been made several orders of magnitude faster, by
removing quadratic updating of axioms.

Version 0.7.0
=============

`Ontology` and `MutableOntology` are now traits, allowing different
implementations. An `OntologyIndex` trait has also been introduced,
allowing composable indexing of ontologies.

More specifically:

 - a new index module has been introduced, with the `OntologyIndex`
   trait, as well as `MutableOntology` implementations supporting one,
   two (and potentially more) indexes.
 - the old concrete `Ontology` struct is now an `AxiomMappedOntology`,
   with an underlying `AxiomMappedIndex` which provides all the
   functionality.
 - A new `SetOntology` has been introduced which is the simplest (and
   hopefully fastest) implementation, simply backed by a `HashSet`. A
   `SetIndex` is also available which is composable with other
   indexes.

Version 0.6.0
=============

## Functionally complete OWL/RDF parser.

There is now a functionally complete OWL/RDF parser.

Performance is current unimpressive!
