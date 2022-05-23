Version Next
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
