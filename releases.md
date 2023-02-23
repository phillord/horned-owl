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
