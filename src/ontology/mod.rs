//! A variety of Ontology Implementations

//! # Overview

//! This module provides a variety of
//! [`MutableOntology`](../model/trait.MutableOntology.html)
//! implementations.

//! The simplest of these is
//! [`SetOntology`](set/struct.SetOntology.html). This provides the
//! simplest, and probably least overhead implementation of
//! [`MutableOntology`](../model/trait.MutableOntology.html) storing
//! all [`AnnotatedAxiom`](../model/enum.AnnotatedAxiom.html)
//! instances as using an in-memory `HashSet`. While this should be
//! fast to add to, any searches are likely to run in linear time.

//! The [`indexed`](indexed.html) package provides a mechanism to
//! allow faster and composable searching strategies, at a cost to
//! insertion performance, through the
//! [OntologyIndex](indexed/trait.OntologyIndex.html) trait.
//! Other than `SetOntology` all other implementations of
//! `MutableOntology` are backed by one or more of `OntologyIndex`
//! implementations. A [`SetIndex`](set/struct.SetIndex.html) is also
//! provided that mirrors the functionality of `SetOntology`.

//! The [`axiom_mapped`](axiom_mapped.html) package provides an
//! `OntologyIndex` that allows rapid retrieval of all
//! `AnnotatedAxiom` instances of a given kind.

//! Other indexes are less general purpose. The
//! [`declaration_mapped`](declaration_mapped.html) indexes only
//! declaration axioms, allowing rapid look up of the declarated type
//! of an IRI. As it ignores most axioms passed to it, it does not
//! provide iteration.

pub mod axiom_mapped;
pub mod declaration_mapped;
pub mod indexed;
pub mod logically_equal;
pub mod set;

// There isn't a very formal interface here, but a set of traits that
// can be implemented.
//  - The Ontology trait provides access to the ID
//  - MutableOntology mutator methods
//  - The IntoIterator for access to the entities either owned or reference
//  - FromIterator allowing collection and conversion

// Ontology implementations should provide what ever other accessors
// they choose, but should be biased toward providing those accessor
// functions that they can implement with good efficiency.
