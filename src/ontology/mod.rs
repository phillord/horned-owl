//
// Different ontology implementations

pub mod simple;



// There isn't a very formal interface here, but a set of traits that
// can be implemented.
//  - The Ontology trait provides access to the ID
//  - MutableOntology mutator methods
//  - The IntoIterator for access to the entities either owned or reference
