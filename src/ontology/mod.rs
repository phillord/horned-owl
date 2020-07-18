//
// Different ontology implementations
pub mod axiom_mapped;
pub mod indexed;
pub mod simple;

// There isn't a very formal interface here, but a set of traits that
// can be implemented.
//  - The Ontology trait provides access to the ID
//  - MutableOntology mutator methods
//  - The IntoIterator for access to the entities either owned or reference
//  - FromIterator allowing collection and conversion

// Ontology implementations should provide what ever other accessors
// they choose, but should be biased toward providing those accessor
// functions that they can implement with good efficiency.
