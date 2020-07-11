//! # Horned-OWL
//!
//! Horned-OWL is a library for the reading, manipulation and
//! generation of
//! [OWL](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/)
//! ontologies. As well as a library, it offers a number of
//! command-line tools for performing the same.
//!
//! The focus of this library is on performance, compared to the [OWL
//! API](https://github.com/owlcs/owlapi). Currently, on IO tasks, it
//! is between 1 and 2 orders of magnitude faster.
//!
//! # Author
//!
//! This library is written by Phillip Lord <phillip.lord@newcastle.ac.uk>
//!
//! # Status
//!
//! At the moment, the library is in early stages. It is, however, a
//! complete implementation of the OWL2 specification.
//extern crate curie;
//extern crate enum_meta;
#[macro_use]
extern crate failure;
//#[macro_use]
extern crate log;
extern crate indexmap;
extern crate quick_xml;

pub mod command;
pub mod error;
pub mod index;
pub mod io;
pub mod model;
pub mod ontology;
pub mod vocab;
