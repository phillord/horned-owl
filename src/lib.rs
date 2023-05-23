//! # Horned-OWL
//!
//! Horned-OWL is a library for the reading, manipulation and
//! generation of
//! [OWL](https://www.w3.org/TR/2012/REC-owl2-primer-20121211/)
//! ontologies.

//! Unlike a simple classification taxonomy, OWL is highly expressive and maps 
//! to a formal semantics which makes the ontology open to computational
//! reasoning.
//!
//! The aim of the library is to provide a representation of OWL that
//! can be used to manipulate OWL ontologies.
//!
//! The focus of this library is on performance, compared to the [OWL
//! API](https://github.com/owlcs/owlapi), thereby allowing large
//! scale, bulk manipulation of ontologies that currently requires
//! specialized machinery.
//!
//! # Author
//!
//! This library is written by Phillip Lord <phillip.lord@newcastle.ac.uk>
//!
//! # Status
//!
//! The core data model of the library now provides a complete
//! implementation of the OWL2 DL specification. It appears to be
//! highly performant, being between 1 and 2 orders of magnitude
//! faster than the OWL API for some tasks.
//extern crate curie;
//extern crate enum_meta;

//#[macro_use]
extern crate indexmap;
extern crate log;
extern crate quick_xml;

pub mod adaptor;
pub mod collection;
pub mod error;
pub mod io;
pub mod model;
pub mod ontology;
pub mod resolve;
pub mod visitor;
pub mod vocab;
