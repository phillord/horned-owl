//! Ontological Modeling Language (OML) support for horned-owl.
//!
//! OML is a systems-engineering language whose semantics are defined by
//! its mapping to OWL 2 DL and SWRL. This crate reads the textual syntax
//! (`.oml`) and produces the OWL that the reference `oml2owl` tool
//! produces from the same file.
//!
//! This is a separate crate rather than a module of `horned-owl`, unlike
//! the other readers and writers.

pub mod catalog;
pub mod closure;
pub mod parser;
pub mod reader;
pub mod validate;
