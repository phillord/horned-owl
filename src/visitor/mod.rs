//! Visitor support for OWL Ontologies.
//!
//! There are two independent implementations, one mutable and one
//! immutable. Both use a single `Visit` trait, with an separate
//! "Walk" struct; this cleanly splits the walking logic, means that
//! the `Visit` default implementation is empty, so walk logic does
//! nothing need replacing in implementations.
//!
//! # Examples
//! ```
//! # use horned_owl::model::*;
//! # use horned_owl::visitor::immutable::Visit;
//! # use horned_owl::visitor::immutable::Walk;
//! # use horned_owl::ontology::set::SetOntology;
//! #
//! #[derive(Default)]
//! struct HelloClass(Option<String>);
//!
//! impl<A: ForIRI> Visit<A> for HelloClass{
//!     fn visit_class(&mut self, c: &Class<A>) {
//!          self.0.replace(format!("Hello {}", c.0 ));
//!     }
//! }
//!
//! let b = Build::new_rc();
//! let mut o = SetOntology::default();
//!
//! o.declare(b.class("World"));
//!
//! let helloclass = HelloClass::default();
//! let mut walk = Walk::new(helloclass);
//! walk.set_ontology(&o);
//!
//! let helloclass = walk.into_visit();
//! assert_eq!(helloclass.0, Some("Hello World".to_string()));
//! ````
pub mod immutable;
pub mod mutable;
