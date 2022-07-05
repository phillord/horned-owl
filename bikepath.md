Version 0.13.0
==============

Lots of fixes for the RDF parser based on lots of known files.


Version next
============

Support fully multi file ontology parsing


Version next
============

Parser Headbutting.

The RDF parser near miss, add strict/lax mode

Version next
============

horned-robot. Create a drop in replacement for robot with the same
command line. Missing functionality will simply call robot directly.




Version next
==============

Consider tighter integration with other crates, especially for
IRIs. Bump quick-xml version so it's compatible with what ever we are
using for RDF parsing.


Other Changes that I'd like to makes
====================================


Removing a layer of complexity in Axiom
---------------------------------------

`Axiom` is currently complicated because it has an extra layer on
indirection. Each variant of the enum is takes a different struct,
rather than using struct like variants in the Enum. This was done
to support typing in methods and means, for example, that the
`axiom_mapped` index has a sensible return type. But it makes querying
the ontology structures harder.

At the moment, there is a choice, but if Rust support enum variants as
types, I could have my cake and eat it.
