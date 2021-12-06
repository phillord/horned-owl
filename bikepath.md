Version 0.11.0
==============

Support fully multi file ontology parsing


Version next
============

Parser Headbutting.

The RDF parser near miss, add strict/lax mode


Version next
==============

Clean up command line code. Add a multiplexer `horned-owl dump` ->
`horned-dump`

Version next
==============

Consider tighter integration with other crates, especially for
IRIs. Bump quick-xml version so it's compatible with what ever we are
using for RDF parsing.


Other Changes that I'd like to makes
====================================


Think about Errors
------------------

These are not well thought out at the moment.


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
