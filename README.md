Horned OWL
==========

![Crates.io](https://img.shields.io/crates/v/horned-owl?style=flat-square) ![docs.rs](https://img.shields.io/docsrs/horned-owl?style=flat-square)

Horned-OWL is a library for manipulating data written using the [Web Ontology Language (OWL)](https://en.wikipedia.org/wiki/Web_Ontology_Language).
While there are a number of libraries that manipulate this form
of data such as the [OWL API](https://github.com/owlcs/owlapi),
they can be quite slow. Horned-OWL is aimed at allowing ontologies
with millions of terms.

The library now implements all of OWL2, and we are working on further
parser functionality. We are testing it with real world tasks, such as
parsing the Gene Ontology, which is does in 2-3 seconds which is 20-40
times faster than the OWL API.

Library
-----

To use the latest version of the library in your Rust project, add the following line to your Cargo.toml file:

```toml
[dependencies]
...
horned-owl = "0.14.0"
```


Command Line Tools
------------------

A set of command line tools are available as in [Horned Bin](horned-bin/README.md).
