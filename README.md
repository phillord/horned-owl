Horned OWL
==========

[![Crates.io](https://img.shields.io/crates/v/horned-owl?style=flat-square)](https://crates.io/crates/horned-owl) [![docs.rs](https://img.shields.io/docsrs/horned-owl?style=flat-square)](https://docs.rs/horned-owl/latest/horned_owl/)

*Horned-OWL* is a library for processing and manipulating documents written using the [Web Ontology Language (OWL)](https://en.wikipedia.org/wiki/Web_Ontology_Language).

It extends the existing ecosystem of Rust crates for the Semantic Web and provides tools to interact with OWL ontologies within Rust applications.
It leverages the characteristics of Rust to be performant and competitive against other libraries for ontologies manipulation, such as the [OWL API](https://github.com/owlcs/owlapi). Horned-OWL is aimed at allowing ontologies
with millions of terms.

+ Support for the OWL 2 language as specified in the [W3C Recommendation](https://www.w3.org/TR/owl-overview/)
+ Support for the Semantic Web Rule Language as specified in the [W3C User Submission](https://www.w3.org/submissions/SWRL/)
+ Support for serialization/deserialization of the following OWL syntaxes:
  + [x] RDF/XML
  + [x] OWL/XML
  + [x] Functional Syntax
  + [x] Manchester Syntax
+ A [visitor](https://en.wikipedia.org/wiki/Visitor_pattern) trait to navigate and manipulate ontologies
+ Traits and implementations for several types of ontologies

## Features

+ `encoding`: enables the `encoding` feature for the [quick-xml](https://crates.io/crates/quick-xml) dependency, expanding support to non-UTF-8 encoded documents while parsing OWL/XML.
+ `remote`: enables the resolution of non-local imports while parsing RDF/XML.
## Performance

We test the performance of our crate in validating large ontologies (e.g. the Gene Ontology) against competing implementations based on the OWL API. Preliminary results are encouraging, showing a potential speedup of 20x-40x.

## Library

To use the latest version of the library in your Rust project, add the following line to your Cargo.toml file:

```toml
[dependencies]
...
horned-owl = "2.0.0"
```


## Command Line Tools


A set of command line tools are available as in [Horned Bin](horned-bin/README.md).

## Projects using this crate

+ [py-horned-owl](https://github.com/ontology-tools/py-horned-owl): Python bindings for *Horned-OWL* 
+ [whelk-rs](https://github.com/INCATools/whelk-rs): an implementation of [Whelk](https://github.com/balhoff/whelk) in Rust
