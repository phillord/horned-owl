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

Command Line tools
---------
In addition to the Rust library, the project includes the implementations of several command line utilities that exemplify the usage of APIs offered by the `horned-owl` crate and provide ready-to-use tools.

### Building

The tool suite is available as a single binary `horned`:

```bash
cargo build --release --bin horned
```

or as a collection of standalone binaries, each can be built using:

```bash
cargo build --release --bin horned-[TOOLNAME]
```

### Running

To use one of the utilities described below, run

```bash
horned [TOOLNAME] <ARGUMENT_LIST>
```

or

```bash
horned-[TOOLNAME] <ARGUMENT_LIST>
```

using the requested arguments.

### Description

| Tool | Arguments | Summary | Source file |
| ---  | ---       | ---     | ---         |
| *big*  | `n`: unsigned integer | Generates an OWL file containing `n` class declarations.     | [source](src/bin/horned_big.rs) |
| *compare*  | `ont-1`, `ont-2`: paths     | Compares the statistics of ontologies specified in `ont-1` and `ont-2`. | [source](src/bin/horned_compare.rs) |
| *dump*  | `ont`: path     | Parses `ont` and returns the content of the data structures created by the parser. | [source](src/bin/horned_dump.rs) |
| *materialize*  | `ont`: path     | Parses `ont`, downloading and resolving all of the ontologies imported by `ont`. | [source](src/bin/horned_materialize.rs) |
| *parse*  | `ont`: path     | Parses `ont` and exits. | [source](src/bin/horned_parse.rs) |
| *round*  | `ont`: path     | Parses `ont` and renders the obtained ontology. | [source](src/bin/horned_round.rs) |
| *summary*  | `ont`: path     | Parses `ont` and returns statistics related to the obtained ontology. | [source](src/bin/horned_summary.rs) |
| *triples*  | `ont`: path     | Parses `ont` as an ontology written using the OWL/RDF format and returns the obtained triples. | [source](src/bin/horned_triple.rs) |
| *unparsed*  | `ont`: path     | Parses `ont` as an ontology written using the OWL/RDF format and shows what has not been successfully parsed. | [source](src/bin/horned_unparsed.rs) |

