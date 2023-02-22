Horned-Bin
==========

This is a set of command line tools built using the `horned` crate and provide core services for the manipulation of OWL ontologies.

### Building

The tool suite is available as a single binary `horned`, collecting all tools as subcommands:

```bash
cargo build --release --bin horned
```

or as a collection of standalone binaries, each can be built using:

```bash
cargo build --release --bin horned-[TOOLNAME]
```

using the tools listed below.

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

| `TOOLNAME` | Arguments | Summary | Source file |
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

