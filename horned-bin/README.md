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
| *convert*  | `ont`: path, `--to`: format     | Converts `ont` to another OWL serialisation format. | [source](src/bin/horned_convert.rs) |
| *dump*  | `ont`: path     | Parses `ont` and returns the content of the data structures created by the parser. | [source](src/bin/horned_dump.rs) |
| *materialize*  | `ont`: path     | Parses `ont`, downloading and resolving all of the ontologies imported by `ont`. | [source](src/bin/horned_materialize.rs) |
| *parse*  | `ont`: path     | Parses `ont` and exits. | [source](src/bin/horned_parse.rs) |
| *profile*  | `ont`: path     | Reports which OWL 2 profile(s) (EL/QL/RL/DL) `ont` conforms to. | [source](src/bin/horned_profile.rs) |
| *round*  | `ont`: path     | Parses `ont` and renders the obtained ontology. | [source](src/bin/horned_round.rs) |
| *summary*  | `ont`: path     | Parses `ont` and returns statistics related to the obtained ontology. | [source](src/bin/horned_summary.rs) |
| *triples*  | `ont`: path     | Parses `ont` as an ontology written using the OWL/RDF format and returns the obtained triples. | [source](src/bin/horned_triples.rs) |
| *unparsed*  | `ont`: path     | Parses `ont` as an ontology written using the OWL/RDF format and shows what has not been successfully parsed. | [source](src/bin/horned_unparsed.rs) |
| *validate*  | `ont`: path     | Parses `ont` and fails if any part of it could not be parsed. | [source](src/bin/horned_validate.rs) |

### Exit codes

Every tool exits `0` on success and `1` on failure (there are no other exit
codes -- this is the default behaviour Rust gives a `fn main() ->
Result<(), E>` when it returns `Err`, printing the error to stderr).
Argument errors, I/O errors, and syntax errors during parsing always cause a
non-zero exit, in every tool.

What differs between tools is whether a **semantically incomplete parse**
also counts as failure. Some OWL/RDF/Turtle input can parse without a syntax
error yet still leave part of the model unrepresented -- e.g. leftover
triples an `AnnotatedComponent` couldn't be built from (see
[`IncompleteParse`](../src/io/rdf/reader.rs)). Whether that counts as
success or failure depends on the tool:

- **`validate`** treats an incomplete parse as failure: it prints the
  unparsed remainder and exits `1`. This is the tool to use in a script or
  CI check that needs to know an ontology parsed *completely*, not just
  that it parsed at all.
- **`parse`**, **`unparsed`**, **`dump`**, **`round`**, **`convert`**,
  **`summary`**, **`compare`**, **`materialize`**, and **`profile`** all
  exit `0` as long as parsing didn't throw a syntax/IO error, *even if*
  the resulting model is incomplete -- they discard the `IncompleteParse`
  the parser returns rather than checking it. `unparsed` (always) and
  `dump` (for RDF/XML/Turtle input, unless run with `--incomplete`/`-l` to
  suppress the full ontology dump) still *print* the unparsed remainder to
  stdout -- they just don't fail the process over it. This is deliberate,
  not an oversight: these tools' job is to show you what parsed, not to
  gate on completeness (see #135/#138, where `validate` was added
  specifically to fill that gap rather than changing `parse`'s existing
  contract, which scripts may already depend on).
- **`big`** and **`triples`** don't parse a pre-existing ontology into a
  `MutableOntology` at all (`big` only generates one; `triples` reads raw
  RDF quads directly, bypassing the axiom-building parser entirely), so
  the concept of an incomplete parse doesn't apply to them.

If you need a hard failure on incomplete parses, use `validate` rather than
relying on `parse`'s exit code -- or inspect `unparsed`'s/`dump`'s printed
output yourself if you need the incomplete remainder itself, not just a
pass/fail signal.

