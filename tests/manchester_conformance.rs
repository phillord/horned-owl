//! OWL 2 Manchester Syntax §2.5 conformance harness for `io::omn`.
//! Submodules live under `tests/manchester/`. Run the report generator with:
//!   cargo test --test manchester_conformance -- --ignored generate_compliance_report
#[path = "manchester/mod.rs"]
mod manchester;
