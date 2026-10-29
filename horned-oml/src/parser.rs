//! The pest parser for OML. The grammar is in `oml.pest`.

use pest_derive::Parser;

#[derive(Parser)]
#[grammar = "oml.pest"]
pub struct OmlParser;
