use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum Format {
    RdfXml,
    OwlXml,
    Ofn,
    Omn,
    Unknown,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Outcome {
    Ok,
    ReadFail,
    WriteFail,
    RereadFail,
    Panic,
    Skipped,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum Category {
    InferredDeclaration,
    NaryReshape,
    AnnotationNormalization,
    BlankNodeRelabel,
    Unknown,
}

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Side {
    Source,
    RoundTrip,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct DiffItem {
    pub side: Side,
    pub component_kind: String,
    pub category: Category,
    pub debug: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Default)]
pub struct IncompleteSummary {
    pub simple: usize,
    pub bnode: usize,
    pub class_expression: usize,
    pub annotation: usize,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct SourceReadReport {
    pub ontology: String,
    pub source_format: Format,
    pub outcome: Outcome,
    pub is_complete: bool,
    pub incomplete: Option<IncompleteSummary>,
    pub error: Option<String>,
    pub read_us: Option<u64>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct CaseResult {
    pub ontology: String,
    pub source_format: Format,
    pub target_format: Format,
    pub outcome: Outcome,
    pub error: Option<String>,
    pub exact: bool,
    pub diffs: Vec<DiffItem>,
    pub category_counts: BTreeMap<Category, usize>,
    pub write_us: Option<u64>,
    pub reread_us: Option<u64>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct RunHeader {
    pub horned_owl_rev: String,
    pub corpus: String,
    pub started: String,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(tag = "record", rename_all = "snake_case")]
pub enum Record {
    Header(RunHeader),
    Source(SourceReadReport),
    Case(CaseResult),
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn record_json_roundtrips() {
        let rec = Record::Case(CaseResult {
            ontology: "X".into(),
            source_format: Format::RdfXml,
            target_format: Format::Omn,
            outcome: Outcome::Ok,
            error: None,
            exact: false,
            diffs: vec![DiffItem {
                side: Side::RoundTrip,
                component_kind: "DeclareClass".into(),
                category: Category::InferredDeclaration,
                debug: "…".into(),
            }],
            category_counts: [(Category::InferredDeclaration, 1)].into_iter().collect(),
            write_us: Some(10),
            reread_us: Some(20),
        });
        let js = serde_json::to_string(&rec).unwrap();
        let back: Record = serde_json::from_str(&js).unwrap();
        assert_eq!(rec, back);
    }
}
