use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum Format {
    RdfXml,
    OwlXml,
    Ofn,
    Omn,
    /// Turtle / N-Triples (read only — via the oxrdfio-backed RDF reader with
    /// `RdfFormat::Turtle`; there is no Turtle *writer*, so it never appears as
    /// a round-trip target).
    Turtle,
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
    AnnotationLoss,
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

/// The four OWL 2 profiles `horned-profile`/ROBOT's `validate-profile` both
/// check. Named to match ROBOT's `-p` flag values (`DL`/`EL`/`QL`/`RL`),
/// which the `profile` module's `robot_verdicts` passes straight through.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "snake_case")]
pub enum Profile {
    Dl,
    El,
    Ql,
    Rl,
}

/// One checker's (horned-profile's, or ROBOT/the OWL API's) verdict for one
/// ontology against one profile.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
pub struct ProfileVerdict {
    pub conformant: bool,
    pub violation_count: usize,
}

/// Profile-conformance results for one ontology: horned-profile's own
/// verdicts (always present), ROBOT/the OWL API's ground-truth verdicts
/// (present only when the cross-check was run -- see `profile` module doc),
/// and, when both are present, whether each profile's `conformant` verdict
/// agrees between the two.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
pub struct ProfileCheckResult {
    pub ontology: String,
    pub horned: BTreeMap<Profile, ProfileVerdict>,
    pub robot: Option<BTreeMap<Profile, ProfileVerdict>>,
    pub agreement: BTreeMap<Profile, bool>,
}

#[derive(Serialize, Deserialize, Clone, Debug, PartialEq)]
#[serde(tag = "record", rename_all = "snake_case")]
pub enum Record {
    Header(RunHeader),
    Source(SourceReadReport),
    Case(CaseResult),
    Profile(ProfileCheckResult),
}

/// How much profile-conformance checking `run_bytes` should do for a given
/// ontology -- see `profile` module doc for why the ROBOT leg is opt-in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ProfileCheckMode {
    /// No profile checking at all (the default, matching this tool's
    /// behaviour before profile checking existed).
    Off,
    /// `horned-profile` only -- cheap, pure Rust, safe to run over a full
    /// corpus by default.
    Horned,
    /// `horned-profile` plus ROBOT/OWL API ground truth -- expensive, see
    /// `profile` module doc.
    HornedAndRobot,
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

    #[test]
    fn profile_record_json_roundtrips() {
        let rec = Record::Profile(ProfileCheckResult {
            ontology: "X".into(),
            horned: [(
                Profile::El,
                ProfileVerdict {
                    conformant: false,
                    violation_count: 1,
                },
            )]
            .into_iter()
            .collect(),
            robot: Some(
                [(
                    Profile::El,
                    ProfileVerdict {
                        conformant: false,
                        violation_count: 1,
                    },
                )]
                .into_iter()
                .collect(),
            ),
            agreement: [(Profile::El, true)].into_iter().collect(),
        });
        let js = serde_json::to_string(&rec).unwrap();
        let back: Record = serde_json::from_str(&js).unwrap();
        assert_eq!(rec, back);
    }
}
