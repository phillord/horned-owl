//! Reading a results file for a report without holding it in memory.
//!
//! A sweep writes every diff of every case, and a large corpus makes that
//! gigabytes: the OBO-lossy cases alone have millions of diffs. A report
//! needs only how many diffs were lost and gained, and the first two
//! `Unknown` or `AnnotationLoss` diffs of each case, which it prints. So
//! `load_records` reads the file one value at a time and cuts each case down
//! to that, and memory is proportional to the number of cases.

use crate::model::*;
use serde::Deserialize;
use serde::de::{DeserializeSeed, Error as _, IgnoredAny, MapAccess, SeqAccess, Visitor};
use std::collections::BTreeMap;
use std::fmt;
use std::io::{BufRead, BufReader};
use std::path::Path;

/// How many `Unknown` or `AnnotationLoss` diffs of a case a report prints.
const SAMPLE: usize = 2;

/// Read every record of the JSONL file at `path`; see the module comment.
pub fn load_records(path: &Path) -> anyhow::Result<Vec<Record>> {
    read_records(BufReader::new(std::fs::File::open(path)?))
}

/// As `load_records`, from any reader.
pub fn read_records(reader: impl BufRead) -> anyhow::Result<Vec<Record>> {
    serde_json::Deserializer::from_reader(reader)
        .into_iter::<Loaded>()
        .map(|r| r.map(|l| l.0).map_err(Into::into))
        .collect()
}

struct Loaded(Record);

impl<'de> Deserialize<'de> for Loaded {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        d.deserialize_map(RecordVisitor)
    }
}

struct RecordVisitor;

impl<'de> Visitor<'de> for RecordVisitor {
    type Value = Loaded;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("a record")
    }

    fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<Loaded, M::Error> {
        // The tag is written first.
        let tag = match map.next_key::<String>()? {
            Some(k) if k == "record" => map.next_value::<String>()?,
            _ => return Err(M::Error::custom("a record starts with its `record` tag")),
        };
        if tag != "case" {
            // Small records: let `Record` read them.
            let mut object = serde_json::Map::new();
            object.insert("record".into(), tag.into());
            while let Some((k, v)) = map.next_entry::<String, serde_json::Value>()? {
                object.insert(k, v);
            }
            return serde_json::from_value(object.into())
                .map(Loaded)
                .map_err(M::Error::custom);
        }

        let mut ontology = None;
        let mut source_format = None;
        let mut target_format = None;
        let mut outcome = None;
        let mut error = None;
        let mut exact = None;
        let mut diffs = None;
        let mut category_counts = None;
        let mut write_us = None;
        let mut reread_us = None;
        while let Some(k) = map.next_key::<String>()? {
            match k.as_str() {
                "ontology" => ontology = Some(map.next_value()?),
                "source_format" => source_format = Some(map.next_value()?),
                "target_format" => target_format = Some(map.next_value()?),
                "outcome" => outcome = Some(map.next_value()?),
                "error" => error = map.next_value()?,
                "exact" => exact = Some(map.next_value()?),
                "diffs" => diffs = Some(map.next_value_seed(SlimDiffs)?),
                "category_counts" => category_counts = Some(map.next_value()?),
                "write_us" => write_us = map.next_value()?,
                "reread_us" => reread_us = map.next_value()?,
                _ => {
                    map.next_value::<IgnoredAny>()?;
                }
            }
        }
        let missing = |f| M::Error::missing_field(f);
        let slim = diffs.unwrap_or_default();
        Ok(Loaded(Record::Case(CaseResult {
            ontology: ontology.ok_or_else(|| missing("ontology"))?,
            source_format: source_format.ok_or_else(|| missing("source_format"))?,
            target_format: target_format.ok_or_else(|| missing("target_format"))?,
            outcome: outcome.ok_or_else(|| missing("outcome"))?,
            error,
            exact: exact.ok_or_else(|| missing("exact"))?,
            diffs: slim.sample,
            side_counts: Some((slim.lost, slim.gained)),
            category_counts: category_counts.unwrap_or_else(BTreeMap::new),
            write_us,
            reread_us,
        })))
    }
}

#[derive(Default)]
struct Slim {
    lost: usize,
    gained: usize,
    sample: Vec<DiffItem>,
}

struct SlimDiffs;

impl<'de> DeserializeSeed<'de> for SlimDiffs {
    type Value = Slim;

    fn deserialize<D: serde::Deserializer<'de>>(self, d: D) -> Result<Slim, D::Error> {
        d.deserialize_seq(self)
    }
}

impl<'de> Visitor<'de> for SlimDiffs {
    type Value = Slim;

    fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("a list of diffs")
    }

    fn visit_seq<S: SeqAccess<'de>>(self, mut seq: S) -> Result<Slim, S::Error> {
        let mut slim = Slim::default();
        while let Some(d) = seq.next_element::<DiffItem>()? {
            match d.side {
                Side::Source => slim.lost += 1,
                Side::RoundTrip => slim.gained += 1,
            }
            if slim.sample.len() < SAMPLE
                && matches!(d.category, Category::Unknown | Category::AnnotationLoss)
            {
                slim.sample.push(d);
            }
        }
        Ok(slim)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn diff(side: Side, category: Category, debug: &str) -> DiffItem {
        DiffItem {
            side,
            component_kind: "SubClassOf".into(),
            category,
            debug: debug.into(),
        }
    }

    fn case(diffs: Vec<DiffItem>) -> CaseResult {
        let mut category_counts = BTreeMap::new();
        category_counts.insert(Category::Unknown, 3);
        CaseResult {
            ontology: "o".into(),
            source_format: Format::RdfXml,
            target_format: Format::Ofn,
            outcome: Outcome::Ok,
            error: None,
            exact: false,
            diffs,
            side_counts: None,
            category_counts,
            write_us: Some(1),
            reread_us: None,
        }
    }

    /// Cases are cut down to counts and the first two printable diffs;
    /// everything else, and every other record, is as written.
    #[test]
    fn cases_are_cut_down() {
        let header = Record::Header(RunHeader {
            horned_owl_rev: "r".into(),
            corpus: "c".into(),
            started: "1".into(),
        });
        let full = case(vec![
            diff(Side::Source, Category::NaryReshape, "a"),
            diff(Side::Source, Category::Unknown, "b"),
            diff(Side::RoundTrip, Category::AnnotationLoss, "c"),
            diff(Side::RoundTrip, Category::Unknown, "d"),
        ]);
        let mut text = String::new();
        for r in [&header, &Record::Case(full.clone())] {
            text += &serde_json::to_string(r).unwrap();
            text += "\n";
        }
        let got = read_records(text.as_bytes()).unwrap();

        let mut cut = full;
        cut.side_counts = Some((2, 2));
        cut.diffs = vec![
            diff(Side::Source, Category::Unknown, "b"),
            diff(Side::RoundTrip, Category::AnnotationLoss, "c"),
        ];
        assert_eq!(got, vec![header, Record::Case(cut)]);
    }

    #[test]
    fn a_case_with_no_diffs() {
        let r = Record::Case(case(Vec::new()));
        let text = serde_json::to_string(&r).unwrap();
        let got = read_records(text.as_bytes()).unwrap();
        let Record::Case(c) = &got[0] else { panic!() };
        assert_eq!(c.side_counts, Some((0, 0)));
        assert!(c.diffs.is_empty());
    }
}
