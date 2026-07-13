//! A3 — semantic axiom-set equality vs OWL-API, with a documented-normalization
//! canonicalizer.
//!
//! The canonicalizer removes two categories of OWL-API round-trip noise so that
//! diffs reflect genuine omn-reader gaps rather than OWL-API transform artefacts:
//!
//! 1. **Declaration conflation** — the OWL-API may add or drop `Declare*` axioms
//!    during conversion; we drop all of them from both sides.
//! 2. **n-ary ↔ pairwise normalization** — the OWL-API may expand an n-ary
//!    `EquivalentClasses(A,B,C)` to three pairwise binary axioms, or vice-versa.
//!    `nary_member_pairs` reduces every n-ary axiom to an unordered set of member
//!    pairs so both representations compare identical.
//!
//! # Interpreting diffs
//!
//! A "missing" entry (in ofn-truth, absent from omn-candidate) can be:
//! - **(a) Reshuffle noise** — balanced missing≈extra on equiv/disjoint/same axioms;
//!   cancelled by using `nary_member_pairs` as the secondary signal.
//! - **(b) OWL-API Manchester lossiness** — ROBOT genuinely cannot serialise some
//!   axioms in Manchester; they are absent from the .omn text.  One-sided missing,
//!   not the reader's fault.
//! - **(c) Genuine omn-reader gap** — axiom present in .omn text but dropped or
//!   mangled by the horned-owl Manchester reader.  Also one-sided missing.
//!
//! The flat `canonical` diff cannot separate (b) from (c) without inspecting the
//! .omn source text.  Report both; note whether the diffs are balanced (→ noise)
//! or one-sided (→ oracle lossiness or reader gap).

use super::*;
use horned_owl::model::{
    Component, DifferentIndividuals, DisjointClasses, DisjointDataProperties,
    DisjointObjectProperties, EquivalentClasses, EquivalentDataProperties,
    EquivalentObjectProperties, SameIndividual,
};
use std::collections::BTreeSet;
use std::rc::Rc;

// ---------------------------------------------------------------------------
// Declaration-conflation canonicalizer
// ---------------------------------------------------------------------------

/// Returns `true` if `c` is a declaration axiom (`Declare*`).
///
/// The OWL-API freely adds / drops declarations during Manchester round-trips,
/// so we strip them from both sides before comparing.
fn is_declaration(c: &Component<Rc<str>>) -> bool {
    matches!(
        c,
        Component::DeclareClass(_)
            | Component::DeclareObjectProperty(_)
            | Component::DeclareDataProperty(_)
            | Component::DeclareAnnotationProperty(_)
            | Component::DeclareNamedIndividual(_)
            | Component::DeclareDatatype(_)
    )
}

/// Returns `true` if `c` is metadata (OntologyID / DocIRI / OntologyAnnotation /
/// Import).  These are rendered differently by OFN vs OMN writers and produce a
/// small constant floor of missing/extra that is not axiom-reader signal.
fn is_meta(c: &Component<Rc<str>>) -> bool {
    matches!(
        c,
        Component::OntologyID(_)
            | Component::DocIRI(_)
            | Component::OntologyAnnotation(_)
            | Component::Import(_)
    )
}

/// Canonicalize an ontology to a sorted `Vec<String>` invariant under
/// documented OWL-API normalizations: declarations and ontology-metadata
/// are stripped; remaining components are rendered with `{:?}` and sorted.
pub fn canonical(ont: &O) -> Vec<String> {
    let mut v: Vec<String> = ont
        .iter()
        .filter(|ac| !is_declaration(&ac.component) && !is_meta(&ac.component))
        .map(|ac| format!("{:?}", ac.component))
        .collect();
    v.sort();
    v
}

// ---------------------------------------------------------------------------
// n-ary ↔ pairwise normalizer
// ---------------------------------------------------------------------------

/// Helper: given a slice of debug-rendered member strings (already sorted),
/// insert every unordered pair into `out` as `"a|b"` (with `a ≤ b` by sort).
fn emit_pairs(members: &[String], out: &mut BTreeSet<String>) {
    let n = members.len();
    for i in 0..n {
        for j in (i + 1)..n {
            out.insert(format!("{}|{}", members[i], members[j]));
        }
    }
}

/// Reduce n-ary equivalence / disjointness / same / different axioms to an
/// unordered set of member-pair strings, so that `EquivalentClasses(A,B,C)`
/// and the three pairwise binary equivalences produce the same set.
///
/// Axioms handled:
/// - `EquivalentClasses`, `DisjointClasses`
/// - `EquivalentObjectProperties`, `DisjointObjectProperties`
/// - `EquivalentDataProperties`, `DisjointDataProperties`
/// - `SameIndividual`, `DifferentIndividuals`
///
/// Each pair is rendered as `"<debug_of_lhs>|<debug_of_rhs>"` with members
/// sorted so the pair is order-independent.
pub fn nary_member_pairs(ont: &O) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for ac in ont.iter() {
        let members_opt: Option<Vec<String>> = match &ac.component {
            Component::EquivalentClasses(EquivalentClasses(v)) => {
                Some(v.iter().map(|m| format!("{m:?}")).collect())
            }
            Component::DisjointClasses(DisjointClasses(v)) => {
                Some(v.iter().map(|m| format!("{m:?}")).collect())
            }
            Component::EquivalentObjectProperties(EquivalentObjectProperties(v)) => {
                Some(v.iter().map(|m| format!("{m:?}")).collect())
            }
            Component::DisjointObjectProperties(DisjointObjectProperties(v)) => {
                Some(v.iter().map(|m| format!("{m:?}")).collect())
            }
            Component::EquivalentDataProperties(EquivalentDataProperties(v)) => {
                Some(v.iter().map(|m| format!("{m:?}")).collect())
            }
            Component::DisjointDataProperties(DisjointDataProperties(v)) => {
                Some(v.iter().map(|m| format!("{m:?}")).collect())
            }
            Component::SameIndividual(SameIndividual(v)) => {
                Some(v.iter().map(|m| format!("{m:?}")).collect())
            }
            Component::DifferentIndividuals(DifferentIndividuals(v)) => {
                Some(v.iter().map(|m| format!("{m:?}")).collect())
            }
            _ => None,
        };
        if let Some(mut members) = members_opt {
            members.sort();
            emit_pairs(&members, &mut out);
        }
    }
    out
}

// ---------------------------------------------------------------------------
// OFN reader helper (mirrors `read_str` in mod.rs but uses the OFN parser)
// ---------------------------------------------------------------------------

use horned_owl::io::ofn::reader::read as read_ofn;

/// Parse a Functional-style OWL document string into a `SetOntology` + prefixes.
pub fn read_ofn_str(s: &str) -> Result<(O, PrefixMapping), String> {
    read_ofn::<Rc<str>, O, _>(
        std::io::BufReader::new(s.as_bytes()),
        horned_owl::io::ParserConfiguration::default(),
    )
    .map_err(|e| format!("{e}"))
}

// ---------------------------------------------------------------------------
// Corpus axiom-equality runner
// ---------------------------------------------------------------------------

/// Per-ontology result from the axiom-equality comparison.
#[derive(Debug)]
pub struct EqRow {
    pub name: String,
    /// Axioms present on BOTH sides (after canonicalization).
    pub matched: usize,
    /// In ofn-truth but missing from omn-candidate (first 20).
    pub missing: Vec<String>,
    /// In omn-candidate but absent from ofn-truth (first 20 — shouldn't be large).
    pub extra: Vec<String>,
    /// Symmetric difference under `nary_member_pairs` (first 20 pairs each side).
    pub nary_missing: Vec<String>,
    pub nary_extra: Vec<String>,
}

/// Run the axiom-equality comparison across the corpus.
///
/// For each ontology:
/// - source → ROBOT(.ofn)  → our OFN reader  = **truth** axiom set
/// - source → ROBOT(.omn)  → our OMN reader  = **candidate** axiom set
///
/// Both are canonicalized (declarations + metadata stripped) before diffing.
pub fn run_axiom_equality() -> Vec<EqRow> {
    let mut rows = Vec::new();
    for p in super::corpus::corpus_paths() {
        let name = p.file_stem().unwrap().to_string_lossy().into_owned();
        eprintln!("[A3] {name}: converting to omn via ROBOT…");
        let omn = match super::corpus::robot_to_fmt(&p, "omn", "owl") {
            Ok(s) => s,
            Err(e) => {
                eprintln!("[A3] {name}: ROBOT→omn failed: {e}");
                continue;
            }
        };
        eprintln!("[A3] {name}: converting to ofn via ROBOT…");
        let ofn = match super::corpus::robot_to_fmt(&p, "ofn", "owl") {
            Ok(s) => s,
            Err(e) => {
                eprintln!("[A3] {name}: ROBOT→ofn failed: {e}");
                continue;
            }
        };

        eprintln!("[A3] {name}: parsing omn ({} bytes)…", omn.len());
        let omn_ont = match read_str(&omn) {
            Ok((o, _)) => o,
            Err(e) => {
                eprintln!(
                    "[A3] {name}: omn parse failed: {}",
                    e.lines().next().unwrap_or("")
                );
                continue;
            }
        };

        eprintln!("[A3] {name}: parsing ofn ({} bytes)…", ofn.len());
        let ofn_ont = match read_ofn_str(&ofn) {
            Ok((o, _)) => o,
            Err(e) => {
                eprintln!(
                    "[A3] {name}: ofn parse failed: {}",
                    e.lines().next().unwrap_or("")
                );
                continue;
            }
        };

        // Flat canonical diff
        let cand: BTreeSet<_> = canonical(&omn_ont).into_iter().collect();
        let truth: BTreeSet<_> = canonical(&ofn_ont).into_iter().collect();
        let matched = cand.intersection(&truth).count();
        let missing: Vec<_> = truth.difference(&cand).take(20).cloned().collect();
        let extra: Vec<_> = cand.difference(&truth).take(20).cloned().collect();

        // n-ary pair-set diff (secondary, less noisy for equiv/disjoint axioms)
        let cand_pairs = nary_member_pairs(&omn_ont);
        let truth_pairs = nary_member_pairs(&ofn_ont);
        let nary_missing: Vec<_> = truth_pairs
            .difference(&cand_pairs)
            .take(20)
            .cloned()
            .collect();
        let nary_extra: Vec<_> = cand_pairs
            .difference(&truth_pairs)
            .take(20)
            .cloned()
            .collect();

        eprintln!(
            "[A3] {name}: matched={matched} missing={} extra={} \
             nary_missing={} nary_extra={}",
            missing.len(),
            extra.len(),
            nary_missing.len(),
            nary_extra.len()
        );
        rows.push(EqRow {
            name,
            matched,
            missing,
            extra,
            nary_missing,
            nary_extra,
        });
    }
    rows
}

// ---------------------------------------------------------------------------
// Unit tests (no docker required)
// ---------------------------------------------------------------------------

#[test]
fn canonical_drops_declarations() {
    let (o, _) = read_str("Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :B\n").unwrap();
    let canon = canonical(&o);
    // The SubClassOf axiom must survive.
    assert!(
        canon.iter().any(|s| s.contains("SubClassOf")),
        "expected SubClassOf in canonical output; got: {canon:?}"
    );
    // No Declare* variants should remain.
    assert!(
        canon.iter().all(|s| !s.contains("DeclareClass")),
        "DeclareClass should be stripped; got: {canon:?}"
    );
}

#[test]
fn nary_and_pairwise_canonicalize_equal() {
    let nary = "Prefix: : <http://e/>\nEquivalentClasses: :A , :B , :C\n";
    let pairwise = concat!(
        "Prefix: : <http://e/>\n",
        "EquivalentClasses: :A , :B\n",
        "EquivalentClasses: :B , :C\n",
        "EquivalentClasses: :A , :C\n",
    );
    let (o1, _) = read_str(nary).unwrap();
    let (o2, _) = read_str(pairwise).unwrap();
    let pairs1 = nary_member_pairs(&o1);
    let pairs2 = nary_member_pairs(&o2);
    assert_eq!(
        pairs1, pairs2,
        "n-ary and pairwise forms should produce the same pair set\n\
         n-ary pairs:    {pairs1:?}\n\
         pairwise pairs: {pairs2:?}"
    );
    // Sanity: we expect exactly 3 pairs (A|B, A|C, B|C).
    assert_eq!(
        pairs1.len(),
        3,
        "expected 3 unordered pairs for 3-member equiv"
    );
}

// ---------------------------------------------------------------------------
// Gated corpus test — reports diffs; does NOT panic on diffs (diffs are findings)
// ---------------------------------------------------------------------------

#[test]
#[ignore = "slow + docker/ROBOT-dependent; run via --ignored or the report generator"]
fn corpus_axiom_equality_documents_diffs() {
    if !super::corpus::docker_available() {
        eprintln!("SKIPPED A3: docker/ROBOT not available");
        return;
    }
    let rows = run_axiom_equality();
    if rows.is_empty() {
        eprintln!("A3: no corpus fixtures found (corpus_paths() returned empty)");
        return;
    }
    for r in &rows {
        eprintln!(
            "{}: matched={} missing={} extra={} nary_missing={} nary_extra={}",
            r.name,
            r.matched,
            r.missing.len(),
            r.extra.len(),
            r.nary_missing.len(),
            r.nary_extra.len()
        );
        for m in &r.missing {
            eprintln!("  MISSING {m}");
        }
        for e in &r.extra {
            eprintln!("  EXTRA   {e}");
        }
        if !r.nary_missing.is_empty() || !r.nary_extra.is_empty() {
            eprintln!("  [n-ary pairs diff]");
            for m in &r.nary_missing {
                eprintln!("  NARY_MISSING {m}");
            }
            for e in &r.nary_extra {
                eprintln!("  NARY_EXTRA   {e}");
            }
        }
    }
}
