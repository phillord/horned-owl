//! OBO flat-file format 1.4 reader.
//!
//! Parses an OBO 1.4 document with the vendored `fastobo-syntax` pest grammar
//! ([`lexer`]) and maps the pairs to horned-owl components ([`from_pair`]),
//! mirroring the structure of the Manchester (`omn`) and Functional (`ofn`)
//! readers.
//!
//! Covers the header and `[Term]` / `[Typedef]` / `[Instance]` frames over the
//! common clause set, validated against the ROBOT/`oboformat` oracle (see
//! [`crate::io::obo::oracle`]). Remaining gaps (GCIs, cardinality qualifiers,
//! alt_id, property chains, is_metadata_tag, treat-xrefs macros) are tracked in
//! [`from_pair`].
//!
//! ## Lenient by design
//!
//! The reader is **lenient by default** — its goal is to read the real OBO
//! Foundry / BioPortal corpus, which routinely deviates from a strict reading
//! of the 1.4 grammar. Like the `omn`/`ofn` readers, it does not consult
//! [`ParserConfiguration::lax`](crate::io::ParserConfiguration); the tolerances
//! are always on. Specifically, relative to the vendored `fastobo` grammar it:
//! - accepts `def:`/`synonym:` with no `[xref…]` list, and a synonym scope as
//!   the last token on the line;
//! - accepts messy real-world dbxref ids (internal spaces, parentheses, angle
//!   brackets, escaped punctuation), which oboformat/ROBOT also preserve;
//! - tolerates unknown / legacy clause tags (`exact_synonym:`, `xref_analog:`,
//!   `inverse_is_a:`, …) by skipping them rather than failing the file;
//! - decodes input **lossily** so a stray non-UTF-8 byte does not abort a read.
//!
//! All grammar relaxations are strict supersets — valid OBO still reads to the
//! same axioms (the ROBOT-oracle fixtures are unchanged). These raised corpus
//! read-coverage from ~54% to ~76% (BioPortal ≤3 MB sample).

pub mod from_pair;
pub mod lexer;

use std::io::BufRead;

use curie::PrefixMapping;

use self::lexer::{OboLexer, Rule};
use crate::error::HornedError;
use crate::io::ParserConfiguration;
use crate::model::{Build, ForIRI, MutableOntology, Ontology};

/// Read a whole ontology from an OBO document, using a fresh IRI `Build`.
/// Mirrors [`crate::io::omn::reader::read`].
pub fn read<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default, R: BufRead>(
    bufread: R,
    _config: ParserConfiguration,
) -> Result<(O, PrefixMapping), HornedError> {
    let b = Build::new();
    read_with_build(bufread, &b)
}

/// Read a whole ontology, interning IRIs into the supplied `build`.
pub fn read_with_build<A: ForIRI, O: MutableOntology<A> + Ontology<A> + Default, R: BufRead>(
    mut bufread: R,
    build: &Build<A>,
) -> Result<(O, PrefixMapping), HornedError> {
    // Lenient by default (see module doc): decode lossily so a stray non-UTF-8
    // byte — common in real bio-ontologies — does not abort the whole read.
    let mut bytes = Vec::new();
    bufread.read_to_end(&mut bytes)?;
    let doc = String::from_utf8_lossy(&bytes);

    // Lex the whole document.
    let obodoc = OboLexer::lex(Rule::OboDoc, &doc)?
        .next()
        .ok_or_else(|| HornedError::invalid("empty OBO document"))?;

    let children: Vec<_> = obodoc.into_inner().collect();

    // Pass 1: header frame → prefix mapping, idspace expansions, and the
    // default-namespace, all threaded through the conversion context.
    let header = children
        .iter()
        .find(|p| p.as_rule() == Rule::HeaderFrame)
        .cloned();
    let (prefixes, idspace, default_ns, onto_ns) = match &header {
        Some(h) => from_pair::scan_header::<A>(h),
        None => (from_pair::obo_prefixes(), Default::default(), None, None),
    };

    // Pass 1.5: scan [Typedef] frames for relation shorthands (bare id + single
    // xref) so relation uses resolve to the canonical IRI.
    let rel_map = from_pair::build_rel_map(&children, &idspace, onto_ns.as_deref());
    let metadata_tags =
        from_pair::build_metadata_tags(&children, &idspace, onto_ns.as_deref(), &rel_map);

    let ctx = from_pair::Context {
        build,
        idspace,
        default_ns,
        onto_ns,
        rel_map,
        metadata_tags,
    };

    // Accumulate every component, then run the finalisation passes (built-in
    // property labels + referenced-entity declarations) that oboformat/ROBOT
    // apply over the whole document, before inserting into the ontology.
    let mut comps = Vec::new();

    // Pass 2: header → ontology-level components.
    if let Some(h) = header {
        comps.extend(from_pair::header_to_components(h, &ctx)?);
    }

    // Pass 3: each [Term]/[Typedef]/[Instance] entity frame → components.
    for frame in children
        .into_iter()
        .filter(|p| p.as_rule() == Rule::EntityFrame)
    {
        // EntityFrame wraps exactly one of Term/Typedef/Instance frame.
        if let Some(inner) = frame.into_inner().next() {
            comps.extend(from_pair::entity_to_components(inner, &ctx)?);
        }
    }

    // Pass 4: whole-document finalisation.
    let comps = from_pair::finalize(comps, build);

    let mut ont = O::default();
    for c in comps {
        ont.insert(c);
    }

    Ok((ont, prefixes))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use crate::model::{
        AnnotationValue, ClassExpression, Component, Individual, Literal, ObjectPropertyExpression,
        Ontology, RcStr,
    };
    use crate::ontology::set::SetOntology;

    fn read(s: &str) -> SetOntology<RcStr> {
        super::read::<RcStr, SetOntology<RcStr>, _>(s.as_bytes(), Default::default())
            .unwrap()
            .0
    }

    /// Abbreviate an IRI to a `prefix:local` form for compact golden assertions.
    fn short(iri: &str) -> String {
        for (ns, p) in [
            ("http://purl.obolibrary.org/obo/", "obo:"),
            ("http://www.geneontology.org/formats/oboInOwl#", "oboInOwl:"),
            ("http://www.w3.org/2000/01/rdf-schema#", "rdfs:"),
            ("http://www.w3.org/2001/XMLSchema#", "xsd:"),
        ] {
            if let Some(local) = iri.strip_prefix(ns) {
                return format!("{p}{local}");
            }
        }
        iri.to_string()
    }

    fn ind(i: &Individual<RcStr>) -> String {
        match i {
            Individual::Named(n) => short(n.0.as_ref()),
            Individual::Anonymous(a) => format!("_:{}", a.0.as_ref()),
        }
    }

    fn op(ope: &ObjectPropertyExpression<RcStr>) -> String {
        match ope {
            ObjectPropertyExpression::ObjectProperty(p) => short(p.0.as_ref()),
            ObjectPropertyExpression::InverseObjectProperty(p) => {
                format!("inverse({})", short(p.0.as_ref()))
            }
        }
    }

    fn value(av: &AnnotationValue<RcStr>) -> String {
        match av {
            AnnotationValue::IRI(i) => short(i.as_ref()),
            AnnotationValue::Literal(Literal::Simple { literal }) => format!("{literal:?}"),
            AnnotationValue::Literal(Literal::Language { literal, lang }) => {
                format!("{literal:?}@{lang}")
            }
            AnnotationValue::Literal(Literal::Datatype {
                literal,
                datatype_iri,
            }) => {
                format!("{literal:?}^^{}", short(datatype_iri.as_ref()))
            }
            AnnotationValue::AnonymousIndividual(a) => format!("_:{}", a.0.as_ref()),
        }
    }

    /// Render the instance-relevant components in a compact, stable form for
    /// golden comparison.
    fn render_set<O: Ontology<RcStr>>(ont: &O) -> BTreeSet<String> {
        ont.iter()
            .map(|ac| match &ac.component {
                Component::DeclareClass(d) => {
                    format!("Declaration(Class {})", short(d.0.0.as_ref()))
                }
                Component::DeclareNamedIndividual(d) => {
                    format!("Declaration(NamedIndividual {})", short(d.0.0.as_ref()))
                }
                Component::DeclareObjectProperty(d) => {
                    format!("Declaration(ObjectProperty {})", short(d.0.0.as_ref()))
                }
                Component::DeclareAnnotationProperty(d) => {
                    format!("Declaration(AnnotationProperty {})", short(d.0.0.as_ref()))
                }
                Component::ClassAssertion(a) => {
                    let c = match &a.ce {
                        ClassExpression::Class(c) => short(c.0.as_ref()),
                        other => format!("{other:?}"),
                    };
                    format!("ClassAssertion({c} {})", ind(&a.i))
                }
                Component::ObjectPropertyAssertion(a) => format!(
                    "ObjectPropertyAssertion({} {} {})",
                    op(&a.ope),
                    ind(&a.from),
                    ind(&a.to)
                ),
                Component::AnnotationAssertion(a) => {
                    let subj = match &a.subject {
                        crate::model::AnnotationSubject::IRI(i) => short(i.as_ref()),
                        crate::model::AnnotationSubject::AnonymousIndividual(x) => {
                            format!("_:{}", x.0.as_ref())
                        }
                    };
                    format!(
                        "AnnotationAssertion({} {subj} {})",
                        short(a.ann.ap.0.as_ref()),
                        value(&a.ann.av)
                    )
                }
                other => format!("{other:?}"),
            })
            .collect()
    }

    fn has_label<O: Ontology<RcStr>>(ont: &O, subj: &str, value: &str) -> bool {
        ont.iter().any(|ac| match &ac.component {
            Component::AnnotationAssertion(a) => {
                matches!(&a.subject, crate::model::AnnotationSubject::IRI(i) if i.as_ref() == subj)
                    && a.ann.ap.0.as_ref() == "http://www.w3.org/2000/01/rdf-schema#label"
                    && matches!(&a.ann.av,
                        crate::model::AnnotationValue::Literal(
                            crate::model::Literal::Simple { literal }) if literal == value)
            }
            _ => false,
        })
    }

    const GO: &str = "http://purl.obolibrary.org/obo/GO_0008150";

    #[test]
    fn term_declaration_and_label() {
        let doc = "format-version: 1.2\n\n[Term]\nid: GO:0008150\nname: biological_process\n";
        let ont = read(doc);
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::DeclareClass(d) if d.0.0.as_ref() == GO)));
        assert!(has_label(&ont, GO, "biological_process"));
    }

    #[test]
    fn is_a_becomes_subclassof() {
        let doc = "[Term]\nid: GO:0008150\nis_a: GO:0003674 ! molecular_function\n";
        let ont = read(doc);
        let parent = "http://purl.obolibrary.org/obo/GO_0003674";
        assert!(ont.iter().any(|ac| match &ac.component {
            Component::SubClassOf(s) => matches!((&s.sub, &s.sup),
                (crate::model::ClassExpression::Class(a),
                 crate::model::ClassExpression::Class(b))
                if a.0.as_ref() == GO && b.0.as_ref() == parent),
            _ => false,
        }));
    }

    #[test]
    fn relationship_becomes_existential_subclassof() {
        let doc = "[Term]\nid: GO:0008150\nrelationship: part_of GO:0003674\n";
        let ont = read(doc);
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::SubClassOf(s) if matches!(&s.sup,
                crate::model::ClassExpression::ObjectSomeValuesFrom { .. }))));
    }

    #[test]
    fn def_carries_dbxref_axiom_annotations() {
        let doc = "[Term]\nid: GO:0008150\ndef: \"A process.\" [GOC:isa, PMID:123]\n";
        let ont = read(doc);
        let def = ont
            .iter()
            .find(|ac| {
                matches!(&ac.component,
                Component::AnnotationAssertion(a)
                if a.ann.ap.0.as_ref() == "http://purl.obolibrary.org/obo/IAO_0000115")
            })
            .expect("definition assertion present");
        // Two dbxrefs become two axiom-level annotations.
        assert_eq!(def.ann.len(), 2);
    }

    #[test]
    fn bare_relation_resolves_to_ontology_namespace() {
        // Oracle (ROBOT convert): a bare, undeclared relation in `ontology: test`
        // becomes `obo/test#part_of`, NOT the generic `obo/part_of`.
        let doc = "ontology: test\n\n[Term]\nid: GO:0008150\nrelationship: part_of GO:0005575\n";
        let ont = read(doc);
        let want = "http://purl.obolibrary.org/obo/test#part_of";
        assert!(ont.iter().any(|ac| match &ac.component {
            Component::SubClassOf(s) => matches!(&s.sup,
                crate::model::ClassExpression::ObjectSomeValuesFrom { ope, .. }
                if matches!(ope,
                    crate::model::ObjectPropertyExpression::ObjectProperty(p)
                    if p.0.as_ref() == want)),
            _ => false,
        }));
    }

    #[test]
    fn synonym_scope_maps_to_property() {
        let doc = "[Term]\nid: GO:0008150\nsynonym: \"bp\" EXACT [GOC:x]\n";
        let ont = read(doc);
        let syn = ont
            .iter()
            .find(|ac| {
                matches!(&ac.component,
                Component::AnnotationAssertion(a)
                if a.ann.ap.0.as_ref()
                    == "http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
            })
            .expect("exact-synonym assertion present");
        // The [GOC:x] dbxref becomes one axiom annotation.
        assert_eq!(syn.ann.len(), 1);
    }

    #[test]
    fn gci_qualifier_becomes_general_class_inclusion() {
        // relationship/is_a with gci_relation+gci_filler -> SubClassOf whose
        // subject is C ⊓ (gci_rel some gci_filler) (ROBOT-verified mapping).
        let doc = "ontology: t\n\n[Term]\nid: GO:0001\n\
                   relationship: part_of GO:0004 {gci_relation=\"part_of\", gci_filler=\"GO:0003\"}\n";
        let ont = read(doc);
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::SubClassOf(s)
            if matches!(&s.sub, ClassExpression::ObjectIntersectionOf(v)
                if v.len() == 2
                && matches!(&v[1], ClassExpression::ObjectSomeValuesFrom { .. })))));
        // and NOT a plain unconditional SubClassOf(GO:0001, ...) subject
        assert!(!ont.iter().any(|ac| matches!(&ac.component,
            Component::SubClassOf(s) if matches!(&s.sub, ClassExpression::Class(c)
                if c.0.as_ref() == "http://purl.obolibrary.org/obo/GO_0001")
                && matches!(&s.sup, ClassExpression::ObjectSomeValuesFrom { .. }))));
    }

    #[test]
    fn is_metadata_tag_typedef_is_annotation_property() {
        // is_metadata_tag: true -> annotation property; relationship uses of it
        // are annotation assertions, not existential SubClassOf (ROBOT-verified).
        let doc = "ontology: t\n\n[Typedef]\nid: mytag\nis_metadata_tag: true\n\n\
                   [Term]\nid: GO:0001\nrelationship: mytag GO:0003\n";
        let ont = read(doc);
        let tag = "http://purl.obolibrary.org/obo/t#mytag";
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::DeclareAnnotationProperty(d) if d.0.0.as_ref() == tag)));
        assert!(!ont.iter().any(
            |ac| matches!(&ac.component, Component::DeclareObjectProperty(d)
            if d.0.0.as_ref() == tag)
        ));
        // the relationship is an annotation assertion, not a SubClassOf
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::AnnotationAssertion(a) if a.ann.ap.0.as_ref() == tag)));
        assert!(
            !ont.iter()
                .any(|ac| matches!(&ac.component, Component::SubClassOf(_)))
        );
    }

    #[test]
    fn alt_id_materialises_deprecated_merged_class() {
        // alt_id -> hasAlternativeId on the term + a deprecated class merged
        // (replaced_by) into it with obsolescence reason "terms merged".
        let doc = "[Term]\nid: GO:0001\nalt_id: GO:0002\n";
        let ont = read(doc);
        let alt = "http://purl.obolibrary.org/obo/GO_0002";
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::AnnotationAssertion(a)
            if a.ann.ap.0.as_ref() == "http://www.geneontology.org/formats/oboInOwl#hasAlternativeId")));
        assert!(ont.iter().any(
            |ac| matches!(&ac.component, Component::DeclareClass(d) if d.0.0.as_ref() == alt)
        ));
        // replaced_by (IAO_0100001) from the alt class to the primary
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::AnnotationAssertion(a)
            if matches!(&a.subject, crate::model::AnnotationSubject::IRI(i) if i.as_ref() == alt)
            && a.ann.ap.0.as_ref() == "http://purl.obolibrary.org/obo/IAO_0100001")));
    }

    #[test]
    fn legacy_obo12_synonym_maps_to_scope() {
        // exact_synonym: "x" [xrefs]  (OBO 1.2) -> hasExactSynonym (ROBOT-verified)
        let doc = "[Term]\nid: GO:0001\nexact_synonym: \"foo\" [X:1]\n";
        let ont = read(doc);
        let syn = ont
            .iter()
            .find(|ac| {
                matches!(&ac.component,
            Component::AnnotationAssertion(a)
            if a.ann.ap.0.as_ref()
                == "http://www.geneontology.org/formats/oboInOwl#hasExactSynonym")
            })
            .expect("legacy exact_synonym maps to hasExactSynonym");
        assert_eq!(syn.ann.len(), 1); // the [X:1] dbxref
    }

    #[test]
    fn logical_definition_clauses() {
        // intersection_of (genus + differentia) -> one EquivalentClasses with an
        // ObjectIntersectionOf; equivalent_to/disjoint_from/union_of per oracle.
        let doc = "ontology: test\n\n\
                   [Term]\nid: GO:0001\n\
                   intersection_of: GO:0002\n\
                   intersection_of: part_of GO:0003\n\
                   equivalent_to: GO:0004\n\
                   disjoint_from: GO:0005\n\
                   union_of: GO:0006\n\
                   union_of: GO:0007\n";
        let ont = read(doc);
        let go = "http://purl.obolibrary.org/obo/GO_0001";
        let equivs: Vec<_> = ont
            .iter()
            .filter_map(|ac| match &ac.component {
                // each EquivalentClasses lists the defined class first
                Component::EquivalentClasses(e) => match &e.0[0] {
                    ClassExpression::Class(c) if c.0.as_ref() == go => Some(e.0[1].clone()),
                    _ => None,
                },
                _ => None,
            })
            .collect();
        // three EquivalentClasses: genus-differentia, equivalent_to, union_of
        assert_eq!(equivs.len(), 3);
        assert!(
            equivs
                .iter()
                .any(|e| matches!(e, ClassExpression::ObjectIntersectionOf(v) if v.len() == 2))
        );
        assert!(
            equivs
                .iter()
                .any(|e| matches!(e, ClassExpression::ObjectUnionOf(v) if v.len() == 2))
        );
        assert!(
            equivs
                .iter()
                .any(|e| matches!(e, ClassExpression::Class(_)))
        );
        assert!(
            ont.iter()
                .any(|ac| matches!(&ac.component, Component::DisjointClasses(_)))
        );
    }

    #[test]
    fn typedef_characteristics_and_relations() {
        let doc = "[Typedef]\nid: RO:0002211\nname: regulates\n\
                   is_transitive: true\nis_symmetric: false\n\
                   domain: GO:0008150\nrange: GO:0008150\ninverse_of: RO:0002212\n";
        let ont = read(doc);
        let ro = "http://purl.obolibrary.org/obo/RO_0002211";
        // true characteristic -> axiom
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::TransitiveObjectProperty(p)
            if matches!(&p.0, crate::model::ObjectPropertyExpression::ObjectProperty(o) if o.0.as_ref() == ro))));
        // false characteristic -> oboInOwl annotation, not an axiom
        assert!(
            !ont.iter()
                .any(|ac| matches!(&ac.component, Component::SymmetricObjectProperty(_)))
        );
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::AnnotationAssertion(a)
            if a.ann.ap.0.as_ref()
                == "http://www.geneontology.org/formats/oboInOwl#is_symmetric")));
        assert!(
            ont.iter()
                .any(|ac| matches!(&ac.component, Component::ObjectPropertyDomain(_)))
        );
        assert!(
            ont.iter()
                .any(|ac| matches!(&ac.component, Component::ObjectPropertyRange(_)))
        );
        assert!(
            ont.iter()
                .any(|ac| matches!(&ac.component, Component::InverseObjectProperties(_)))
        );
    }

    /// Golden test for `[Instance]` frames.
    ///
    /// oboformat/ROBOT reject Instance frames, so this mapping has no tool
    /// oracle; every expected axiom below is pinned to the normative OBO 1.4 →
    /// OWL mapping spec
    /// (<https://owlcollab.github.io/oboformat/doc/obo-syntax.html>), except the
    /// `property_value` rows, whose AnnotationAssertion form is oracle-grounded
    /// on a Term (see `src/ont/obo/property-values.obo`), and our two
    /// conventions (the `oboInOwl:id` annotation and its built-in label), which
    /// match how we map Term/Typedef frames.
    #[test]
    fn trailing_qualifier_becomes_axiom_annotation() {
        // is_a: X {source="PMID:1"} -> SubClassOf annotated with oboInOwl:source
        // (oracle: qualifiers.obo).
        let doc = "[Term]\nid: GO:0001\nis_a: GO:0002 {source=\"PMID:1\"}\n";
        let ont = read(doc);
        let sc = ont
            .iter()
            .find(|ac| matches!(&ac.component, Component::SubClassOf(_)))
            .expect("subclassof present");
        assert_eq!(sc.ann.len(), 1);
        let a = sc.ann.iter().next().unwrap();
        assert_eq!(
            a.ap.0.as_ref(),
            "http://www.geneontology.org/formats/oboInOwl#source"
        );
        assert!(matches!(&a.av,
            AnnotationValue::Literal(Literal::Simple { literal }) if literal == "PMID:1"));
    }

    #[test]
    fn relation_shorthand_resolves_to_xref() {
        // A bare [Typedef] id with a single xref is a shorthand: the property is
        // the xref IRI, the bare id survives as oboInOwl:id + oboInOwl:shorthand,
        // and relation uses resolve to the xref (oracle: shorthand.obo).
        let doc = "ontology: test\n\n\
                   [Typedef]\nid: part_of\nname: part of\nxref: BFO:0000050\n\n\
                   [Term]\nid: GO:0001\nrelationship: part_of GO:0002\n";
        let ont = read(doc);
        let bfo = "http://purl.obolibrary.org/obo/BFO_0000050";
        // property is declared under the xref IRI, not obo/test#part_of
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::DeclareObjectProperty(d) if d.0.0.as_ref() == bfo)));
        assert!(!ont.iter().any(|ac| matches!(&ac.component,
            Component::DeclareObjectProperty(d)
            if d.0.0.as_ref() == "http://purl.obolibrary.org/obo/test#part_of")));
        // shorthand annotation carries the bare id
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::AnnotationAssertion(a)
            if a.ann.ap.0.as_ref() == "http://www.geneontology.org/formats/oboInOwl#shorthand")));
        // the term relationship uses the xref IRI
        assert!(ont.iter().any(|ac| match &ac.component {
            Component::SubClassOf(s) => matches!(&s.sup,
                ClassExpression::ObjectSomeValuesFrom { ope, .. }
                if matches!(ope, ObjectPropertyExpression::ObjectProperty(p) if p.0.as_ref() == bfo)),
            _ => false,
        }));
    }

    #[test]
    fn instance_frame_golden() {
        let doc = "[Instance]\n\
                   id: ex:i1\n\
                   name: instance one\n\
                   instance_of: ex:C1\n\
                   relationship: ex:r ex:i2\n\
                   property_value: ex:p ex:i3\n\
                   property_value: ex:n \"5\" xsd:integer\n";
        let got = render_set(&read(doc));
        let want: BTreeSet<String> = [
            // §5.1 Instance frame declaration
            "Declaration(NamedIndividual obo:ex_i1)",
            // §5.5 instance_of -> ClassAssertion
            "ClassAssertion(obo:ex_C1 obo:ex_i1)",
            // §5.5 relationship -> object PropertyAssertion (individual -> individual)
            "ObjectPropertyAssertion(obo:ex_r obo:ex_i1 obo:ex_i2)",
            // §5.6 name -> rdfs:label
            "AnnotationAssertion(rdfs:label obo:ex_i1 \"instance one\")",
            // §5.6 property_value (resource) -> IRI-valued AnnotationAssertion
            "AnnotationAssertion(obo:ex_p obo:ex_i1 obo:ex_i3)",
            // §5.6 property_value (literal) -> typed-literal AnnotationAssertion
            "AnnotationAssertion(obo:ex_n obo:ex_i1 \"5\"^^xsd:integer)",
            // our convention: oboInOwl:id + its built-in label
            "AnnotationAssertion(oboInOwl:id obo:ex_i1 \"ex:i1\")",
            "AnnotationAssertion(rdfs:label oboInOwl:id \"id\")",
            // referenced-entity declarations (finalize)
            "Declaration(Class obo:ex_C1)",
            "Declaration(NamedIndividual obo:ex_i2)",
            "Declaration(ObjectProperty obo:ex_r)",
            "Declaration(AnnotationProperty obo:ex_p)",
            "Declaration(AnnotationProperty obo:ex_n)",
            "Declaration(AnnotationProperty oboInOwl:id)",
            "Declaration(AnnotationProperty rdfs:label)",
        ]
        .into_iter()
        .map(String::from)
        .collect();
        assert_eq!(got, want);
    }

    #[test]
    fn idspace_overrides_purl_expansion() {
        let doc = "idspace: CL http://example.org/cl/\n\n[Term]\nid: CL:0000000\n";
        let ont = read(doc);
        assert!(ont.iter().any(|ac| matches!(&ac.component,
            Component::DeclareClass(d) if d.0.0.as_ref() == "http://example.org/cl/0000000")));
    }
}
