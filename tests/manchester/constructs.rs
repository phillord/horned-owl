//! A1 — §2.5 per-construct coverage matrix.
//!
//! Each row exercises ONE §2.5 construct. Non-residual rows must pass
//! read + round-trip (`roundtrip_ok`). Residual rows document a known
//! limitation; they only need to parse or behave as characterized.
use super::*;

// ---------------------------------------------------------------------------
// Case type
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Residual {
    /// No residual — the construct is fully supported and must round-trip.
    None,
    /// Manchester §2.5 has no `Rule:` syntax; parse fails by design.
    SwrlRule,
    /// Complex-LHS GCI expressed as a `# General axioms` functional-syntax
    /// block.  The reader skips that block with a warning; the document parses
    /// (`read_ok`), but the axiom is absent from the component set (visible
    /// via `note`).
    ComplexLhsGci,
    /// Nested annotations are parsed and silently dropped (model limit).
    NestedAnnotationDropped,
    /// `HasKey:` with a data-property key when the key IRI has no `DataProperty:`
    /// declaration in the document — the reader cannot determine the property
    /// type and defaults to `ObjectPropertyExpression`.  Parses and round-trips
    /// stably (as an object key), but the round-trip axiom differs from the
    /// intended data-property form.  Resolved when a `DataProperty:` declaration
    /// is present (see `haskey.data.declared`).
    HasKeyObjectDataConflation,
    /// A bare local name with no declared default prefix is not lexable.
    BareNameNeedsPrefix,
}

pub struct Case {
    pub id: &'static str,
    /// A complete, minimal Manchester document exercising one construct.
    pub omn: &'static str,
    /// A substring expected in the Debug of at least one parsed component.
    /// Use `""` when only round-trip identity is asserted.
    pub expect_debug_contains: &'static str,
    pub residual: Residual,
}

// ---------------------------------------------------------------------------
// The case table — ONE ROW PER §2.5 CONSTRUCT
// ---------------------------------------------------------------------------

pub const CASES: &[Case] = &[
    // -----------------------------------------------------------------------
    // Class frame — per-clause
    // -----------------------------------------------------------------------
    Case {
        id: "class.subclassof",
        residual: Residual::None,
        expect_debug_contains: "SubClassOf",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :B\n",
    },
    Case {
        id: "class.equivalentto",
        residual: Residual::None,
        expect_debug_contains: "EquivalentClasses",
        omn: "Prefix: : <http://e/>\nClass: :A\n    EquivalentTo: :B\n",
    },
    Case {
        id: "class.disjointwith",
        residual: Residual::None,
        expect_debug_contains: "DisjointClasses",
        omn: "Prefix: : <http://e/>\nClass: :A\n    DisjointWith: :B\n",
    },
    Case {
        id: "class.disjunionof",
        residual: Residual::None,
        expect_debug_contains: "DisjointUnion",
        omn: "Prefix: : <http://e/>\nClass: :A\n    DisjointUnionOf: :B , :C\n",
    },
    Case {
        id: "class.haskey",
        residual: Residual::None,
        expect_debug_contains: "HasKey",
        omn: "Prefix: : <http://e/>\nClass: :A\n    HasKey: :r , :s\n",
    },
    Case {
        id: "class.annotations",
        residual: Residual::None,
        expect_debug_contains: "AnnotationAssertion",
        omn: "Prefix: : <http://e/>\nPrefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Class: :A\n    Annotations: rdfs:comment \"frame-level\"\n    SubClassOf: :B\n",
    },
    // -----------------------------------------------------------------------
    // ObjectProperty frame — per-clause
    // -----------------------------------------------------------------------
    Case {
        id: "op.domain",
        residual: Residual::None,
        expect_debug_contains: "ObjectPropertyDomain",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Domain: :A\n",
    },
    Case {
        id: "op.range",
        residual: Residual::None,
        expect_debug_contains: "ObjectPropertyRange",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Range: :A\n",
    },
    Case {
        id: "op.subpropertyof",
        residual: Residual::None,
        expect_debug_contains: "SubObjectPropertyOf",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    SubPropertyOf: :s\n",
    },
    Case {
        id: "op.equivalentto",
        residual: Residual::None,
        expect_debug_contains: "EquivalentObjectProperties",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    EquivalentTo: :s\n",
    },
    Case {
        id: "op.disjointwith",
        residual: Residual::None,
        expect_debug_contains: "DisjointObjectProperties",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    DisjointWith: :s\n",
    },
    Case {
        id: "op.inverseof",
        residual: Residual::None,
        expect_debug_contains: "InverseObjectProperties",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    InverseOf: :s\n",
    },
    // Characteristics
    Case {
        id: "op.char.functional",
        residual: Residual::None,
        expect_debug_contains: "FunctionalObjectProperty",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Characteristics: Functional\n",
    },
    Case {
        id: "op.char.inversefunctional",
        residual: Residual::None,
        expect_debug_contains: "InverseFunctionalObjectProperty",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Characteristics: InverseFunctional\n",
    },
    Case {
        id: "op.char.reflexive",
        residual: Residual::None,
        expect_debug_contains: "ReflexiveObjectProperty",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Characteristics: Reflexive\n",
    },
    Case {
        id: "op.char.irreflexive",
        residual: Residual::None,
        expect_debug_contains: "IrreflexiveObjectProperty",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Characteristics: Irreflexive\n",
    },
    Case {
        id: "op.char.symmetric",
        residual: Residual::None,
        expect_debug_contains: "SymmetricObjectProperty",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Characteristics: Symmetric\n",
    },
    Case {
        id: "op.char.asymmetric",
        residual: Residual::None,
        expect_debug_contains: "AsymmetricObjectProperty",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Characteristics: Asymmetric\n",
    },
    Case {
        id: "op.char.transitive",
        residual: Residual::None,
        expect_debug_contains: "TransitiveObjectProperty",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    Characteristics: Transitive\n",
    },
    // SubPropertyChain
    Case {
        id: "op.subpropertychain",
        residual: Residual::None,
        expect_debug_contains: "ObjectPropertyChain",
        omn: "Prefix: : <http://e/>\nObjectProperty: :r\n    SubPropertyChain: :s o :t\n",
    },
    // -----------------------------------------------------------------------
    // DataProperty frame — per-clause
    // -----------------------------------------------------------------------
    Case {
        id: "dp.domain",
        residual: Residual::None,
        expect_debug_contains: "DataPropertyDomain",
        omn: "Prefix: : <http://e/>\nDataProperty: :p\n    Domain: :A\n",
    },
    Case {
        id: "dp.range",
        residual: Residual::None,
        expect_debug_contains: "DataPropertyRange",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:string\n",
    },
    Case {
        id: "dp.subpropertyof",
        residual: Residual::None,
        expect_debug_contains: "SubDataPropertyOf",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    SubPropertyOf: :q\n",
    },
    Case {
        id: "dp.equivalentto",
        residual: Residual::None,
        expect_debug_contains: "EquivalentDataProperties",
        omn: "Prefix: : <http://e/>\nDataProperty: :p\n    EquivalentTo: :q\n",
    },
    Case {
        id: "dp.disjointwith",
        residual: Residual::None,
        expect_debug_contains: "DisjointDataProperties",
        omn: "Prefix: : <http://e/>\nDataProperty: :p\n    DisjointWith: :q\n",
    },
    Case {
        id: "dp.char.functional",
        residual: Residual::None,
        expect_debug_contains: "FunctionalDataProperty",
        omn: "Prefix: : <http://e/>\nDataProperty: :p\n    Characteristics: Functional\n",
    },
    // -----------------------------------------------------------------------
    // AnnotationProperty frame — per-clause
    // -----------------------------------------------------------------------
    Case {
        id: "annprop.domain",
        residual: Residual::None,
        expect_debug_contains: "AnnotationPropertyDomain",
        omn: "Prefix: : <http://e/>\nAnnotationProperty: :note\n    Domain: :A\n",
    },
    Case {
        id: "annprop.range",
        residual: Residual::None,
        expect_debug_contains: "AnnotationPropertyRange",
        omn: "Prefix: : <http://e/>\nAnnotationProperty: :note\n    Range: :A\n",
    },
    Case {
        id: "annprop.subpropertyof",
        residual: Residual::None,
        expect_debug_contains: "SubAnnotationPropertyOf",
        omn: "Prefix: : <http://e/>\nAnnotationProperty: :note\n    SubPropertyOf: :meta\n",
    },
    // -----------------------------------------------------------------------
    // Restriction forms (all tested via Class SubClassOf)
    // -----------------------------------------------------------------------
    Case {
        id: "ce.some",
        residual: Residual::None,
        expect_debug_contains: "ObjectSomeValuesFrom",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r some :B\n",
    },
    Case {
        id: "ce.only",
        residual: Residual::None,
        expect_debug_contains: "ObjectAllValuesFrom",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r only :B\n",
    },
    Case {
        id: "ce.value",
        residual: Residual::None,
        expect_debug_contains: "ObjectHasValue",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r value :x\n",
    },
    // OWL-API/Protégé emit bare `true`/`false` as xsd:boolean DataHasValue.
    // Must parse as DataHasValue, NOT ObjectHasValue over a bare-name IRI.
    Case {
        id: "dp.value.boolean.true",
        residual: Residual::None,
        expect_debug_contains: "DataHasValue",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :p value true\n",
    },
    Case {
        id: "dp.value.boolean.false",
        residual: Residual::None,
        expect_debug_contains: "DataHasValue",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :p value false\n",
    },
    Case {
        id: "ce.self",
        residual: Residual::None,
        expect_debug_contains: "ObjectHasSelf",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r Self\n",
    },
    Case {
        id: "ce.min.qualified",
        residual: Residual::None,
        expect_debug_contains: "ObjectMinCardinality",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r min 2 :B\n",
    },
    Case {
        id: "ce.max.qualified",
        residual: Residual::None,
        expect_debug_contains: "ObjectMaxCardinality",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r max 2 :B\n",
    },
    Case {
        id: "ce.exactly.qualified",
        residual: Residual::None,
        expect_debug_contains: "ObjectExactCardinality",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r exactly 2 :B\n",
    },
    Case {
        id: "ce.min.unqualified",
        residual: Residual::None,
        expect_debug_contains: "ObjectMinCardinality",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r min 1\n",
    },
    Case {
        id: "ce.max.unqualified",
        residual: Residual::None,
        expect_debug_contains: "ObjectMaxCardinality",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r max 1\n",
    },
    Case {
        id: "ce.exactly.unqualified",
        residual: Residual::None,
        expect_debug_contains: "ObjectExactCardinality",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r exactly 1\n",
    },
    // -----------------------------------------------------------------------
    // Class-expression operators
    // -----------------------------------------------------------------------
    Case {
        id: "ce.and",
        residual: Residual::None,
        expect_debug_contains: "ObjectIntersectionOf",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :B and :C\n",
    },
    Case {
        id: "ce.or",
        residual: Residual::None,
        expect_debug_contains: "ObjectUnionOf",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :B or :C\n",
    },
    Case {
        id: "ce.not",
        residual: Residual::None,
        expect_debug_contains: "ObjectComplementOf",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: not :B\n",
    },
    Case {
        id: "ce.oneof",
        residual: Residual::None,
        expect_debug_contains: "ObjectOneOf",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: {:x , :y}\n",
    },
    // Parenthesized `inverse(R)` form — the canonical round-trip path.
    Case {
        id: "ce.inverse",
        residual: Residual::None,
        expect_debug_contains: "InverseObjectProperty",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: inverse(:r) some :B\n",
    },
    // Bare `inverse R` (no parentheses) — §2.5 grammar allows both forms;
    // parses identically to the parenthesized form above.
    Case {
        id: "ce.inverse.bare",
        residual: Residual::None,
        expect_debug_contains: "InverseObjectProperty",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: inverse :r some :B\n",
    },
    Case {
        id: "ce.parens",
        residual: Residual::None,
        expect_debug_contains: "ObjectIntersectionOf",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: (:B and :C)\n",
    },
    // Nested class expression: restriction whose filler is itself a conjunction.
    Case {
        id: "ce.nested",
        residual: Residual::None,
        expect_debug_contains: "ObjectIntersectionOf",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r some (:B and :C)\n",
    },
    // -----------------------------------------------------------------------
    // Data ranges
    // -----------------------------------------------------------------------
    Case {
        id: "dr.datatype",
        residual: Residual::None,
        expect_debug_contains: "DataPropertyRange",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:integer\n",
    },
    Case {
        id: "dr.and",
        residual: Residual::None,
        expect_debug_contains: "DataIntersectionOf",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:integer and xsd:string\n",
    },
    Case {
        id: "dr.or",
        residual: Residual::None,
        expect_debug_contains: "DataUnionOf",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:integer or xsd:string\n",
    },
    Case {
        id: "dr.not",
        residual: Residual::None,
        expect_debug_contains: "DataComplementOf",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: not xsd:integer\n",
    },
    Case {
        id: "dr.oneof",
        residual: Residual::None,
        expect_debug_contains: "DataOneOf",
        omn: "Prefix: : <http://e/>\nDataProperty: :p\n    Range: {\"a\", \"b\"}\n",
    },
    Case {
        id: "dr.parens",
        residual: Residual::None,
        expect_debug_contains: "DataPropertyRange",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: (xsd:integer)\n",
    },
    // Facet restrictions
    Case {
        id: "dr.facet.mininclusive",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:integer[>= 1]\n",
    },
    Case {
        id: "dr.facet.minexclusive",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:integer[> 0]\n",
    },
    Case {
        id: "dr.facet.maxinclusive",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:integer[<= 100]\n",
    },
    Case {
        id: "dr.facet.maxexclusive",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:integer[< 100]\n",
    },
    Case {
        id: "dr.facet.length",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:string[length 5]\n",
    },
    Case {
        id: "dr.facet.minlength",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:string[minLength 3]\n",
    },
    Case {
        id: "dr.facet.maxlength",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:string[maxLength 10]\n",
    },
    Case {
        id: "dr.facet.pattern",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              DataProperty: :p\n    Range: xsd:string[pattern \"[a-z]+\"]\n",
    },
    Case {
        id: "dr.facet.langrange",
        residual: Residual::None,
        expect_debug_contains: "DatatypeRestriction",
        omn: "Prefix: : <http://e/>\n\
              Prefix: rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n\
              DataProperty: :p\n    Range: rdf:langString[langRange \"en\"]\n",
    },
    // -----------------------------------------------------------------------
    // Literals
    // -----------------------------------------------------------------------
    Case {
        id: "lit.bare.integer",
        residual: Residual::None,
        expect_debug_contains: "DataPropertyAssertion",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    Facts: :p 3\n",
    },
    Case {
        id: "lit.bare.decimal",
        residual: Residual::None,
        expect_debug_contains: "DataPropertyAssertion",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    Facts: :p 3.14\n",
    },
    Case {
        id: "lit.bare.float",
        residual: Residual::None,
        expect_debug_contains: "DataPropertyAssertion",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    Facts: :p 1.5f\n",
    },
    Case {
        id: "lit.plain.string",
        residual: Residual::None,
        expect_debug_contains: "AnnotationAssertion",
        omn: "Prefix: : <http://e/>\nPrefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Class: :A\n    Annotations: rdfs:comment \"hello\"\n",
    },
    Case {
        id: "lit.lang.tagged",
        residual: Residual::None,
        expect_debug_contains: "AnnotationAssertion",
        omn: "Prefix: : <http://e/>\nPrefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Class: :A\n    Annotations: rdfs:comment \"hello\"@en\n",
    },
    Case {
        id: "lit.typed",
        residual: Residual::None,
        expect_debug_contains: "AnnotationAssertion",
        omn: "Prefix: : <http://e/>\n\
              Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Class: :A\n    Annotations: rdfs:comment \"42\"^^xsd:integer\n",
    },
    // §2.5 quotedString escape sequences: `\"` and `\\` inside a literal.
    // The writer must use `char_indices()` (byte offsets) when scanning for
    // characters to escape — using `chars().enumerate()` (char ordinals)
    // causes incorrect byte-slicing for multi-byte UTF-8 prefixes, corrupting
    // the literal content after the first non-ASCII character.
    Case {
        id: "lit.escaped.quote",
        residual: Residual::None,
        expect_debug_contains: "AnnotationAssertion",
        // Literal contains `α` (2-byte UTF-8) BEFORE the `"` that must be
        // escaped — this triggers the char-vs-byte index bug if present.
        omn: "Prefix: : <http://e/>\n\
              Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Class: :A\n    Annotations: rdfs:comment \"17α the \\\"stress hormone\\\"\"\n",
    },
    Case {
        id: "lit.escaped.backslash",
        residual: Residual::None,
        expect_debug_contains: "AnnotationAssertion",
        // Literal contains `α` before a `\` that must be escaped.
        omn: "Prefix: : <http://e/>\n\
              Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Class: :A\n    Annotations: rdfs:comment \"17α path\\\\separator\"\n",
    },
    // -----------------------------------------------------------------------
    // Datatype definition
    // -----------------------------------------------------------------------
    Case {
        id: "datatype.def",
        residual: Residual::None,
        expect_debug_contains: "DatatypeDefinition",
        omn: "Prefix: : <http://e/>\nPrefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              Datatype: :Small\n    EquivalentTo: xsd:integer[<= 9]\n",
    },
    // -----------------------------------------------------------------------
    // Misc top-level axioms
    // Note: Misc axioms are re-emitted as frame clauses by the writer.
    // When the writer turns `DisjointClasses: :A , :B` into `Class: A
    // DisjointWith: B`, re-parsing adds `DeclareClass(A)`.  We include
    // explicit entity-frame declarations in each document so the round-trip
    // is stable (the first parse already has those declarations).
    // -----------------------------------------------------------------------
    Case {
        id: "misc.equivalentclasses",
        residual: Residual::None,
        expect_debug_contains: "EquivalentClasses",
        omn: "Prefix: : <http://e/>\nClass: :A\nClass: :B\nEquivalentClasses: :A , :B\n",
    },
    Case {
        id: "misc.disjointclasses",
        residual: Residual::None,
        expect_debug_contains: "DisjointClasses",
        omn: "Prefix: : <http://e/>\nClass: :A\nClass: :B\nDisjointClasses: :A , :B\n",
    },
    Case {
        id: "misc.equivalentproperties.obj",
        residual: Residual::None,
        expect_debug_contains: "EquivalentObjectProperties",
        omn: "Prefix: : <http://e/>\n\
              ObjectProperty: :r\nObjectProperty: :s\nEquivalentProperties: :r , :s\n",
    },
    Case {
        id: "misc.disjointproperties.obj",
        residual: Residual::None,
        expect_debug_contains: "DisjointObjectProperties",
        omn: "Prefix: : <http://e/>\n\
              ObjectProperty: :r\nObjectProperty: :s\nDisjointProperties: :r , :s\n",
    },
    Case {
        id: "misc.sameindividual",
        residual: Residual::None,
        expect_debug_contains: "SameIndividual",
        omn: "Prefix: : <http://e/>\n\
              Individual: :a\nIndividual: :b\nSameIndividual: :a , :b\n",
    },
    Case {
        id: "misc.differentindividuals",
        residual: Residual::None,
        expect_debug_contains: "DifferentIndividuals",
        omn: "Prefix: : <http://e/>\n\
              Individual: :a\nIndividual: :b\nDifferentIndividuals: :a , :b\n",
    },
    // -----------------------------------------------------------------------
    // Annotations — various forms
    // -----------------------------------------------------------------------
    // Entity-frame-level annotation (already covered by class.annotations)
    // Per-list-item annotation: leading `Annotations:` binds FIRST item only.
    Case {
        id: "ann.peritem.leading",
        residual: Residual::None,
        expect_debug_contains: "SubClassOf",
        omn: "Prefix: : <http://e/>\n\
              Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Class: :A\n    SubClassOf: Annotations: rdfs:comment \"x\" :B , :C\n",
    },
    // Post-comma annotation binds the SECOND item.
    Case {
        id: "ann.peritem.postcomma",
        residual: Residual::None,
        expect_debug_contains: "SubClassOf",
        omn: "Prefix: : <http://e/>\n\
              Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Class: :A\n    SubClassOf: :B , Annotations: rdfs:comment \"y\" :C\n",
    },
    // Nested annotation-on-annotation: the inner nesting is parsed but dropped.
    Case {
        id: "ann.nested",
        residual: Residual::NestedAnnotationDropped,
        expect_debug_contains: "AnnotationAssertion",
        omn: "Prefix: : <http://e/>\n\
              Class: :A\n    Annotations: Annotations: :m \"x\" :note \"y\"\n",
    },
    // Ontology annotation
    Case {
        id: "ann.ontology",
        residual: Residual::None,
        expect_debug_contains: "OntologyAnnotation",
        omn: "Prefix: : <http://e/>\n\
              Prefix: rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n\
              Ontology:\n    Annotations: rdfs:comment \"test\"\n",
    },
    // Anonymous-individual annotation value
    Case {
        id: "ann.anon.indiv.value",
        residual: Residual::None,
        expect_debug_contains: "AnnotationAssertion",
        omn: "Prefix: : <http://e/>\n\
              Class: :A\n    Annotations: :note _:b1\n",
    },
    // -----------------------------------------------------------------------
    // Header
    // -----------------------------------------------------------------------
    Case {
        id: "header.ontology.iri",
        residual: Residual::None,
        expect_debug_contains: "OntologyID",
        omn: "Prefix: : <http://e/>\nOntology: <http://e/o>\n",
    },
    Case {
        id: "header.versioniri",
        residual: Residual::None,
        expect_debug_contains: "",
        omn: "Prefix: : <http://e/>\nOntology: <http://e/o> <http://e/o/1.0>\n",
    },
    Case {
        id: "header.import",
        residual: Residual::None,
        expect_debug_contains: "Import",
        omn: "Prefix: : <http://e/>\nOntology:\n    Import: <http://other/>\n",
    },
    // -----------------------------------------------------------------------
    // Individual frame
    // -----------------------------------------------------------------------
    Case {
        id: "indiv.named.type",
        residual: Residual::None,
        expect_debug_contains: "ClassAssertion",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    Types: :A\n",
    },
    Case {
        id: "indiv.named.sameas",
        residual: Residual::None,
        expect_debug_contains: "SameIndividual",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    SameAs: :b\n",
    },
    Case {
        id: "indiv.named.differentfrom",
        residual: Residual::None,
        expect_debug_contains: "DifferentIndividuals",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    DifferentFrom: :b\n",
    },
    Case {
        id: "indiv.named.opafact",
        residual: Residual::None,
        expect_debug_contains: "ObjectPropertyAssertion",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    Facts: :r :b\n",
    },
    Case {
        id: "indiv.named.dpafact",
        residual: Residual::None,
        expect_debug_contains: "DataPropertyAssertion",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    Facts: :p \"hello\"\n",
    },
    Case {
        id: "indiv.named.neg.opa",
        residual: Residual::None,
        expect_debug_contains: "NegativeObjectPropertyAssertion",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    Facts: not :r :b\n",
    },
    Case {
        id: "indiv.named.neg.dpa",
        residual: Residual::None,
        expect_debug_contains: "NegativeDataPropertyAssertion",
        omn: "Prefix: : <http://e/>\nIndividual: :a\n    Facts: not :p \"hello\"\n",
    },
    // Anonymous individuals as frame subjects round-trip correctly: the writer
    // emits an `Individual: _:b1` frame (matching the reader's accepted form)
    // instead of routing to the `# General axioms` block.
    Case {
        id: "indiv.anonymous",
        residual: Residual::None,
        expect_debug_contains: "AnonymousIndividual",
        omn: "Prefix: : <http://e/>\nIndividual: _:b1\n    Types: :A\n",
    },
    // -----------------------------------------------------------------------
    // DataProperty restriction: known-datatype filler → DataSomeValuesFrom
    // (filler-shape heuristic: known xsd:/rdf:/rdfs:/owl: prefix ⇒ data restriction)
    // -----------------------------------------------------------------------
    Case {
        id: "dr.restriction.known_datatype",
        residual: Residual::None,
        expect_debug_contains: "DataSomeValuesFrom",
        omn: "Prefix: : <http://e/>\n\
              Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              Class: :A\n    SubClassOf: :p some xsd:integer\n",
    },
    // DataProperty restriction: faceted filler → DataSomeValuesFrom
    // (filler-shape heuristic: facet bracket `dt[…]` ⇒ data restriction)
    Case {
        id: "dr.restriction.faceted",
        residual: Residual::None,
        expect_debug_contains: "DataSomeValuesFrom",
        omn: "Prefix: : <http://e/>\n\
              Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n\
              Class: :A\n    SubClassOf: :p some xsd:double[>= \"0.0\"^^xsd:double]\n",
    },
    // Object restriction with plain class-IRI filler MUST stay ObjectSomeValuesFrom
    // (regression guard: the heuristic must NOT capture plain class IRIs as data)
    Case {
        id: "dr.restriction.object_guard",
        residual: Residual::None,
        expect_debug_contains: "ObjectSomeValuesFrom",
        omn: "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r some :B\n",
    },
    // -----------------------------------------------------------------------
    // HasKey object/data conflation — now resolved via declaration pre-pass.
    // -----------------------------------------------------------------------
    Case {
        id: "residual.haskey.objonly",
        residual: Residual::None,
        expect_debug_contains: "HasKey",
        omn: "Prefix: : <http://e/>\nClass: :A\n    HasKey: :r , :s\n",
    },
    // Declared data-property key — pre-pass flips to PropertyExpression::DataProperty.
    // Round-trip re-reads the document which now has a `DataProperty: :p` declaration
    // → key survives as DataProperty through the re-read.
    Case {
        id: "haskey.data.declared",
        residual: Residual::None,
        expect_debug_contains: "DataProperty",
        omn: "Prefix: : <http://e/>\n\
              DataProperty: :p\n\
              Class: :A\n    HasKey: :p\n",
    },
    // Undeclared key — no declaration, falls back to ObjectPropertyExpression.
    // This is the documented residual tail: undeclared → stays object.
    Case {
        id: "residual.haskey.undeclared",
        residual: Residual::HasKeyObjectDataConflation,
        expect_debug_contains: "HasKey",
        omn: "Prefix: : <http://e/>\nClass: :A\n    HasKey: :p\n",
    },
    // -----------------------------------------------------------------------
    // EquivalentProperties/DisjointProperties over data properties in the
    // Misc section — now resolved via declaration pre-pass.
    // -----------------------------------------------------------------------
    Case {
        id: "residual.misc.equivdp",
        residual: Residual::None,
        expect_debug_contains: "EquivalentDataProperties",
        omn: "Prefix: : <http://e/>\n\
              DataProperty: :p\nDataProperty: :q\nEquivalentProperties: :p , :q\n",
    },
    Case {
        id: "residual.misc.disjdp",
        residual: Residual::None,
        expect_debug_contains: "DisjointDataProperties",
        omn: "Prefix: : <http://e/>\n\
              DataProperty: :p\nDataProperty: :q\nDisjointProperties: :p , :q\n",
    },
    // Undeclared / mixed property lists stay as object-property form
    // (regression guard: object properties without DataProperty: declaration).
    Case {
        id: "misc.equivprops.obj.undeclared",
        residual: Residual::None,
        expect_debug_contains: "EquivalentObjectProperties",
        omn: "Prefix: : <http://e/>\n\
              ObjectProperty: :r\nObjectProperty: :s\nEquivalentProperties: :r , :s\n",
    },
    // -----------------------------------------------------------------------
    // SWRL Rule — Manchester §2.5 has no rule syntax; parse fails.
    // -----------------------------------------------------------------------
    Case {
        id: "residual.swrl",
        residual: Residual::SwrlRule,
        expect_debug_contains: "",
        omn: "Prefix: : <http://e/>\nRule: :A(?x) -> :B(?x)\n",
    },
    // -----------------------------------------------------------------------
    // Bare local name without a declared default prefix is not lexable.
    // -----------------------------------------------------------------------
    Case {
        id: "residual.barename",
        residual: Residual::BareNameNeedsPrefix,
        expect_debug_contains: "",
        omn: "Class: Foo\n",
    },
    // -----------------------------------------------------------------------
    // Complex-LHS GCI — no §2.5 frame form.  The writer emits it to the
    // `# General axioms` functional-syntax block; the reader skips that block
    // (returns Ok, prints a warning).  Document parses cleanly (`read_ok`),
    // but the complex-subject axiom is absent from the component set —
    // demonstrated by asserting `expect_debug_contains` yields a non-empty
    // `note` (the axiom was not found).
    // -----------------------------------------------------------------------
    Case {
        id: "residual.complexgci",
        residual: Residual::ComplexLhsGci,
        // The complex SubClassOf axiom's subject is an intersection; if the
        // axiom were present we'd see "ObjectIntersectionOf" in a component.
        // Since the block is skipped it will be absent → note populated.
        expect_debug_contains: "ObjectIntersectionOf",
        omn: "Prefix: : <http://e/>\n\
              # General axioms\n\
              SubClassOf(ObjectIntersectionOf(<http://e/A> <http://e/B>) <http://e/C>)\n",
    },
    // -----------------------------------------------------------------------
    // Complex-LHS GCI as a `Class:` frame — OWL-API/Protégé/ROBOT extension.
    // The reader accepts `Class: <complexExpr> SubClassOf: ...` and emits the
    // GCI axiom.  The writer (FIX-7) emits complex-LHS SubClassOf as a
    // `Class: <expr>` frame, so the axiom now round-trips.
    // -----------------------------------------------------------------------
    Case {
        id: "class.complexgci.frame",
        residual: Residual::None,
        // The GCI is parsed and round-trips → ObjectSomeValuesFrom present.
        expect_debug_contains: "ObjectSomeValuesFrom",
        omn: "Prefix: : <http://e/>\n\
              Class: :r some :C\n\
                  SubClassOf: :D\n",
    },
];

// ---------------------------------------------------------------------------
// Boolean DataHasValue exact-type assertion
// ---------------------------------------------------------------------------

/// Verify `p value true` and `p value false` produce `DataHasValue` with the
/// EXACT typed literal `"true"^^xsd:boolean` / `"false"^^xsd:boolean`,
/// and that `r value :x` still produces `ObjectHasValue`.
#[test]
fn dp_value_boolean_exact_literal() {
    use horned_owl::model::{ClassExpression, Component, Literal, SubClassOf};

    let xsd_boolean = "http://www.w3.org/2001/XMLSchema#boolean";

    for (label, src, expected_value) in [
        (
            "true",
            "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :p value true\n",
            "true",
        ),
        (
            "false",
            "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :p value false\n",
            "false",
        ),
    ] {
        let (ont, _) =
            read_str(src).unwrap_or_else(|e| panic!("dp.value.boolean.{label}: parse failed: {e}"));

        // Find the SubClassOf axiom with a non-named filler.
        let found = ont.iter().find_map(|ac| {
            if let Component::SubClassOf(SubClassOf { sup, .. }) = &ac.component {
                if let ClassExpression::DataHasValue { dp: _, l } = sup {
                    return Some(l.clone());
                }
            }
            None
        });

        let lit = found.unwrap_or_else(|| {
            let debug: Vec<_> = ont.iter().map(|ac| format!("{:?}", ac.component)).collect();
            panic!(
                "dp.value.boolean.{label}: expected DataHasValue, got:\n{}",
                debug.join("\n")
            )
        });

        match &lit {
            Literal::Datatype {
                literal,
                datatype_iri,
            } => {
                assert_eq!(
                    literal, expected_value,
                    "dp.value.boolean.{label}: literal text mismatch"
                );
                let dt_str: &str = datatype_iri;
                assert_eq!(
                    dt_str, xsd_boolean,
                    "dp.value.boolean.{label}: datatype IRI mismatch (expected xsd:boolean)"
                );
            }
            other => panic!("dp.value.boolean.{label}: expected Literal::Datatype, got {other:?}"),
        }
    }

    // Sanity: `r value :x` must still be ObjectHasValue.
    let obj_src = "Prefix: : <http://e/>\nClass: :A\n    SubClassOf: :r value :x\n";
    let (obj_ont, _) = read_str(obj_src).unwrap_or_else(|e| panic!("ce.value sanity: {e}"));
    let has_object_has_value = obj_ont.iter().any(|ac| {
        matches!(&ac.component, Component::SubClassOf(SubClassOf { sup, .. })
            if matches!(sup, ClassExpression::ObjectHasValue { .. }))
    });
    assert!(
        has_object_has_value,
        "ce.value sanity: expected ObjectHasValue for ':r value :x', got:\n{}",
        obj_ont
            .iter()
            .map(|ac| format!("{:?}", ac.component))
            .collect::<Vec<_>>()
            .join("\n")
    );

    // Sanity: typed integer `p value "5"^^xsd:integer` still → DataHasValue.
    let int_src = concat!(
        "Prefix: : <http://e/>\n",
        "Prefix: xsd: <http://www.w3.org/2001/XMLSchema#>\n",
        "Class: :A\n    SubClassOf: :p value \"5\"^^xsd:integer\n"
    );
    let (int_ont, _) = read_str(int_src).unwrap_or_else(|e| panic!("dp.value.int sanity: {e}"));
    let has_data_has_value_int = int_ont.iter().any(|ac| {
        matches!(&ac.component, Component::SubClassOf(SubClassOf { sup, .. })
            if matches!(sup, ClassExpression::DataHasValue { .. }))
    });
    assert!(
        has_data_has_value_int,
        "dp.value.int sanity: expected DataHasValue for '\"5\"^^xsd:integer'"
    );
}

// ---------------------------------------------------------------------------
// Matrix runner
// ---------------------------------------------------------------------------

#[derive(Debug)]
pub struct Row {
    pub id: String,
    pub read_ok: bool,
    pub write_ok: bool,
    pub roundtrip_ok: bool,
    pub residual: Residual,
    pub note: String,
}

pub fn run_case(c: &Case) -> Row {
    let mut note = String::new();
    let (read_ok, ont_pm) = match read_str(c.omn) {
        Ok(op) => (true, Some(op)),
        Err(e) => {
            note = e.lines().next().unwrap_or("").to_string();
            (false, None)
        }
    };
    let mut write_ok = false;
    let mut roundtrip_ok = false;
    if let Some((ont, pm)) = &ont_pm {
        if !c.expect_debug_contains.is_empty() {
            let hit = ont
                .iter()
                .any(|ac| format!("{:?}", ac.component).contains(c.expect_debug_contains));
            if !hit {
                note = format!("expected {} in components", c.expect_debug_contains);
            }
        }
        let rendered =
            std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| write_str(ont, pm)));
        if let Ok(text) = rendered {
            write_ok = true;
            if let Ok((ont2, _)) = read_str(&text) {
                roundtrip_ok = components_sorted(ont) == components_sorted(&ont2);
            }
        }
    }
    Row {
        id: c.id.into(),
        read_ok,
        write_ok,
        roundtrip_ok,
        residual: c.residual,
        note,
    }
}

// ---------------------------------------------------------------------------
// Main test
// ---------------------------------------------------------------------------

#[test]
fn construct_matrix_has_no_unexpected_failures() {
    let mut failures = Vec::new();
    for c in CASES {
        let row = run_case(c);
        println!("{:?}", row);
        // Every arm must be a SPECIFIC assertion — no unconditional `true`.
        // If a residual row's actual behavior differs from the expectation,
        // fix the tag/expectation in the CASES table, not this match.
        let ok = match c.residual {
            Residual::None => {
                // Fully supported: must parse, pass debug check, and round-trip.
                row.read_ok && row.note.is_empty() && row.roundtrip_ok
            }
            Residual::SwrlRule => {
                // SWRL `Rule:` is now fully supported: it parses into a Rule
                // component (body -> head) and the writer emits native `Rule:`
                // syntax, so it round-trips.
                row.read_ok && row.roundtrip_ok
            }
            Residual::BareNameNeedsPrefix => {
                // Bare local name without default prefix is not lexable.
                !row.read_ok
            }
            Residual::NestedAnnotationDropped => {
                // Parses successfully; nesting silently dropped (model limit).
                row.read_ok
            }
            Residual::HasKeyObjectDataConflation => {
                // HasKey key IRI without a DataProperty: declaration — falls back
                // to ObjectPropertyExpression; parses and round-trips stably
                // (as an object key, which is the documented undeclared tail).
                row.read_ok && row.roundtrip_ok
            }
            Residual::ComplexLhsGci => {
                // The `# General axioms` block is skipped by the reader
                // (returns Ok, warning printed).  Document parses, but the
                // complex axiom is absent from components — demonstrated by
                // `expect_debug_contains` not matching → note is non-empty.
                row.read_ok && !row.note.is_empty()
            }
        };
        if !ok {
            failures.push(format!("{:?}", row));
        }
    }
    assert!(
        failures.is_empty(),
        "unexpected construct failures:\n{}",
        failures.join("\n")
    );
}

// ---------------------------------------------------------------------------
// FIX-9: declaration pre-pass canaries
//
// These tests verify that the pre-pass actually flips the correct types,
// not merely that a document "parses" (silent no-op guard).
// ---------------------------------------------------------------------------

/// Canary A: declared data-property key flips to PropertyExpression::DataProperty.
/// NEGATIVES-FIRST: would fail if the lookup used a different IRI form (keying mismatch).
#[test]
fn decl_prepass_haskey_data_key_flips() {
    use horned_owl::model::{Component, DataProperty, PropertyExpression};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "DataProperty: :p\n",
        "Class: :A\n",
        "    HasKey: :p\n",
    );
    let (ont, _) = read_str(src)
        .unwrap_or_else(|e| panic!("decl_prepass_haskey_data_key_flips: parse failed: {e}"));

    let key_iri = "http://e/p";

    // There must be a HasKey component.
    let haskey = ont
        .iter()
        .find_map(|ac| {
            if let Component::HasKey(hk) = &ac.component {
                Some(hk.clone())
            } else {
                None
            }
        })
        .unwrap_or_else(|| {
            let debug: Vec<_> = ont.iter().map(|ac| format!("{:?}", ac.component)).collect();
            panic!(
                "decl_prepass_haskey_data_key_flips: no HasKey component:\n{}",
                debug.join("\n")
            )
        });

    // The key MUST be a DataProperty, NOT an ObjectPropertyExpression.
    assert_eq!(haskey.vpe.len(), 1, "expected exactly 1 key");
    match &haskey.vpe[0] {
        PropertyExpression::DataProperty(DataProperty(iri)) => {
            let iri_str: &str = iri;
            assert_eq!(iri_str, key_iri, "data key IRI mismatch (keying error?)");
        }
        PropertyExpression::ObjectPropertyExpression(ope) => {
            panic!(
                "decl_prepass_haskey_data_key_flips: key was ObjectPropertyExpression({ope:?}), \
                 expected DataProperty — likely a keying mismatch in the pre-pass lookup"
            );
        }
        other => panic!("unexpected property expression: {other:?}"),
    }
}

/// Canary B (guard): undeclared key stays ObjectPropertyExpression.
#[test]
fn decl_prepass_haskey_undeclared_stays_object() {
    use horned_owl::model::{Component, PropertyExpression};

    let src = "Prefix: : <http://e/>\nClass: :A\n    HasKey: :p\n";
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!("decl_prepass_haskey_undeclared_stays_object: parse failed: {e}")
    });

    let hk = ont
        .iter()
        .find_map(|ac| {
            if let Component::HasKey(hk) = &ac.component {
                Some(hk.clone())
            } else {
                None
            }
        })
        .expect("expected a HasKey");

    assert!(
        matches!(&hk.vpe[0], PropertyExpression::ObjectPropertyExpression(_)),
        "undeclared key must stay ObjectPropertyExpression, got {:?}",
        hk.vpe[0]
    );
}

/// Canary B2 (guard): declared ObjectProperty key stays ObjectPropertyExpression.
#[test]
fn decl_prepass_haskey_declared_object_stays_object() {
    use horned_owl::model::{Component, PropertyExpression};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "ObjectProperty: :q\n",
        "Class: :A\n",
        "    HasKey: :q\n",
    );
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!("decl_prepass_haskey_declared_object_stays_object: parse failed: {e}")
    });

    let hk = ont
        .iter()
        .find_map(|ac| {
            if let Component::HasKey(hk) = &ac.component {
                Some(hk.clone())
            } else {
                None
            }
        })
        .expect("expected a HasKey");

    assert!(
        matches!(&hk.vpe[0], PropertyExpression::ObjectPropertyExpression(_)),
        "ObjectProperty-declared key must stay ObjectPropertyExpression, got {:?}",
        hk.vpe[0]
    );
}

/// Canary C: EquivalentProperties over two declared data properties → EquivalentDataProperties.
#[test]
fn decl_prepass_misc_equiv_data_props_flips() {
    use horned_owl::model::Component;

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "DataProperty: :p\n",
        "DataProperty: :q\n",
        "EquivalentProperties: :p , :q\n",
    );
    let (ont, _) = read_str(src)
        .unwrap_or_else(|e| panic!("decl_prepass_misc_equiv_data_props_flips: parse failed: {e}"));

    let has_equiv_dp = ont
        .iter()
        .any(|ac| matches!(&ac.component, Component::EquivalentDataProperties(_)));
    let has_equiv_op = ont
        .iter()
        .any(|ac| matches!(&ac.component, Component::EquivalentObjectProperties(_)));

    assert!(
        has_equiv_dp,
        "expected EquivalentDataProperties, not present"
    );
    assert!(
        !has_equiv_op,
        "EquivalentObjectProperties must NOT be present when all members are declared data"
    );
}

/// Canary D: DisjointProperties over two declared data properties → DisjointDataProperties.
#[test]
fn decl_prepass_misc_disjoint_data_props_flips() {
    use horned_owl::model::Component;

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "DataProperty: :p\n",
        "DataProperty: :q\n",
        "DisjointProperties: :p , :q\n",
    );
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!("decl_prepass_misc_disjoint_data_props_flips: parse failed: {e}")
    });

    let has_disj_dp = ont
        .iter()
        .any(|ac| matches!(&ac.component, Component::DisjointDataProperties(_)));
    let has_disj_op = ont
        .iter()
        .any(|ac| matches!(&ac.component, Component::DisjointObjectProperties(_)));

    assert!(has_disj_dp, "expected DisjointDataProperties, not present");
    assert!(
        !has_disj_op,
        "DisjointObjectProperties must NOT be present when all members are declared data"
    );
}

/// Canary E (guard): mixed list (one data, one undeclared) stays EquivalentObjectProperties.
#[test]
fn decl_prepass_misc_mixed_list_stays_object() {
    use horned_owl::model::Component;

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "DataProperty: :p\n",
        // :r is NOT declared → mixed list → stays object
        "EquivalentProperties: :p , :r\n",
    );
    let (ont, _) = read_str(src)
        .unwrap_or_else(|e| panic!("decl_prepass_misc_mixed_list_stays_object: parse failed: {e}"));

    let has_equiv_op = ont
        .iter()
        .any(|ac| matches!(&ac.component, Component::EquivalentObjectProperties(_)));

    assert!(
        has_equiv_op,
        "mixed list must stay EquivalentObjectProperties, not flip"
    );
}

/// Canary F: restriction with declared-DataProperty property + declared-Datatype filler
/// → DataSomeValuesFrom.  The critical canary: proves type ACTUALLY FLIPS.
#[test]
fn decl_prepass_restriction_data_prop_declared_datatype_filler() {
    use horned_owl::model::{ClassExpression, Component, SubClassOf};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "DataProperty: :p\n",
        "Datatype: :MyDt\n",
        "Class: :A\n",
        "    SubClassOf: :p some :MyDt\n",
    );
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!("decl_prepass_restriction_data_prop_declared_datatype_filler: parse failed: {e}")
    });

    let ce = ont.iter().find_map(|ac| {
        if let Component::SubClassOf(SubClassOf { sup, .. }) = &ac.component {
            Some(sup.clone())
        } else {
            None
        }
    });

    match ce {
        Some(ClassExpression::DataSomeValuesFrom { .. }) => {} // correct
        Some(ClassExpression::ObjectSomeValuesFrom { .. }) => {
            panic!(
                "decl_prepass_restriction_data_prop_declared_datatype_filler: \
                 got ObjectSomeValuesFrom — pre-pass flip did not fire (keying error?)"
            );
        }
        Some(other) => panic!("unexpected CE: {other:?}"),
        None => panic!("no SubClassOf found"),
    }
}

/// Canary G (guard): restriction with declared-ObjectProperty + plain class filler
/// stays ObjectSomeValuesFrom.
#[test]
fn decl_prepass_restriction_declared_object_stays_object() {
    use horned_owl::model::{ClassExpression, Component, SubClassOf};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "ObjectProperty: :r\n",
        "Class: :A\n",
        "    SubClassOf: :r some :B\n",
    );
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!("decl_prepass_restriction_declared_object_stays_object: parse failed: {e}")
    });

    let ce = ont.iter().find_map(|ac| {
        if let Component::SubClassOf(SubClassOf { sup, .. }) = &ac.component {
            Some(sup.clone())
        } else {
            None
        }
    });

    match ce {
        Some(ClassExpression::ObjectSomeValuesFrom { .. }) => {} // correct
        Some(ClassExpression::DataSomeValuesFrom { .. }) => {
            panic!(
                "decl_prepass_restriction_declared_object_stays_object: \
                 got DataSomeValuesFrom — declared-object restriction was wrongly flipped"
            );
        }
        Some(other) => panic!("unexpected CE: {other:?}"),
        None => panic!("no SubClassOf found"),
    }
}

/// Canary H (guard): compound filler `:p some (:B and :C)` with declared DataProperty
/// stays ObjectSomeValuesFrom (compound fillers are never flipped).
#[test]
fn decl_prepass_restriction_compound_filler_not_flipped() {
    use horned_owl::model::{ClassExpression, Component, SubClassOf};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "DataProperty: :p\n",
        "Class: :A\n",
        "    SubClassOf: :p some (:B and :C)\n",
    );
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!("decl_prepass_restriction_compound_filler_not_flipped: parse failed: {e}")
    });

    // The compound filler (:B and :C) is an ObjectIntersectionOf.
    // The result should be ObjectSomeValuesFrom — the restriction filler
    // is not a bare class IRI, so it cannot be a DataRange.
    let ce = ont.iter().find_map(|ac| {
        if let Component::SubClassOf(SubClassOf { sup, .. }) = &ac.component {
            Some(sup.clone())
        } else {
            None
        }
    });

    match ce {
        Some(ClassExpression::ObjectSomeValuesFrom { .. }) => {} // correct
        Some(ClassExpression::DataSomeValuesFrom { .. }) => {
            panic!(
                "decl_prepass_restriction_compound_filler_not_flipped: \
                 compound filler was wrongly flipped to DataSomeValuesFrom"
            );
        }
        Some(other) => panic!("unexpected CE: {other:?}"),
        None => panic!("no SubClassOf found"),
    }
}

/// HasKey read→write→read round-trip: the `DataProperty: :p` declaration must be
/// re-emitted by the writer so the re-read can flip the key back to DataProperty.
#[test]
fn decl_prepass_haskey_data_roundtrip() {
    use horned_owl::model::{Component, PropertyExpression};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "DataProperty: :p\n",
        "Class: :A\n",
        "    HasKey: :p\n",
    );
    let (ont, pm) = read_str(src)
        .unwrap_or_else(|e| panic!("decl_prepass_haskey_data_roundtrip (pass 1): {e}"));

    // Write back to Manchester text.
    let text = write_str(&ont, &pm);

    // Re-read.
    let (ont2, _) = read_str(&text)
        .unwrap_or_else(|e| panic!("decl_prepass_haskey_data_roundtrip (pass 2): {e}"));

    // The re-read must also have a DataProperty key (not ObjectPropertyExpression).
    let hk2 = ont2
        .iter()
        .find_map(|ac| {
            if let Component::HasKey(hk) = &ac.component {
                Some(hk.clone())
            } else {
                None
            }
        })
        .expect("no HasKey after round-trip");

    assert!(
        matches!(&hk2.vpe[0], PropertyExpression::DataProperty(_)),
        "after round-trip, key must still be DataProperty; got {:?}",
        hk2.vpe[0]
    );
}

/// Canary I (guard): unqualified data-cardinality (`:p min 1`, no filler) with a
/// declared DataProperty does NOT flip to DataMinCardinality — the no-filler case
/// keeps ObjectMinCardinality with the default `owl:Thing` filler.  Flipping would
/// produce a wrong filler (`owl:Thing` instead of `rdfs:Literal`).
#[test]
fn decl_prepass_restriction_unqualified_card_not_flipped() {
    use horned_owl::model::{ClassExpression, Component, SubClassOf};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "DataProperty: :p\n",
        "Class: :A\n",
        "    SubClassOf: :p min 1\n",
    );
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!("decl_prepass_restriction_unqualified_card_not_flipped: parse failed: {e}")
    });

    let ce = ont.iter().find_map(|ac| {
        if let Component::SubClassOf(SubClassOf { sup, .. }) = &ac.component {
            Some(sup.clone())
        } else {
            None
        }
    });

    match ce {
        Some(ClassExpression::ObjectMinCardinality { .. }) => {} // correct
        Some(ClassExpression::DataMinCardinality { .. }) => {
            panic!(
                "decl_prepass_restriction_unqualified_card_not_flipped: \
                 unqualified :p min 1 was wrongly flipped to DataMinCardinality \
                 — this would use owl:Thing as the data range, which is wrong"
            );
        }
        Some(other) => panic!("unexpected CE: {other:?}"),
        None => panic!("no SubClassOf found"),
    }
}

// ---------------------------------------------------------------------------
// Isolating canaries for is_datatype (declared-Datatype filler, undeclared prop)
// ---------------------------------------------------------------------------

/// Canary I: `:r some :MyDt` where `:r` is NOT declared (prop_is_data = false)
/// but `:MyDt` IS declared as a Datatype.  The `is_datatype` path in the
/// restriction handler is the SOLE trigger: `prop_is_data` cannot mask it.
///
/// Asserts:
///   - result is `DataSomeValuesFrom`
///   - `dr` is `Datatype(:MyDt)` (the IRI payload is correct, not just the variant)
///
/// This canary was added because the existing
/// `decl_prepass_restriction_data_prop_declared_datatype_filler` canary also
/// declares `:p` as a DataProperty, so a broken `is_datatype` is masked by the
/// `prop_is_data` fallback.  Here, there is no such fallback.
#[test]
fn decl_prepass_restriction_datatype_filler_undeclared_prop() {
    use horned_owl::model::{ClassExpression, Component, DataRange, Datatype, SubClassOf};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "Datatype: :MyDt\n",
        "Class: :A\n",
        "    SubClassOf: :r some :MyDt\n",
    );
    // :r is intentionally NOT declared — prop_is_data stays false.
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!("decl_prepass_restriction_datatype_filler_undeclared_prop: parse failed: {e}")
    });

    let ce = ont.iter().find_map(|ac| {
        if let Component::SubClassOf(SubClassOf { sup, .. }) = &ac.component {
            Some(sup.clone())
        } else {
            None
        }
    });

    match ce {
        Some(ClassExpression::DataSomeValuesFrom { dr, .. }) => {
            // Strong assertion: the datatype IRI payload must be :MyDt.
            assert!(
                matches!(&dr, DataRange::Datatype(Datatype(iri)) if iri.as_ref() == "http://e/MyDt"),
                "decl_prepass_restriction_datatype_filler_undeclared_prop: \
                 DataSomeValuesFrom has wrong dr: {dr:?} (expected Datatype(http://e/MyDt))"
            );
        }
        Some(ClassExpression::ObjectSomeValuesFrom { .. }) => {
            panic!(
                "decl_prepass_restriction_datatype_filler_undeclared_prop: \
                 got ObjectSomeValuesFrom — is_datatype did not fire (is_datatype broken?)"
            );
        }
        Some(other) => panic!(
            "decl_prepass_restriction_datatype_filler_undeclared_prop: unexpected CE: {other:?}"
        ),
        None => {
            panic!("decl_prepass_restriction_datatype_filler_undeclared_prop: no SubClassOf found")
        }
    }
}

/// Canary I2: `:r exactly 1 :MyDt` where `:r` is NOT declared (prop_is_data = false)
/// but `:MyDt` IS declared as a Datatype.  The `exactly` cardinality arm uses the
/// same `bare_datatype_iri` path — this is an independent witness that `is_datatype`
/// is the sole trigger for qualified-cardinality flipping too.
///
/// Asserts:
///   - result is `DataExactCardinality { n: 1, .. }`
///   - `dr` is `Datatype(:MyDt)`
#[test]
fn decl_prepass_restriction_datatype_filler_undeclared_prop_exact_card() {
    use horned_owl::model::{ClassExpression, Component, DataRange, Datatype, SubClassOf};

    let src = concat!(
        "Prefix: : <http://e/>\n",
        "Datatype: :MyDt\n",
        "Class: :A\n",
        "    SubClassOf: :r exactly 1 :MyDt\n",
    );
    // :r is intentionally NOT declared.
    let (ont, _) = read_str(src).unwrap_or_else(|e| {
        panic!(
            "decl_prepass_restriction_datatype_filler_undeclared_prop_exact_card: parse failed: {e}"
        )
    });

    let ce = ont.iter().find_map(|ac| {
        if let Component::SubClassOf(SubClassOf { sup, .. }) = &ac.component {
            Some(sup.clone())
        } else {
            None
        }
    });

    match ce {
        Some(ClassExpression::DataExactCardinality { n, dr, .. }) => {
            assert_eq!(
                n, 1,
                "decl_prepass_restriction_datatype_filler_undeclared_prop_exact_card: \
                 expected cardinality 1, got {n}"
            );
            assert!(
                matches!(&dr, DataRange::Datatype(Datatype(iri)) if iri.as_ref() == "http://e/MyDt"),
                "decl_prepass_restriction_datatype_filler_undeclared_prop_exact_card: \
                 DataExactCardinality has wrong dr: {dr:?} (expected Datatype(http://e/MyDt))"
            );
        }
        Some(ClassExpression::ObjectExactCardinality { .. }) => {
            panic!(
                "decl_prepass_restriction_datatype_filler_undeclared_prop_exact_card: \
                 got ObjectExactCardinality — is_datatype did not fire (is_datatype broken?)"
            );
        }
        Some(other) => panic!(
            "decl_prepass_restriction_datatype_filler_undeclared_prop_exact_card: \
             unexpected CE: {other:?}"
        ),
        None => panic!(
            "decl_prepass_restriction_datatype_filler_undeclared_prop_exact_card: \
             no SubClassOf found"
        ),
    }
}
