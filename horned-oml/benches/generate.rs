//! Synthetic OML of a chosen size, for benchmarks and tool comparisons.

pub const VOCABULARY_IRI: &str = "http://example.com/big/vocab#";
pub const DESCRIPTION_IRI: &str = "http://example.com/big/data#";

/// A vocabulary with `n` concepts, relation entities and rules.
pub fn vocabulary(n: usize) -> String {
    let mut s = format!(
        "vocabulary <{VOCABULARY_IRI}> as v {{\n\n\textends <http://www.w3.org/2001/XMLSchema#> as xsd\n\n\
         \taspect Thing\n\n\
         \tscalar property id [\n\t\tdomain Thing\n\t\trange xsd:string\n\t\tfunctional\n\t]\n\n\
         \tscalar property age [\n\t\tdomain Thing\n\t\trange xsd:integer\n\t]\n\n"
    );
    for i in 0..n {
        let sup = if i == 0 {
            "Thing".to_string()
        } else {
            format!("C{}", i / 2)
        };
        s.push_str(&format!(
            "\tconcept C{i} < {sup} [\n\t\trestricts age to min 0 xsd:integer\n\t]\n\n\
             \trelation entity E{i} [\n\t\tfrom C{i}\n\t\tto C{i}\n\t\tforward e{i}\n\t\treverse eBy{i}\n\t]\n\n\
             \trule R{i} [\n\t\tC{i}(p) & e{i}(p, q) -> C{i}(q)\n\t]\n\n"
        ));
    }
    s.push_str("}\n");
    s
}

/// A description with `m` instances and relation instances over a
/// vocabulary of `n` concepts.
pub fn description(n: usize, m: usize) -> String {
    let mut s =
        format!("description <{DESCRIPTION_IRI}> as d {{\n\n\tuses <{VOCABULARY_IRI}> as v\n\n");
    for k in 0..m {
        s.push_str(&format!(
            "\tinstance i{k} : v:C{} [\n\t\tv:id \"i{k}\"\n\t\tv:age {}\n\t]\n\n",
            k % n,
            k % 90
        ));
    }
    for k in 0..m.saturating_sub(n) {
        s.push_str(&format!(
            "\trelation instance r{k} : v:E{} [\n\t\tfrom i{k}\n\t\tto i{}\n\t]\n\n",
            k % n,
            k + n
        ));
    }
    s.push_str("}\n");
    s
}
