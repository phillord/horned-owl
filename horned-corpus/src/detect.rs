use crate::model::Format;

pub fn detect(bytes: &[u8]) -> Format {
    // Strip a UTF-8 BOM and leading whitespace.
    let s = String::from_utf8_lossy(bytes);
    let s = s.strip_prefix('\u{feff}').unwrap_or(&s);
    let trimmed = s.trim_start();

    if trimmed.starts_with('<') {
        // N-Triples / IRI-subject Turtle: a full-IRI subject and predicate on
        // the first line (`<...> <...>`). Checked before XML disambiguation
        // because it also starts with `<`. RDF/XML never matches: it opens
        // with `<?xml` / `<!…` or a single root element tag, not two
        // space-separated angle-bracket IRIs.
        if !trimmed.starts_with("<?")
            && !trimmed.starts_with("<!")
            && trimmed.lines().next().is_some_and(|l| l.contains("> <"))
        {
            return Format::Turtle;
        }
        // Find the first XML element name after prologue / comments / doctype.
        if let Some(root) = first_xml_element(trimmed) {
            let local = root.rsplit(':').next().unwrap_or(root);
            if local.eq_ignore_ascii_case("RDF") {
                return Format::RdfXml;
            }
            if local.eq_ignore_ascii_case("Ontology") {
                return Format::OwlXml;
            }
        }
        return Format::Unknown;
    }
    // First significant line drives text-syntax detection (skip blanks and
    // `#` comments, which Turtle/OFN/OMN all allow).
    for line in trimmed.lines() {
        let l = line.trim_start();
        if l.is_empty() || l.starts_with('#') {
            continue;
        }
        // Turtle / N3: `@prefix`/`@base` directives (case-insensitive keyword).
        let lower = l.to_ascii_lowercase();
        if lower.starts_with("@prefix") || lower.starts_with("@base") {
            return Format::Turtle;
        }
        // N-Triples / IRI-subject Turtle: a full-IRI subject followed by a
        // full-IRI predicate on the first line (`<...> <...>`). RDF/XML never
        // matches this: it opens with `<?xml`/`<!`/`<rdf:RDF …>` (a single
        // element tag), not two space-separated angle-bracket IRIs.
        if l.starts_with('<') && !l.starts_with("<?") && !l.starts_with("<!") && l.contains("> <") {
            return Format::Turtle;
        }
        if l.starts_with("Prefix:") || l.starts_with("Ontology:") {
            return Format::Omn;
        }
        if l.starts_with("Prefix(") || l.starts_with("Ontology(") {
            return Format::Ofn;
        }
        // OBO flat-file: a header clause tag (`format-version:` is the
        // conventional first line, but not the only header tag), or a
        // stanza header for a header-less document.
        if l.starts_with("format-version:") || l.starts_with('[') {
            return Format::Obo;
        }
        break;
    }
    Format::Unknown
}

// Return the tag name of the first real element, skipping <?...?>, <!-- -->, <!DOCTYPE ...>.
fn first_xml_element(s: &str) -> Option<&str> {
    let mut rest = s;
    loop {
        let lt = rest.find('<')?;
        rest = &rest[lt..];
        if rest.starts_with("<?") {
            let end = rest.find("?>")? + 2;
            rest = &rest[end..];
            continue;
        }
        if rest.starts_with("<!--") {
            let end = rest.find("-->")? + 3;
            rest = &rest[end..];
            continue;
        }
        if rest.starts_with("<!") {
            let end = rest.find('>')? + 1;
            rest = &rest[end..];
            continue;
        }
        // real element: <name ...>
        let after = &rest[1..];
        let name: &str = after
            .split(|c: char| c.is_whitespace() || c == '>' || c == '/')
            .next()?;
        return Some(name);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::Format;
    #[test]
    fn sniffs_each_format() {
        assert_eq!(detect(b"<?xml version=\"1.0\"?>\n<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"), Format::RdfXml);
        assert_eq!(
            detect(b"<?xml version=\"1.0\"?>\n<Ontology xmlns=\"http://www.w3.org/2002/07/owl#\">"),
            Format::OwlXml
        );
        assert_eq!(
            detect(b"Prefix(:=<http://ex/>)\nOntology(<http://ex/o>)"),
            Format::Ofn
        );
        assert_eq!(
            detect(b"Prefix: : <http://ex/>\nOntology: <http://ex/o>"),
            Format::Omn
        );
        assert_eq!(detect(b"format-version: 1.4\n[Term]"), Format::Obo);
    }

    #[test]
    fn sniffs_obo_without_a_header() {
        assert_eq!(
            detect(b"[Term]\nid: GO:0008150\nname: biological_process\n"),
            Format::Obo
        );
    }
    #[test]
    fn sniffs_turtle_and_ntriples() {
        // @prefix / @base directives
        assert_eq!(
            detect(b"@prefix skos: <http://www.w3.org/2004/02/skos/core#> .\n"),
            Format::Turtle
        );
        assert_eq!(
            detect(b"@base <http://ex/> .\n:A a owl:Class ."),
            Format::Turtle
        );
        // N-Triples: full-IRI subject + predicate (starts with `<`)
        assert_eq!(
            detect(b"<http://ex/s> <http://ex/p> <http://ex/o> .\n"),
            Format::Turtle
        );
        // Turtle content behind a lying .owl extension is still detected by content
        assert_eq!(
            detect(b"# a comment\n@prefix : <http://ex/> ."),
            Format::Turtle
        );
        // RDF/XML must NOT be misread as Turtle
        assert_eq!(
            detect(b"<?xml version=\"1.0\"?>\n<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"),
            Format::RdfXml
        );
    }

    #[test]
    fn handles_bom_and_comments() {
        assert_eq!(
            detect("\u{feff}Ontology: <http://ex/o>".as_bytes()),
            Format::Omn
        );
        assert_eq!(detect(b"<?xml version=\"1.0\"?><!-- c --><rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"), Format::RdfXml);
    }
}
