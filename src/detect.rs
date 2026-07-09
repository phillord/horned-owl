use crate::model::Format;

pub fn detect(bytes: &[u8]) -> Format {
    // Strip a UTF-8 BOM and leading whitespace.
    let s = String::from_utf8_lossy(bytes);
    let s = s.strip_prefix('\u{feff}').unwrap_or(&s);
    let trimmed = s.trim_start();

    if trimmed.starts_with("<?xml") || trimmed.starts_with('<') {
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
    // Text syntaxes: first significant line.
    for line in trimmed.lines() {
        let l = line.trim_start();
        if l.is_empty() || l.starts_with('#') {
            continue;
        }
        if l.starts_with("Prefix:") || l.starts_with("Ontology:") {
            return Format::Omn;
        }
        if l.starts_with("Prefix(") || l.starts_with("Ontology(") {
            return Format::Ofn;
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
        assert_eq!(detect(b"format-version: 1.4\n[Term]"), Format::Unknown);
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
