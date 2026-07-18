//! OASIS XML Catalog parsing and resolution.
//!
//! See `docs/horned-catalog-plan.md` at the repository root for the
//! design rationale and supported-entry-type scope. This crate is
//! deliberately standalone: every public function that takes an
//! IRI-like value is bounded by `AsRef<str>`, not by any particular IRI
//! type, so it can be used with `horned-owl`'s `IRI<A>` or with a plain
//! `&str`/`String` equally.

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use quick_xml::Reader;
use quick_xml::events::{BytesStart, Event};
use quick_xml::name::QName;

/// Errors that can occur while parsing a catalog file.
#[derive(Debug, thiserror::Error)]
pub enum CatalogError {
    #[error("IO error reading catalog: {0}")]
    Io(#[from] std::io::Error),
    #[error("XML error parsing catalog: {0}")]
    Xml(#[from] quick_xml::Error),
    #[error("XML attribute error parsing catalog: {0}")]
    Attr(#[from] quick_xml::events::attributes::AttrError),
    #[error("malformed catalog: {0}")]
    Malformed(String),
}

/// A single problem found by [`Catalog::validate`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CatalogValidationError {
    /// The catalog file the offending entry came from (useful once
    /// `nextCatalog` chains are involved).
    pub catalog: PathBuf,
    /// Human-readable description of what's wrong.
    pub message: String,
}

#[derive(Debug, Clone)]
enum CatalogEntry {
    Uri { name: String, uri: String },
    RewriteUri { start: String, prefix: String },
    NextCatalog { path: PathBuf },
}

/// A parsed OASIS XML Catalog (the subset described in
/// `docs/horned-catalog-plan.md`: `uri`, `system`, `rewriteURI`,
/// `rewriteSystem`, `nextCatalog`, and `group` (flattened, `xml:base`
/// honoured). `public`/`delegate*` entries are not supported -- see the
/// design doc for why.
#[derive(Debug, Clone)]
pub struct Catalog {
    entries: Vec<CatalogEntry>,
    /// Directory this catalog's own relative paths are resolved
    /// against.
    base: PathBuf,
    /// The path this catalog itself was loaded from, if any -- used for
    /// cycle detection across `nextCatalog` chains and for
    /// [`CatalogValidationError::catalog`].
    source: Option<PathBuf>,
}

impl Catalog {
    /// Parse a catalog file from disk. Relative `uri`/`rewriteURI`
    /// targets are resolved against the catalog file's own parent
    /// directory.
    pub fn from_path(path: impl AsRef<Path>) -> Result<Catalog, CatalogError> {
        let path = path.as_ref();
        let xml = fs::read_to_string(path)?;
        let base = path
            .parent()
            .map(Path::to_path_buf)
            .unwrap_or_else(|| PathBuf::from("."));
        let mut catalog = Catalog::from_str(&xml, base)?;
        catalog.source = Some(path.to_path_buf());
        Ok(catalog)
    }

    /// Parse catalog XML already in memory. `base` is the directory
    /// relative `uri`/`rewriteURI` targets are resolved against
    /// (normally the directory the catalog file lives in).
    pub fn from_str(xml: &str, base: impl AsRef<Path>) -> Result<Catalog, CatalogError> {
        let entries = parse_entries(xml, base.as_ref())?;
        Ok(Catalog {
            entries,
            base: base.as_ref().to_path_buf(),
            source: None,
        })
    }

    /// Resolve `iri` to a local path using this catalog, chasing any
    /// `nextCatalog` entries if there's no direct match. Returns
    /// `None`, not an error, if nothing matches -- callers are expected
    /// to fall back to their own resolution strategy.
    pub fn resolve(&self, iri: impl AsRef<str>) -> Option<PathBuf> {
        let mut visited = HashSet::new();
        if let Some(source) = &self.source {
            visited.insert(source.clone());
        }
        self.resolve_inner(iri.as_ref(), &mut visited)
    }

    fn resolve_inner(&self, iri: &str, visited: &mut HashSet<PathBuf>) -> Option<PathBuf> {
        // Exact uri/system matches first. `uri` was already fully
        // resolved against the entry's own effective base (including
        // any `file:`-URI handling) back in `push_entry` -- no base to
        // re-apply here.
        for entry in &self.entries {
            if let CatalogEntry::Uri { name, uri } = entry
                && name == iri
            {
                return Some(PathBuf::from(uri));
            }
        }

        // Then longest-prefix rewriteURI/rewriteSystem match. `prefix`
        // is likewise already fully resolved; only `rest` (the tail of
        // the IRI past the matched prefix) is a genuinely new path
        // component to append.
        let mut best: Option<(&str, &str)> = None;
        for entry in &self.entries {
            if let CatalogEntry::RewriteUri { start, prefix } = entry
                && iri.starts_with(start.as_str())
                && best.is_none_or(|(b, _)| start.len() > b.len())
            {
                best = Some((start, prefix));
            }
        }
        if let Some((start, prefix)) = best {
            let rest = &iri[start.len()..];
            let rest = rest.strip_prefix('/').unwrap_or(rest);
            return Some(PathBuf::from(prefix).join(rest));
        }

        // Then nextCatalog delegation, in document order.
        for entry in &self.entries {
            if let CatalogEntry::NextCatalog { path } = entry {
                let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
                if !visited.insert(canonical) {
                    continue; // already chased this catalog -- cycle guard
                }
                if let Ok(next) = Catalog::from_path(path)
                    && let Some(resolved) = next.resolve_inner(iri, visited)
                {
                    return Some(resolved);
                }
            }
        }

        None
    }

    /// Check that every concrete target this catalog names actually
    /// exists on disk: `uri`/`system` targets exactly, `rewriteURI`/
    /// `rewriteSystem` prefix directories, and `nextCatalog` targets
    /// (recursively validating the chained catalog too). Returns every
    /// problem found, not just the first.
    pub fn validate(&self) -> Vec<CatalogValidationError> {
        let mut visited = HashSet::new();
        if let Some(source) = &self.source {
            visited.insert(source.clone());
        }
        self.validate_inner(&mut visited)
    }

    fn validate_inner(&self, visited: &mut HashSet<PathBuf>) -> Vec<CatalogValidationError> {
        let mut errors = Vec::new();
        let here = self
            .source
            .clone()
            .unwrap_or_else(|| self.base.join("<in-memory catalog>"));

        for entry in &self.entries {
            match entry {
                CatalogEntry::Uri { name, uri } => {
                    let target = PathBuf::from(uri);
                    if !target.exists() {
                        errors.push(CatalogValidationError {
                            catalog: here.clone(),
                            message: format!(
                                "entry for '{name}' points at '{}', which does not exist",
                                target.display()
                            ),
                        });
                    }
                }
                CatalogEntry::RewriteUri { start, prefix } => {
                    let target = PathBuf::from(prefix);
                    if !target.exists() {
                        errors.push(CatalogValidationError {
                            catalog: here.clone(),
                            message: format!(
                                "rewrite rule for '{start}' points at '{}', which does not exist",
                                target.display()
                            ),
                        });
                    }
                }
                CatalogEntry::NextCatalog { path } => {
                    if !path.exists() {
                        errors.push(CatalogValidationError {
                            catalog: here.clone(),
                            message: format!(
                                "nextCatalog points at '{}', which does not exist",
                                path.display()
                            ),
                        });
                        continue;
                    }
                    let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
                    if !visited.insert(canonical) {
                        continue; // cycle guard
                    }
                    match Catalog::from_path(path) {
                        Ok(next) => errors.extend(next.validate_inner(visited)),
                        Err(e) => errors.push(CatalogValidationError {
                            catalog: here.clone(),
                            message: format!(
                                "nextCatalog '{}' failed to parse: {e}",
                                path.display()
                            ),
                        }),
                    }
                }
            }
        }

        errors
    }
}

/// Resolve a catalog entry's target attribute (a `uri=`/`rewritePrefix=`/
/// `catalog=` value) against `base`. Real-world catalogs (confirmed
/// against Protege-generated `catalog-v001.xml` files, which use the
/// same template as `OWLZipSaver.catalogIndex()` in the OWL API) can use
/// an absolute `file:` URI here instead of a plain relative path, e.g.
/// `uri="file:/home/user/ontology/imports/bfo.owl"`. `Path::join` does
/// not recognise `file:...` as absolute (it doesn't start with `/`), so
/// joining it against `base` naively produces a nonsense concatenated
/// path -- this strips a `file://` or `file:` prefix first, in which
/// case the remainder is used as an absolute path outright, ignoring
/// `base` (matching what the URI actually means). A plain relative or
/// already-absolute path (no `file:` prefix) is joined against `base` as
/// normal -- `Path::join` already handles a plain absolute path
/// correctly on its own (it replaces `base` rather than concatenating).
fn resolve_target(base: &Path, target: &str) -> PathBuf {
    if let Some(rest) = target.strip_prefix("file://") {
        return PathBuf::from(rest);
    }
    if let Some(rest) = target.strip_prefix("file:") {
        return PathBuf::from(rest);
    }
    base.join(target)
}

/// Strip any namespace prefix off a qualified XML tag/attribute name --
/// catalog files are conventionally written with the OASIS namespace as
/// the default namespace (no prefix), but tolerate a prefixed form too
/// rather than erroring, since being lenient here costs nothing and
/// catalogs are hand-edited more often than most XML.
fn local_name(name: QName) -> String {
    let raw = name.as_ref();
    let local = raw.rsplit(|&b| b == b':').next().unwrap_or(raw);
    String::from_utf8_lossy(local).into_owned()
}

/// Read a start/empty tag's local name, its attributes (also
/// local-named), and the `xml:base`-adjusted base directory that
/// applies to it.
fn tag_name_attrs_base(
    reader: &Reader<&[u8]>,
    e: &BytesStart,
    current_base: &Path,
) -> Result<(String, HashMap<String, String>, PathBuf), CatalogError> {
    let tag = local_name(e.name());
    let mut attrs = HashMap::new();
    let mut xml_base_override = None;
    for attr in e.attributes() {
        let attr = attr?;
        let key = local_name(attr.key);
        let value = attr
            .decode_and_unescape_value(reader.decoder())?
            .into_owned();
        if key == "base" {
            xml_base_override = Some(PathBuf::from(&value));
        }
        attrs.insert(key, value);
    }
    let effective_base = match xml_base_override {
        Some(b) => current_base.join(b),
        None => current_base.to_path_buf(),
    };
    Ok((tag, attrs, effective_base))
}

/// Turn one `<uri>`/`<system>`/`<rewriteURI>`/`<rewriteSystem>`/
/// `<nextCatalog>` tag into a `CatalogEntry`, pushing it onto `entries`.
/// Any other tag (`<catalog>` itself, `<group>`, or an unsupported entry
/// type such as `<public>`) is silently ignored -- see the scope table
/// in `docs/horned-catalog-plan.md`.
fn push_entry(
    tag: &str,
    attrs: &HashMap<String, String>,
    base: &Path,
    entries: &mut Vec<CatalogEntry>,
) -> Result<(), CatalogError> {
    let get = |key: &str| -> Result<String, CatalogError> {
        attrs
            .get(key)
            .cloned()
            .ok_or_else(|| CatalogError::Malformed(format!("<{tag}> missing '{key}'")))
    };

    match tag {
        "uri" => entries.push(CatalogEntry::Uri {
            name: get("name")?,
            uri: resolve_target(base, &get("uri")?)
                .to_string_lossy()
                .into_owned(),
        }),
        "system" => entries.push(CatalogEntry::Uri {
            name: get("systemId")?,
            uri: resolve_target(base, &get("uri")?)
                .to_string_lossy()
                .into_owned(),
        }),
        "rewriteuri" => entries.push(CatalogEntry::RewriteUri {
            start: get("uriStartString")?,
            prefix: resolve_target(base, &get("rewritePrefix")?)
                .to_string_lossy()
                .into_owned(),
        }),
        "rewritesystem" => entries.push(CatalogEntry::RewriteUri {
            start: get("systemIdStartString")?,
            prefix: resolve_target(base, &get("rewritePrefix")?)
                .to_string_lossy()
                .into_owned(),
        }),
        "nextcatalog" => entries.push(CatalogEntry::NextCatalog {
            path: resolve_target(base, &get("catalog")?),
        }),
        _ => {}
    }
    Ok(())
}

fn parse_entries(xml: &str, base: &Path) -> Result<Vec<CatalogEntry>, CatalogError> {
    let mut reader = Reader::from_str(xml);
    reader.config_mut().trim_text(true);

    let mut entries = Vec::new();
    // Stack of `xml:base` overrides for nested `<group>` elements; the
    // innermost applicable base wins. Only pushed for non-empty
    // `<group>` elements (an empty one has no children to apply to).
    let mut base_stack: Vec<PathBuf> = vec![base.to_path_buf()];
    let mut buf = Vec::new();

    loop {
        let event = reader.read_event_into(&mut buf)?;
        match event {
            Event::Eof => break,
            Event::Start(e) => {
                let current_base = base_stack
                    .last()
                    .cloned()
                    .unwrap_or_else(|| base.to_path_buf());
                let (tag, attrs, effective_base) = tag_name_attrs_base(&reader, &e, &current_base)?;
                push_entry(
                    &tag.to_ascii_lowercase(),
                    &attrs,
                    &effective_base,
                    &mut entries,
                )?;
                if tag.eq_ignore_ascii_case("group") {
                    base_stack.push(effective_base);
                }
            }
            Event::Empty(e) => {
                let current_base = base_stack
                    .last()
                    .cloned()
                    .unwrap_or_else(|| base.to_path_buf());
                let (tag, attrs, effective_base) = tag_name_attrs_base(&reader, &e, &current_base)?;
                push_entry(
                    &tag.to_ascii_lowercase(),
                    &attrs,
                    &effective_base,
                    &mut entries,
                )?;
                // Empty elements never push onto base_stack: a
                // self-closing <group/> has no children to apply
                // xml:base to.
            }
            Event::End(e)
                if local_name(e.name()).eq_ignore_ascii_case("group") && base_stack.len() > 1 =>
            {
                base_stack.pop();
            }
            _ => {}
        }
        buf.clear();
    }

    Ok(entries)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fs;

    fn write(dir: &Path, name: &str, content: &str) -> PathBuf {
        let path = dir.join(name);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(&path, content).unwrap();
        path
    }

    #[test]
    fn resolve_simple_uri_entry() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "foo.owl", "# not real OWL, just needs to exist");
        let catalog_xml = r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <uri name="http://example.org/foo.owl" uri="foo.owl"/>
</catalog>"#;
        let catalog = Catalog::from_str(catalog_xml, dir.path()).unwrap();
        assert_eq!(
            catalog.resolve("http://example.org/foo.owl"),
            Some(dir.path().join("foo.owl"))
        );
        assert_eq!(
            catalog.resolve("http://example.org/no-such-entry.owl"),
            None
        );
    }

    #[test]
    fn resolve_from_path_uses_catalog_directory_as_base() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "sub/foo.owl", "irrelevant");
        let catalog_path = write(
            dir.path(),
            "catalog-v001.xml",
            r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <uri name="http://example.org/foo.owl" uri="sub/foo.owl"/>
</catalog>"#,
        );
        let catalog = Catalog::from_path(&catalog_path).unwrap();
        assert_eq!(
            catalog.resolve("http://example.org/foo.owl"),
            Some(dir.path().join("sub/foo.owl"))
        );
    }

    #[test]
    fn rewrite_uri_longest_prefix_wins() {
        let dir = tempfile::tempdir().unwrap();
        let catalog_xml = r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <rewriteURI uriStartString="http://example.org/" rewritePrefix="general"/>
  <rewriteURI uriStartString="http://example.org/specific/" rewritePrefix="special"/>
</catalog>"#;
        let catalog = Catalog::from_str(catalog_xml, dir.path()).unwrap();
        assert_eq!(
            catalog.resolve("http://example.org/specific/foo.owl"),
            Some(dir.path().join("special").join("foo.owl"))
        );
        assert_eq!(
            catalog.resolve("http://example.org/other/foo.owl"),
            Some(dir.path().join("general").join("other/foo.owl"))
        );
    }

    #[test]
    fn next_catalog_is_chased_on_miss() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "bar.owl", "irrelevant");
        let inner_path = write(
            dir.path(),
            "inner-catalog.xml",
            r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <uri name="http://example.org/bar.owl" uri="bar.owl"/>
</catalog>"#,
        );
        let outer_xml = format!(
            r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <nextCatalog catalog="{}"/>
</catalog>"#,
            inner_path.display()
        );
        let outer = Catalog::from_str(&outer_xml, dir.path()).unwrap();
        assert_eq!(
            outer.resolve("http://example.org/bar.owl"),
            Some(dir.path().join("bar.owl"))
        );
    }

    #[test]
    fn next_catalog_cycle_does_not_hang() {
        let dir = tempfile::tempdir().unwrap();
        let a_path = dir.path().join("a.xml");
        let b_path = dir.path().join("b.xml");
        write(
            dir.path(),
            "a.xml",
            &format!(
                r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <nextCatalog catalog="{}"/>
</catalog>"#,
                b_path.display()
            ),
        );
        write(
            dir.path(),
            "b.xml",
            &format!(
                r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <nextCatalog catalog="{}"/>
</catalog>"#,
                a_path.display()
            ),
        );
        let catalog = Catalog::from_path(&a_path).unwrap();
        // Should terminate (not hang) and simply find nothing.
        assert_eq!(catalog.resolve("http://example.org/nothing.owl"), None);
    }

    #[test]
    fn validate_reports_dangling_uri_target() {
        let dir = tempfile::tempdir().unwrap();
        let catalog_xml = r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <uri name="http://example.org/missing.owl" uri="missing.owl"/>
</catalog>"#;
        let catalog = Catalog::from_str(catalog_xml, dir.path()).unwrap();
        let errors = catalog.validate();
        assert_eq!(errors.len(), 1);
        assert!(errors[0].message.contains("missing.owl"));
    }

    #[test]
    fn validate_passes_when_target_exists() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "present.owl", "irrelevant");
        let catalog_xml = r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <uri name="http://example.org/present.owl" uri="present.owl"/>
</catalog>"#;
        let catalog = Catalog::from_str(catalog_xml, dir.path()).unwrap();
        assert!(catalog.validate().is_empty());
    }

    #[test]
    fn group_xml_base_applies_to_nested_entries() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "nested/foo.owl", "irrelevant");
        let catalog_xml = r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <group xml:base="nested">
    <uri name="http://example.org/foo.owl" uri="foo.owl"/>
  </group>
</catalog>"#;
        let catalog = Catalog::from_str(catalog_xml, dir.path()).unwrap();
        assert_eq!(
            catalog.resolve("http://example.org/foo.owl"),
            Some(dir.path().join("nested").join("foo.owl"))
        );
    }

    #[test]
    fn missing_required_attribute_is_malformed_error() {
        let dir = tempfile::tempdir().unwrap();
        let catalog_xml = r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <uri uri="foo.owl"/>
</catalog>"#;
        let result = Catalog::from_str(catalog_xml, dir.path());
        assert!(matches!(result, Err(CatalogError::Malformed(_))));
    }

    #[test]
    fn accepts_a_plain_str_or_string_as_well_as_anything_asref_str() {
        let dir = tempfile::tempdir().unwrap();
        write(dir.path(), "foo.owl", "irrelevant");
        let catalog_xml = r#"<?xml version="1.0"?>
<catalog xmlns="urn:oasis:names:tc:entity:xmlns:xml:catalog">
  <uri name="http://example.org/foo.owl" uri="foo.owl"/>
</catalog>"#;
        let catalog = Catalog::from_str(catalog_xml, dir.path()).unwrap();

        // &str
        assert!(catalog.resolve("http://example.org/foo.owl").is_some());
        // String
        assert!(
            catalog
                .resolve(String::from("http://example.org/foo.owl"))
                .is_some()
        );
    }
}
