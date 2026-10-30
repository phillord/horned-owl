//! Finding imported ontologies through an OASIS XML catalog.

use std::fs;
use std::path::{Path, PathBuf};

use horned_catalog::{Catalog, CatalogError};

use crate::reader::Resolver;

/// The catalog next to `path` or in the nearest directory above it.
///
/// OML projects keep a `catalog.xml` at their root.
pub fn find_catalog(path: &Path) -> Option<PathBuf> {
    let start = path.canonicalize().ok()?;
    start
        .ancestors()
        .skip(1)
        .map(|dir| dir.join("catalog.xml"))
        .find(|candidate| candidate.is_file())
}

/// Resolves imports with a catalog, which maps an ontology IRI to the path
/// of its `.oml` file without the extension.
pub struct CatalogResolver(Catalog);

impl CatalogResolver {
    pub fn from_path(catalog: impl AsRef<Path>) -> Result<CatalogResolver, CatalogError> {
        Catalog::from_path(catalog).map(CatalogResolver)
    }
}

impl Resolver for CatalogResolver {
    fn load(&self, iri: &str) -> Option<String> {
        fs::read_to_string(self.path(iri)?).ok()
    }

    fn path(&self, iri: &str) -> Option<PathBuf> {
        let base = self.0.resolve(iri)?;
        // The extension is appended: ontology names may contain dots.
        Some(PathBuf::from(format!("{}.oml", base.display())))
    }
}
