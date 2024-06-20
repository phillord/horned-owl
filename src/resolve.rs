//! Fetches data from an IRI

//! # Overview
//!
//! Given an IRI return the content using local resources if possible.
//! Remote resolution is feature gated to reduce the binary size a little.
use crate::error::HornedError;
use crate::model::{Build, ForIRI, IRI};

use std::path::{Path, PathBuf};

#[cfg(feature = "remote")]
use ureq;

// fn from_dir_bufread<R: BufRead>(dir: PathBuf, iri:&String) -> R {
//     // Split the string from the last / (rsplit)
//     // File in same directory exists?
//     // Read it!
// }

/// Return a `PathBuf` for the given IRI
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// let b = Build::new_rc();

/// let doc_iri = b.iri("file://base_dir/and.owl");

/// let path_buf = file_iri_to_pathbuf(&doc_iri);
/// assert_eq!(path_buf.to_str().unwrap(), "base_dir/and.owl");
/// ```
#[deprecated(since = "1.0.0", note = "please use `as_local_path_buffer` instead")]
pub fn file_iri_to_pathbuf<A: ForIRI>(iri: &IRI<A>) -> PathBuf {
    Path::new(iri.split_at(7).1).into()
}

/// Return an `IRI` for the given `PathBuf`
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// # use std::path::Path;
/// let b = Build::new_rc();

/// let target_iri = b.iri("file://base_dir/and.owl");

/// let path = Path::new("base_dir/and.owl");
/// let source_iri = path_to_file_iri(&b, &path);
/// assert_eq!(source_iri.as_ref(), "file://base_dir/and.owl");
/// ```
pub fn path_to_file_iri<A: ForIRI>(b: &Build<A>, pb: &Path) -> IRI<A> {
    pb.to_str()
        .map(|path_str| b.iri(&format!("file://{path_str}")))
        .expect("path should contain valid Unicode")
}

/// Returns `Some(path_buf)` if the input corresponds to a file IRI.
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// let b = Build::new_rc();

/// let doc_iri = b.iri("file://base_dir/and.owl");
/// let path_buf = as_local_path_buffer(&doc_iri);

/// assert!(path_buf.is_some());
/// assert_eq!(path_buf.unwrap().to_str().unwrap(), "base_dir/and.owl");
/// ```
pub fn as_local_path_buffer<A: ForIRI>(iri: &IRI<A>) -> Option<PathBuf> {
    iri.strip_prefix("file://")
        .map(|path_str| Path::new(path_str).into())
}

/// Assuming that doc_iri is a local file IRI, return a new IRI for
/// that is the local equivalent of `iri`.
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// let b = Build::new_rc();

/// let doc_iri = b.iri("file://base_dir/and.owl");
/// let iri = b.iri("http://www.example.com/or.owl");

/// let local = b.iri("file://base_dir/or.owl");

/// assert_eq!(localize_iri(&iri, &doc_iri), local);
/// ```
pub fn localize_iri<A: ForIRI>(iri: &IRI<A>, doc_iri: &IRI<A>) -> IRI<A> {
    let b = Build::new();
    let (_, term_iri) = iri.split_at(iri.rfind('/').unwrap() + 1);

    let s = if let Some(index) = doc_iri.rfind('/') {
        format!("{}/{}", doc_iri.split_at(index).0, term_iri)
    } else {
        format!("./{}", term_iri)
    };

    b.iri(&s)
}

/// Return contents of an IRI as a string
///
/// This method will use local files if possible, or remote access if needed
pub fn resolve_iri<A: ForIRI>(
    iri: &IRI<A>,
    doc_iri: Option<&IRI<A>>,
) -> Result<(IRI<A>, String), HornedError> {
    let local = if let Some(doc_iri) = doc_iri {
        localize_iri(iri, doc_iri)
    } else {
        iri.clone()
    };

    if let Some(mut path) = as_local_path_buffer(&local) {
        let file_exists = path.try_exists()?;

        if file_exists {
            let result = ::std::fs::read_to_string(path)?;
            Ok((local, result))
        } else if let Some(doc_iri) = doc_iri {
            let doc_ext = doc_iri.split_once('.').map(|(_, ext)| ext).unwrap_or("");
            path.set_extension(doc_ext);

            let doc_file_exists = path.try_exists()?;
            if doc_file_exists {
                let result = ::std::fs::read_to_string(path)?;
                Ok((local, result))
            } else {
                Err(HornedError::IOError(std::io::Error::from(
                    std::io::ErrorKind::NotFound,
                )))
            }
        } else {
            Err(HornedError::IOError(std::io::Error::from(
                std::io::ErrorKind::NotFound,
            )))
        }
    } else {
        Ok((local, strict_resolve_iri(iri).unwrap()))
    }
}

// Return the ontology as Vec<u8> from `iri`.
#[cfg(feature = "remote")]
pub fn strict_resolve_iri<A: ForIRI>(iri: &IRI<A>) -> Result<String, HornedError> {
    // let s: String = iri.into();
    ureq::get(iri).call()?.into_string().map_err(|e| e.into())
}

#[cfg(not(feature = "remote"))]
pub fn strict_resolve_iri<A: ForIRI>(_iri: &IRI<A>) -> Result<String, HornedError> {
    todo!("fail")
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::model::Build;

    #[test]
    fn localize() {
        let b = Build::new_rc();

        let doc_iri = b.iri("file://base_dir/and.owl");

        let iri = b.iri("http://www.example.com/or.owl");

        let local = b.iri("file://base_dir/or.owl");

        assert_eq!(localize_iri(&iri, &doc_iri), local);
    }

    #[test]
    fn simple_iri() {
        let _dir_path_buf = PathBuf::from(file!());
        let b = Build::new_rc();
        let i: IRI<_> = b.iri("http://www.example.com");

        assert!(strict_resolve_iri(&i).is_ok());
    }

    #[test]
    fn test_resolve_iri() {
        let b = Build::new_rc();
        let i: IRI<_> = b.iri("http://www.example.com/bikepath.md");
        let doc_iri = b.iri("file://cargo.toml");

        let bikepath_str = ::std::fs::read_to_string("bikepath.md").unwrap();
        let (_, iri_str) = resolve_iri(&i, Some(&doc_iri)).unwrap();
        assert_eq!(bikepath_str, iri_str);
    }
}
