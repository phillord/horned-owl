//! Fetches data from an IRI

//! # Overview
//!
//! Given an IRI return the content using local resources if possible.
//! Remote resolution is feature gated to reduce the binary size a little.
use crate::error::HornedError;
use crate::model::{Build, ForIRI, IRI};

use oxiri::Iri;

use std::path::{Path, PathBuf};

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
///
/// let doc_iri = b.iri("file://base_dir/and.owl");
///
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
///
/// let target_iri = b.iri("file://base_dir/and.owl");
///
/// let path = Path::new("base_dir/and.owl");
/// let source_iri = path_to_file_iri(&b, &path);
/// assert_eq!(source_iri.as_ref(), "file://base_dir/and.owl");
/// ```
pub fn path_to_file_iri<A: ForIRI>(b: &Build<A>, pb: &Path) -> IRI<A> {
    pb.to_str()
        .map(|path_str| b.iri(format!("file://{path_str}")))
        .expect("path should contain valid Unicode")
}

/// Returns `Some(path_buf)` if the input corresponds to a file
/// IRI. If the IRI is not a file IRI, return None.
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// let b = Build::new_rc();
///
/// let doc_iri = b.iri("file://base_dir/and.owl");
/// let path_buf = as_local_path_buffer(&doc_iri);
///
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
///
/// let doc_iri = b.iri("file://base_dir/and.owl");
/// let iri = b.iri("http://www.example.com/or.owl");
///
/// assert_eq!(localize_iri(&iri, &doc_iri).to_str().unwrap(), "base_dir/or.owl");
/// ```
pub fn localize_iri<A: ForIRI>(iri: &IRI<A>, doc_iri: &IRI<A>) -> PathBuf {
    let parsed_iri = Iri::parse(iri.to_string()).unwrap();
    let doc_iri_path_buf = as_local_path_buffer(doc_iri);

    let iri_path = parsed_iri.path().strip_prefix("/").unwrap();

    if let Some(buf) = doc_iri_path_buf
        && let Some(parent) = buf.parent()
    {
        let mut location = parent.to_path_buf();
        location.push(iri_path);
        location
    } else {
        PathBuf::from(iri_path)
    }
}

/// Return contents of an IRI as a string
///
/// This will return the content accessible relevant for `iri`. It
/// will attempt to use local contents which are assumed to be the
/// same as the content at `iri`. This is done relative to `doc_iri`
/// which will normally be the Document IRI of an importing ontology.
///
/// Should the local resolution fail, remote access is used instead.
///
/// Returns the doc IRI from which it was resolved, the content or an
/// error.
pub fn resolve_iri<A: ForIRI>(
    iri: &IRI<A>,
    doc_iri: Option<&IRI<A>>,
) -> Result<(IRI<A>, String), HornedError> {
    let b = Build::new();

    // Do we have a file IRI
    let mut some_local = as_local_path_buffer(iri);

    if some_local.is_none() {
        // Attempt to determine the local IRI if there is a `doc_iri`,
        // otherwise use the IRI to be resolved.
        some_local = doc_iri.map(|di| localize_iri(iri, di))
    }

    // If we now have a local file iri, we can attempt to read from local
    if let Some(mut local) = some_local {
        // Does the file exist. If so we are all sorted
        let file_exists = local.try_exists()?;
        if file_exists {
            let result = ::std::fs::read_to_string(&local)?;
            return Ok((path_to_file_iri(&b, &local), result));
        }

        // The path might not have the correct extension, so again check
        if let Some(doc_iri) = doc_iri {
            // take the extension of the doc_iri and assume we have the same thing
            // and try again
            let doc_ext = doc_iri.split_once('.').map(|(_, ext)| ext).unwrap_or("");
            local.set_extension(doc_ext);

            let doc_file_exists = local.try_exists()?;
            if doc_file_exists {
                let result = ::std::fs::read_to_string(&local)?;
                return Ok((path_to_file_iri(&b, &local), result));
            }
        }

        // It looks like a local file, but we cannot resolve it
        return Err(HornedError::IOError(std::io::Error::from(
            std::io::ErrorKind::NotFound,
        )));
    }

    // It is not a file IRI so hope that it is a http(s) IRI and resolve it using ureq
    Ok((iri.clone(), strict_resolve_iri(iri)?))
}

/// Resolve the contents of the IRI as a String.
///
/// This functions only over "http(s)" IRIs and will not resolve any
/// other form of IRI.
///
/// Fails with panic if the `remote` feature is not enabled.
#[cfg(feature = "remote")]
pub fn strict_resolve_iri<A: ForIRI>(iri: &IRI<A>) -> Result<String, HornedError> {
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
    fn test_as_local_path_buffer() {
        let b = Build::new_rc();

        assert!(as_local_path_buffer(&b.iri("http://www.example.com")).is_none());
        assert!(as_local_path_buffer(&b.iri("file://b.owl")).is_some());
    }

    #[test]
    fn localize() {
        let b = Build::new_rc();

        let doc_iri = b.iri("file://base_dir/and.owl");

        let iri = b.iri("http://www.example.com/or.owl");

        assert_eq!(
            localize_iri(&iri, &doc_iri).to_str().unwrap(),
            "base_dir/or.owl"
        );
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
