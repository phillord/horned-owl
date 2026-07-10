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

/// Assuming that doc_iri is a local file IRI, return a Vec of BufRead
/// instances that are the local equivalent of `iri`. The BufRead
/// instances cover a range of possible options for the local
/// equivalent, in the order that they will be checked during
/// resolution.
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
/// let localized_vec:Vec<_> = localize_iri(&iri, Some(&doc_iri))
///       .into_iter().map(|pb|
///                 pb.to_string_lossy().to_string()
///       ).collect();
///
/// assert_eq!(
///   localized_vec,
///   vec![
///     "base_dir/or.owl"
///   ]
/// );
/// ```
pub fn localize_iri<'a, A: ForIRI + 'a, IO: Into<Option<&'a IRI<A>>>>(
    iri: &IRI<A>,
    doc_iri: IO,
) -> Vec<PathBuf> {
    let doc_iri = doc_iri.into();
    let parsed_iri = Iri::parse(iri.to_string()).unwrap();
    let doc_iri_path_buf = doc_iri.and_then(as_local_path_buffer);

    let iri_path = parsed_iri.path().strip_prefix("/").unwrap();
    let iri_path_with_underscore = iri_path.replace("/", "_");
    let option_iri_path_final = iri_path.rsplit("/").next();
    let option_iri_path_has_extension = iri_path.contains('.');

    let with_doc_iri_or_not = |name_from_iri| {
        if let Some(buf) = doc_iri_path_buf.clone()
            && let Some(parent) = buf.parent()
        {
            let mut location = parent.to_path_buf();
            location.push(name_from_iri);
            if !option_iri_path_has_extension && let Some(extension) = buf.extension() {
                location.set_extension(extension);
            }

            location
        } else {
            PathBuf::from(name_from_iri)
        }
    };

    let mut v = vec![with_doc_iri_or_not(iri_path_with_underscore.as_str())];

    if let Some(iri_path_final) = option_iri_path_final {
        let pb = with_doc_iri_or_not(iri_path_final);
        if !v.contains(&pb) {
            v.push(pb)
        }
    }

    v
}

/// Assuming that doc_iri is a local file IRI, return a new BufRead
/// for that is the local equivalent of `iri`. This function will
/// return the local equivalent which is Horned-OWL favored local
/// equivalent, which where it serializes to.
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
/// assert_eq!(localize_iri_favored(&iri, &doc_iri).to_string_lossy(), "base_dir/or.owl");
/// ```
pub fn localize_iri_favored<'a, A: ForIRI + 'a, IO: Into<Option<&'a IRI<A>>>>(
    iri: &IRI<A>,
    doc_iri: IO,
) -> PathBuf {
    localize_iri(iri, doc_iri)
        .into_iter()
        .next()
        .expect("localize_iri should always return at least one element")
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
/// `remote_body_limit` bounds the number of bytes read from a remote
/// response if resolution falls back to a network fetch -- see
/// [strict_resolve_iri].
///
/// Returns the doc IRI from which it was resolved, the content or an
/// error.
pub fn resolve_iri<'a, A: ForIRI + 'a, IO: Into<Option<&'a IRI<A>>>>(
    iri: &IRI<A>,
    doc_iri: IO,
    remote_body_limit: u64,
) -> Result<(IRI<A>, String), HornedError> {
    let b = Build::new();

    // Do we have a file IRI
    let some_local_pb = as_local_path_buffer(iri);

    if let Some(pb) = some_local_pb {
        let file_exists = pb.try_exists()?;
        if file_exists {
            let result = ::std::fs::read_to_string(&pb)?;
            return Ok((path_to_file_iri(&b, &pb), result));
        }

        // It looks like a file IRI but we cannot resolve it, so we
        // should return an error
        return Err(HornedError::IOError(std::io::Error::from(
            std::io::ErrorKind::NotFound,
        )));
    }

    // Attempt to determine potential local locations if there is a `doc_iri`
    let doc_iri = doc_iri.into();
    let some_local = doc_iri
        .map(|di| localize_iri(iri, Some(di)))
        .unwrap_or_default();

    // If we now have a local file locations, we can attempt to read from them
    for mut local in some_local {
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
    }

    // All attempts to resolve it locally have failed, so try remote
    Ok((iri.clone(), strict_resolve_iri(iri, remote_body_limit)?))
}

/// Resolve the contents of the IRI as a String.
///
/// This functions only over "http(s)" IRIs and will not resolve any
/// other form of IRI.
///
/// `remote_body_limit` caps the number of bytes read from the
/// response body; use `u64::MAX` for no limit.
///
/// Fails with panic if the `remote` feature is not enabled.
#[cfg(feature = "remote")]
pub fn strict_resolve_iri<A: ForIRI>(
    iri: &IRI<A>,
    remote_body_limit: u64,
) -> Result<String, HornedError> {
    ureq::get(iri.as_ref())
        .call()?
        .body_mut()
        .with_config()
        .limit(remote_body_limit)
        .read_to_string()
        .map_err(|e| e.into())
}

#[cfg(not(feature = "remote"))]
pub fn strict_resolve_iri<A: ForIRI>(
    iri: &IRI<A>,
    _remote_body_limit: u64,
) -> Result<String, HornedError> {
    Err(HornedError::ImportError(format!(
        "cannot resolve IRI {iri} remotely: the 'remote' feature is not enabled"
    )))
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
    fn test_localize_favored() {
        let b = Build::new_rc();
        let favored = |iri, doc_iri| {
            localize_iri_favored(&b.iri(iri), doc_iri)
                .to_string_lossy()
                .to_string()
        };

        let favored_none = |iri| favored(iri, None);

        assert_eq!(favored_none("http://www.example.com/or.owl"), "or.owl");

        assert_eq!(
            favored_none("http://www.example.com/intermediate/or.owl"),
            "intermediate_or.owl"
        );

        // assert_eq!(
        //     favored_none("http://www.example.com/or/2025-12-10"),
        //     "or_2025-12-10.owl"
        // );

        let doc_iri = b.iri("file://base_dir/and.owl");
        let favored_with_base = |iri| favored(iri, Some(&doc_iri));

        // TODO -- should this include the host?
        assert_eq!(
            favored_with_base("http://www.example.com/or.owl"),
            "base_dir/or.owl"
        );

        assert_eq!(
            favored_with_base("http://www.example.com/intermediate/or.owl"),
            "base_dir/intermediate_or.owl"
        );

        assert_eq!(
            favored_with_base("http://www.example.com/or/2025-12-10"),
            "base_dir/or_2025-12-10.owl"
        );
    }

    #[test]
    fn test_localize() {
        let b = Build::new_rc();
        let localized = |iri, doc_iri| {
            localize_iri(&b.iri(iri), doc_iri)
                .into_iter()
                .map(|pb| pb.to_string_lossy().to_string())
                .collect::<Vec<_>>()
        };

        let localized_none = |iri| localized(iri, None);

        assert_eq!(
            localized_none("http://www.example.com/or.owl"),
            vec!["or.owl"]
        );

        assert_eq!(
            localized_none("http://www.example.com/intermediate/or.owl"),
            vec!["intermediate_or.owl", "or.owl"]
        );

        let doc_iri = b.iri("file://base_dir/and.owl");
        let localized_with_base = |iri| localized(iri, Some(&doc_iri));

        assert_eq!(
            localized_with_base("http://www.example.com/or.owl"),
            vec!["base_dir/or.owl"]
        );

        assert_eq!(
            localized_with_base("http://www.example.com/intermediate/or.owl"),
            vec!["base_dir/intermediate_or.owl", "base_dir/or.owl"]
        );

        assert_eq!(
            localized_with_base("http://www.example.com/or/2025-12-10"),
            vec!["base_dir/or_2025-12-10.owl", "base_dir/2025-12-10.owl"]
        );

        assert_eq!(
            localized_with_base("http://www.example.com/or.ext"),
            vec!["base_dir/or.ext",]
        );
        assert_eq!(
            localized(
                "http://www.example.com/or/2025-12-10",
                Some(&b.iri("file://base_dir/and.rdf"))
            ),
            vec!["base_dir/or_2025-12-10.rdf", "base_dir/2025-12-10.rdf"]
        );
    }

    #[test]
    fn simple_iri() {
        let _dir_path_buf = PathBuf::from(file!());
        let b = Build::new_rc();
        let i: IRI<_> = b.iri("http://www.example.com");

        // This does network access (to example.com). This cannot be
        // guaranteed to succeed. Perhaps we don't need this test at all.
        assert!(strict_resolve_iri(&i, u64::MAX).is_ok());
    }

    #[test]
    fn test_resolve_iri() {
        let b = Build::new_rc();
        let i: IRI<_> = b.iri("http://www.example.com/bikepath.md");
        let doc_iri = b.iri("file://Cargo.toml");

        let bikepath_str = ::std::fs::read_to_string("bikepath.md").unwrap();
        let (_, iri_str) = resolve_iri(&i, &doc_iri, u64::MAX).unwrap();
        assert_eq!(bikepath_str, iri_str);
    }

    #[test]
    fn test_resolve_iri_multiple() {
        let b = Build::new_rc();
        let tester = |iri, resolve_to, doc_iri| {
            let read_str = ::std::fs::read_to_string(format!("dev/resolve/{resolve_to}")).unwrap();
            let (_, iri_str) = resolve_iri(
                &b.iri(iri),
                &b.iri(format!("file://dev/resolve/{doc_iri}")),
                u64::MAX,
            )
            .unwrap();
            assert_eq!(read_str, iri_str);
        };

        // Simple case -- find and.txt relative to or.txt
        tester(
            "http://www.example.com/and.txt",
            "simple/and.txt",
            "simple/or.txt",
        );

        // With more complex file path
        tester(
            "http://www.example.com/intermediate/and.txt",
            "intermediate/intermediate_and.txt",
            "intermediate/or.txt",
        );

        // With more complex file path and fall back to simple resolution
        tester(
            "http://www.example.com/intermediate/and.txt",
            "simple/and.txt",
            "simple/or.txt",
        );

        // Without file extension, fall back to extension from doc IRI
        tester(
            "http://www.example.com/and",
            "simple/and.txt",
            "simple/or.txt",
        );
    }
}
