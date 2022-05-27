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

/// let doc_iri = b.iri("file://blah/and.owl");

/// let path_buf = file_iri_to_pathbuf(&doc_iri);
/// assert_eq!(path_buf.to_str().unwrap(), "blah/and.owl");
/// ```
pub fn file_iri_to_pathbuf<A: ForIRI>(iri: &IRI<A>) -> PathBuf {
    Path::new(&*iri.split_at(7).1).into()
}

/// Return an `IRI` for the given `PathBuf`
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// # use std::path::Path;
/// let b = Build::new_rc();

/// let target_iri = b.iri("file://blah/and.owl");

/// let path = Path::new("blah/and.owl");
/// let source_iri = path_to_file_iri(&b, &path);
/// assert_eq!(source_iri.as_ref(), "file://blah/and.owl");
/// ```
pub fn path_to_file_iri<A: ForIRI>(b: &Build<A>, pb: &Path) -> IRI<A> {
    b.iri(format!("file://{}", pb.to_str().unwrap()))
}

/// Return true if the iri is a file IRI.
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// let b = Build::new_rc();

/// let doc_iri = b.iri("file://blah/and.owl");

/// assert!(is_file_iri(&doc_iri))
/// ```
pub fn is_file_iri<A: ForIRI>(iri: &IRI<A>) -> bool {
    (*iri).starts_with("file:/")
}

/// Assuming that doc_iri is a local file IRI, return a new IRI for
/// that is the local equivalent of `iri`.
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// let b = Build::new_rc();

/// let doc_iri = b.iri("file://blah/and.owl");
/// let iri = b.iri("http://www.example.com/or.owl");

/// let local = b.iri("file://blah/or.owl");

/// assert_eq!(localize_iri(&iri, &doc_iri), local);
/// ```
pub fn localize_iri<A: ForIRI>(iri: &IRI<A>, doc_iri: &IRI<A>) -> IRI<A> {
    let b = Build::new();
    let (_, term_iri) = iri.split_at(iri.rfind('/').unwrap() + 1);

    b.iri(if let Some(index) = doc_iri.rfind('/') {
        format!("{}/{}", doc_iri.split_at(index).0, term_iri)
    } else {
        format!("./{}", term_iri)
    })
}

// Return the ontology as Vec<u8> from `iri` unless we think that it
// is local to doc_iri
pub fn resolve_iri<A: ForIRI>(iri: &IRI<A>, doc_iri: Option<&IRI<A>>) -> (IRI<A>, String) {
    let local = if let Some(doc_iri) = doc_iri {
        localize_iri(iri, doc_iri)
    } else {
        iri.clone()
    };

    if is_file_iri(&local) {
        let mut path = file_iri_to_pathbuf(&local);
        if path.as_path().exists() {
            return (local, ::std::fs::read_to_string(path).unwrap());
        }

        if let Some(doc_iri) = doc_iri {
            let doc_ext = doc_iri.split_once('.').unwrap();
            path.set_extension(doc_ext.1);
            if path.exists() {
                return (local, ::std::fs::read_to_string(path).unwrap());
            }
        }
        todo!("resolve_iri doesn't have error handlign");
    }

    (local, strict_resolve_iri(iri))
}

// Return the ontology as Vec<u8> from `iri`.
#[cfg(feature = "remote")]
pub fn strict_resolve_iri<A: ForIRI>(iri: &IRI<A>) -> String {
    let s: String = iri.into();
    ureq::get(&s).call().unwrap().into_string().unwrap()
}

#[cfg(not(feature = "remote"))]
pub fn strict_resolve_iri<A: ForIRI>(_iri: &IRI<A>) -> String {
    todo!("fail")
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::model::Build;

    #[test]
    fn localize() {
        let b = Build::new_rc();

        let doc_iri = b.iri("file://blah/and.owl");

        let iri = b.iri("http://www.example.com/or.owl");

        let local = b.iri("file://blah/or.owl");

        assert_eq!(localize_iri(&iri, &doc_iri), local);
    }

    #[test]
    fn simple_iri() {
        let _dir_path_buf = PathBuf::from(file!());
        let b = Build::new_rc();
        let i: IRI<_> = b.iri("http://www.example.com");

        strict_resolve_iri(&i);
    }

    #[test]
    fn test_resolve_iri() {
        let b = Build::new_rc();
        let i: IRI<_> = b.iri("http://www.example.com/bikepath.md");
        let doc_iri = b.iri("file://cargo.toml");

        let bikepath_str = ::std::fs::read_to_string("bikepath.md").unwrap();
        let (_, iri_str) = resolve_iri(&i, Some(&doc_iri));
        assert_eq!(bikepath_str, iri_str);
    }
}
