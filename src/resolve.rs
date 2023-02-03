use crate::{model::{Build, ForIRI, IRI}, error::{HornedError, Location}};

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
/// # fn doctest_file_iri_to_pathbuf() -> Result<(), Box<dyn std::error::Error>> {
/// let b = Build::new_rc();

/// let doc_iri = b.iri("file://blah/and.owl")?;

/// let path_buf = file_iri_to_pathbuf(&doc_iri)?;
/// assert_eq!(path_buf.to_str().unwrap(), "blah/and.owl");
/// # return Ok(());
/// # };
/// ```
pub fn file_iri_to_pathbuf<A: ForIRI>(iri: &IRI<A>) -> Result<PathBuf, HornedError> {
    if is_file_iri(iri) {
        Ok(Path::new(&iri.split_at(7).1).into())
    } else {
        Err(HornedError::ValidityError(format!("Expected IRI with file scheme, found {}", &iri.scheme()), Location::Unknown))
    }
}

/// Return an `IRI` for the given `PathBuf`
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// # use std::path::Path;
/// # fn doctest_path_to_file_iri() -> Result<(), Box<dyn std::error::Error>> {
/// let b = Build::new_rc();

/// let target_iri = b.iri("file://blah/and.owl")?;

/// let path = Path::new("blah/and.owl");
/// let source_iri = path_to_file_iri(&b, &path)?;
/// assert_eq!(source_iri.as_ref(), "file://blah/and.owl");
/// # return Ok(());
/// # };
/// ```
pub fn path_to_file_iri<A: ForIRI>(b: &Build<A>, pb: &Path) -> Result<IRI<A>, HornedError> {
    if let Some(path_str) = pb.to_str() {
        b.iri(format!("file://{path_str}"))
    } else {
        Err(HornedError::ValidityError(format!("Expected valid Unicode, found: {pb:?}"), Location::Unknown))
    }
    
}

/// Return true if the iri is a file IRI.
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// # fn doctest_is_file_iri() -> Result<(), Box<dyn std::error::Error>> {
/// let b = Build::new_rc();

/// let doc_iri = b.iri("file://local_path/and.owl")?;

/// assert!(is_file_iri(&doc_iri));
/// # return Ok(());
/// # };
/// ```
pub fn is_file_iri<A: ForIRI>(iri: &IRI<A>) -> bool {
    iri.scheme() == "file"
    // (*iri).starts_with("file://")
}

/// Assuming that doc_iri is a local file IRI, return a new IRI for
/// that is the local equivalent of `iri`.
///
/// # Examples
/// ```
/// # use horned_owl::model::*;
/// # use horned_owl::resolve::*;
/// # fn doctest_localize_iri() -> Result<(), Box<dyn std::error::Error>> {
/// let b = Build::new_rc();

/// let doc_iri = b.iri("file://blah/and.owl")?;
/// let iri = b.iri("http://www.example.com/or.owl")?;

/// let local = b.iri("file://blah/or.owl")?;

/// assert_eq!(localize_iri(&iri, &doc_iri)?, local);
/// # return Ok(());
/// # };
/// ```
/// 
/// # Errors
/// 
/// The method fails if `doc_iri` is not an IRI corresponding to a local file.
pub fn localize_iri<A: ForIRI>(iri: &IRI<A>, doc_iri: &IRI<A>) -> Result<IRI<A>, HornedError> {
    if is_file_iri(doc_iri) {
        let b = Build::new();
        // let iri_path = iri.path();
        // let iri_fragment = if let Some(frag) = iri.fragment() {
        //     format!("#{}", frag)
        // } else {
        //     String::with_capacity(0)
        // };
        // let term_iri = A::from(format!("{}{}",iri_path,iri_fragment));
        let iri_path_begin = iri.as_ref().rfind('/').unwrap() + 1;
        let (_, term_iri) = iri.as_ref().split_at(iri_path_begin);
        //
        b.iri(if let Some(index) = doc_iri.rfind('/') {
        format!("{}/{}", doc_iri.split_at(index).0, term_iri)
        } else {
            format!("./{term_iri}")
        })
        // doc_iri.resolve(&term_iri)
            // .map(|result| IRI::from(result))
            // .map_err(|err| err.into())
    } else {
        Err(HornedError::ValidityError(format!("Expected IRI with file scheme, found {}", doc_iri.scheme()), Location::Unknown))
    }
}

// Return the ontology as Vec<u8> from `iri` unless we think that it
// is local to doc_iri
pub fn resolve_iri<A: ForIRI>(iri: &IRI<A>, doc_iri: Option<&IRI<A>>) -> Result<(IRI<A>, String), HornedError> {
    let local = if let Some(doc_iri) = doc_iri {
        localize_iri(iri, doc_iri)?
    } else {
        iri.clone()
    };

    dbg!(&local);

    if is_file_iri(&local) {
        let mut path = file_iri_to_pathbuf(&local)?;
        if path.try_exists()? {
            return Ok((local, ::std::fs::read_to_string(path)?));
        } else if let Some(doc_iri) = doc_iri {
                let (_, doc_ext) = doc_iri.split_once('.').unwrap();
                path.set_extension(doc_ext);
                if path.try_exists()? {
                    return Ok((local, ::std::fs::read_to_string(path)?));
                }
        } else {
            todo!("resolve_iri doesn't have error handling");
        }
    } 
    // else {
    //     Err(HornedError::ValidityError(format!("Expected IRI with file scheme, found {}", local.into_inner().scheme()), Location::Unknown))
    // }

    Ok((local, strict_resolve_iri(iri)?))
}

// Return the ontology as Vec<u8> from `iri`.
#[cfg(feature = "remote")]
/// Downloads the document pointed by the IRI as a [String].
pub fn strict_resolve_iri<A: ForIRI>(iri: &IRI<A>) -> Result<String, crate::error::HornedError> {
    ureq::get(iri)
        .call()?
        .into_string()
        // .map(|res| A::from(res))
        .map_err(crate::error::HornedError::IOError)
}

#[cfg(not(feature = "remote"))]
pub fn strict_resolve_iri<A: ForIRI>(_iri: &IRI<A>) -> Result<String, crate::error::HornedError> {
    todo!("fail")
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::model::Build;

    #[test]
    fn localize() -> Result<(), Box<dyn std::error::Error>> {
        let b = Build::new_rc();

        let doc_iri = b.iri("file://blah/and.owl")?;

        let iri = b.iri("http://www.example.com/or.owl")?;

        let local = b.iri("file://blah/or.owl")?;

        assert_eq!(localize_iri(&iri, &doc_iri)?, local);

        Ok(())
    }

    #[test]
    fn simple_iri() -> Result<(), Box<dyn std::error::Error>> {
        let _dir_path_buf = PathBuf::from(file!());
        let b = Build::new_rc();
        let i: IRI<_> = b.iri("http://www.example.com")?;

        strict_resolve_iri(&i)?;

        Ok(())
    }

    #[test]
    fn test_resolve_iri() -> Result<(), Box<dyn std::error::Error>> {
        let b = Build::new_rc();
        let i: IRI<_> = b.iri("http://www.example.com/bikepath.md")?;
        let doc_iri = b.iri("file://cargo.toml")?;

        let bikepath_str = ::std::fs::read_to_string("bikepath.md")?;
        let (_, iri_str) = resolve_iri(&i, Some(&doc_iri))?;
        assert_eq!(bikepath_str, iri_str);

        Ok(())
    }
}
