use crate::model::{Build, ForIRI, IRI};

#[cfg(feature = "remote")]
use ureq;

// fn from_dir_bufread<R: BufRead>(dir: PathBuf, iri:&String) -> R {
//     // Split the string from the last / (rsplit)
//     // File in same directory exists?
//     // Read it!
// }

// Given an `iri`, return the IRI local to `doc_iri` that this would
// have.
pub fn localize_iri<A: ForIRI>(iri: &IRI<A>, doc_iri: &IRI<A>) -> IRI<A> {
    let b = Build::new();
    let (_, term_iri) = iri.split_at(iri.rfind('/').unwrap() + 1);

    b.iri(if let Some(index) = doc_iri.rfind("/") {
        format!("{}/{}", doc_iri.split_at(index).0, term_iri)
    } else {
        format!("./{}", term_iri)
    })
}

// Return the ontology as Vec<u8> from `iri` unless we think that it
// is local to doc_iri
pub fn resolve_iri<A: ForIRI>(iri: &IRI<A>, doc_iri: &IRI<A>) -> String {
    strict_resolve_iri(&localize_iri(iri, doc_iri))
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
    use std::path::PathBuf;

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
}
