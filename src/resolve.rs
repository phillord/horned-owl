use crate::model::{Build, IRI};

use ureq;

// fn from_dir_bufread<R: BufRead>(dir: PathBuf, iri:&String) -> R {
//     // Split the string from the last / (rsplit)
//     // File in same directory exists?
//     // Read it!
// }

// Given an `iri`, return the IRI local to `doc_iri` that this would
// have.
pub fn localize_iri(iri:&IRI, doc_iri:&IRI) -> IRI {
    let b = Build::new();
    let (_,term_iri) = iri.split_at(iri.rfind('/').unwrap() + 1);

    b.iri(
        if let Some(index) = doc_iri.rfind("/") {
            format!("{}/{}", doc_iri.split_at(index).0, term_iri)
        }
        else{
            format!("./{}", term_iri)
        }
    )
}

// Return the ontology as Vec<u8> from `iri` unless we think that it
// is local to doc_iri
pub fn resolve_iri(iri:&IRI, doc_iri: &IRI) -> String {
    strict_resolve_iri(&localize_iri(iri, doc_iri))
}

// Return the ontology as Vec<u8> from `iri`.
pub fn strict_resolve_iri(iri: &IRI) -> String {
    let s:String = iri.into();
    ureq::get(&s).call().unwrap().into_string().unwrap()
}

#[cfg(test)]
mod test{
    use super::*;
    use crate::model::Build;

    #[test]
    fn localize() {
        let b = Build::new();

        let doc_iri = b.iri(
            "file://blah/and.owl"
        );

        let iri = b.iri(
          "http://www.example.com/or.owl"
        );

        let local = b.iri(
          "file://blah/or.owl"
        );


        assert_eq!(localize_iri(&iri, &doc_iri), local);
    }

    // #[test]
    // fn simple_iri() {
    //     let dir_path_buf = PathBuf::from(file!());
    //     let dir = dir_path_buf.parent().unwrap();
    //     let cdir = dir.canonicalize().unwrap();
    //     let b = Build::new();
    //     let i:IRI = b.iri(
    //         format!("file://{}/ont/owl-rdf/and.owl", cdir.to_string_lossy())
    //     );

    //     let s:String = strict_resolve_iri(&i);

    //     let ont_s = include_str!("./ont/owl-rdf/and.owl");

    //     assert_eq!(s, ont_s);
    //}
}
