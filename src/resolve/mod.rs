use curl::easy::Easy;
use crate::model::IRI;


// fn from_dir_bufread<R: BufRead>(dir: PathBuf, iri:&String) -> R {
//     // Split the string from the last / (rsplit)
//     // File in same directory exists?
//     // Read it!
// }



pub fn resolve_iri(iri: &IRI) -> Vec<u8> {
    let mut data = Vec::new();
    let mut handle = Easy::new();
    {
        let s:String = iri.into();
        handle.url(&s).unwrap();
        let mut transfer = handle.transfer();
        transfer.write_function(|new_data| {
            data.extend_from_slice(new_data);
            Ok(new_data.len())
        }).unwrap();
        transfer.perform().unwrap();
    }
    data
}

#[cfg(test)]
mod test{
    use super::*;
    use crate::model::Build;
    use std::path::PathBuf;

    #[test]
    fn simple_iri() {
        let dir_path_buf = PathBuf::from(file!());
        let dir = dir_path_buf.parent().unwrap().parent().unwrap();
        let cdir = dir.canonicalize().unwrap();
        let b = Build::new();
        let i:IRI = b.iri(
            format!("file://{}/ont/owl-rdf/and.owl", cdir.to_string_lossy())
        );

        let s:String = String::from_utf8(resolve_iri(&i)).unwrap();

        let ont_s = include_str!("../ont/owl-rdf/and.owl");

        assert_eq!(s, ont_s);
    }
}
