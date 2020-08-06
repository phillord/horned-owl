use crate::model::{IRI, Ontology};


use std::collections::HashMap;


pub struct OntologyCollection (HashMap<IRI, Box<dyn Ontology>>);
