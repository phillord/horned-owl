use crate::model::{Ontology, IRI};

use std::collections::HashMap;

pub struct OntologyCollection(HashMap<IRI, Box<dyn Ontology>>);
