use crate::model::{Ontology, IRI, ForIRI};

use std::collections::HashMap;

pub struct OntologyCollection<A: ForIRI>(HashMap<IRI<A>, Box<dyn Ontology<A>>>);
