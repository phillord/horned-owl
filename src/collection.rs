use crate::model::{ForIRI, Ontology, IRI};

use std::collections::HashMap;

pub struct OntologyCollection<A: ForIRI>(HashMap<IRI<A>, Box<dyn Ontology<A>>>);
