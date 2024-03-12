(cc/load-file "ontology.clj")

(defdproperty dp
  :range (iri "http://www.w3.org/2001/XMLSchema#real"))

(save-all)
