(cc/load-file "ontology.clj")

(defdatatype D
  :equivalent (iri "http://www.w3.org/2002/07/owl#real"))

(save-all)
