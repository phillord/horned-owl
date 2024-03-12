(cc/load-file "ontology.clj")

(defdproperty d)
(defclass C
  :super (some d (iri "http://www.w3.org/2001/XMLSchema#integer")))

(save-all)
