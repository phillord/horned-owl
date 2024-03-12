(cc/load-file "ontology.clj")

(defdproperty d)
(defclass C
  :super
  (.getOWLDataMinCardinality
   (owl-data-factory)
   1
   (#'tawny.owl/ensure-data-property d)
   (#'tawny.owl/ensure-data-range (iri "http://www.w3.org/2001/XMLSchema#integer"))))

(save-all)
