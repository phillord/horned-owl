(cc/load-file "ontology.clj")

(defclass A
  :equivalent (some (.getOWLBottomDataProperty (owl-data-factory))
                    (iri "http://www.w3.org/2000/01/rdf-schema#Literal")))

(save-all)
