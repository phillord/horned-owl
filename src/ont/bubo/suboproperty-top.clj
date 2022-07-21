(clojure.core/load-file "ontology.clj")


(defoproperty s :super (iri "http://www.w3.org/2002/07/owl#topObjectProperty"))

(save-all)
