(clojure.core/load-file "ontology.clj")

(defindividual i)

(add-axiom o
 (.getOWLAnnotationAssertionAxiom
  (owl-data-factory)
  (iri-for-name o "i")
  (comment "non-anonymous individual")))

(save-all)
