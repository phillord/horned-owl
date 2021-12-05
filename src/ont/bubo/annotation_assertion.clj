(clojure.core/load-file "ontology.clj")

(add-axiom o
 (.getOWLAnnotationAssertionAxiom
  (owl-data-factory)
  (iri "http://www.example.com/i")
  (comment "non-anonymous individual")))

(save-all)
