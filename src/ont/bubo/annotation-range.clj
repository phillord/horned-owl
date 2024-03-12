(clojure.core/load-file "ontology.clj")

(defaproperty a)

(add-axiom
 o
 (.getOWLAnnotationPropertyRangeAxiom
  (owl-data-factory)
  a (iri "http://www.example.com/d")))

(save-all)
