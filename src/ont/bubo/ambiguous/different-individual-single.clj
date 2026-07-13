(clojure.core/load-file "ontology.clj")

(defindividual I)

;; OWL-API allows a single-member DifferentIndividuals axiom even though
;; the OWL 2 spec requires n >= 2; several real-world ontologies contain
;; this pattern.  Add it directly via the Java API to reproduce the case.
(.applyChange
 (owl-ontology-manager)
 (org.semanticweb.owlapi.model.AddAxiom.
  o
  (.getOWLDifferentIndividualsAxiom
   (owl-data-factory)
   #{I})))

(save-all)
