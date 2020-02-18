(clojure.core/load-file "ontology.clj")

(defoproperty r)
(add-axiom
 o (.getOWLTransitiveObjectPropertyAxiom (owl-data-factory) (inverse r)))

(save-all)
