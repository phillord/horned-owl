(clojure.core/load-file "ontology.clj")

(defclass P)
(defindividual J
  :type (not P))

(save-all)
