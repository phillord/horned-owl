(clojure.core/load-file "ontology.clj")

(defclass C)
(defoproperty op)
(defindividual D)
(defindividual C
  :fact (is op D))


(save-all)
