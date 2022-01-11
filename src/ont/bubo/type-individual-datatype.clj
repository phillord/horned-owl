(clojure.core/load-file "ontology.clj")

(defclass P)
(defoproperty r)
(defindividual J
  :type
  (at-least 2 r P))

(save-all)
