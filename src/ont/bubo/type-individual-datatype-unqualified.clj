(clojure.core/load-file "ontology.clj")

(defclass P)
(defoproperty r)
(defindividual J
  :type
  (exactly 2 r))

(save-all)
