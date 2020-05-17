(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defclass C :subclass (at-most 1 r))

(save-all)
