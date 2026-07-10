(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defclass C :subclass (exactly 1 r))

(save-all)
