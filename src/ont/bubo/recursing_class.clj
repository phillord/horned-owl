(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defclass X)
(refine X
  :equivalent
  (some r X))

(save-all)

