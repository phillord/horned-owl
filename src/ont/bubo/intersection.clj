(clojure.core/load-file "ontology.clj")

(defclass A)
(defoproperty r)

(defclass X)
(refine X
  :equivalent
  (and
   (some r X)
   (only r X)))

(save-all)
