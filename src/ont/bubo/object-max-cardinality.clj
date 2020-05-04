(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defclass D)
(defclass C :super (at-most 1 r D))

(save-all)
