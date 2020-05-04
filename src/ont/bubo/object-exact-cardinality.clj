(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defclass D)
(defclass C :super (exactly 1 r D))

(save-all)
