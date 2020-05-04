(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defclass D)
(defclass C :super (at-least 1 r D))

(save-all)
