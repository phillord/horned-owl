(clojure.core/load-file "ontology.clj")

(defclass A)
(defoproperty r)
(defclass B :super (only r A))

(save-all)
