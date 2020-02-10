(clojure.core/load-file "ontology.clj")


(defclass A)
(defclass B :super (not A))

(save-all)
