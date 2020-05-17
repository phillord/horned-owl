(clojure.core/load-file "ontology.clj")

(defaproperty b)
(defaproperty a :super b)

(save-all)
