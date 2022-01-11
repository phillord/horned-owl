(clojure.core/load-file "ontology.clj")

(defindividual I)
(defindividual J :different I)

(save-all)
