(clojure.core/load-file "ontology.clj")

(defindividual I)
(defindividual J)
(defindividual K :different I J)

(save-all)
