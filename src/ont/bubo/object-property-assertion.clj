(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defindividual J)
(defindividual I :fact (fact r J))


(save-all)
