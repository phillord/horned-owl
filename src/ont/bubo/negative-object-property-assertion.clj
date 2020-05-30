(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defindividual J)
(defindividual I :fact (fact-not r J))


(save-all)
