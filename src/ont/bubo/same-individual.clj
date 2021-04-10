(clojure.core/load-file "ontology.clj")

(defindividual r)
(defindividual s :same r)

(save-all)
