(clojure.core/load-file "ontology.clj")

(defindividual p)
(defindividual q)
(defindividual r)

(defindividual s :same [p q r])

(save-all)
