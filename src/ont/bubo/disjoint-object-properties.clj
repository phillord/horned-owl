(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defoproperty s :disjoint r)

(save-all)
