(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defoproperty s :equivalent r)

(save-all)
