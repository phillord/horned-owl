(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defoproperty s :inverse r)

(save-all)

