(clojure.core/load-file "ontology.clj")


(defoproperty r)
(defoproperty s :super r)

(save-all)
