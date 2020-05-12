(clojure.core/load-file "ontology.clj")


(defoproperty r)
(defoproperty s)
(defoproperty t :subchain r s)

(save-all)
