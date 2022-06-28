(clojure.core/load-file "ontology.clj")


(defoproperty r)
(defoproperty s)

(add-subproperty o (inverse r) (inverse s))
(save-all)
