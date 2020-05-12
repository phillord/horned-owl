(clojure.core/load-file "ontology.clj")


(defoproperty r)
(defoproperty s :super (inverse r))

(save-all)
