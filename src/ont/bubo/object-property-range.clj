(clojure.core/load-file "ontology.clj")

(defclass C)
(defoproperty r :range C)


(save-all)
