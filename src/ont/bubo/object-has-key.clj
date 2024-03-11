(clojure.core/load-file "ontology.clj")

(defoproperty r)


(defclass C
  :haskey r)

(save-all)
