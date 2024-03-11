(clojure.core/load-file "ontology.clj")

(defdproperty dp)


(defclass C
  :haskey dp)

(save-all)
