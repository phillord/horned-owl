(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defdproperty A)


(defclass C
  :haskey r)

(save-all)
