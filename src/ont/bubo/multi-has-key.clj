(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defoproperty s)
(defdproperty A)


(defclass C
  :haskey r s)

(save-all)
