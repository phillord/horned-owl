(cc/load-file "ontology.clj")

(defdproperty d)
(defclass C
  :super (has-value d "A Literal"))

(save-all)
