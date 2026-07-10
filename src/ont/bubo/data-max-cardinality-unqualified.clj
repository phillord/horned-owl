(cc/load-file "ontology.clj")

(defdproperty d)
(defclass C
  :super (data-at-most 1 d))

(save-all)
