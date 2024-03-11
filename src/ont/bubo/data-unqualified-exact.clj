(cc/load-file "ontology.clj")

(defdproperty d)
(defclass C
  :super (data-exactly 1 d))

(save-all)
