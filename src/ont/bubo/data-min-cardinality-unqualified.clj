(cc/load-file "ontology.clj")

(defdproperty d)
(defclass C
  :super (data-at-least 1 d))

(save-all)
