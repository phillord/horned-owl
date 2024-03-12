(cc/load-file "ontology.clj")

(defclass C)
(defdproperty dp
  :domain C)

(save-all)
