(cc/load-file "ontology.clj")


(defoproperty op)
(defindividual I)
(defclass C
  :subclass (has-value op I))



(save-all)
