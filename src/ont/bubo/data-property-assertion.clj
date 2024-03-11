(cc/load-file "ontology.clj")

(defdproperty dp)

(defindividual I
  :fact (fact dp "A literal"))

(save-all)
