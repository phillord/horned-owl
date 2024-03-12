(cc/load-file "ontology.clj")

(defdproperty dp)

(defindividual I
  :fact (not dp (literal "A literal")))

(save-all)
