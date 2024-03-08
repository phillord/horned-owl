(cc/load-file "ontology.clj")

(defoproperty op)

(defclass C
  :super (has-self op))

(save-all)
