(cc/load-file "ontology.clj")

(defoproperty t
  :characteristic [(annotate :transitive (label "Annotation on transitive"))])


(save-all)
