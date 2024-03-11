(cc/load-file "ontology.clj")

(defoproperty t
  :characteristic [(annotate :transitive
                             (label "Annotation on transitive")
                             (label "Second Annotation")
                             )])

(save-all)
