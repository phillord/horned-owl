(cc/load-file "ontology.clj")

(defclass A)
(defclass B
  :super (annotate A (comment "Annotation on subclass axiom")))

(save-all)
