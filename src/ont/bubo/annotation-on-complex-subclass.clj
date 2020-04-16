(cc/load-file "ontology.clj")

(defclass A)
(defoproperty r)
(defclass B
  :super (annotate (some r A) (comment "Annotation on subclass axiom")))

(save-all)
