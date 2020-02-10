(clojure.core/load-file "ontology.clj")

(defaproperty a)
(defclass A
  :annotation (annotation a (literal "annotation")))

(save-all)
