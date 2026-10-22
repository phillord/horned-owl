(clojure.core/load-file "ontology.clj")

(defclass A
  :annotation
  (annotate
   (comment "Same value")
   (comment "First"))
  (annotate
   (comment "Same value")
   (comment "Second")))

(save-all)
