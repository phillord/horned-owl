(clojure.core/load-file "ontology.clj")

(defaproperty ann)

(defclass A
  :annotation
  (annotate
   (comment "Comment on Class")
   (annotation ann "Comment on Comment")))

(save-all)
