(clojure.core/load-file "ontology.clj")

(defclass A
  :annotation
  (annotate
   (comment "Comment on Class")
   (comment "Comment on Comment")))


(save-all)
