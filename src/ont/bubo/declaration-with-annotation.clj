(clojure.core/load-file "ontology.clj")

(owl-class
 (annotate
  (iri-for-name o "C")
  (owl-comment "Comment on Declaration" "en")))

(save-all)
;;(save :owl)

