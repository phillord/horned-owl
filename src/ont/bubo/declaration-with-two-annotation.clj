(clojure.core/load-file "ontology.clj")

(class
 (annotate
  (iri-for-name o "C")
  (label "Label on Declaration" "en")
  (comment "Comment on Declaration" "en")))

(save-all)
 ;;(save :owl)

 
