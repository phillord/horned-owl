(clojure.core/load-file "ontology.clj")

(defclass C
  :annotation
  (owl-comment "Comment on Declaration" "en")
  (label "Label on C"))

(save-all)
;;(save :owl)
