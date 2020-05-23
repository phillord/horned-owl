(clojure.core/load-file "ontology.clj")

(defdproperty r)

(defclass C
  :super
  (owl-some r (> 10)))

(save-all)
