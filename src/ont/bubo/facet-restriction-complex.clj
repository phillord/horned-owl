(clojure.core/load-file "ontology.clj")

(defdproperty r)

(defclass C
  :super
  (owl-some r (min-max 10 20)))

(save-all)
