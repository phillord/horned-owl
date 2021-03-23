(clojure.core/load-file "ontology.clj")

(declare-classes B)
(defclass A :equivalent B)

(save-all)
