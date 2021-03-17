(clojure.core/load-file "ontology.clj")

(declare-classes B C D)
(defclass A :equivalent B C D)

(save-all)
