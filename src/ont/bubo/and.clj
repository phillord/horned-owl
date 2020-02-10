(clojure.core/load-file "ontology.clj")

(declare-classes B C D)
(defclass A :super (and B C D))

(save-all)
