(clojure.core/load-file "ontology.clj")

(declare-classes B C D)
(defclass A :super (or B C D))

(save-all)
