(clojure.core/load-file "ontology.clj")


(defoproperty r)
(declare-classes B C D)

(defclass A
  :equivalent
  (some r B C D))

(save-all)
