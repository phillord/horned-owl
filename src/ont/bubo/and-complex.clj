(clojure.core/load-file "ontology.clj")

(declare-classes B C D)
(defoproperty r)
(defclass A :super
  (and
   (owl-some r B)
   (owl-only r C)
   D
   ))

(save-all)
