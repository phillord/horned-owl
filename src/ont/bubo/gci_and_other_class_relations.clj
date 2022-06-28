;; https://github.com/phillord/horned-owl/issues/43


(clojure.core/load-file "ontology.clj")


(defclass mononucleate)
(defclass nucleus)

(defoproperty bearer_of)
(defoproperty has_part)

;; This is the original bug report
(gci
 (owl-some bearer_of mononucleate)
 (owl-some has_part nucleus))


;; These two should have an equivalent problem
(add-equivalent
 o
 (owl-some bearer_of mononucleate)
 (owl-some has_part nucleus))

(add-disjoint
 o
 (owl-some bearer_of mononucleate)
 (owl-some has_part nucleus))

(save-all)
