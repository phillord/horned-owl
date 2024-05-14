(cc/load-file "ontology.clj")

;; This test is not designed to check handling of equivalent classes
;; per se, but the handling of axioms with an arbitrary number of
;; elements. These are handled differently in RDF and previously
;; annotations were not being rendered but just ignored.

(declare-classes A B C)

(defclass D :equivalent
  (annotate
   [A B C]
   (comment "This is an annotation on an axiom")))

(save-all)
