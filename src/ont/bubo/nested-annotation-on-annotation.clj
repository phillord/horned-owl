(clojure.core/load-file "ontology.clj")

;; tawny-owl has no syntax for annotating an annotation (annotationAnnotations
;; in OWL 2 spec), so use the OWL API directly.
;; See https://github.com/phillord/horned-owl/issues/175
(defclass A)

(clojure.core/let
 [df        (owl-data-factory)
  prop      (.getRDFSComment df)
  comment-on-comment (.getOWLLiteral df "Comment on Comment" "en")
  nested-comment-on-comment (.getOWLLiteral df "Nested Comment" "en")
  comment-on-class    (.getOWLLiteral df "Comment on Class" "en")
  inner-ann    (.getOWLAnnotation df prop nested-comment-on-comment)
  outer-ann (.getOWLAnnotation df prop comment-on-comment #{inner-ann})]
 (add-axiom o
  (.getOWLAnnotationAssertionAxiom df
   prop
   (iri-for-name o "A")
   comment-on-class
   #{outer-ann})))

(save-all)
