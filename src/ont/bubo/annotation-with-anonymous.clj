(clojure.core/load-file "ontology.clj")

;; There is no nice syntax for this in tawny.
(add-axiom
 o
 (.getOWLAnnotationAssertionAxiom
  (owl-data-factory)
  (anonymous-individual)
  (owl-comment "fred")
  ))


;; <AnnotationAssertion>
;;     <AnnotationProperty IRI="http://purl.obolibrary.org/obo/IAO_0000412"/>
;;     <AnonymousIndividual nodeID="_:genid2147502004"/>
;;     <IRI>http://purl.obolibrary.org/obo/uberon.owl</IRI>
;; </AnnotationAssertion>
;; <AnnotationAssertion>
;;     <AnnotationProperty abbreviatedIRI="owl:minQualifiedCardinality"/>
;;     <AnonymousIndividual nodeID="_:genid2147502005"/>
;;     <Literal datatypeIRI="http://www.w3.org/2001/XMLSchema#nonNegativeInteger">2</Literal>
;; </AnnotationAssertion>

(save-all)
