;; Save a copy of the ontology carrying the OML markers that owl2oml needs.
;;
;; owl2oml picks the OML kind of an ontology from oml:type and reads the
;; namespace and prefix from oml:namespace and oml:prefix; without them it
;; converts nothing. The markers are removed again afterwards, as some
;; scripts save the ontology after save-all.

(cc/import 'org.semanticweb.owlapi.model.AddOntologyAnnotation)
(cc/import 'org.semanticweb.owlapi.model.RemoveOntologyAnnotation)

(cc/defn oml-markers []
  (cc/for [[property value]
           [["type" (iri "http://opencaesar.io/oml#Vocabulary")]
            ["namespace" (iri "http://www.example.com/iri#")]
            ["prefix" (literal "o")]]]
    (annotation (iri (cc/str "http://opencaesar.io/oml#" property)) value)))

(cc/defn save-oml []
  (cc/let [markers (oml-markers)
           change (cc/fn [f]
                    (cc/doseq [m markers]
                      (.applyChange (owl-ontology-manager) (f o m))))]
    (change #(AddOntologyAnnotation. %1 %2))
    (try
      (save-one "oml-annotated" ".ttl" :ttl)
      (finally
        (change #(RemoveOntologyAnnotation. %1 %2))))))
