(clojure.core/load-file "ontology.clj")


(defclass A)
(defclass A1)
(defclass B)
(defclass B1)

(clojure.core/let [df (owl-data-factory)
                   var (.getSWRLVariable
                        df
                        (iri-for-name o "x"))]
  (.applyChange
   (owl-ontology-manager)
   (org.semanticweb.owlapi.model.AddAxiom.
    o
    (.getSWRLRule
     df
     #{(.getSWRLClassAtom df A var)(.getSWRLClassAtom df A1 var)}
     #{(.getSWRLClassAtom df B var)(.getSWRLClassAtom df B1 var)}))))



(save-all)
