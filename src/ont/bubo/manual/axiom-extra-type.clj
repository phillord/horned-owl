;; Writes ../owl-xml/manual/axiom-extra-type.owx: the OWL API's lax-mode
;; reading of ../owl-ttl/manual/axiom-extra-type.ttl. The OWL API cannot
;; write an extra rdf:type on an owl:Axiom node, so the Turtle is
;; hand-written and the OWL API is only the oracle for reading it.
;; Run from this directory: bubo-0.4.0 script manual/axiom-extra-type.clj
(cc/import
 '(org.semanticweb.owlapi.apibinding OWLManager)
 '(org.semanticweb.owlapi.formats OWLXMLDocumentFormat)
 '(org.semanticweb.owlapi.model IRI))

(cc/let [m (OWLManager/createOWLOntologyManager)
         o (.loadOntologyFromOntologyDocument
            m (java.io.File. "../owl-ttl/manual/axiom-extra-type.ttl"))]
  (.saveOntology m o (OWLXMLDocumentFormat.)
                 (IRI/create (.toURI (java.io.File. "../owl-xml/manual/axiom-extra-type.owx")))))
