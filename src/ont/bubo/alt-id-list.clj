(clojure.core/load-file "ontology.clj")

(defclass A
  :annotation
  (annotation (iri "http://www.geneontology.org/formats/oboInOwl#hasAlternativeId")
              (literal "CHEBI:41526, CHEBI:3282")))

(save-all)
