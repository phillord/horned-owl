(clojure.core/load-file "ontology.clj")

(defclass A
  :annotation
  (annotation (iri "http://www.geneontology.org/formats/oboInOwl#hasDbXref")
              (literal "KEGG COMPOUND:C00395")))

(save-all)
