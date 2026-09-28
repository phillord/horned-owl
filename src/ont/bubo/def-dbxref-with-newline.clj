(clojure.core/load-file "ontology.clj")

(defclass A
  :annotation
  (annotate
   (annotation (iri "http://purl.obolibrary.org/obo/IAO_0000115")
               (literal "A definition."))
   (annotation (iri "http://www.geneontology.org/formats/oboInOwl#hasDbXref")
               (literal "PMID:34805795\nPomBase:val"))))

(save-all)
