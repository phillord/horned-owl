(clojure.core/load-file "ontology.clj")

(owl-class (iri "http://purl.obolibrary.org/obo/LFID_0000006")
  :annotation
  (annotation (iri "http://www.geneontology.org/formats/oboInOwl#id")
              (literal "use of codeine")))

(save-all)
