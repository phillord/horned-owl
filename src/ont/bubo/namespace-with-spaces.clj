(clojure.core/load-file "ontology.clj")

(defclass A
  :annotation
  (annotation (iri "http://www.geneontology.org/formats/oboInOwl#hasOBONamespace")
              (literal "FlyBase miscellaneous CV")))

(save-all)
