(defontology o
  :iri "http://www.example.com/iri"
  :viri "http://www.example.com/viri"
  :annotation
  (annotation (iri "http://www.geneontology.org/formats/oboInOwl#default-namespace")
              (literal "FlyBase miscellaneous CV"))
  :noname true)

(cc/load-file "save.clj")

(save-all)
