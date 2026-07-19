(defontology o
  :iri "http://www.example.com/iri"
  :viri "http://www.example.com/viri"
  :noname true
  :import "http://www.example.com/other-iri"
  :annotation (label "An ontology with both an import and an ontology annotation"))

(cc/load-file "save.clj")
(save-all)
