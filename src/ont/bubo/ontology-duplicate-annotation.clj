(defontology o
  :iri "http://www.example.com/iri"
  :viri "http://www.example.com/viri"
  :annotation
  (annotation (iri "http://www.w3.org/2002/07/owl#versionInfo") (literal "first"))
  (annotation (iri "http://www.w3.org/2002/07/owl#versionInfo") (literal "second"))
  :noname true)

(cc/load-file "save.clj")

(save-all)
