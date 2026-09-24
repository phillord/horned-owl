(defontology o
  :iri "http://www.example.com/iri"
  :annotation
  (annotation (iri "http://www.w3.org/2000/01/rdf-schema#comment")
              (literal "First line of the comment.\nSecond line.\nThird line."))
  :noname true)

(cc/load-file "save.clj")

(save-all)
