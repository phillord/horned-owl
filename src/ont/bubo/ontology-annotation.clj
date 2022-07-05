(defontology o
  :iri "http://www.example.com/iri"
  :viri "http://www.example.com/viri"
  :comment "A comment"
  :annotation
  (annotation (iri "http://purl.org/dc/terms/created") "2021-12-09")
  (annotation (iri "http://purl.org/dc/terms/description") "Description annotation")
  :noname true)

(cc/load-file "save.clj")

(save-all)
