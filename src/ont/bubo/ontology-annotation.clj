(defontology o
  :iri "http://www.example.com/iri"
  :viri "http://www.example.com/viri"
  :annotation
  (annotation (iri "http://www.example.com/iri/propformat-version") "1.2")
  :noname true)

(cc/load-file "save.clj")

(save-all)
