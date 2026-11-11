(defontology o
  :iri "http://www.example.com/iri"
  :viri "http://www.example.com/viri"
  :noname true)

(#'tawny.owl/add-ontology-annotation
 o
 (.getOWLAnnotation
  (tawny.owl/owl-data-factory)
  (tawny.owl/annotation-property (iri "http://www.example.com/iri/propformat-version"))
  (.getOWLLiteral (tawny.owl/owl-data-factory) "1.2")
  #{(.getOWLAnnotation
     (tawny.owl/owl-data-factory)
     (tawny.owl/annotation-property (iri "http://www.example.com/iri/note"))
     (.getOWLLiteral (tawny.owl/owl-data-factory) "Annotation on the annotation"))}))

(cc/load-file "save.clj")

(save-all)
