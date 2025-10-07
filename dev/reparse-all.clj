(clojure.core/use 'clojure.core)


(defn parse-file [parse-file format]
  (try
    (let [documentsource (org.semanticweb.owlapi.io.FileDocumentSource. parse-file)
          config (.get (org.semanticweb.owlapi.OWLAPIConfigProvider.))
          config (.setMissingImportHandlingStrategy config org.semanticweb.owlapi.model.MissingImportHandlingStrategy/SILENT)
          ontology
          (.createOntology
           (org.semanticweb.owlapi.apibinding.OWLManager/createOWLOntologyManager))
          ;; This is the format specific stuff
          parser
          (case format
            "owl-xml" (org.semanticweb.owlapi.owlxml.parser.OWLXMLParser.)
            "owl-rdf" (org.semanticweb.owlapi.rdf.rdfxml.parser.RDFXMLParser.)
            )]
      (.parse parser documentsource ontology config))
    (catch Exception e
      (println "Exit with error" parse-file e)
      (System/exit -1)))

  (println "Exit with success:" parse-file))

(def format-kind (nth tawny.bubo.cli/cmd-args 1))
(def file-list (.listFiles(clojure.java.io/file (format "./tmp/%s" format-kind))))


(doall
 (map
  #(parse-file %1 format-kind)
  (filter #(.isFile %)
          file-list)))


(println "Complete")
