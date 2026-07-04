(clojure.core/use 'clojure.core)


(defn parse-file [parse-file format]
  (try
    (let [documentsource (org.semanticweb.owlapi.io.FileDocumentSource. parse-file)
          config (-> (org.semanticweb.owlapi.model.OWLOntologyLoaderConfiguration.)
                     (.setMissingImportHandlingStrategy org.semanticweb.owlapi.model.MissingImportHandlingStrategy/SILENT))
          ontology
          (.createOntology
           (org.semanticweb.owlapi.apibinding.OWLManager/createOWLOntologyManager))
          ;; This is the format specific stuff
          parser
          (case format
            "owl-xml" (org.semanticweb.owlapi.owlxml.parser.OWLXMLParser.)
            "owl-rdf" (org.semanticweb.owlapi.rdf.rdfxml.parser.RDFXMLParser.)
            "owl-functional" (org.semanticweb.owlapi.functional.parser.OWLFunctionalSyntaxOWLParser.)
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
  (filter #(and
            ;; For some reason swrl_individual cannot be parsed by
            ;; OWL API even when it is produced by the OWL API (in
            ;; owl-xml or owl-functional syntax; the anonymous
            ;; individual in the SWRL atom is not valid there). So,
            ;; filter this out for the moment.
            (not
             (.startsWith (.getName %) "swrl_individual."))
            (.isFile %))
          file-list)))


(println "Complete")
