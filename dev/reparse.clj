(clojure.core/use 'clojure.core)

(def parse-file
  (clojure.java.io/file
   (nth tawny.bubo.cli/cmd-args 1)))
(def resource-file (nth tawny.bubo.cli/cmd-args 2))

;; (println "Reparsing" parse-file)

;; (println "as resource" (clojure.java.io/file parse-file))
;; (println "as iri" (iri
;;                    (clojure.java.io/file parse-file)))


;;	parse(OWLOntologyDocumentSource documentSource, OWLOntology ontology, OWLOntologyLoaderConfiguration configuration)


(try
  (let [documentsource (org.semanticweb.owlapi.io.FileDocumentSource. parse-file)
        config (.get (org.semanticweb.owlapi.OWLAPIConfigProvider.))
        ontology
        (.createOntology
         (org.semanticweb.owlapi.apibinding.OWLManager/createOWLOntologyManager))
        ;; This is the format specific stuff
        parser (org.semanticweb.owlapi.owlxml.parser.OWLXMLParser.)
        ]
    (.parse parser documentsource ontology config))
  (catch Exception e
    (println "Exit with error" resource-file e)
    (System/exit -1)))

(println "Exit with success:" resource-file)
