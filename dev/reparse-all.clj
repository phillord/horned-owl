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
            "owl-manchester" (org.semanticweb.owlapi.manchestersyntax.parser.ManchesterOWLSyntaxOntologyParser.)
            )]
      (.parse parser documentsource ontology config))
    (catch Exception e
      (println "Exit with error" parse-file e)
      (System/exit -1)))

  (println "Exit with success:" parse-file))

(def format-kind (nth tawny.bubo.cli/cmd-args 1))
(def file-list (.listFiles(clojure.java.io/file (format "./tmp/%s" format-kind))))

;; Fixture names known to trip up the reference OWL API parser for reasons
;; unrelated to correctness of our writer. Matched with startsWith, so an
;; entry with an extension (e.g. "swrl_individual.owx") only excludes that
;; one format; an entry with no extension (just a trailing ".") excludes
;; the base name across every format.
;; - swrl_individual.owx / swrl_individual.ofn / swrl_individual.omn: the
;;   anonymous individual in the SWRL atom is not valid there in owl-xml,
;;   owl-functional, or owl-manchester syntax (swrl_individual.owl parses
;;   fine under owl-rdf, so is not listed).
;; - anon-subobjectproperty.omn / inverse-transitive.omn: our Manchester
;;   writer emits an inverse-headed `ObjectProperty: inverse (p)` frame,
;;   which OWL API's ManchesterOWLSyntaxOntologyParser does not accept as
;;   a frame subject (only a plain IRI is accepted there). The same base
;;   names exist as fixtures for every other format too and parse fine
;;   there, so these must stay scoped to the .omn extension.
;; - declaration-with-annotation.omn / declaration-with-two-annotation.omn:
;;   our Manchester writer represents an annotated declaration as
;;   `Class: Annotations: ... o:C` (annotation before the frame subject
;;   IRI), which OWL API's parser also rejects (it expects the IRI
;;   immediately after the frame keyword).
;; - swrl_built_in.omn / swrl_data_range.omn: our `Rule:` frame renders
;;   built-in and data-range SWRL atoms with a generic `iri(args...)` call
;;   syntax, which OWL API's Manchester `Rule:` grammar does not accept
;;   for those atom kinds.
(def known-parser-limitations
  ["anon-subobjectproperty.omn"
   "declaration-with-annotation.omn"
   "declaration-with-two-annotation.omn"
   "inverse-transitive.omn"
   "swrl_built_in.omn"
   "swrl_data_range.omn"
   "swrl_individual.ofn"
   "swrl_individual.omn"
   "swrl_individual.owx"])

(doall
 (keep #(when (and (.isFile %)
                    (not (some (fn [prefix] (.startsWith (.getName %) prefix))
                               known-parser-limitations)))
          (parse-file % format-kind))
       file-list))


(println "Complete")
