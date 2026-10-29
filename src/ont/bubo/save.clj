(cc/defn save-one [dir ext format]
  (cc/let
      [file
       (cc/str
        "../" dir "/"
        (cc/first
         (clojure.string/split
          (cc/first tawny.bubo.cli/cmd-args)
          #"[.]"))
        ext)]
      (cc/println "\tSaving:" file)
      (save-ontology file format)))

(cc/import org.semanticweb.owlapi.formats.FunctionalSyntaxDocumentFormat)
(cc/import org.semanticweb.owlapi.formats.OBODocumentFormat)

(cc/load-file "oml.clj")

(cc/defn save-all []
  (save-one "owl-rdf" ".owl" :rdf)
  (save-one "owl-xml" ".owx" :owl)
  (save-one "owl-ttl" ".ttl" :ttl)
  (save-one "owl-functional" ".ofn" (FunctionalSyntaxDocumentFormat.))
  (save-one "owl-manchester" ".omn" :omn)
  (save-one "owl-obo" ".obo" (OBODocumentFormat.))
  ;; Last, as it adds ontology annotations. Subdirectory scripts
  ;; (ambiguous, withimport) have no OML form.
  (cc/when-not (clojure.string/includes?
                (cc/first tawny.bubo.cli/cmd-args) "/")
    (save-oml)))
