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

(cc/defn save-all []
  (save-one "owl-rdf" ".owl" :rdf)
  (save-one "owl-xml" ".owx" :owl)
  (save-one "owl-ttl" ".ttl" :ttl)
  (save-one "owl-functional" ".ofn" (FunctionalSyntaxDocumentFormat.)))
