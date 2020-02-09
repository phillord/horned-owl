(defontology o
  :iri "http://www.example.com/iri"
  :viri "http://www.example.com/viri"
  :noname true)


(cc/defn save-one [dir ext format]
  (cc/let
      [file
       (cc/str
        "../" dir "/"
        (cc/first
         (clojure.string/split
          (cc/last
           (clojure.string/split
            cc/*file*
            #"[/]"))
          #"[.]"))
        ext)]
    (cc/println "\tSaving:" file)
    (save-ontology o file format)))

(cc/defn save-all []
  (save-one "owl-rdf" ".owl" :rdf)
  (save-one "owl-xml" ".owx" :owl)
  (save-one "owl-ttl" ".ttl" :ttl))
