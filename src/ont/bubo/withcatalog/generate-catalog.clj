(ns gencatalog
  (:use [tawny.owl]))

(clojure.core/alias 'cc 'clojure.core)

(cc/load-file "withimport/other-property.clj")

(cc/import 'org.semanticweb.owlapi.util.OWLZipSaver)
(cc/import 'java.util.ArrayList)
(cc/import 'java.io.FileWriter)

;; OWLZipSaver.entryPath's own default just returns the ontology IRI
;; verbatim (real behaviour, confirmed by reading OWLZipSaver.java --
;; it's meant for zip-archive entries keyed by IRI, not filesystem
;; redirect paths). setEntryPath is the library's own supported
;; customisation point for exactly this: telling it what local path an
;; ontology should resolve to. Everything else -- the XML header, the
;; <group>/<uri> structure, attribute escaping -- is untouched real
;; OWLZipSaver.catalogIndex() output.
(cc/let [saver (OWLZipSaver.)
         _ (.setEntryPath saver (cc/reify java.util.function.Function (apply [_this id] "imports/other-property.owl")))
         xml (.catalogIndex saver (ArrayList.) (ArrayList. [other/other]))]
  (cc/println "----CATALOG-XML-START----")
  (cc/println xml)
  (cc/println "----CATALOG-XML-END----")
  (cc/with-open [w (FileWriter. "../owl-rdf/withcatalog/catalog-v001.xml")]
    (.write w xml)))
