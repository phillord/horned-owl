(clojure.core/load-file "ontology.clj")

(owl-import "http://purl.obolibrary.org/obo/bfo.owl")

(save-all)
