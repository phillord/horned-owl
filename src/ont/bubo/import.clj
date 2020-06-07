(clojure.core/load-file "ontology.clj")

(cc/load-file "other.clj")
(owl-import other/other)

(save-all)
