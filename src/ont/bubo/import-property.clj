(clojure.core/load-file "ontology.clj")

(cc/load-file "other-property.clj")
(owl-import other/other)

(defclass A)
(defclass B :super (some other/other-o A))

(save-all)

