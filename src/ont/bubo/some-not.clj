(clojure.core/load-file "ontology.clj")

(defclass A)
(defoproperty r)

(defclass B :super (some r (not A)))

(save-all)
(save :omn)
