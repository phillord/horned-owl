(clojure.core/load-file "ontology.clj")

(defclass A)
(defoproperty r)

(defclass B :super (some r A))

(save-all)

