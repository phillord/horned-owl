(cc/load-file "ontology.clj")

(defclass A)
(defclass B :super A)

(save-all)
