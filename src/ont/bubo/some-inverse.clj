(cc/load-file "ontology.clj")

(defclass A)
(defoproperty r)

(defclass B
  :super (some (inverse r) A))

(save-all)

