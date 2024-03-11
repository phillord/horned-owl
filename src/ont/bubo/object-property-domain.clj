(cc/load-file "ontology.clj")

(defclass C)
(defoproperty r :domain C)

(save-all)
