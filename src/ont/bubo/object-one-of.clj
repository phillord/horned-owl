(cc/load-file "ontology.clj")

(defoproperty o)
(defindividual I)
(defindividual J)

(defclass C
  :super (oneof I J))

(save-all)
