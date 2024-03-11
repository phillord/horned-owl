(cc/load-file "ontology.clj")

(defdproperty dp)
(defdproperty dp1 :disjoint dp)

(save-all)
