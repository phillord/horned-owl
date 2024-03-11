(cc/load-file "ontology.clj")

(defdatatype D
  :equivalent (data-oneof 10 20 30))

(save-all)
