(cc/load-file "ontology.clj")

(defdatatype D
  :equivalent (data-not
               (iri "http://www.w3.org/2002/07/owl#rational")))

(save-all)
