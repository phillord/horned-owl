(cc/load-file "ontology.clj")

(defdatatype D
  :equivalent (data-and
               (iri "http://www.w3.org/2002/07/owl#rational")
               (iri "http://www.w3.org/2002/07/owl#real")))

(save-all)
