(cc/load-file "ontology.clj")

(defdatatype Percentage)
(defdproperty load)
(defclass A
  :super (only load (data-or Percentage
                             (data-oneof 10 20))))
(defclass B
  :super (some load (data-and Percentage
                              (iri "http://www.w3.org/2001/XMLSchema#decimal"))))
(defclass C
  :super (only load (data-or Percentage
                             (data-not (iri "http://www.w3.org/2001/XMLSchema#decimal")))))

(save-all)
