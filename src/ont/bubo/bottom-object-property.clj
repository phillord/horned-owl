(cc/load-file "ontology.clj")

(defclass A
  :equivalent (some (.getOWLBottomObjectProperty (owl-data-factory))
                    (owl-thing)))

(save-all)
