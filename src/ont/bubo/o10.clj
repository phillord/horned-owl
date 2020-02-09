(cc/load-file "ontology.clj")

(cc/doseq [n (cc/range 1 11)]
  (owl-class (cc/str "n" n)))

(save-all)
