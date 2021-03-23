(clojure.core/load-file "ontology.clj")

(as-disjoint
 (defclass A)
 (defclass B))

(save-all)
