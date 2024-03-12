(clojure.core/load-file "ontology.clj")


(defclass A)
(defclass B)
(defclass C)

(add-disjoint-union
 o A (cc/list B C))

(save-all)
