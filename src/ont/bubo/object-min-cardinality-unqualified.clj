(clojure.core/load-file "ontology.clj")

(defoproperty r)
(defclass C :subclass (at-least 1 r))

(save-all)
