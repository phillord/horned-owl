;; https://github.com/phillord/horned-owl/issues/124

(clojure.core/load-file "ontology.clj")

(defoproperty r)
(declare-classes A B)
(defindividual A)
(defindividual B
  :fact (fact r A))



(save-all)
