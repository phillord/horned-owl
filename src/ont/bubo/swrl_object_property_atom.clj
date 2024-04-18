(clojure.core/load-file "ontology.clj")
(clojure.core/load-file "swrl_rule_support.clj")

(defoproperty r)
(defoproperty s)

(with-df
  #(clojure.core/let [
                      x (.getSWRLVariable
                            %
                            (iri-for-name o "x"))
                      y (.getSWRLVariable
                         %
                         (iri-for-name o "y"))]
    (swrl-rule
     (.getSWRLObjectPropertyAtom % r x y)
     (.getSWRLObjectPropertyAtom % s x y))))

(save-all)
