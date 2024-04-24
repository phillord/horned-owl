(clojure.core/load-file "ontology.clj")
(clojure.core/load-file "swrl_rule_support.clj")

(defclass A)
(defclass B)
(defindividual I)

(with-df
  #(clojure.core/let [
                      var (.getSWRLVariable
                           %
                           (iri-for-name o "x"))
                      ]
     (swrl-rule
      (.getSWRLClassAtom % A
                         (.getSWRLIndividualArgument % (anonymous-individual)))
      (.getSWRLClassAtom % B
                         (.getSWRLIndividualArgument % I)))))

(save-all)
