(clojure.core/load-file "ontology.clj")
(clojure.core/load-file "swrl_rule_support.clj")

(defindividual I)
(defindividual J)

(with-df
  #(clojure.core/let [Iarg (.getSWRLIndividualArgument % I)
                      Jarg (.getSWRLIndividualArgument % J)]
     (swrl-rule
      (.getSWRLSameIndividualAtom % Iarg Jarg)
      (.getSWRLSameIndividualAtom % Jarg Iarg))))

(save-all)
