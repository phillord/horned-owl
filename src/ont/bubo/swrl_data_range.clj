(clojure.core/load-file "ontology.clj")
(clojure.core/load-file "swrl_rule_support.clj")


(with-df
  #(clojure.core/let [
                      var (.getSWRLVariable
                           %
                           (iri-for-name o "x"))
                      ]
     (swrl-rule
      (.getSWRLDataRangeAtom %
                             (#'tawny.owl/ensure-data-range (iri "http://www.w3.org/2001/XMLSchema#integer"))
                             (.getSWRLLiteralArgument % (literal "literal1")))
      (.getSWRLDataRangeAtom %
                             (#'tawny.owl/ensure-data-range (iri "http://www.w3.org/2001/XMLSchema#real"))
                             (.getSWRLLiteralArgument % (literal "literal2"))))))


(save-all)
