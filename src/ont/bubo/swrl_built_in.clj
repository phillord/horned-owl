(clojure.core/load-file "ontology.clj")
(clojure.core/load-file "swrl_rule_support.clj")

(defclass A)
(defclass B)

(with-df
  #(clojure.core/let [
                      var (.getSWRLVariable
                           %
                           (iri-for-name o "x"))
                      ]
     (swrl-rule
      (.getSWRLBuiltInAtom %
                           (iri-for-name o "y")
                           (clojure.core/list
                            (.getSWRLLiteralArgument % (literal "literal1"))
                            (.getSWRLLiteralArgument % (literal "literal2"))))
      (.getSWRLClassAtom % B var))))

(save-all)
