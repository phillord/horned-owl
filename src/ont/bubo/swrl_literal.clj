(clojure.core/load-file "ontology.clj")
(clojure.core/load-file "swrl_rule_support.clj")

(defclass A)
(defdproperty d)

(with-df
  #(clojure.core/let [
                      var (.getSWRLVariable
                           %
                           (iri-for-name o "x"))
                      ]
     (swrl-rule
      (.getSWRLClassAtom % A var)
      (.getSWRLDataPropertyAtom % d
                                var
                                (.getSWRLLiteralArgument % (literal "Literal String"))))))

(save-all)
