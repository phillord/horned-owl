(clojure.core/load-file "ontology.clj")


(defclass A)
(defclass B)

(with-df
  #(clojure.core/let [
                      var (.getSWRLVariable
                           %
                           (iri-for-name o "x"))
                      ]
     (swrl-rule
      (.getSWRLClassAtom % A var)
      (.getSWRLClassAtom % (and A B) var))))

(save-all)
