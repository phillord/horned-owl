(clojure.core/load-file "ontology.clj")


(defclass A)
(defclass A1)
(defclass B)
(defclass B1)

(with-df
  #(clojure.core/let [
                     var (.getSWRLVariable
                          %
                          (iri-for-name o "x"))]
    (swrl_rule_set
     #{(.getSWRLClassAtom % A var)(.getSWRLClassAtom % A1 var)}
     #{(.getSWRLClassAtom % B var)(.getSWRLClassAtom % B1 var)})))

(save-all)
