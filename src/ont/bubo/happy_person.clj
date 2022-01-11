(clojure.core/load-file "ontology.clj")

(owl-import "http://homepages.cs.ncl.ac.uk/phillip.lord/scratch/family-other.owl#")

(defoproperty hasChild
  :characteristic :asymmetric)

(defclass HappyPerson)
(refine HappyPerson
        :equivalent (and (only hasChild HappyPerson)
                         (some hasChild HappyPerson)))


(save-all)
