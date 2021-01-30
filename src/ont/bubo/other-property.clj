(ns other
  (:use [tawny.owl]))

(defontology other
  :iri "http://www.example.com/other-property"
  :viri "http://www.example.com/other-property-viri"
  :noname true)

(defoproperty other-o)


(alias 'cc 'clojure.core)
(load-file "save.clj")
(save-all)
