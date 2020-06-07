(ns other
  (:use [tawny.owl]))

(defontology other
  :iri "http://www.example.com/other-iri"
  :viri "http://www.example.com/other-viri"
  :noname true)

(defoproperty other-o)


(alias 'cc 'clojure.core)
(load-file "save.clj")
(save-all)
