(ns other
  (:use [tawny.owl]))

(defontology other
  :iri "http://www.example.com/other-iri"
  :viri "http://www.example.com/other-iri-viri"
  :noname true)

(defclass C)


(alias 'cc 'clojure.core)
(load-file "save.clj")
(save-all)
