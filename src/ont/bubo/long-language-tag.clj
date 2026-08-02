(clojure.core/load-file "ontology.clj")

;; Regression fixture for https://github.com/phillord/horned-owl/issues/236:
;; a real BCP-47 language tag with a long (>4 char) unhyphenated variant
;; subtag right after the language, e.g. "en-scotland" (found in the
;; FOODON corpus ontology).
(defclass A :annotation (label "neep" "en-scotland"))

(save-all)
