;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

(asdf:defsystem #:WordNet
  :description "CommonLisp interface to WordNet"
  :author "Mark Nahabedian"
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "parts-of-speech")
               (:file "wordnet-database-files")
               (:file "parse-wordnet-data")
               (:file "representation")
               (:file "relationship-algorithms")))
