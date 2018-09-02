;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

(defpackage #:wordnet
  (:nicknames #:wn)
  (:use #:common-lisp)
  (:export
   ;; file "parts-of-speech"
   #:canonicalize-part-of-speech
   #:do-parts-of-speech
   #:parts-of-speech

   ;; file "wordnet-database-files"
   #:exception-entry-for-word
   #:index-entry-for-word
   #:read-data-file-entry

   ;; file "parse-wordnet-data"
   #:parse-data-file-entry
   #:parse-exception-file-entry
   #:parse-index-file-entry

   ;; file "representation"
   #:cached-data-lookup
   #:cached-index-lookup
   #:index-entry-synsets
   #:index-entry-word
   #:wordnet-index-entry
   #:wordnet-synset-entry
   #:wordnet-noun-entry
   #:wordnet-adjective-entry
   #:wordnet-adverb-entry
   #:wordnet-verb-entry
   #:wordnet-pointer
   #:wordnet-pointers
   #:wordnet-pointer-type
   #:wordnet-pointer-from-synset
   #:wordnet-pointer-from-synset-index
   #:wordnet-pointer-to-synset
   #:wordnet-pointer-to-synset-index
   #:pretty-print-synset
   #:part-of-speech
   #:synset-words
   #:wordnet-pointer-from-word
   #:wordnet-pointer-to-word

   ;; file "relationship-algorithms"
   #:relation-transitive-closure
   #:commonality

   ;; file "examples"
   #:synsets-containing-words
   #:get-synonyms
   #:get-antonyms
   #:find-synset-with-sense
   #:wordnet-describe
   ))

;; TODO move to wordnet/clim system
#+CLIM
(defpackage #:wordnet-interface
  (:nicknames #:wni)
  (:use #:wordnet #:clim #:clim-lisp)
  (:export
   "WORDNET-BROWSER"
   ))
