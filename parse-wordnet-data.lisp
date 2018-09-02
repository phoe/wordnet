;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

;;; The functions in this file take a string as read from a WordNet data or
;;; index file, and decode it.  Any structural representation of the result is
;;; done at a higher level.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; pointer types

(in-package #:wordnet)

(defparameter +wordnet-pointer-types+ nil)
(defparameter +wordnet-pointer-symbols+ nil)

(defun define-wordnet-pointer-type (name transitive-p direction
                                    &optional reverse-type)
  (pushnew name +wordnet-pointer-types+)
  (setf (get name 'transitive-relation-p) transitive-p)
  (when direction
    (setf (get name 'pointer-direction)
          (ecase direction (:up :up) (:down :down))))
  (when reverse-type
    (pushnew reverse-type +wordnet-pointer-types+)
    (setf (get name 'reverse-type) reverse-type)
    (setf (get reverse-type 'reverse-type) name)
    (setf (get reverse-type 'transitive-relation-p) transitive-p)
    (when direction
      (setf (get name 'upward-relation-p)
            (ecase direction (:up :down) (:down :up)))))
  name)

(defun wordnet-relation-p (thing)
  (member thing +wordnet-pointer-types+))

(defun define-wordnet-pointer-symbol (symbol part-of-speech definition)
  (assert (wordnet-relation-p definition))
  (push (list part-of-speech symbol definition) +wordnet-pointer-symbols+))

(progn
  ;; It's debatable which of these should or shouldn't be considered transitive.
  ;; I made a quick guess and could easily be wrong about some of these.  Let me
  ;; know what you think.
  (define-wordnet-pointer-type :also-see nil nil)
  (define-wordnet-pointer-type :antonym nil nil)
  (define-wordnet-pointer-type :attribute nil nil)
  (define-wordnet-pointer-type :cause :up nil)
  (define-wordnet-pointer-type :derived-from nil nil)
  (define-wordnet-pointer-type :entailment t :up)
  (define-wordnet-pointer-type :hypernym t :up :hyponym)
  (define-wordnet-pointer-type :substance-hypernym t :up :substance-hyponym)
  (define-wordnet-pointer-type :instance-hypernym t :up :instance-hyponym)
  (define-wordnet-pointer-type :member-holonym nil :up :member-meronym)
  (define-wordnet-pointer-type :part-holonym t :up :part-meronym)
  (define-wordnet-pointer-type :participle-of-verb nil nil)
  (define-wordnet-pointer-type :pertainym nil nil)
  (define-wordnet-pointer-type :similar-to nil nil)
  (define-wordnet-pointer-type :substance-holonym t :up :substance-meronym)
  (define-wordnet-pointer-type :verb-group nil nil)
  (define-wordnet-pointer-type :derivationally-related-form nil nil)
  (define-wordnet-pointer-type :domain-of-synset-topic nil :down :member-of-domain-topic)
  (define-wordnet-pointer-type :domain-of-synset-region nil :down :member-of-domain-region)
  (define-wordnet-pointer-type :domain-of-synset-usage nil :down :member-of-domain-usage)

  (define-wordnet-pointer-symbol "!" :noun :antonym)
  (define-wordnet-pointer-symbol "@" :noun :hypernym)
  (define-wordnet-pointer-symbol "~" :noun :hyponym)
  (define-wordnet-pointer-symbol "@i" :noun :substance-hypernym)
  (define-wordnet-pointer-symbol "~i" :noun :substance-hyponym)
  (define-wordnet-pointer-symbol "#m" :noun :member-meronym)
  (define-wordnet-pointer-symbol "#s" :noun :substance-meronym)
  (define-wordnet-pointer-symbol "#p" :noun :part-meronym)
  (define-wordnet-pointer-symbol "%m" :noun :member-holonym)
  (define-wordnet-pointer-symbol "%s" :noun :substance-holonym)
  (define-wordnet-pointer-symbol "%p" :noun :part-holonym)
  (define-wordnet-pointer-symbol "=" :noun :attribute)
  (define-wordnet-pointer-symbol "+" :noun :derivationally-related-form)
  (define-wordnet-pointer-symbol ";c" :noun :domain-of-synset-topic)
  (define-wordnet-pointer-symbol "-c" :noun :member-of-domain-topic)
  (define-wordnet-pointer-symbol ";r" :noun :domain-of-synset-region)
  (define-wordnet-pointer-symbol "-r" :noun :member-of-domain-region)
  (define-wordnet-pointer-symbol ";u" :noun :domain-of-synset-usage)
  (define-wordnet-pointer-symbol "-u" :noun :member-of-domain-usage)

  (define-wordnet-pointer-symbol "!" :verb :antonym)
  (define-wordnet-pointer-symbol "@" :verb :hypernym)
  (define-wordnet-pointer-symbol "~" :verb :hyponym)
  (define-wordnet-pointer-symbol "*" :verb :entailment)
  (define-wordnet-pointer-symbol ">" :verb :cause)
  (define-wordnet-pointer-symbol "^" :verb :also-see)
  (define-wordnet-pointer-symbol "$" :verb :verb-group)
  (define-wordnet-pointer-symbol "+" :verb :derivationally-related-form)
  (define-wordnet-pointer-symbol ";c" :verb :domain-of-synset-topic)
  (define-wordnet-pointer-symbol ";r" :verb :domain-of-synset-region)
  (define-wordnet-pointer-symbol ";u" :verb :domain-of-synset-usage)

  (define-wordnet-pointer-symbol "!" :adjective :antonym)
  (define-wordnet-pointer-symbol "&" :adjective :similar-to)
  (define-wordnet-pointer-symbol "<" :adjective :participle-of-verb)
  (define-wordnet-pointer-symbol "\\" :adjective :pertainym)
  (define-wordnet-pointer-symbol "=" :adjective :attribute)
  (define-wordnet-pointer-symbol "^" :adjective :also-see)
  (define-wordnet-pointer-symbol "+" :adjective :derivationally-related-form)
  (define-wordnet-pointer-symbol ";c" :adjective :domain-of-synset-topic)
  (define-wordnet-pointer-symbol ";r" :adjective :domain-of-synset-region)
  (define-wordnet-pointer-symbol ";u" :adjective :domain-of-synset-usage)

  (define-wordnet-pointer-symbol "!" :adverb :antonym)
  (define-wordnet-pointer-symbol "\\" :adverb :derived-from)
  (define-wordnet-pointer-symbol ";c" :adverb :domain-of-synset-topic)
  (define-wordnet-pointer-symbol ";r" :adverb :domain-of-synset-region)
  (define-wordnet-pointer-symbol ";u" :adverb :domain-of-synset-usage))

(defun decode-pointer-symbol-type (pointer-symbol part-of-speech)
  (dolist (pointer-entry +wordnet-pointer-symbols+)
    (when (and (eq part-of-speech (first pointer-entry))
               (string-equal pointer-symbol (second pointer-entry)))
      (return-from decode-pointer-symbol-type (third pointer-entry))))
  (error "Unknown pointer symbol: ~S for ~S" pointer-symbol part-of-speech))

(defmethod transitive-relation-p ((pointer-type symbol))
  (get pointer-type 'transitive-relation-p))

(defmethod relation-direction ((pointer-type symbol))
  (get pointer-type 'pointer-direction))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Index file entries

(defun parse-index-file-entry (entry)
  "Given a string as returned by INDEX-ENTRY-FOR-WORD, decode it and return the
elements of the index entry."
  (when entry
    (let ((stack (split-sequence:split-sequence #\Space entry :remove-empty-subseqs t))
          word part-of-speech poly_cnt pointer-types synset-offsets)
      (setq word (pop stack))
      (setq part-of-speech (part-of-speech-for-wordnet-db-token (pop stack)))
      (setq poly_cnt (parse-integer (pop stack) :junk-allowed t))
      (dotimes (i (parse-integer (pop stack))
                  (setq pointer-types (nreverse pointer-types)))
        (push (pop stack) pointer-types))
      (dotimes (i (parse-integer (prog1 (pop stack) (pop stack)))
                  (setq synset-offsets (nreverse synset-offsets)))
        (push (parse-integer (pop stack)) synset-offsets))
      (values word part-of-speech poly_cnt pointer-types synset-offsets))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data file entries

(defparameter +wordnet-gloss-character+ #\|)

(defun parse-data-file-entry (entry)
  "Given a string as returned by READ-DATA-FILE-ENTRY, representing a symset,
return the data."
  (let* ((gloss-index (position +wordnet-gloss-character+ entry :test #'string-equal))
         (gloss (when gloss-index (string-trim '(#\space) (subseq entry (1+ gloss-index)))))
         lex_file_num part-of-speech words pointers verb-frames
         (stack (split-sequence:split-sequence #\Space entry :remove-empty-subseqs t
                                                             :end gloss-index)))
    (pop stack)						;file offset check token
    (setq lex_file_num (pop stack))				;decimal integer
    (setq part-of-speech (part-of-speech-for-wordnet-db-token (pop stack)))
    (dotimes (i (parse-integer (pop stack) :radix 16)
                (setq words (nreverse words)))
      (let ((word (pop stack))
            (sense-number (parse-integer (pop stack) :radix 16)))
        (push (list word sense-number) words)))
    (dotimes (i (parse-integer (pop stack))
                (setq pointers (nreverse pointers)))
      (let* ((pointer (decode-pointer-symbol-type (pop stack) part-of-speech))
             (target (parse-integer (pop stack)))
             (part-of-speech (part-of-speech-for-wordnet-db-token (pop stack)))
             (source/target (parse-integer (pop stack) :radix 16))
             (source-index (ldb (byte 8 8) source/target))
             (target-index (ldb (byte 8 0) source/target)))
        (push (list pointer target part-of-speech source-index target-index)
              pointers)))
    (let ((frame-count (pop stack)))
      (when frame-count
        (dotimes (i (parse-integer frame-count)
                    (setq verb-frames (nreverse verb-frames)))
          (push (list (pop stack) (pop stack) (pop stack)) verb-frames))))
    (values part-of-speech words pointers gloss verb-frames lex_file_num)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Exception file entries

(defun parse-exception-file-entry (entry)
  (when entry
    (let* ((length (length entry))
           (index 0)
           (words nil))
      (flet ((next-space (entry index length)
               (or (position #\space entry :start index :test #'char-equal)
                   length)))
        (loop
          (setq index (next-space entry index length))
          (let ((s (next-space entry index length)))
            (when (= s index)
              (return))
            (push (subseq entry index s) words)
            (setq index s))))
      (setq words (nreverse words))
      (values (cdr words) (first words)))))
