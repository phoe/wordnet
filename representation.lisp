;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

;;; Representation of WordNet data.  Uses the lower layers defined in
;;; "wordnet-database-files" and "parse-wordnet-data" to extract data from the
;;; WordNet database files and then constructs an object oriented
;;; representation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Representation

(in-package #:wordnet)

(defclass wordnet-object () ())

(defclass wordnet-index-entry (wordnet-object)
  ((word :initarg :word :reader index-entry-word)
   (part-of-speech :initarg :part-of-speech
                   :reader part-of-speech)
   (synset-offsets :initarg :synset-offsets
                   :reader index-entry-synset-offsets)))

(defmethod print-object ((object wordnet-index-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a ~a"
            (ignore-errors (part-of-speech object))
            (ignore-errors (index-entry-word object)))))

(defclass wordnet-synset-entry (wordnet-object)
  ((part-of-speech :initarg :part-of-speech :reader part-of-speech)
   (offset :initarg :offset :reader synset-offset)
   (words :initarg :words :initform nil :reader synset-words)
   (raw-pointers :initarg :pointers :initform nil)
   (pointers)
   (gloss :initarg :gloss :initform nil :reader synset-gloss)))

(defmethod print-object ((object wordnet-synset-entry) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (dolist (w (ignore-errors (synset-words object)))
      (format stream "~a " (car w)))))

(defclass wordnet-noun-entry (wordnet-synset-entry) ())
(defclass wordnet-adjective-entry (wordnet-synset-entry) ())
(defclass wordnet-adverb-entry (wordnet-synset-entry) ())
(defclass wordnet-verb-entry (wordnet-synset-entry)
  ((verb-frames :initarg :verb-frames :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constructing the objects

;;; The objects we've made are kept in a cache so that if we ever get a request
;;; for the same object again, we can fetch the existing one rather than read
;;; the WordNet files again and make a new one.

(defvar *wordnet-index-cache*
  (mapcar #'(lambda (pos) (list pos (make-hash-table :test #+Genera 'string=
                                                           #-Genera 'equal)))
          (parts-of-speech)))

(defvar *wordnet-synset-cache*
  (mapcar #'(lambda (pos)
              (list pos (make-hash-table :test 'equal)))
          (parts-of-speech)))

(defun cached-index-lookup (word part-of-speech)
  "Looks up the entries for word (a string or a symbol) for the specified 
part-of-speech."
  (let* ((table (second (assoc part-of-speech *wordnet-index-cache*)))
	 (index-cache-entry (gethash word table)))
    (unless index-cache-entry
      (multiple-value-bind (word part-of-speech poly_cnt pointer-types synset-offsets)
	  (parse-index-file-entry
	   (index-entry-for-word part-of-speech word))
	(declare (ignore poly_cnt pointer-types))
	(when word
	  (setq index-cache-entry (make-instance 'wordnet-index-entry
						 :word word
						 :part-of-speech part-of-speech
						 :synset-offsets synset-offsets))
	  (setf (gethash word table) index-cache-entry))))
    index-cache-entry))

(defun cached-data-lookup (synset-index part-of-speech)
  (let* ((table (second (assoc part-of-speech *wordnet-synset-cache*)))
         (synset-entry (gethash synset-index table)))
    (unless synset-entry
      (multiple-value-bind (part-of-speech words pointers gloss verb-frames)
          (parse-data-file-entry (read-data-file-entry part-of-speech synset-index))
        (setq synset-entry
              (apply #'make-instance
                     (ecase part-of-speech
                       (:noun 'wordnet-noun-entry)
                       (:verb 'wordnet-verb-entry)
                       (:adjective 'wordnet-adjective-entry)
                       (:adverb 'wordnet-adverb-entry))
                     :offset synset-index
                     :part-of-speech part-of-speech
                     :words words
                     :pointers pointers
                     :gloss gloss
                     (when (eq part-of-speech :verb)
                       (list :verb-frames verb-frames)))))
      (setf (gethash synset-index table) synset-entry))
    synset-entry))

(defun index-entry-synsets (index-entry)
  (when index-entry
    (mapcar #'(lambda (offset)
                (cached-data-lookup offset (part-of-speech index-entry)))
            (index-entry-synset-offsets index-entry))))

(defun morphology-exception-lookup (word part-of-speech)
  (parse-exception-file-entry (exception-entry-for-word part-of-speech word)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Relationships among words and synsets

;;; I don't have a good idea about what the best way to represent WordNet
;;; pointers is.  Let's try this.

(defclass wordnet-pointer (wordnet-object)
  ((type :initarg :type
         :reader wordnet-pointer-type)
   (from-synset :initarg :from
                :reader wordnet-pointer-from-synset)
   (from-word-index :initarg :from-index
                    :reader wordnet-pointer-from-synset-index)
   (to-synset :initarg :to
              :reader wordnet-pointer-to-synset)
   (to-word-index :initarg :to-index
                  :reader wordnet-pointer-to-synset-index)))

(defmethod print-object ((object wordnet-pointer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a" (ignore-errors (wordnet-pointer-type object)))))

(defmethod transitive-relation-p ((pointer wordnet-pointer))
  (transitive-relation-p (wordnet-pointer-type pointer)))

(defmethod relation-direction ((pointer wordnet-pointer))
  (relation-direction (wordnet-pointer-type pointer)))

(defmethod wordnet-pointer-from-word ((pointer wordnet-pointer))
  (with-slots (from-synset from-word-index) pointer
    (if (zerop from-word-index)
        from-synset
        (elt (synset-words from-synset) (1- from-word-index)))))

(defmethod wordnet-pointer-to-word ((pointer wordnet-pointer))
  (with-slots (to-synset to-word-index) pointer
    (if (zerop to-word-index)
        to-synset
        (elt (synset-words to-synset) (1- to-word-index)))))

(defmethod reify-pointers ((synset wordnet-synset-entry))
  (with-slots (raw-pointers pointers) synset
    (let ((new-pointers nil))
      (dolist (p raw-pointers)
        (destructuring-bind (pointer-type target part-of-speech
                             source-index target-index) p
          (let ((to-synset (cached-data-lookup target part-of-speech)))
            (unless to-synset
              (error "Pointer ~s has invalid target" p))
            (push (make-instance 'wordnet-pointer
                                 :type pointer-type
                                 :from synset
                                 :from-index source-index
                                 :to to-synset
                                 :to-index target-index)
                  new-pointers))))
      (setq pointers (nreverse new-pointers)))))

(defmethod wordnet-pointers ((synset wordnet-synset-entry))
  (unless (slot-boundp synset 'pointers)
    (reify-pointers synset))
  (slot-value synset 'pointers))

(defmacro do-synset-pointers ((pointer-var synset &optional (pointer-types nil pt?))
                              &body body)
  (assert (symbolp pointer-var))
  `(dolist (,pointer-var (wordnet-pointers ,synset))
     (when ,@(if pt?
                 `((member (wordnet-pointer-type ,pointer-var)
                           ,pointer-types))
                 t)
       ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fancy printing

(defun pretty-print-synset (stream synset &key (word-index 0) gloss word-sense-tags)
  (flet ((call-with-text-face (stream face function)
           #+Genera
           (typecase stream
             (clim-internals::output-protocol-mixin
              (clim:with-text-face (stream face)
                (funcall function stream)))
             (t (scl:with-character-face (face stream)
                  (funcall function stream))))
           #-Genera
           (funcall function stream))
         (print-pos (stream)
           (write-string
            (ecase (part-of-speech synset)
              (:noun "n") (:verb "v") (:adjective "adj") (:adverb "adv"))
            stream))
         (print-word+sense (word+sense stream)
           (if word-sense-tags
               (write-string (first word+sense) stream)
               (format stream "~a:~d" (first word+sense) (second word+sense)))))
    (call-with-text-face stream :italic #'print-pos)
    (write-char #\{ stream)
    (do* ((words (synset-words synset) (cdr words))
          (word+sense (car words) (car words))
          (index 1 (1+ index)))
         ((null words))
      (if (= index word-index)
          (call-with-text-face stream :bold
                               #'(lambda (stream)
                                   (print-word+sense word+sense stream)))
          (print-word+sense word+sense stream))
      (unless (null words)
        (write-char #\space stream)))
    (when gloss
      (let ((gloss (synset-gloss synset)))
        (when gloss
          (format stream " | ~a" gloss))))
    (write-char #\} stream))
  synset)
