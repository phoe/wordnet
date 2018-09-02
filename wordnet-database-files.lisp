;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

;;; Code for reading WordNet "data" and "index" files as output by WordNet's
;;; "grind" command.  These functions merely return the requested entry as a
;;; string. These are other functions which will parse those strings and extract
;;; the data.

;;; This file provides the functions INDEX-ENTRY-FOR-WORD and
;;; READ-DATA-FILE-ENTRY which are used to read raw data from WordNet's compiled
;;; database files.  You must tell them which file to read from using the
;;; FILE-DESCRIPTION argument.  You can pass a file stream (as opened by
;;; WITH-WORDNET-FILE-STREAM), a pathname or a part of speech keyword as the
;;; value for this argument.

;;;   (INDEX-ENTRY-FOR-WORD file-description word)
;;; Looks up word in the appropriate index file and returns the string
;;; corresponding to that record of the index file.

;;;	(READ-DATA-FILE-ENTRY file-description offset)
;;; Reads a WordNet "symset" record from the specified offset in the specified
;;; file.  A string is returned.  Offset was either read from an index record,
;;; or from a pointer description in another synset record.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:wordnet)

(defparameter +wordnet-database-directory+
  (asdf:system-relative-pathname '#:wordnet "dict/"))
(defparameter +wordnet-data-files+
  (directory (make-pathname :name "data"
                            :type :wild
                            :defaults +wordnet-database-directory+)))
(defparameter +wordnet-index-files+
  (directory (make-pathname :name "index"
                            :type :wild
                            :defaults +wordnet-database-directory+)))
(defparameter +wordnet-exception-files+
  (directory (make-pathname :name :wild
                            :type "exc"
                            :defaults +wordnet-database-directory+)))

(defun file-for-part-of-speech (part-of-speech file-type)
  (car (member (part-of-speech-file-type part-of-speech)
               (ecase file-type
                 (:index +wordnet-index-files+)
                 (:data +wordnet-data-files+))
               :test #'string-equal
               :key #'pathname-type)))

(defun exception-file-for-part-of-speech (part-of-speech)
  (car (member (part-of-speech-file-type part-of-speech)
               +wordnet-exception-files+
               :test #'string-equal
               :key #'pathname-name)))

(defmacro with-wordnet-file-stream ((stream-var file-identifier) &body body)
  `(with-open-file (,stream-var ,file-identifier :element-type 'character)
     ,@body))

(defun skip-wordnet-preamble (stream)
  (let ((position 0) line)
    (file-position stream 0)
    (loop
      (unless (string-equal "  " (setq line (read-line stream))
                            :end2 2)
        (return (values position line)))
      (setq position (file-position stream)))))


(defgeneric read-data-file-entry (from-where file-offset)
  (:documentation
   "Return a string that represents the synset at OFFSET in the specified
WordNet data file"))

(defmethod read-data-file-entry ((pos symbol) file-offset)
  (read-data-file-entry (file-for-part-of-speech pos :data) file-offset))

(defmethod read-data-file-entry ((pathname pathname) file-offset)
  (with-wordnet-file-stream (stream pathname)
    (read-data-file-entry stream file-offset)))

(defmethod read-data-file-entry ((data-file-stream t) file-offset)
  (file-position data-file-stream file-offset)
  (let ((raw-data (read-line data-file-stream)))
    (unless (= (parse-integer raw-data :end (position #\space raw-data))
               file-offset)
      (error "The offset ~d does not point to a synset entry in the file ~s"
             file-offset data-file-stream))
    raw-data))


(defgeneric index-entry-for-word (from-where word)
  (:documentation
   "Return a string that is the entry for WORD in the specified WordNet index file"))

(defmethod index-entry-for-word ((pos null) word)
  nil)

(defmethod index-entry-for-word ((pos symbol) word)
  (index-entry-for-word (file-for-part-of-speech pos :index) word))

(defmethod index-entry-for-word ((pathname pathname) word)
  (with-wordnet-file-stream (stream pathname)
    (index-entry-for-word stream word)))

(defmethod index-entry-for-word ((index-file-stream t) word)
  (let* ((bottom 0)
         (top (file-length index-file-stream))
         (word (string-downcase word))
         (word-length (length word))
         try where raw-entry)
    (setq bottom (skip-wordnet-preamble index-file-stream))
    (setq try (floor (+ bottom top) 2))
    (loop
      (multiple-value-setq (raw-entry where)
        (find-index-entry-raw index-file-stream try))
      ;;(format t "~&~8d ~8d ~8d ~8d ~s" bottom top try where raw-entry)
      (cond ((null raw-entry)
             (return nil))
            ((and (string-equal raw-entry word :end1 word-length)
                  (char-equal #\space (aref raw-entry word-length)))
             ;; no suffix, we found it
             (return raw-entry))
            ((and (>= where top)
                  (= where bottom))
             (return nil))
            ((>= where top)
             ;; The line that TRY points into is the last line of the current
             ;; search space.  Start searching from BOTTOM.
             (setq try bottom))
            ((string< raw-entry word)
             ;; beginning of next line
             (setq bottom (+ where (length raw-entry) 1)
                   try (floor (+ bottom top) 2)))
            ((string>= raw-entry word)
             ;; end of previous line
             (setq top (1- where)
                   try (floor (+ bottom top) 2)))))))

(defun find-index-entry-raw (index-file-stream file-offset)
  (let ((file-offset file-offset))
    (file-position index-file-stream (max 0 (1- file-offset)))
    (unless (char-equal #\newline (read-char index-file-stream))
      ;; if not at a line boundary, skip to end of line
      (read-line index-file-stream nil nil)
      (setq file-offset (file-position index-file-stream)))
    (values (read-line index-file-stream nil nil) file-offset)))

(defun list-index-file (file from to)
  (with-wordnet-file-stream (stream file)
    (let (line where)
      (multiple-value-setq (where line)
        (skip-wordnet-preamble file))
      (loop
        (unless (string< line from :end1 (min (length from) (length line)))
          (return))
        (setq line (read-line stream)))
      (setq where (- (file-position stream) (length line) 1))
      (loop
        (when (string> line to :end1 (length to))
          (return))
        (format t "~&~8d~15t~s" where line)
        (setq where (file-position stream))
        (setq line (read-line stream))))))

#|
;;; Test cases for INDEX-ENTRY-FOR-WORD.

(index-entry-for-word :noun "cat")
(index-entry-for-word :noun "bed")
(index-entry-for-word :noun "0")				;it's in there
(index-entry-for-word :noun "'s_gravenhage")			;first noun
(index-entry-for-word :noun "foobar")				;not there, should return nil
(index-entry-for-word :noun "zymurgy")				;last noun
|#

(defmethod exception-entry-for-word ((pos symbol) word)
  (with-wordnet-file-stream (stream (exception-file-for-part-of-speech pos))
    (index-entry-for-word stream word)))
