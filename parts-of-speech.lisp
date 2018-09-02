;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

(in-package #:wordnet)

;;; Representing parts of speech.  In WordNet, the part of speech determines
;;; which index and data files to look in.

(defparameter +part-of-speech-table+ nil)

(defmacro define-part-of-speech (canonical-name (file-type &rest wn-db-tokens)
                                 &rest abbreviations)
  `(progn
     (setf (get ,canonical-name :wordnet-file-type) ,file-type)
     (push (list nil (string-downcase (symbol-name ,canonical-name)) ,canonical-name)
           +part-of-speech-table+)
     (dolist (n ',abbreviations)
       (push (list nil n ,canonical-name)
             +part-of-speech-table+))
     (dolist (n ',wn-db-tokens)
       (push (list n nil ,canonical-name)
             +part-of-speech-table+))
     ,canonical-name))

(progn
  (define-part-of-speech :noun ("noun" "n") "n")
  (define-part-of-speech :verb ("verb" "v") "v")
  (define-part-of-speech :adjective ("adj" "a" "s") "adj")
  (define-part-of-speech :adverb ("adv" "r") "adv"))

(defmacro do-parts-of-speech ((part-of-speech-var pos-name-var) &body body)
  (let ((pos-entry-var '#:pos-entry)
        (wn-db-token-var '#:wn-db-token))
    `(dolist (,pos-entry-var +part-of-speech-table+)
       (destructuring-bind (,wn-db-token-var ,pos-name-var ,part-of-speech-var)
           ,pos-entry-var
         ,wn-db-token-var ;ignore
         (when ,pos-name-var
           ,@body)))))

(defun parts-of-speech ()
  (let ((parts-of-speech nil))
    (do-parts-of-speech (pos ignore)
      ignore
      (pushnew pos parts-of-speech))
    parts-of-speech))

(defun part-of-speech-for-wordnet-db-token (token)
  (third (car (member token +part-of-speech-table+ :key #'first :test #'string=))))

(defun canonicalize-part-of-speech (part-of-speech)
  (etypecase part-of-speech
    (string (third (member part-of-speech +part-of-speech-table+
                           :key #'second
                           :test #'string-equal)))
    (symbol (third (car (member part-of-speech +part-of-speech-table+
                                :key #'third))))))

(defun part-of-speech-file-type (part-of-speech)
  (get (canonicalize-part-of-speech part-of-speech) :wordnet-file-type))
