(in-package #:wordnet)

(defun get-state-names ()
  (let ((states nil)
        (pointers (wn:wordnet-pointers
                   (car (wn:index-entry-synsets
                         (wn:cached-index-lookup "american_state" :noun))))))
    (dolist (p pointers)
      (when (eq (wn:wordnet-pointer-type p) :hyponym)
        (push (mapcar #'car (wn:synset-words (wn:wordnet-pointer-to-synset p)))
              states)))
    states))

(defun synsets-containing-words (part-of-speech words)
  (reduce #'intersection
          (mapcar #'(lambda (word)
                      (wn:index-entry-synsets
                       (wn:cached-index-lookup word part-of-speech)))
                  words)))

(defun get-synonyms (words part-of-speech)
  (reduce #'union
          (mapcar #'wn:synset-words
                  (synsets-containing-words part-of-speech
                                            (if (listp words) words (list words))))
          :initial-value nil))

(defun get-antonyms (word part-of-speech)
  (let ((antonyms nil)
        (synsets (wn:index-entry-synsets
                  (wn:cached-index-lookup word part-of-speech))))
    (dolist (s synsets)
      (dolist (p (wn:wordnet-pointers s))
        (when (eq (wn:wordnet-pointer-type p) :antonym)
          (let ((from (wn:wordnet-pointer-from-word p))
                (to (wn:wordnet-pointer-to-word p)))
            (when (or (typep from 'wn:wordnet-synset-entry)
                      (and (stringp (car from))
                           (string-equal (car from) word)))
              (if (typep to 'wn:wordnet-synset-entry)
                  (dolist (w (wn:synset-words to))
                    (pushnew (car w) antonyms :test #'string-equal))
                  (pushnew (car to) antonyms :test #'string-equal)))))))
    antonyms))

(defun find-synset-with-sense (part-of-speech word like-word)
  (let ((word-synsets (wn:index-entry-synsets
                       (wn:cached-index-lookup word part-of-speech)))
        (like-word-synsets (wn:index-entry-synsets
                            (wn:cached-index-lookup like-word part-of-speech)))
        (found nil))
    (dolist (like-synset like-word-synsets)
      (dolist (synset word-synsets)
        (let ((c (wn::commonality :hypernym synset like-synset)))
          (when c
            (push c found)))))
    (sort (pairlis (mapcar #'(lambda (f)
                               (apply #'+ (mapcar #'cdr (cdr f))))
                           found)
                   found)
          #'< :key #'car)))
