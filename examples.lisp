(in-package #:wordnet)

#|
(defun get-state-names ()
  (let ((states nil)
        (pointers (wordnet-pointers
                   (car (index-entry-synsets (cached-index-lookup "american_state" :noun))))))
    (dolist (p pointers)
      (when (eq (wordnet-pointer-type p) :substance-hyponym)
        (push (mapcar #'car (synset-words (wordnet-pointer-to-synset p)))
              states)))
    states))
|#

(defun synsets-containing-word/s (word/s &optional (part-of-speech :any))
  "Returns a list of synsets containing the (or all of the) words in word/s."
  (let ((words (if (listp word/s) word/s (list word/s))))
    (reduce #'intersection
	    (mapcar #'(lambda (word)
			(if (equal part-of-speech :any)
			    (apply #'append
				   (list (index-entry-synsets (cached-index-lookup word :noun))
					 (index-entry-synsets (cached-index-lookup word :verb))
					 (index-entry-synsets (cached-index-lookup word :adjective))
					 (index-entry-synsets (cached-index-lookup word :adverb))))
			  (index-entry-synsets (cached-index-lookup word part-of-speech))))
		    words))))

(defun get-synonyms (words &optional (part-of-speech :any))
  (reduce #'union
          (mapcar #'synset-words
                  (synsets-containing-word/s (if (listp words) words (list words))
					    part-of-speech))
          :initial-value nil))

(defun get-antonyms (word part-of-speech)
  (let ((antonyms nil)
        (synsets (index-entry-synsets (cached-index-lookup word part-of-speech))))
    (dolist (s synsets)
      (dolist (p (wordnet-pointers s))
        (when (eq (wordnet-pointer-type p) :antonym)
          (let ((from (wordnet-pointer-from-word p))
                (to (wordnet-pointer-to-word p)))
            (when (or (typep from 'wordnet-synset-entry)
                      (and (stringp (car from))
                           (string-equal (car from) word)))
              (if (typep to 'wordnet-synset-entry)
                  (dolist (w (synset-words to))
                    (pushnew (car w) antonyms :test #'string-equal))
                  (pushnew (car to) antonyms :test #'string-equal)))))))
    antonyms))

(defun find-synset-with-sense (part-of-speech word like-word)
  (let ((word-synsets (index-entry-synsets (cached-index-lookup word part-of-speech)))
        (like-word-synsets (index-entry-synsets (cached-index-lookup like-word part-of-speech)))
        (found '()))
    (dolist (like-synset like-word-synsets)
      (dolist (synset word-synsets)
        (let ((c (commonality :hypernym synset like-synset)))
          (when c (push c found)))))
    (sort (pairlis (mapcar #'(lambda (f) (reduce #'+ (mapcar #'cdr (cdr f)))) found)
                   found)
          #'< :key #'car)))

(defun %%wordnet-describe (word-or-phrase part-of-speech)
  (let* ((word-or-phrase (substitute #\_ #\Space word-or-phrase))
         (synsets (synsets-containing-word/s (list word-or-phrase) part-of-speech)))
    (when synsets
      (let* ((glossaries (mapcar (lambda (x) (slot-value x 'gloss)) synsets))
             (synonyms (remove-duplicates
                        (mapcar #'car (get-synonyms word-or-phrase part-of-speech))
                        :test #'string=))
             (fixed-synonyms (mapcar (lambda (x) (substitute #\Space #\_ x)) synonyms))
             (fixed-synonyms2 (remove word-or-phrase fixed-synonyms :test #'string=))
             (antonyms (remove-duplicates (get-antonyms word-or-phrase part-of-speech)
                                          :test #'string=))
             (fixed-antonyms (mapcar (lambda (x) (substitute #\Space #\_ x)) antonyms)))
        (values (substitute #\Space #\_ word-or-phrase)
                part-of-speech glossaries fixed-synonyms2 fixed-antonyms)))))

(defun %wordnet-describe (word-or-phrase part-of-speech)
  (let* ((count 0))
    (with-output-to-string (*standard-output*)
      (multiple-value-bind (word-or-phrase part-of-speech glossaries synonyms antonyms)
          (%%wordnet-describe word-or-phrase part-of-speech)
        (when word-or-phrase
          (format t "~A (~A)~%" word-or-phrase (string-downcase (string part-of-speech)))
          (mapc (lambda (x) (format t "~4D. ~A~%" (incf count) x)) glossaries)
          (when synonyms (format t "  Synonyms: ~{~A~^, ~}~%" synonyms))
          (when antonyms (format t "  Antonyms: ~{~A~^, ~}~%" antonyms)))))))

(defun wordnet-describe (word-or-phrase &optional part-of-speech)
  (if part-of-speech
      (format t "~A" (%wordnet-describe word-or-phrase part-of-speech))
      (loop for part-of-speech in '(:noun :verb :adjective :adverb)
            for string = (%wordnet-describe word-or-phrase part-of-speech)
            when (string/= "" string)
              collect string into strings
            finally (format t "~{~A~^~%~}" strings)))
  (values))

(defun wordnet-describe* (word-or-phrase &optional part-of-speech)
  (if part-of-speech
      (let ((result (multiple-value-list (%%wordnet-describe word-or-phrase part-of-speech))))
        (when (car result) (list result)))
      (loop for part-of-speech in '(:noun :verb :adjective :adverb)
            for list = (multiple-value-list (%%wordnet-describe word-or-phrase part-of-speech))
            when (car list) collect list)))
