;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

(in-package #:wordnet-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a view for displaying stuff in a pretty format, stuff printed in this
;;; format isn't necessarily parsible.

(defclass pretty-synset-view (textual-view) ())
(defparameter +pretty-synset-view+ (make-instance 'pretty-synset-view))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; presentation type for parts of speech

(define-presentation-type part-of-speech-token ()
  :description "A part of speech")

(define-presentation-method present ((object symbol) (type part-of-speech-token)
                                                     stream (view textual-view) &key)
  (let ((names nil))
    (do-parts-of-speech (pos name)
      (when (eq pos object)
        (push name names)))
    (unless names
      (error "~s is not a part of speech" object))
    (write-string (first names) stream)))

(define-presentation-method accept ((type part-of-speech-token) stream
                                                                (view textual-view) &key)
  (multiple-value-bind (object success string)
      (completing-from-suggestions (stream)
                                   (dolist (pos (parts-of-speech))
                                     (suggest (symbol-name pos) pos)))
    (if success
        (values object type)
        (input-not-of-required-type string type))))

(define-presentation-method presentation-typep (object (type part-of-speech-token))
  (and (symbolp object)
       (canonicalize-part-of-speech object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; presentation type for index entries

(define-presentation-type wordnet-index-entry ()
  :description "A WordNet index entry")

(define-presentation-method present ((object wordnet-index-entry)
                                     (type wordnet-index-entry) stream
                                     (view textual-view) &key)
  (write-char #\[ stream)
  (present (part-of-speech object) 'part-of-speech-token
           :stream stream)
  (write-char #\space stream)
  (write-string (index-entry-word object) stream)
  (write-char #\] stream))

(define-presentation-method accept ((type wordnet-index-entry) stream
                                                               (view textual-view) &key)
  (let (char part-of-speech word)
    (unless (char-equal #\[ (setq char (read-char stream)))
      (input-not-of-required-type char type))
    (peek-char t stream)
    (setq part-of-speech (accept 'part-of-speech-token
                                 :stream stream
                                 :view view
                                 :delimiter-gestures '(#\space)))
    (peek-char t stream)
    (setq word (accept 'string
                       :stream stream
                       :view view
                       :prompt nil
                       :delimiter-gestures '(#\space #\])))
    (peek-char t stream)
    (unless (char-equal #\] (setq char (read-char stream)))
      (input-not-of-required-type char type))
    (let ((ie (cached-index-lookup word part-of-speech)))
      (unless ie
        (input-not-of-required-type (format nil "[~a ~a]" part-of-speech word)
                                    type))
      (values ie type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; presentation type for synsets

(define-presentation-type wordnet-synset-entry ()
  :options (word-index gloss word-sense-tags)
  :description "A WordNet synset")

(define-presentation-method present ((object wordnet-synset-entry)
                                     (type wordnet-synset-entry) stream
                                     (view pretty-synset-view) &key)
  (with-output-as-presentation (stream object type)
    (wn::pretty-print-synset stream object
                             :word-index (or word-index 0)
                             :gloss gloss
                             :word-sense-tags word-sense-tags)))

(define-presentation-method present ((object wordnet-synset-entry)
                                     (type wordnet-synset-entry) stream
                                     (view textual-view) &key)
  (with-output-as-presentation (stream object type)
    (write-char #\[ stream)
    (present (part-of-speech object) 'part-of-speech-token
             :stream stream)
    (write-char #\space stream)
    (present (wn::synset-offset object) '((integer) :base 10)
             :stream stream)
    (write-char #\] stream)))

(define-presentation-method accept ((type wordnet-synset-entry) stream
                                                                (view textual-view) &key)
  (let (char part-of-speech index)
    (unless (char-equal #\[ (setq char (read-char stream)))
      (input-not-of-required-type char type))
    (peek-char t stream)
    (setq part-of-speech (accept 'part-of-speech-token
                                 :delimiter-gestures '(#\space)
                                 :stream stream :view view))
    (peek-char t stream)
    (setq index (accept '((integer) :base 10)
                        :delimiter-gestures '(#\space #\tab #\])
                        :stream stream
                        :view view
                        :provide-default nil
                        :prompt nil))
    (peek-char t stream)
    (unless (char-equal #\] (setq char (read-char stream)))
      (input-not-of-required-type char type))
    (let ((synset (cached-data-lookup index part-of-speech)))
      (unless synset
        (input-not-of-required-type (format nil "[~a ~d]" part-of-speech index)
                                    type))
      (values synset type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; presentation type for wordnet pointers

(define-presentation-type wordnet-pointer ()
  :options ((show-from-synset t) (show-to-synset t))
  :description "A WordNet pointer")

(define-presentation-method present ((object wordnet-pointer)
                                     (type wordnet-pointer) stream
                                     (view pretty-synset-view) &key)
  (with-output-as-presentation (stream object type)
    (when show-from-synset
      (present (wordnet-pointer-from-synset object)
               (list '(wordnet-synset-entry)
                     :word-index (wordnet-pointer-from-synset-index object))
               :stream stream
               :view view
               :word-sense-tags t)))
  (format stream " >== ~a ==> " (wordnet-pointer-type object))
  (when show-to-synset
    (present (wordnet-pointer-to-synset object)
             (list '(wordnet-synset-entry)
                   :word-index (wordnet-pointer-to-synset-index object))
             :stream stream
             :view view
             :word-sense-tags t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; commands

(define-command-table wordnet :inherit-from (clim-demo::lisp-listener))

(define-command (com-lookup :name "Lookup" :menu t
                            :command-table wordnet)
    ((word 'string)
     &key
     (parts-of-speech '(sequence part-of-speech-token)
                      :default (parts-of-speech)))
  (let ((index-entries nil))
    (dolist (pos parts-of-speech)
      (let ((ie (cached-index-lookup word pos)))
        (when ie
          (push ie index-entries))))
    (dolist (ie index-entries)
      (fresh-line)
      (write-string "    ")
      (present ie 'wordnet-index-entry :view +pretty-synset-view+)))
  (values))

(define-command (com-get-synsets :name "Get SYNSETs" :menu t
                                 :command-table wordnet)
    ((ie 'wordnet-index-entry))
  (dolist (ss (index-entry-synsets ie))
    (fresh-line)
    (write-string "    ")
    (present ss 'wordnet-synset-entry
             :view +pretty-synset-view+
             :word-sense-tags t
             :gloss t)))

(define-presentation-to-command-translator index-entry-get-synsets
    (wordnet-index-entry com-get-synsets wordnet
                         :gesture :select
                         :menu t
                         :documentation "Get synsets")
  (object)
  (list object))

(define-command (com-show-synset-pointers :name "Show Pointers" :menu t
                                          :command-table wordnet)
    ((synset 'wordnet-synset-entry))
  (dolist (pointer (wordnet-pointers synset))
    (fresh-line)
    (write-string "    ")
    (present pointer '((wordnet-pointer) :show-from-synset nil)
             :allow-sensitive-inferiors t
             :view +pretty-synset-view+)))

(define-presentation-to-command-translator synset-show-pointers
    (wordnet-synset-entry com-show-synset-pointers wordnet
                          :gesture :select
                          :menu t
                          :documentation "Show pointers")
  (object)
  (list object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Application

(define-application-frame wordnet-browser (clim-demo::lisp-listener)
  ((process :initform nil :accessor frame-process))
  (:command-table wordnet)
  (:command-definer nil)
  (:top-level (wordnet-browser-top-level))
  (:panes (clim-demo::interactor :interactor :scroll-bars :both)))

(defun wordnet-browser-top-level (frame)
  ;; why dosn't this do the right thing?
                                        ;  (when (frame-process frame)
                                        ;    (assert (eq (frame-process frame)
                                        ;		process::*current-process*)))
  (clim-demo::lisp-listener-top-level frame))

(defun wordnet-browser (&key (port (find-port)) frame-manager (force nil))
  (let* ((frame-manager (or frame-manager
                            (find-frame-manager :port port)))
         (frames nil) frame)
    (map-over-frames #'(lambda (frame)
                         (when (typep frame 'wordnet-browser)
                           (push frame frames)))
                     :port port
                     :frame-manager frame-manager)
    (setq frame (car frames))
    (when (or force (null frame))
      (setq frame (make-application-frame 'wordnet-browser
                                          :frame-manager frame-manager
                                          :width 600 :height 500)))
    (cond ((null (frame-process frame))
           (setf (frame-process frame)
                 (process:process-run-function "WordNet Browser"
                                               'run-frame-top-level
                                               frame)))
          ;;maybe check state of existing process and either poke it, or select
          ;;the underlying window.
          )
    (values frame frames)))

#+Genera
(define-genera-application wordnet-browser :select-key #\!)
