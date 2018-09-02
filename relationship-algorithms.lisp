;;; CommonLisp interface to WordNet
;;; 1995, Mark Nahabedian
;;; Artificial Intelligence Laboratory
;;; Massachusetts Institute of Technology

;;; some hacks for doing graph reasoning on WordNet relationship pointers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; relationship operations

(in-package #:wordnet)

(defun relation-transitive-closure (synset relation-type)
  (assert (wordnet-relation-p relation-type))
  (assert (transitive-relation-p relation-type))
  ;; collect the synset and its distance from the one we started at.
  (let ((to-do (list (list synset 0)))
        (closure-set nil)
        (relation-types (list relation-type)))
    (loop
      (when (null to-do)
        (return))
      (destructuring-bind (synset distance) (pop to-do)
        (push (list synset distance) closure-set)
        (do-synset-pointers (pointer synset relation-types)
          (let ((new-synset (wordnet-pointer-to-synset pointer)))
            (unless (member new-synset to-do :key #'first)
              (unless (member new-synset closure-set :key #'first)
                (push (list new-synset (1+ distance)) to-do)))))))
    closure-set))

;;; If there's only one synset, it's the common one.
;;; if one of the sysnsets is a superior of any of the others, its the common one.

(defun commonality (relation-type &rest synsets)
  (assert (wordnet-relation-p relation-type))
  (assert (transitive-relation-p relation-type))
  (assert (eq :up (relation-direction relation-type)))
  ;; Assume that the closure sets are already ordered by distance with the
  ;; root of the relationship graph (most distant node for an upward
  ;; relationship) first.
  ;; I suppose if the relationship weren't an :UP one, we could compute the
  ;; interesection of the closure sets and select the elements of the closure
  ;; sets which are also members of the intersection, order those and pick the
  ;; closest one.  Maybe later.
  (let ((closures (mapcar #'(lambda (s) (relation-transitive-closure s relation-type))
                          synsets))
        last-common last-distances)
    (loop
      ;; have we exhausted any of the closure sets?
      (when (some #'null closures)
        (return))
      ;; if there's a difference at the current layer, we're done.
      (unless (reduce #'(lambda (a b) (if (eq a b) a nil))
                      closures :key #'caar)
        (return))
      ;; note the common element at the current layer
      (setq last-common (first (first (first closures))))
      ;; and each of the distances for this layer
      (setq last-distances
            (mapcar #'(lambda (x) (second (car x))) closures))
      ;; go to next layer
      (setq closures (mapcar #'cdr closures)))
    (when last-common
      (list* last-common
             (mapcar #'(lambda (synset distance)    ;PAIRLIS doesn't preserve order
                         (cons synset distance))
                     synsets last-distances)))))
