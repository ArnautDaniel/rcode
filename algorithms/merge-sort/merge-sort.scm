;;; Copyright (C) Jack Lucas
;;; Merge Sort

(use-modules (srfi srfi-1))

(define (merge-sort lst)
  (if (= 1 (length lst))
      lst
      (let* ((len (floor (/ (length lst) 2)))
             (right (merge-sort (take lst len)))
             (left (merge-sort (drop lst len)))
             (result (merge left right)))
        result)))

;;;For taking advantage of that sweet sweet TCO
;;;this function sends itself to an function
;;;with an accumulator
(define (merge left right)
  (define (merge-acc left0 right0 acc)
    (cond
     ((null? left0) (append acc right0))
     ((null? right0) (append acc left0))
     ((<= (first left0) (first right0))
      (merge-acc (cdr left0) right0 (append acc (list (car left0)))))
     (else
      (merge-acc left0 (cdr right0) (append acc (list (car right0)))))))
  (merge-acc left right '()))

;;;Just used for generating big random lists of numbers
(define (my-random n)
  (if (= n 0)
      '()
      (cons (random n) (my-random (- n 1)))))

;;;Appears to work just fine.  .15s to sort 3000 numbers.  No idea
;;;if that's good for guile.  I'll know more once I do the C version.

;;;Might be a good idea to investigate parallel processing in guile

