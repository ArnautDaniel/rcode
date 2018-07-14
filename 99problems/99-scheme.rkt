;;;All functions will be prepended with pnn-*

;;; P01 Find the last element of a list.

(define (pnn-last x)
  (cond
   ((not (pair? (cdr x))) (car x))
   (else
    (pnn-last (cdr x)))))

;;;Other ways to do this
;;;Find length of list and use list-ref to get the last element
;;;Compose 'First . Reverse'

;;; P02 Find the last but one element of a list.

(define (pnn-but-one x)
  (define (pnn-last? y)
    (not (pair? (cdr y))))
  (cond
   ((pnn-last? (cdr x)) x)
   (else
    (pnn-but-one (cdr x)))))

;;;Same cost as P01 with TCO
;;;I could use list-ref to make
;;;this insanely easy but I'm trying to
;;;keep it primitive.

;;; P03 Find the K'th element of a list.

(define (pnn-elem-at lst position)
  (cond
   ((empty? lst) (error "Requested indice exceeds length of list"))
   ((= position 0) (car lst))
   (else
    (pnn-elem-at (cdr lst) (- position 1)))))

;;; P04 Find the number of elements of a list.

(define (pnn-length lst)
  (cond
   ((empty? lst) 0)
   (else
    (+ 1 (pnn-length (cdr lst))))))

;;; Smarter with folds
(define (pnn-length0 lst)
  (foldl (lambda (x y) (+ 1 y)) 0 lst))

;;; P05 Reverse a list.

(define (pnn-reverse lst)
  
(define (pnn-reverse0 lst)
  (foldl (lambda (x y) (cons x y)) '() lst))
