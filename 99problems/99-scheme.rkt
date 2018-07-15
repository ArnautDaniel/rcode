;;;All functions will be prepended with pnn-*
#lang racket
;;; [P01] -  Find the last element of a list.

(define (pnn-last x)
  (cond
   ((not (pair? (cdr x))) (car x))
   (else
    (pnn-last (cdr x)))))

;;;Other ways to do this
;;;Find length of list and use list-ref to get the last element
;;;Compose 'First . Reverse'

;;; [P02] -  Find the last but one element of a list.

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

;;; [P03] -  Find the K'th element of a list.

(define (pnn-elem-at lst position)
  (cond
   ((empty? lst) (error "Requested indice exceeds length of list"))
   ((= position 0) (car lst))
   (else
    (pnn-elem-at (cdr lst) (- position 1)))))

;;; [P04] - Find the number of elements of a list.

(define (pnn-length lst)
  (cond
   ((empty? lst) 0)
   (else
    (+ 1 (pnn-length (cdr lst))))))

;;; Smarter with folds
(define (pnn-length0 lst)
  (foldl (lambda (x y) (+ 1 y)) 0 lst))

;;; [P05] - Reverse a list.

(define (pnn-reverse0 lst)
  (foldl (lambda (x y) (cons x y)) '() lst))

;;; [P06] - Find out wether a list is a palindrome

(define (pnn-palindrome lst)
  (let ((lst0 (pnn-reverse0 lst)))
    ;shadow lst and lst0
    (let inner-loop ((lst lst)
		     (lst0 lst0))
      (cond
       ((and (empty? lst) (empty? lst0))
	#t)
       ((or (empty? lst) (empty? lst0))
	#f)
       ((equal? (car lst) (car lst0))
	(inner-loop (cdr lst) (cdr lst0)))
       (else
	#f)))))

;;; [P07]- Flatten a nested list structure

;;; [x0 x1 (x2 (x3 x4)) x5 xn]
;;; -> [x0 x1 ... xn]

(define (pnn-flatten lst)
  
  (define (atom? x)
    (and (not (pair? x)) (not (null? x))))
  
  (cond
   ((empty? lst) '())
   
   ((atom? (car lst))
    (cons (car lst) (pnn-flatten (cdr lst))))
   
   ((list? (car lst))
    (append (pnn-flatten (car lst))
	    (pnn-flatten (cdr lst))))
   (else
    (error "Malformed List"))))

;;; Tested with '(a (b (c d)) e) -> (a b c d e)
;;; Would be trivial to add an accumulator to speed it up.

;;; [P08] - Eliminate consecutive duplicates of list elements

;;; Does not specify nested lists and the example uses a linear one
;;; This version will only support linear lists of the [x0 x1 ... xn] type

(define (pnn-remove-duplicates lst)

  (define (contained-in? memory element)
    (member element memory))
  
  (let inner-loop
      ((memory '())
       (lst lst))
    
    (cond
     ((empty? lst) '())
     
     ((contained-in? memory (car lst))
      (inner-loop memory (cdr lst)))
     
     (else
      (cons (car lst)
	    (inner-loop (cons (car lst) memory) (cdr lst)))))))


;;; [P09] - Pack consecutive duplicates of list elements into sublists

(define (pnn-pack-duplicates lst)
  (define (take-dup-until element lst)
    (let inner-loop
	((accumulate '())
	 (lst lst))
    (cond
     ((empty? lst) accumulate)
     
     ((equal? element (car lst))
      (inner-loop (cons (car lst) accumulate)
		  (cdr lst)))
     (else
      accumulate))))

  (define (pack-duplicates cur-elem lst accumulate)
    (cond
     
     ((empty? lst) accumulate)
     
     ((equal? cur-elem (car lst))
      (let*
	  ((result (take-dup-until cur-elem lst))
	   (lst0 (drop lst (length result))))
	(if (empty? lst0)
	    (reverse (append (list result) accumulate))
	    (pack-duplicates (car lst0) lst0 (append (list result) accumulate)))))

     (else
      (pack-duplicates (car lst) lst accumulate))))

  (if (empty? lst)
      '()
      (pack-duplicates (car lst) lst '())))
  

;;; Not a big fan of the way I did this.  FIXME

;;; [P10] - Run-length encoding of a list
;;; [P11] - Modified run-length encoding

(define (pnn-length-encoding lst map-lambda)
  (let ((packed-lst (pnn-pack-duplicates lst)))
    (map map-lambda packed-lst)))

(define (pnn-duplicate-encoding lst)
  (pnn-length-encoding lst (lambda (x) (cons (length x) (list (car x))))))

(define (pnn-mod-duplicate-encoding lst)
  (pnn-length-encoding lst
		       (lambda (x)
			 (if (= (length x) 1)
			     (car x)
			     (cons (length x) (list (car x)))))))

;;; [P12] - Decode a run-length encoded list

(define (pnn-decode-list lst)
  (pnn-flatten
   (map (lambda (elem)
	  (if (not (list? elem))
	      elem
	      (let loop ((len (car elem))
			 (elem0 (cadr elem)))
		(cond
		 ((= len 0) '())
		 (else
		  (cons elem0 (loop (- len 1) elem0)))))))
	lst)))

;;; [P13] - Run length encoding of a list (direct solution)
;;; Skipping this one for now

;;; [P14] - Duplicate elements of a list

(define (pnn-duplicate-elements lst)
  (pnn-flatten
   (map (lambda (x) (list x x))
	       lst)))

;;; [P15] - Duplicate elements of a list an N number of times

(define (pnn-n-duplicate-elements lst n)
  (pnn-flatten
   (map (lambda (x) (let loop ((n n))
		      (cond
		       ((= n 0) '())
		       (else
			(cons x (loop (- n 1)))))))
	lst)))

;;; Not sure if he means (x, 3) becomes (x x, 2) -> (x x x x, 1) -> etc
;;; or (x, 3) -> (x x x).  I did both but I'm sticking with the latter.

;;; [P16] - Drop every N'th element from a list
