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

;;; [P10] - Run-length encoding of a list &
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
(define (pnn-drop-every lst n)
  (let looper ((lst lst)
	       (n0 (- n 1))
	       (acc '()))
    (cond
     ((empty? lst) (reverse acc))
     ((= n0 0) (looper (cdr lst) (- n 1) acc))
     (else
      (looper (cdr lst) (- n0 1) (cons (car lst) acc))))))

;;;This is a really good one to think about better ways to do it ^

;;; [P17] - Split a list into two parts; the length of the first part is given

(define (pnn-split lst n)
  (let ((first-half (take lst n))
	(second-half (drop lst n)))
    (values first-half second-half)))

;;; [P18] - Extract a slice from a list

(define (pnn-sublist lst n k)
  (define-values (lst0 lst1) (pnn-split lst n))
  (define-values (lst2 lst3) (pnn-split lst1 (+ k 1)))
  lst2)
;;; This one is pretty funky.  FIXME

;;; [P19] - Rotate a list N place to the left

(define (pnn-rotate lst n)
  
  (define (rotate lst0 n0)
    (define-values (lst0 lst1) (pnn-split lst n))
    (append lst1 lst0))

  ;; Should probably have a let so (length lst) isn't computed
  ;; twice
  (if (< (length lst) n)
      (pnn-rotate lst (- n (length lst)))
      (rotate lst n)))

;;;  Handle n > length lst by successively subtracting
;;;  length of lst from n.  You could throw an error too.

;;; [P20] - Remove the K'th element from a list

(define (pnn-remove-at lst n)
  (append (take lst n)
	  (drop lst (+ n 1))))

;;; [P21] - Insert an element at a given position into a list

(define (pnn-insert lst elem n)
  (append (take lst n)
	  (cons elem
		(drop lst n))))

;;; [P22] - Create a list containing all integers within a given range

(define (pnn-range n k)
  (cond
   ((= n k) '())
   (else
    (cons n (pnn-range (+ n 1) k)))))

;;; [P23] - Extract a given number of randomly selected elements from a list

(define (pnn-random-select lst n)
  (let loop ((len (length lst))
	     (n n)
	     (acc '())
	     (lst lst))
    (cond
     ((= n 0) acc)
     (else
      (let ((rand (random len)))
	(loop (- len 1) (- n 1)
	      (cons (pnn-elem-at lst rand) acc)
	      (pnn-remove-at lst rand)))))))

;;;Explicit stack is interesting.

;;; [P24] - Lotto: Draw N different random numbers from the set 1..M
(define (pnn-lotto n k)
  (let ((lotto-numbers (pnn-range 1 k)))
    (pnn-random-select lotto-numbers n)))

;;; [P25] - Generate a random permutation of the elements of a list

(define (pnn-random-permutation lst)
  (pnn-random-select lst (length lst)))

;;; [P26] - Generate the combinations of K distinct objects chosen from
;;; the N elements of a list

(define (pnn-combination lst k)
  (cond
   ((empty? lst) '())
   ((= k 1) (map list lst))
   (else
    (append (map (lambda (x) (cons (car lst) x))
		 (pnn-combination (cdr lst) (- k 1)))
	    (pnn-combination (cdr lst) k)))))


;;; [P27] - Group the elements of a set into disjoint subsets
;;; [P28] - Sorting a list of lists according to length of sublists

;;; [P31] - Determine whether a given integer number is prime

;;; Implementation translated from "The Haskell Road to Logic,
;;; Maths and Programming", mostly since I find it a pretty
;;; way to do it.

(define (pnn-prime? n)

  (define (divides? d n)
    (= (remainder n d) 0))

  (define (ld n)
    (ldf 2 n))

  (define (ldf k n)
    (cond
     ((divides? k n) k)
     ((> (* k k) n) n)
     (else
      (ldf (+ k 1) n))))

  (cond
   ((< n 1) (error "Not a positive integer"))
   ((= n 1) #f)
   (else
    (= (ld n) n))))

;;; Other ways are of course to use something like Fermat's or
;;; if you're feeling extra crazy using CPS with the Sieve of Eratosthenes
;;; However, I'm not saying any of these are the best.

;;; [P32] - Determine the greatest common divisor of two positive integer
;;; numbers

(define (pnn-gcd n k)
  (if (> k n)
      (gcd k n)
      (cond
       ((= k 0) n)
       (else
	(gcd k (modulo n k))))))

;;; [P33] - Determine wether two positive integer numbers are coprime

(define (pnn-coprime? n k)
  (if (= 1 (gcd n k))
      #t
      #f))

;;; [P34] - Calculate Euler's totient function phi(m)

(define (pnn-totient-phi n)
  (if (> n 0)
      (let loop ((k 2)
		 (acc 1))
	(cond
	 ((= k n) acc)
	 ((pnn-coprime? n k)
	  (loop (+ k 1) (+ acc 1)))
	 (else
	  (loop (+ k 1) acc))))
      (error "Not defined for negative numbers")))

;;; Start at 2 since phi(1) = 1.  Probably much faster than
;;; building a list with a range function and mapping over it

;;; [P35] - Determine the prime factors of a given positive integer
(define (pnn-prime-factors n)

  (define (prime-factors n k)
    (cond
     ((= n 1) '())
     ((= (modulo n 2) 0)
      (cons 2 (prime-factors (/ n 2) 3)))
     ((= (modulo n k) 0)
      (cons k (prime-factors (/ n k) 3)))
     (else
      (prime-factors n (+ k 1)))))

  (prime-factors n 3))

;;; [P36] - Determine the prime factors of a give positive integer (2)
;;; Construct a list containing the prime factors and their multiplicity

(define (pnn-prime-encoding lst)
  (pnn-length-encoding lst (lambda (x) (cons (car x) (list (length x))))))

(define (pnn-prime-decoding lst)
  (pnn-decode-list (map (lambda (k) (pnn-reverse0 k))
		   lst)))

(define (pnn-prime-factors-multi n)
  (pnn-prime-encoding (pnn-prime-factors n)))

;;; [P37] - Calculate Euler's totient function phi(m) (improved)

(define (pnn-totient-phi-improved n)
  
  (define (phi-calculate p-pair)
    (if (number? p-pair)
	p-pair
	(let ((p (car p-pair))
	      (m (cadr p-pair)))
	  (* (- p 1) (expt p (- m 1))))))
  
  (foldl (lambda (c k)
	   (* (phi-calculate c) (phi-calculate k)))
	 1 (pnn-prime-factors-multi n)))

;;; [P38] - Compare the two methods of calculating Euler's totient function

;;; (time (pnn-totient-phi 100900))
;;; cpu time: 11

;;; (time (pnn-totient-phi-improved 100900))
;;; cpu time: 0

;;; Direct math computation is obviously much faster than iteration

;;; [P39] - A list of prime numbers

(define (pnn-prime-list start end)
  (cond
   ((= start end) '())
   ((pnn-prime? start)
    (cons start (pnn-prime-list (+ start 1) end)))
   (else
    (pnn-prime-list (+ start 1) end))))

;;; Improvements,  make it check odd numbers only
;;; No TCO

;;; [P40] - Goldbach's Conjecture

(define (pnn-goldbach n)
  
  (let loop ((primes (pnn-prime-list 1 n))
	     (acc '()))
    (cond
     ((empty? primes) acc)
     ((pnn-prime? (- n (car primes)))
      (if (= (- n (car primes)) (car primes))
	  (loop (cdr primes) acc)
	  (let ((prime0 (remove (- n (car primes)) primes)))
	    (loop (cdr prime0)
		  (cons (list (car primes) (- n (car primes))) acc)))))
      (else
       (loop (cdr primes) acc)))))
;;; This version returns all possible goldbachs instead of just one as in
;;; the example

;;; [P41] - A list of Goldbach compositions
(define (goldbach-list n k step)
  (let ((elements (range n k step)))
    (map pnn-goldbach elements)))

;;; [P46] - Truth tables for logical expressions

