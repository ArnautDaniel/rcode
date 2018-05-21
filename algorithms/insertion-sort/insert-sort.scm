
(define (insertion-sort! lst)
  (let ((len (length lst))
        (vec (list->vector lst)))
    (let o-loop ((n 1))
      (cond
       ((= n len) (vector->list vec))
       (else
        (let i-loop ((key (vector-ref vec n))
                     (i (- n 1)))
            (cond
             ((and (>= i 0) (> (vector-ref vec i) key))
              (vector-set! vec (+ i 1) (vector-ref vec i))
              (i-loop key (- i 1)))
             (else
              (vector-set! vec (+ i 1) key)
              (o-loop (+ n 1))))))))))

;;;This (below func)  would only be necessary if the
;;;function above didn't wrap itself in
;;;clojure with a the let and convert the lst
;;;to a vector for speed

;;;If we got rid of the outer binding above we'd
;;;use this to wrap a clojure here without affecting
;;;our original variable

(define (insertion-sort lst)
  (let ((lat lst))
    (insertion-sort! lat)))

;;;Man it was much harder to write this in Scheme than the
;;;C version.  I suppose that's do the algorithm outline being
;;;much more "C-like".  The hardest part was wanting to jump
;;;into a recursive style like some other solutions I saw AFTER
;;;I completed this one.  But I'm not so sure their "haskelly"
;;;version is better.  For one the other versions don't have
;;;proper tail-calls, and secondly I think it's easier to reason
;;;about named lets with an "explicit stack" than spliting it into
;;;two functions and relying on TCO.  Just so happens you're going
;;;to have to explicitly name a stack anyway if you go down that road,
;;;so why not put a sign up?
