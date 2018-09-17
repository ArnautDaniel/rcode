#| Copyright (C) 2018 Jack Lucas

While trying to learn factor
I thought it'd be instrumental to convert their
compositional version of the Roman library into
the familiar territory of scheme.

Factor is pretty cool.  Infact, I dare say it's
becoming my favorite langue very quickly.

There's something impressive about the marriage
of the functional and imperative paradigms.

|#

#lang racket

(define roman-digits '("m" "cm" "d" "cd" "c" "xc" "l" "xl" "x" "ix" "v" "iv" "i"))

(define roman-values '(1000 900 500 400 100 90 50 40 10 9 5 4 1))

;Simple helper for making repeated character strings
(define (<string-repeat> elt len)
  (define (<acc> len acc)
    (if (= len 0) acc
	(<acc> (- len 1) (string-append acc elt))))
  (<acc> len ""))

#|  Based entirely off of Factor's Roman library

: >roman ( n -- str )
    roman-range-check
    roman-values roman-digits [
        [ /mod swap ] dip <repetition> concat
    ] 2map "" concat-as nip ;

Namely a translation of this function into Scheme. 

Basic operation is mapping over a list of values.

- If the number is divisible by the current value in the list
  then use the quotient as the new n and repeat the current digit
  by the #quotient.
  
  Use the remainder to continue onwards.

- If the number isn't divisible continue to the next digit/value pair.

That explains the basics of the Scheme version although the Factor
version would be explained a bit differently.
|#

(define (>roman n)
  (define (roman-fold)
    (for/fold ([acc ""]
	       [y n])
	([digits roman-digits]
	 [val roman-values])
      (values
       (string-append acc
		      (<string-repeat>
		       digits (floor (/ y val))))
       (modulo y val))))
  (define-values (res empt) (roman-fold))
  res)
