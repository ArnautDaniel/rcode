#lang racket
(require racket/set)

;;;Making it better
;;;Include more punctuation
;;;Include uppercase letters

;;;Key Cipher Caeser

;;;Insert a set (S1) with unique letters only 'test' -> 'tes'
;;;at the beginning of the the set of letters A-Z (S2)
;;;Which corresponds to the standard letters S3

;;; S1 + S2 = cipher
;;; 'tesabcd...' with 'tes' and 'abc...'
(define alphabet "abcdefghijklmnopqrstuvwxyz")

;;;Generate a set of unique letters from a string
(define (string->set str)
  (list->string (remove-duplicates (string->list str))))

;;;Add S1 to S2
(define (generate-cipher word)
  (let ((set1 (filter (lambda (x) (not (char=? x #\space)))
                      (string->list (string->set word))))
        (set2 (string->list alphabet)))
    (list->string (remove-duplicates (append set1 set2)))))

;;;Takes strings as arguments to generate cipher
;;;adds space character to allow for spaces in strings
(define (simple-zip lst lat)
  (map (lambda (x y) (list x y)) 
       (cons #\space (string->list lst))
       (cons #\space (string->list lat))))

;;;Search cipher (such as [[#\a #\j]] to find the corresponding
;;;shifted letter
(define (find-cipher-alphabet cipher letter)
  (cadr (findf (lambda (arg)
                 (char=? (car arg) letter))
               cipher)))

(define test-cipher (simple-zip 
                     alphabet 
                     (generate-cipher "julius caesar")))

;;;Abstracted boilerplate
(define (generic-cypher cypher inp)
  (list->string
   (map (lambda (x) (find-cipher-alphabet cypher x))
        (string->list inp))))

;;;Simply reverse the cipher to decrypt
(define (decrypt-with-cypher cypher inp)
  (generic-cypher (map reverse cypher) inp))

(define (encrypt-with-cypher cypher inp)
  (generic-cypher cypher inp))

