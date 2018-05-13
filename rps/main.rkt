#lang racket
(require graph)

;;;Directed graph of R -> P -> S ->

(define valid-moves '("r" "s" "p"))
(define log '())
(define game-data (directed-graph '((p r) (r s) (s p))))

(define (flush-log)
  (call-with-output-file "game.log" #:exists 'truncate
                         (lambda (out)
                           (map (lambda (c)
                                  (write c out)
                                  (newline out))
                                log))))

(define (winning-side p1 p2)
  (let ((p1-move (get-neighbors game-data p1))
        (p2-move (get-neighbors game-data p2)))
    (cond
     ((equal? p1-move p2-move)
      'draw)
     ((member p2 p1-move) 'lwin)
     ((member p1 p2-move) 'rwin))))

(define (robot-highest-use stats)
(let ((debug
    (sort (map (lambda (c)
                 (cons c
                       (list
                        (count (lambda (e)
                                 (equal? (string->symbol c) e)) stats))))
               valid-moves) #:key cadr >)))
  (display debug)
  (newline)
  (string->symbol (caar debug))))

(define (robot-choice)
  (let ((r (random 3)))
    (cond
     ((empty? log) (string->symbol (list-ref valid-moves r)))
     (else
      (let ((statistics (map (lambda (c) (first c)) log)))
        (robot-inverse-move (robot-highest-use statistics)))))))

(define (robot-inverse-move move)
  (let ((inverse (transpose game-data)))
    (first (get-neighbors inverse move))))

(define (determine-winner choice)
  (let* ((robot (robot-choice))
         (winner (winning-side (string->symbol choice) robot)))
    (write-to-log (string->symbol choice) robot winner)
    (cond
     ((eq? winner 'lwin) (display "You win!"))
     ((eq? winner 'rwin) (display "You lost!"))
     (else
      (display "Draw!")))))

(define (play-match)
  (display "\nChoose move: [R][P][S] -- ")
  (let ((choice (string-downcase (read-line))))
    (cond
     ((valid-move? choice) (begin (determine-winner choice)
                                  (play-match)))
     ((equal? choice "q") (begin (flush-log)
                                 (exit)))
     (else
      (display "Invalid move\n")
      (play-match)))))

(define (valid-move? choice)
  (if (member choice valid-moves)
      #t
      #f))

(define (write-to-log l ri r)
  (set! log (cons (list l ri r) log)))

(play-match)

