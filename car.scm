(use-modules (ice-9 format))
(define (l-feet time length)
  (let ((cur-pos (* time time)))
    (- (sqrt (+ length cur-pos)) time)))

(define (current-position time)
  (* time time))

(define (amount-traveled time-o time-f)
  (- (current-position time-o) (current-position time-f)))

(define car-a (lambda (x) (current-position x)))

(define car-b (lambda (x) (+ (/ (current-position x) 2) x)))

(define (start-cars car-a car-b start stop inc)
  (let loop ((start start)
             (stop stop)
             (acc '()))
    (cond
     ((>= start stop) '())
     (else
      (format #t "[T: ~1,3f] CAR-A AT: ~1,2f | CAR-B at ~1,2f \n"
              start (car-a start) (exact->inexact (car-b start)))
      (loop (+ start inc) stop
            (append (list (car-a start) (car-b start)) acc))))))

