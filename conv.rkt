(define data-prelude 14)

(define (show-data-from-xlsx filename)
  (let ((showhack 0))
    (with-input-from-xlsx-file filename
                                    (lambda (xlsx)
                                      (load-sheet "Sheet1" xlsx)
                                      (set! showhack (get-sheet-rows xlsx))))
    (cons
     (find-project-and-set showhack)
     (map (lambda (c) (take c 6))
         (filter (lambda (c) (number? (car c)))
                 (filter (lambda (c) (not (string=? "" (cadr c))))
                         showhack))))))

(define (find-project-and-set sh)
  (let ((data (flatten (take sh data-prelude))))
    (list (cadr (member "PROJECT:" data))
          (cadr (member "SET:" data))
          (cadr (member "Ordered by:" data)))))
