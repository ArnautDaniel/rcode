; Copyright (C) 2018 Jack Lucas
; Uncommented code
; Converts data about a show directory into
; a "Catalogue" inspired by the ideas of Guix.
; Unused at the moment, but works

(use-modules (ice-9 rdelim))
(use-modules (ice-9 ftw))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))

(define (convert-xlsx-to-catalogue file-dir)
  (convert-xlsx-to-csv file-dir)
  (convert-csv-to-catalogue file-dir))

;;;first pass
(define (convert-xlsx-to-csv file-dir)
  (let ((cwd (getcwd)))
    (nftw file-dir
          (lambda (filename statinfo flag base level)
            (if (equal? flag 'regular)
                (let ((pathname (cadr (string-split (basename filename) #\.))))
                  (if (string=? pathname "xlsx")
                        (let ((rfile
                               (string-append cwd (string-drop filename 1))))
                          (system
                           (string-append "libreoffice --headless --convert-to csv \""
                                          rfile "\" --outdir \""
                                          (string-take rfile (- (+ base (string-length cwd)) 1))
                                          "\""))
                          #t)
                      #t))
                #t)))))

;;;second pass
(define (convert-csv-to-catalogue file-dir)
  (let ((cwd (getcwd)))
    (nftw file-dir
          (lambda (filename statinfo flag base level)
            (if (and (equal? flag 'regular)
                     (string=? (cadr (string-split (basename filename) #\.))
                               "csv"))

                  (let ((rfile (string-append cwd (string-drop filename 1))))
                    (make-catalogue rfile base cwd))
                #t)))))

(define (parse-in-csv file-handle)
  (let looper ((acc '())
               (data (read-line file-handle)))
    (cond
     ((eof-object? data) (reverse acc))
     (else
      (looper (cons (list data) acc) (read-line file-handle))))))

(define (make-catalogue rfile base cwd)
  (let* ((r (parse-in-csv (open-input-file rfile)))
         (show (show-name (string-split (car (list-ref r 7)) #\,)))
         (set (show-name (string-split (car (list-ref r 8)) #\,)))
         (items (map (lambda (x) (string-split (car x) #\,)) (filter empty-desc? (drop r 14)))))
    (write-catalogue rfile base show set items)
    #t)
  #t)

(define (picture-list dir)
  (let ((sc (scandir dir)))
    (filter (lambda (x) (string=? "JPG" (if (= 2 (length (string-split (string-drop x 1) #\.)))
                                            (cadr (string-split (string-drop x 1) #\.))
                                            "")))
            sc)))

(define (write-catalogue rfile base show set items)
  (let ((output-path
         (string-append (string-take rfile (- (+ base (string-length (getcwd))) 1)) (clean-white show) ".riley")))
    (with-output-to-file output-path
      (lambda ()
        (format #t "~a~%~a~%--~%" show set)
        (map (lambda (x y) (format #t "~a #::| ~a ~a~%~a #:: ~a ~a~%"
                                   (clean-description (cadr x)) (caddr x) (cadddr x) y (string-hash y)
                                   (string-hash (clean-description (cadr x)))))
             items (picture-list (string-take rfile (- (+ base (string-length (getcwd))) 1))))))))

(define (clean-description desc)
  (list->string (filter (lambda (x) (or (char-alphabetic? x) (char-whitespace? x)))
                        (string->list desc))))

(define (show-name line)
  (clean-description (cadr line)))

(define (empty-desc? desc)
  (not (or   (string=? "" (cadr (string-split (car desc) #\,)))
             (string=? "" (caddr (string-split (car desc) #\,))))))

(define (clean-white show)
  (list->string (map (lambda (x) (if (char-whitespace? x)
                                     #\-
                                     x))
                (string->list show))))
