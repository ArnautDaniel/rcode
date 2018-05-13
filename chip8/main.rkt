#lang racket

;;;TODO
;;;Why the hell are you using strings?

;;;needed for hex-strings
(require file/sha1)

;;;Load rom a byte at a time
;;;converts to a hex-string
;;;which allows (string->number) to work
(define (load-rom rom-file)
  (let ((file (open-input-file rom-file)))
    (let loop ((r (bytes->hex-string (read-bytes 1 file))))
      (cond
        ((eof-object? (peek-bytes 1 0 file))
         (list r))
        (else
         (cons r (loop (bytes->hex-string (read-bytes 1 file)))))))))

;;;Poor implementation
(define (hexstr-to-int hstr)
  (string->number (string-append "#x" hstr)))

;;;Return a new block of memory
(define (return-memory-table size)
  (let ((h (make-hash))
        (r (range size)))
    (for-each (lambda (c)
                (hash-set! h c 0))
              r)
    h))

;;;RAM
(define ram (return-memory-table 4095))

;;;Load a block of data to memory starting at inc
(define (load-to-memory rom inc)
  (let ((si (map (lambda (c) (+ c inc))
                 (range (length rom)))))
    (for-each (lambda (c k)
                (hash-set! ram c k))
              si rom)))

(load-to-memory (load-rom "/home/silver/Downloads/invaders.c8") 512)

;;;Read a portion of ram
;;;Automatically filter empty values
(define (read-program-ram start end)
  (sort (filter (lambda (c) (not (equal? c #f)))
                (hash-map ram (lambda (k v)
                                (if (and (member k (range start end)) (string? v))
                                    `(ADDR ,k INSTR ,v)
                                    #f)))) #:key cadr <))

;;;Utility -- I'm drunk
(define (insatadd start)
  (string-append (hash-ref ram start)
                 (hash-ref ram (+ start 1))))

;;;Registers

;;;Program Counter
(define pcoun 512)

;;;Opcode
(define opcod 0)
(define index 0)

(define stack '())
(define sp 0)

(define vf 0)

;;; VX0 - VXF
(define register-map (return-memory-table 16))

;;;Accessor
(define (vx n)
  (hash-ref register-map n))

;;;Virtual Register SET!
(define (vxs reg val)
  (hash-set! register-map reg val))

;;;Timers
(define dt 0)
(define st 0)
                          
;;;Generate Chip Font and load in #x000 - #x1FF
(define chip-font (map (lambda (c)
                         (string-append "#x" c))
                       '("f0" "90" "90" "90" "f0"      ;0
                              "20" "60" "20" "20" "70" ;1
                              "f0" "10" "f0" "80" "f0" ;2
                              "90" "90" "f0" "10" "10" ;3
                              "f0" "80" "f0" "10" "f0" ;4
                              "f0" "80" "f0" "90" "f0" ;5
                              "f0" "80" "f0" "90" "f0" ;6
                              "f0" "10" "20" "40" "40" ;7
                              "f0" "90" "f0" "90" "f0" ;8
                              "f0" "90" "f0" "10" "f0" ;9
                              "f0" "90" "f0" "90" "90" ;A
                              "e0" "90" "e0" "90" "e0" ;B
                              "f0" "80" "80" "80" "f0" ;C
                              "e0" "90" "90" "90" "e0" ;D
                              "f0" "80" "f0" "80" "f0" ;E
                              "f0" "80" "f0" "80" "80" ;F
                              )))
(load-to-memory chip-font 0)


;;;OPCODE #xFX65
(define (fill-registers n)
  (let looper ((x 0))
    (cond
      ((= x n) #t)
      (else
       (vxs x (hash-ref ram index))
       (set! index (+ index 1))
       (looper (+ x 1))))))

;;;Possible screen
(define screen (map (lambda (c)
                      (list c (map (lambda (k)
                                     (list k 0))
                                   (range 64))))
                    (range 32)))

;;;Graphics

(define (draw-row x-val y-val)
  (display "write screen code\n"))

(define (draw-sprites opcode)
  (let looper 
    ((x-val (vx (bitwise-bit-field opcode 8 12)))
     (y-val (vx (bitwise-bit-field opcode 4 8)))
     (data (bitwise-bit-field opcode 0 4)))
    (set! vf 0)
    (cond
      ((= data 0) #t)
      (else
       (draw-row x-val y-val)
       (looper x-val (+ y-val 1) (- data 1))))))


;;;For #xCY get Y
(define (opcode-postlude opcode)
  (bitwise-bit-field (hexstr-to-int opcode) 0 4))
;;;For #xCY get C

(define (opcode-prelude opcode)
  (arithmetic-shift (hexstr-to-int opcode) -4))

;;;Rudimentary Stack
(define (push lat lst)
  (cons lat lst))

(define (pop lst)
  (cdr lst))

;;;Next instruction
(define (tick-pcoun)
  (set! pcoun (+ pcoun 2)))


(define (clear-screen pc)
  (display "Clearing screen\n")
  (set! pcoun (+ pcoun 2))
  (chip-run))


(define (chip-run)

  ;;;Delay timer, workout proper timing
  (cond
    ((= dt 0) #t)
    (else
     (set! dt (- dt 1))))
  
  (cond
    ((= st 0) #t)
    (else
     (set! st (- st 1))))

  (let* ((current-opcode (hash-ref ram pcoun))
         (opcode-pair (hash-ref ram (+ pcoun 1)))
         (full-opcode (hexstr-to-int (string-append current-opcode opcode-pair))))
    
    (display "At opcode: ")
    (display pcoun)
    (newline)
    
    (match full-opcode

      [#x00e0 (clear-screen pcoun)]
      [#x00ee (begin (set! pcoun (car stack))
                     (set! stack (cdr stack))
                     (tick-pcoun)
                     (chip-run))]
      [_

       (match (opcode-prelude current-opcode)
         
         [1 (set! pcoun (bitwise-bit-field full-opcode 0 12))
            (chip-run)]

         [2 (set! sp (+ sp 1))
            (set! stack (push pcoun stack))
            (set! pcoun (bitwise-bit-field full-opcode 0 12))
            (chip-run)]

         [3 (let ((vxv (vx (opcode-postlude current-opcode))))
              (if (= vxv (hexstr-to-int opcode-pair))
                  (begin
                    (tick-pcoun)
                    (tick-pcoun)
                    (chip-run))
                  (begin
                    (tick-pcoun)
                    (chip-run))))]

          [4 (let ((vxv (vx (opcode-postlude current-opcode))))
              (if (= vxv (hexstr-to-int opcode-pair))
                  (begin
                    (tick-pcoun)
                    (chip-run))
                  (begin
                    (tick-pcoun)
                    (tick-pcoun)
                    (chip-run))))]
         
         [5 (let ((x-val (opcode-postlude current-opcode))
                  (y-val (opcode-prelude opcode-pair)))
              (cond
                ((= x-val y-val)
                 (begin
                   (tick-pcoun)
                   (tick-pcoun)
                   (chip-run)))
                (else
                 (tick-pcoun)
                 (chip-run))))]
         
         [6 (vxs (opcode-postlude current-opcode)
                 (hexstr-to-int opcode-pair))
            (tick-pcoun)
            (chip-run)]

         [7 (vxs (opcode-postlude current-opcode)
                 (+ (hexstr-to-int opcode-pair) 
                    (vx (bitwise-bit-field 
                         (hexstr-to-int current-opcode)
                         0 4))))
            (tick-pcoun)
            (chip-run)]
         
         [8 (match (opcode-postlude opcode-pair)

              [#x0 (vxs (opcode-postlude current-opcode)
                        (vx (opcode-prelude opcode-pair)))
                   (tick-pcoun)
                   (chip-run)]
              
              [#x1 (let ((x-val (vx (opcode-postlude current-opcode)))
                         (y-val (vx (opcode-prelude opcode-pair))))
                     (vxs (opcode-postlude current-opcode)
                          (bitwise-ior x-val y-val)))]
              
              [#x2 (vxs (opcode-postlude current-opcode)
                        (bitwise-and (vx (opcode-postlude current-opcode))
                                     (vx (opcode-prelude opcode-pair))))
                   (tick-pcoun)
                   (chip-run)]

              [#x3 (let ((x-val (vx (opcode-postlude current-opcode)))
                         (y-val (vx (opcode-prelude opcode-pair))))
                     (vxs (opcode-postlude current-opcode)
                          (bitwise-xor x-val y-val)))]

              [#x4 (vxs (opcode-postlude current-opcode)
                        (+ (vx (opcode-postlude current-opcode))
                           (vx (opcode-prelude opcode-pair))))
                   (tick-pcoun)
                   (chip-run)]
              
              [#x5 (let ((x-val (vx (opcode-postlude current-opcode)))
                         (y-val (vx (opcode-prelude opcode-pair))))
                     (cond
                       ((> x-val y-val) (set! vf 1))
                       (else
                        (set! vf 0)))
                     (vxs x-val (- x-val y-val))
                     (tick-pcoun)
                     (chip-run))]

              [_ (display "Need new 8 opcode")])]
         
         [9 (let ((x-val (opcode-postlude current-opcode))
                   (y-val (opcode-prelude opcode-pair)))
               (cond
                 ((not (= x-val y-val))
                  (begin
                    (tick-pcoun)
                    (tick-pcoun)
                    (chip-run)))
                 (else
                  (tick-pcoun)
                  (chip-run))))]
         
         [#xf (match (hexstr-to-int opcode-pair)

                [#x07 (vxs (opcode-postlude current-opcode) dt)
                      (tick-pcoun)
                      (chip-run)]
                
                [#x15 (set! dt (vx (opcode-postlude current-opcode)))
                      (tick-pcoun)
                      (chip-run)]

                [#x18 (set! st (vx (opcode-postlude current-opcode)))
                      (tick-pcoun)
                      (chip-run)]
                         
                [#x29 (set! index (vx (opcode-postlude current-opcode)))
                      (tick-pcoun)
                      (chip-run)]
                
                [#x33 (let ((vxc (vx (opcode-postlude current-opcode))))
                        (hash-set! ram index (floor (/ vxc 100)))
                        (hash-set! ram (+ index 1) (modulo (floor (/ vxc 10)) 10))
                        (hash-set! ram (+ index 2) (modulo (floor (modulo vxc 100)) 10))
                        (tick-pcoun)
                        (chip-run))]
                
                [#x65 (fill-registers (opcode-postlude current-opcode))
                      (tick-pcoun)
                      (chip-run)]

                [#x1e (set! index (+ index (vx (opcode-postlude current-opcode))))
                      (tick-pcoun)
                      (chip-run)]
                
                [_ (display "Need new F clause")])]

         [#xc (vxs (opcode-postlude current-opcode)
                   (bitwise-and (random 255) (hexstr-to-int opcode-pair)))
              (tick-pcoun)
              (chip-run)]

         ;;;Implement INPUT
         [#xe (match (hexstr-to-int opcode-pair)

                [#x9e (tick-pcoun)
                      (tick-pcoun)
                      (chip-run)]
                
                [#xa1 (tick-pcoun)
                      (tick-pcoun)
                      (chip-run)])]

         ;;;Implement DRAWING
         [#xd (draw-sprites full-opcode)
              (tick-pcoun)
              (chip-run)]
         
         [#xa (set! index (bitwise-bit-field full-opcode 0 12))
              (tick-pcoun)
              (chip-run)]

         [#xb (let ((x-val (vx 0)))
                (set! pcoun (+ (bitwise-bit-field full-opcode 0 12) x-val))
                (chip-run))]
         
         [_ (display "no opcode matches")])])))
