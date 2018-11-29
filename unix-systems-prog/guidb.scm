;;translating the database from APUE to guile scheme
;;probably a silly task and extremely unidomatic in this first iteration
;;but im finding it helpful

(use-modules (srfi srfi-9))
(use-modules (srfi srfi-9 gnu))
(use-modules (ice-9 format))

(define *db_insert* 1)
(define *db_replace* 2)
(define *db_store* 3)

(define *idxlen_min* 6)
(define *idxlen_max* 1024)
(define *datalen_min* 2)
(define *datalen_max* 1024)

(define *idxlen_sz* 4)
(define *sep* ":")
(define *space* " ")
(define *newline* "\n")

(define *ptr_sz* 6)
(define *ptr_max* 999999)
(define *nhash_def* 137)
(define *free_off* 0)
(define *hash_off* *ptr_sz*)

(define-record-type <db>
  (make-db)
  database?
  (idxfd db-idxfd set-db-idxfd!)
  (datfd db-datfd set-db-datfd!)
  (idxbuf db-idxbuf)
  (datbuf db-datbuf)
  (name db-name set-db-name!)
  (idxoff db-idxoff)
  (idxlen db-idxlen)
  (datoff db-datoff)
  (datlen db-datlen)
  (ptrval db-ptrval)
  (ptroff db-ptroff set-db-ptroff!)
  (chainoff db-chainoff set-db-chainoff!)
  (hashoff db-hashoff set-db-hashoff!)
  (nhash db-nhash set-db-nhash!)
  (cnt-delok db-cnt-delok)
  (cnt-delerr db-cnt-delerr)
  (cnt-fetchok db-cnt-fetchok set-db-fetchok!)
  (cnt-fetcherr db-cnt-fetcherr set-db-fetcherr!)
  (cnt-nextrec db-cnt-nextrec)
  (cnt-stor1 db-cnt-stor1)
  (cnt-stor2 db-cnt-stor2)
  (cnt-stor3 db-cnt-stor3)
  (cnt-stor4 db-cnt-stor4)
  (cnt-storerr db-cnt-storerr))

(define (name-new-db pathname db name)
  (set-db-name! db (string-append pathname "/" name ".idx")))

;;;very unidomatic
(define (db-open pathname)
  (let ((db (make-db)))
    (name-new-db pathname db "test")
    (set-db-nhash! db *nhash_def*)
    (set-db-hashoff! db *hash_off*)
    (set-db-idxfd! db
		   (open-file (db-name db) "rw"))
    (set-db-datfd! db
		   (open-file (string-append
			       (substring (db-name db) 0
					  (- (length (db-name db)) 4)) ".dat") "rw"))
    (flock (db-idxfd db) LOCK_EX) ;; lock index file
    (flock (db-datfd db) LOCK_EX) ;; lock data file
    (let ((stat-idx (stat (db-idxfd db)))
	  (stat-dat (stat (db-datfd db))))
      (if (= (stat:size stat-idx) 0)
	  (initialize-index db)))
    db))

(define (initialize-index db)
  (let ((idx (db-idxfd db)))
    (let loop ((i 0))
      (if (= i *nhash_def*)
	  (flock idx LOCK_UN)
	  (begin
	    (format idx "~6d" i)
	    (loop (+ i 1)))))))
      
(define (db-close db)
  (close-port (db-idxfd db))
  (close-port (db-datfd db)))

(define (db-fetch db key)
  (if (< (db-find-and-lock db key 0) 0)
      (set-db-fetcherr! db
			(+ (db-cnt-fetcherr db) 1))
      (set-db-fetchok!  db
			(+ (db-cnt-fetchok db) 1)))
  ;;; course as hecc
  (let ((res (db-readdat db)))
    (flock (db-idxfd db) LOCK_UN)
    res))
  
(define (db-find-and-lock db key writelock)
  (set-db-chainoff! db
		    (+ (* (db-hash db key) *ptr_sz*)
		       (db-hashoff db)))
  (set-db-ptroff! db (db-chainoff db))
  (flock (db-idxfd db) LOCK_EX)
  (let ((offset (db-readptr db (db-ptroff db))))
    (while (not (= offset 0))
      (let ((nextoffset (db-readidx db offset)))
	(if (equal? (db-idxbuf db) key)
	    (set! offset offset))
	(set-db-ptroff! db offset)
	(set! offset nextoffset)))
    (if (= offset 0)
	-1
	0)))
	    
(define (db-hash db key)
  (let ((keylst (string->list key)))
    (modulo
     (let looper ((hval 0) (i 1) (c keylst))
      (cond
       ((null? c) hval)
       (else
	(looper (+ hval (* (char->integer c) i)) (+ i 1) (cdr keylst)))))
     (db-nhash db))))

