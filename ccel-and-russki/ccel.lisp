(defparameter root "/home/silver/ccel/")

(defun get-author-divs (link)
  (let ((req (drakma:http-request link)))
    (remove-if-not
     #'(lambda (x)
	 (equal "author_bookList" (plump:attribute x "class")))
     (plump:get-elements-by-tag-name (plump:parse req) "div"))))

(defun get-book-link (plp)
  (remove-if #'(lambda (y)
		 (or (search "htm" y) (search "html" y)))
	     (mapcar #'(lambda (x)
			 (list (plump:attribute x "href") x))
		     (plump:get-elements-by-tag-name plp "a"))))

(defparameter alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defun get-book-description (plp)
  (plump:text (car (remove-if-not (lambda (y)
				    (equal "bookinfo_description"
					   (plump:attribute y "class")))
				  (plump:get-elements-by-tag-name plp "td")))))

(defun get-author-name (link)
  (let ((req (plump:parse (drakma:http-request (concatenate 'string "https://www.ccel.org" link)))))
    (if (or (null req) (equal link "/ccel/heidelberg-cat-ext.txt"))
	'("trash" "trash" "trash")
        (list
	 (if (null (car (plump:get-elements-by-tag-name (plump:get-element-by-id req "titleWrapper") "h3")))
	     "Misc"
	     (subseq (plump:text (car (plump:get-elements-by-tag-name (plump:get-element-by-id req "titleWrapper") "h3"))) 3))
	 (plump:text  (car (plump:get-elements-by-tag-name (plump:get-element-by-id req "titleWrapper") "h1")))
	 (get-book-description req)))))
  
(defun make-fishy-path (author end)
  (concatenate 'string root (car author) "/" (cadr author) end))

(defun output-book-description (author)
  (with-open-file (file (remove #\? (make-fishy-path author ".info"))
			:direction :output :if-exists :supersede)
    (format file "~a" (caddr author))))

(defun fishy-download (link author)
  (ignore-errors
   (unless (probe-file (make-fishy-path author ".pdf"))
     (trivial-download:download (concatenate 'string "https://www.ccel.org" link ".pdf")
			      (remove #\? (make-fishy-path author ".pdf")))))
  (ignore-errors
   (unless (probe-file (make-fishy-path author ".txt"))
     (trivial-download:download (concatenate 'string "https://www.ccel.org" link ".txt")
			     (remove #\? (make-fishy-path author ".txt")))))
  (ignore-errors
   (unless (probe-file (make-fishy-path author ".info"))
     (output-book-description author))))

(defun fishy (link)
  (let ((author (get-author-name link)))
    (format t "~%Now working on: ~a | ~a~%" (car author) (cadr author))
    (if (directory (concatenate 'string root (car author)))
	(fishy-download link author)
	(progn
	  (ensure-directories-exist (concatenate 'string root (car author)))
	  (fishy-download link author)))
    (format t "~%Completed!~%")))

(defun generate-fishy-links (alpha)
  (loop for c across alpha collect (format nil "https://www.ccel.org/index/author/~a" c)))

(defun go-fish ()
  (mapcar #'(lambda (y)
		     (mapcar #'(lambda (x)
				 (if (equal x "/creeds/heidelberg-cat-ext.txt")
				     nil
				     (ignore-errors (fishy x))))
			     (cdr (reverse (get-book-link y)))))
		 (get-author-divs "https://www.ccel.org/index/author")))

(defun process-fish-string (x qp)
  (let ((temp (cdr (cl-strings:split (car x) #\/))))
    (cond
      ((equal 2 (length temp))
       nil)
      (t
       (let ((name (reverse (cdr (reverse
				  (cl-strings:split (plump:text (plump:get-element-by-id qp (concatenate 'string "author_link_" (cadr temp))))
						    #\space))))))
	 (remove #\, (cl-strings:join (append (cdr name) (list (car name))) :separator " ")))))))
		    
(defun fish-probe (x qp)
  (let ((res (process-fish-string x qp))
	(title (plump:text (cadr x))))
    (format t "~% Checking res:~a | title ~a" res title)
    (cond
      ((null res) nil)
      ((search "." (car x)) nil)
      ((or (remove #\? (probe-file (make-fishy-path (list res title) ".pdf")))
		(remove #\? (probe-file (make-fishy-path (list res title) ".txt")))
	    (remove #\? (probe-file (make-fishy-path (list res title) ".info"))))
       (format t "~%Found ~a~%" title)
       nil)
      (t t))))
      
(defun go-fish-fast ()
  (let ((qp (plump:parse (drakma:http-request "https://www.ccel.org/index/author"))))
    (mapcar #'(lambda (y)
		     (mapcar #'(lambda (x)
				 (if (equal x "/creeds/heidelberg-cat-ext.txt")
				     nil
				     (ignore-errors (let ((res (fish-probe x qp)))
				       (if res
					   (fishy (car x))
					   nil)))))
			     (cdr (reverse (get-book-link y)))))
		 (get-author-divs "https://www.ccel.org/index/author"))))
