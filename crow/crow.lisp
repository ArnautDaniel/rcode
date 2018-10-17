;;; Crow (toy program)
;;; Simple CLI program for keeping track of 
;;; reading files through large codebases
;;; (SBCL, OpenBSD etc)

;;; Not anywhere near done but getting it going
;;; on version control

(defun crow-walker (path ident)
  (let
      ((res
	(remove-if #'null
		   (mapcar
		    #'(lambda (files)
			(unless (null (pathname-type files))
			  (and (member (pathname-type files)
				       ident :test #'equal)
			       files)))
		    (uiop:directory-files path)))))
    (if (null res)
	nil
         res)))

(defun crow-directory (path ident)
  (remove-if #'null
	     (mapcar #'(lambda (dir)
			 (unless (null (uiop:subdirectories dir))
			   (cons (crow-walker dir ident)
				 (crow-directory dir ident))))
		     (uiop:subdirectories path))))

(defun crow (path ident)
  (alexandria:flatten
   (crow-directory path ident)))

