(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))
(defparameter *italize-threshold* 100)
(defun remove-blank-lines (data)
  (remove-if #'(lambda (x) (equal x ""))
	     data))

(defun individual-process (line)
  (if (equal (subseq line 0 1) "(")
      (format nil "\\par ~a" line)
      line))

(defun insert-quote-blocks (line)
  (if (equal "" line)
      "")
  (if (find #\" line)
      (let* ((first-pos (position #\" line))
	     (second-pos (+ first-pos (position #\" (subseq line (+ 1 first-pos))))))
        (if (> (- second-pos first-pos) *italize-threshold*)
	    (format nil "~a \\begin{displayquote}``~a\\end{displayquote} ~a"
		    (subseq line 0  first-pos) (subseq line (+ first-pos 1) (+ second-pos 2))
		    (insert-quote-blocks (subseq line (+ second-pos 2))))
	    (format nil "~a \\textit {``~a} ~a"
		    (subseq line 0 first-pos) (subseq line (+ first-pos 1) (+ second-pos 2))
		    (insert-quote-blocks (subseq line (+ second-pos 2))))))
       
      line))

(defun chapters (line)
  (if (>= (length line) 7)
      (if (search "HOMILY" (subseq line 0 7))
	  (format nil "\\chapter{~a}" line)
	  (if (equal (subseq line 0 6) "______")
	      ""
	      line))
      line))

(defun sections (line)
  (if (<= (length line) 5)
      (if (> (parse-roman (subseq line 0 (length line))) 0)
	  (format nil "\\section {~a}" line)
	  line)
      line))

(defun compile-to-latex (file)
(mapcar #'sections
	  (mapcar #'chapters
		  (mapcar #'insert-quote-blocks
			  (mapcar #'individual-process (remove-blank-lines (get-file file)))))))

(defun mapcn (chars nums string)
  (loop as char across string as i = (position char chars) collect (and i (nth i nums))))
 
(defun parse-roman (R)
  (loop with nums = (mapcn "IVXLCDM" '(1 5 10 50 100 500 1000) R)
        as (A B) on nums if A sum (if (and B (< A B)) (- A) A)))

(defun output-latex (data name)
  (with-open-file (str name
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
    (dolist (e data)
      (format str "~A~%" e))))
