(defpackage :aoc-monad
  (:use :cl))

(in-package :aoc-monad)

(defun bind (parser function)
  (lambda (string)
    (let ((result1 (funcall parser string)))
      (apply #'nconc (map 'list
			  #'(lambda (pair)
			      (funcall (funcall function (car pair)) (cdr pair)))
			  result1)))))

(defun unit (x)
  (lambda (string)
    (list (cons x string))))

(defun fail ()
  (lambda (string)
    (declare (ignore string))
    '()))

(defmacro then (parser1 parser2)
  `(bind ,parser1
	#'(lambda (x) (declare (ignore x)) ,parser2)))

(defun either (parser1 parser2 &rest parsers)
  (lambda (string)
    (let ((parsers (cons parser1 (cons parser2 parsers))))
      (loop
	 for parser in parsers
	 for result = (funcall parser string)
	 while (null result)
	 finally (return result)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rewrite-forms (forms)
    (flet ((rewrite-form (cur rest)
	     (if (and (listp cur) (eq (car cur) 'assign))
		 (if rest
		     `(bind ,(third cur)
			#'(lambda (,(second cur))
			    ,(rewrite-forms rest)))
		     (third cur))
		 (if rest
		     `(then ,cur
			    ,(rewrite-forms rest))
		     cur))))
      (if (null forms)
	  '()
	  (rewrite-form (car forms) (cdr forms)))))
	  
  (defmacro with-monad (&body body)
    (rewrite-forms body)))

(defun parse-empty-p ()
  (lambda (string)
    (if (= 0 (length string))
	(list (cons t string))
	(list (cons nil string)))))

(defun parse-eof ()
  (lambda (string)
    (if (= 0 (length string))
	(list (cons t string))
	'())))

(defun parse-character (item)
  (lambda (string)
    (if (= 0 (length string))
	'()
	(let ((char (char string 0)))
	  (if (or (and (functionp item)
 		       (funcall item char))
		  (and (or (listp item) (stringp item))
		       (find char item))
		  (and (characterp item)
		       (char= char item)))
	      (list (cons char (subseq string 1)))
	      '())))))

(defun zero-or-more (parser)
  (lambda (string)
    (let ((ret (loop for res = (funcall parser string)
                      until (null res)
                      do (setf string (cdr (first res)))
                      collect (car (first res)))))
      (list (cons ret string)))))

(defun two-of (parser)
  (bind parser
	#'(lambda (first-result)
	    (bind parser
		  #'(lambda (second-result)
		      (unit (list first-result second-result)))))))

(defun n-of (n parser)
  (labels ((n-of-rec (acc n)
	     (if (= n 0)
		 (unit (nreverse acc))
		 (bind parser
		       #'(lambda (result)
			   (n-of-rec (cons result acc) (- n 1)))))))
    (n-of-rec '() n)))

(defun one-or-more (parser)
  (bind parser
	#'(lambda (first-result)
	    (bind (zero-or-more parser)
		  #'(lambda (rest-result)
		      (unit (cons first-result rest-result)))))))

(defun whitespace-char-p (x)
  (or (char= #\Space x)
      (not (graphic-char-p x))))
  
(defun parse-lower-case ()
  (parse-character #'lower-case-p))
    
(defun parse-newline ()
  (either (either (then (parse-character #\Return) (parse-character #\Linefeed))
		  (parse-character #\Linefeed))
	  (parse-eof)))

(defun parse-line (parser)
  "Returns the results of parser followed by a newline or eof"
  (with-monad
    (assign res parser)
    (parse-newline)
    (unit res)))

(defun parse-string (string)
  (labels ((parse-string-rec (acc string)
	     (if (= 0 (length string))
		 (unit (format nil "~{~A~}" (reverse acc)))
		 (bind (parse-character #'(lambda (x) x))
		       #'(lambda (char)
			   (if (char= char (char string 0))
			       (parse-string-rec (cons (char string 0) acc)
						 (subseq string 1))
			       (fail)))))))
    (parse-string-rec '() string)))

(defun parse-word ()
  (bind (one-or-more (parse-lower-case))
	#'(lambda (chars)
	    (unit (format nil "~{~A~}" chars)))))

(defun parse-characters (item)
  (with-monad
    (assign chars (one-or-more (parse-character item)))
    (unit (format nil "~{~a~}" chars))))

(defun parse-space ()
  (parse-character #\Space))

(defun run-parser (parser string)
  (caar (funcall parser string)))

(defun parse-digit ()
  (with-monad
    (assign digit (parse-character #'digit-char-p))
    (unit (digit-char-p digit))))

(defun parse-number ()
  (with-monad
    (assign sign (either (parse-character "-+")			 
			 (unit #\+)))
    (assign digits (one-or-more (parse-digit)))
    (unit (* (if (char= sign #\-) -1 1)
	     (reduce #'(lambda (last cur)
			 (+ (* 10 last) cur))
		     digits)))))
		       

(defun parse-number-list (&optional (separator #\,))
  (one-or-more (with-monad
		 (either (parse-character separator)
			 (unit t))
		 (parse-number))))

(defun parse-list (parser &optional (separator #\,))
  (one-or-more (with-monad
		 (either (cond
			   ((characterp separator) (parse-character separator))
			   ((stringp separator) (parse-string separator))
			   (t separator))
			 (unit t))
		 parser)))

