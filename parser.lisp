(in-package :aoc)

;; Use indexed-string in state to prevent copying
(defstruct (indexed-string
	    (:constructor make-indexed-string (&key str (cur-idx 0))))
  (str nil)
  (cur-idx 0))

(defun indexed-string-length (string)
  (- (length (indexed-string-str string))
     (indexed-string-cur-idx string)))

(defun indexed-string-char (string n)
  (char (indexed-string-str string)
	(+ (indexed-string-cur-idx string)
	   n)))

(defun indexed-string-tail (string &optional (n 1))
  (make-indexed-string :str (indexed-string-str string)
		       :cur-idx (min (+ (indexed-string-cur-idx string)
					n)
				     (length (indexed-string-str string)))))


(defstruct (rc-string
            (:constructor make-rc-string (&key str (cur-idx 0) (r 0) (c 0))))
  (str nil)
  (cur-idx 0)
  (r 0)
  (c 0))

(defun rc-string-length (rc-string)
  (- (length (rc-string-str rc-string))
     (rc-string-cur-idx rc-string)))

(defun rc-string-char (rc-string n)
  (char (rc-string-str rc-string)
	(+ (rc-string-cur-idx rc-string)
	   n)))

(defun rc-string-tail (rc-string &optional (n 1))
  (make-rc-string :str (rc-string-str rc-string)
		       :cur-idx (min (+ (rc-string-cur-idx rc-string)
					n)
				     (length (rc-string-str rc-string)))))
(defun rc-string-rc (rc-string)
  (list (rc-string-r rc-string) (rc-string-c rc-string)))

(defun rc-string-inc-r (rc-string)
  (make-rc-string :str (rc-string-str rc-string)
                  :cur-idx (rc-string-cur-idx rc-string)
                  :r (1+ (rc-string-r rc-string))
                  :c (rc-string-c rc-string)))
(defun rc-string-inc-c (rc-string)
  (make-rc-string :str (rc-string-str rc-string)
                  :cur-idx (rc-string-cur-idx rc-string)
                  :r (rc-string-r rc-string)
                  :c (1+ (rc-string-c rc-string))))

(defvar *unit-function*)
(defvar *bind-function*)

(defun unit (x)
  (funcall (symbol-function *unit-function*) x))

(defun bind (m f)
  (funcall (symbol-function *bind-function*) m f))


(defmacro then (m1 m2)
  (with-gensyms (x)
    `(bind ,m1 #'(lambda (,x) (declare (ignore ,x)) ,m2))))

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


(defun parser-bind (parser function)
  (lambda (string)
    (let ((result1 (funcall parser string)))
      (if result1
          (let* ((first-item (caar result1))
                 (rest-string (cdar result1))
                 (next-parser (funcall function first-item)))
            (funcall next-parser rest-string))
          nil))))

(defun parser-unit (x)
  (lambda (string)
    (list (cons x string))))

(defun parser-fail ()
  (lambda (string)
    (declare (ignore string))
    '()))

(defmacro run-parser (parser string &key (output-state nil) (indexed nil))
  `(let ((*unit-function* 'parser-unit)
         (*bind-function* 'parser-bind))
     (,(if output-state 'car 'caar)
      (funcall ,parser
	       ,(if indexed
                    string
                    `(make-indexed-string :str ,string))))))

(defun either (&rest parsers)
  "Parser that tries parsers in order and returns the first successful one."
  (lambda (string)
    (loop
      for parser in parsers
      for result = (funcall parser string)
      while (null result)
      finally (return result))))

(defun zero-or-more (parser)
  "Parses zero or more lots of parser. Always successfully returns with list of results. "
  (lambda (string)
    (let ((ret (loop for res = (funcall parser string)
                      until (null res)
                      do (setf string (cdr (first res)))
                      collect (car (first res)))))
      (list (cons ret string)))))

(defun one-or-more (parser)
  (with-monad
    (assign first parser)
    (assign rest (zero-or-more parser))
    (unit (cons first rest))))

(defun two-of (parser)
  (with-monad
    (assign first parser)
    (assign second parser)
    (unit (list first second))))

(defun n-of (n parser)
  (labels ((n-of-rec (acc n)
	     (if (= n 0)
		 (unit (nreverse acc))
		 (with-monad
                   (assign result parser)
                   (n-of-rec (cons result acc) (- n 1))))))
    (n-of-rec '() n)))

(defun parse-list (parser &optional (separator #\,))
  "Parses non-empty list of PARSER separated by SEPARATOR which is either a character, string, or other parser."
  (with-monad
    (assign first parser)
    (assign rest (one-or-more
                  (with-monad
		    (cond
		      ((characterp separator) (parse-character separator))
		      ((stringp separator) (parse-string separator))
		      (t separator))
		    parser)))
    (unit (cons first rest))))


(defun parse-empty-p ()
  "Parses sucessfully and returns t at end of string, nil otherwise."
  (lambda (string)
    (if (= 0 (indexed-string-length string))
	(list (cons t string))
	(list (cons nil string)))))

(defun parse-eof ()
  "Parses successfully and returns t at end of string, otherwise fails."
  (lambda (string)
    (if (= 0 (indexed-string-length string))
	(list (cons t string))
	'())))

(defun parse-character (item)
  "Parses a character literal, a character in a string, or a character that a predicate function returns T for."
  (lambda (string)
    (if (= 0 (indexed-string-length string))
	'()
	(let ((char (indexed-string-char string 0)))
	  (if (or (and (functionp item)
 		       (funcall item char))
		  (and (or (listp item) (stringp item))
		       (find char item))
		  (and (characterp item)
		       (char= char item)))
	      (list (cons char (indexed-string-tail string 1)))
	      '())))))

(defun parse-characters (item)
  (with-monad
    (assign chars (one-or-more (parse-character item)))
    (unit (format nil "~{~a~}" chars))))

(defun parse-any-character ()
  "Parse any character."
  (parse-character #'(lambda (x) x)))





(defun whitespace-char-p (x)
  (or (char= #\Space x)
      (not (graphic-char-p x))))
  
(defun parse-space ()
  (parse-character #\Space))

(defun parse-newline ()
  (either (then (parse-character #\Return)
                (parse-character #\Linefeed))
	  (parse-character #\Linefeed)
	  (parse-eof)))

(defun parse-line (parser)
  "Returns the results of parser followed by a newline or eof"
  (with-monad
    (assign res parser)
    (parse-newline)
    (unit res)))

(defun parse-lines (parser)
  (parse-list parser (parse-newline)))



(defun parse-lower-case ()
  (parse-character #'lower-case-p))

(defun parse-upper-case ()
  (parse-character #'upper-case-p))

(defun parse-alphanumeric ()
  (parse-character #'alphanumericp))


(defun parse-word ()
  (with-monad
    (assign chars (one-or-more
                   (parse-character (complement #'whitespace-char-p))))
    (unit (format nil "~{~A~}" chars))))

(defun parse-string (string)
  "Parses the given string or fails"
  (labels ((parse-string-rec (acc string)
	     (if (= 0 (length string))
		 (unit (format nil "~{~A~}" (reverse acc)))
		 (with-monad
                   (assign char (parse-any-character))
                   (if (char= char (char string 0))
                       (parse-string-rec (cons (char string 0) acc)
                                         (subseq string 1))
                       (parser-fail))))))
    (parse-string-rec '() string)))



(defun parse-digit ()
  (with-monad
    (assign digit (parse-character #'digit-char-p))
    (unit (digit-char-p digit))))

(defun parse-number ()
  "Parses an integer with an optional sign prefix +-."
  (with-monad
    (assign sign (either (parse-character "-+")			 
			 (unit #\+)))
    (assign digits (one-or-more (parse-digit)))
    (unit (* (if (char= sign #\-) -1 1)
	     (reduce #'(lambda (last cur)
			 (+ (* 10 last) cur))
		     digits)))))
		       
(defun parse-number-list (&optional (separator #\,))
  (parse-list (parse-number) separator))



(defmacro run-rc-parser (parser string &key (output-state nil))
  `(let ((*unit-function* 'parser-unit)
         (*bind-function* 'parser-bind))
     (,(if output-state 'car 'caar) (funcall ,parser
		    (make-rc-string :str ,string)))))


(defun lift-parser-rc (parser)
  "Transforms a parser into a row column parser."
  (lambda (rc-string)
    (let ((start-length (rc-string-length rc-string))
          (parser-result
            (run-parser parser (make-indexed-string
                                :str (rc-string-str rc-string)
                                :cur-idx (rc-string-cur-idx rc-string))
                        :indexed t
                        :output-state t)))
      (when parser-result
        (destructuring-bind (value . rest-string) parser-result            
            (let ((end-length (indexed-string-length rest-string)))
              (list (cons value (make-rc-string
                                 :str (indexed-string-str rest-string)
                                 :cur-idx (indexed-string-cur-idx rest-string)
                                 :c (+ (rc-string-c rc-string)
                                       (- start-length end-length))
                                 :r (rc-string-r rc-string))))))))))

(defun parse-rc-get-rc ()
  (lambda (rc-string)
    (list (cons (rc-string-rc rc-string) rc-string))))

(defun parse-rc-set-rc (rc)
  (lambda (rc-string)
    (list (cons nil (make-rc-string :str (rc-string-str rc-string)
                                    :cur-idx (rc-string-cur-idx rc-string)
                                    :r (first rc)
                                    :c (second rc))))))

(defun parse-rc-newline ()
  (with-monad
    (assign rc (parse-rc-get-rc))
    (assign ret (lift-parser-rc (parse-newline)))
    (parse-rc-set-rc (list (1+ (first rc)) 0))
    (unit ret)))


(defun parse-rc-character (item)
  (with-monad
    (assign rc (parse-rc-get-rc))
    (assign char (lift-parser-rc (parse-character item)))
    (unit (list rc char))))
