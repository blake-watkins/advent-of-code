(in-package :aoc)

;;; Monadic parser combinators
;;; Based on https://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf and parsec.

;;; A parser is a function that takes a string and returns a list of pairs of
;;; the thing that it's parsing and the rest of the string.
;;; Parser a :: String -> [(a, String)]

;;; Parsers are combined with BIND. Bind takes a parser for a's and (a function
;;; taking an a and returning a parser for b's). Bind returns a parser for b's.
;;; Bind :: Parser a -> (a -> Parser b) -> Parser b.
;;; Bind for parsers runs the first parser on the string to parse a, then calls
;;; the provided function on any parse results  to get a parser for b's, and then
;;; runs that parser on the unconsumed part of the string, returning the b's and
;;; the rest of the string.


;; *UNIT-FUNCTION* and *BIND-FUNCTION* should be dynamically bound to appropriate
;; functions and are called by UNIT and BIND. This allows for different monads
;; (state, continuation etc). PARSER-UNIT and PARSER-BIND are the implementations
;; for parsers. The RUN-PARSER macro binds the dynamic variables appropriately,
;; runs the parser on the input, and then returns the result of the parse.

(defvar *unit-function*)
(defvar *bind-function*)

(defun unit (x)
  (funcall (symbol-function *unit-function*) x))

(defun bind (m f)
  (funcall (symbol-function *bind-function*) m f))

(defun parser-unit (x)
  (lambda (string)
    (list (cons x string))))

(defun parser-bind (parser function)
  (lambda (string)
    (let ((result1 (funcall parser string)))
      (if result1
          (let* ((first-item (caar result1))
                 (rest-string (cdar result1))
                 (next-parser (funcall function first-item)))
            (funcall next-parser rest-string))
          nil))))

(defun parser-fail ()
  (lambda (string)
    (declare (ignore string))
    '()))

;; Binds unit and bind functions appropriately for parsers. Runs PARSER on the
;; input string STRING. If OUTPUT-STATE is T, then will return both the parse
;; result and the rest of the unparsed input string.
;; RUN-PARSER converts normal strings into INDEXED-STRINGs unless INDEXED is T.
(defmacro run-parser (parser string &key (output-state nil) (indexed nil))
  `(let ((*unit-function* 'parser-unit)
         (*bind-function* 'parser-bind))
     (,(if output-state 'car 'caar)
      (funcall ,parser
	       ,(if indexed
                    string
                    `(make-indexed-string :str ,string))))))


;; INDEXED-STRING contains a string and an index into that string. Rewrote all
;; parsers to use them rather than normal strings so that not so much copying
;; is happening. 
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



;; THEN takes two parsers and returns a parser that runs them in sequence but
;; ignores the result of the first.
;; It's written as a macro to prevent evaluation of M2 until the evaluation
;; of the lambda function in BIND. 
(defmacro then (m1 m2)
  (with-gensyms (x)
    `(bind ,m1 #'(lambda (,x) (declare (ignore ,x)) ,m2))))

;; The WITH-MONAD macro is syntax sugar similar to Haskell's DO notation.
;; https://en.wikibooks.org/wiki/Haskell/do_notation
;; It takes a list of forms, each form should either evaluate to a parser or
;; should be of the form (ASSIGN symbol parser) where symbol is any symbol and
;; parser is a form that evaluates to a parser. WITH-MONAD rewrites these
;; into calls to BIND/THEN and lambda functions.
;; For example:
;; AOC> (macroexpand '(with-monad
;;	          	(assign a (parse-number-list))
;;                      (parse-string " -> ")
;;                      (assign b (parse-number-list))
;;                      (unit (list a b))))
;; (BIND (PARSE-NUMBER-LIST)
;;       #'(LAMBDA (A)
;;           (THEN (PARSE-STRING " -> ")
;;                 (BIND (PARSE-NUMBER-LIST) #'(LAMBDA (B) (UNIT (LIST A B)))))))
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


;; Sequencing parsers

(defun either (&rest parsers)
  "Parser that tries each parser in PARSERS in order and returns the first successful parse."
  (lambda (string)
    (loop
      for parser in parsers
      for result = (funcall parser string)
      while (null result)
      finally (return result))))

(defun zero-or-more (parser)
  "Parser that parses zero or more lots of PARSER. Always successfully returns with list of results. "
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
  "Parser that runs PARSER N times. Returns the results in a list."
  (labels ((n-of-rec (acc n)
	     (if (= n 0)
		 (unit (nreverse acc))
		 (with-monad
                   (assign result parser)
                   (n-of-rec (cons result acc) (- n 1))))))
    (n-of-rec '() n)))

(defun parse-list (parser &optional (separator #\,))
  "Parser that parses a non-empty list of PARSERs separated by SEPARATOR. SEPARATOR can be either a character, string, or any other parser. Returns the results of the PARSER calls in a list."
  (with-monad
    (assign first parser)
    (assign rest (zero-or-more
                  (with-monad
		    (cond
		      ((characterp separator) (parse-character separator))
		      ((stringp separator) (parse-string separator))
		      (t separator))
		    parser)))
    (unit (cons first rest))))


;; Primitive parsers

(defun parse-empty-p ()
  "Parser that returns t at end of string, nil otherwise."
  (lambda (string)
    (if (= 0 (indexed-string-length string))
	(list (cons t string))
	(list (cons nil string)))))

(defun parse-eof ()
  "Parser that returns t at end of string, otherwise fails."
  (lambda (string)
    (if (= 0 (indexed-string-length string))
	(list (cons t string))
	'())))

(defun parse-character (item)
  "Parser that parses a character literal if ITEM is a character, any character from ITEM if ITEM is a string, or any character that ITEM returns T for if ITEM is a function."
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


;; Parsers built out of the primitive parsers

(defun parse-characters (item)
  "Parser that parses one or more of ITEM and returns the results as a string."
  (with-monad
    (assign chars (one-or-more (parse-character item)))
    (unit (format nil "~{~a~}" chars))))

(defun parse-any-character ()
  (parse-character #'(lambda (x) x)))


(defun parse-until (parser)
  "Parser that runs PARSER at subsequent characters until it succeeds."
  (either parser
          (with-monad
            (parse-any-character)
            (parse-until parser))))


(defun whitespace-char-p (x)
  (or (char= #\Space x)
      (not (graphic-char-p x))))
  
(defun parse-whitespace ()
  (one-or-more (parse-character #'whitespace-char-p)))

(defun parse-space ()
  (parse-character #\Space))

(defun parse-newline ()
  "Parser that parses either CRLF, LF, or the end of input."
  (either (then (parse-character #\Return)
                (parse-character #\Linefeed))
	  (parse-character #\Linefeed)
	  (parse-eof)))

(defun parse-line (parser)
  "Parser that runs PARSER followed by PARSE-NEWLINE. Returns the results of PARSER."
  (with-monad
    (assign res parser)
    (parse-newline)
    (unit res)))

(defun parse-lines (parser)
  "Parser that runs PARSER separated by PARSE-NEWLINE as many times as possible. Returns a list of the results of PARSER."
  (parse-list parser (parse-newline)))


(defun parse-lower-case ()
  (parse-character #'lower-case-p))

(defun parse-upper-case ()
  (parse-character #'upper-case-p))

(defun parse-alphanumeric ()
  (parse-character #'alphanumericp))

(defun parse-word ()
  "Parser that parses a group of non-whitespace digits."
  (with-monad
    (assign chars (one-or-more
                   (parse-character (complement #'whitespace-char-p))))
    (unit (format nil "~{~A~}" chars))))

(defun parse-keyword ()
  "Parser that runs PARSE-WORD to parse a group of non-whitespace digits and then returns that group as a symbol interned in the KEYWORD package. "
  (with-monad
    (assign word (parse-word))
    (unit (intern (string-upcase word) :keyword))))

(defun parse-string (string)
  "Parser that parses the given string or fails"
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

(defun parse-number (&key (base 10))
  "Parses an integer to the given BASE with an optional sign prefix +-."
  (with-monad
    (assign sign (either (parse-character "-+")			 
			 (unit #\+)))
    (assign digits (one-or-more (parse-digit)))
    (unit (* (if (char= sign #\-) -1 1)
             (digits-to-int digits :base base)))))
		       
(defun parse-number-list (&optional (separator #\,))
  (parse-list (parse-number) separator))



;; Row Column parsers. Parses a string keeping track of rows and columns. Was
;; useful for a few puzzles where layout was important but not often used. 
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
