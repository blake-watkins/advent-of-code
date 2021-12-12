(in-package :aoc)

;; *UNIT-FUNCTION* and *BIND-FUNCTION* should be dynamically bound to appropriate
;; functions and are called by UNIT and BIND. This allows for different monads
;; (state, continuation etc). 

(defvar *unit-function*)
(defvar *bind-function*)

(defun unit (x)
  (funcall (symbol-function *unit-function*) x))

(defun bind (m f)
  (funcall (symbol-function *bind-function*) m f))

;; THEN takes two monads and returns a monad that runs them in sequence but
;; ignores the result of the first.
;; It's written as a macro to prevent evaluation of M2 until the evaluation
;; of the lambda function in BIND. 
(defmacro then (m1 m2)
  (with-gensyms (x)
    `(bind ,m1 #'(lambda (,x) (declare (ignore ,x)) ,m2))))

;; The WITH-MONAD macro is syntax sugar similar to Haskell's DO notation.
;; https://en.wikibooks.org/wiki/Haskell/do_notation
;; It takes a list of forms, each form should either evaluate to a monad or
;; should be of the form (ASSIGN symbol monad) where symbol is any symbol and
;; monad is a form that evaluates to a monad. WITH-MONAD rewrites these
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


