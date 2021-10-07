(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "alexandria")
  (use-package :alexandria)
  ;; has tree-map
  (ql:quickload "sycamore")
  (use-package :sycamore)  )



(defclass computer-state ()
  ((program
    :initarg :program
    :initform nil)
   (ip
    :initarg :ip
    :initform 0)
   (input
    :initform nil)
   (input-source
    :initform nil)
   (output
    :initform nil)
   (relative-base
    :initform 0)))

(defun compare-index (a b)
  (cond ((< a b) -1)
	((> a b) 1)
	(t 0)))

(defmethod initialize-instance :after ((state computer-state)
				       &key program-text template)
  (when template
    (loop
       for field in '(program ip input input-source output relative-base)
       do (setf (slot-value state field) (slot-value template field))))

  (when program-text
    (let* ((program-list  (parse-program-text program-text))
	   (program-alist
	    (loop 
	       for index from 0
	       for instruction in program-list
	       collect (cons index instruction)))
	   (program (tree-map-insert-alist
		     (make-tree-map #'compare-index)
		     program-alist)))
	(setf (slot-value state 'program) program))))

(defun parse-program-text (text)
  (run-parser (with-monad
                (assign list (parse-list (parse-number)))
                (parse-newline)
                (unit list))
              text))

;; State a => State -> (a, State)
;; bind :: State -> (a, State) -> (a -> State -> (b, State)) -> State -> (b, State)
;; State Monad Primitives
(defun state-bind (ma function)
  (lambda (state-1)
    (multiple-value-bind (a state-2) (funcall ma state-1)
      (let ((mb (funcall function a)))
	(funcall mb state-2)))))

(defun state-then (ma mb)
  (state-bind ma
	      #'(lambda (x) (declare (ignore x)) mb)))

(defun state-unit (a)
  (lambda (state)
    (values a state)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rewrite-state-forms (forms)
    (flet ((rewrite-state-form (cur rest)
	     (if (eq (car cur) 'assign)
		 (if rest
		     `(state-bind ,(third cur)
				  #'(lambda (,(second cur))
				      ,(rewrite-state-forms rest)))
		     (third cur))
		 (if rest
		     `(state-then ,cur
				  ,(rewrite-state-forms rest))
		     cur))))
      (if (null forms)
	  '()
	  (rewrite-state-form (car forms) (cdr forms))))))
	  
(defmacro with-state-monad (&body body)
  (rewrite-state-forms body))

(defmacro execute-program (computer program)
  (with-gensyms (output new-computer)
    `(multiple-value-bind (,output ,new-computer)
         (funcall ,program ,computer)
       (setf ,computer ,new-computer)
       ,output)))

;; Defines a state primitive reader
;; Defines a function taking state.
;; Evaluates body forms and returns two values: the value of the last body
;;  form and the state.
;; Wraps the body forms in a with-slots for access to the slots of the stat
(defmacro def-state-reader (name args slots &body body)
  (with-gensyms (ret)
    (let ((body `((let ((,ret (progn ,@body)))
                    (values ,ret state)))))
      `(defun ,name ,args
         (lambda (state)
           ,@(if slots
                 `((with-slots ,slots state
                     ,@body))
                 body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rewrite-state-writer-body (slots body)
    (labels ((with-slots-code (slots state body)
               (if slots 
                   `((with-slots ,slots ,state
                       ,@body))
                   body))
             (new-slot-entry (slot)
               (let ((new-name
                       (concatenate 'string "NEW-" (symbol-name slot))))
                 (list (intern new-name) slot)))
             (new-state-code (slots body)
               `((let ((new-state (make-instance 'computer-state
                                                 :template state)))
                   ,@(let ((new-slots (mapcar #'new-slot-entry slots)))
                       (with-slots-code new-slots 'new-state body))))))
      
      (with-slots-code slots 'state
        (new-state-code slots body)))))

;; Defines a state primitive writer
;; Defines a function taking state.
;; Wraps the body forms in a with-slots for access to the slots of the state
;; Creates a new state named new-state and wraps the body forms in a
;;  with-slots for access to the slots of the new-state. All the new
;;  slot names are prefixed with "NEW-"
;; Evaluates body forms. If the last form is not a (values ... ) form, then
;;  appends (values nil new-state) to the end of body.
(defmacro def-state-writer (name args slots &body body)
  (let ((final-form (lastcar body)))
    (unless (and final-form
                 (listp final-form)
                 (eq 'values (car final-form)))
      (setf body (append body '((values nil new-state))))))
  `(defun ,name ,args
     (lambda (state)
       ,@(rewrite-state-writer-body slots body))))

;; State primitives

(def-state-reader get-memory (index) (program)
  (tree-map-find program index 0))

(def-state-writer set-memory (index value) (program)
  (setf new-program (tree-map-insert program index value)))

(def-state-reader get-ip () (ip) ip)

(def-state-writer set-ip (value) (ip) (setf new-ip value))

(def-state-writer front-input () (input)
  (setf new-input (cdr input))
  (values (car input) new-state))

(def-state-writer front-output () (output)
  (setf new-output (cdr output))
  (values (car output) new-state))

(def-state-writer push-back-input (value) (input)
  (setf new-input (if (listp value)
		      (append input value)
		      (append input (list value)))))

(def-state-writer push-back-output (value) (output)
  (setf new-output (if (listp value)
		       (append output value)
		       (append output (list value)))))

(def-state-reader queue-length (name) ()
  (length (slot-value state name)))

(def-state-reader get-relative-base () (relative-base) relative-base)

(def-state-writer set-relative-base (value) (relative-base)
  (setf new-relative-base value))

(def-state-reader get-input-source () (input-source) input-source)

(def-state-writer set-input-source (value) (input-source)
  (setf new-input-source value))


(defun get-input ()
  (with-state-monad
    (assign input-source (get-input-source))
    (if input-source
	(state-unit (funcall input-source :get-input))
	(front-input))))

(defun provide-input (value)
  (push-back-input (if (stringp value) 
                       (map 'list #'char-code value)
                       value)))

(defun input-available ()
  (with-state-monad
    (assign input-source (get-input-source))
    (if input-source
        (state-unit (funcall input-source :input-available))
        (with-state-monad
          (assign input-size (queue-length 'input))
          (state-unit (> input-size 0))))))

(defun get-output (&key (ascii nil))
  (with-state-monad
    (assign output (front-output))
    (state-unit (if ascii (code-char output) output))))


(defun get-all-output (&key (ascii nil))
  (labels ((get-all-output-rec (ascii)
             (with-state-monad
               (assign first (get-output :ascii ascii))
               (assign more (output-available))
               (if more
	           (with-state-monad
	             (assign rest (get-all-output-rec ascii))
	             (state-unit (cons first rest)))
	           (state-unit (list first))))))
    (with-state-monad
      (assign output-available (output-available))
      (if output-available
          (with-state-monad
            (assign output (get-all-output-rec ascii))
            (state-unit (if ascii
                            (format nil "~{~a~}" output)
                            output)))
          (state-unit nil)))))

(defun provide-output (value)
  (push-back-output value))

(defun output-available ()
  (with-state-monad
    (assign output-size (queue-length 'output))
    (state-unit (> output-size 0))))

(defun get-opcode ()
  (with-state-monad
    (assign cur-ip (get-ip))
    (assign cur-instruction (get-memory cur-ip))
    (state-unit (ecase (decode-opcode cur-instruction)
		  (1 :add)
		  (2 :multiply)
		  (3 :input)
		  (4 :output)
		  (5 :jump-if-true)
		  (6 :jump-if-false)
		  (7 :less-than)
		  (8 :equals)
		  (9 :adjust-relative-base)
		  (99 :halt)))))

(defun decode-parameter-mode (parameter instruction)
  (let ((val (mod (floor instruction (ecase parameter
				       (1 100)
				       (2 1000)
				       (3 10000)))
		  10)))
    (ecase val
      (0 :position)
      (1 :immediate)
      (2 :relative))))


(defun decode-opcode (instruction)
  (mod instruction 100))

(defun get-parameter (n)
  (with-state-monad
    (assign cur-ip (get-ip))
    (assign cur-instruction (get-memory cur-ip))
    (assign parameter (get-memory (+ cur-ip n)))
    (ecase (decode-parameter-mode n cur-instruction)
      (:position (get-memory parameter))
      (:immediate (state-unit parameter))
      (:relative (with-state-monad
		   (assign relative-base (get-relative-base))
		   (get-memory (+ parameter relative-base)))))))

(defun set-parameter (n value)
  (with-state-monad
    (assign cur-ip (get-ip))
    (assign cur-instruction (get-memory cur-ip))
    (assign parameter (get-memory (+ cur-ip n)))
    (ecase (decode-parameter-mode n cur-instruction)
      (:position (set-memory parameter value))
      (:relative (with-state-monad
		   (assign relative-base (get-relative-base))
		   (set-memory (+ parameter relative-base)
			       value))))))

(defun advance-ip (n)
  (with-state-monad
    (assign cur-ip (get-ip))
    (set-ip (+ cur-ip n))))

(defun binary-operation (op)
  (with-state-monad
    (assign a (get-parameter 1))
    (assign b (get-parameter 2))
    (set-parameter 3 (funcall op a b))))

(defun add () (binary-operation #'+))
(defun multiply () (binary-operation #'*))

(defun step-program ()
  (with-state-monad
    (assign opcode (get-opcode))
    (ecase opcode
      ((:add :multiply)
       (with-state-monad
	 (ecase opcode
           (:add (add))
           (:multiply (multiply)))
	 (advance-ip 4)
	 (state-unit t)))
      (:input
       (with-state-monad
	 (assign value (get-input))
	 (set-parameter 1 value)
	 (advance-ip 2)
	 (state-unit t)))
      (:output
       (with-state-monad
	 (assign value (get-parameter 1))
	 (provide-output value)
	 (advance-ip 2)
	 (state-unit t)))
      ((:jump-if-true :jump-if-false)
       (with-state-monad
	 (assign test (get-parameter 1))
	 (assign target (get-parameter 2))
	 (if (ecase opcode
               (:jump-if-false (= 0 test))
               (:jump-if-true (/= 0 test)))
	     (set-ip target)
	     (advance-ip 3))
	 (state-unit t)))
      ((:less-than :equals)
       (with-state-monad
	 (assign param1 (get-parameter 1))
	 (assign param2 (get-parameter 2))
	 (set-parameter 3 (if (ecase opcode
                                (:less-than (< param1 param2))
                                (:equals (= param1 param2)))
	                      1
                              0))
	 (advance-ip 4)
	 (state-unit t)))
      (:adjust-relative-base
       (with-state-monad
	 (assign adjust-by (get-parameter 1))
	 (assign cur-relative-base (get-relative-base))
	 (set-relative-base (+ cur-relative-base adjust-by))
	 (advance-ip 2)
	 (state-unit t)))
      (:halt
       (with-state-monad
	 (state-unit nil))))))

(defun run-until-halt ()
  (with-state-monad
    (assign continue (step-program))
    (if continue
	(run-until-halt)
	(state-unit nil))))

(defun run-until-output-or-halt ()
  (with-state-monad
    (assign continue (step-program))
    (if continue
	(with-state-monad
	  (assign output-available (output-available))
	  (if output-available
	      (state-unit t)
	      (run-until-output-or-halt)))
	(state-unit nil))))

(defun run-until-input-or-halt ()
  (with-state-monad
    (assign opcode (get-opcode))
    (assign input-available (input-available))
    (if (and (eq opcode :input) (not input-available))
        (state-unit t)
        (with-state-monad
          (assign continue (step-program))
          (if continue
              (run-until-input-or-halt)
              (state-unit nil))))))
